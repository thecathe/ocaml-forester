(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 *)

open Forester_prelude
open Forester_core

open struct
  module T = Types
end

open State.Syntax

let load (tree_dirs : Eio.Fs.dir_ty Eio.Path.t list) =
  Logs.debug (fun m ->
      m "loading trees from %i directories" (List.length tree_dirs));
  Dir_scanner.scan_directories tree_dirs |> Seq.map Imports.load_tree

let parse (forest : State.t) =
  let trees = forest.index |> URI.Tbl.to_seq_values |> List.of_seq in
  let results =
    let@ tree = List.filter_map @~ trees in
    match tree with
    | Document doc -> Some (Parse.parse_document ~config:forest.config doc)
    | Parsed _ | Expanded _ | Resource _ -> None
  in
  let@ result = List.partition_map @~ results in
  match result with Ok t -> Right t | Error d -> Left d

let reparse (doc : Lsp.Text_document.t) (forest : State.t) =
  Logs.debug (fun m -> m "reparsing");
  let uri =
    URI_scheme.lsp_uri_to_uri ~base:forest.config.url
    @@ Lsp.Text_document.documentUri doc
  in
  begin match Parse.parse_document ~config:forest.config doc with
  | Ok code ->
    forest.={uri} <- Parsed code;
    Imports.fixup code forest
  | Error d -> forest.?{uri} <- [d]
  end;
  forest

let build_import_graph (forest : State.t) =
  let@ () = Reporter.trace "when resolving imports" in
  let errors = ref [] in
  let push d = errors := d :: !errors in
  let new_graph =
    Reporter.run ~emit:push ~fatal:(fun d ->
        push d;
        Reporter.Tty.display d;
        forest.import_graph)
    @@ fun () -> Imports.build forest
  in
  (!errors, new_graph)

let expand (forest : State.t) = Expand.expand_tree ~forest

let expand_all (forest : State.t) =
  let diagnostics = ref [] in
  let task vertex =
    match vertex with
    | T.Content_vertex _ -> assert false
    | T.Uri_vertex uri -> (
      match forest.={uri} with
      | None -> ()
      | Some tree -> (
        match Tree.to_code tree with
        | Some tree ->
          let expanded, errors = Expand.expand_tree ~forest tree in
          diagnostics := errors @ !diagnostics;
          forest.={uri} <- Expanded expanded;
          forest.?{uri} <- errors
        | None ->
          Logs.debug (fun m -> m "expanding: no source code for %a" URI.pp uri);
          assert false))
  in
  Forest_graph.topo_iter task forest.import_graph;
  !diagnostics

let run_jobs (forest : State.t) jobs =
  Logs.debug (fun m -> m "Running %d jobs" (List.length jobs));
  (* All resources induced by LaTeX jobs must be planted prior to publication
     export. *)
  let resources_to_plant =
    let@ Range.{value; loc} = Eio.Fiber.List.map ~max_fibers:20 @~ jobs in
    let@ () = Reporter.easy_run in
    match value with
    | Job.LaTeX_to_svg job ->
      let svg = Build_latex.latex_to_svg ~env:forest.env ?loc job.source in
      let uri = Job.uri_for_latex_to_svg_job ~base:forest.config.url job in
      T.Asset {uri; content = svg}
    | Job.Syndicate syndication -> T.Syndication syndication
  in
  begin
    (* It is probably not save to plant the articles in parallel, so this is
       done sequentially! *)
    let@ resource = List.iter @~ resources_to_plant in
    State.plant_resource resource forest
  end

let eval (forest : State.t) =
  let result =
    State.get_all_unevaluated forest
    |> Seq.filter Tree.is_expanded
    |> Seq.map (fun tree ->
        let tree = Option.get @@ Tree.to_syn tree in
        match identity_to_uri tree.identity with
        | None ->
          Reporter.fatal Internal_error
            ~extra_remarks:
              [Asai.Diagnostic.loctext "can't evaluate a tree with no URI"]
        | Some uri ->
          let source_path =
            if forest.dev then URI.Tbl.find_opt forest.resolver uri else None
          in
          Eval.eval_tree ~config:forest.config ~source_path ~uri tree.nodes)
  in
  result

let eval_only (uri : URI.t) (forest : State.t) =
  match forest.={uri} with
  | None -> assert false
  | Some (Document _) -> assert false
  | Some (Parsed _) | Some (Resource _) -> assert false
  | Some (Expanded expanded) ->
    let source_path =
      if forest.dev then URI.Tbl.find_opt forest.resolver uri else None
    in
    (* NOTE: Not running jobs. *)
    let Eval.{articles; jobs = _}, diagnostics =
      Eval.eval_tree ~config:forest.config ~source_path ~uri expanded.nodes
    in
    begin
      let@ article = List.iter @~ articles in
      State.plant_article article forest
    end;
    (forest, diagnostics)

let check_status _uri (forest : State.t) =
  match forest with {dependency_cache = _; _} -> (forest, None)

let implant_foreign (state : State.t) : State.t * _ =
  begin
    Logs.debug (fun m ->
        m "implanting %i foreign forests" (List.length state.config.foreign));
    let module EP = Eio.Path in
    let@ foreign = List.iter @~ state.config.foreign in
    let path = Eio_util.path_of_file ~env:state.env foreign.path in
    let path_str = EP.native_exn path in
    Reporter.log Format.pp_print_string
      (Format.sprintf "Implant foreign forest from `%s'" path_str);
    let blob =
      try EP.load path
      with _ ->
        Reporter.fatal IO_error
          ~extra_remarks:
            [
              Asai.Diagnostic.loctextf
                "Could not read foreign forest blob at `%s`" path_str;
            ]
    in
    match Repr.of_json_string (T.forest_t T.content_t) blob with
    | Ok foreign_forest ->
      let@ r = List.iter @~ foreign_forest in
      State.plant_resource ~route_locally:foreign.route_locally
        ~include_in_manifest:foreign.include_in_manifest r state
    | Error (`Msg err) ->
      Reporter.fatal Parse_error
        ~extra_remarks:
          [
            Asai.Diagnostic.loctextf "Could not parse foreign forest blob: %s"
              err;
          ]
    | exception exn ->
      Reporter.fatal Parse_error
        ~extra_remarks:
          [
            Asai.Diagnostic.loctextf
              "Encountered unknown error while decoding foreign forest blob: %s"
              (Printexc.to_string exn);
          ]
  end;
  (state, [])
