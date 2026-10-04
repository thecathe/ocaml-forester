(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_prelude
open State.Syntax

open struct
  module T = Types
end

let update (action : Action.t) (forest : State.t) =
  let open Action in
  let forest = State.update_history forest action in
  match action with
  | Quit e -> begin match e with Fail -> exit 1 | Finished -> exit 0 end
  | Query q ->
    let@ () = Reporter.trace "when running query" in
    let r = Forest.run_datalog_query forest.graphs q in
    (Query_results r, forest)
  | Query_results _ -> (Done, forest)
  | Report_errors (_, next_action) -> (next_action, forest)
  | Load_all_configured_dirs ->
    let@ () = Reporter.trace "when loading files from disk" in
    let tree_dirs =
      Eio_util.paths_of_dirs ~env:forest.env forest.config.trees
    in
    List.iter (fun path -> assert (Eio.Path.is_directory path)) tree_dirs;
    let docs = Phases.load tree_dirs in
    Seq.iter
      (fun doc ->
        let lsp_uri = Lsp.Text_document.documentUri doc in
        let uri = URI_scheme.lsp_uri_to_uri ~base:forest.config.url lsp_uri in
        URI.Tbl.replace forest.resolver uri (Lsp.Uri.to_path lsp_uri);
        forest.={uri} <- Document doc)
      docs;
    Logs.debug (fun m -> m "loaded %d trees" (Seq.length docs));
    (Parse_all, forest)
  | Parse_all ->
    let@ () = Reporter.trace "when parsing trees" in
    let errors, succeeded = Phases.parse forest in
    List.iter
      (fun (code : Tree.code) ->
        let@ uri = Option.iter @~ Tree.(identity_to_uri code.identity) in
        forest.={uri} <- Parsed code;
        forest.?{uri} <- [])
      succeeded;
    if List.length errors = 0 then
      assert (Seq.for_all Tree.is_parsed (URI.Tbl.to_seq_values forest.index));
    List.iter
      (fun diag ->
        let@ uri =
          Option.iter
          @~ Option.map
               (URI_scheme.lsp_uri_to_uri ~base:forest.config.url)
               (Reporter.guess_uri diag)
        in
        forest.?{uri} <- [diag])
      errors;
    (Action.report ~errors ~next_action:Build_import_graph, forest)
  | Build_import_graph ->
    let@ () = Reporter.trace "when building import graph" in
    let errors, import_graph = Phases.build_import_graph forest in
    Logs.debug (fun m ->
        m "import graph has %d vertices" (Forest_graph.nb_vertex import_graph));
    (Action.report ~errors ~next_action:Expand_all, {forest with import_graph})
  | Expand_all ->
    let@ () = Reporter.tracef "when expanding trees" in
    Logs.debug (fun m -> m "expanding trees");
    let errors = Phases.expand_all forest in
    (Action.report ~errors ~next_action:Eval_all, forest)
  | Expand uri ->
    let@ () = Reporter.tracef "when expanding %a" URI.pp uri in
    begin match Option.bind forest.={uri} Tree.to_code with
    | None ->
      ( Action.report
          ~errors:[Reporter.diagnostic (Resource_not_found uri)]
          ~next_action:Done,
        forest )
    | Some code ->
      let result, errors = Phases.expand forest code in
      forest.={uri} <- Expanded result;
      (Action.report ~errors ~next_action:(Eval uri), forest)
    end
  | Eval_all ->
    let@ () = Reporter.tracef "when evaluating" in
    Logs.debug (fun m -> m "evaluating");
    let result = Phases.eval forest in
    let jobs, errors =
      result |> List.of_seq
      |> List.map (fun (Eval.{articles; jobs}, diagnostics) ->
          begin
            let@ article = List.iter @~ articles in
            State.plant_article article forest
          end;
          (jobs, diagnostics))
      |> List.split
      |> fun (j, e) -> (List.concat j, List.concat e)
    in
    Logs.debug (fun m ->
        m "got %d resources " (Seq.length (State.get_all_resources forest)));
    (Action.report ~errors ~next_action:(Run_jobs jobs), forest)
  | Eval uri ->
    let@ () = Reporter.tracef "when evaluating %a" URI.pp uri in
    let result, _err = Phases.eval_only uri forest in
    (Done, result)
  | Plant_assets ->
    let@ () = Reporter.tracef "when planting assets" in
    (* TODO: We really only need to plant the assets that are referred to (look
       for calls to Asset_router.uri_of_asset).*)
    let paths =
      Dir_scanner.scan_asset_directories
        (Eio_util.paths_of_dirs ~env:forest.env forest.config.assets)
    in
    Logs.debug (fun m -> m "planting %i assets" (Seq.length paths));
    let module EP = Eio.Path in
    begin
      let@ path = Eio.Fiber.List.iter ~max_fibers:20 @~ List.of_seq paths in
      let@ () = Reporter.easy_run in
      let content = EP.load path in
      let source_path = EP.native_exn path in
      let uri =
        Asset_router.install ~config:forest.config ~source_path ~content
      in
      Logs.debug (fun m -> m "Installed %s at %a" source_path URI.pp uri);
      State.plant_resource (T.Asset {uri; content}) forest
    end;
    (Done, forest)
  | Plant_foreign ->
    let@ () = Reporter.tracef "when planting foreign forest" in
    Logs.debug (fun m -> m "Planting foreign forests");
    let result, err = Phases.implant_foreign forest in
    (Report_errors (err, Done), result)
  | Run_jobs jobs ->
    Phases.run_jobs forest jobs;
    (Done, forest)
  | Load_tree path ->
    let@ () = Reporter.tracef "when loading %a" Eio.Path.pp path in
    let doc = Imports.load_tree path in
    Logs.debug (fun m -> m "%s" (Lsp.Text_document.text doc));
    let lsp_uri = Lsp.Text_document.documentUri doc in
    let uri = URI_scheme.lsp_uri_to_uri ~base:forest.config.url lsp_uri in
    forest.={uri} <- Document doc;
    (Parse lsp_uri, forest)
  | Parse uri ->
    let@ () = Reporter.tracef "when parsing %s" (Lsp.Uri.to_string uri) in
    Logs.debug (fun m -> m "Reparsing");
    let uri = URI_scheme.lsp_uri_to_uri ~base:forest.config.url uri in
    begin match Option.bind forest.={uri} Tree.to_doc with
    | Some doc -> begin
      match Parse.parse_document ~config:forest.config doc with
      | Ok code ->
        forest.={uri} <- Parsed code;
        forest.?{uri} <- [];
        Imports.fixup code forest;
        (Expand uri, forest)
      | Error diagnostic ->
        forest.?{uri} <- [diagnostic];
        (Report_errors ([diagnostic], Done), forest)
    end
    | None -> (
      match Imports.resolve_uri_to_code forest uri with
      | None -> Reporter.fatal (Resource_not_found uri)
      | Some code ->
        Imports.fixup code forest;
        forest.={uri} <- Parsed code;
        (Expand uri, forest))
    end
  | Done -> (Done, forest)

let run_until_done a s : State.t =
  let fatal d =
    Reporter.Tty.display d;
    exit 1
  in
  let emit = Reporter.Tty.display in
  Reporter.run ~emit ~fatal @@ fun () ->
  let rec go action state =
    let new_action, new_state = update action state in
    match action with
    | Quit Fail -> exit 1
    | Quit Finished -> exit 0
    | Done -> new_state
    | _ -> go new_action new_state
  in
  go a s

let implant_foreign = run_until_done Plant_foreign
let plant_assets = run_until_done Plant_assets

let any_fatal =
  List.fold_left
    Asai.Diagnostic.(fun acc x -> acc || x.severity = Error || x.severity = Bug)
    false

let batch_run ~env ~(config : Config.t) ~dev =
  let init =
    State.make ~env ~config ~dev () |> plant_assets |> implant_foreign
  in
  let rec go action state =
    let new_action, new_state = update action state in
    match action with
    | Quit Fail -> exit 1
    | Quit Finished -> exit 0
    | Done -> new_state
    | Report_errors (errors, _) ->
      assert (List.length errors > 0);
      Logs.debug (fun m -> m "got %d errors" (List.length errors));
      List.iter Reporter.Tty.display errors;
      if any_fatal errors then go (Quit Fail) new_state
      else go new_action new_state
    | _ -> go new_action new_state
  in
  go Load_all_configured_dirs init

let language_server ~env ~config =
  let init = State.make ~env ~config ~dev:true () in
  let rec go action state =
    let new_action, new_state = update action state in
    match action with
    | Quit Fail -> exit 1
    | Quit Finished -> exit 0
    | Done -> new_state
    | _ -> go new_action new_state
  in
  let _, state = update Plant_assets init in
  (* TODO: this ought to implant the foreign trees as well *)
  go Load_all_configured_dirs state

let run_with_history a s =
  let history = ref [] in
  let rec go action state =
    history := action :: !history;
    match update action state with
    | new_action, new_state ->
      if action = Done then new_state
      else begin
        go new_action new_state
      end
  in
  let forest = go a s in
  (forest, List.rev !history)

let collect_emitted_errors a s =
  let errors = ref [] in
  let rec go action state =
    match update action state with
    | new_action, new_state -> (
      match action with
      | Done -> new_state
      | Report_errors (errs, _) -> begin
        errors := errs @ !errors;
        go new_action new_state
      end
      | _ -> go new_action new_state)
  in
  let forest = go a s in
  (forest, List.rev !errors)

let rec force : Action.t list -> State.t -> State.t =
 fun msgs state ->
  match msgs with
  | [] -> state
  | msg :: remaining ->
    let _discard, new_state = update msg state in
    force remaining new_state
