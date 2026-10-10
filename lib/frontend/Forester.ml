(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler

open struct
  module M = URI.Map
  module T = Types
  module EP = Eio.Path
end

type env = Eio_unix.Stdenv.base
type dir = Eio.Fs.dir_ty EP.t
type target = HTML | JSON | XML | STRING

let output_dir_name = "output"

let create_tree ~env ~dest_dir ~prefix ~template ~mode ~(forest : State.t) =
  let next = URI_util.next_uri ~prefix ~mode ~forest in
  let fname = next ^ ".tree" in
  let now = Human_datetime.now () in
  let template_content =
    match template with
    | None -> ""
    | Some name ->
      EP.load EP.(Eio.Stdenv.cwd env / "templates" / (name ^ ".tree"))
  in
  let body =
    Format.asprintf "\\date{%a}\n" Human_datetime.pp now
    ^ String.concat ""
        (List.map
           (fun a -> Printf.sprintf "\\author{%s}\n" a)
           forest.config.new_authors)
  in
  let create = `Exclusive 0o644 in
  (* If no dest_dir is passed, use the config *)
  let dir =
    match dest_dir with
    | Some dir -> dir
    | None -> (
      match forest.config.trees with
      | dir :: _ -> dir
      | [] ->
        Reporter.fatal Missing_argument
          ~extra_remarks:
            [
              Asai.Diagnostic.loctext
                "Unable to guess destination director for new tree; please \
                 supply one.";
            ])
  in
  let path = EP.(env#fs / dir / fname) in
  EP.save ~create path @@ body ^ template_content;
  EP.native_exn path

let complete ~(forest : State.t) prefix : (string * string) List.t =
  let config = forest.config in
  let@ article =
    List.filter_map @~ List.of_seq @@ State.get_all_articles forest
  in
  let@ uri = Option.bind article.frontmatter.uri in
  let short_uri = URI.display_path_string ~base:config.url uri in
  let@ title = Option.bind article.frontmatter.title in
  let title = Plain_text_client.string_of_content ~forest title in
  if String.starts_with ~prefix title then Some (short_uri, title) else None

let is_hidden_file fname = String.starts_with ~prefix:"." fname

let output_path ~cwd ~(forest : State.t) =
  let suffix =
    String.concat "/"
    @@ List.filter (fun x -> not (x = ""))
    @@ URI.path_components forest.config.url
  in
  Eio.Path.(cwd / output_dir_name / suffix)

let copy_contents_of_dir ~env ~(forest : State.t) dir =
  let cwd = Eio.Stdenv.cwd env in
  let dest_dir = EP.native_exn @@ output_path ~cwd ~forest in
  Logs.debug (fun m ->
      m "copying contents of directory %s to %s." (Eio.Path.native_exn dir)
        dest_dir);
  let@ fname = List.iter @~ EP.read_dir dir in
  if not @@ is_hidden_file fname then
    let path = EP.(dir / fname) in
    let source = EP.native_exn path in
    Eio_util.copy_to_dir ~env ~cwd ~source ~dest_dir

let json_manifest ~dev ~(forest : State.t) : string =
  let render = Json_manifest_client.render_tree ~forest in
  let articles =
    let@ tree = Seq.filter_map @~ Forest.to_seq_values forest.index in
    let@ evaluated = Option.bind @@ Tree.to_evaluated tree in
    if evaluated.include_in_manifest then Tree.to_article tree else None
  in
  articles |> List.of_seq
  |> List.sort (Forest_util.compare_article ~forest)
  |> List.filter_map (fun tree -> render ~dev tree)
  |> (fun t -> `List t)
  |> Yojson.Safe.to_string

let html_redirect uri_string =
  Pure_html.to_xml
  @@
  let open Pure_html in
  let open HTML in
  html []
    [
      head []
        [
          meta [http_equiv `refresh; content "0;url=%s" uri_string];
          meta [charset "utf-8"];
        ];
    ]

let outputs_for_article ~(forest : State.t) (article : _ T.article) =
  match article.frontmatter.uri with
  | None -> []
  | Some uri ->
    let xml_route =
      URI.with_path_components
        (URI.append_path_component (URI.path_components uri) "index.xml")
        uri
    in
    let html_route =
      URI.with_path_components
        (URI.append_path_component (URI.path_components uri) "index.html")
        uri
    in
    let xml_content =
      Format.asprintf "%a"
        (Legacy_xml_client.pp_xml ~forest ~stylesheet:"default.xsl")
        article
    in
    let html_content =
      html_redirect @@ URI.href_string
      @@ URI.make
           ~path:
             ("" :: Legacy_xml_client.local_path_components forest.config xml_route)
           ()
    in
    [(xml_route, xml_content); (html_route, html_content)]

let outputs_for_asset (asset : T.asset) =
  let route = asset.uri in
  [(route, asset.content)]

let outputs_for_json_blob_syndication ~(forest : State.t)
    (syndication : _ T.json_blob_syndication) =
  if URI.host syndication.blob_uri = URI.host forest.config.url then
    let vertices = Forest.run_datalog_query forest.graphs syndication.query in
    let resources =
      let@ vertex = List.filter_map @~ Vertex_set.elements vertices in
      match vertex with
      | Content_vertex _ -> None
      | Uri_vertex uri ->
        (* Same rule as the generated backmatter sections: a hidden or
           redirect-flagged tree must not surface through a query. *)
        if Forest_util.is_redirect_flagged forest uri then None
        else State.get_resource forest uri
    in
    let json_content =
      Repr.to_json_string ~minify:true (T.forest_t T.content_t) resources
    in
    [(syndication.blob_uri, json_content)]
  else []

let outputs_for_atom_feed_syndication ~(forest : State.t)
    (syndication : T.atom_feed_syndication) =
  let atom_nodes =
    Atom_client.render_feed forest ~source_uri:syndication.source_uri
      ~feed_uri:syndication.feed_uri
  in
  let atom_content =
    Format.asprintf "%a" (Pure_html.pp_xml ~header:true) atom_nodes
  in
  [(syndication.feed_uri, atom_content)]

let outputs_for_syndication ~(forest : State.t) = function
  | T.Json_blob syndication ->
    outputs_for_json_blob_syndication ~forest syndication
  | T.Atom_feed syndication ->
    outputs_for_atom_feed_syndication ~forest syndication

let outputs_for_resource ~(forest : State.t) (evaluated : Tree.evaluated) =
  if not evaluated.route_locally then []
  else
    match evaluated.resource with
    | T.Article article -> outputs_for_article ~forest article
    | T.Asset asset -> outputs_for_asset asset
    | T.Syndication syndication -> outputs_for_syndication ~forest syndication

let uri_to_local_path ~(forest : State.t) uri =
  String.concat "/" @@ Legacy_xml_client.local_path_components forest.config uri

let render_forest ~dev ~(forest : State.t) : unit =
  let cwd = Eio.Stdenv.cwd forest.env in
  let all_resources = forest |> State.get_all_evaluated in
  Logs.debug (fun m -> m "Rendering %i resources" (Seq.length all_resources));
  begin
    let json_string = json_manifest ~dev ~forest in
    let json_path = EP.(output_path ~cwd ~forest / "forest.json") in
    Eio_util.ensure_context_of_path ~perm:0o755 json_path;
    EP.save ~create:(`Or_truncate 0o644) json_path json_string
  end;
  let jobs =
    let bare_host_uri = URI.with_path_components [] forest.config.url in
    let home_route =
      URI.with_path_components
        (URI.append_path_component
           (URI.path_components forest.config.url)
           "index.html")
        forest.config.url
    in
    let home_content =
      html_redirect @@ "/"
      ^ URI.relative_path_string ~base:bare_host_uri
          (Config.home_uri forest.config)
    in
    List.cons [(home_route, home_content)]
    @@
    let@ resource =
      Eio.Fiber.List.map ~max_fibers:40 @~ List.of_seq all_resources
    in
    let@ () = Reporter.easy_run in
    outputs_for_resource ~forest resource
  in
  Logs.debug (fun m -> m "Writing %i files to output" (List.length jobs));
  begin
    (* Note: this part appears to be fast! *)
    let@ items = Eio.Fiber.List.iter ~max_fibers:20 @~ jobs in
    let@ (route : URI.t), content = List.iter @~ items in
    let@ () = Reporter.easy_run in
    let path = EP.(cwd / output_dir_name / uri_to_local_path ~forest route) in
    Eio_util.ensure_context_of_path ~perm:0o755 path;
    EP.save ~create:(`Or_truncate 0o644) path content
  end
