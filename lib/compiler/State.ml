(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Tree
open Forester_core

open struct
  module T = Types
end

type resource = T.content T.resource

type t = {
  env: Eio_unix.Stdenv.base;
  dev: bool;
  config: Config.t;
  index: Tree.t URI.Tbl.t;
  diagnostics: Reporter.Message.t Asai.Diagnostic.t list URI.Tbl.t;
  graphs: (module Forest_graphs.S);
  import_graph: Forest_graph.t;
  dependency_cache: Cache.t;
  resolver: string URI.Tbl.t;
  search_index: Forester_search.Index.t;
  usages: (Tree.exports, URI.t Asai.Range.located) Hashtbl.t;
  history: Action.t list;
  hosts: (string, unit) Hashtbl.t;
  suggestions: URI.t URI.Tbl.t;
}

let make ~(env : Eio_unix.Stdenv.base) ~(config : Config.t) ~(dev : bool)
    ?(graphs = (module Forest_graphs.Make () : Forest_graphs.S))
    ?(import_graph = Forest_graph.create ~size:1000 ())
    ?(resolver = URI.Tbl.create 1000) ?(index = URI.Tbl.create 1000)
    ?(diagnostics = URI.Tbl.create 1000) ?(usages = Hashtbl.create 1000)
    ?(search_index = Forester_search.Index.create [])
    ?(dependency_cache = Cache.empty) ?(hosts = Hashtbl.create 10)
    ?(suggestions = URI.Tbl.create 1000) () =
  {
    env;
    dev;
    config;
    index;
    diagnostics;
    resolver;
    import_graph;
    graphs;
    search_index;
    dependency_cache;
    usages;
    hosts;
    suggestions;
    history = [];
  }

module Syntax = struct
  let ( .={} ) state uri = URI.Tbl.find_opt state.index uri

  let ( .={}<- ) state uri tree =
    match state.={uri} with
    | None -> URI.Tbl.replace state.index uri tree
    | Some existing ->
      let o1 = Tree.origin tree in
      let o2 = Tree.origin existing in
      if o1 <> o2 then begin
        Reporter.emit (Duplicate_tree (o1, o2));
        URI.Tbl.replace state.index uri tree
      end
      else URI.Tbl.replace state.index uri tree
  (* URI.Tbl.replace state.index uri item *)

  (* / for units*)
  let ( ./{} ) state uri =
    Option.bind (URI.Tbl.find_opt state.index uri) Tree.get_units

  (* updating units*)
  let ( ./{}<- ) state uri units =
    let@ () = Reporter.tracef "when updating units for %a" URI.pp uri in
    match URI.Tbl.find_opt state.index uri with
    | None ->
      Reporter.fatal Internal_error
        ~extra_remarks:
          [Asai.Diagnostic.loctextf "Updating units: %a not found" URI.pp uri]
    | Some (Document _) | Some (Parsed _) ->
      Reporter.fatal Internal_error
        ~extra_remarks:
          [Asai.Diagnostic.loctextf "%a has not been expanded yet" URI.pp uri]
    | Some (Expanded expanded) ->
      URI.Tbl.replace state.index uri (Expanded {expanded with units})
    | Some (Resource _) -> ()

  (* ? for diagnostics*)
  let ( .?{} ) state uri =
    Option.value ~default:[] (URI.Tbl.find_opt state.diagnostics uri)

  let ( .?{}<- ) state uri diagnostics =
    URI.Tbl.add state.diagnostics uri diagnostics

  (* @ for article/resource *)
  let ( .@{} ) state uri =
    match URI.Tbl.find_opt state.index uri with
    | Some (Document _) -> None
    | Some (Parsed _) | Some (Expanded _) | None -> None
    | Some (Resource res) -> Some res.resource
end

open Syntax

let update_history forest action =
  {forest with history = action :: forest.history}

let find_opt state uri = URI.Tbl.find_opt state.index uri
let to_seq state = URI.Tbl.to_seq state.index

let get_all_unparsed state =
  state.index |> URI.Tbl.to_seq_values |> Seq.filter is_unparsed

let get_all_code state =
  state.index |> URI.Tbl.to_seq_values |> Seq.filter_map to_code

let get_all_unexpanded state =
  state.index |> URI.Tbl.to_seq_values |> Seq.filter is_unexpanded

let get_all_expanded state =
  state.index |> URI.Tbl.to_seq_values |> Seq.filter_map to_syn

let get_all_unevaluated state =
  state.index |> URI.Tbl.to_seq_values |> Seq.filter is_unevaluated

let get_all_articles : t -> T.content T.article Seq.t =
 fun state -> state.index |> URI.Tbl.to_seq_values |> Seq.filter_map to_article

let get_all_evaluated : t -> evaluated Seq.t =
 fun state ->
  state.index |> URI.Tbl.to_seq_values |> Seq.filter_map to_evaluated

let get_all_resources : t -> T.content T.resource Seq.t =
 fun state -> state.index |> URI.Tbl.to_seq_values |> Seq.filter_map to_resource

let get_resource state uri =
  match state.={uri} with None -> None | Some tree -> to_resource tree

let get_code state uri =
  match state.={uri} with None -> None | Some tree -> to_code tree

let get_article : URI.t -> t -> T.content T.article option =
 fun uri forest ->
  match URI.Tbl.find_opt forest.index uri with
  | None | Some (Document _) | Some (Parsed _) | Some (Expanded _) -> None
  | Some (Resource {resource; _}) -> (
    match resource with T.Article a -> Some a | _ -> None)

let section_symbol = "§"

let rec get_expanded_title ?scope ?(flags = T.{empty_when_untitled = false})
    (frontmatter : _ T.frontmatter) forest =
  let short_title =
    match frontmatter.title with
    | Some content -> content
    | None when not flags.empty_when_untitled -> begin
      match frontmatter.uri with
      | Some uri -> T.Content [T.Uri uri]
      | _ -> T.Content [T.Text "Untitled"]
    end
    | _ -> T.Content []
  in
  Option.value ~default:short_title
  @@
  match frontmatter.designated_parent with
  | Some parent_uri
    when not (Option.equal URI.equal scope frontmatter.designated_parent) ->
    let@ parent = Option.map @~ get_article parent_uri forest in
    let parent_title = get_expanded_title parent.frontmatter forest in
    let parent_link = T.Link {href = parent_uri; content = parent_title} in
    let chevron = T.Text " › " in
    T.map_content (fun xs -> parent_link :: chevron :: xs) short_title
  | _ -> None

let get_content_of_transclusion (transclusion : T.transclusion) forest =
  match transclusion.target with
  | Full flags ->
    let@ article = Option.map @~ get_article transclusion.href forest in
    T.Content [T.Section (T.article_to_section article ~flags)]
  | Mainmatter ->
    let@ article = Option.map @~ get_article transclusion.href forest in
    article.mainmatter
  | Title flags ->
    Option.some
    @@ begin match get_article transclusion.href forest with
    | None -> T.Content [T.Uri transclusion.href]
    | Some article -> get_expanded_title ~flags article.frontmatter forest
    end
  | Taxon ->
    let@ article = Option.map @~ get_article transclusion.href forest in
    let default = T.Content [T.Text section_symbol] in
    Option.value ~default article.frontmatter.taxon

let get_title_or_content_of_vertex ?(not_found = fun _ -> None) vertex forest =
  match vertex with
  | T.Content_vertex content -> Some content
  | T.Uri_vertex uri -> begin
    match get_article uri forest with
    | Some article -> article.frontmatter.title
    | None -> not_found uri
  end

(* A list of mistakes that a user might make when typing a given URI. For
   example, they might type "https://www.forester-notes.com/005P" instead of
   "https://www.forester-notes.com/005P/".
*)
let wrong_variants_for_uri uri =
  let components = URI.path_components uri in
  match List.rev components with
  | "" :: rest ->
    [
      URI.with_path_components (List.rev rest) uri;
      URI.with_path_components (components @ ["index.html"]) uri;
      URI.with_path_components (components @ ["index.xml"]) uri;
    ]
  | _ -> []

type uri_suggestion = Ok | Not_found of {suggestion: URI.t option}

let suggestion_for_uri uri forest =
  match URI.host uri with
  | None -> Ok
  | Some host -> (
    match Hashtbl.find_opt forest.hosts host with
    | None -> Ok
    | Some () -> (
      match URI.Tbl.find_opt forest.index uri with
      | Some _ -> Ok
      | None -> Not_found {suggestion = URI.Tbl.find_opt forest.suggestions uri}
      ))

(* The reserved `\meta{redirect}{<url>}` key: when present on a locally
   authored article, the article is planted as if it were a foreign,
   non-routed resource whose canonical URI is the given external target --
   see `plant_article` below. We deliberately do NOT overwrite
   `frontmatter.uri` with the redirect target: that field is used both as
   this resource's index key (`T.uri_for_resource`) and as the scope for all
   of its own outbound Datalog facts (see `Forest.analyse_resource`), so
   mutating it would file the tree under the wrong key and corrupt the
   link-graph's scoping. `redirect_uri` is therefore a separate field on
   `Tree.evaluated`, consulted only by routing/rendering. *)
let redirect_meta_key = "redirect"

let text_of_simple_content (content : T.content) : string option =
  let rec loop acc = function
    | [] -> Some (String.concat "" (List.rev acc))
    | (T.Text txt | T.CDATA txt) :: rest -> loop (txt :: acc) rest
    | T.Uri uri :: rest -> loop (URI.to_string uri :: acc) rest
    | _ -> None
  in
  loop [] (T.extract_content content)

let redirect_uri_of_frontmatter (fm : T.content T.frontmatter) : URI.t option
    =
  match List.assoc_opt redirect_meta_key fm.metas with
  | None -> None
  | Some content -> (
    match text_of_simple_content content with
    | None ->
      Reporter.fatal Invalid_URI
        ~extra_remarks:
          [
            Asai.Diagnostic.loctext
              "the `redirect` meta must be a simple URL string, not rich \
               content";
          ]
    | Some str -> (
      match URI.of_string_exn str with
      | uri -> Some uri
      | exception _ ->
        Reporter.fatal Invalid_URI
          ~extra_remarks:
            [
              Asai.Diagnostic.loctextf
                "the `redirect` meta is not a valid URL: %s" str;
            ]))

let plant_resource ?(route_locally = true) ?(include_in_manifest = true)
    ?(redirect_uri = None) resource forest =
  let module Graphs = (val forest.graphs) in
  Forest.analyse_resource forest.graphs resource;
  let@ uri = Option.iter @~ T.uri_for_resource resource in
  let uri = URI.canonicalise uri in
  (* Seems dodgy if this isn't already canonical! *)
  Graphs.register_uri uri;
  begin
    let@ host = Option.iter @~ URI.host uri in
    Hashtbl.add forest.hosts host ()
  end;
  begin
    let@ wrong_variant = List.iter @~ wrong_variants_for_uri uri in
    URI.Tbl.add forest.suggestions wrong_variant uri
  end;
  match forest.={uri} with
  | None ->
    forest.={uri} <-
      Resource
        {
          resource;
          expanded = None;
          route_locally;
          include_in_manifest;
          redirect_uri;
        }
  | Some (Tree.Expanded syn) ->
    forest.={uri} <-
      Resource
        {
          resource;
          expanded = Some syn;
          route_locally;
          include_in_manifest;
          redirect_uri;
        }
  | _ ->
    forest.={uri} <-
      Resource
        {
          resource;
          expanded = None;
          route_locally;
          include_in_manifest;
          redirect_uri;
        }

(* Entry point for planting a locally-authored article (as opposed to a
   foreign/imported resource, which goes through `plant_resource` directly
   with its own `route_locally`/`include_in_manifest` config). Checks for a
   `redirect` meta and, if present, plants the article as fully non-local
   and manifest-excluded, with its redirect target recorded for routing. *)
let plant_article (article : T.content T.article) (forest : t) : unit =
  match redirect_uri_of_frontmatter article.frontmatter with
  | None -> plant_resource (T.Article article) forest
  | Some target ->
    plant_resource ~route_locally:false ~include_in_manifest:false
      ~redirect_uri:(Some target) (T.Article article) forest

let serialize_graphs : (module Forest_graphs.S) -> 'a =
 fun s ->
  let module Graphs = (val s) in
  Graphs.dl_db

let batch_write : t -> _ = function
  | {import_graph; _} ->
    (* let dl_db = serialize_graphs graphs in *)
    let open Cache in
    let module Gmap = Forest_graph.Map (Cache.Dependecy_graph) in
    let tbl = Dependency_tbl.create 100 in
    let now = Unix.time () in
    let g =
      import_graph
      |> Gmap.map @@ function
         | T.Content_vertex _ ->
           (*Import graph has no content vertices*)
           assert false
         | T.Uri_vertex uri ->
           let item = Item.Tree uri in
           Dependency_tbl.add tbl item
             Item.{timestamp = Some now; color = Green};
           item
    in
    {Cache.empty with graph = g; tbl}

let reconstruct =
 fun ~env:_ ~(_config : Config.t) paths cache ->
  match cache with
  | {search_index = _; _} ->
    (* let init = Phases.init ~env ~config ~dev: true in *)
    (* let graphs = Forest_graphs.init dl_db in *)
    paths
    |> Seq.iter (fun _path ->
        (* let uri = URI_scheme.path_to_uri ~base: config.url (Eio.Path.native_exn path) in *)
        (* match URI.Tbl.find_opt forest uri with *)
        (* | None -> () *)
        (* | Some tree -> *)
        (*   match check_timestamp path tree.timestamp with *)
        (*   | _ -> () *)
        ())

let rec lsp_uri_of_uri (uri : URI.t) (forest : t) : Lsp.Uri.t option =
  let@ tree = Option.bind @@ find_opt forest uri in
  lsp_uri_of_tree tree forest

and lsp_uri_of_tree (tree : Tree.t) (forest : t) : Lsp.Uri.t option =
  match tree with
  | Document doc -> Some (Lsp.Text_document.documentUri doc)
  | Parsed code -> lsp_uri_of_origin code.origin forest
  | Expanded syn -> lsp_uri_of_identity syn.identity forest
  | Resource evaluated -> lsp_uri_of_resource evaluated.resource forest

and lsp_uri_of_resource (resource : resource) (forest : t) : Lsp.Uri.t option =
  match resource with
  | Article article ->
    Option.map Lsp.Uri.of_string article.frontmatter.source_path
  | Asset _ | Syndication _ -> None

and lsp_uri_of_origin (origin : origin) (forest : t) : Lsp.Uri.t option =
  match origin with
  | Physical document -> Some (Lsp.Text_document.documentUri document)
  | Subtree {parent} -> lsp_uri_of_identity parent forest
  | Undefined -> None

and lsp_uri_of_identity (identity : identity) (forest : t) : Lsp.Uri.t option =
  match identity with URI uri -> lsp_uri_of_uri uri forest | Anonymous -> None

let source_path_of_uri uri forest =
  Option.map Lsp.Uri.to_path @@ lsp_uri_of_uri uri forest
