(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open struct
  module T = Types
  module R = Resolver
  include Base
end

type exports = (R.P.data, Asai.Range.t option) Trie.t

type code = {
  nodes: Code.t;
  identity: identity;
  origin: origin;
  (* document: Lsp.Text_document.t; [@opaque] *)
  timestamp: float option;
}
[@@deriving show]

type syn = {
  nodes: Syn.t;
  code: code;
  identity: identity;
  units: exports; [@opaque]
}
[@@deriving show]

type evaluated = {
  resource: T.content T.resource;
  route_locally: bool;
  include_in_manifest: bool;
  redirect_uri: URI.t option;
  hidden: bool;
  unpublished: bool;
      (* kept out of the output by its taxon's `unpublished` rule: links to
         it render as their text alone *)
  expanded: syn option;
}
[@@deriving show]

type t =
  | Document of (Lsp.Text_document.t[@opaque])
  | Parsed of code
  | Expanded of syn
  | Resource of evaluated
[@@deriving show]

let origin = function
  | Document doc -> Physical doc
  | Parsed parsed -> parsed.origin
  | Expanded expanded -> expanded.code.origin
  | Resource resource -> (
    match resource.expanded with
    | None -> Undefined
    | Some expanded -> expanded.code.origin)

let show_phase = function
  | Document _ -> "document"
  | Parsed _ -> "parsed"
  | Expanded _ -> "expanded"
  | Resource _ -> "resource"

(* let get_uri ~base = fun t ->
  let of_lsp_uri doc = Some (URI_scheme.lsp_uri_to_uri ~base (Lsp.Text_document.documentUri doc)) in
  let uri_opt =
    match t with
    | Document doc -> of_lsp_uri doc
    | Resource {tree; _} -> T.uri_for_resource tree
    | Expanded {identity; _}
    | Parsed {identity; _} ->
      identity_to_uri identity
  in
  match uri_opt with
  | Some uri -> uri
  | None -> Reporter.fatal Internal_error ~extra_remarks: [Asai.Diagnostic.loctext "tried to get URI of an anonymous resource"] *)

(* IDK if subtrees should resolve to their parent document*)
let to_doc : t -> Lsp.Text_document.t option = function
  | Document doc -> Some doc
  | Resource {expanded; _} -> begin
    match expanded with
    | None -> None
    | Some {code; _} -> (
      match code.origin with
      | Physical doc -> Some doc
      | Subtree _ -> None
      | Undefined -> None)
  end
  | Parsed {origin; _} | Expanded {code = {origin; _}; _} -> (
    match origin with
    | Physical doc -> Some doc
    | Subtree _ -> None
    | Undefined -> None)

let to_resource : t -> T.content T.resource option = function
  | Document _ | Parsed _ | Expanded _ -> None
  | Resource {resource; _} -> Some resource

let to_evaluated : t -> evaluated option = function
  | Document _ | Parsed _ | Expanded _ -> None
  | Resource evaluated -> Some evaluated

let to_article : t -> T.content T.article option = function
  | Document _ | Parsed _ | Expanded _ -> None
  | Resource {resource; _} -> (
    match resource with T.Article a -> Some a | _ -> None)

let get_frontmatter : t -> T.content T.frontmatter option = function
  | Resource {resource = Types.Article {frontmatter; _}; _} -> Some frontmatter
  | _ -> None

let to_code : t -> code option = function
  | Document _doc ->
    (* Logs.debug (fun m -> m "tried to get code of unparsed document %s" (Lsp.Uri.to_string @@ Lsp.Text_document.documentUri doc)); *)
    (* assert false *)
    None
  | Parsed code -> Some code
  | Resource {expanded; _} -> begin
    match expanded with None -> None | Some {code; _} -> Some code
  end
  | Expanded {code; _} -> Some code

let to_syn : t -> syn option = function
  | Document _ -> None
  | Parsed _ -> None
  | Expanded syn -> Some syn
  | Resource {expanded; _} -> expanded

let get_units : t -> exports option =
 fun item ->
  match item with
  | Document _ -> None
  | Parsed _ -> None
  | Expanded {units; _} -> Some units
  | Resource {expanded; _} -> (
    match expanded with Some {units; _} -> Some units | None -> None)

let is_unparsed = function Document _ -> true | _ -> false
let is_parsed t = not @@ is_unparsed t

let is_unexpanded = function
  | Document _ | Parsed _ -> true
  | Expanded _ | Resource _ -> false

let is_expanded : t -> bool = function
  | Document _ | Parsed _ -> false
  | Expanded _ -> true
  | Resource {expanded; _} -> Option.is_some expanded

let is_unevaluated = function
  | Document _ | Parsed _ | Expanded _ -> true
  | Resource _ -> false

let is_asset = function
  | Document _ | Parsed _ | Expanded _ -> false
  | Resource {resource; _} -> (
    match resource with T.Asset _ -> true | _ -> false)

let update_units : t -> exports -> t =
 fun item units ->
  match item with
  | Document _ | Parsed _ ->
    Reporter.fatal Internal_error
      ~extra_remarks:
        [
          Asai.Diagnostic.loctext
            "can't update units for this item. It has not been expanded yet";
        ]
  | Expanded e -> Expanded {e with units}
  | Resource ({expanded; _} as e) -> (
    match expanded with
    | None ->
      Reporter.fatal Internal_error
        ~extra_remarks:
          [
            Asai.Diagnostic.loctext
              "can't update units for this item. It is not a tree.";
          ]
    | Some expanded -> Resource {e with expanded = Some {expanded with units}})
