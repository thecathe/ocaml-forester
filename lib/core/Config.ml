(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

type foreign = {path: string; route_locally: bool; include_in_manifest: bool}
[@@deriving show, repr]

(* How a `\meta{<key>}{...}` entry is treated, optionally scoped to a taxon.
   [Redirect]/[Local_redirect] plant the article as non-local, pointing
   inbound links at the meta's target; [External] keeps it a normal article
   and shows the meta as an external link. The config name "self" is an alias
   of [External]. *)
type meta_behaviour = Redirect | Local_redirect | External
[@@deriving show, repr]

type meta_rule = {
  key: string;
  taxon: string option;
  behaviour: meta_behaviour;
}
[@@deriving show, repr]

type tag_behaviour = Hide | Show [@@deriving show, repr]

type tag_rule = {
  tag: string;
  tag_behaviour: tag_behaviour;
  page: bool;
}
[@@deriving show, repr]

(* A single punctuation character that, as the first character of a tree
   address, marks a class of trees and names the taxon they carry. *)
type prefix_rule = {prefix: char; prefix_taxon: string}
[@@deriving show, repr]

(* Every tree under [dir] (a path relative to the config file, recursively)
   is given [dir_tags] as if each had written `\tag{...}` itself. *)
type directory_rule = {dir: string; dir_tags: string list}
[@@deriving show, repr]

type t = {
  trees: string list;
  assets: string list;
  foreign: foreign list;
  url: URI.t;
  home: URI.t;
  meta_rules: meta_rule list;
  tag_rules: tag_rule list;
  prefix_rules: prefix_rule list;
  directory_rules: directory_rule list;
  new_authors: string list;
}
[@@deriving show, repr]

let default_meta_rules =
  [
    {key = "redirect"; taxon = None; behaviour = Redirect};
    {key = "external"; taxon = None; behaviour = External};
  ]

let default_url = URI.of_string_exn "http://forest.local/"

let default ?(url = default_url) () : t =
  {
    trees = ["trees"];
    assets = [];
    foreign = [];
    url;
    home = URI_scheme.named_uri ~base:url "index";
    meta_rules = default_meta_rules;
    tag_rules = [];
    prefix_rules = [];
    directory_rules = [];
    new_authors = [];
  }

let home_uri config = config.home

(* Address of the generated page for a tag (see the tag-page generator in the
   forest's scripts, which must use the same rule): "tag-" followed by the
   tag lowercased with runs of non-alphanumerics collapsed to "-". *)
let tag_page_address tag =
  let buf = Buffer.create (String.length tag) in
  let pending_dash = ref false in
  String.iter
    (fun c ->
      match c with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' ->
        if !pending_dash && Buffer.length buf > 0 then Buffer.add_char buf '-';
        pending_dash := false;
        Buffer.add_char buf (Char.lowercase_ascii c)
      | _ -> pending_dash := true)
    tag;
  "tag-" ^ if Buffer.length buf = 0 then "tag" else Buffer.contents buf
