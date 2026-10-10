(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

type foreign = {path: string; route_locally: bool; include_in_manifest: bool}
[@@deriving show, repr]

(* How a `\meta{<key>}{...}` entry is treated, optionally scoped to a taxon.
   [Redirect]/[Local_redirect] plant the article as non-local, pointing
   inbound links at the meta's target; [External]/[Self] keep it a normal
   article, with [Self] additionally suppressing the meta's own annotation. *)
type meta_behaviour = Redirect | Local_redirect | External | Self
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

type t = {
  trees: string list;
  assets: string list;
  foreign: foreign list;
  url: URI.t;
  home: URI.t;
  meta_rules: meta_rule list;
  tag_rules: tag_rule list;
  prefix_rules: prefix_rule list;
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
  }

let home_uri config = config.home
