(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

type foreign = {path: string; route_locally: bool; include_in_manifest: bool}
[@@deriving show]

type meta_behaviour = Redirect | Local_redirect | External
[@@deriving show]

type meta_rule = {
  key: string;
  taxon: string option;
  behaviour: meta_behaviour;
}
[@@deriving show]

type tag_behaviour = Hide | Show [@@deriving show]

type tag_rule = {
  tag: string;
  tag_behaviour: tag_behaviour;
  page: bool;
}
[@@deriving show]

type prefix_rule = {prefix: char; prefix_taxon: string} [@@deriving show]

type directory_rule = {dir: string; dir_tags: string list} [@@deriving show]

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
      (** Addresses written as `\author{...}` under the date of every tree
          created by `forester new`. *)
}
[@@deriving show]

val default_meta_rules : meta_rule list
(** The built-in rules: [redirect] ↦ [Redirect], [external] ↦ [External]. *)

val default_url : URI.t
val default : ?url:URI.t -> unit -> t
val home_uri : t -> URI.t

val tag_page_address : string -> string
(** [tag_page_address "must-preorder"] is ["tag-must-preorder"]: the address of
    the generated page for a tag with [page = true]. *)
