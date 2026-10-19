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
  display: bool;
  title: string option;
  label: string option;
}
[@@deriving show]

type prefix_rule = {prefix: char; prefix_taxon: string} [@@deriving show]

type directory_rule = {dir: string; dir_tags: string list} [@@deriving show]

type taxon_rule = {
  taxon_name: string;
  taxon_page: bool;
  taxon_unpublished: bool;
      (** [unpublished = true]: trees of this taxon are not built unless their
          address is in [taxon_publish]; a redirect meta still wins. *)
  taxon_publish: string list;  (** addresses exempt from [unpublished] *)
}
[@@deriving show]

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
  taxon_rules: taxon_rule list;
  publish_tags: string list;
      (** [[forest.publish] tags = [...]]: when non-empty, only trees carrying
          one of these tags are published; every other tree is unpublished
          (as by a taxon's [unpublished] rule). A publish tag also overrides
          its taxon's [unpublished] rule. *)
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

val is_unpublished :
  t -> taxon:string option -> address:string -> tags:string list -> bool
(** Whether a tree of [taxon] at [address] (e.g. ["@jpears"]) with [tags]
    (directory tags included) is kept out of the output: by [publish_tags],
    when set, unless it carries one; otherwise by its taxon's [unpublished]
    rule, unless its address is in that rule's [publish] list or it carries
    a publish tag. *)

val is_displayed_tag : t -> string -> bool
(** Whether a tag is shown in page headers: false for [display = false]. *)

val tag_label : t -> string -> string
(** The text a tag is shown as in page headers: its [label], else its [title]
    (which the page generator also uses as its page's title), else the tag
    itself. *)

val taxon_page_address : string -> string
(** [taxon_page_address "Person"] is ["taxon-person"]: the address of the
    generated page for a taxon. *)
