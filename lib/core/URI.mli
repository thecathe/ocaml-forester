(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

type t

val host : t -> string option
val scheme : t -> string option
val path_string : t -> string
val stripped_path_components : t -> string list
val append_path_component : string list -> string -> string list
val path_components : t -> string list
val with_path_components : string list -> t -> t
val canonicalise : t -> t
val relative_path_string : base:t -> t -> string
val display_path_string : base:t -> t -> string
val resolve : base:t -> t -> t
val equal : t -> t -> bool
val compare : t -> t -> int

val make :
  ?scheme:string ->
  ?user:string ->
  ?host:string ->
  ?port:int ->
  ?path:string list ->
  unit ->
  t

val t : t Repr.t
val pp : Format.formatter -> t -> unit
val to_string : t -> string

val href_string : t -> string
(** Like {!to_string}, but keeps percent-encoding; use when the result is
    emitted as a URL (href) rather than displayed. *)
val of_string_exn : string -> t

module Set : Set.S with type elt = t
module Map : Map.S with type key = t
module Tbl : Hashtbl.S with type key = t
