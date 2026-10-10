(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

val parse_forest_config_string : string -> Config.t
val parse_forest_config_file : string -> Config.t

val parse_prefix_char : string -> char
(** Validates a [[forest.prefix."<punct>"]] key: exactly one ASCII punctuation
    character that is not vetoed. Fails with [Configuration_error] otherwise. *)
