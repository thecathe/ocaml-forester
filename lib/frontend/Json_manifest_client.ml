(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_compiler
open Forester_core

open struct
  module T = Types
  module PT = Plain_text_client
end

let render_tree ~dev ~(forest : State.t) (doc : T.content T.article) :
    Yojson.Safe.t option =
  let@ uri = Option.bind doc.frontmatter.uri in
  (* TODO : Check routing *)
  let route = Legacy_xml_client.route forest uri in
  let title_string =
    PT.string_of_content ~forest
    @@ State.get_expanded_title doc.frontmatter forest
  in
  let title = `String title_string in
  let taxon =
    match doc.frontmatter.taxon with
    | None -> `Null
    | Some content -> `String (PT.string_of_content ~forest content)
  in
  let tags =
    `List
      begin
        let@ tag = List.filter_map @~ doc.frontmatter.tags in
        let@ content =
          Option.map @~ State.get_title_or_content_of_vertex tag forest
        in
        `String (PT.string_of_content ~forest content)
      end
  in
  let route = `String (URI.href_string route) in
  let metas =
    let meta_string meta = String.trim @@ PT.string_of_content ~forest meta in
    let meta_assoc (s, meta) = (s, `String (meta_string meta)) in
    `Assoc (List.map meta_assoc doc.frontmatter.metas)
  in
  let path =
    if dev then
      match doc.frontmatter.source_path with
      | Some p -> [("sourcePath", `String p)]
      | None -> []
    else []
  in
  (* TODO: filter out anonymous stuff *)
  Option.some
  @@
  let fm =
    path
    @ [
        ("title", title);
        ("uri", `String (URI.display_path_string ~base:forest.config.url uri));
        ("taxon", taxon);
        ("tags", tags);
        ("route", route);
        ("metas", metas);
      ]
  in
  `Assoc fm
