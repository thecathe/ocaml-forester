(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

(* TODO: should just use cram tests for this instead. *)

open Forester_core
open Forester_prelude
open Forester_compiler
open Forester_frontend

open struct
  module T = Types
  module HTML = Pure_html.HTML
end

let config = {(Config.default ()) with trees = ["transclude"]}
let href = URI_scheme.named_uri ~base:config.url "transcludee"

module Transclusions = struct
  (* It would be cool to use quickcheck here, but no good way to test the result*)
  open T

  let full_default = {href; target = Full default_section_flags}
  let metadata_shown = {default_section_flags with metadata_shown = Some true}
end

let () =
  let@ env = Eio_main.run in
  Logs.set_level (Some Debug);
  let@ () = Reporter.easy_run in
  let uri = URI_scheme.named_uri ~base:config.url "transcludee" in
  let index = URI.Tbl.create 10 in
  URI.Tbl.add index uri
  @@ Tree.Resource
       {
         resource =
           T.Article
             {
               frontmatter =
                 T.default_frontmatter
                   ~uri:(URI.of_string_exn "forest://test/transcludee")
                   ~title:(T.Content [Text "I am being transcluded"]) ();
               mainmatter = Content [Text "Hello"];
               backmatter = Content [];
             };
         expanded = None;
         route_locally = true;
         include_in_manifest = true;
         redirect_uri = None;
         hidden = false;
       };
  let forest = {(State.make ~env ~config ~dev:false ()) with index} in
  let print_transclusion : T.transclusion -> unit =
   fun t ->
    let content = Option.get @@ State.get_content_of_transclusion t forest in
    Format.printf "%a"
      (Legacy_xml_client.pp_xml ~forest ?stylesheet:None)
      T.
        {
          frontmatter = default_frontmatter ~uri:href ();
          mainmatter = content;
          backmatter = Content [];
        }
  in
  let test_full_default () = print_transclusion Transclusions.full_default in
  let test_title_default () =
    print_transclusion
      {href = uri; target = Title {empty_when_untitled = false}}
  in
  let test_full_metadata () =
    print_transclusion {href = uri; target = Full Transclusions.metadata_shown}
  in
  List.iter
    (fun f -> f ())
    [test_full_default; test_title_default; test_full_metadata]
