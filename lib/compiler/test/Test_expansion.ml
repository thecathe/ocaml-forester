(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_test
open Forester_lsp
open Forester_frontend
open Testables

open struct
  module T = Types
end

open struct
  module S = Resolver.Scope
  module P = Resolver.P

  let config = Config.default ()
  let _data = Alcotest.testable Syn.pp_resolver_data ( = )
end

open State.Syntax

let expand ~forest src =
  let@ code = Result.map @~ parse_string_no_loc src in
  S.run ~init_visible:Expand.initial_visible_trie @@ fun () ->
  Expand.expand ~forest code

let render ~forest expanded =
  Result.map
    (fun expanded ->
      let Eval.{articles; _}, _ =
        Eval.eval_tree ~config:(Config.default ())
          ~uri:(URI.of_string_exn "http://localhost/test")
          ~source_path:None expanded
      in
      let () =
        List.iter
          (fun article ->
            let@ uri = Option.iter @~ T.(article.frontmatter.uri) in
            forest.={uri} <-
              Resource
                {
                  resource = Article article;
                  expanded = None;
                  route_locally = true;
                  include_in_manifest = true;
                  redirect_uri = None;
                })
          articles
      in
      let rendered =
        List.map
          (fun article ->
            Plain_text_client.string_of_content ~forest T.(article.mainmatter))
          articles
      in
      String.concat "" rendered)
    expanded

let test_subtree ~env () =
  let@ () = Reporter.easy_run in
  let forest = State.make ~env ~config ~dev:false () in
  let expanded =
    expand ~forest
      {|
  \subtree[foo]{
    \title{Hello}
    \taxon{Example}
  }
  |}
  in
  let evaluated = render ~forest expanded in
  Alcotest.(check @@ result string diagnostic)
    "" (Ok {|<omitted content: Hello>|}) evaluated

let test_visible ~env () =
  let forest = State.make ~env ~config ~dev:false () in
  let code =
    Result.get_ok
    @@ parse_string {|
\def\greet[name]{Hello, \name!}
\p{\greet{Jon}}
    |}
  in
  let result =
    Trie.to_seq
    @@ Analysis.get_visible ~forest ~position:{line = 2; character = 5} code
  in
  let greet =
    let@ path, _ =
      Option.map @~ Seq.find (fun (p, _) -> p = ["greet"]) result
    in
    path
  in
  Alcotest.(check (option path)) "greet is visible" (Some ["greet"]) greet

let () =
  Logs.set_level (Some Debug);
  Logs.set_reporter (Logs.format_reporter ());
  let open Alcotest in
  let@ env = Eio_main.run in
  run "Test_expansion"
    [
      ( "",
        [
          test_case "subtree" `Quick (test_subtree ~env);
          test_case "get_visible" `Quick (test_visible ~env);
        ] );
    ]
