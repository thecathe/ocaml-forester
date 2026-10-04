(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_test
open Testables
open State.Syntax

open struct
  module T = Types
end

let config = Config.default ()

let raw_trees =
  let t1 = {path = "t1.tree"; content = {||}} in
  let t2 = {path = "t2.tree"; content = {||}} in
  let t3 = {path = "t3.tree"; content = {||}} in
  let t4 = {path = "t4.tree"; content = {||}} in
  let t5 = {path = "t5.tree"; content = {||}} in
  let t6 = {path = "t6.tree"; content = {||}} in
  let t7 = {path = "t7.tree"; content = {||}} in
  let t8 = {path = "t8.tree"; content = {||}} in
  [t1; t2; t3; t4; t5; t6; t7; t8]

let test_batch_run ~env () =
  let forest, history =
    let@ path = with_test_forest ~raw_trees ~env ~config in
    Sys.chdir (Eio.Path.native_exn path);
    let@ () = Reporter.easy_run in
    let forest = State.make ~env ~config ~dev:false () in
    Driver.run_with_history Load_all_configured_dirs forest
  in
  Alcotest.(check @@ list action)
    "all actions have run"
    [
      Load_all_configured_dirs;
      Parse_all;
      Build_import_graph;
      Expand_all;
      Eval_all;
      Run_jobs [];
      Done;
    ]
    history;
  Alcotest.(check @@ int)
    "no tree is unparsed" 0
    (Seq.length (State.get_all_unparsed forest));
  Alcotest.(check @@ int)
    "no tree is unexpanded" 0
    (Seq.length (State.get_all_unexpanded forest));
  Alcotest.(check @@ int)
    "no tree is unevaluated" 0
    (Seq.length (State.get_all_unevaluated forest));
  Alcotest.(check @@ int)
    "has correct number of articles" 8
    (Seq.length (State.get_all_articles forest))

let test_includes_paths ~env () =
  let@ () = Reporter.easy_run in
  let config = Config.default () in
  with_test_forest ~raw_trees ~env ~config (fun path ->
      Sys.chdir (Eio.Path.native_exn path);
      let@ () = Reporter.easy_run in
      let forest, history =
        State.make ~env ~config ~dev:true ()
        |> Driver.run_with_history Load_all_configured_dirs
      in
      Alcotest.(check int)
        "number of parsed trees" 8
        (URI.Tbl.length forest.index);
      Alcotest.(check int)
        "number of trees in resolver" 8
        (URI.Tbl.length forest.resolver);
      Alcotest.(check @@ list action)
        "evaluation succeeded"
        [
          Load_all_configured_dirs;
          Parse_all;
          Build_import_graph;
          Expand_all;
          Eval_all;
          Run_jobs [];
          Done;
        ]
        history;
      let uri = URI.of_string_exn "http://forest.local/t8/" in
      let path =
        match forest.@{uri} with
        | Some (Article {frontmatter = {source_path; _}; _}) -> source_path
        | Some _ -> Alcotest.fail "not an article"
        | None ->
          URI.Tbl.iter
            (fun uri _ -> Logs.debug (fun m -> m "%a" URI.pp uri))
            forest.index;
          Alcotest.fail "not found"
      in
      Alcotest.(check bool) "path is some" true (Option.is_some path))

let test_reparsing ~env () =
  let config = Config.default () in
  let@ tmp_path = with_test_forest ~raw_trees ~env ~config in
  Logs.app (fun m ->
      m "In temp dir %s" (Unix.realpath @@ Eio.Path.native_exn tmp_path));
  let@ () = Reporter.easy_run in
  let forest =
    State.make ~env ~config ~dev:false ()
    |> Driver.run_until_done Load_all_configured_dirs
  in
  let reparse_addr = "t8.tree" in
  let reparse_uri = URI_scheme.path_to_uri ~base:config.url reparse_addr in
  let vtx = T.Uri_vertex reparse_uri in
  Alcotest.(check int)
    "Number of vertices before reparsing" 8
    (Forest_graph.nb_vertex forest.import_graph);
  Alcotest.(check int)
    "old vertex has no import" 0
    (Forest_graph.in_degree forest.import_graph vtx);
  let _, path =
    Option.get
    @@ Seq.find_map
         (fun (uri, path) ->
           if String.ends_with ~suffix:"t8.tree" path then begin
             Logs.debug (fun m -> m "%s" path);
             Some (uri, Eio.Path.(forest.env#fs / path))
           end
           else None)
         (URI.Tbl.to_seq forest.resolver)
  in
  Eio.Path.save ~create:(`Or_truncate 0o644) path {|\import{t1}|};
  let reparsed = Driver.run_until_done (Load_tree path) forest in
  Alcotest.(check bool)
    "vertex has an import" true
    (Forest_graph.in_degree reparsed.import_graph vtx > 0)

let test_omits_paths ~env () =
  let@ () = Reporter.easy_run in
  let forest = Driver.batch_run ~env ~config ~dev:false () in
  let path =
    match forest.@{URI.of_string_exn "http://forest.local/t8/"} with
    | Some (Article {frontmatter = {source_path; _}; _}) -> source_path
    | Some _ -> Alcotest.fail "not an article"
    | None ->
      URI.Tbl.iter
        (fun uri _ -> Logs.debug (fun m -> m "%a" URI.pp uri))
        forest.index;
      Alcotest.fail "not found"
  in
  Alcotest.(check bool) "" true @@ Option.is_none path

let () =
  let@ env = Eio_main.run in
  Logs.set_level (Some Debug);
  Logs.set_reporter (Logs.format_reporter ());
  let open Alcotest in
  run "Test_driver"
    [
      ( "Steps",
        [
          test_case "Batch compilation steps" `Quick (test_batch_run ~env);
          test_case "reparsing" `Quick (test_reparsing ~env);
        ] );
      ( "dev mode",
        [
          test_case "includes paths in dev mode" `Quick
            (test_includes_paths ~env);
          test_case "omits paths outside dev mode" `Quick
            (test_omits_paths ~env);
        ] );
    ]
