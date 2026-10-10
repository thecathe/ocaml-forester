(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_test
open Testables
open Forester_core
open Forester_frontend

let test_parsing () =
  Alcotest.(check config)
    "is the same"
    Config.
      {
        trees = ["trees"];
        assets = [];
        url = URI.of_string_exn "https://www.forester-notes.org/";
        home = URI.of_string_exn "https://www.forester-notes.org/index/";
        foreign =
          [
            {
              path = "foreign/forest.json";
              route_locally = true;
              include_in_manifest = true;
            };
          ];
        meta_rules = default_meta_rules;
        tag_rules = [];
        prefix_rules = [];
        directory_rules = [];
        new_authors = [];
        taxon_rules = [];
      }
    begin
      Forester_core.Reporter.easy_run @@ fun () ->
      Config_parser.parse_forest_config_string
        {|
        [forest]
        trees = ["trees"]
        foreign = [{path = "foreign/forest.json"}]
        url = "https://www.forester-notes.org/"
        home = "index"
        |}
    end

let test_missing_fields () =
  Alcotest.(check config)
    "is the same"
    Config.
      {
        trees = ["trees"];
        assets = [];
        foreign = [];
        url = URI.of_string_exn "/";
        home = URI.of_string_exn "/index/";
        meta_rules = default_meta_rules;
        tag_rules = [];
        prefix_rules = [];
        directory_rules = [];
        new_authors = [];
        taxon_rules = [];
      }
    ( Forester_core.Reporter.easy_run @@ fun () ->
      Config_parser.parse_forest_config_string
        {|
        [forest]
        trees = ["trees"]
        url = "/"
        |}
    )

let test_schema () =
  let c =
    Forester_core.Reporter.easy_run @@ fun () ->
    Config_parser.parse_forest_config_string
      {|
      [forest]
      trees = ["trees"]
      url = "/"

      [forest.meta.redirect.Person]
      behaviour = "external"

      [forest.meta.redirect]
      behaviour = "local-redirect"

      [forest.meta.homepage]
      behaviour = "self"

      [forest.tag.private]
      behaviour = "hide"

      [forest.tag.must-preorder]
      page = true

      [forest.prefix."#"]
      taxon = "Thing"

      [forest.prefix."~"]
      taxon = "Place"

      [forest.taxon.Person]
      page = false

      [forest.new]
      author = ["@jpears", "@cathe"]

      [forest.directory."trees/papers"]
      tags = ["paper", "reading"]
      |}
  in
  let open Config in
  Alcotest.(check bool)
    "taxon-scoped meta rule"
    true
    (List.mem {key = "redirect"; taxon = Some "Person"; behaviour = External} c.meta_rules);
  Alcotest.(check bool)
    "unscoped meta rule" true
    (List.mem {key = "redirect"; taxon = None; behaviour = Local_redirect} c.meta_rules);
  Alcotest.(check bool)
    "\"self\" is an alias of external" true
    (List.mem {key = "homepage"; taxon = None; behaviour = External} c.meta_rules);
  Alcotest.(check bool)
    "built-in external default kept" true
    (List.mem {key = "external"; taxon = None; behaviour = External} c.meta_rules);
  Alcotest.(check bool)
    "tag hide rule" true
    (List.mem {tag = "private"; tag_behaviour = Hide; page = false} c.tag_rules);
  Alcotest.(check bool)
    "tag page rule defaults to show" true
    (List.mem {tag = "must-preorder"; tag_behaviour = Show; page = true} c.tag_rules);
  Alcotest.(check bool)
    "taxon page opt-out" true
    (c.taxon_rules = [{taxon_name = "Person"; taxon_page = false}]);
  Alcotest.(check (list string))
    "new-tree authors" ["@jpears"; "@cathe"] c.new_authors;
  Alcotest.(check bool)
    "directory tag mask" true
    (c.directory_rules = [{dir = "trees/papers"; dir_tags = ["paper"; "reading"]}]);
  Alcotest.(check bool)
    "prefix rules" true
    (List.mem {prefix = '#'; prefix_taxon = "Thing"} c.prefix_rules
     && List.mem {prefix = '~'; prefix_taxon = "Place"} c.prefix_rules)

let test_vetoed_prefix () =
  let accepted p =
    Forester_core.Reporter.run
      ~emit:(fun _ -> ())
      ~fatal:(fun _ -> false)
      (fun () ->
        ignore (Config_parser.parse_prefix_char p);
        true)
  in
  List.iter
    (fun p ->
      Alcotest.(check bool)
        (Printf.sprintf "prefix %S is rejected loudly" p)
        false (accepted p))
    ["/"; "\\"; "{"; "}"; "["; "]"; "%"; ":"; "."; "-"; "ab"; "a"; "1"; ""];
  List.iter
    (fun p ->
      Alcotest.(check bool)
        (Printf.sprintf "prefix %S is accepted" p)
        true (accepted p))
    ["#"; "@"; "~"; "!"; "?"; "$"; "&"; "+"; "="; "*"; ";"; ","; "_"]

let () =
  let open Alcotest in
  Logs.set_level (Some Debug);
  Logs.set_reporter (Logs.format_reporter ());
  run "Config parsing"
    [
      ( "example config works",
        [test_case "it parses correctly" `Quick test_parsing] );
      ( "can parse config with missing fields",
        [test_case "" `Quick test_missing_fields] );
      ( "meta/tag/prefix schema",
        [
          test_case "it parses" `Quick test_schema;
          test_case "unsafe prefixes are vetoed" `Quick test_vetoed_prefix;
        ] );
    ]
