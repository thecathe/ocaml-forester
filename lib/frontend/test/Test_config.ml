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
        publish_tags = [];
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
        publish_tags = [];
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
    (List.mem {tag = "private"; tag_behaviour = Hide; page = false; display = true; title = None; label = None} c.tag_rules);
  Alcotest.(check bool)
    "tag page rule defaults to show" true
    (List.mem {tag = "must-preorder"; tag_behaviour = Show; page = true; display = true; title = None; label = None} c.tag_rules);
  Alcotest.(check bool)
    "taxon page opt-out" true
    (c.taxon_rules = [{taxon_name = "Person"; taxon_page = false; taxon_unpublished = false; taxon_publish = []}]);
  Alcotest.(check (list string))
    "new-tree authors" ["@jpears"; "@cathe"] c.new_authors;
  Alcotest.(check bool)
    "directory tag mask" true
    (c.directory_rules = [{dir = "trees/papers"; dir_tags = ["paper"; "reading"]}]);
  Alcotest.(check bool)
    "prefix rules" true
    (List.mem {prefix = '#'; prefix_taxon = "Thing"} c.prefix_rules
     && List.mem {prefix = '~'; prefix_taxon = "Place"} c.prefix_rules)

let test_unpublished_taxon () =
  let c =
    Forester_core.Reporter.easy_run @@ fun () ->
    Config_parser.parse_forest_config_string
      {|
      [forest]
      url = "/"
      [forest.taxon.Person]
      page = false
      unpublished = true
      publish = ["@jpears"]
      |}
  in
  Alcotest.(check bool)
    "unpublished taxon with exceptions" true
    (c.taxon_rules
    = [
        {
          taxon_name = "Person";
          taxon_page = false;
          taxon_unpublished = true;
          taxon_publish = ["@jpears"];
        };
      ]);
  Alcotest.(check bool)
    "a listed address is published" false
    (Config.is_unpublished c ~taxon:(Some "Person") ~address:"@jpears" ~tags:[]);
  Alcotest.(check bool)
    "any other is not" true
    (Config.is_unpublished c ~taxon:(Some "Person") ~address:"@someone" ~tags:[])

let test_publish_tags () =
  let c =
    Forester_core.Reporter.easy_run @@ fun () ->
    Config_parser.parse_forest_config_string
      {|
      [forest]
      url = "/"
      [forest.publish]
      tags = ["public"]
      [forest.tag.public]
      display = false
      [forest.taxon.Person]
      unpublished = true
      |}
  in
  Alcotest.(check (list string)) "publish tags parsed" ["public"] c.publish_tags;
  Alcotest.(check bool) "a hidden-display tag" false (Config.is_displayed_tag c "public");
  Alcotest.(check bool) "other tags display" true (Config.is_displayed_tag c "notes");
  Alcotest.(check bool)
    "an untagged tree is unpublished" true
    (Config.is_unpublished c ~taxon:None ~address:"note" ~tags:["notes"]);
  Alcotest.(check bool)
    "a tagged tree is published" false
    (Config.is_unpublished c ~taxon:None ~address:"home" ~tags:["public"]);
  Alcotest.(check bool)
    "the tag beats an unpublished taxon" false
    (Config.is_unpublished c ~taxon:(Some "Person") ~address:"@me" ~tags:["public"])

let tag_label_toml =
  {|
  [forest]
  url = "/"
  [forest.tag."~srepls"]
  title = "S-REPLS"
  page = true
  [forest.tag.mebi]
  title = "MEBI"
  label = "Mebi"
  [forest.tag.notes]
  label = "Field notes"
  [forest.tag.plain]
  page = true
  |}

let test_tag_labels () =
  let warnings = ref [] in
  let c =
    Forester_core.Reporter.run
      ~emit:(fun d -> warnings := d :: !warnings)
      ~fatal:(fun _ -> Alcotest.fail "unexpected fatal diagnostic")
      (fun () -> Config_parser.parse_forest_config_string tag_label_toml)
  in
  let label = Config.tag_label c in
  Alcotest.(check string) "title is the default label" "S-REPLS" (label "~srepls");
  Alcotest.(check string) "label beats title" "Mebi" (label "mebi");
  Alcotest.(check string) "label without a title" "Field notes" (label "notes");
  Alcotest.(check string) "a tag with neither is shown as itself" "plain" (label "plain");
  Alcotest.(check string) "an unconfigured tag is shown as itself" "other" (label "other");
  Alcotest.(check bool)
    "title and label are interpreted, not reported as unknown options" false
    (List.exists
       (fun (d : _ Asai.Diagnostic.t) ->
         match d.message with
         | Forester_core.Reporter.Message.Uninterpreted_config_options _ -> true
         | _ -> false)
       !warnings)

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
          test_case "unpublished taxon" `Quick test_unpublished_taxon;
          test_case "publish tags" `Quick test_publish_tags;
          test_case "tag title and label" `Quick test_tag_labels;
        ] );
    ]
