(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_frontend
open Forester_test
open Testables

open struct
  module T = Types
end

let config = Config.default ()

let raw_trees =
  [
    {
      path = "person.tree";
      content =
        {|\title{Person}\taxon{Person}\meta{redirect}{https://example.com/person}\p{[Other](other)}|};
    };
    {path = "other.tree"; content = {|\title{Other}\p{[Person](person)}|}};
  ]

let person_uri = URI_scheme.named_uri ~base:config.url "person"
let other_uri = URI_scheme.named_uri ~base:config.url "other"

let index_of_substring ~needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec loop i =
    if i + n > h then None
    else if String.sub haystack i n = needle then Some i
    else loop (i + 1)
  in
  loop 0

let contains_substring ~needle haystack =
  Option.is_some @@ index_of_substring ~needle haystack

let with_forest ?(config = config) ?(raw_trees = raw_trees) ~env
    (f : State.t -> unit) =
  let@ () = Reporter.easy_run in
  with_test_forest ~raw_trees ~env ~config (fun path ->
      Sys.chdir (Eio.Path.native_exn path);
      let forest = Driver.batch_run ~env ~config ~dev:false () in
      f forest)

let evaluated_of ~forest uri =
  match State.find_opt forest uri with
  | Some tree -> (
    match Tree.to_evaluated tree with
    | Some e -> e
    | None -> Alcotest.fail "not a planted resource")
  | None -> Alcotest.fail "tree not found"


let test_output_excluded ~env () =
  with_forest ~env (fun forest ->
      let evaluated =
        match State.find_opt forest person_uri with
        | Some tree -> (
          match Tree.to_evaluated tree with
          | Some e -> e
          | None -> Alcotest.fail "person is not a planted resource")
        | None -> Alcotest.fail "person tree not found"
      in
      Alcotest.(check bool)
        "redirect-flagged tree is not routed locally" false
        evaluated.route_locally;
      Alcotest.(check bool)
        "redirect-flagged tree is excluded from the manifest" false
        evaluated.include_in_manifest;
      Alcotest.(check (option uri))
        "redirect target is recorded on the evaluated tree"
        (Some (URI.of_string_exn "https://example.com/person"))
        evaluated.redirect_uri;
      Forester.render_forest ~dev:false ~forest;
      Alcotest.(check bool)
        "no output directory is written for a redirect-flagged tree" false
        (Sys.file_exists "output/person");
      Alcotest.(check bool)
        "an ordinary tree's output is still written" true
        (Sys.file_exists "output/other/index.xml"))

let test_link_redirects ~env () =
  with_forest ~env (fun forest ->
      let other_article =
        match State.get_article other_uri forest with
        | Some article -> article
        | None -> Alcotest.fail "other tree not found"
      in
      let rendered =
        Format.asprintf "%a"
          (Legacy_xml_client.pp_xml ~forest ?stylesheet:None)
          other_article
      in
      Alcotest.(check bool)
        "redirected link's href points at the external target" true
        (contains_substring ~needle:{|href="https://example.com/person"|}
           rendered);
      Alcotest.(check bool)
        "redirected link is typed external, not local" true
        (contains_substring ~needle:{|type="external"|} rendered);
      Alcotest.(check bool)
        "redirected link does not leak the tree's real local address" false
        (contains_substring ~needle:(URI.to_string person_uri) rendered))

let test_backlinks_excludes_redirect ~env () =
  with_forest ~env (fun forest ->
      (* `other` is linked to by `person`, so without the redirect filter in
         `Forest_util.get_sorted_articles`, `other`'s auto-generated
         Backlinks section would list `person`. Rendering `other`'s full
         article (mainmatter + backmatter) via the same public `pp_xml` path
         used for real builds, and slicing from the first occurrence of
         "backmatter" onward, isolates that auto-generated region without
         needing to reach into Forest_util/Builtin_queries directly. *)
      let other_article =
        match State.get_article other_uri forest with
        | Some article -> article
        | None -> Alcotest.fail "other tree not found"
      in
      let rendered =
        Format.asprintf "%a"
          (Legacy_xml_client.pp_xml ~forest ?stylesheet:None)
          other_article
      in
      let backmatter_region =
        match index_of_substring ~needle:"backmatter" rendered with
        | Some idx -> String.sub rendered idx (String.length rendered - idx)
        | None -> Alcotest.fail "could not locate backmatter region"
      in
      Alcotest.(check bool)
        "the redirect-flagged tree's title is absent from the other tree's \
         backmatter"
        false
        (contains_substring ~needle:"Person" backmatter_region);
      Alcotest.(check bool)
        "the redirect-flagged tree's address does not leak into backmatter"
        false
        (contains_substring ~needle:(URI.to_string person_uri)
           backmatter_region))

(* [forest.meta.redirect.Person] behaviour = "external": the same `redirect`
   meta no longer excludes the tree, since its taxon is `Person`. *)
let test_taxon_rule_overrides_redirect ~env () =
  let config =
    {
      config with
      meta_rules =
        {key = "redirect"; taxon = Some "Person"; behaviour = External}
        :: config.meta_rules;
    }
  in
  with_forest ~config ~env (fun forest ->
      let evaluated = evaluated_of ~forest person_uri in
      Alcotest.(check bool)
        "taxon-scoped `external` rule keeps the tree routed locally" true
        evaluated.route_locally;
      Alcotest.(check (option uri))
        "and records no redirect target" None evaluated.redirect_uri;
      Forester.render_forest ~dev:false ~forest;
      Alcotest.(check bool)
        "the tree's own output is written" true
        (Sys.file_exists "output/person/index.xml"))

let test_rich_redirect_target ~env () =
  let raw_trees =
    [
      {
        path = "site.tree";
        content = {|\title{Site}\meta{redirect}{[the site](https://example.com/s)}|};
      };
    ]
  in
  with_forest ~raw_trees ~env (fun forest ->
      let evaluated =
        evaluated_of ~forest (URI_scheme.named_uri ~base:config.url "site")
      in
      Alcotest.(check (option uri))
        "the href of a [text](url) redirect meta is used as the target"
        (Some (URI.of_string_exn "https://example.com/s"))
        evaluated.redirect_uri)

let hide_raw_trees =
  [
    {path = "priv.tree"; content = {|\title{Priv}\tag{private}\p{secret}|}};
    {path = "pub.tree"; content = {|\title{Pub}\tag{open}\p{[Priv](priv)}|}};
  ]

let hide_config =
  {
    config with
    tag_rules = [{tag = "private"; tag_behaviour = Hide; page = false}];
  }

let test_tag_hide ~env () =
  with_forest ~config:hide_config ~raw_trees:hide_raw_trees ~env (fun forest ->
      let priv = evaluated_of ~forest (URI_scheme.named_uri ~base:config.url "priv") in
      Alcotest.(check bool) "hidden tree is not routed" false priv.route_locally;
      Alcotest.(check bool)
        "hidden tree is excluded from the manifest" false
        priv.include_in_manifest;
      Alcotest.(check bool) "hidden tree is flagged hidden" true priv.hidden;
      let pub = evaluated_of ~forest (URI_scheme.named_uri ~base:config.url "pub") in
      Alcotest.(check bool) "untagged-private tree is routed" true pub.route_locally;
      Forester.render_forest ~dev:false ~forest;
      Alcotest.(check bool)
        "no output for the hidden tree" false (Sys.file_exists "output/priv");
      Alcotest.(check bool)
        "output for the public tree" true
        (Sys.file_exists "output/pub/index.xml"))

let test_tag_hide_excluded_from_backlinks ~env () =
  (* `priv` links to `pub`; `pub`'s generated backmatter must not list it. *)
  let raw_trees =
    [
      {path = "priv.tree"; content = {|\title{Priv}\tag{private}\p{[Pub](pub)}|}};
      {path = "pub.tree"; content = {|\title{Pub}|}};
    ]
  in
  with_forest ~config:hide_config ~raw_trees ~env (fun forest ->
      let pub_article =
        match State.get_article (URI_scheme.named_uri ~base:config.url "pub") forest with
        | Some a -> a
        | None -> Alcotest.fail "pub not found"
      in
      let rendered =
        Format.asprintf "%a" (Legacy_xml_client.pp_xml ~forest ?stylesheet:None) pub_article
      in
      let backmatter_region =
        match index_of_substring ~needle:"backmatter" rendered with
        | Some idx -> String.sub rendered idx (String.length rendered - idx)
        | None -> Alcotest.fail "could not locate backmatter region"
      in
      Alcotest.(check bool)
        "hidden tree's title is absent from backmatter" false
        (contains_substring ~needle:"Priv" backmatter_region))

let test_tag_show_is_default ~env () =
  let config =
    {config with tag_rules = [{tag = "private"; tag_behaviour = Show; page = false}]}
  in
  with_forest ~config ~raw_trees:hide_raw_trees ~env (fun forest ->
      let priv = evaluated_of ~forest (URI_scheme.named_uri ~base:config.url "priv") in
      Alcotest.(check bool) "`show` leaves the tree routed" true priv.route_locally;
      Alcotest.(check bool) "and not hidden" false priv.hidden)

let () =
  let@ env = Eio_main.run in
  Logs.set_level (Some Debug);
  let open Alcotest in
  run "Test_redirect_meta"
    [
      ( "redirect meta",
        [
          test_case "excludes own output" `Quick (test_output_excluded ~env);
          test_case "redirects inbound links" `Quick
            (test_link_redirects ~env);
          test_case "excluded from backlinks" `Quick
            (test_backlinks_excludes_redirect ~env);
          test_case "taxon rule overrides the redirect" `Quick
            (test_taxon_rule_overrides_redirect ~env);
          test_case "rich [text](url) redirect target" `Quick
            (test_rich_redirect_target ~env);
        ] );
      ( "tag behaviour",
        [
          test_case "hide excludes output and manifest" `Quick
            (test_tag_hide ~env);
          test_case "hide excluded from backlinks" `Quick
            (test_tag_hide_excluded_from_backlinks ~env);
          test_case "show is a no-op" `Quick (test_tag_show_is_default ~env);
        ] );
    ]
