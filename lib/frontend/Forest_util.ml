(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler

open struct
  module T = Types
end

let compare_article ~forest =
  let module C = Types.Comparators (struct
    let string_of_content x = Plain_text_client.string_of_content ~forest x
  end) in
  C.compare_article

(* A redirect-flagged or hidden tree must never surface in an auto-generated
   References/Context/Backlinks/Related/Contributions section (this is the
   single choke point all of those, plus indirect-contributor inference via
   [collect_attributions] below, funnel through) -- even though the link
   graph itself still legitimately records that other trees link to it. *)
let is_redirect_flagged (forest : State.t) (uri : URI.t) : bool =
  match State.find_opt forest uri with
  | None -> false
  | Some tree -> (
    match Tree.to_evaluated tree with
    | Some {redirect_uri = Some _; _} | Some {hidden = true; _} -> true
    | _ -> false)

let get_sorted_articles (forest : State.t) addrs =
  addrs |> Vertex_set.to_seq
  |> Seq.filter_map Vertex.uri_of_vertex
  |> Seq.filter (fun uri -> not (is_redirect_flagged forest uri))
  |> Seq.filter_map (fun uri -> State.get_article uri forest)
  |> List.of_seq
  |> List.sort (compare_article ~forest)

let collect_attributions (forest : State.t) (uri_opt : URI.t option)
    (primary_attributions : _ T.attribution list) =
  match uri_opt with
  | None -> primary_attributions
  | Some uri ->
    let indirect_attributions =
      let open Datalog_expr.Notation in
      let articles =
        let x = "X" in
        let positives =
          [
            Builtin_relation.has_indirect_contributor
            @* [const (T.Uri_vertex uri); var x];
          ]
        in
        let negatives = [] in
        Datalog_expr.{var = x; positives; negatives}
        |> Forest.run_datalog_query forest.graphs
        |> get_sorted_articles forest
      in
      let@ biotree : _ T.article = List.filter_map @~ articles in
      let@ uri = Option.map @~ biotree.frontmatter.uri in
      T.{vertex = T.Uri_vertex uri; role = Contributor}
    in
    primary_attributions
    @
    let@ attribution = List.filter_map @~ indirect_attributions in
    if
      List.exists
        (fun (existing : _ T.attribution) ->
          Vertex.equal attribution.vertex existing.vertex)
        primary_attributions
    then None
    else Some attribution
