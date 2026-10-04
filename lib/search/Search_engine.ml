(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_search
open Forester_frontend

open struct
  module T = Types
end

let ranked_search : ?fuzz:int -> State.t -> string -> (URI.t * float) list =
 fun ?fuzz:_ forest terms ->
  Tokenizer.tokenize terms |> function
  | tokens ->
    (* In order to rank documents, I search for the first token and then rank
       the returned documents according to all tokens. This duplicates the
       search for the first token, so this should be changed.*)
    let first_token = List.hd tokens in
    let matches = Index.search ~fuzz:1 forest.search_index first_token in
    let uris =
      List.filter_map
        (fun (_, uri) ->
          match URI.Tbl.find_opt forest.index uri with
          | Some (Resource {resource = T.Article a; _}) ->
            Some (uri, Index.BM_25.score a terms forest.search_index)
          | None -> assert false
          | _ -> None)
        matches
    in
    List.sort
      (fun (_, score_a) (_, score_b) -> Float.compare score_a score_b)
      uris

let test_ranked (forest : State.t) =
  let ranked_results =
    Reporter.profile "Ranked search" @@ fun () ->
    ranked_search ~fuzz:2 forest "hyprtext format"
  in
  Format.printf "got %i ranked results.@." (List.length ranked_results);
  List.iter
    (fun (uri, score) ->
      match State.get_article uri forest with
      | Some _article -> Format.printf "%a, %f@." URI.pp uri score
      | None -> assert false)
    ranked_results

let test_search (forest : State.t) =
  let s = read_line () in
  let results =
    (* Reporter.profile "Searching" @@ fun () -> *)
    Index.search ~fuzz:1 forest.search_index s
  in
  Format.printf "got %i results@." (List.length results)

let main ~env () =
  let config = Config_parser.parse_forest_config_file "forest.toml" in
  let dev = true in
  let forest = Driver.batch_run ~env ~dev ~config () in
  let articles = List.of_seq @@ State.get_all_articles forest in
  let index =
    (* Reporter.profile "Building index" @@ fun () -> *)
    Index.create articles
  in
  let size = Obj.reachable_words @@ Obj.repr index in
  Format.printf "index size: %i@." size;
  let forest = {forest with search_index = index} in
  test_search forest
(*   test_search_cache forest.search_index *)
(*   test_ranked forest; *)

let () =
  let@ env = Eio_main.run in
  let@ () = Forester_core.Reporter.easy_run in
  main ~env ()
