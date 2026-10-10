(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core

(* type keys = (Toml.Types.Table.key
   [@printer fun fmt key -> fprintf fmt "%s" (Toml.Types.Table.Key.to_string
    key)]) list list [@@deriving show] *)

module Key_set = struct
  include Set.Make (struct
    type t = Toml.Types.Table.key list

    let compare = compare
  end)

  let remove : string list -> t -> t =
   fun strs set ->
    let key = List.map Toml.Types.Table.Key.of_string strs in
    remove key set
end

let keys (tbl : Toml.Types.value Toml.Types.Table.t) =
  let rec go current keys tbl =
    List.fold_left
      begin fun acc (key, value) ->
        match value with
        | Toml.Types.TBool _ | TInt _ | TFloat _ | TString _ | TDate _
        | TArray _ ->
          (key :: current) :: acc
        | TTable tbl -> go (key :: current) acc tbl
      end
      keys
      (Toml.Types.Table.to_list tbl)
  in
  Key_set.of_list @@ List.map List.rev @@ go [] [] tbl


(* Punctuation that may NOT be used as an address prefix, because it is
   syntactically meaningful in tree markup or addresses (or in URLs in a way
   that cannot be made safe), so using it would silently misbehave. Every
   other ASCII punctuation character is accepted; the ones that are unsafe in
   a URL path (`#`, `?`) are percent-encoded when hrefs are emitted. *)
let vetoed_prefixes =
  [
    ('/', "path separator"); ('\\', "markup escape"); ('{', "markup"); ('}', "markup");
    ('[', "markup"); (']', "markup"); ('(', "markup"); (')', "markup");
    ('%', "comment / percent-escape"); ('\'', "datalog constant"); ('"', "quoting");
    ('`', "quoting"); ('<', "URL-unsafe"); ('>', "URL-unsafe"); ('|', "URL-unsafe");
    ('^', "URL-unsafe"); (':', "URI scheme separator");
    ('.', "extension / hidden-file marker"); ('-', "address key separator");
  ]

let is_ascii_punctuation c =
  match c with
  | '!' .. '/' | ':' .. '@' | '[' .. '`' | '{' .. '~' -> true
  | _ -> false

(* [Key.to_string] re-quotes keys that are not valid bare TOML keys. *)
let key_name k =
  let s = Toml.Types.Table.Key.to_string k in
  let n = String.length s in
  if n >= 2 && s.[0] = '"' && s.[n - 1] = '"' then String.sub s 1 (n - 2) else s

let parse_prefix_char name =
  let fail reason =
    Reporter.fatal Configuration_error
      ~extra_remarks:
        [
          Asai.Diagnostic.loctextf
            "Invalid prefix %S in `[forest.prefix]`: %s." name reason;
        ]
  in
  if String.length name <> 1 then
    fail "a prefix must be exactly one ASCII punctuation character"
  else
    let c = name.[0] in
    if not (is_ascii_punctuation c) then
      fail "a prefix must be an ASCII punctuation character"
    else
      match List.assoc_opt c vetoed_prefixes with
      | Some why -> fail (Printf.sprintf "%C is not allowed (%s)" c why)
      | None -> c

let parse_meta_behaviour ~where = function
  | "redirect" -> Config.Redirect
  | "local-redirect" -> Config.Local_redirect
  | "external" | "self" -> Config.External
  | other ->
    Reporter.fatal Configuration_error
      ~extra_remarks:
        [
          Asai.Diagnostic.loctextf
            "Unknown behaviour %S in `[%s]`; expected one of \"redirect\", \
             \"local-redirect\", \"external\", \"self\"."
            other where;
        ]

let parse_tag_behaviour ~where = function
  | "hide" -> Config.Hide
  | "show" -> Config.Show
  | other ->
    Reporter.fatal Configuration_error
      ~extra_remarks:
        [
          Asai.Diagnostic.loctextf
            "Unknown behaviour %S in `[%s]`; expected \"hide\" or \"show\"."
            other where;
        ]

let invalid_value ~where what =
  Reporter.fatal Configuration_error
    ~extra_remarks:
      [Asai.Diagnostic.loctextf "Invalid value in `[%s]`: %s." where what]

let parse lexbuf filename =
  let@ () = Reporter.easy_run in
  match Toml.Parser.parse lexbuf filename with
  | `Error (desc, {source; _}) ->
    let@ () = Reporter.tracef "when parsing configuration file" in
    let loc = Asai.Range.of_lexbuf ~source:(`File source) lexbuf in
    Reporter.fatal ~loc Configuration_error
      ~extra_remarks:[Asai.Diagnostic.loctextf "%s" desc]
  | `Ok tbl ->
    let open Toml.Lenses in
    let keys = ref (keys tbl) in
    let with_default ~value k lens =
      let open Toml.Lenses in
      match get tbl lens with
      | None ->
        Reporter.emit (Using_default_option k);
        value
      | Some v ->
        keys := Key_set.remove k !keys;
        v
    in
    let forest = key "forest" |-- table in
    let url =
      let k = ["forest"; "url"] in
      match get tbl (forest |-- key "url" |-- string) with
      | Some url ->
        keys := Key_set.remove k !keys;
        begin try URI.of_string_exn url
        with _ ->
          Reporter.fatal Configuration_error
            ~extra_remarks:
              [Asai.Diagnostic.loctext "Invalid URL specified in `url` key."]
        end
      | None ->
        Reporter.emit (Using_default_option k);
        Config.default_url
    in
    let default = Config.default ~url () in
    let trees =
      let k = ["forest"; "trees"] in
      with_default ~value:default.trees k
        (forest |-- key "trees" |-- array |-- strings)
    in
    let foreign =
      let k = ["forest"; "foreign"] in
      match get tbl (forest |-- key "foreign" |-- array |-- tables) with
      | None -> default.foreign
      | Some foreign_tbls ->
        keys := Key_set.remove k !keys;
        let@ foreign_tbl = List.map @~ foreign_tbls in
        let path =
          match get foreign_tbl (key "path" |-- string) with
          | None -> Reporter.fatal (Required_config_option "path")
          | Some path -> path
        in
        let route_locally =
          match get foreign_tbl (key "route_locally" |-- bool) with
          | None -> true
          | Some b -> b
        in
        let include_in_manifest =
          match get foreign_tbl (key "include_in_manifest" |-- bool) with
          | None -> true
          | Some b -> b
        in
        Config.{path; route_locally; include_in_manifest}
    in
    let assets =
      with_default ~value:default.assets ["forest"; "assets"]
        (forest |-- key "assets" |-- array |-- strings)
    in
    let home =
      let k = ["forest"; "home"] in
      URI_scheme.named_uri ~base:url
      @@ with_default ~value:"index" k (forest |-- key "home" |-- string)
    in
    let sub_table ~k =
      match get tbl (forest |-- key k |-- table) with
      | None -> []
      | Some t -> Toml.Types.Table.to_list t
    in
    let claim path = keys := Key_set.remove path !keys in
    let string_field ~where t name =
      match Toml.Types.Table.find_opt (Toml.Types.Table.Key.of_string name) t with
      | None -> None
      | Some (Toml.Types.TString s) -> Some s
      | Some _ -> invalid_value ~where (Printf.sprintf "`%s` must be a string" name)
    in
    let bool_field ~where t name =
      match Toml.Types.Table.find_opt (Toml.Types.Table.Key.of_string name) t with
      | None -> None
      | Some (Toml.Types.TBool b) -> Some b
      | Some _ -> invalid_value ~where (Printf.sprintf "`%s` must be a boolean" name)
    in
    (* [forest.meta.<key>] (unscoped) and [forest.meta.<key>.<Taxon>] *)
    let meta_rules =
      let user =
        let@ k, v = List.concat_map @~ sub_table ~k:"meta" in
        let key = key_name k in
        match v with
        | Toml.Types.TTable t ->
          let where = "forest.meta." ^ key in
          let unscoped =
            match string_field ~where t "behaviour" with
            | None -> []
            | Some b ->
              claim ["forest"; "meta"; key; "behaviour"];
              [Config.{key; taxon = None; behaviour = parse_meta_behaviour ~where b}]
          in
          let scoped =
            let@ sk, sv = List.filter_map @~ Toml.Types.Table.to_list t in
            let taxon = key_name sk in
            match sv with
            | Toml.Types.TTable st ->
              let where = where ^ "." ^ taxon in
              begin match string_field ~where st "behaviour" with
              | None ->
                invalid_value ~where "missing required `behaviour`"
              | Some b ->
                claim ["forest"; "meta"; key; taxon; "behaviour"];
                Some
                  Config.
                    {key; taxon = Some taxon; behaviour = parse_meta_behaviour ~where b}
              end
            | _ -> None
          in
          unscoped @ scoped
        | _ -> invalid_value ~where:"forest.meta" "expected a table per meta key"
      in
      user @ default.meta_rules
    in
    (* [forest.tag.<name>]: behaviour = "hide" | "show"; page = true | false *)
    let tag_rules =
      let@ k, v = List.map @~ sub_table ~k:"tag" in
      let tag = key_name k in
      let where = "forest.tag." ^ tag in
      match v with
      | Toml.Types.TTable t ->
        let tag_behaviour =
          match string_field ~where t "behaviour" with
          | None -> Config.Show
          | Some b ->
            claim ["forest"; "tag"; tag; "behaviour"];
            parse_tag_behaviour ~where b
        in
        let page =
          match bool_field ~where t "page" with
          | None -> false
          | Some b ->
            claim ["forest"; "tag"; tag; "page"];
            b
        in
        Config.{tag; tag_behaviour; page}
      | _ -> invalid_value ~where:"forest.tag" "expected a table per tag"
    in
    (* [forest.prefix."<punct>"]: taxon = "<Taxon>" *)
    let prefix_rules =
      let@ k, v = List.map @~ sub_table ~k:"prefix" in
      let name = key_name k in
      let prefix = parse_prefix_char name in
      let where = Printf.sprintf "forest.prefix.%S" name in
      match v with
      | Toml.Types.TTable t -> begin
        match string_field ~where t "taxon" with
        | None -> invalid_value ~where "missing required `taxon`"
        | Some prefix_taxon ->
          claim ["forest"; "prefix"; name; "taxon"];
          Config.{prefix; prefix_taxon}
      end
      | _ -> invalid_value ~where:"forest.prefix" "expected a table per prefix"
    in
    (* [forest.directory."<path>"]: tags = ["<tag>", ...] *)
    let directory_rules =
      let@ k, v = List.map @~ sub_table ~k:"directory" in
      let dir = key_name k in
      let where = Printf.sprintf "forest.directory.%S" dir in
      match v with
      | Toml.Types.TTable t -> begin
        match
          Toml.Types.Table.find_opt (Toml.Types.Table.Key.of_string "tags") t
        with
        | Some (Toml.Types.TArray (Toml.Types.NodeString tags)) ->
          claim ["forest"; "directory"; dir; "tags"];
          Config.{dir; dir_tags = tags}
        | Some (Toml.Types.TArray Toml.Types.NodeEmpty) ->
          claim ["forest"; "directory"; dir; "tags"];
          Config.{dir; dir_tags = []}
        | Some _ -> invalid_value ~where "`tags` must be an array of strings"
        | None -> invalid_value ~where "missing required `tags`"
      end
      | _ ->
        invalid_value ~where:"forest.directory" "expected a table per directory"
    in
    (* [forest.new]: author = "<addr>" | ["<addr>", ...] *)
    let new_authors =
      match get tbl (forest |-- key "new" |-- table) with
      | None -> default.new_authors
      | Some t -> begin
        match
          Toml.Types.Table.find_opt (Toml.Types.Table.Key.of_string "author") t
        with
        | None -> []
        | Some (Toml.Types.TString a) ->
          claim ["forest"; "new"; "author"];
          [a]
        | Some (Toml.Types.TArray (Toml.Types.NodeString l)) ->
          claim ["forest"; "new"; "author"];
          l
        | Some _ ->
          invalid_value ~where:"forest.new"
            "`author` must be a string or an array of strings"
      end
    in
    begin if not (Key_set.is_empty !keys) then
      let keys =
        !keys |> Key_set.to_list
        |> List.map (List.map Toml.Types.Table.Key.to_string)
      in
      Reporter.emit (Uninterpreted_config_options keys)
    end;
    Config.{url; assets; trees; foreign; home; meta_rules; tag_rules; prefix_rules; directory_rules; new_authors}

let parse_forest_config_string str =
  let lexbuf = Lexing.from_string str in
  parse lexbuf "<anonymous>"

let parse_forest_config_file filename =
  try
    let ch = open_in filename in
    let@ () = Fun.protect ~finally:(fun _ -> close_in ch) in
    let lexbuf = Lexing.from_channel ch in
    let result = parse lexbuf filename in
    Sys.chdir @@ Filename.dirname filename;
    result
  with exn ->
    Reporter.fatal Configuration_error
      ~extra_remarks:[Asai.Diagnostic.loctextf "%a" Eio.Exn.pp exn]
