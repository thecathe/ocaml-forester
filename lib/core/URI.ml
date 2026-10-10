(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

module Basics = struct
  type t = {
    scheme: string option;
    userinfo: string option;
    host: string option;
    port: int option;
    path: string;
    query: (string * string list) list;
    fragment: string option;
  }

  let hydrate {scheme; userinfo; host; port; path; query; fragment} =
    Uri.make ?scheme ?userinfo ?host ?port ~path ~query ?fragment ()

  let dehydrate x =
    {
      scheme = Uri.scheme x;
      userinfo = Uri.userinfo x;
      host = Uri.host x;
      port = Uri.port x;
      path = Uri.path x;
      query = Uri.query x;
      fragment = Uri.fragment x;
    }

  let host x = x.host
  let scheme x = x.scheme
  let port x = x.port
  let path_components x = String.split_on_char '/' @@ Uri.pct_decode x.path

  let rec strip_path_components xs =
    match xs with "" :: xs -> strip_path_components xs | xs -> xs

  let stripped_path_components x = strip_path_components @@ path_components x
  let path_string x = String.concat "/" @@ path_components x

  let append_path_component xs x =
    List.rev @@ (x :: strip_path_components (List.rev xs))

  let equal = ( = )
  let compare = compare
  let resolve ~base x = dehydrate @@ Uri.resolve "" (hydrate base) (hydrate x)
  let canonicalise uri = dehydrate @@ Uri.canonicalize @@ hydrate uri
  let hash (uri : t) = Hashtbl.hash uri

  let with_path_components xs uri =
    dehydrate @@ Uri.canonicalize
    @@ Uri.with_path (hydrate uri)
    @@ String.concat "/" xs

  let pp (fmt : Format.formatter) (uri : t) =
    Format.fprintf fmt "%s" @@ Uri.pct_decode @@ Uri.to_string @@ hydrate uri

  let to_string x = Uri.pct_decode @@ Uri.to_string @@ hydrate x

  (* Unlike [to_string], keeps percent-encoding, so that characters which are
     meaningful in a URL (e.g. a `#` or `?` in a tree address) survive as part
     of the path when emitted as an href rather than starting a fragment/query. *)
  let href_string x = Uri.to_string @@ hydrate x
  let t = Repr.map Repr.string (Fun.compose dehydrate Uri.of_string) to_string
  let of_string_exn str = dehydrate @@ Uri.canonicalize @@ Uri.of_string str

  let make ?scheme ?user ?host ?port ?path () =
    let path = Option.map (String.concat "/") path in
    dehydrate @@ Uri.canonicalize
    @@ Uri.make ?scheme ?userinfo:user ?host ?port ?path ()

  let relative_path_string ~(base : t) uri : string =
    Str.replace_first (Str.regexp (Format.asprintf "^%a" pp base)) ""
    @@ to_string uri

  let display_path_string ~base uri =
    if host uri = host base then
      Str.replace_first (Str.regexp (Format.asprintf "^%a" pp base)) ""
      @@ to_string
      @@ with_path_components
           (List.rev @@ strip_path_components @@ List.rev @@ path_components uri)
           uri
    else to_string uri
end

module Set = Set.Make (Basics)
module Map = Map.Make (Basics)
module Tbl = Hashtbl.Make (Basics)
include Basics
