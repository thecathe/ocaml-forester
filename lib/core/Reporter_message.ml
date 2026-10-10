open Base

open struct
  module R = Resolver
  module Sc = R.Scope
end

type expected_value =
  | Content
  | Text
  | Obj
  | Bool
  | Sym
  | Dx_query
  | Dx_sequent
  | Dx_prop
  | Datalog_term
  | Node
  | URI
  | Argument
[@@deriving show]

type t =
  | Import_not_found of URI.t
  | Invalid_URI
  | Asset_has_no_content_address of string
  | Asset_not_found of string
  | Current_tree_has_no_uri
  | Duplicate_tree of origin * origin
  | Parse_error
  | Unbound_method of (string * Value.obj)
  | Type_warning
  | Type_error of {got: Value.t option; expected: expected_value list}
  | Unbound_fluid_symbol of Symbol.t
  | Unbound_variable of string
  | Unresolved_identifier of ((Sc.data, R.P.tag) Trie.t[@opaque]) * Trie.path
  | Unresolved_xmlns of string
  | Reference_error of URI.t
  | Unhandled_case
  | Transclusion_loop
  | Internal_error
  | Configuration_error
  | Initialization_warning
  | Routing_error
  | Profiling of float * float
  | External_error
  | Resource_not_found of URI.t
  | Broken_link of {uri: URI.t; suggestion: URI.t option}
  | Reference_to_hidden_tree of {
      uri: URI.t;
      kind: string;
      tags: string list;
      source: string option;
      from: string option;
    }
  | IO_error
  | Log
  | Missing_argument
  | Uninterpreted_config_options of string list list
  | Using_default_option of string list
  | Required_config_option of string
[@@deriving show]

let default_severity : t -> Asai.Diagnostic.severity = function
  | Import_not_found _ -> Error
  | Unresolved_identifier _ -> Warning
  | Unresolved_xmlns _ -> Error
  | Invalid_URI -> Error
  | Unbound_method _ -> Error
  | Asset_has_no_content_address _ -> Error
  | Asset_not_found _ -> Error
  | Current_tree_has_no_uri -> Error
  | Reference_error _ -> Error
  | Duplicate_tree _ -> Error
  | Parse_error -> Error
  | Type_error _ -> Error
  | Type_warning -> Warning
  | Unbound_fluid_symbol _ -> Error
  | Unbound_variable _ -> Error
  | Unhandled_case -> Bug
  | Transclusion_loop -> Error
  | Internal_error -> Bug
  | Configuration_error -> Error
  | Initialization_warning -> Warning
  | Routing_error -> Error
  | Profiling _ -> Info
  | External_error -> Error
  | Log -> Info
  | Resource_not_found _ -> Error
  | Broken_link _ -> Warning
  | Reference_to_hidden_tree _ -> Warning
  | IO_error -> Error
  | Missing_argument -> Error
  | Uninterpreted_config_options _ -> Warning
  | Using_default_option _ -> Info
  | Required_config_option _ -> Error

let short_code : t -> string = function
  | Import_not_found _ -> "import_not_found"
  | Invalid_URI -> "invalid_uri"
  | Asset_has_no_content_address _ ->
    "asset_not_found"
    (* This is taken from the original wording of the message, but I think this
       is very confusing.*)
  | Asset_not_found _ -> "asset_not_found"
  | Current_tree_has_no_uri -> "current_tree_has_no_uri"
  | Duplicate_tree _ -> "duplicate_tree"
  | Parse_error -> "parse_error"
  | Unbound_method _ -> "unbound_method"
  | Type_warning -> "type_warning"
  | Type_error _ -> "type_error"
  | Unbound_fluid_symbol _ -> "unbound_fluid_symbol"
  | Unbound_variable _ -> "Unbound_variable"
  | Unresolved_xmlns _ -> "unresolved_xmlns"
  | Unresolved_identifier _ -> "unresolved_identifier"
  | Reference_error _ -> "reference_error"
  | Unhandled_case -> "unhandled_case"
  | Transclusion_loop -> "transclusion_loop"
  | Internal_error -> "internal_error"
  | Configuration_error -> "configuration_error"
  | Initialization_warning -> "initialization_warning"
  | Routing_error -> "routing_error"
  | Profiling (_, _) -> "profiling"
  | External_error -> "external_error"
  | Resource_not_found _ -> "resource_not_found"
  | Broken_link _ -> "broken_link"
  | Reference_to_hidden_tree _ -> "reference_to_hidden_tree"
  | IO_error -> "io_error"
  | Log -> "log"
  | Missing_argument -> "missing_argument"
  | Uninterpreted_config_options _ -> "unknown_config_option"
  | Using_default_option _ -> "using_default_option"
  | Required_config_option _ -> "required_config_option"

let this_is : Value.t -> string = function
  | Value.Content _ -> "content"
  | Value.Clo (_, _, _) -> "a closure"
  | Value.Dx_prop _ -> "a datalog proposition"
  | Value.Dx_sequent _ -> "a datalog sequent"
  | Value.Dx_query _ -> "a datalog query"
  | Value.Dx_var _ -> "a datalog variable"
  | Value.Dx_const _ -> "a datalog constant"
  | Value.Sym _ -> "a symbol"
  | Value.Obj _ -> "an object"

let show_expected_value : expected_value -> string = function
  | Content -> "content"
  | Text -> "text"
  | Obj -> "an object"
  | Bool -> "a boolean"
  | Sym -> "a symbol"
  | Dx_query -> "a datalog query"
  | Dx_sequent -> "a datalog sequent"
  | Dx_prop -> "a datalog proposition"
  | Datalog_term -> "a datalog term"
  | Node -> "a node" (* This might be hard to understand for the end user*)
  | URI -> "a URI"
  | Argument -> "an argument"

let default_text : t -> Asai.Diagnostic.text = function
  | Import_not_found uri -> Asai.Diagnostic.textf "%a not found" URI.pp uri
  | Unresolved_xmlns prefix ->
    Asai.Diagnostic.textf "Could not resolve prefix `%s` to XML namespace"
      prefix
  | Unresolved_identifier (_, p) ->
    Asai.Diagnostic.textf
      "Unknown binding \\%a. To interpret as a TeX control sequence, \
       explicitly enter TeX mode using #{...}."
      Trie.pp_path p
  | Type_error {got; expected} -> begin
    let expected_msg =
      match expected with
      | [] -> Asai.Diagnostic.textf "An unknown type error ocurred"
      | expected :: [] ->
        Asai.Diagnostic.textf "Expected %s" (show_expected_value expected)
      | _ ->
        Asai.Diagnostic.textf "Expected one of %a"
          (Format.pp_print_list
             ~pp_sep:(fun out () -> Format.fprintf out ", ")
             pp_expected_value)
          expected
    in
    let got_msg =
      match got with
      | None -> Asai.Diagnostic.textf ""
      | Some v -> Asai.Diagnostic.textf " but this is %s" (this_is v)
    in
    let hint =
      match got with
      | Some (Value.Clo (_, _, _)) ->
        Asai.Diagnostic.textf "Did you forget to supply an argument?"
      | Some (Value.Content _)
      | Some (Value.Dx_prop _)
      | Some (Value.Dx_sequent _)
      | Some (Value.Dx_query _)
      | Some (Value.Dx_var _)
      | Some (Value.Dx_const _)
      | Some (Value.Sym _)
      | Some (Value.Obj _)
      | None ->
        Asai.Diagnostic.textf ""
    in
    Asai.Diagnostic.textf "%t%t.\n%t" expected_msg got_msg hint
  end
  | Asset_not_found msg -> Asai.Diagnostic.text msg
  | Unbound_method (mthd, {prototype = _; methods; _}) ->
    let method_names = List.map fst @@ Value.Method_table.to_list methods in
    Asai.Diagnostic.textf "Unbound method %s. Available methods are:@.%a" mthd
      Format.(pp_print_list (fun ppf s -> fprintf ppf "   %s" s))
      method_names
  | Uninterpreted_config_options keys ->
    Asai.Diagnostic.textf "Uninterpreted config option%s: %a"
      (if List.length keys = 1 then ""
       else if List.length keys > 1 then "s"
       else assert false)
      Format.(
        pp_print_list
          ~pp_sep:(fun out () -> fprintf out ", ")
          (fun ppf k ->
            fprintf ppf "%a"
              (pp_print_list
                 ~pp_sep:(fun out () -> fprintf out ".")
                 pp_print_string)
              k))
      keys
  | Using_default_option k ->
    Asai.Diagnostic.textf
      "Configuration option %a is not set. Using default value."
      Format.(
        pp_print_list ~pp_sep:(fun out () -> fprintf out ".") pp_print_string)
      k
  | Required_config_option k ->
    Asai.Diagnostic.textf "Required option %s is not set." k
  | Broken_link {uri; suggestion} -> begin
    match suggestion with
    | None -> Asai.Diagnostic.textf "Potentially broken link to `%a`" URI.pp uri
    | Some suggestion ->
      Asai.Diagnostic.textf
        "Potentially broken link to `%a`; did you mean `%a`?" URI.pp uri URI.pp
        suggestion
  end
  | Reference_to_hidden_tree {uri; kind; tags; source; from} ->
    Asai.Diagnostic.textf
      "%s %s hidden tree `%a`%s, which is hidden by tag%s %s; %s will appear \
       in the public output."
      (match from with None -> "This tree" | Some path -> path)
      kind URI.pp uri
      (match source with None -> "" | Some path -> " (" ^ path ^ ")")
      (if List.length tags = 1 then "" else "s")
      (String.concat ", " tags)
      (if kind = "transcludes" then "its content" else "its title and address")
  | Resource_not_found uri ->
    Asai.Diagnostic.textf "Resource not found: %a" URI.pp uri
  | Duplicate_tree (o1, o2) ->
    let show_origin = function
      | Physical doc -> Lsp.(Uri.to_path @@ Text_document.documentUri doc)
      | Subtree {parent} -> Format.asprintf "%a" pp_identity parent
      | Undefined -> "undefined"
    in
    Asai.Diagnostic.textf "%s@ and@ %s@ use@ the@ same@ URI" (show_origin o1)
      (show_origin o2)
  | Invalid_URI | Asset_has_no_content_address _ | Reference_error _
  | Parse_error | Type_warning | Unbound_fluid_symbol _ | Unbound_variable _
  | Unhandled_case | Transclusion_loop | Internal_error | Configuration_error
  | Initialization_warning | Routing_error | Profiling _ | External_error
  | Current_tree_has_no_uri | IO_error | Log | Missing_argument ->
    Asai.Diagnostic.text ""
