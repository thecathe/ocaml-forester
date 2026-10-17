(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_xml_names
open Forester_core
open Forester_compiler
open State.Syntax

open struct
  module T = Types
  module P = Pure_html
  module X = Xml_forester
end

let local_path_components (config : Config.t) (uri : URI.t) =
  let host = Option.get @@ URI.host uri in
  let base_host = Option.get @@ URI.host config.url in
  if host = base_host then URI.stripped_path_components uri
  else "foreign" :: host :: URI.stripped_path_components uri

let local_base_url_string (config : Config.t) =
  let path = URI.path_components config.url in
  String.concat "/" path

let route (forest : State.t) uri : URI.t =
  match forest.={uri} with
  | None -> uri
  | Some tree -> begin
    match Tree.to_evaluated tree with
    | Some {redirect_uri = Some target; _} -> target
    | Some evaluated when evaluated.route_locally ->
      let path = "" :: local_path_components forest.config uri in
      URI.make ~path ()
    | _ -> uri
  end

let mainmatter_cache = Hashtbl.create 1000

type env = {
  forest: State.t;
  in_backmatter: bool;
  uri: URI.t option;
  loops: Loop_detection.t;
  xmlns: Xmlns.t;
}

(* Reported once per (referring page, hidden target, kind) per build. *)
let reported_hidden_references : (URI.t option * URI.t * string, unit) Hashtbl.t =
  Hashtbl.create 16

let range ~env =
  let@ uri = Option.bind env.uri in
  let@ path = Option.map @~ State.source_path_of_uri uri env.forest in
  let position =
    Range.{source = `File path; offset = 0; start_of_line = 0; line_num = 0}
  in
  Range.make (position, position)

let render_xml_qname qname =
  match qname.prefix with
  | "" -> qname.uname
  | _ -> Format.sprintf "%s:%s" qname.prefix qname.uname

let render_xml_attr ~env T.{key; value} =
  let str_value =
    Plain_text_client.string_of_content ~forest:env.forest
      ~router:(route env.forest) value
  in
  P.string_attr (render_xml_qname key) "%s" str_value

let render_xmlns_prefix ({prefix; xmlns} : Forester_xml_names.xmlns_attr) =
  let attr = match prefix with "" -> "xmlns" | _ -> "xmlns:" ^ prefix in
  P.string_attr attr "%s" xmlns

let render_section_flags (dict : T.section_flags) =
  [
    X.optional_ X.show_heading dict.header_shown;
    X.optional_ X.show_metadata dict.metadata_shown;
    X.optional_ X.hidden_when_empty dict.hidden_when_empty;
    X.optional_ X.expanded dict.expanded;
    X.optional_ X.toc dict.included_in_toc;
    X.optional_ X.numbered dict.numbered;
  ]

let rec render_section ~env (section : T.content T.section) : P.node =
  X.tree
    (render_section_flags section.flags)
    [
      render_frontmatter ~env section.frontmatter;
      begin
        let env = {env with uri = section.frontmatter.uri} in
        X.mainmatter []
        @@
        if Loop_detection.have_seen_uri_opt section.frontmatter.uri env.loops
        then
          [X.info [] [P.txt "Transclusion loop detected, rendering stopped."]]
        else
          render_mainmatter
            ~env:
              {
                env with
                loops =
                  Loop_detection.add_seen_uri_opt section.frontmatter.uri
                    env.loops;
              }
            section
      end;
    ]

and render_mainmatter ~env (section : T.content T.section) =
  match section.frontmatter.uri with
  | None -> render_content ~env section.mainmatter
  | Some uri -> begin
    match Hashtbl.find_opt mainmatter_cache uri with
    | None ->
      let nodes = render_content ~env section.mainmatter in
      Hashtbl.add mainmatter_cache uri nodes;
      nodes
    | Some nodes -> nodes
  end

and render_frontmatter ~env (frontmatter : T.content T.frontmatter) : P.node =
  let result =
    X.frontmatter []
      [
        render_attributions ~env frontmatter.uri frontmatter.attributions;
        render_dates ~env frontmatter.dates;
        X.conditional env.forest.dev
        @@ X.optional (X.source_path [] "%s") frontmatter.source_path;
        X.optional
          (fun uri -> X.uri [] "%s" @@ URI.to_string uri)
          frontmatter.uri;
        X.optional
          (fun uri ->
            X.display_uri [] "%s"
            @@ URI.display_path_string ~base:env.forest.config.url uri)
          frontmatter.uri;
        X.optional (X.route [] "%s")
        @@ Option.map
             (Fun.compose URI.href_string (route env.forest))
             frontmatter.uri;
        begin match frontmatter.title with
        | None -> X.null []
        | Some _ ->
          let title =
            State.get_expanded_title ?scope:env.uri frontmatter env.forest
          in
          X.title
            [
              X.text_ "%s"
              @@ Plain_text_client.string_of_content ~forest:env.forest
                   ~router:(route env.forest) title;
            ]
          @@ render_content ~env title
        end;
        begin match frontmatter.taxon with
        | None -> X.null []
        | Some taxon ->
          (* Link the taxon to its generated page, when that exists. *)
          let href =
            let@ name =
              Option.bind (State.text_of_simple_content taxon)
            in
            let uri =
              URI_scheme.named_uri ~base:env.forest.config.url
                (Config.taxon_page_address (String.trim name))
            in
            Option.map
              (fun _ -> X.href "%s" @@ URI.href_string @@ route env.forest uri)
              (State.get_article uri env.forest)
          in
          X.taxon (Option.to_list href) @@ render_content ~env taxon
        end;
        X.null @@ List.map (render_meta ~env) frontmatter.metas;
        render_tags ~env frontmatter;
      ]
  in
  result

(* Tags a tree carries. A tag with `page = true` links to its generated page
   (address `Config.tag_page_address`) when that page actually exists. *)
and render_tags ~env (frontmatter : _ T.frontmatter) =
  let tags =
    let@ vertex = List.filter_map @~ frontmatter.tags in
    match vertex with
    | T.Content_vertex content ->
      Option.map String.trim (State.text_of_simple_content content)
    | T.Uri_vertex _ -> None
  in
  if tags = [] then X.null []
  else
    let render tag =
      let page =
        let@ rule =
          Option.bind
            (List.find_opt
               (fun (r : Config.tag_rule) -> r.tag = tag && r.page)
               env.forest.config.tag_rules)
        in
        let uri =
          URI_scheme.named_uri ~base:env.forest.config.url
            (Config.tag_page_address rule.tag)
        in
        Option.map (fun _ -> uri) (State.get_article uri env.forest)
      in
      X.tag
        (X.name "%s" tag
        :: (match page with
           | Some uri ->
             [X.href "%s" @@ URI.href_string @@ route env.forest uri]
           | None -> []))
        []
    in
    X.tags [] @@ List.map render tags

and render_meta ~env (key, body) =
  X.meta [X.name "%s" key] @@ render_content ~env body

and render_content ~env (Content content : T.content) : P.node list =
  match content with
  | T.Text txt0 :: T.Text txt1 :: content ->
    render_content ~env (Content (T.Text (txt0 ^ txt1) :: content))
  | node :: content ->
    let xs = render_content_node ~env node in
    let ys = render_content ~env (Content content) in
    xs @ ys
  | [] -> []

and render_content_node ~env (node : 'a T.content_node) : P.node list =
  match node with
  | Text str -> [P.txt "%s" str]
  | CDATA str -> [P.txt ~raw:true "<![CDATA[%s]]>" str]
  | Uri uri ->
    [P.txt "%s" (URI.display_path_string ~base:env.forest.config.url uri)]
  | Route_of_uri uri -> [P.txt "%s" (URI.href_string (route env.forest uri))]
  | Xml_elt elt ->
    let xmlns_attrs = Xmlns.xmlns_attrs_for_elt elt env.xmlns in
    let env = {env with xmlns = Xmlns.extend xmlns_attrs env.xmlns} in
    let content = render_content ~env elt.content in
    [
      P.std_tag
        (render_xml_qname elt.name)
        (List.map render_xmlns_prefix xmlns_attrs
        @ List.map (render_xml_attr ~env) elt.attrs)
        content;
    ]
  | Transclude transclusion -> render_transclusion ~env transclusion
  | Contextual_number uri ->
    let custom_number =
      let@ resource = Option.bind @@ env.forest.@{uri} in
      match resource with
      | T.Article article -> article.frontmatter.number
      | _ -> None
    in
    begin match custom_number with
    | None ->
      [
        X.contextual_number
          [
            X.uri_ "%s" @@ URI.to_string uri;
            X.display_uri_ "%s"
            @@ URI.display_path_string ~base:env.forest.config.url uri;
          ];
      ]
    | Some num -> [P.txt "%s" num]
    end
  | Link link -> render_link ~env link
  | Results_of_datalog_query q ->
    let article_to_section =
      T.article_to_section
        ~flags:
          {
            T.default_section_flags with
            expanded = Some false;
            numbered = Some false;
            included_in_toc = Some false;
            metadata_shown = Some true;
          }
    in
    let results = Forest.run_datalog_query env.forest.graphs q in
    let@ article =
      List.map @~ Forest_util.get_sorted_articles env.forest results
    in
    render_section ~env @@ article_to_section article
  | Section section -> [render_section ~env section]
  | KaTeX (mode, content) ->
    let display = match mode with Inline -> "inline" | Display -> "block" in
    let body = Format.asprintf "%a" TeX_like.pp_content content in
    [X.tex [X.display "%s" display] "<![CDATA[%s]]>" body]
  | Artefact resource -> [render_artefact ~env resource]
  | Datalog_script _ -> []

and render_artefact ~env (resource : T.content T.artefact) =
  X.resource
    [X.hash "%s" resource.hash]
    [
      X.resource_content [] @@ render_content ~env resource.content;
      render_resource_sources resource.sources;
    ]

and render_resource_sources sources =
  X.null @@ List.map render_resource_source sources

and render_resource_source source =
  X.resource_source
    [X.type_ "%s" source.type_; X.resource_part "%s" source.part]
    "<![CDATA[%s]]>" source.source

(* A public page that links to or transcludes a tree hidden by a `hide` tag
   still gets built (that is allowed), but it leaks the hidden tree's title
   or content, so say so loudly. Not reported inside generated backmatter,
   which already filters hidden trees. *)
and warn_if_hidden_reference ~env ~kind uri =
  let rendering_hidden_tree =
    match env.uri with
    | Some current -> Option.is_some (State.hide_tags_of_uri env.forest current)
    | None -> false
  in
  if (not env.in_backmatter) && not rendering_hidden_tree then
    match State.hide_tags_of_uri env.forest uri with
    | None -> ()
    | Some tags ->
      let key = (env.uri, uri, kind) in
      if not (Hashtbl.mem reported_hidden_references key) then begin
        Hashtbl.add reported_hidden_references key ();
        let path_of uri =
          (* `resolver` maps every loaded tree to its native path. *)
          match URI.Tbl.find_opt env.forest.resolver uri with
          | Some path -> Some path
          | None -> State.source_path_of_uri uri env.forest
        in
        Reporter.emit ?loc:(range ~env)
          (Reference_to_hidden_tree
             {
               uri;
               kind;
               tags;
               source = path_of uri;
               from = Option.bind env.uri path_of;
             })
      end

and render_transclusion ~env (transclusion : T.transclusion) : P.node list =
  warn_if_hidden_reference ~env ~kind:"transcludes" transclusion.href;
  match State.get_content_of_transclusion transclusion env.forest with
  | None ->
    Reporter.fatal ?loc:(range ~env) (Resource_not_found transclusion.href)
  | Some content -> render_content ~env content

and render_link ~env (link : T.content T.link) : P.node list =
  if State.is_unpublished env.forest link.href then
    (* An unpublished tree (its taxon's rule, no redirect) has no page and no
       address worth showing: the link is just its text. *)
    render_content ~env link.content
  else render_published_link ~env link

and render_published_link ~env (link : T.content T.link) : P.node list =
  warn_if_hidden_reference ~env ~kind:"links to" link.href;
  let article_opt = State.get_article link.href env.forest in
  (* A redirect-flagged local tree is still a known article (`article_opt`
     is `Some`), but it must be rendered exactly like a genuine external
     link: an external `type_`, and no `uri`/`display-uri` attributes
     exposing its real local address (the `href` itself is already handled
     correctly by `route`, which returns the redirect target). *)
  let redirect_target =
    match env.forest.={link.href} with
    | None -> None
    | Some tree -> (
      match Tree.to_evaluated tree with
      | Some {redirect_uri; _} -> redirect_uri
      | None -> None)
  in
  let attrs =
    match article_opt with
    | None ->
      begin if not env.in_backmatter then
        match State.suggestion_for_uri link.href env.forest with
        | Ok -> ()
        | Not_found {suggestion} ->
          Reporter.emit ?loc:(range ~env)
          @@ Broken_link {uri = link.href; suggestion}
      end;
      [
        X.href "%s" @@ URI.href_string @@ route env.forest link.href;
        X.type_ "external";
      ]
    | Some article ->
      let canonical_uri =
        match redirect_target with
        | Some _ -> None
        | None -> article.frontmatter.uri
      in
      [
        X.href "%s" @@ URI.href_string @@ route env.forest link.href;
        X.title_ "%s"
        @@ Plain_text_client.string_of_content ~forest:env.forest
             ~router:(route env.forest)
        @@ State.get_expanded_title ?scope:env.uri article.frontmatter
             env.forest;
        X.optional_ (X.uri_ "%s") @@ Option.map URI.to_string canonical_uri;
        X.optional_ (X.display_uri_ "%s")
        @@ Option.map
             (URI.display_path_string ~base:env.forest.config.url)
             canonical_uri;
        X.type_ (match redirect_target with Some _ -> "external" | None -> "local");
      ]
  in
  [X.link attrs @@ render_content ~env link.content]

and render_attributions ~env (scope : URI.t option)
    (primary_attributions : _ T.attribution list) =
  X.authors []
  @@ List.map (render_attribution ~env)
  @@ Forest_util.collect_attributions env.forest scope primary_attributions

and render_attribution ~env (attrib : _ T.attribution) =
  let tag =
    match attrib.role with Author -> X.author | Contributor -> X.contributor
  in
  tag [] @@ render_attribution_vertex ~env attrib.vertex

and render_attribution_vertex ~env vtx =
  match vtx with
  | T.Uri_vertex href ->
    let content =
      T.Content
        [T.Transclude {href; target = Title {empty_when_untitled = false}}]
    in
    render_link ~env T.{href; content}
  | T.Content_vertex content -> render_content ~env content

and render_dates ~env dates = X.null @@ List.map (render_date ~env) dates

and render_date ~env (date : Human_datetime.t) =
  let config = env.forest.config in
  let href_attr =
    let str =
      Format.asprintf "%a" Human_datetime.pp (Human_datetime.drop_time date)
    in
    let uri = URI_scheme.named_uri ~base:config.url str in
    match State.get_article uri env.forest with
    | None -> X.null_
    | Some _ -> X.href "%s" @@ URI.href_string @@ route env.forest uri
  in
  X.date [href_attr]
    [
      X.year [] "%i" (Human_datetime.year date);
      Human_datetime.month date |> X.optional @@ X.month [] "%i";
      Human_datetime.day date |> X.optional @@ X.day [] "%i";
    ]

let render_article (forest : State.t) (article : T.content T.article) : P.node =
  let before = Unix.gettimeofday () in
  let@ () =
   fun kont ->
    let result = kont () in
    let after = Unix.gettimeofday () in
    let elapsed = after -. before in
    if elapsed > 0.1 then
      Logs.debug (fun m ->
          m "[Performance] rendering %a took %f seconds"
            Format.(pp_print_option URI.pp)
            article.frontmatter.uri elapsed);
    result
  in
  let config = forest.config in
  let env =
    {
      forest;
      in_backmatter = false;
      uri = article.frontmatter.uri;
      loops = Loop_detection.empty;
      xmlns = Xmlns.init ~reserved:X.reserved_xmlnss;
    }
  in
  X.tree
    begin
      List.map render_xmlns_prefix X.reserved_xmlnss
      @ [
          X.optional_ X.root
          @@ begin
            let@ uri = Option.map @~ article.frontmatter.uri in
            URI.equal (Config.home_uri config) uri
          end;
          P.string_attr "base-url" "%s" (local_base_url_string config);
        ]
    end
    [
      render_frontmatter ~env article.frontmatter;
      X.mainmatter []
      @@ begin
        render_mainmatter
          ~env:
            {
              env with
              loops =
                Loop_detection.add_seen_uri_opt article.frontmatter.uri
                  env.loops;
            }
        @@ T.article_to_section article
      end;
      X.backmatter []
      @@ render_content ~env:{env with in_backmatter = true} article.backmatter;
    ]

let pp_xml ~(forest : State.t) ?stylesheet fmt (article : _ T.article) =
  Format.fprintf fmt {|<?xml version="1.0" encoding="UTF-8"?>|};
  Format.pp_print_newline fmt ();
  begin
    let@ xsl_path = Option.iter @~ stylesheet in
    Format.fprintf fmt "<?xml-stylesheet type=\"text/xsl\" href=\"%s%s\"?>"
      (local_base_url_string forest.config)
      xsl_path
  end;
  Format.pp_print_newline fmt ();
  P.pp_xml fmt @@ render_article forest article
