(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_xml_names
open Pure_html

let forester_xmlns = {prefix = "fr"; xmlns = "http://www.forester-notes.org"}
let html_xlmns = {prefix = "html"; xmlns = "http://www.w3.org/1999/xhtml"}
let xml_xmlns = {prefix = "xml"; xmlns = "http://www.w3.org/XML/1998/namespace"}
let reserved_xmlnss = [forester_xmlns; html_xlmns; xml_xmlns]
let null = HTML.null
let null_ = HTML.null_
let conditional b n = if b then n else null []
let optional kont opt = match opt with Some x -> kont x | None -> null []
let optional_ kont opt = match opt with Some x -> kont x | None -> null_
let add_forester_ns name = Format.sprintf "%s:%s" forester_xmlns.prefix name
let add_html_ns name = Format.sprintf "%s:%s" html_xlmns.prefix name
let f_std_tag name = std_tag @@ add_forester_ns name
let f_text_tag name = text_tag @@ add_forester_ns name
let f_void_tag name attrs = std_tag (add_forester_ns name) attrs []
let html_void_tag name attrs = std_tag (add_html_ns name) attrs []
let info = f_std_tag "info"
let tree = f_std_tag "tree"
let numbered = bool_attr "numbered"
let toc = bool_attr "toc"
let expanded = bool_attr "expanded"
let hidden_when_empty = bool_attr "hidden-when-empty"
let show_heading = bool_attr "show-heading"
let show_metadata = bool_attr "show-metadata"
let root = bool_attr "root"
let frontmatter = f_std_tag "frontmatter"
let mainmatter = f_std_tag "mainmatter"
let backmatter = f_std_tag "backmatter"
let taxon attrs = f_std_tag "taxon" attrs
let uri attrs = f_text_tag "uri" attrs
let display_uri attrs = f_text_tag "display-uri" attrs
let route attrs = f_text_tag "route" attrs
let source_path attrs = f_text_tag "source-path" attrs
let href fmt = string_attr "href" fmt
let date = f_std_tag "date"
let last_changed = f_std_tag "last-changed"
let year attrs = f_text_tag "year" attrs
let month attrs = f_text_tag "month" attrs
let day attrs = f_text_tag "day" attrs
let authors = f_std_tag "authors"
let author = f_std_tag "author"
let contributor = f_std_tag "contributor"
let title = f_std_tag "title"
let link = f_std_tag "link"
let type_ fmt = string_attr "type" fmt
let uri_ fmt = string_attr "uri" fmt
let display_uri_ fmt = string_attr "display-uri" fmt
let text_ fmt = string_attr "text" fmt
let number attrs = f_text_tag "number" attrs
let meta = f_std_tag "meta"
let tags = f_std_tag "tags"
let tag = f_std_tag "tag"
let name fmt = string_attr "name" fmt
let tex attrs = f_text_tag ~raw:true "tex" attrs
let display fmt = string_attr "display" fmt
let title_ fmt = string_attr "title" fmt
let ref = f_void_tag "ref"
let number_ fmt = string_attr "number" fmt
let img = html_void_tag "img"
let src fmt = string_attr "src" fmt
let resource = f_std_tag "resource"
let resource_content = f_std_tag "resource-content"
let resource_source attrs = f_text_tag ~raw:true "resource-source" attrs
let resource_part fmt = string_attr "part" fmt
let contextual_number = f_void_tag "contextual-number"
let hash fmt = string_attr "hash" fmt
