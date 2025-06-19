module main

import os
import net.urllib
import encoding.html
import strings
import markdown
import v.scanner
import v.ast
import v.token
import document as doc
import v.pref
import v.util { tabs }

const css_js_assets = ['doc.css', 'normalize.css', 'doc.js', 'dark-mode.js']
const default_theme = os.resource_abs_path('theme')
const link_svg = '<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24"><path d="M0 0h24v24H0z" fill="none"/><path d="M3.9 12c0-1.71 1.39-3.1 3.1-3.1h4V7H7c-2.76 0-5 2.24-5 5s2.24 5 5 5h4v-1.9H7c-1.71 0-3.1-1.39-3.1-3.1zM8 13h8v-2H8v2zm9-6h-4v1.9h4c1.71 0 3.1 1.39 3.1 3.1s-1.39 3.1-3.1 3.1h-4V17h4c2.76 0 5-2.24 5-5s-2.24-5-5-5z"/></svg>'

const single_quote = "'"
const double_quote = '"'
const quote_escape_seq = [single_quote, '', double_quote, '']

enum HighlightTokenTyp {
	unone
	boolean
	builtin
	char
	comment
	function
	keyword
	name
	number
	operator
	punctuation
	string
	// For string interpolation
	opening_string
	string_interp
	partial_string
	closing_string
	symbol
	none
	module_
	prefix
}

struct SearchModuleResult {
	description string
	link        string
}

struct SearchResult {
	prefix      string
	badge       string
	description string
	link        string
}

fn (vd &VDoc) render_search_index(out Output) {
	mut js_search_index := strings.new_builder(200)
	mut js_search_data := strings.new_builder(200)
	js_search_index.write_string('var searchModuleIndex = [\n')
	js_search_data.write_string('var searchModuleData = [\n')
	for i, title in vd.search_module_index {
		data := vd.search_module_data[i]
		js_search_index.write_string('"${title}",\n')
		description := data.description.replace('\n', '').replace('\r', '') // fix multiline js string bug
		js_search_data.write_string('["${description}","${data.link}"],\n')
	}
	js_search_index.writeln('];\n')
	js_search_index.write_string('var searchIndex = [\n')
	js_search_data.writeln('];\n')
	js_search_data.write_string('var searchData = [\n')
	for i, title in vd.search_index {
		data := vd.search_data[i]
		js_search_index.write_string('"${title}",\n')
		// array instead of object to reduce file size
		js_search_data.write_string('["${data.badge}","${data.description}","${data.link}","${data.prefix}"],\n')
	}
	js_search_index.writeln('];\n')
	js_search_data.writeln('];\n')
	out_file_path := os.join_path(out.path, 'search_index.js')
	os.write_file(out_file_path, js_search_index.str() + js_search_data.str()) or { panic(err) }
}

fn (mut vd VDoc) render_static_html(out Output) {
	vd.assets = {
		'doc_css':       vd.get_resource(css_js_assets[0], out)
		'normalize_css': vd.get_resource(css_js_assets[1], out)
		'doc_js':        vd.get_resource(css_js_assets[2], out)
		'dark_mode_js':  vd.get_resource(css_js_assets[3], out)
		'light_icon':    vd.get_resource('light.svg', out)
		'dark_icon':     vd.get_resource('dark.svg', out)
		'menu_icon':     vd.get_resource('menu.svg', out)
		'arrow_icon':    vd.get_resource('arrow.svg', out)
	}
}

fn (vd &VDoc) get_resource(name string, out Output) string {
	cfg := vd.cfg
	path := os.join_path(cfg.theme_dir, name)
	mut res := os.read_file(path) or { panic('vdoc: could not read ${path}') }
	/*
	if minify {
		if name.ends_with('.js') {
			res = js_compress(res)
		} else {
			res = res.split_into_lines().map(it.trim_space()).join('')
		}
	}
	*/
	// TODO: Make SVG inline for now
	if cfg.inline_assets || path.ends_with('.svg') {
		return res
	} else {
		output_path := os.join_path(out.path, name)
		if !os.exists(output_path) {
			println('Generating ${out.typ} in "${output_path}"')
			os.write_file(output_path, res) or { panic(err) }
		}
		return name
	}
}

fn (mut vd VDoc) collect_search_index(out Output) {
	cfg := vd.cfg
	for doc in vd.docs {
		mod := doc.head.name
		vd.search_module_index << mod
		comments := if cfg.include_examples {
			doc.head.merge_comments()
		} else {
			doc.head.merge_comments_without_examples()
		}
		vd.search_module_data << SearchModuleResult{
			description: trim_doc_node_description(mod, comments)
			link:        vd.get_file_name(mod, out)
		}
		for _, dn in doc.contents {
			vd.create_search_results(mod, dn, out)
		}
	}
}

fn (mut vd VDoc) create_search_results(mod string, dn doc.DocNode, out Output) {
	cfg := vd.cfg
	if dn.kind == .const_group {
		return
	}
	comments := if cfg.include_examples {
		dn.merge_comments()
	} else {
		dn.merge_comments_without_examples()
	}
	dn_description := trim_doc_node_description(dn.name, comments)
	vd.search_index << dn.name
	vd.search_data << SearchResult{
		prefix:      if dn.parent_name != '' {
			'${dn.kind} (${dn.parent_name})'
		} else {
			'${dn.kind} '
		}
		description: dn_description
		badge:       mod
		link:        vd.get_file_name(mod, out) + '#' + get_node_id(dn)
	}
	for child in dn.children {
		vd.create_search_results(mod, child, out)
	}
}

fn (vd &VDoc) write_content(cn &doc.DocNode, d &doc.Doc, mut hw strings.Builder) {
	cfg := vd.cfg
	base_dir := os.dir(os.real_path(cfg.input_path))
	file_path_name := if cfg.is_multi {
		cn.file_path.replace('${base_dir}/', '')
	} else {
		os.file_name(cn.file_path)
	}
	src_link := get_src_link(vd.manifest.repo_url, vd.manifest.repo_branch, file_path_name,
		cn.pos.line_nr + 1)
	if cn.content.len != 0 || cn.name == 'Constants' {
		hw.write_string(doc_node_html(cn, src_link, false, cfg.include_examples, d.table))
		hw.write_string('\n')
	}
	for child in cn.children {
		child_file_path_name := child.file_path.replace('${base_dir}/', '')
		child_src_link := get_src_link(vd.manifest.repo_url, vd.manifest.repo_branch,
			child_file_path_name, child.pos.line_nr + 1)
		hw.write_string(doc_node_html(child, child_src_link, false, cfg.include_examples,
			d.table))
		hw.write_string('\n')
	}
}

fn (vd &VDoc) gen_html(d doc.Doc) string {
	cfg := vd.cfg
	mut symbols_toc := strings.new_builder(200)
	mut modules_toc := strings.new_builder(200)
	mut contents := strings.new_builder(200)
	dcs_contents := d.contents.arr()
	// generate toc first
	contents.writeln(doc_node_html(d.head, '', true, cfg.include_examples, d.table))
	if is_module_readme(d.head) {
		write_toc(d.head, mut symbols_toc)
	}
	for cn in dcs_contents {
		vd.write_content(&cn, &d, mut contents)
		write_toc(cn, mut symbols_toc) // write head
	}
	if cfg.html_only_contents {
		// no need for theming, styling etc, useful for testing and for external documentation generators
		return contents.str()
	}

	// write css
	header_name := if cfg.is_multi && vd.docs.len > 1 {
		os.file_name(os.real_path(cfg.input_path))
	} else {
		d.head.name
	}
	// write nav1
	if cfg.is_multi || vd.docs.len > 1 {
		mut used_submod_prefixes := map[string]bool{}
		for dc in vd.docs {
			mut submod_prefix := dc.head.name.all_before('.')
			if index := dc.head.frontmatter['index'] {
				if dc.head.name == 'index' {
					submod_prefix = index
				}
			}
			if used_submod_prefixes[submod_prefix] {
				continue
			}
			used_submod_prefixes[submod_prefix] = true
			mut href_name := './${dc.head.name}.html'
			if dc.head.name in ['README', 'index'] {
				href_name = './index.html'
			} else if submod_prefix !in vd.docs.map(it.head.name) {
				href_name = '#'
			}
			submodules := vd.docs.filter(it.head.name.starts_with(submod_prefix + '.'))
			dropdown := if submodules.len > 0 { vd.assets['arrow_icon'] } else { '' }
			active_class := if dc.head.name == d.head.name { ' active' } else { '' }
			modules_toc.write_string('<li class="open${active_class}">\n<div class="menu-row">${dropdown}<a href="${href_name}">${submod_prefix}</a></div>\n')
			for j, cdoc in submodules {
				if j == 0 {
					modules_toc.write_string('<ul>\n')
				}
				submod_name := cdoc.head.name.all_after(submod_prefix + '.')
				sub_selected_classes := if cdoc.head.name == d.head.name {
					' class="active"'
				} else {
					''
				}
				modules_toc.write_string('<li${sub_selected_classes}><a href="./${cdoc.head.name}.html">${submod_name}</a></li>\n')
				if j == submodules.len - 1 {
					modules_toc.write_string('</ul>\n')
				}
			}
			modules_toc.write_string('</li>\n')
		}
	}
	modules_toc_str := modules_toc.str()
	symbols_toc_str := symbols_toc.str()
	mut result := os.read_file(os.join_path(cfg.theme_dir, 'index.html')) or { panic(err) }
	if cfg.html_no_vhash {
		result = result.replace('{{ version }}', 'latest')
	} else {
		mut version := if vd.manifest.version.len != 0 { vd.manifest.version } else { '' }
		version = [version, @VCURRENTHASH].join(' ')
		result = result.replace('{{ version }}', version)
	}
	result = result.replace('{{ title }}', d.head.name)
	result = result.replace('{{ head_name }}', header_name)
	result = result.replace('{{ light_icon }}', vd.assets['light_icon'])
	result = result.replace('{{ dark_icon }}', vd.assets['dark_icon'])
	result = result.replace('{{ menu_icon }}', vd.assets['menu_icon'])
	if cfg.html_no_assets {
		result = result.replace('{{ head_assets }}', '')
	} else {
		result = result.replace('{{ head_assets }}', if cfg.inline_assets {
			'<style>${vd.assets['doc_css']}</style>
${tabs(2)}<style>${vd.assets['normalize_css']}</style>
${tabs(2)}<script>${vd.assets['dark_mode_js']}</script>'
		} else {
			'<link rel="stylesheet" href="${vd.assets['doc_css']}" />
${tabs(2)}<link rel="stylesheet" href="${vd.assets['normalize_css']}" />
${tabs(2)}<script src="${vd.assets['dark_mode_js']}"></script>'
		})
	}
	if cfg.html_no_toc_urls {
		result = result.replace('{{ toc_links }}', '')
	} else {
		result = result.replace('{{ toc_links }}', if cfg.is_multi || vd.docs.len > 1 {
			modules_toc_str
		} else {
			symbols_toc_str
		})
	}
	result = result.replace('{{ contents }}', contents.str())
	if cfg.html_no_right {
		result = result.replace('{{ right_content }}', '')
	} else {
		result = result.replace('{{ right_content }}', if cfg.is_multi && d.head.name != 'README' {
			'<div class="doc-toc"><ul>${symbols_toc_str}</ul></div>'
		} else {
			''
		})
	}
	if cfg.html_no_footer {
		result = result.replace('{{ footer_content }}', '')
	} else {
		result = result.replace('{{ footer_content }}', gen_footer_text(d, !cfg.no_timestamp))
	}
	if cfg.html_no_assets {
		result = result.replace('{{ footer_assets }}', '')
	} else {
		result = result.replace('{{ footer_assets }}', if cfg.inline_assets {
			'<script>${vd.assets['doc_js']}</script>'
		} else {
			'<script src="${vd.assets['doc_js']}"></script>'
		})
	}
	return result
}

fn get_src_link(repo_url string, repo_branch string, file_name string, line_nr int) string {
	mut url := urllib.parse(repo_url) or { return '' }
	if url.path.len <= 1 || file_name == '' {
		return ''
	}
	url.path = url.path.trim_right('/') + match url.host {
		'github.com' { '/blob/${repo_branch}/${file_name}' }
		'gitlab.com' { '/-/blob/${repo_branch}/${file_name}' }
		'git.sir.ht' { '/tree/${repo_branch}/${file_name}' }
		else { '' }
	}
	if url.path == '/' {
		return ''
	}
	url.fragment = 'L${line_nr}'
	return url.str()
}

fn write_token(tok token.Token, typ HighlightTokenTyp, mut buf strings.Builder) {
	match typ {
		.unone, .operator, .punctuation {
			buf.write_string(tok.kind.str())
		}
		.string_interp {
			// tok.kind.str() for this returns $2 instead of $
			buf.write_byte(`$`)
		}
		.opening_string {
			buf.write_string("'${tok.lit}")
		}
		.closing_string {
			// A string as the next token of the expression
			// inside the string interpolation indicates that
			// this is the closing of string interpolation
			buf.write_string("${tok.lit}'")
		}
		.string {
			buf.write_string("'${tok.lit}'")
		}
		.char {
			buf.write_string('`${tok.lit}`')
		}
		.comment {
			buf.write_string('//')
			if tok.lit != '' && tok.lit[0] == 1 {
				buf.write_string(tok.lit[1..])
			} else {
				buf.write_string(tok.lit)
			}
		}
		else {
			buf.write_string(tok.lit)
		}
	}
}

fn html_highlight(code string, tb &ast.Table) string {
	mut s := scanner.new_scanner(code, .parse_comments, &pref.Preferences{ output_mode: .silent })
	mut tok := s.scan()
	mut next_tok := s.scan()
	mut buf := strings.new_builder(200)
	mut i := 0
	mut inside_string_interp := false
	for i < code.len {
		if i != tok.pos {
			// All characters not detected by the scanner
			// (mostly whitespaces) go here.
			buf.write_u8(code[i])
			i++
			continue
		}

		mut tok_typ := HighlightTokenTyp.unone
		match tok.kind {
			.name {
				if tok.lit in highlight_builtin_types || tb.known_type(tok.lit) {
					tok_typ = .builtin
				} else if next_tok.kind == .lcbr {
					tok_typ = .symbol
				} else if next_tok.kind == .lpar || (!tok.lit[0].is_capital()
					&& next_tok.kind == .lt && next_tok.pos == tok.pos + tok.lit.len) {
					tok_typ = .function
				} else {
					tok_typ = .name
				}
			}
			.comment {
				tok_typ = .comment
			}
			.chartoken {
				tok_typ = .char
			}
			.str_dollar {
				tok_typ = .string_interp
				inside_string_interp = true
			}
			.string {
				if inside_string_interp {
					if next_tok.kind == .str_dollar {
						// the " hello " in "${a} hello ${b} world"
						tok_typ = .partial_string
					} else {
						// the " world" in "${a} hello ${b} world"
						tok_typ = .closing_string
					}

					// NOTE: Do not switch inside_string_interp yet!
					// It will be handy later when we do some special
					// handling in generating code (see code below)
				} else if next_tok.kind == .str_dollar {
					tok_typ = .opening_string
				} else {
					tok_typ = .string
				}
			}
			.number {
				tok_typ = .number
			}
			.key_true, .key_false {
				tok_typ = .boolean
			}
			.lpar, .lcbr, .rpar, .rcbr, .lsbr, .rsbr, .semicolon, .colon, .comma, .dot, .dotdot,
			.ellipsis {
				tok_typ = .punctuation
			}
			else {
				if token.is_key(tok.lit) || token.is_decl(tok.kind) {
					tok_typ = .keyword
				} else if tok.kind.is_assign() || tok.is_unary() || tok.kind.is_relational()
					|| tok.kind.is_infix() || tok.kind.is_postfix() {
					tok_typ = .operator
				}
			}
		}

		if tok_typ in [.unone, .name] {
			write_token(tok, tok_typ, mut buf)
		} else {
			// Special handling for "complex" string literals
			if tok_typ in [.partial_string, .closing_string] && inside_string_interp {
				// rcbr is not rendered when the string on the right
				// side of the expr/string interpolation is not empty.
				// e.g. "${a}.${b}${c}"
				// expectation: "${a}.${b}${c}"
				// reality: "${a.${b}${c}"
				if tok.lit.len != 0 {
					write_token(token.Token{ kind: .rcbr }, .unone, mut buf)
				}

				inside_string_interp = false
			}

			// Properly treat and highlight the "string"-related types
			// as if they are "string" type.
			final_tok_typ := match tok_typ {
				.opening_string, .partial_string, .closing_string { HighlightTokenTyp.string }
				else { tok_typ }
			}

			buf.write_string('<span class="token ${final_tok_typ}">')
			if tok_typ == .string {
				// Make sure to escape html in strings. Otherwise it will be rendered in the
				// html documentation outputs / its style rules will affect the readme.
				buf.write_string("'${html.escape(tok.lit.str())}'")
			} else {
				write_token(tok, tok_typ, mut buf)
			}
			buf.write_string('</span>')
		}

		if next_tok.kind == .eof {
			break
		}

		i = tok.pos + tok.len

		// This is to avoid issues that skips any "unused" tokens
		// For example: Call expr with complex string literals as arg
		if i - 1 == next_tok.pos {
			i--
		}

		tok = next_tok
		next_tok = s.scan()
	}
	return buf.str()
}

fn doc_node_html(dn doc.DocNode, link string, head bool, include_examples bool, tb &ast.Table) string {
	mut dnw := strings.new_builder(200)
	head_tag := if head { 'h1' } else { 'h2' }
	mut renderer := markdown.HtmlRenderer{
		transformer: &MdHtmlCodeHighlighter{
			table: tb
		}
	}
	only_comments_text := dn.merge_comments_without_examples()
	md_content := markdown.render(only_comments_text, mut renderer) or { '' }
	highlighted_code := html_highlight(dn.content, tb)
	node_class := if dn.kind == .const_group { ' const' } else { '' }
	sym_name := get_sym_name(dn)
	mut deprecated_tags := dn.tags.filter(it.starts_with('deprecated'))
	if doc.should_sort {
		deprecated_tags.sort()
	}
	mut tags := dn.tags.filter(!it.starts_with('deprecated'))
	if doc.should_sort {
		tags.sort()
	}
	mut node_id := get_node_id(dn)
	mut hash_link := if !head { ' <a href="#${node_id}">#</a>' } else { '' }
	if head && is_module_readme(dn) {
		node_id = 'readme_${node_id}'
		hash_link = ' <a href="#${node_id}">#</a>'
	}
	dnw.writeln('${tabs(2)}<section id="${node_id}" class="doc-node${node_class}">')
	if dn.name != '' {
		if dn.kind == .const_group {
			dnw.write_string('${tabs(3)}<div class="title"><${head_tag}>${sym_name}${hash_link}</${head_tag}>')
		} else {
			dnw.write_string('${tabs(3)}<div class="title"><${head_tag}>${dn.kind} ${sym_name}${hash_link}</${head_tag}>')
		}
		if link != '' {
			dnw.write_string('<a class="link" rel="noreferrer" target="_blank" href="${link}">${link_svg}</a>')
		}
		dnw.write_string('</div>\n')
	}
	if deprecated_tags.len > 0 {
		attributes := deprecated_tags.map('<div class="attribute attribute-deprecated">${it.replace_each(quote_escape_seq)}</div>\n').join('')
		dnw.writeln('<div class="attributes">${attributes}</div>\n')
	}
	if tags.len > 0 {
		attributes := tags.map('<div class="attribute">${it}</div>').join('')
		dnw.writeln('<div class="attributes">${attributes}</div>')
	}
	if !head && dn.content.len > 0 {
		dnw.writeln('<pre class="signature">\n<code>${highlighted_code}</code></pre>')
	}
	// do not mess with md_content further, its formatting is important, just output it 1:1 !
	dnw.writeln('${md_content}\n')
	// Write examples if any found
	examples := dn.examples()
	if include_examples && examples.len > 0 {
		example_title := if examples.len > 1 { 'Examples' } else { 'Example' }
		dnw.writeln('<section class="doc-node examples"><h4>${example_title}</h4>')
		for example in examples {
			hl_example := html_highlight(example, tb)
			dnw.writeln('<pre>\n<code class="language-v">${hl_example}</code></pre>')
		}
		dnw.writeln('</section>')
	}
	dnw.writeln('</section>')
	dnw_str := dnw.str()
	return dnw_str
}

fn write_toc(dn doc.DocNode, mut toc strings.Builder) {
	mut toc_slug := if dn.name == '' || dn.content.len == 0 { '' } else { slug(dn.name) }
	if toc_slug == '' && dn.children.len > 0 {
		if dn.children[0].name == '' {
			toc_slug = slug(dn.name)
		} else {
			toc_slug = slug(dn.name + '.' + dn.children[0].name)
		}
	}
	if is_module_readme(dn) {
		if dn.comments.len == 0 || (dn.comments.len > 0 && dn.comments[0].text.len == 0) {
			return
		}
		toc.write_string('<li class="open"><a href="#readme_${toc_slug}">README</a>')
	} else if dn.name != 'Constants' {
		toc.write_string('<li class="open"><a href="#${toc_slug}">${dn.kind} ${dn.name}</a>')
		toc.writeln('        <ul>')
		for child in dn.children {
			cname := dn.name + '.' + child.name
			toc.writeln('<li><a href="#${slug(cname)}">${child.kind} ${child.name}</a></li>')
		}
		toc.writeln('</ul>')
	} else {
		toc.write_string('<li class="open"><a href="#${toc_slug}">${dn.name}</a>')
	}
	toc.writeln('</li>')
}

struct MdHtmlCodeHighlighter {
mut:
	language string
	table    &ast.Table
}

fn (f &MdHtmlCodeHighlighter) transform_attribute(p markdown.ParentType, name string, value string) string {
	return markdown.default_html_transformer.transform_attribute(p, name, value)
}

fn (f &MdHtmlCodeHighlighter) transform_content(parent markdown.ParentType, text string) string {
	if parent is markdown.MD_BLOCKTYPE && parent == .md_block_code {
		if f.language == '' {
			return html.escape(text)
		}
		output := html_highlight(text, f.table)
		// Reset the language, so that it will not persist between blocks,
		// and will not be accidentally re-used for the next block, that may be lacking ```language :
		unsafe {
			f.language = ''
		}
		return output
	}
	return text
}

fn (mut f MdHtmlCodeHighlighter) config_set(key string, val string) {
	if key == 'code_language' {
		f.language = val
	}
}
