module main

import os
import net.urllib
import strings
import markdown
import regex
import v.scanner
import v.ast
import v.token
import v.doc
import v.pref

const (
	css_js_assets         = ['doc.css', 'normalize.css', 'doc.js', 'dark-mode.js']
	default_theme         = os.resource_abs_path('theme')
	link_svg              = '<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24"><path d="M0 0h24v24H0z" fill="none"/><path d="M3.9 12c0-1.71 1.39-3.1 3.1-3.1h4V7H7c-2.76 0-5 2.24-5 5s2.24 5 5 5h4v-1.9H7c-1.71 0-3.1-1.39-3.1-3.1zM8 13h8v-2H8v2zm9-6h-4v1.9h4c1.71 0 3.1 1.39 3.1 3.1s-1.39 3.1-3.1 3.1h-4V17h4c2.76 0 5-2.24 5-5s-2.24-5-5-5z"/></svg>'

	single_quote          = "'"
	double_quote          = '"'
	no_quotes_replacement = [single_quote, '', double_quote, '']
)

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
	symbol
	none_
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

fn (vd VDoc) render_search_index(out Output) {
	mut js_search_index := strings.new_builder(200)
	mut js_search_data := strings.new_builder(200)
	js_search_index.write_string('var searchModuleIndex = [')
	js_search_data.write_string('var searchModuleData = [')
	for i, title in vd.search_module_index {
		data := vd.search_module_data[i]
		js_search_index.write_string('"$title",')
		js_search_data.write_string('["$data.description","$data.link"],')
	}
	js_search_index.writeln('];')
	js_search_index.write_string('var searchIndex = [')
	js_search_data.writeln('];')
	js_search_data.write_string('var searchData = [')
	for i, title in vd.search_index {
		data := vd.search_data[i]
		js_search_index.write_string('"$title",')
		// array instead of object to reduce file size
		js_search_data.write_string('["$data.badge","$data.description","$data.link","$data.prefix"],')
	}
	js_search_index.writeln('];')
	js_search_data.writeln('];')
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

fn (vd VDoc) get_resource(name string, out Output) string {
	cfg := vd.cfg
	path := os.join_path(cfg.theme_dir, name)
	mut res := os.read_file(path) or { panic('vdoc: could not read $path') }
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
			println('Generating $out.typ in "$output_path"')
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
			description: trim_doc_node_description(comments)
			link: vd.get_file_name(mod, out)
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
	dn_description := trim_doc_node_description(comments)
	vd.search_index << dn.name
	vd.search_data << SearchResult{
		prefix: if dn.parent_name != '' { '$dn.kind ($dn.parent_name)' } else { '$dn.kind ' }
		description: dn_description
		badge: mod
		link: vd.get_file_name(mod, out) + '#' + get_node_id(dn)
	}
	for child in dn.children {
		vd.create_search_results(mod, child, out)
	}
}

fn (vd VDoc) write_content(cn &doc.DocNode, d &doc.Doc, mut hw strings.Builder) {
	cfg := vd.cfg
	base_dir := os.dir(os.real_path(cfg.input_path))
	file_path_name := if cfg.is_multi {
		cn.file_path.replace('$base_dir/', '')
	} else {
		os.file_name(cn.file_path)
	}
	src_link := get_src_link(vd.manifest.repo_url, file_path_name, cn.pos.line_nr + 1)
	if cn.content.len != 0 || (cn.name == 'Constants') {
		hw.write_string(doc_node_html(cn, src_link, false, cfg.include_examples, d.table))
	}
	for child in cn.children {
		child_file_path_name := child.file_path.replace('$base_dir/', '')
		child_src_link := get_src_link(vd.manifest.repo_url, child_file_path_name,
			child.pos.line_nr + 1)
		hw.write_string(doc_node_html(child, child_src_link, false, cfg.include_examples,
			d.table))
	}
}

fn (vd VDoc) gen_html(d doc.Doc) string {
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
		write_toc(cn, mut symbols_toc)
	} // write head
	// write css
	mut version := if vd.manifest.version.len != 0 { vd.manifest.version } else { '' }
	version = [version, @VHASH].join(' ')
	header_name := if cfg.is_multi && vd.docs.len > 1 {
		os.file_name(os.real_path(cfg.input_path))
	} else {
		d.head.name
	}
	// write nav1
	if cfg.is_multi || vd.docs.len > 1 {
		mut submod_prefix := ''
		for i, dc in vd.docs {
			if i - 1 >= 0 && dc.head.name.starts_with(submod_prefix + '.') {
				continue
			}
			names := dc.head.name.split('.')
			submod_prefix = if names.len > 1 { names[0] } else { dc.head.name }
			mut href_name := './${dc.head.name}.html'
			if (cfg.is_vlib && dc.head.name == 'builtin' && !cfg.include_readme)
				|| dc.head.name == 'README' {
				href_name = './index.html'
			} else if submod_prefix !in vd.docs.map(it.head.name) {
				href_name = '#'
			}
			submodules := vd.docs.filter(it.head.name.starts_with(submod_prefix + '.'))
			dropdown := if submodules.len > 0 { vd.assets['arrow_icon'] } else { '' }
			active_class := if dc.head.name == d.head.name { ' active' } else { '' }
			modules_toc.write_string('<li class="open$active_class"><div class="menu-row">$dropdown<a href="$href_name">$submod_prefix</a></div>')
			for j, cdoc in submodules {
				if j == 0 {
					modules_toc.write_string('<ul>')
				}
				submod_name := cdoc.head.name.all_after(submod_prefix + '.')
				sub_selected_classes := if cdoc.head.name == d.head.name {
					' class="active"'
				} else {
					''
				}
				modules_toc.write_string('<li$sub_selected_classes><a href="./${cdoc.head.name}.html">$submod_name</a></li>')
				if j == submodules.len - 1 {
					modules_toc.write_string('</ul>')
				}
			}
			modules_toc.write_string('</li>')
		}
	}
	modules_toc_str := modules_toc.str()
	symbols_toc_str := symbols_toc.str()
	result := (os.read_file(os.join_path(cfg.theme_dir, 'index.html')) or { panic(err) }).replace('{{ title }}',
		d.head.name).replace('{{ head_name }}', header_name).replace('{{ version }}',
		version).replace('{{ light_icon }}', vd.assets['light_icon']).replace('{{ dark_icon }}',
		vd.assets['dark_icon']).replace('{{ menu_icon }}', vd.assets['menu_icon']).replace('{{ head_assets }}',
		if cfg.inline_assets {
		'\n${tabs[0]}<style>' + vd.assets['doc_css'] + '</style>\n${tabs[0]}<style>' +
			vd.assets['normalize_css'] + '</style>\n${tabs[0]}<script>' +
			vd.assets['dark_mode_js'] + '</script>'
	} else {
		'\n${tabs[0]}<link rel="stylesheet" href="' + vd.assets['doc_css'] +
			'" />\n${tabs[0]}<link rel="stylesheet" href="' + vd.assets['normalize_css'] +
			'" />\n${tabs[0]}<script src="' + vd.assets['dark_mode_js'] + '"></script>'
	}).replace('{{ toc_links }}', if cfg.is_multi || vd.docs.len > 1 {
		modules_toc_str
	} else {
		symbols_toc_str
	}).replace('{{ contents }}', contents.str()).replace('{{ right_content }}', if cfg.is_multi
		&& vd.docs.len > 1 && d.head.name != 'README' {
		'<div class="doc-toc"><ul>' + symbols_toc_str + '</ul></div>'
	} else {
		''
	}).replace('{{ footer_content }}', gen_footer_text(d, !cfg.no_timestamp)).replace('{{ footer_assets }}',
		if cfg.inline_assets {
		'<script>' + vd.assets['doc_js'] + '</script>'
	} else {
		'<script src="' + vd.assets['doc_js'] + '"></script>'
	})
	return result
}

fn get_src_link(repo_url string, file_name string, line_nr int) string {
	mut url := urllib.parse(repo_url) or { return '' }
	if url.path.len <= 1 || file_name.len == 0 {
		return ''
	}
	url.path = url.path.trim_right('/') + match url.host {
		'github.com' { '/blob/master/$file_name' }
		'gitlab.com' { '/-/blob/master/$file_name' }
		'git.sir.ht' { '/tree/master/$file_name' }
		else { '' }
	}
	if url.path == '/' {
		return ''
	}
	url.fragment = 'L$line_nr'
	return url.str()
}

fn html_highlight(code string, tb &ast.Table) string {
	builtin := ['bool', 'string', 'i8', 'i16', 'int', 'i64', 'i128', 'byte', 'u16', 'u32', 'u64',
		'u128', 'rune', 'f32', 'f64', 'int_literal', 'float_literal', 'byteptr', 'voidptr', 'any']
	highlight_code := fn (tok token.Token, typ HighlightTokenTyp) string {
		lit := if typ in [.unone, .operator, .punctuation] {
			tok.kind.str()
		} else if typ == .string {
			"'$tok.lit'"
		} else if typ == .char {
			'`$tok.lit`'
		} else if typ == .comment {
			if tok.lit[0] == 1 { '//${tok.lit[1..]}' } else { '//$tok.lit' }
		} else {
			tok.lit
		}
		if typ in [.unone, .name] {
			return lit
		}
		return '<span class="token $typ">$lit</span>'
	}
	mut s := scanner.new_scanner(code, .parse_comments, &pref.Preferences{})
	mut tok := s.scan()
	mut next_tok := s.scan()
	mut buf := strings.new_builder(200)
	mut i := 0
	for i < code.len {
		if i == tok.pos {
			mut tok_typ := HighlightTokenTyp.unone
			match tok.kind {
				.name {
					if tok.lit in builtin || tb.known_type(tok.lit) {
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
				.string {
					tok_typ = .string
				}
				.number {
					tok_typ = .number
				}
				.key_true, .key_false {
					tok_typ = .boolean
				}
				.lpar, .lcbr, .rpar, .rcbr, .lsbr, .rsbr, .semicolon, .colon, .comma, .dot,
				.dotdot, .ellipsis {
					tok_typ = .punctuation
				}
				else {
					if token.is_key(tok.lit) || token.is_decl(tok.kind) {
						tok_typ = .keyword
					} else if tok.kind == .decl_assign || tok.kind.is_assign() || tok.is_unary()
						|| tok.kind.is_relational() || tok.kind.is_infix() || tok.kind.is_postfix() {
						tok_typ = .operator
					}
				}
			}
			buf.write_string(highlight_code(tok, tok_typ))
			if next_tok.kind != .eof {
				i = tok.pos + tok.len
				tok = next_tok
				next_tok = s.scan()
			} else {
				break
			}
		} else {
			buf.write_u8(code[i])
			i++
		}
	}
	return buf.str()
}

fn doc_node_html(dn doc.DocNode, link string, head bool, include_examples bool, tb &ast.Table) string {
	mut dnw := strings.new_builder(200)
	head_tag := if head { 'h1' } else { 'h2' }
	comments := dn.merge_comments_without_examples()
	// Allow README.md to go through unescaped except for script tags
	escaped_html := if head && is_module_readme(dn) {
		// Strip markdown [TOC] directives, since we generate our own.
		stripped := comments.replace('[TOC]', '')
		markdown_escape_script_tags(stripped)
	} else {
		html_tag_escape(comments)
	}
	md_content := markdown.to_html(escaped_html)
	highlighted_code := html_highlight(dn.content, tb)
	node_class := if dn.kind == .const_group { ' const' } else { '' }
	sym_name := get_sym_name(dn)
	mut deprecated_tags := dn.tags.filter(it.starts_with('deprecated'))
	deprecated_tags.sort()
	mut tags := dn.tags.filter(!it.starts_with('deprecated'))
	tags.sort()
	mut node_id := get_node_id(dn)
	mut hash_link := if !head { ' <a href="#$node_id">#</a>' } else { '' }
	if head && is_module_readme(dn) {
		node_id = 'readme_$node_id'
		hash_link = ' <a href="#$node_id">#</a>'
	}
	dnw.writeln('${tabs[1]}<section id="$node_id" class="doc-node$node_class">')
	if dn.name.len > 0 {
		if dn.kind == .const_group {
			dnw.write_string('${tabs[2]}<div class="title"><$head_tag>$sym_name$hash_link</$head_tag>')
		} else {
			dnw.write_string('${tabs[2]}<div class="title"><$head_tag>$dn.kind $sym_name$hash_link</$head_tag>')
		}
		if link.len != 0 {
			dnw.write_string('<a class="link" rel="noreferrer" target="_blank" href="$link">$link_svg</a>')
		}
		dnw.write_string('</div>')
	}
	if deprecated_tags.len > 0 {
		attributes := deprecated_tags.map('<div class="attribute attribute-deprecated">${no_quotes(it)}</div>').join('')
		dnw.writeln('<div class="attributes">$attributes</div>')
	}
	if tags.len > 0 {
		attributes := tags.map('<div class="attribute">$it</div>').join('')
		dnw.writeln('<div class="attributes">$attributes</div>')
	}
	if !head && dn.content.len > 0 {
		dnw.writeln('<pre class="signature"><code>$highlighted_code</code></pre>')
	}
	// do not mess with md_content further, its formatting is important, just output it 1:1 !
	dnw.writeln('$md_content\n')
	// Write examples if any found
	examples := dn.examples()
	if include_examples && examples.len > 0 {
		example_title := if examples.len > 1 { 'Examples' } else { 'Example' }
		dnw.writeln('<section class="doc-node examples"><h4>$example_title</h4>')
		for example in examples {
			hl_example := html_highlight(example, tb)
			dnw.writeln('<pre><code class="language-v">$hl_example</code></pre>')
		}
		dnw.writeln('</section>')
	}
	dnw.writeln('</section>')
	dnw_str := dnw.str()
	return dnw_str
}

fn html_tag_escape(str string) string {
	excaped_string := str.replace_each(['<', '&lt;', '>', '&gt;'])
	mut re := regex.regex_opt(r'`.+[(&lt;)(&gt;)].+`') or { regex.RE{} }
	if re.find_all_str(excaped_string).len > 0 {
		return str
	}
	return excaped_string
}

/*
fn js_compress(str string) string {
	mut js := strings.new_builder(200)
	lines := str.split_into_lines()
	rules := [') {', ' = ', ', ', '{ ', ' }', ' (', '; ', ' + ', ' < ', ' - ', ' || ', ' var',
		': ', ' >= ', ' && ', ' else if', ' === ', ' !== ', ' else ']
	clean := ['){', '=', ',', '{', '}', '(', ';', '+', '<', '-', '||', 'var', ':', '>=', '&&',
		'else if', '===', '!==', 'else']
	for line in lines {
		mut trimmed := line.trim_space()
		if trimmed.starts_with('//') || (trimmed.starts_with('/*') && trimmed.ends_with('*/')) {
			continue
		}
		for i in 0 .. rules.len - 1 {
			trimmed = trimmed.replace(rules[i], clean[i])
		}
		js.write_string(trimmed)
	}
	js_str := js.str()
	return js_str
}
*/
fn write_toc(dn doc.DocNode, mut toc strings.Builder) {
	mut toc_slug := if dn.name.len == 0 || dn.content.len == 0 { '' } else { slug(dn.name) }
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
		toc.write_string('<li class="open"><a href="#readme_$toc_slug">README</a>')
	} else if dn.name != 'Constants' {
		toc.write_string('<li class="open"><a href="#$toc_slug">$dn.kind $dn.name</a>')
		toc.writeln('        <ul>')
		for child in dn.children {
			cname := dn.name + '.' + child.name
			toc.writeln('<li><a href="#${slug(cname)}">$child.kind $child.name</a></li>')
		}
		toc.writeln('</ul>')
	} else {
		toc.write_string('<li class="open"><a href="#$toc_slug">$dn.name</a>')
	}
	toc.writeln('</li>')
}

fn no_quotes(s string) string {
	return s.replace_each(no_quotes_replacement)
}
