module main

import io
import net
import net.urllib
import v.scanner
import v.table
import v.token

const (
	css_js_assets   = ['doc.css', 'normalize.css', 'doc.js', 'dark-mode.js']
	favicons_path   = os.join_path(res_path, 'favicons')
	html_content    = '<!DOCTYPE html>
<html lang="en">
	<head>
		<meta charset="UTF-8">
		<meta http-equiv="x-ua-compatible" content="IE=edge" />
		<meta name="viewport" content="width=device-width, initial-scale=1.0">
		<title>{{ title }} | vdoc</title>
		<link rel="preconnect" href="https://fonts.gstatic.com">
		<link href="https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=swap" rel="stylesheet">
		<link href="https://fonts.googleapis.com/css2?family=Work+Sans:wght@400;500;600&display=swap" rel="stylesheet">
		<link rel="apple-touch-icon" sizes="180x180" href="apple-touch-icon.png">
		<link rel="icon" type="image/png" sizes="32x32" href="favicon-32x32.png">
		<link rel="icon" type="image/png" sizes="16x16" href="favicon-16x16.png">
		<link rel="manifest" href="site.webmanifest">
		<link rel="mask-icon" href="safari-pinned-tab.svg" color="#5bbad5">
		<meta name="msapplication-TileColor" content="#da532c">
		<meta name="theme-color" content="#ffffff">
		{{ head_assets }}
	</head>
	<body>
		<div id="page">
			<header class="doc-nav hidden">
				<div class="heading-container">
					<div class="heading">
						<input type="text" id="search" placeholder="Search... (beta)" autocomplete="off">
						<div class="module">{{ head_name }}</div>
						<div class="toggle-version-container">
							<span>{{ version }}</span>
							<div id="dark-mode-toggle" role="switch" aria-checked="false" aria-label="Toggle dark mode">{{ light_icon }}{{ dark_icon }}</div>
						</div>
						{{ menu_icon }}
					</div>
				</div>
				<nav class="search"></nav>
				<nav class="content hidden">
					<ul>
						{{ toc_links }}
					</ul>
				</nav>
			</header>
			<div class="doc-scrollview">
				<div class="doc-container">
					<div class="doc-content">
{{ contents }}
						<div class="footer">
							{{ footer_content }}
						</div>
					</div>
					{{ right_content }}
				</div>
			</div>
		</div>
		{{ footer_assets }}
		<script async src="search_index.js" type="text/javascript"></script>
	</body>
</html>'
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
}

fn (mut cfg DocConfig) serve_html() {
	cfg.render_static()
	docs := cfg.render()
	dkeys := docs.keys()
	if dkeys.len < 1 {
		eprintln('no documentation created, the module has no `pub` functions')
		exit(1)
	}
	def_name := docs.keys()[0]
	server_url := 'http://localhost:' + cfg.server_port.str()
	server := net.listen_tcp(cfg.server_port) or { panic(err) }
	println('Serving docs on: $server_url')
	if cfg.open_docs {
		open_url(server_url)
	}
	content_type := match cfg.output_type {
		.html { 'text/html' }
		.markdown { 'text/markdown' }
		.json { 'application/json' }
		else { 'text/plain' }
	}
	server_context := VdocHttpServerContext{
		docs: docs
		content_type: content_type
		default_filename: def_name
	}
	for {
		mut conn := server.accept() or {
			server.close() or { }
			panic(err)
		}
		handle_http_connection(mut conn, server_context)
		conn.close() or { eprintln('error closing the connection: $err') }
	}
}

struct VdocHttpServerContext {
	docs             map[string]string
	content_type     string
	default_filename string
}

fn handle_http_connection(mut con net.TcpConn, ctx &VdocHttpServerContext) {
	mut reader := io.new_buffered_reader(reader: io.make_reader(con))
	first_line := reader.read_line() or {
		send_http_response(mut con, 501, ctx.content_type, 'bad request')
		return
	}
	request_parts := first_line.split(' ')
	if request_parts.len != 3 {
		send_http_response(mut con, 501, ctx.content_type, 'bad request')
		return
	}
	urlpath := request_parts[1]
	filename := if urlpath == '/' {
		ctx.default_filename.trim_left('/')
	} else {
		urlpath.trim_left('/')
	}
	if ctx.docs[filename].len == 0 {
		send_http_response(mut con, 404, ctx.content_type, 'file not found')
		return
	}
	send_http_response(mut con, 200, ctx.content_type, ctx.docs[filename])
}

fn send_http_response(mut con net.TcpConn, http_code int, content_type string, html string) {
	content_length := html.len.str()
	shttp_code := http_code.str()
	mut http_response := strings.new_builder(20000)
	http_response.write('HTTP/1.1 ')
	http_response.write(shttp_code)
	http_response.write(' OK\r\n')
	http_response.write('Server: VDoc\r\n')
	http_response.write('Content-Type: ')
	http_response.write(content_type)
	http_response.write('\r\n')
	http_response.write('Content-Length: ')
	http_response.write(content_length)
	http_response.write('\r\n')
	http_response.write('Connection: close\r\n')
	http_response.write('\r\n')
	http_response.write(html)
	sresponse := http_response.str()
	con.write_str(sresponse) or { eprintln('error sending http response: $err') }
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
		js.write(trimmed)
	}
	js_str := js.str()
	js.free()
	return js_str
}

fn html_highlight(code string, tb &table.Table) string {
	builtin := ['bool', 'string', 'i8', 'i16', 'int', 'i64', 'i128', 'byte', 'u16', 'u32', 'u64',
		'u128', 'rune', 'f32', 'f64', 'any_int', 'any_float', 'byteptr', 'voidptr', 'any']
	highlight_code := fn (tok token.Token, typ HighlightTokenTyp) string {
		lit := if typ in [.unone, .operator, .punctuation] {
			tok.kind.str()
		} else if typ == .string {
			"'$tok.lit'"
		} else if typ == .char {
			'`$tok.lit`'
		} else {
			tok.lit
		}
		return if typ in [.unone, .name] {
			lit
		} else {
			'<span class="token $typ">$lit</span>'
		}
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
					} else if next_tok.kind == .lpar {
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
				.lpar, .lcbr, .rpar, .rcbr, .lsbr, .rsbr, .semicolon, .colon, .comma, .dot {
					tok_typ = .punctuation
				}
				else {
					if token.is_key(tok.lit) || token.is_decl(tok.kind) {
						tok_typ = .keyword
					} else if tok.kind == .decl_assign || tok.kind.is_assign() || tok.is_unary() ||
						tok.kind.is_relational() || tok.kind.is_infix() {
						tok_typ = .operator
					}
				}
			}
			buf.write(highlight_code(tok, tok_typ))
			if next_tok.kind != .eof {
				i = tok.pos + tok.len
				tok = next_tok
				next_tok = s.scan()
			} else {
				break
			}
		} else {
			buf.write_b(code[i])
			i++
		}
	}
	return buf.str()
}

fn doc_node_html(dd doc.DocNode, link string, head bool, include_examples bool, tb &table.Table) string {
	mut dnw := strings.new_builder(200)
	link_svg := '<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24"><path d="M0 0h24v24H0z" fill="none"/><path d="M3.9 12c0-1.71 1.39-3.1 3.1-3.1h4V7H7c-2.76 0-5 2.24-5 5s2.24 5 5 5h4v-1.9H7c-1.71 0-3.1-1.39-3.1-3.1zM8 13h8v-2H8v2zm9-6h-4v1.9h4c1.71 0 3.1 1.39 3.1 3.1s-1.39 3.1-3.1 3.1h-4V17h4c2.76 0 5-2.24 5-5s-2.24-5-5-5z"/></svg>'
	head_tag := if head { 'h1' } else { 'h2' }
	comments := dd.merge_comments_without_examples()
	md_content := markdown.to_html(html_tag_escape(comments))
	hlighted_code := html_highlight(dd.content, tb)
	node_class := if dd.kind == .const_group { ' const' } else { '' }
	sym_name := get_sym_name(dd)
	mut node_id := get_node_id(dd)
	mut hash_link := if !head { ' <a href="#$node_id">#</a>' } else { '' }
	if head && is_module_readme(dd) {
		node_id = 'readme_$node_id'
		hash_link = ' <a href="#$node_id">#</a>'
	}
	dnw.writeln('${tabs[1]}<section id="$node_id" class="doc-node$node_class">')
	if dd.name.len > 0 {
		if dd.kind == .const_group {
			dnw.write('${tabs[2]}<div class="title"><$head_tag>$sym_name$hash_link</$head_tag>')
		} else {
			dnw.write('${tabs[2]}<div class="title"><$head_tag>$dd.kind $sym_name$hash_link</$head_tag>')
		}
		if link.len != 0 {
			dnw.write('<a class="link" rel="noreferrer" target="_blank" href="$link">$link_svg</a>')
		}
		dnw.write('</div>')
	}
	if !head && dd.content.len > 0 {
		dnw.writeln('<pre class="signature"><code>$hlighted_code</code></pre>')
	}
	// do not mess with md_content further, its formatting is important, just output it 1:1 !
	dnw.writeln('$md_content\n')
	// Write examples if any found
	examples := dd.examples()
	if include_examples && examples.len > 0 {
		example_title := if examples.len > 1 { 'Examples' } else { 'Example' }
		dnw.writeln('<section class="doc-node examples"><h4>$example_title</h4>')
		for example in examples {
			// hl_example := html_highlight(example, tb)
			dnw.writeln('<pre><code class="language-v">$example</code></pre>')
		}
		dnw.writeln('</section>')
	}
	dnw.writeln('</section>')
	dnw_str := dnw.str()
	defer {
		dnw.free()
	}
	return dnw_str
}

fn html_tag_escape(str string) string {
	return str.replace_each(['<', '&lt;', '>', '&gt;'])
}

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
		toc.write('<li class="open"><a href="#readme_$toc_slug">README</a>')
	} else if dn.name != 'Constants' {
		toc.write('<li class="open"><a href="#$toc_slug">$dn.kind $dn.name</a>')
		toc.writeln('        <ul>')
		for child in dn.children {
			cname := dn.name + '.' + child.name
			toc.writeln('<li><a href="#${slug(cname)}">$child.kind $child.name</a></li>')
		}
		toc.writeln('</ul>')
	} else {
		toc.write('<li class="open"><a href="#$toc_slug">$dn.name</a>')
	}
	toc.writeln('</li>')
}


fn (cfg DocConfig) write_content(cn &doc.DocNode, dcs &doc.Doc, mut hw strings.Builder) {
	base_dir := os.dir(os.real_path(cfg.input_path))
	file_path_name := if cfg.is_multi {
		cn.file_path.replace('$base_dir/', '')
	} else {
		os.file_name(cn.file_path)
	}
	src_link := get_src_link(cfg.manifest.repo_url, file_path_name, cn.pos.line)
	if cn.content.len != 0 || (cn.name == 'Constants') {
		hw.write(doc_node_html(cn, src_link, false, cfg.include_examples, dcs.table))
	}
	for child in cn.children {
		child_file_path_name := child.file_path.replace('$base_dir/', '')
		child_src_link := get_src_link(cfg.manifest.repo_url, child_file_path_name, child.pos.line)
		hw.write(doc_node_html(child, child_src_link, false, cfg.include_examples, dcs.table))
	}
}

fn (cfg DocConfig) gen_html(idx int) string {
	mut symbols_toc := strings.new_builder(200)
	mut modules_toc := strings.new_builder(200)
	mut contents := strings.new_builder(200)
	dcs := cfg.docs[idx]
	dcs_contents := dcs.contents.arr()
	// generate toc first
	contents.writeln(doc_node_html(dcs.head, '', true, cfg.include_examples, dcs.table))
	if is_module_readme(dcs.head) {
		write_toc(dcs.head, mut symbols_toc)
	}
	for cn in dcs_contents {
		cfg.write_content(&cn, &dcs, mut contents)
		write_toc(cn, mut symbols_toc)
	} // write head
	// write css
	version := if cfg.manifest.version.len != 0 { cfg.manifest.version } else { '' }
	header_name := if cfg.is_multi && cfg.docs.len > 1 {
		os.file_name(os.real_path(cfg.input_path))
	} else {
		dcs.head.name
	}
	// write nav1
	if cfg.is_multi || cfg.docs.len > 1 {
		mut submod_prefix := ''
		for i, doc in cfg.docs {
			if i - 1 >= 0 && doc.head.name.starts_with(submod_prefix + '.') {
				continue
			}
			names := doc.head.name.split('.')
			submod_prefix = if names.len > 1 { names[0] } else { doc.head.name }
			mut href_name := './${doc.head.name}.html'
			if (cfg.is_vlib && doc.head.name == 'builtin' && !cfg.include_readme) ||
				doc.head.name == 'README' {
				href_name = './index.html'
			} else if submod_prefix !in cfg.docs.map(it.head.name) {
				href_name = '#'
			}
			submodules := cfg.docs.filter(it.head.name.starts_with(submod_prefix + '.'))
			dropdown := if submodules.len > 0 { cfg.assets['arrow_icon'] } else { '' }
			active_class := if doc.head.name == dcs.head.name { ' active' } else { '' }
			modules_toc.write('<li class="open$active_class"><div class="menu-row">$dropdown<a href="$href_name">$submod_prefix</a></div>')
			for j, cdoc in submodules {
				if j == 0 {
					modules_toc.write('<ul>')
				}
				submod_name := cdoc.head.name.all_after(submod_prefix + '.')
				sub_selected_classes := if cdoc.head.name == dcs.head.name {
					' class="active"'
				} else {
					''
				}
				modules_toc.write('<li$sub_selected_classes><a href="./${cdoc.head.name}.html">$submod_name</a></li>')
				if j == submodules.len - 1 {
					modules_toc.write('</ul>')
				}
			}
			modules_toc.write('</li>')
		}
	}
	modules_toc_str := modules_toc.str()
	symbols_toc_str := symbols_toc.str()
	modules_toc.free()
	symbols_toc.free()
	return html_content.replace('{{ title }}', dcs.head.name).replace('{{ head_name }}',
		header_name).replace('{{ version }}', version).replace('{{ light_icon }}', cfg.assets['light_icon']).replace('{{ dark_icon }}',
		cfg.assets['dark_icon']).replace('{{ menu_icon }}', cfg.assets['menu_icon']).replace('{{ head_assets }}',
		if cfg.inline_assets {
		'\n${tabs[0]}<style>' + cfg.assets['doc_css'] + '</style>\n${tabs[0]}<style>' + cfg.assets['normalize_css'] +
			'</style>\n${tabs[0]}<script>' + cfg.assets['dark_mode_js'] + '</script>'
	} else {
		'\n${tabs[0]}<link rel="stylesheet" href="' + cfg.assets['doc_css'] + '" />\n${tabs[0]}<link rel="stylesheet" href="' +
			cfg.assets['normalize_css'] + '" />\n${tabs[0]}<script src="' + cfg.assets['dark_mode_js'] + '"></script>'
	}).replace('{{ toc_links }}', if cfg.is_multi || cfg.docs.len > 1 {
		modules_toc_str
	} else {
		symbols_toc_str
	}).replace('{{ contents }}', contents.str()).replace('{{ right_content }}', if cfg.is_multi &&
		cfg.docs.len > 1 && dcs.head.name != 'README' {
		'<div class="doc-toc"><ul>' + symbols_toc_str + '</ul></div>'
	} else {
		''
	}).replace('{{ footer_content }}', cfg.gen_footer_text(idx)).replace('{{ footer_assets }}',
		if cfg.inline_assets {
		'<script>' + cfg.assets['doc_js'] + '</script>'
	} else {
		'<script src="' + cfg.assets['doc_js'] + '"></script>'
	})
}

fn (cfg DocConfig) gen_plaintext(idx int) string {
	dcs := cfg.docs[idx]
	mut pw := strings.new_builder(200)
	pw.writeln('$dcs.head.content\n')
	comments := if cfg.include_examples {
		dcs.head.merge_comments()
	} else {
		dcs.head.merge_comments_without_examples()
	}
	if comments.trim_space().len > 0 && !cfg.pub_only {
		pw.writeln(comments.split_into_lines().map('    ' + it).join('\n'))
	}
	cfg.write_plaintext_content(dcs.contents.arr(), mut pw)
	return pw.str()
}
