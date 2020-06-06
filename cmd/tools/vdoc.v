module main

import markdown
import net
import net.urllib
import os
import os.cmdline
import time
import strings
import v.doc
import v.scanner
import v.table
import v.token
import v.vmod

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

const (
	allowed_formats = ['md', 'markdown', 'json', 'text', 'stdout', 'html', 'htm']
	exe_path        = os.executable()
	exe_dir         = os.dir(exe_path)
	res_path        = os.join_path(exe_dir, 'vdoc-resources')
	vexe_path       = os.base_dir(@VEXE)
)

enum OutputType {
	unset
	html
	markdown
	json
	plaintext
	stdout
}

struct DocConfig {
mut:
	pub_only       bool = true
	show_loc       bool = false // for plaintext
	serve_http     bool = false // for html
	is_multi       bool = false
	is_verbose     bool = false
	include_readme bool = false
	inline_assets  bool = false
	output_path    string
	input_path     string
	output_type    OutputType = .unset
	docs           []doc.Doc
	manifest       vmod.Manifest
}

fn slug(title string) string {
	return title.replace(' ', '-')
}

fn open_url(url string) {
	$if windows {
		os.system('start $url')
	}
	$if macos {
		os.system('open $url')
	}
	$if linux {
		os.system('xdg-open $url')
	}
}

fn (mut cfg DocConfig) serve_html() {
	docs := cfg.render()
	def_name := docs.keys()[0]
	server := net.listen(8046) or {
		panic(err)
	}
	println('Serving docs on: http://localhost:8046')
	open_url('http://localhost:8046')
	for {
		con := server.accept() or {
			server.close() or { }
			panic(err)
		}
		s := con.read_line()
		first_line := s.all_before('\n')
		mut filename := def_name
		if first_line.len != 0 {
			data := first_line.split(' ')
			url := urllib.parse(data[1]) or { return }
			filename = if url.path == '/' { def_name } else { url.path.trim_left('/') }
		}
		html := docs[filename]
		con.write('HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n$html') or {
			con.close() or { return }
			return
		}
		con.close() or { return }
	}
}

fn get_src_link(repo_url string, file_name string, line_nr int) string {
	mut url := urllib.parse(repo_url) or {
		return ''
	}
	if url.path.len <= 1 || file_name.len == 0 {
		return ''
	}
	url.path = url.path.trim_right('/') + match url.host {
		'github.com' { '/blob/master/$file_name' }
		'gitlab.com' { '/-/blob/master/$file_name' }
		'git.sir.ht' { '/tree/master/$file_name' }
		else { '' }
	}
	if repo_url.starts_with('https://github.com/vlang/v') && !url.path.contains('master/vlib')  {
		url.path = url.path.replace('/blob/master/$file_name', '/blob/master/vlib/$file_name')
	}
	if url.path == '/' { return '' }
	url.fragment = 'L$line_nr'
	return url.str()
}

fn escape(str string) string {
	return str.replace_each(['"', '\\"', '\r\n', '\\n', '\n', '\\n'])
}

fn (cfg DocConfig) gen_json(idx int) string {
	dcs := cfg.docs[idx]
	mut jw := strings.new_builder(200)
	jw.writeln('{\n\t"module_name": "$dcs.head.name",\n\t"description": "${escape(dcs.head.comment)}",\n\t"contents": [')
	for i, cn in dcs.contents {
		name := cn.name.all_after(dcs.head.name)
		jw.writeln('\t\t{')
		jw.writeln('\t\t\t"name": "$name",')
		jw.writeln('\t\t\t"signature": "${escape(cn.content)}",')
		jw.writeln('\t\t\t"description": "${escape(cn.comment)}"')
		jw.write('\t\t}')
		if i < dcs.contents.len-1 { jw.writeln(',') }
	}
	jw.writeln('\n\t],')
	jw.write('\t"generator": "vdoc",\n\t"time_generated": "${dcs.time_generated.str()}"\n}')
	return jw.str()
}

fn html_highlight(code string, tb &table.Table) string {
	builtin := ['bool', 'string', 'i8', 'i16', 'int', 'i64', 'i128', 'byte', 'u16', 'u32', 'u64', 'u128', 'rune', 'f32', 'f64', 'any_int', 'any_float', 'byteptr', 'voidptr', 'any']
	highlight_code := fn (tok token.Token, typ HighlightTokenTyp) string {
		lit := if typ in [.unone, .operator, .punctuation] { tok.kind.str() } else { tok.lit }
		return if typ in [.unone, .name] { lit } else { '<span class="token $typ">$lit</span>' } 
	}
	s := scanner.new_scanner(code, .parse_comments)
	mut tok := s.scan()
	mut next_tok := s.scan()
	mut buf := strings.new_builder(200)
	mut i := 0
	for i < code.len {
		if i == tok.pos {
			mut tok_typ := HighlightTokenTyp.unone
			match tok.kind {
				.name {
					if tok.lit in builtin {
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
				.lpar, .lcbr, .rpar, .rcbr, .lsbr, 
				.rsbr, .semicolon, .colon, .comma, .dot {
					tok_typ = .punctuation
				}
				else {
					if token.is_key(tok.lit) || token.is_decl(tok.kind) {
						tok_typ = .keyword
					} else if tok.kind == .decl_assign || tok.kind.is_assign() || tok.is_unary() || tok.kind.is_relational() || tok.kind.is_infix() {
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

fn doc_node_html(dd doc.DocNode, link string, head bool, tb &table.Table) string {
	mut dnw := strings.new_builder(200)
	link_svg := '<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24"><path d="M0 0h24v24H0z" fill="none"/><path d="M3.9 12c0-1.71 1.39-3.1 3.1-3.1h4V7H7c-2.76 0-5 2.24-5 5s2.24 5 5 5h4v-1.9H7c-1.71 0-3.1-1.39-3.1-3.1zM8 13h8v-2H8v2zm9-6h-4v1.9h4c1.71 0 3.1 1.39 3.1 3.1s-1.39 3.1-3.1 3.1h-4V17h4c2.76 0 5-2.24 5-5s-2.24-5-5-5z"/></svg>'
	head_tag := if head { 'h1' } else { 'h2' }
	md_content := markdown.to_html(dd.comment)
	hlighted_code := html_highlight(dd.content, tb)
	mut sym_name := dd.name
	if dd.parent_type !in ['void', ''] { 
		sym_name = '${dd.parent_type}.' + sym_name
	}
	node_id := slug(sym_name)
	dnw.writeln('<section id="$node_id" class="doc-node">')
	if dd.name != 'README' {
		dnw.write('<div class="title"><$head_tag>$sym_name <a href="#$node_id">#</a></$head_tag>')
		if link.len != 0 {
			dnw.write('<a class="link" rel="noreferrer" target="_blank" href="$link">$link_svg</a>')
		}
		dnw.write('</div>')
	}
	if head {
		dnw.write(md_content)
	} else {
		dnw.writeln('<pre class="signature language-v"><code class="language-v">$hlighted_code</code></pre>')
		dnw.writeln(md_content)
	}
	dnw.writeln('</section>')
	return dnw.str()
}

fn (cfg DocConfig) gen_html(idx int) string {
	dcs := cfg.docs[idx]
	time_gen := dcs.time_generated.day.str() + ' ' + dcs.time_generated.smonth() + ' ' + dcs.time_generated.year.str() + ' ' + dcs.time_generated.hhmmss()
	mut hw := strings.new_builder(200)
	mut toc := strings.new_builder(200)
	// generate toc first
	for cn in dcs.contents {
		if cn.parent_type !in ['void', ''] { continue }
		toc.write('<li><a href="#${slug(cn.name)}">${cn.name}</a>')
		children := dcs.contents.find_children_of(cn.name)
		if children.len != 0 {
			toc.writeln('        <ul>')
			for child in children {
				cname := cn.name + '.' + child.name
				toc.writeln('<li><a href="#${slug(cname)}">${child.name}</a></li>')
			}
			toc.writeln('</ul>')
		}
		toc.writeln('</li>')
	}	// write head

	// get resources
	doc_css_min := cfg.get_resource('doc.css', true)
	doc_js_min := cfg.get_resource('doc.js', false)
	light_icon := cfg.get_resource('light.svg', true)
	dark_icon := cfg.get_resource('dark.svg', true)
	menu_icon := cfg.get_resource('menu.svg', true)
	arrow_icon := cfg.get_resource('arrow.svg', true)
	v_prism_css := cfg.get_resource('v-prism.css', true)

	hw.write('
	<!DOCTYPE html>
	<html lang="en">
	<head>
    <meta charset="UTF-8">
	<meta http-equiv="x-ua-compatible" content="IE=edge" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>${dcs.head.name} | vdoc</title>')

	// write css
	if cfg.inline_assets {
		hw.write('\n    <style>$v_prism_css</style>')
		hw.write('\n    <style>$doc_css_min</style>')
	} else {
		hw.write('\n	<link rel="stylesheet" href="$v_prism_css" />')
		hw.write('\n	<link rel="stylesheet" href="$doc_css_min" />')
	}

	version := if cfg.manifest.version.len != 0 { cfg.manifest.version } else { '' }
	header_name := if cfg.is_multi && cfg.docs.len > 1 { os.file_name(os.real_path(cfg.input_path)) } else { dcs.head.name }
	// write nav1
	hw.write('
	<body>
	<div id="page">
		<header class="doc-nav hidden">
		<div class="heading-container">
			<div class="heading">
				<input type="text" id="search" placeholder="Search...">
				<div class="module">${header_name}</div>
				<div class="toggle-version-container">
					<span>${version}</span>
					<div id="dark-mode-toggle" role="switch" aria-checked="false" aria-label="Toggle dark mode">$light_icon $dark_icon</div>
				</div>
				$menu_icon
			</div>
		</div>
		<nav class="content hidden">
			<ul>')
	if cfg.is_multi && cfg.docs.len > 1 {
		mut submod_prefix := ''
		for i, doc in cfg.docs {
			if i-1 >= 0 && doc.head.name.starts_with(submod_prefix + '.') {
				continue
			}
			names := doc.head.name.split('.')
			submod_prefix = if names.len > 1 { names[0] } else { doc.head.name }
			href_name := if doc.head.name == 'README' {
				'./index.html'
			} else if submod_prefix !in cfg.docs.map(it.head.name) {
				'#'
			} else {
				'./' + doc.head.name + '.html'
			}
			submodules := cfg.docs.filter(it.head.name.starts_with(submod_prefix + '.'))
			dropdown := if submodules.len > 0 { arrow_icon } else { '' }
			mut is_submodule_open := false
			for _, cdoc in submodules {
				if cdoc.head.name == dcs.head.name {
					is_submodule_open = true
				}
			}
			open_class := if doc.head.name == dcs.head.name || is_submodule_open { ' open' } else { '' }
			active_class := if doc.head.name == dcs.head.name { ' active' } else { '' }
			hw.write('<li class="$open_class$active_class"><div class="menu-row">$dropdown<a href="$href_name">${submod_prefix}</a></div>')
			for j, cdoc in submodules {
				if j == 0 {
					hw.write('<ul>')
				}
				submod_name := cdoc.head.name.all_after(submod_prefix + '.')
				sub_selected_classes := if cdoc.head.name == dcs.head.name { ' class="active"' } else { '' }
				hw.write('<li$sub_selected_classes><a href="./${cdoc.head.name}.html">${submod_name}</a></li>')
				if j == submodules.len - 1 {
					hw.write('</ul>')
				}
			}
			hw.write('</li>')
		}
	} else {
		hw.writeln(toc.str())
	}
	hw.write('</ul>\n</nav>\n</header>')
	hw.write('<div class="doc-container">\n<div class="doc-content">\n')
	hw.write(doc_node_html(dcs.head, '', true, dcs.table))
	for cn in dcs.contents {
		if cn.parent_type !in ['void', ''] { continue }
		base_dir := os.base_dir(os.real_path(cfg.input_path))
		file_path_name := cn.file_path.replace('$base_dir/', '')
		hw.write(doc_node_html(cn, get_src_link(cfg.manifest.repo_url, file_path_name, cn.pos.line), false, dcs.table))

		children := dcs.contents.find_children_of(cn.name)

		if children.len != 0 {
			for child in children {
				child_file_path_name := child.file_path.replace('$base_dir/', '')
				hw.write(doc_node_html(child, get_src_link(cfg.manifest.repo_url, child_file_path_name, child.pos.line), false, dcs.table))
			}
		}
	}
	hw.write('\n<div class="footer">Powered by vdoc. Generated on: $time_gen</div>\n</div>\n')
	if cfg.is_multi && cfg.docs.len > 1 && dcs.head.name != 'README' {
		hw.write('<div class="doc-toc">\n\n<ul>\n${toc.str()}</ul>\n</div>')
	}
	hw.write('</div></div>')
	if cfg.inline_assets {
		hw.write('<script>$doc_js_min</script>')
	} else {
		hw.write('<script src="$doc_js_min"></script>')
	}
	hw.write('</body>
	</html>')
	return hw.str()
}

fn (cfg DocConfig) gen_plaintext(idx int) string {
	dcs := cfg.docs[idx]
	mut pw := strings.new_builder(200)
	pw.writeln('${dcs.head.content}\n')
	for cn in dcs.contents {
		pw.writeln(cn.content)
		if cn.comment.len > 0 {
			pw.writeln('\n' + cn.comment)
		}
		if cfg.show_loc {
			pw.writeln('Location: ${cn.file_path}:${cn.pos.line}:${cn.pos.col}\n\n')
		}
	}
	return pw.str()
}

fn (cfg DocConfig) gen_markdown(idx int, with_toc bool) string {
	dcs := cfg.docs[idx]
	mut hw := strings.new_builder(200)
	mut cw := strings.new_builder(200)
	hw.writeln('# ${dcs.head.content}\n${dcs.head.comment}\n')
	if with_toc {
		hw.writeln('## Contents')
	}
	for cn in dcs.contents {
		name := cn.name.all_after(dcs.head.name+'.')

		if with_toc {
			hw.writeln('- [#$name](${slug(name)})')
		}
		cw.writeln('## $name')
		cw.writeln('```v\n${cn.content}\n```${cn.comment}\n')
		cw.writeln('[\[Return to contents\]](#Contents)\n')
	}
	cw.writeln('#### Generated by vdoc. Last generated: ${dcs.time_generated.str()}')
	return hw.str() + '\n' + cw.str()
}

fn (cfg DocConfig) render() map[string]string {
	mut docs := map[string]string
	for i, doc in cfg.docs {
		mut name := if doc.head.name == 'README' {
			'index'
		} else if !cfg.is_multi && !os.is_dir(cfg.output_path) {
			os.file_name(cfg.output_path)
		} else {
			doc.head.name
		}
		name = name + match cfg.output_type {
			.html { '.html' }
			.markdown { '.md' }
			.json { '.json' }
			else { '.txt' }
		}
		docs[name] = match cfg.output_type {
			.html { cfg.gen_html(i) }
			.markdown { cfg.gen_markdown(i, true) }
			.json { cfg.gen_json(i) }
			else { cfg.gen_plaintext(i) }
		}
	}
	cfg.vprintln('Rendered: ' + docs.keys().str())
	return docs
}

fn (mut cfg DocConfig) generate_docs_from_file() {
	if cfg.output_path.len == 0 {
		if cfg.output_type == .unset {
			cfg.output_type = .stdout
		} else {
			cfg.vprintln('No output path has detected. Using input path instead.')
			cfg.output_path = cfg.input_path
		}
	} else if cfg.output_type == .unset {
		cfg.vprintln('Output path detected. Identifying output type..')
		ext := os.file_ext(cfg.output_path)
		cfg.set_output_type_from_str(ext.all_after('.'))
	}
	if cfg.include_readme && cfg.output_type !in [.html, .stdout] {
		eprintln('vdoc: Including README.md for doc generation is supported on HTML output, or when running directly in the terminal.')
		exit(1)
	}
	is_vlib := 'vlib' in cfg.input_path
	dir_path := if is_vlib {
		vexe_path
	} else if os.is_dir(cfg.input_path) { 
		cfg.input_path
	} else { 
		os.base_dir(cfg.input_path) 
	}
	manifest_path := os.join_path(dir_path, 'v.mod')
	readme_path := os.join_path(dir_path, 'README.md')
	if os.exists(manifest_path) {
		cfg.vprintln('Reading v.mod info from $manifest_path')
		if manifest := vmod.from_file(manifest_path) {
			cfg.manifest = manifest
		}
	}
	if os.exists(readme_path) && cfg.include_readme {
		cfg.vprintln('Reading README file from $readme_path')
		readme_contents := os.read_file(readme_path) or { '' }
        if cfg.output_type == .stdout {
			println(markdown.to_plain(readme_contents))
        }
        if cfg.output_type == .html {
			cfg.docs << doc.Doc{
				head: doc.DocNode{
					name: 'README',
					comment: readme_contents
				}
				time_generated: time.now()
			}
        }
	}
	dirs := if cfg.is_multi { get_modules_list(cfg.input_path) } else { [cfg.input_path] } 
	for dirpath in dirs {
		cfg.vprintln('Generating docs for ${dirpath}...')
		mut dcs := doc.generate(dirpath, cfg.pub_only, !is_vlib) or {
			panic(err)
		}
		if dcs.contents.len == 0 { continue }
		if cfg.pub_only {
			for i, c in dcs.contents {
				dcs.contents[i].content = c.content.all_after('pub ')	
			}
		}
		cfg.docs << dcs
	}
	if cfg.serve_http {
		cfg.serve_html()
		return
	}
	cfg.vprintln('Rendering docs...')
	if cfg.output_path.len == 0 {
		outputs := cfg.render()
		first := outputs.keys()[0]
		println(outputs[first])
	} else {
		if !os.is_dir(cfg.output_path) {
			cfg.output_path = os.real_path('.')
		}
		if cfg.is_multi {
			cfg.output_path = os.join_path(cfg.output_path, '_docs')
			if !os.exists(cfg.output_path) {
				os.mkdir(cfg.output_path) or {
					panic(err)
				}
			} else {
				for fname in ['doc.css', 'v-prism.css', 'doc.js'] {
					os.rm(os.join_path(cfg.output_path, fname))
				}
			}
		}
		outputs := cfg.render()
		for file_name, content in outputs {
			output_path := os.join_path(cfg.output_path, file_name)
			println('Generating ${output_path}...')
			os.write_file(output_path, content)
		}
	}
}

fn (mut cfg DocConfig) set_output_type_from_str(format string) {
	match format {
		'htm', 'html' { 
			cfg.output_type = .html 
		}
		'md', 'markdown' { 
			cfg.output_type = .markdown 
		}
		'json' { 
			cfg.output_type = .json 
		}
		'stdout' {
			cfg.output_type = .stdout
		}
		else { 
			cfg.output_type = .plaintext 
		}
	}
	cfg.vprintln('Setting output type to "$cfg.output_type"')
}

fn (cfg DocConfig) vprintln(str string) {
	if cfg.is_verbose {
		println('vdoc: $str')
	}
}

fn lookup_module(mod string) ?string {
	mod_path := mod.replace('.', os.path_separator)
	compile_dir := os.real_path(os.base_dir('.'))
	modules_dir := os.join_path(compile_dir, 'modules', mod_path)
	vlib_path := os.join_path(vexe_path, 'vlib', mod_path)
	vmodules_path := os.join_path(os.home_dir(), '.vmodules', mod_path)
	paths := [modules_dir, vlib_path, vmodules_path]
	for path in paths {
		if os.is_dir_empty(path) { continue }
		return path
	}
	return error('vdoc: Module "${mod}" not found.')
}

fn get_modules_list(path string) []string {
	files := os.walk_ext(path, 'v')
	mut dirs := []string{}
	for file in files {
		if 'test' in file || 'js' in file || 'x64' in file || 'bare' in file || 'uiold' in file || 'vweb' in file { continue }
		dirname := os.base_dir(file)
		if dirname in dirs { continue }
		dirs << dirname
	}
	dirs.sort()
	return dirs
}

fn (cfg DocConfig) get_resource(name string, minify bool) string {
	path := os.join_path(res_path, name)
	mut res := os.read_file(path) or { panic('could not read $path') }
	if minify {
		res = res.replace('\n', ' ')
	}
	// TODO: Make SVG inline for now
	if cfg.inline_assets || path.ends_with('.svg') {
		return res
	} else {
		output_path := os.join_path(cfg.output_path, name)
		if !os.exists(output_path) {
			println('Generating ${output_path}...')
			os.write_file(output_path, res)
		}
		return name
	}
}

fn main() {
	args := os.args[2..]
	if args.len == 0 || args[0] == 'help' {
		os.system('${@VEXE} help doc')
		exit(0)
	}
	mut cfg := DocConfig{
		manifest: vmod.Manifest{ repo_url: '' }
	}
	for i := 0; i < args.len; i++ {
		arg := args[i]
		current_args := args[i..]
		match arg {
			'-all' {
				cfg.pub_only = false
			}
			'-f' {
				format := cmdline.option(current_args, '-f', '')
				allowed_str := allowed_formats.join(', ')
				if format !in allowed_formats {
					eprintln('vdoc: "$format" is not a valid format. Only $allowed_str are allowed.')
					exit(1)
				}
				cfg.set_output_type_from_str(format)
				i++
			}
			'-inline-assets' {
				cfg.inline_assets = true
			}
			'-l' {
				cfg.show_loc = true
			}
			'-m' {
				cfg.is_multi = true
			}
			'-o' {
				opath := cmdline.option(current_args, '-o', '')
				cfg.output_path = os.real_path(opath)
				i++
			}
			'-s' {
				cfg.inline_assets = true
				cfg.serve_http = true
				cfg.output_type = .html
			}
			'-r' {
				cfg.include_readme = true
			}
			'-v' {
				cfg.is_verbose = true
			}
			else {
				cfg.input_path = arg
				break
			}
		}
	}
	if cfg.input_path.len == 0 {
		eprintln('vdoc: No input path found.')
		exit(1)
	}
	is_path := cfg.input_path.ends_with('.v') || cfg.input_path.split(os.path_separator).len > 1 || cfg.input_path == '.'
	if cfg.input_path == 'vlib' {
		cfg.is_multi = true
		cfg.input_path = os.join_path(vexe_path, 'vlib')
	} else if !is_path {
		cfg.vprintln('Input "$cfg.input_path" is not a valid path. Looking for modules named "$cfg.input_path"...')
		mod_path := lookup_module(cfg.input_path) or {
			eprintln(err)
			exit(1)
		}
		cfg.input_path = mod_path
	}
	cfg.generate_docs_from_file()
}
