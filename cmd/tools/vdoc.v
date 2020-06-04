module main

import markdown
import net
import net.urllib
import os
import os.cmdline
import strings
import v.doc
import v.util
import v.vmod

const (
	exe_path = os.executable()
	exe_dir  = os.dir(exe_path)
	res_path = os.join_path(exe_dir, 'vdoc-resources')
)

enum OutputType {
	html
	markdown
	json
	plaintext
	stdout
}

struct DocConfig {
	pub_only bool = true
	show_loc bool = false // for plaintext
	serve_http bool = false // for html
	is_multi bool = false
	include_readme bool = false
mut:
	opath string
	src_path string
	docs []doc.Doc
	manifest vmod.Manifest
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
	docs := cfg.multi_render(.html)
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
		name := cn.name[dcs.head.name.len+1..]
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

fn (cfg DocConfig) gen_html(idx int) string {
	dcs := cfg.docs[idx]
	mut hw := strings.new_builder(200)
	mut toc := strings.new_builder(200)
	mut doc_node_html := fn (dd doc.DocNode, link string, head bool) string {
		mut dnw := strings.new_builder(200)
		link_svg := '<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24"><path d="M0 0h24v24H0z" fill="none"/><path d="M3.9 12c0-1.71 1.39-3.1 3.1-3.1h4V7H7c-2.76 0-5 2.24-5 5s2.24 5 5 5h4v-1.9H7c-1.71 0-3.1-1.39-3.1-3.1zM8 13h8v-2H8v2zm9-6h-4v1.9h4c1.71 0 3.1 1.39 3.1 3.1s-1.39 3.1-3.1 3.1h-4V17h4c2.76 0 5-2.24 5-5s-2.24-5-5-5z"/></svg>'
		head_tag := if head { 'h1' } else { 'h2' }
		md_content := markdown.to_html(dd.comment)
		dnw.writeln('<section id="${slug(dd.name)}" class="doc-node">')
		if dd.name != 'README' {
			dnw.write('<div class="title"><$head_tag>${dd.name} <a href="#${slug(dd.name)}">#</a></$head_tag>')
			if link.len != 0 {
				dnw.write('<a class="link" target="_blank" href="$link">$link_svg</a>')
			}
			dnw.write('</div>')
		}
		if head {
			dnw.write(md_content)
		} else {
			dnw.writeln('<pre><code class="signature">${dd.content}</code></pre>')
			dnw.writeln(md_content)
		}
		dnw.writeln('</section>')
		return dnw.str()
	}
	// generate toc first
	for cn in dcs.contents {
		if cn.parent_type !in ['void', ''] { continue }
		toc.write('<li><a href="#${slug(cn.name)}">${cn.name}</a>')
		children := dcs.contents.find_children_of(cn.name)
		if children.len != 0 {
			toc.writeln('        <ul>')
			for child in children {
				toc.writeln('<li><a href="#${slug(child.name)}">${child.name}</a></li>')
			}
			toc.writeln('</ul>')
		}
		toc.writeln('</li>')
	}	// write head
	hw.write('
	<!DOCTYPE html>
	<html lang="en">
	<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>${dcs.head.name} | vdoc</title>
	<link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&family=Source+Code+Pro:wght@500&display=swap" rel="stylesheet">
	<link rel="stylesheet" href="https://necolas.github.io/normalize.css/8.0.1/normalize.css">')

	// get resources
	doc_css_min := get_resource('doc.css', true)
	light_icon := get_resource('light.svg', true)
	dark_icon := get_resource('dark.svg', true)
	menu_icon := get_resource('menu.svg', true)

	// write css
	hw.write('<style>$doc_css_min</style>')
	version := if cfg.manifest.version.len != 0 { cfg.manifest.version } else { '' }
	header_name := if cfg.is_multi && cfg.docs.len > 1 { os.file_name(os.real_path(cfg.src_path)) } else { dcs.head.name }
	// write nav1
	hw.write('
	<body>
	<div id="page">
		<header class="doc-nav hidden">
		<div class="heading-container">
			<div class="heading">
				<div class="module">${header_name}</div>
				<div class="toggle-version-container">
					<span>${version}</span>
					<div id="dark-mode-toggle" role="checkbox">$light_icon $dark_icon</div>
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
			class_css := if doc.head.name == dcs.head.name { 'class="font-bold" ' } else { '' }
			href_name := if doc.head.name == 'README' {
				'./index.html'
			} else if submod_prefix !in cfg.docs.map(it.head.name) {
				'#'
			} else {
				'./' + doc.head.name + '.html'
			}
			submodules := cfg.docs.filter(it.head.name.starts_with(submod_prefix + '.'))
			hw.write('<li><a ${class_css}href="$href_name">${submod_prefix}</a>')
			for j, cdoc in submodules {
				if j == 0 {
					hw.write('<ul>')
				}
				submod_name := cdoc.head.name.all_after(submod_prefix + '.')
				hw.write('<li><a ${class_css}href="./${cdoc.head.name}.html">${submod_name}</a></li>')
				if j == submodules.len-1 {
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
	hw.write(doc_node_html(dcs.head, '', true))
	for cn in dcs.contents {
		if cn.parent_type !in ['void', ''] { continue }
		base_dir := os.base_dir(os.real_path(cfg.src_path))
		file_path_name := cn.file_path.replace('$base_dir/', '')
		hw.write(doc_node_html(cn, get_src_link(cfg.manifest.repo_url, file_path_name, cn.pos.line), false))

		children := dcs.contents.find_children_of(cn.name)

		if children.len != 0 {
			for child in children {
				child_file_path_name := child.file_path.replace('$base_dir/', '')
				hw.write(doc_node_html(child, get_src_link(cfg.manifest.repo_url, child_file_path_name, child.pos.line), false))
			}
		}
	}
	hw.write('\n</div>\n')
	if cfg.is_multi && cfg.docs.len > 1 && dcs.head.name != 'README' {
		hw.write('<div class="doc-toc">\n\n<ul>\n${toc.str()}</ul>\n</div>')
	}
	doc_js_min := get_resource('doc.js', true)
	hw.write('</div></div><script>$doc_js_min</script>
	</body>
	</html>')
	return hw.str()
}

fn (cfg DocConfig) gen_plaintext(idx int) string {
	dcs := cfg.docs[idx]
	mut pw := strings.new_builder(200)
	head_lines := '='.repeat(dcs.head.content.len)
	pw.writeln('${dcs.head.content}\n$head_lines\n')
	for cn in dcs.contents {
		pw.writeln(cn.content)
		if cn.comment.len > 0 {
			pw.writeln('\n' + cn.comment)
		}
		if cfg.show_loc {
			pw.writeln('Location: ${cn.file_path}:${cn.pos.line}:${cn.pos.col}\n\n')
		}
	}
	pw.writeln('Generated on $dcs.time_generated')
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

fn (cfg DocConfig) multi_render(output_type OutputType) map[string]string {
	mut docs := map[string]string
	for i, doc in cfg.docs {
		mut name := if doc.head.name == 'README' {
			'index'
		} else if cfg.docs.len == 1 && !os.is_dir(cfg.opath) {
			os.dir(os.file_name(cfg.opath))
		} else {
			doc.head.name
		}
		name = name + match output_type {
			.html { '.html' }
			.markdown { '.md' }
			.json { '.json' }
			else { '.txt' }
		}
		docs[name] = match output_type {
			.html { cfg.gen_html(i) }
			.markdown { cfg.gen_markdown(i, true) }
			.json { cfg.gen_json(i) }
			else { cfg.gen_plaintext(i) }
		}
	}
	return docs
}

fn (mut config DocConfig) generate_docs_from_file() {
	if config.opath.len != 0 {
		config.opath = os.join_path(os.real_path(os.base_dir(config.opath)), os.file_name(config.opath))
	}
	println(config.opath)
	mut output_type := OutputType.plaintext
	// identify output type
	if config.serve_http {
		output_type = .html
	} else if config.opath.len == 0 {
		output_type = .stdout
	} else {
		ext := os.file_ext(config.opath)
		if ext in ['.md', '.markdown'] || config.opath in [':md:', ':markdown:'] {
			output_type = .markdown
		} else if ext in ['.html', '.htm'] || config.opath == ':html:' {
			output_type = .html
		} else if ext == '.json' || config.opath == ':json:' {
			output_type = .json
		} else {
			output_type = .plaintext
		}
	}
	if config.include_readme && output_type !in [.html, .stdout] {
		eprintln('vdoc: Including README.md for doc generation is supported on HTML output, or when running directly in the terminal.')
		exit(1)
	}
	mut manifest_path := os.join_path(if os.is_dir(config.src_path) { config.src_path } else { os.base_dir(config.src_path) }, 'v.mod')
	if os.exists(manifest_path) && 'vlib' !in config.src_path {
		if manifest := vmod.from_file(manifest_path) {
			config.manifest = manifest
		}
	}
	if 'vlib' in config.src_path {
		config.manifest.version = util.v_version
		config.manifest.repo_url = 'https://github.com/vlang/v'
	}
	readme_path := if 'vlib' in config.src_path {
		os.join_path(os.base_dir(@VEXE), 'README.md')
	} else {
		os.join_path(config.src_path, 'README.md')
	}
	// check README.md
	if os.exists(readme_path) && config.include_readme {
		readme_contents := os.read_file(readme_path) or { '' }
        if output_type == .stdout {
			println(readme_contents)
        }
        if output_type == .html {
			config.docs << doc.Doc{
				head: doc.DocNode{
					name: 'README',
					comment: readme_contents
				}
			}
        }
	}
	if config.is_multi {
		dirs := get_modules_list(config.src_path)
		for dirpath in dirs {
			dcs := doc.generate(dirpath, config.pub_only, 'vlib' !in config.src_path) or {
				panic(err)
			}
			if dcs.contents.len == 0 { continue }
			config.docs << dcs
		}
	} else {
		dcs := doc.generate(config.src_path, config.pub_only, 'vlib' !in config.src_path) or {
			panic(err)
		}
		config.docs << dcs
	}
	if config.serve_http {
		config.serve_html()
		return
	}
	outputs := config.multi_render(output_type)
	if output_type == .stdout || (config.opath.starts_with(':') && config.opath.ends_with(':')) {
		first := outputs.keys()[0]
		println(outputs[first])
	} else {
		if !os.is_dir(config.opath) {
			config.opath = os.base_dir(config.opath)
		}
		if config.is_multi {
			config.opath = os.join_path(config.opath, '_docs')
			if !os.exists(config.opath) {
				os.mkdir(config.opath) or {
					panic(err)
				}
			}
		}
		for file_name, content in outputs {
			opath := os.join_path(config.opath, file_name)
			println('Generating ${opath}...')
			os.write_file(opath, content)
		}
	}
}

fn lookup_module(mod string) ?string {
	mod_path := mod.replace('.', '/')
	vexe_path := os.base_dir(@VEXE)
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

fn get_resource(name string, minify bool) string {
	path := os.join_path(res_path, name)
	res := os.read_file(path) or { panic('could not read $path') }
	if minify {
		res.replace('\n', ' ')
	}
	return res
}

fn main() {
	args_after_doc := cmdline.options_after(os.args[1..], ['doc'])
	opts := cmdline.only_options(os.args[1..])
	args := cmdline.only_non_options(args_after_doc)
	if args.len == 0 || args[0] == 'help' {
		os.system('v help doc')
		exit(0)
	}
	mut config := DocConfig{
		src_path: args[0],
		opath: if args.len >= 2 { args[1] } else { '' },
		pub_only: '-all' !in opts,
		show_loc: '-loc' in opts,
		serve_http: '-s' in opts,
		is_multi: '-m' in opts,
		include_readme: '-r' in opts,
		manifest: vmod.Manifest{ repo_url: '' }
	}
	is_path := config.src_path.ends_with('.v') || config.src_path.split('/').len > 1 || config.src_path == '.'
	if !is_path {
		mod_path := lookup_module(config.src_path) or {
			eprintln(err)
			exit(1)
		}
		config.src_path = mod_path
	}
	config.generate_docs_from_file()
}
