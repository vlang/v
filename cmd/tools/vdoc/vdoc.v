module main

import markdown
import os
import os.cmdline
import time
import strings
import sync
import runtime
import v.doc
import v.pref
import v.vmod
import json

const (
	allowed_formats = ['md', 'markdown', 'json', 'text', 'stdout', 'html', 'htm']
	vexe            = pref.vexe_path()
	vroot           = os.dir(vexe)
	tabs            = ['\t\t', '\t\t\t\t\t\t', '\t\t\t\t\t\t\t']
)

enum OutputType {
	unset
	html
	markdown
	json
	plaintext
	stdout
}

struct VDoc {
mut:
	cfg                 Config [required]
	docs                []doc.Doc
	assets              map[string]string
	search_index        []string
	search_data         []SearchResult
	search_module_index []string // search results are split into a module part and the rest
	search_module_data  []SearchModuleResult
}

struct Config {
mut:
	pub_only            bool = true
	is_local            bool
	local_filename      string
	local_pos           int
	show_loc            bool // for plaintext
	serve_http          bool // for html
	is_multi            bool
	is_vlib             bool
	is_verbose          bool
	include_readme      bool
	include_examples    bool = true
	open_docs           bool
	server_port         int = 8046
	inline_assets       bool
	no_timestamp        bool
	output_path         string
	input_path          string
	symbol_name         string
	output_type         OutputType = .unset
	manifest            vmod.Manifest
}

struct ParallelDoc {
	d doc.Doc
	i int
}

[inline]
fn slug(title string) string {
	return title.replace(' ', '-')
}

[inline]
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

fn escape(str string) string {
	return str.replace_each(['"', '\\"', '\r\n', '\\n', '\n', '\\n', '\t', '\\t'])
}

fn (vd VDoc) gen_json(idx int) string {
	dcs := vd.docs[idx]
	mut jw := strings.new_builder(200)
	comments := if vd.cfg.include_examples {
		dcs.head.merge_comments()
	} else {
		dcs.head.merge_comments_without_examples()
	}
	jw.write('{"module_name":"$dcs.head.name","description":"${escape(comments)}","contents":')
	jw.write(json.encode(dcs.contents.keys().map(dcs.contents[it])))
	jw.write(',"generator":"vdoc","time_generated":"$dcs.time_generated.str()"}')
	return jw.str()
}

fn get_sym_name(dn doc.DocNode) string {
	sym_name := if dn.parent_name.len > 0 && dn.parent_name != 'void' {
		'($dn.parent_name) $dn.name'
	} else {
		dn.name
	}
	return sym_name
}

fn get_node_id(dn doc.DocNode) string {
	tag := if dn.parent_name.len > 0 && dn.parent_name != 'void' {
		'${dn.parent_name}.$dn.name'
	} else {
		dn.name
	}
	return slug(tag)
}

fn (vd VDoc) write_plaintext_content(contents []doc.DocNode, mut pw strings.Builder) {
	for cn in contents {
		if cn.content.len > 0 {
			pw.writeln(cn.content)
			if cn.comments.len > 0 && !vd.cfg.pub_only {
				comments := if vd.cfg.include_examples {
					cn.merge_comments()
				} else {
					cn.merge_comments_without_examples()
				}
				pw.writeln(comments.trim_space().split_into_lines().map('    ' + it).join('\n'))
			}
			if vd.cfg.show_loc {
				pw.writeln('Location: $cn.file_path:$cn.pos.line\n')
			}
		}
		vd.write_plaintext_content(cn.children, mut pw)
	}
}

fn (vd VDoc) gen_footer_text(idx int) string {
	current_doc := vd.docs[idx]
	footer_text := 'Powered by vdoc.'
	if vd.cfg.no_timestamp {
		return footer_text
	}
	generated_time := current_doc.time_generated
	time_str := '$generated_time.day $generated_time.smonth() $generated_time.year $generated_time.hhmmss()'
	return '$footer_text Generated on: $time_str'
}

fn (vd VDoc) render_doc(doc doc.Doc, i int) (string, string) {
	name := vd.get_file_name(doc.head.name)
	output := match vd.cfg.output_type {
		.html { vd.gen_html(i) }
		.markdown { vd.gen_markdown(i, true) }
		.json { vd.gen_json(i) }
		else { vd.gen_plaintext(i) }
	}
	return name, output
}

// get_file_name returns the final file name from a module name
fn (vd VDoc) get_file_name(mod string) string {
	mut name := mod
	// since builtin is generated first, ignore it
	if (vd.cfg.is_vlib && mod == 'builtin' && !vd.cfg.include_readme) || mod == 'README' {
		name = 'index'
	} else if !vd.cfg.is_multi && !os.is_dir(vd.cfg.output_path) {
		name = os.file_name(vd.cfg.output_path)
	}
	name = name + match vd.cfg.output_type {
		.html { '.html' }
		.markdown { '.md' }
		.json { '.json' }
		else { '.txt' }
	}
	return name
}

fn (vd VDoc) work_processor(mut work sync.Channel, mut wg sync.WaitGroup) {
	for {
		mut pdoc := ParallelDoc{}
		if !work.pop(&pdoc) {
			break
		}
		file_name, content := vd.render_doc(pdoc.d, pdoc.i)
		output_path := os.join_path(vd.cfg.output_path, file_name)
		println('Generating $output_path')
		os.write_file(output_path, content)
	}
	wg.done()
}

fn (vd VDoc) render_parallel() {
	vjobs := runtime.nr_jobs()
	mut work := sync.new_channel<ParallelDoc>(vd.docs.len)
	mut wg := sync.new_waitgroup()
	for i in 0 .. vd.docs.len {
		p_doc := ParallelDoc{vd.docs[i], i}
		work.push(&p_doc)
	}
	work.close()
	wg.add(vjobs)
	for _ in 0 .. vjobs {
		go vd.work_processor(mut work, mut wg)
	}
	wg.wait()
}

fn (vd VDoc) render() map[string]string {
	mut docs := map[string]string{}
	for i, doc in vd.docs {
		name, output := vd.render_doc(doc, i)
		docs[name] = output.trim_space()
	}
	vd.vprintln('Rendered: ' + docs.keys().str())
	return docs
}

fn trim_doc_node_description(description string) string {
	mut dn_description := description.replace_each(['\r\n', '\n', '"', '\\"'])
	// 80 is enough to fill one line
	if dn_description.len > 80 {
		dn_description = dn_description[..80]
	}
	if '\n' in dn_description {
		dn_description = dn_description.split('\n')[0]
	}
	// if \ is last character, it ends with \" which leads to a JS error
	if dn_description.ends_with('\\') {
		dn_description = dn_description.trim_right('\\')
	}
	return dn_description
}

fn (mut vd VDoc) render_static() {
	if vd.cfg.output_type != .html {
		return
	}
	vd.assets = {
		'doc_css':       vd.get_resource(css_js_assets[0], true)
		'normalize_css': vd.get_resource(css_js_assets[1], true)
		'doc_js':        vd.get_resource(css_js_assets[2], !vd.cfg.serve_http)
		'dark_mode_js':  vd.get_resource(css_js_assets[3], !vd.cfg.serve_http)
		'light_icon':    vd.get_resource('light.svg', true)
		'dark_icon':     vd.get_resource('dark.svg', true)
		'menu_icon':     vd.get_resource('menu.svg', true)
		'arrow_icon':    vd.get_resource('arrow.svg', true)
	}
}

fn (vd VDoc) get_readme(path string) string {
	mut fname := ''
	for name in ['readme', 'README'] {
		if os.exists(os.join_path(path, '${name}.md')) {
			fname = name
			break
		}
	}
	if fname == '' {
		return ''
	}
	readme_path := os.join_path(path, '${fname}.md')
	vd.vprintln('Reading README file from $readme_path')
	readme_contents := os.read_file(readme_path) or { '' }
	return readme_contents
}

fn (vd VDoc) emit_generate_err(err string, errcode int) {
	mut err_msg := err
	if errcode == 1 {
		mod_list := get_modules_list(vd.cfg.input_path, []string{})
		println('Available modules:\n==================')
		for mod in mod_list {
			println(mod.all_after('vlib/').all_after('modules/').replace('/', '.'))
		}
		err_msg += ' Use the `-m` flag when generating docs from a directory that has multiple modules.'
	}
	eprintln(err_msg)
}

fn (mut vd VDoc) generate_docs_from_file() {
	if vd.cfg.output_path.len == 0 {
		if vd.cfg.output_type == .unset {
			vd.cfg.output_type = .stdout
		} else {
			vd.vprintln('No output path has detected. Using input path instead.')
			vd.cfg.output_path = vd.cfg.input_path
		}
	} else if vd.cfg.output_type == .unset {
		vd.vprintln('Output path detected. Identifying output type..')
		ext := os.file_ext(vd.cfg.output_path)
		vd.cfg.output_type = set_output_type_from_str(ext.all_after('.'))
	}
	if vd.cfg.include_readme && vd.cfg.output_type !in [.html, .stdout] {
		eprintln('vdoc: Including README.md for doc generation is supported on HTML output, or when running directly in the terminal.')
		exit(1)
	}
	dir_path := if vd.cfg.is_vlib {
		vroot
	} else if os.is_dir(vd.cfg.input_path) {
		vd.cfg.input_path
	} else {
		os.dir(vd.cfg.input_path)
	}
	manifest_path := os.join_path(dir_path, 'v.mod')
	if os.exists(manifest_path) {
		vd.vprintln('Reading v.mod info from $manifest_path')
		if manifest := vmod.from_file(manifest_path) {
			vd.cfg.manifest = manifest
		}
	}
	if vd.cfg.include_readme {
		readme_contents := vd.get_readme(dir_path)
		comment := doc.DocComment{
			text: readme_contents
		}
		if vd.cfg.output_type == .stdout {
			println(markdown.to_plain(readme_contents))
		} else if vd.cfg.output_type == .html && vd.cfg.is_multi {
			vd.docs << doc.Doc{
				head: doc.DocNode{
					name: 'README'
					comments: [comment]
				}
				time_generated: time.now()
			}
		}
	}
	dirs := if vd.cfg.is_multi {
		get_modules_list(vd.cfg.input_path, []string{})
	} else {
		[vd.cfg.input_path]
	}
	is_local_and_single := vd.cfg.is_local && !vd.cfg.is_multi
	for dirpath in dirs {
		mut dcs := doc.Doc{}
		vd.vprintln('Generating docs for $dirpath')
		if is_local_and_single {
			dcs = doc.generate_with_pos(dirpath, vd.cfg.local_filename, vd.cfg.local_pos) or {
				vd.emit_generate_err(err, errcode)
				exit(1)
			}
		} else {
			dcs = doc.generate(dirpath, vd.cfg.pub_only, true) or {
				vd.emit_generate_err(err, errcode)
				exit(1)
			}
		}
		if dcs.contents.len == 0 {
			continue
		}
		if !is_local_and_single {
			if vd.cfg.is_multi || (!vd.cfg.is_multi && vd.cfg.include_readme) {
				readme_contents := vd.get_readme(dirpath)
				comment := doc.DocComment{
					text: readme_contents
				}
				dcs.head.comments = [comment]
			}
			if vd.cfg.pub_only {
				for name, dc in dcs.contents {
					dcs.contents[name].content = dc.content.all_after('pub ')
					for i, cc in dc.children {
						dcs.contents[name].children[i].content = cc.content.all_after('pub ')
					}
				}
			}
			if !vd.cfg.is_multi && vd.cfg.symbol_name.len > 0 {
				if vd.cfg.symbol_name in dcs.contents {
					for _, c in dcs.contents[vd.cfg.symbol_name].children {
						dcs.contents[c.name] = c
					}
				}
			}
		}
		vd.docs << dcs
	}
	if vd.cfg.is_vlib {
		mut docs := vd.docs.filter(it.head.name == 'builtin')
		docs << vd.docs.filter(it.head.name != 'builtin')
		vd.docs = docs
	}
	if vd.cfg.serve_http {
		vd.serve_html()
		return
	}
	vd.vprintln('Rendering docs...')
	if vd.cfg.output_path.len == 0 || vd.cfg.output_path == 'stdout' {
		vd.render_static()
		outputs := vd.render()
		if outputs.len == 0 {
			println('No documentation for $dirs')
		} else {
			first := outputs.keys()[0]
			println(outputs[first])
		}
	} else {
		if !os.is_dir(vd.cfg.output_path) {
			vd.cfg.output_path = os.real_path('.')
		}
		if !os.exists(vd.cfg.output_path) {
			os.mkdir(vd.cfg.output_path) or { panic(err) }
		}
		if vd.cfg.is_multi {
			vd.cfg.output_path = os.join_path(vd.cfg.output_path, '_docs')
			if !os.exists(vd.cfg.output_path) {
				os.mkdir(vd.cfg.output_path) or { panic(err) }
			} else {
				for fname in css_js_assets {
					os.rm(os.join_path(vd.cfg.output_path, fname))
				}
			}
		}
		vd.render_static()
		vd.render_parallel()
		println('Creating search index...')
		vd.collect_search_index()
		vd.render_search_index()
		// move favicons to target directory
		println('Copying favicons...')
		favicons := os.ls(favicons_path) or { panic(err) }
		for favicon in favicons {
			favicon_path := os.join_path(favicons_path, favicon)
			destination_path := os.join_path(vd.cfg.output_path, favicon)
			os.cp(favicon_path, destination_path)
		}
	}
}

fn set_output_type_from_str(format string) OutputType {
	output_type := match format {
		'htm', 'html' { OutputType.html }
		'md', 'markdown' { OutputType.markdown }
		'json' { OutputType.json }
		'stdout' { OutputType.stdout }
		else { OutputType.plaintext }
	}
	return output_type
}

fn (vd VDoc) vprintln(str string) {
	if vd.cfg.is_verbose {
		println('vdoc: $str')
	}
}

fn get_ignore_paths(path string) ?[]string {
	ignore_file_path := os.join_path(path, '.vdocignore')
	ignore_content := os.read_file(ignore_file_path) or {
		return error_with_code('ignore file not found.', 1)
	}
	mut res := []string{}
	if ignore_content.trim_space().len > 0 {
		rules := ignore_content.split_into_lines().map(it.trim_space())
		mut final := []string{}
		for rule in rules {
			if rule.contains('*.') || rule.contains('**') {
				println('vdoc: Wildcards in ignore rules are not allowed for now.')
				continue
			}
			final << rule
		}
		res = final.map(os.join_path(path, it.trim_right('/')))
	} else {
		mut dirs := os.ls(path) or { return []string{} }
		res = dirs.map(os.join_path(path, it)).filter(os.is_dir(it))
	}
	return res.map(it.replace('/', os.path_separator))
}

fn is_included(path string, ignore_paths []string) bool {
	if path.len == 0 {
		return true
	}
	for ignore_path in ignore_paths {
		if ignore_path !in path {
			continue
		}
		return false
	}
	return true
}

fn is_module_readme(dn doc.DocNode) bool {
	if dn.comments.len > 0 && dn.content == 'module $dn.name' {
		return true
	}
	return false
}

fn get_modules_list(path string, ignore_paths2 []string) []string {
	files := os.ls(path) or { return []string{} }
	mut ignore_paths := get_ignore_paths(path) or { []string{} }
	ignore_paths << ignore_paths2
	mut dirs := []string{}
	for file in files {
		fpath := os.join_path(path, file)
		if os.is_dir(fpath) && is_included(fpath, ignore_paths) && !os.is_link(path) {
			dirs << get_modules_list(fpath, ignore_paths.filter(it.starts_with(fpath)))
		} else if fpath.ends_with('.v') && !fpath.ends_with('_test.v') {
			if path in dirs {
				continue
			}
			dirs << path
		}
	}
	dirs.sort()
	return dirs
}

fn (vd VDoc) get_resource(name string, minify bool) string {
	path := os.join_path(res_path, name)
	mut res := os.read_file(path) or { panic('vdoc: could not read $path') }
	if minify {
		if name.ends_with('.js') {
			res = js_compress(res)
		} else {
			res = res.split_into_lines().map(it.trim_space()).join('')
		}
	}
	// TODO: Make SVG inline for now
	if vd.cfg.inline_assets || path.ends_with('.svg') {
		return res
	} else {
		output_path := os.join_path(vd.cfg.output_path, name)
		if !os.exists(output_path) {
			println('Generating $output_path')
			os.write_file(output_path, res)
		}
		return name
	}
}

fn parse_arguments(args []string) Config {
	mut cfg := Config{}
	for i := 0; i < args.len; i++ {
		arg := args[i]
		current_args := args[i..]
		match arg {
			'-all' {
				cfg.pub_only = false
			}
			'-filename' {
				cfg.is_local = true
				cfg.local_filename = cmdline.option(current_args, '-filename', '')
				i++
			}
			'-f' {
				format := cmdline.option(current_args, '-f', '')
				if format !in allowed_formats {
					allowed_str := allowed_formats.join(', ')
					eprintln('vdoc: "$format" is not a valid format. Only $allowed_str are allowed.')
					exit(1)
				}
				cfg.output_type = set_output_type_from_str(format)
				// TODO vd.vprintln('Setting output type to "$vd.cfg.output_type"')
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
				cfg.output_path = if opath == 'stdout' { opath } else { os.real_path(opath) }
				i++
			}
			'-open' {
				cfg.open_docs = true
			}
			'-pos' {
				if !cfg.is_local {
					eprintln('vdoc: `-pos` is only allowed with `-filename` flag.')
					exit(1)
				}
				cfg.local_pos = cmdline.option(current_args, '-pos', '').int()
				i++
			}
			'-p' {
				s_port := cmdline.option(current_args, '-p', '')
				s_port_int := s_port.int()
				if s_port.len == 0 {
					eprintln('vdoc: No port number specified on "-p".')
					exit(1)
				}
				if s_port != s_port_int.str() {
					eprintln('vdoc: Invalid port number.')
					exit(1)
				}
				cfg.server_port = s_port_int
				i++
			}
			'-s' {
				cfg.inline_assets = true
				cfg.serve_http = true
				if cfg.output_type == .unset {
					cfg.output_type = .html
				}
			}
			'-no-timestamp' {
				cfg.no_timestamp = true
			}
			'-no-examples' {
				cfg.include_examples = false
			}
			'-readme' {
				cfg.include_readme = true
			}
			'-v' {
				cfg.is_verbose = true
			}
			else {
				if cfg.input_path.len < 1 {
					cfg.input_path = arg
				} else {
					cfg.symbol_name = arg
				}
				if i == args.len - 1 {
					break
				}
			}
		}
	}
	return cfg
}

fn main() {
	if os.args.len < 2 || '-h' in os.args || '--help' in os.args || os.args[1..] == ['doc', 'help'] {
		os.system('$vexe help doc')
		exit(0)
	}
	args := os.args[2..].clone()

	mut cfg := parse_arguments(args)
	// Correct configuration
	if cfg.output_path == 'stdout' && cfg.output_type == .html {
		cfg.inline_assets = true
	}
	$if windows {
		cfg.input_path = cfg.input_path.replace('/', os.path_separator)
	} $else {
		cfg.input_path = cfg.input_path.replace('\\', os.path_separator)
	}
	is_path := cfg.input_path.ends_with('.v') || cfg.input_path.split(os.path_separator).len >
		1 || cfg.input_path == '.'
	if cfg.input_path.trim_right('/') == 'vlib' {
		cfg.is_vlib = true
		cfg.is_multi = true
		cfg.input_path = os.join_path(vroot, 'vlib')
	} else if !is_path {
		// TODO vd.vprintln('Input "$cfg.input_path" is not a valid path. Looking for modules named "$cfg.input_path"...')
		mod_path := doc.lookup_module(cfg.input_path) or {
			eprintln('vdoc: $err')
			exit(1)
		}
		cfg.input_path = mod_path
	}

	if cfg.input_path.len == 0 {
		eprintln('vdoc: No input path found.')
		exit(1)
	}

	cfg.manifest = vmod.Manifest{
		repo_url: ''
	}

	mut vd := VDoc{
		cfg: cfg
	}

	vd.generate_docs_from_file()
}
