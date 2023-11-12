module main

import markdown
import os
import time
import strings
import sync
import runtime
import v.doc
import v.vmod
import json
import term

enum OutputType {
	unset
	html
	markdown
	json
	plaintext
	stdout
}

[heap]
struct VDoc {
	cfg Config [required]
mut:
	docs                []doc.Doc
	assets              map[string]string
	manifest            vmod.Manifest
	search_index        []string
	search_data         []SearchResult
	search_module_index []string // search results are split into a module part and the rest
	search_module_data  []SearchModuleResult
}

struct Config {
mut:
	pub_only         bool = true
	show_loc         bool // for plaintext
	is_color         bool
	is_multi         bool
	is_vlib          bool
	is_verbose       bool
	include_readme   bool
	include_examples bool = true
	include_comments bool // for plaintext
	inline_assets    bool
	theme_dir        string = default_theme
	no_timestamp     bool
	output_path      string
	output_type      OutputType = .unset
	input_path       string
	symbol_name      string
	platform         doc.Platform
}

//
struct Output {
mut:
	path string
	typ  OutputType = .unset
}

struct ParallelDoc {
	d   doc.Doc
	out Output
}

fn (vd &VDoc) gen_json(d doc.Doc) string {
	cfg := vd.cfg
	mut jw := strings.new_builder(200)
	comments := if cfg.include_examples {
		d.head.merge_comments()
	} else {
		d.head.merge_comments_without_examples()
	}
	jw.write_string('{"module_name":"${d.head.name}","description":"${escape(comments)}","contents":')
	jw.write_string(json.encode(d.contents.keys().map(d.contents[it])))
	jw.write_string(',"generator":"vdoc","time_generated":"${d.time_generated.str()}"}')
	return jw.str()
}

fn (vd &VDoc) gen_plaintext(d doc.Doc) string {
	cfg := vd.cfg
	mut pw := strings.new_builder(200)
	if cfg.is_color {
		content_arr := d.head.content.split(' ')
		pw.writeln('${term.bright_blue(content_arr[0])} ${term.green(content_arr[1])}\n')
	} else {
		pw.writeln('${d.head.content}\n')
	}
	if cfg.include_comments {
		comments := if cfg.include_examples {
			d.head.merge_comments()
		} else {
			d.head.merge_comments_without_examples()
		}
		if comments.trim_space().len > 0 {
			pw.writeln(indent(comments))
		}
	}
	vd.write_plaintext_content(d.contents.arr(), mut pw)
	return pw.str()
}

fn indent(s string) string {
	return '    ' + s.replace('\n', '\n    ')
}

fn (vd &VDoc) write_plaintext_content(contents []doc.DocNode, mut pw strings.Builder) {
	cfg := vd.cfg
	for cn in contents {
		if cn.content.len > 0 {
			if cfg.is_color {
				pw.writeln(color_highlight(cn.content, vd.docs[0].table))
			} else {
				pw.writeln(cn.content)
			}
			if cn.comments.len > 0 && cfg.include_comments {
				comments := cn.merge_comments_without_examples()
				pw.writeln(indent(comments.trim_space()))
				if cfg.include_examples {
					examples := cn.examples()
					for ex in examples {
						pw.write_string('    Example: ')
						mut fex := ex
						if ex.index_u8(`\n`) >= 0 {
							// multi-line example
							pw.write_u8(`\n`)
							fex = indent(ex)
						}
						if cfg.is_color {
							fex = color_highlight(fex, vd.docs[0].table)
						}
						pw.writeln(fex)
					}
				}
			}
			if cfg.show_loc {
				pw.writeln('Location: ${cn.file_path}:${cn.pos.line_nr + 1}\n')
			}
		}
		vd.write_plaintext_content(cn.children, mut pw)
	}
}

fn (vd &VDoc) render_doc(d doc.Doc, out Output) (string, string) {
	name := vd.get_file_name(d.head.name, out)
	output := match out.typ {
		.html { vd.gen_html(d) }
		.markdown { vd.gen_markdown(d, true) }
		.json { vd.gen_json(d) }
		else { vd.gen_plaintext(d) }
	}
	return name, output
}

// get_file_name returns the final file name from a module name
fn (vd &VDoc) get_file_name(mod string, out Output) string {
	cfg := vd.cfg
	mut name := mod
	// since builtin is generated first, ignore it
	if (cfg.is_vlib && mod == 'builtin' && !cfg.include_readme) || mod == 'README' {
		name = 'index'
	} else if !cfg.is_multi && !os.is_dir(out.path) {
		name = os.file_name(out.path)
	}
	if name == '' {
		name = 'index'
	}
	name = name + match out.typ {
		.html { '.html' }
		.markdown { '.md' }
		.json { '.json' }
		else { '.txt' }
	}
	return name
}

fn (vd &VDoc) work_processor(mut work sync.Channel, mut wg sync.WaitGroup) {
	for {
		mut pdoc := ParallelDoc{}
		if !work.pop(&pdoc) {
			break
		}
		vd.vprintln('> start processing ${pdoc.d.base_path} ...')
		flush_stdout()
		file_name, content := vd.render_doc(pdoc.d, pdoc.out)
		output_path := os.join_path(pdoc.out.path, file_name)
		println('Generating ${pdoc.out.typ} in "${output_path}"')
		flush_stdout()
		os.write_file(output_path, content) or { panic(err) }
	}
	wg.done()
}

fn (vd &VDoc) render_parallel(out Output) {
	vjobs := runtime.nr_jobs()
	mut work := sync.new_channel[ParallelDoc](u32(vd.docs.len))
	mut wg := sync.new_waitgroup()
	for i in 0 .. vd.docs.len {
		p_doc := ParallelDoc{vd.docs[i], out}
		work.push(&p_doc)
	}
	work.close()
	wg.add(vjobs)
	for _ in 0 .. vjobs {
		spawn vd.work_processor(mut work, mut wg)
	}
	wg.wait()
}

fn (vd &VDoc) render(out Output) map[string]string {
	mut docs := map[string]string{}
	for doc in vd.docs {
		name, output := vd.render_doc(doc, out)
		docs[name] = output.trim_space()
	}
	vd.vprintln('Rendered: ' + docs.keys().str())
	return docs
}

fn (vd &VDoc) get_readme(path string) string {
	mut fname := ''
	for name in ['readme', 'README'] {
		if os.exists(os.join_path(path, '${name}.md')) {
			fname = name
			break
		}
	}
	if fname == '' {
		if path.all_after_last(os.path_separator) == 'src' {
			return vd.get_readme(path.all_before_last(os.path_separator))
		}
		return ''
	}
	readme_path := os.join_path(path, '${fname}.md')
	vd.vprintln('Reading README file from ${readme_path}')
	readme_contents := os.read_file(readme_path) or { '' }
	return readme_contents
}

fn (vd &VDoc) emit_generate_err(err IError) {
	cfg := vd.cfg
	mut err_msg := err.msg()
	if err.code() == 1 {
		mod_list := get_modules_list(cfg.input_path, []string{})
		println('Available modules:\n==================')
		for mod in mod_list {
			println(mod.all_after('vlib/').all_after('modules/').replace('/', '.'))
		}
		err_msg += ' Use the `-m` flag when generating docs from a directory that has multiple modules.'
	}
	eprintln(err_msg)
}

fn (mut vd VDoc) generate_docs_from_file() {
	cfg := vd.cfg
	mut out := Output{
		path: cfg.output_path
		typ: cfg.output_type
	}
	if out.path.len == 0 {
		if cfg.output_type == .unset {
			out.typ = .stdout
		} else {
			vd.vprintln('No output path has detected. Using input path instead.')
			out.path = cfg.input_path
		}
	} else if out.typ == .unset {
		vd.vprintln('Output path detected. Identifying output type..')
		ext := os.file_ext(out.path)
		out.typ = set_output_type_from_str(ext.all_after('.'))
	}
	if cfg.include_readme && out.typ !in [.html, .stdout] {
		eprintln('vdoc: Including README.md for doc generation is supported on HTML output, or when running directly in the terminal.')
		exit(1)
	}
	dir_path := if cfg.is_vlib {
		vroot
	} else if os.is_dir(cfg.input_path) {
		cfg.input_path
	} else {
		os.dir(cfg.input_path)
	}
	manifest_path := os.join_path(dir_path, 'v.mod')
	if os.exists(manifest_path) {
		vd.vprintln('Reading v.mod info from ${manifest_path}')
		if manifest := vmod.from_file(manifest_path) {
			vd.manifest = manifest
		}
	}
	if cfg.include_readme {
		readme_contents := vd.get_readme(dir_path)
		comment := doc.DocComment{
			text: readme_contents
		}
		if out.typ == .stdout {
			println(markdown.to_plain(readme_contents))
		} else if out.typ == .html && cfg.is_multi {
			vd.docs << doc.Doc{
				head: doc.DocNode{
					name: 'README'
					comments: [comment]
				}
				time_generated: time.now()
			}
		}
	}
	dirs := if cfg.is_multi { get_modules_list(cfg.input_path, []string{}) } else { [
			cfg.input_path,
		] }
	for dirpath in dirs {
		vd.vprintln('Generating ${out.typ} docs for "${dirpath}"')
		mut dcs := doc.generate(dirpath, cfg.pub_only, true, cfg.platform, cfg.symbol_name) or {
			vd.emit_generate_err(err)
			exit(1)
		}
		if dcs.contents.len == 0 {
			continue
		}
		if cfg.is_multi || (!cfg.is_multi && cfg.include_readme) {
			readme_contents := vd.get_readme(dirpath)
			comment := doc.DocComment{
				text: readme_contents
			}
			dcs.head.comments = [comment]
		}
		if cfg.pub_only {
			for name, dc in dcs.contents {
				dcs.contents[name].content = dc.content.all_after('pub ')
				for i, cc in dc.children {
					dcs.contents[name].children[i].content = cc.content.all_after('pub ')
				}
			}
		}
		vd.docs << dcs
	}
	// Important. Let builtin be in the top of the module list
	// if we are generating docs for vlib.
	if cfg.is_vlib {
		mut docs := vd.docs.filter(it.head.name == 'builtin')
		docs << vd.docs.filter(it.head.name != 'builtin')
		vd.docs = docs
	}
	if dirs.len == 0 && cfg.is_multi {
		eprintln('vdoc: -m requires at least 1 module folder')
		exit(1)
	}
	vd.vprintln('Rendering docs...')
	if out.path.len == 0 || out.path == 'stdout' {
		if out.typ == .html {
			vd.render_static_html(out)
		}
		outputs := vd.render(out)
		if outputs.len == 0 {
			if dirs.len == 0 {
				eprintln('vdoc: No documentation found')
			} else {
				eprintln('vdoc: No documentation found for ${dirs[0]}')
			}
			exit(1)
		} else {
			first := outputs.keys()[0]
			println(outputs[first])
		}
	} else {
		if !os.exists(out.path) {
			os.mkdir_all(out.path) or { panic(err) }
		} else if !os.is_dir(out.path) {
			out.path = os.real_path('.')
		}
		if cfg.is_multi {
			out.path = os.join_path(out.path, '_docs')
			if !os.exists(out.path) {
				os.mkdir(out.path) or { panic(err) }
			} else {
				for fname in css_js_assets {
					existing_asset_path := os.join_path(out.path, fname)
					if os.exists(existing_asset_path) {
						os.rm(existing_asset_path) or { panic(err) }
					}
				}
			}
		}
		if out.typ == .html {
			vd.render_static_html(out)
		}
		vd.render_parallel(out)
		if out.typ == .html {
			println('Creating search index...')
			vd.collect_search_index(out)
			vd.render_search_index(out)
			// move favicons to target directory
			println('Copying favicons...')

			favicons_path := os.join_path(cfg.theme_dir, 'favicons')

			favicons := os.ls(favicons_path) or { panic(err) }
			for favicon in favicons {
				favicon_path := os.join_path(favicons_path, favicon)
				destination_path := os.join_path(out.path, favicon)
				os.cp(favicon_path, destination_path) or { panic(err) }
			}
		}
	}
}

fn (vd &VDoc) vprintln(str string) {
	if vd.cfg.is_verbose {
		println('vdoc: ${str}')
	}
}
