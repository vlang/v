module main

import markdown
import os
import time
import strings
import sync
import runtime
import document as doc
import v.vmod
import v.util
import json
import term

struct Readme {
	frontmatter map[string]string
	content     string
}

enum OutputType {
	unset
	html
	markdown
	json
	ansi // text with ANSI color escapes
	plaintext
}

@[heap]
struct VDoc {
	cfg Config @[required]
mut:
	docs                []doc.Doc
	assets              map[string]string
	manifest            vmod.Manifest
	search_index        []string
	search_data         []SearchResult
	search_module_index []string // search results are split into a module part and the rest
	search_module_data  []SearchModuleResult
	example_failures    int // how many times an example failed to compile or run with non 0 exit code; when positive, finish with exit code 1
	example_oks         int // how many ok examples were found when `-run-examples` was passed, that compiled and finished with 0 exit code.
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
	jw.write_string('{"module_name":"${d.head.name}",')
	if d.head.comments.len > 0 && cfg.include_comments {
		comments := if cfg.include_examples {
			d.head.merge_comments()
		} else {
			d.head.merge_comments_without_examples()
		}
		jw.write_string('"description":"${escape(comments)}",')
	}
	jw.write_string('"contents":')
	jw.write_string(json.encode(d.contents.keys().map(d.contents[it])))
	jw.write_string(',"generator":"vdoc","time_generated":"${d.time_generated.str()}"}')
	return jw.str()
}

fn (mut vd VDoc) gen_plaintext(d doc.Doc) string {
	cfg := vd.cfg
	mut pw := strings.new_builder(200)
	if cfg.is_color {
		content_arr := d.head.content.split(' ')
		pw.writeln('${term.bright_blue(content_arr[0])} ${term.green(content_arr[1])}')
	} else {
		pw.writeln('${d.head.content}')
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
	pw.writeln('')
	vd.write_plaintext_content(d.contents.arr(), mut pw)
	return pw.str()
}

fn indent(s string) string {
	return '    ' + s.replace('\n', '\n    ')
}

fn dn_to_location(cn doc.DocNode) string {
	location := '${util.path_styled_for_error_messages(cn.file_path)}:${cn.pos.line_nr + 1:-4}'
	if location.len > 24 {
		return '${location:-38s} '
	}
	return '${location:-24s} '
}

fn write_location(cn doc.DocNode, mut pw strings.Builder) {
	pw.write_string(dn_to_location(cn))
}

fn (mut vd VDoc) write_plaintext_content(contents []doc.DocNode, mut pw strings.Builder) {
	cfg := vd.cfg
	for cn in contents {
		if cn.content.len > 0 {
			if cfg.show_loc {
				write_location(cn, mut pw)
			}
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
			vd.run_examples(cn, mut pw)
		}
		vd.write_plaintext_content(cn.children, mut pw)
	}
}

fn (mut vd VDoc) render_doc(d doc.Doc, out Output) (string, string) {
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
	if mod == 'README' {
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

fn (mut vd VDoc) work_processor(mut work sync.Channel, mut wg sync.WaitGroup) {
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

fn (mut vd VDoc) render_parallel(out Output) {
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

fn (mut vd VDoc) render(out Output) map[string]string {
	mut docs := map[string]string{}
	for doc in vd.docs {
		name, output := vd.render_doc(doc, out)
		docs[name] = output.trim_space()
	}
	vd.vprintln('Rendered: ' + docs.keys().str())
	return docs
}

fn (vd &VDoc) get_readme(path string) Readme {
	mut fname := ''
	for name in ['readme.md', 'README.md'] {
		if os.exists(os.join_path(path, name)) {
			fname = name
			break
		}
	}
	if fname == '' {
		if path.all_after_last(os.path_separator) == 'src' {
			next_path := path.all_before_last(os.path_separator)
			if next_path != '' && path != next_path && os.is_dir(next_path) {
				return vd.get_readme(next_path)
			}
		}
		return Readme{}
	}
	readme_path := os.join_path(path, fname)
	vd.vprintln('Reading README file from ${readme_path}')
	mut readme_contents := os.read_file(readme_path) or { '' }
	mut readme_frontmatter := map[string]string{}
	if readme_contents.starts_with('---\n') {
		if frontmatter_lines_end_idx := readme_contents.index('\n---\n') {
			front_matter_lines := readme_contents#[4..frontmatter_lines_end_idx].trim_space().split_into_lines()
			for line in front_matter_lines {
				x := line.split(': ')
				if x.len == 2 {
					readme_frontmatter[x[0]] = x[1]
				}
			}
			readme_contents = readme_contents#[5 + frontmatter_lines_end_idx..]
		}
	}
	return Readme{
		frontmatter: readme_frontmatter
		content:     readme_contents
	}
}

fn (vd &VDoc) emit_generate_err(err IError) {
	cfg := vd.cfg
	mut err_msg := err.msg()
	if err.code() == 1 {
		mod_list := get_modules(cfg.input_path)
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
		typ:  cfg.output_type
	}
	if out.path == '' {
		if cfg.output_type == .unset {
			out.typ = .ansi
		} else {
			vd.vprintln('No output path has detected. Using input path instead.')
			out.path = cfg.input_path
		}
	} else if out.typ == .unset {
		vd.vprintln('Output path detected. Identifying output type..')
		ext := os.file_ext(out.path)
		out.typ = set_output_type_from_str(ext.all_after('.'))
	}
	if cfg.include_readme && out.typ !in [.html, .ansi, .plaintext] {
		eprintln('vdoc: Including README.md for doc generation is supported on HTML output, or when running directly in the terminal.')
		exit(1)
	}
	dir_path := if cfg.is_vlib {
		os.join_path(vroot, 'vlib')
	} else if os.is_dir(cfg.input_path) {
		cfg.input_path
	} else {
		os.dir(cfg.input_path)
	}
	manifest_path := if cfg.is_vlib {
		os.join_path(vroot, 'v.mod')
	} else {
		os.join_path(dir_path, 'v.mod')
	}
	if os.exists(manifest_path) {
		vd.vprintln('Reading v.mod info from ${manifest_path}')
		if manifest := vmod.from_file(manifest_path) {
			vd.manifest = manifest
		}
	} else if cfg.is_vlib {
		assert false, 'vdoc: manifest does not exist for vlib'
	}
	if cfg.include_readme || cfg.is_vlib {
		mut readme_name := 'README'
		readme := vd.get_readme(dir_path)
		if page := readme.frontmatter['page'] {
			readme_name = page
		}
		comment := doc.DocComment{
			is_readme:   true
			frontmatter: readme.frontmatter
			text:        readme.content
		}
		if out.typ == .ansi {
			println(markdown.to_plain(readme.content))
		} else if out.typ == .html && cfg.is_multi {
			vd.docs << doc.Doc{
				head:           doc.DocNode{
					is_readme:   true
					name:        readme_name
					frontmatter: readme.frontmatter
					comments:    [comment]
				}
				time_generated: time.now()
			}
		}
	}
	dirs := if cfg.is_multi { get_modules(cfg.input_path) } else { [cfg.input_path] }
	for dirpath in dirs {
		vd.vprintln('Generating ${out.typ} docs for "${dirpath}"')
		mut dcs := doc.generate(dirpath, cfg.pub_only, true, cfg.platform, cfg.symbol_name) or {
			// TODO: use a variable like `src_path := os.join_path(dirpath, 'src')` after `https://github.com/vlang/v/issues/21504`
			if os.exists(os.join_path(dirpath, 'src')) {
				doc.generate(os.join_path(dirpath, 'src'), cfg.pub_only, true, cfg.platform,
					cfg.symbol_name) or {
					vd.emit_generate_err(err)
					exit(1)
				}
			} else {
				vd.emit_generate_err(err)
				exit(1)
			}
		}
		if dcs.contents.len == 0 {
			continue
		}
		if cfg.is_multi || (!cfg.is_multi && cfg.include_readme) {
			readme := vd.get_readme(dirpath)
			comment := doc.DocComment{
				is_readme:   true
				frontmatter: readme.frontmatter
				text:        readme.content
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
	if dirs.len == 0 && cfg.is_multi {
		eprintln('vdoc: -m requires at least 1 module folder')
		exit(1)
	}
	vd.vprintln('Rendering docs...')
	if out.path == '' || out.path == 'stdout' || out.path == '-' {
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
			out.path = if cfg.input_path == out.path {
				os.join_path(out.path, '_docs')
			} else {
				out.path
			}
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
