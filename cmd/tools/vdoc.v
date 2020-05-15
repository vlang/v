module main

import v.ast
import v.parser
import v.pref
import v.table
import os
import strings
import time

enum OutputType {
	html
	markdown
	stdout
	json
}

struct Doc {
mut:
	input_path string
	prefs &pref.Preferences
	table &table.Table
	output_type OutputType	
	pub_only bool = true

	head_node DocNode
	content_nodes []DocNode
}

struct DocNode {
mut:
	name string
	content string
	comment string
}

fn slug(title string) string {
	return title.replace(' ', '-')
}

fn escape(str string) string {
	return str.replace_each(['"', '\\"', '\r\n', '\\n', '\n', '\\n'])
}

fn write_comment_bw(stmts []ast.Stmt, start_idx int) string {
	mut comment := ''

	for i := start_idx; i >= 0; i-- {
		stmt := stmts[i]

		if stmt is ast.Comment {
			cmt := stmt as ast.Comment
			cmt_content := cmt.text.trim_left('|')
			comment = cmt_content + if cmt_content.starts_with('```') { '\n' } else { ' ' } + comment
		} else {
			panic('Not a comment')
		}

		if i-1 >= 0 && !(stmts[i-1] is ast.Comment) {
			break
		}
	}

	return comment
}

fn (d Doc) get_signature(stmt ast.Stmt) string {
	match stmt {
		ast.Module {
			return 'module $it.name'
		}
		ast.StructDecl {
			name := it.name[d.head_node.name.len+1..]
			mut sig := 'struct $name {'

			if it.is_pub {
				sig = 'pub ' + sig
			}

			for field in it.fields {
				field_typ := d.table.get_type_name(field.typ)
				mut fi_sig := '$field.name $field_typ'

				// TODO: Add default expressions
				// if field.has_default_expr {
				// 	fi_sig := 
				// }

				sig = sig + '\n    $fi_sig'
			}

			sig = sig + if it.fields.len == 0 { '}' } else { '\n}' }
			return sig
		}
		ast.EnumDecl {
			name := it.name[d.head_node.name.len+1..]
			mut sig := 'enum $name {'
			if it.is_pub {
				sig = 'pub ' + sig
			}

			for field in it.fields {
				mut fi_sig := '$field.name'

				if field.has_expr {
					fi_sig = fi_sig + ' = ' + field.expr.str()
				}
				
				sig = sig + '\n    $fi_sig'
			}
			sig = sig + if it.fields.len == 0 { '}' } else { '\n}' }
			return sig
		}
		ast.FnDecl {
			return it.str(d.table)
		}
		else {
			return stmt.str()
		}
	}
}

fn (d Doc) get_name(stmt ast.Stmt) string {
	match stmt {
		ast.FnDecl { return it.name }
		ast.StructDecl { return it.name }
		ast.EnumDecl { return it.name }
		else { return '' }
	}
}

fn new_doc(input_path string) Doc {
	return Doc{
		input_path: os.real_path(input_path),
		prefs: &pref.Preferences{},
		table: table.new_table(),
		head_node: DocNode{},
		content_nodes: []DocNode{}
	}
}

fn (mut d Doc) generate_from_file(opath string) {
	// identify output type
	if opath.len == 0 {
		d.output_type = .stdout
	} else {
		ext := os.file_ext(opath)[1..]
		if ext in ['md', 'markdown'] || opath in [':md:', ':markdown:'] {
			d.output_type = .markdown
		} else if ext in ['html', 'htm'] || opath == ':html:' {
			d.output_type = .html
		} else if ext == 'json' || opath == ':json:' {
			d.output_type = .json
		} else {
			d.output_type = .markdown
		}
	}
	
	d.generate(opath)
}

fn (mut d Doc) generate(opath string) {
	// get all files
	base_path := if os.is_dir(d.input_path) { d.input_path } else { os.real_path(os.base_dir(d.input_path)) }
	project_files := os.ls(base_path) or { panic(err) }
	v_files := d.prefs.should_compile_filtered_files(base_path, project_files)

	if v_files.len == 0 {
		panic('No valid V files were found.')
	}

	// parse files
	mut asts := []ast.File{}

	for file in v_files {
		file_ast := parser.parse_file(
			file,
			d.table,
			.parse_comments,
			d.prefs,
			&ast.Scope{parent: 0}
		)

		asts << file_ast
	}

	mut module_name := ''

	for i, file_ast in asts {
		if i == 0 {
			module_name = file_ast.mod.name
			d.head_node = DocNode{
				name: module_name,
				content: 'module $module_name',
				comment: ''
			}
		} else if file_ast.mod.name != module_name {
			continue
		}
		stmts := file_ast.stmts

		for si, stmt in stmts {
			if stmt is ast.Comment { continue }
			if !(stmt is ast.Module) {
				name := d.get_name(stmt)
				signature := d.get_signature(stmt)

				if !signature.starts_with('pub') && d.pub_only { 
					continue
				}

				d.content_nodes << DocNode{
					name: name,
					content: signature,
					comment: ''
				}
			}

			if si-1 >= 0 && stmts[si-1] is ast.Comment {
				if stmt is ast.Module {
					d.head_node.comment = write_comment_bw(stmts, si-1)
				} else {
					last_comment := d.content_nodes[d.content_nodes.len-1].comment
					d.content_nodes[d.content_nodes.len-1].comment = last_comment + '\n' + write_comment_bw(stmts, si-1)
				}
			}	
		}
	}

	output := match d.output_type {
		.stdout { d.markdown(false) }
		.html { d.html() }
		.markdown { d.markdown(true) }
		.json { d.json() }
	}

	if d.output_type == .stdout || (opath.starts_with(':') && opath.ends_with(':')) {
		println(output)
	} else {
		// os.write_file(os.join_path(base_path, os.file_name(base_path) + '.v'), output)
		os.write_file(opath, output)
	}
}

fn (d Doc) json() string {
	mut jw := strings.new_builder(200)
	jw.writeln('{\n\t"module_name": "$d.head_node.name",\n\t"description": "${escape(d.head_node.comment)}",\n\t"contents": [')
	for i, cn in d.content_nodes {
		name := cn.name[d.head_node.name.len+1..]
		jw.writeln('\t\t{')
		jw.writeln('\t\t\t"name": "$name",')
		jw.writeln('\t\t\t"signature": "${escape(cn.content)}",')
		jw.writeln('\t\t\t"description": "${escape(cn.comment)}"')
		jw.write('\t\t}')
		if i < d.content_nodes.len-1 { jw.writeln(',') }
	}
	jw.writeln('\n\t],')
	jw.write('\t"generator": "vdoc",\n\t"time_generated": "${time.now().str()}"\n}')
	return jw.str()
}

fn (d Doc) html() string {
	eprintln('vdoc: HTML output is disabled for now.')
	exit(1)
	return ''
}

fn (d Doc) markdown(with_toc bool) string {
	mut hw := strings.new_builder(200)
	mut cw := strings.new_builder(200)

	hw.writeln('# ${d.head_node.content}\n${d.head_node.comment}\n')
	if with_toc {
		hw.writeln('## Contents')
	}
	
	for cn in d.content_nodes {
		name := cn.name[d.head_node.name.len+1..]

		if with_toc {
			hw.writeln('- [#$name](${slug(name)})')
		}
		cw.writeln('## $name')
		cw.writeln('```v\n${cn.content}\n```${cn.comment}\n')
		cw.writeln('[\[Return to contents\]](#Contents)\n')
	}

	cw.writeln('#### Generated by vdoc. Last generated: ${time.now().str()}')
	return hw.str() + '\n' + cw.str()
}

fn (d Doc) lookup_module(mod string) ?string {
	mod_path := mod.replace('.', '/')
	vexe_path := os.base_dir(os.base_dir(os.base_dir(os.executable())))

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

fn main() {
	args := os.args[2..]
	
	if args.len == 0 || args[0] == 'help' {
		os.system('v help doc')
		exit(0)
	}

	mut src_path := args[0]
	mut opath := if args.len >= 2 { args[1] } else { '' }
	mut pub_only := true

	if args[0] == '-pub_only' {
		if args[1] in ['true', 'false'] {
			pub_only = args[1] == 'true'
			src_path = args[2]
			opath = args[3]
		} else {
			src_path = args[1]
			opath = args[2]
		}
	}

	mut doc := new_doc(src_path)
	doc.pub_only = pub_only
	is_path := src_path.ends_with('.v') || src_path.split('/').len > 1 || src_path == '.'

	if is_path {
		doc.generate_from_file(opath)
	} else {
		mod_path := doc.lookup_module(src_path) or {
			eprintln(err)
			exit(1)
		}

		doc.input_path = mod_path
		doc.generate(opath)
	}
}