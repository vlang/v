module doc

import v.ast
import v.parser
import v.pref
import v.table
import v.token
import v.util
import os
import time

pub struct Doc {
pub mut:
	input_path string = ''
	prefs &pref.Preferences
	table &table.Table
	pub_only bool = true
	head_node DocNode
	content_nodes []DocNode
	time_generated time.Time
}

pub struct DocPos {
pub:
	line int
	col int
}

pub struct DocNode {
pub mut:
	name string
	content string
	comment string
	pos DocPos
	file_path string = ''
}

pub fn write_comment_bw(stmts []ast.Stmt, start_idx int) string {
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

fn convert_pos(file_path string, pos token.Position) DocPos {
    source := util.read_file(file_path) or {''}
    mut p := util.imax(0, util.imin(source.len - 1, pos.pos))
    column := util.imax(0, pos.pos - p - 1)
    return DocPos{ line: pos.line_nr+1, col: util.imax(1, column+1) }
}

pub fn (d Doc) get_signature(stmt ast.Stmt) string {
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

pub fn (d Doc) get_pos(stmt ast.Stmt) token.Position {
	match stmt {
		ast.FnDecl { return it.pos }
		ast.StructDecl { return it.pos }
		ast.EnumDecl { return it.pos }
		else { return token.Position{} }
	}
}

pub fn (d Doc) get_name(stmt ast.Stmt) string {
	match stmt {
		ast.FnDecl { return it.name }
		ast.StructDecl { return it.name }
		ast.EnumDecl { return it.name }
		else { return '' }
	}
}

pub fn new(input_path string) Doc {
	return Doc{
		input_path: os.real_path(input_path),
		prefs: &pref.Preferences{},
		table: table.new_table(),
		head_node: DocNode{},
		content_nodes: []DocNode{},
		time_generated: time.now()
	}
}

pub fn (mut d Doc) generate() ?bool {
	// get all files
	base_path := if os.is_dir(d.input_path) { d.input_path } else { os.real_path(os.base_dir(d.input_path)) }
	project_files := os.ls(base_path) or { panic(err) }
	v_files := d.prefs.should_compile_filtered_files(base_path, project_files)

	if v_files.len == 0 {
		return error('vdoc: No valid V files were found.')
	}

	// parse files
	mut file_asts := []ast.File{}

	for file in v_files {
		file_ast := parser.parse_file(
			file,
			d.table,
			.skip_comments,
			d.prefs,
			&ast.Scope{parent: 0}
		)

		file_asts << file_ast
	}

	mut module_name := ''

	for i, file_ast in file_asts {
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
				pos := d.get_pos(stmt)

				if !signature.starts_with('pub') && d.pub_only { 
					continue
				}

				d.content_nodes << DocNode{
					name: name,
					content: signature,
					comment: '',
					pos: convert_pos(v_files[i], pos),
					file_path: v_files[i]
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

	if d.content_nodes.len == 0 {
		return error('vdoc: No content was found.')
	}

	d.time_generated = time.now()
	return true
}

pub fn generate(input_path string, pub_only bool) ?Doc {
	mut doc := doc.new(input_path)
	doc.pub_only = pub_only

	_ = doc.generate() or {
		return error(err)
	}

	return doc
}