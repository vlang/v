module doc

import os
import strings
import time
import v.ast
import v.fmt
import v.parser
import v.pref
import v.scanner
import v.table
import v.token
import v.util

pub struct Doc {
pub mut:
	input_path     string = ''
	prefs          &pref.Preferences = &pref.Preferences{}
	table          &table.Table = &table.Table{}
	pub_only       bool = true
	head           DocNode
	with_comments  bool = true
	contents       []DocNode
	time_generated time.Time
}

pub struct DocPos {
pub:
	line int
	col  int
}

pub struct DocNode {
pub mut:
	name        string
	content     string = ''
	comment     string
	pos         DocPos = DocPos{-1, -1}
	file_path   string = ''
	parent_type string = ''
}

pub fn write_comment_bw(stmts []ast.Stmt, start_idx int) string {
	mut comment := ''
	for i := start_idx; i >= 0; i-- {
		stmt := stmts[i]
		if stmt is ast.Comment {
			cmt := stmt as ast.Comment
			cmt_content := cmt.text.trim_left('|')
			comment = cmt_content + if cmt_content.starts_with('```') {
				'\n'
			} else {
				' '
			} + comment
		} else {
			panic('Not a comment')
		}
		if i - 1 >= 0 && !(stmts[i - 1] is ast.Comment) {
			break
		}
	}
	return comment
}

fn convert_pos(file_path string, pos token.Position) DocPos {
	source := util.read_file(file_path) or {
		''
	}
	mut p := util.imax(0, util.imin(source.len - 1, pos.pos))
	column := util.imax(0, pos.pos - p - 1)
	return DocPos{
		line: pos.line_nr + 1
		col: util.imax(1, column + 1)
	}
}

pub fn (d Doc) get_signature(stmt ast.Stmt) string {
	mut f := fmt.Fmt{
		out: strings.new_builder(1000)
		out_imports: strings.new_builder(200)
		table: d.table
		indent: 0
		is_debug: false
	}
	match stmt {
		ast.Module {
			return 'module $it.name'
		}
		ast.FnDecl {
			return it.str(d.table)
		}
		else {
			f.stmt(stmt)
			return f.out.str().trim_space()
		}
	}
}

pub fn (d Doc) get_pos(stmt ast.Stmt) token.Position {
	match stmt {
		ast.FnDecl { return it.pos }
		ast.StructDecl { return it.pos }
		ast.EnumDecl { return it.pos }
		ast.InterfaceDecl { return it.pos }
		ast.ConstDecl { return it.pos }
		else { return token.Position{} }
	}
}

pub fn (d Doc) get_name(stmt ast.Stmt) string {
	match stmt {
		ast.FnDecl { return it.name }
		ast.StructDecl { return it.name }
		ast.EnumDecl { return it.name }
		ast.InterfaceDecl { return it.name }
		ast.ConstDecl { return 'Constants' }
		else { return '' }
	}
}

pub fn new(input_path string) Doc {
	return Doc{
		input_path: os.real_path(input_path)
		prefs: &pref.Preferences{}
		table: table.new_table()
		head: DocNode{}
		contents: []DocNode{}
		time_generated: time.now()
	}
}

pub fn (nodes []DocNode) find_children_of(parent_type string) []DocNode {
	if parent_type.len == 0 {
		return []DocNode{}
	}
	mut children := []DocNode{}
	for node in nodes {
		if node.parent_type != parent_type {
			continue
		}
		children << node
	}
	return children
}

fn get_parent_mod(dir string) ?string {
	if dir.len == 0  { return error('root folder reached') }
	base_dir := os.base_dir(dir)
	if os.file_name(base_dir) in ['encoding', 'v'] && 'vlib' in base_dir {
		return os.file_name(base_dir)
	}
	prefs := &pref.Preferences{}
	files := os.ls(base_dir) or {
		[]string{}
	}
	v_files := prefs.should_compile_filtered_files(base_dir, files)
	if v_files.len == 0 {
		parent_mod := get_parent_mod(base_dir) or {
			''
		}
		if parent_mod.len > 0 {
			return parent_mod + '.' + os.file_name(base_dir)
		}
		return error('No V files found.')
	}
	file_ast := parser.parse_file(v_files[0], table.new_table(), .skip_comments, prefs, &ast.Scope{
		parent: 0
	})
	if file_ast.mod.name == 'main' {
		return ''
	}
	parent_mod := get_parent_mod(base_dir) or {
		''
	}
	if parent_mod.len > 0 {
		return parent_mod + '.' + file_ast.mod.name
	}
	return file_ast.mod.name
}

pub fn (mut d Doc) generate() ?bool {
	// get all files
	base_path := if os.is_dir(d.input_path) { d.input_path } else { os.real_path(os.base_dir(d.input_path)) }
	project_files := os.ls(base_path) or {
		panic(err)
	}
	v_files := d.prefs.should_compile_filtered_files(base_path, project_files)
	if v_files.len == 0 {
		return error('vdoc: No valid V files were found.')
	}
	// parse files
	mut file_asts := []ast.File{}
	// TODO: remove later for vlib
	comments_mode := if d.with_comments { scanner.CommentsMode.parse_comments } else { scanner.CommentsMode.skip_comments }
	for file in v_files {
		file_ast := parser.parse_file(file, d.table, comments_mode, d.prefs, &ast.Scope{
			parent: 0
		})
		file_asts << file_ast
	}
	mut module_name := ''
	mut parent_mod_name := ''
	mut orig_mod_name := ''
	for i, file_ast in file_asts {
		if i == 0 {
			parent_mod_name = get_parent_mod(base_path) or {
				''
			}
			module_name = file_ast.mod.name
			orig_mod_name = module_name
			if module_name != 'main' && parent_mod_name.len > 0 {
				module_name = parent_mod_name + '.' + module_name
			}
			d.head = DocNode{
				name: module_name
				content: 'module $module_name'
				comment: ''
			}
		} else if file_ast.mod.name != module_name {
			continue
		}
		stmts := file_ast.stmts
		for si, stmt in stmts {
			if stmt is ast.Comment {
				continue
			}
			if stmt !is ast.Module {
				// todo: accumulate consts
				mut name := d.get_name(stmt)
				signature := d.get_signature(stmt)
				pos := d.get_pos(stmt)
				if !signature.starts_with('pub') && d.pub_only {
					continue
				}
				if name.starts_with(orig_mod_name + '.') {
					name = name.all_after(orig_mod_name + '.')
				}
				mut node := DocNode{
					name: name
					content: signature
					comment: ''
					pos: convert_pos(v_files[i], pos)
					file_path: v_files[i]
				}
				if stmt is ast.FnDecl {
					fnd := stmt as ast.FnDecl
					if fnd.receiver.typ != 0 {
						mut parent_type := d.table.get_type_name(fnd.receiver.typ)
						if parent_type.starts_with(module_name + '.') {
							parent_type = parent_type.all_after(module_name + '.')
						}
						node.parent_type = parent_type
					}
				}
				d.contents << node
			}
			if d.with_comments && (si - 1 >= 0 && stmts[si - 1] is ast.Comment) {
				if stmt is ast.Module {
					d.head.comment = write_comment_bw(stmts, si - 1)
				} else {
					last_comment := d.contents[d.contents.len - 1].comment
					d.contents[d.contents.len - 1].comment = last_comment + '\n' + write_comment_bw(stmts,
						si - 1)
				}
			}
		}
	}
	d.time_generated = time.now()
	return true
}

pub fn generate(input_path string, pub_only, with_comments bool) ?Doc {
	mut doc := new(input_path)
	doc.pub_only = pub_only
	doc.with_comments = with_comments
	_ = doc.generate() or {
		return error(err)
	}
	return doc
}
