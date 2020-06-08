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

pub fn get_comment_block_right_before(stmts []ast.Stmt) string {
	if stmts.len == 0 {
		return ''
	}
	mut comment := ''
	mut last_comment_line_nr := 0
	for i := stmts.len-1; i >= 0; i-- {
		stmt := stmts[i]
		if stmt !is ast.Comment {
			panic('Not a comment')
		}
		cmt := stmt as ast.Comment
		if last_comment_line_nr != 0 && cmt.pos.line_nr < last_comment_line_nr - 1 {
			// skip comments that are not part of a continuous block,
			// located right above the top level statement.
			//			break
		}
		mut cmt_content := cmt.text.trim_left('|')
		if cmt_content.len == cmt.text.len || cmt.is_multi {
			// ignore /* */ style comments for now
			continue
			// if cmt_content.len == 0 {
			// 	continue
			// }
			// mut new_cmt_content := ''
			// mut is_codeblock := false
			// // println(cmt_content)
			// lines := cmt_content.split_into_lines()
			// for j, line in lines {
			// 	trimmed := line.trim_space().trim_left(cmt_prefix)
			// 	if trimmed.starts_with('- ') || (trimmed.len >= 2 && trimmed[0].is_digit() && trimmed[1] == `.`) || is_codeblock {
			// 		new_cmt_content += line + '\n'
			// 	} else if line.starts_with('```') {
			// 		is_codeblock = !is_codeblock
			// 		new_cmt_content += line + '\n'
			// 	} else {
			// 		new_cmt_content += trimmed + '\n'
			// 	}
			// }
			// return new_cmt_content
		}
		//eprintln('cmt: $cmt')
		cseparator := if cmt_content.starts_with('```') {'\n'} else {' '}
		comment = cmt_content + cseparator + comment
		last_comment_line_nr = cmt.pos.line_nr
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
		cur_mod: d.head.name.split('.').last()
		indent: 0
		is_debug: false
	}
	match stmt {
		ast.Module {
			return 'module $it.name'
		}
		ast.FnDecl {
			return it.str(d.table).replace(f.cur_mod + '.', '')
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

pub fn (d Doc) get_type_name(decl ast.TypeDecl) string {
	match decl {
		ast.SumTypeDecl { return it.name }
		ast.FnTypeDecl { return it.name }
		ast.AliasTypeDecl { return it.name }
	}
}

pub fn (d Doc) get_name(stmt ast.Stmt) string {
	cur_mod := d.head.name.split('.').last()
	match stmt {
		ast.FnDecl { return it.name }
		ast.StructDecl { return it.name }
		ast.EnumDecl { return it.name }
		ast.InterfaceDecl { return it.name }
		ast.TypeDecl { return d.get_type_name(it).replace('&' + cur_mod + '.', '').replace(cur_mod + '.', '') }
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

pub fn (nodes []DocNode) index_by_name(node_name string) ?int {
	for i, node in nodes {
		if node.name != node_name { continue }
		return i
	}
	return error('Node with the name "$node_name" was not found.')
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
	$if windows {
		// windows root path is C: or D:
		if dir.len <= 2 { return error('root folder reached') }
	} $else {
		if dir.len == 0 { return error('root folder reached') }
	}
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
	tbl := table.new_table()
	scope := &ast.Scope{ parent: 0 }
	file_ast := parser.parse_file(v_files[0], tbl, .skip_comments, prefs, scope)
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
	comments_mode := if d.with_comments { scanner.CommentsMode.toplevel_comments } else { scanner.CommentsMode.skip_comments }
	for file in v_files {
		file_ast := parser.parse_file(file, d.table, comments_mode, d.prefs, &ast.Scope{
			parent: 0
		})
		file_asts << file_ast
	}
	mut module_name := ''
	mut parent_mod_name := ''
	mut orig_mod_name := ''
	mut const_idx := -1
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
		} else if file_ast.mod.name != orig_mod_name {
			continue
		}
		mut prev_comments := []ast.Stmt{}
		stmts := file_ast.stmts
		for o, stmt in stmts {
			//eprintln('stmt typeof: ' + typeof(stmt))
			if stmt is ast.Comment {
				prev_comments << stmt
				continue
			}
			if stmt is ast.Module {
				// the previous comments were probably a copyright/license one
				module_comment := get_comment_block_right_before(prev_comments)
				prev_comments = []
				if module_comment == '' {
					continue
				}
				if module_comment == d.head.comment {
					continue
				}
				if d.head.comment != '' {
					d.head.comment += '\n'
				}
				d.head.comment += module_comment
				continue
			}
			signature := d.get_signature(stmt)
			pos := d.get_pos(stmt)
			mut name := d.get_name(stmt)
			if (!signature.starts_with('pub') && d.pub_only) || stmt is ast.GlobalDecl {
				prev_comments = []
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
			if stmt is ast.ConstDecl {
				if const_idx == -1 {
					const_idx = o
				} else {
					node.parent_type = 'Constants'
				}
			}
			if node.name.len == 0 && node.comment.len == 0 && node.content.len == 0 {
				continue
			}
			d.contents << node
			if d.with_comments && (prev_comments.len > 0) {
				last_comment := d.contents[d.contents.len - 1].comment
				cmt := last_comment + '\n' + get_comment_block_right_before(prev_comments)
				d.contents[d.contents.len - 1].comment = cmt
			}
			prev_comments = []
		}
	}
	d.time_generated = time.now()
	return true
}

pub fn generate(input_path string, pub_only, with_comments bool) ?Doc {
	mut doc := new(input_path)
	doc.pub_only = pub_only
	doc.with_comments = with_comments
	doc.generate() or {
		return error(err)
	}
	return doc
}
