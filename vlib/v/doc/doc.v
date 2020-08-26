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
	fmt            fmt.Fmt
	time_generated time.Time
}

pub struct DocPos {
pub:
	line int
	col  int
}

pub struct DocNode {
pub mut:
	name      string
	content   string = ''
	comment   string
	pos       DocPos = DocPos{-1, -1}
	file_path string = ''
	attrs     map[string]string
}

pub fn merge_comments(comments []ast.Comment) string {
	mut res := []string{}
	for comment in comments {
		res << comment.text.trim_left('|')
	}
	return res.join('\n')
}

pub fn get_comment_block_right_before(comments []ast.Comment) string {
	if comments.len == 0 {
		return ''
	}
	mut comment := ''
	mut last_comment_line_nr := 0
	for i := comments.len - 1; i >= 0; i-- {
		cmt := comments[i]
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
		// eprintln('cmt: $cmt')
		cseparator := if cmt_content.starts_with('```') { '\n' } else { ' ' }
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

pub fn (mut d Doc) get_signature(stmt ast.Stmt, file &ast.File) string {
	match stmt {
		ast.Module {
			return 'module $stmt.name'
		}
		ast.FnDecl {
			return stmt.stringify(d.table, d.fmt.cur_mod)
		}
		else {
			d.fmt.out = strings.new_builder(1000)
			d.fmt.stmt(stmt)
			return d.fmt.out.str().trim_space()
		}
	}
}

pub fn (d Doc) get_pos(stmt ast.Stmt) token.Position {
	match stmt {
		ast.FnDecl { return stmt.pos }
		ast.StructDecl { return stmt.pos }
		ast.EnumDecl { return stmt.pos }
		ast.InterfaceDecl { return stmt.pos }
		ast.ConstDecl { return stmt.pos }
		else { return token.Position{} }
	}
}

pub fn (d Doc) get_type_name(decl ast.TypeDecl) string {
	match decl {
		ast.SumTypeDecl { return decl.name }
		ast.FnTypeDecl { return decl.name }
		ast.AliasTypeDecl { return decl.name }
	}
}

pub fn (d Doc) get_name(stmt ast.Stmt) string {
	match stmt {
		ast.FnDecl { return stmt.name }
		ast.StructDecl { return stmt.name }
		ast.EnumDecl { return stmt.name }
		ast.InterfaceDecl { return stmt.name }
		ast.TypeDecl { return d.get_type_name(stmt) }
		ast.ConstDecl { return 'Constants' }
		else { return '' }
	}
}

pub fn new(input_path string) Doc {
	mut d := Doc{
		input_path: os.real_path(input_path)
		prefs: &pref.Preferences{}
		table: table.new_table()
		head: DocNode{}
		contents: []DocNode{}
		time_generated: time.now()
	}
	d.fmt = fmt.Fmt{
		indent: 0
		is_debug: false
		table: d.table
	}
	return d
}

pub fn (mut nodes []DocNode) sort_by_name() {
	nodes.sort_with_compare(compare_nodes_by_name)
}

pub fn (mut nodes []DocNode) sort_by_category() {
	nodes.sort_with_compare(compare_nodes_by_category)
}

fn compare_nodes_by_name(a, b &DocNode) int {
	al := a.name.to_lower()
	bl := b.name.to_lower()
	return compare_strings(al, bl)
}

fn compare_nodes_by_category(a, b &DocNode) int {
	al := a.attrs['category']
	bl := b.attrs['category']
	return compare_strings(al, bl)
}

pub fn (nodes []DocNode) index_by_name(node_name string) int {
	for i, node in nodes {
		if node.name != node_name {
			continue
		}
		return i
	}
	return -1
}

pub fn (nodes []DocNode) find_children_of(parent string) []DocNode {
	return nodes.find_nodes_with_attr('parent', parent)
}

pub fn (nodes []DocNode) find_nodes_with_attr(attr_name, value string) []DocNode {
	mut subgroup := []DocNode{}
	if attr_name.len == 0 {
		return subgroup
	}
	for node in nodes {
		if !node.attrs.exists(attr_name) || node.attrs[attr_name] != value {
			continue
		}
		subgroup << node
	}
	subgroup.sort_by_name()
	return subgroup
}

fn get_parent_mod(dir string) ?string {
	$if windows {
		// windows root path is C: or D:
		if dir.len <= 2 {
			return error('root folder reached')
		}
	} $else {
		if dir.len == 0 {
			return error('root folder reached')
		}
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
	scope := &ast.Scope{
		parent: 0
	}
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

fn (mut d Doc) generate() ?Doc {
	// get all files
	base_path := if os.is_dir(d.input_path) { d.input_path } else { os.real_path(os.base_dir(d.input_path)) }
	project_files := os.ls(base_path) or {
		return error_with_code(err, 0)
	}
	v_files := d.prefs.should_compile_filtered_files(base_path, project_files)
	if v_files.len == 0 {
		return error_with_code('vdoc: No valid V files were found.', 1)
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
		stmts := file_ast.stmts
		d.fmt.file = file_ast
		d.fmt.set_current_module_name(orig_mod_name)
		d.fmt.process_file_imports(file_ast)
		mut last_import_stmt_idx := 0
		for sidx, stmt in stmts {
			if stmt is ast.Import {
				last_import_stmt_idx = sidx
			}
		}
		mut prev_comments := []ast.Comment{}
		mut imports_section := true
		for sidx, stmt in stmts {
			// eprintln('stmt typeof: ' + typeof(stmt))
			if stmt is ast.ExprStmt {
				if stmt.expr is ast.Comment as cmt {
					prev_comments << cmt
					continue
				}
			}
			// TODO: Fetch head comment once
			if stmt is ast.Module {
				// the previous comments were probably a copyright/license one
				module_comment := get_comment_block_right_before(prev_comments)
				prev_comments = []
				if 'vlib' !in base_path && !module_comment.starts_with('Copyright (c)') {
					if module_comment in ['', d.head.comment] {
						continue
					}
					if d.head.comment != '' {
						d.head.comment += '\n'
					}
					d.head.comment += module_comment
				}
				continue
			}
			if last_import_stmt_idx > 0 && sidx == last_import_stmt_idx {
				// the accumulated comments were interspersed before/between the imports;
				// just add them all to the module comment:
				import_comments := merge_comments(prev_comments)
				if d.head.comment != '' {
					d.head.comment += '\n'
				}
				d.head.comment += import_comments
				prev_comments = []
				imports_section = false
			}
			if stmt is ast.Import {
				continue
			}
			signature := d.get_signature(stmt, file_ast)
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
			if node.name.len == 0 && node.comment.len == 0 && node.content.len == 0 {
				continue
			}
			if stmt is ast.FnDecl {
				if stmt.is_deprecated {
					continue
				}
				if stmt.receiver.typ != 0 {
					node.attrs['parent'] = d.fmt.type_to_str(stmt.receiver.typ).trim_left('&')
					p_idx := d.contents.index_by_name(node.attrs['parent'])
					if p_idx == -1 && node.attrs['parent'] != 'void' {
						d.contents << DocNode{
							name: node.attrs['parent']
							content: ''
							comment: ''
							attrs: {
								'category': 'Structs'
							}
						}
					}
				}
			}
			if stmt is ast.ConstDecl {
				if const_idx == -1 {
					const_idx = sidx
				} else {
					node.attrs['parent'] = 'Constants'
				}
			}
			match stmt {
				ast.ConstDecl { node.attrs['category'] = 'Constants' }
				ast.EnumDecl { node.attrs['category'] = 'Enums' }
				ast.InterfaceDecl { node.attrs['category'] = 'Interfaces' }
				ast.StructDecl { node.attrs['category'] = 'Structs' }
				ast.TypeDecl { node.attrs['category'] = 'Typedefs' }
				ast.FnDecl {
					node.attrs['category'] = if node.attrs['parent'] in ['void', ''] || !node.attrs.exists('parent') {
						'Functions'
					} else {
						'Methods'
					}
				}
				else {}
			}
			d.contents << node
			if d.with_comments && (prev_comments.len > 0) {
				last_comment := d.contents[d.contents.len - 1].comment
				cmt := last_comment + '\n' + get_comment_block_right_before(prev_comments)
				d.contents[d.contents.len - 1].comment = cmt
			}
			prev_comments = []
		}
		d.fmt.mod2alias = map[string]string{}
	}
	d.time_generated = time.now()
	d.contents.sort_by_name()
	d.contents.sort_by_category()
	return *d
}

pub fn generate(input_path string, pub_only, with_comments bool) ?Doc {
	mut doc := new(input_path)
	doc.pub_only = pub_only
	doc.with_comments = with_comments
	return doc.generate()
}
