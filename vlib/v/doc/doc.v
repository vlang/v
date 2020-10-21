module doc

import os
import strings
import time
import v.ast
import v.checker
import v.fmt
import v.parser
import v.pref
import v.scanner
import v.table
import v.token
import v.util

// intentionally in order as a guide when arranging the docnodes
pub enum SymbolKind {
	none_
	typedef
	interface_
	const_group
	const_field
	enum_
	variable
	function
	method
	struct_
}

pub struct Doc {
	prefs          &pref.Preferences = new_vdoc_preferences()
pub mut:
	input_path     string
	table          &table.Table = &table.Table{}
	checker        checker.Checker = checker.Checker{
	table: 0
	cur_fn: 0
	pref: 0
}
	fmt            fmt.Fmt
	filename       string
	pos            int
	pub_only       bool = true
	with_comments  bool = true
	with_pos       bool
	with_head      bool = true
	is_vlib        bool
	time_generated time.Time
	head           DocNode
	contents       map[string]DocNode
	scoped_contents map[string]DocNode
}

pub struct DocPos {
pub:
	line int
	col  int
	len  int
}

pub struct DocNode {
pub mut:
	name      string
	content   string
	comment   string
	pos       DocPos = DocPos{-1, -1, 0}
	file_path string
	kind      SymbolKind
	deprecated bool
	parent_name string
	children  []DocNode
	attrs     map[string]string
	from_scope bool
}

pub fn merge_comments(comments []ast.Comment) string {
	mut res := []string{}
	for comment in comments {
		res << comment.text.trim_left('\x01')
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
			// break
		}
		mut cmt_content := cmt.text.trim_left('\x01')
		if cmt_content.len == cmt.text.len || cmt.is_multi {
			// ignore /* */ style comments for now
			continue
			// if cmt_content.len == 0 {
			// continue
			// }
			// mut new_cmt_content := ''
			// mut is_codeblock := false
			// // println(cmt_content)
			// lines := cmt_content.split_into_lines()
			// for j, line in lines {
			// trimmed := line.trim_space().trim_left(cmt_prefix)
			// if trimmed.starts_with('- ') || (trimmed.len >= 2 && trimmed[0].is_digit() && trimmed[1] == `.`) || is_codeblock {
			// new_cmt_content += line + '\n'
			// } else if line.starts_with('```') {
			// is_codeblock = !is_codeblock
			// new_cmt_content += line + '\n'
			// } else {
			// new_cmt_content += trimmed + '\n'
			// }
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
		len: pos.len
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
	if stmt is ast.InterfaceDecl {
		return stmt.pos
	}

	return stmt.position()
}

pub fn get_type_decl_name(decl ast.TypeDecl) string {
	match decl {
		ast.SumTypeDecl, ast.FnTypeDecl, ast.AliasTypeDecl { return decl.name }
	}
}

pub fn (d Doc) get_name(stmt ast.Stmt) string {
	match stmt {
		ast.FnDecl, ast.StructDecl, ast.EnumDecl, ast.InterfaceDecl { return stmt.name }
		ast.TypeDecl { return get_type_decl_name(stmt) }
		ast.ConstDecl { return '' } // leave it blank
		else { return '' }
	}
}

pub fn new_vdoc_preferences() &pref.Preferences {
	// vdoc should be able to parse as much user code as possible
	// so its preferences should be permissive:
	return &pref.Preferences{
		enable_globals: true
	}
}

pub fn new(input_path string) Doc {
	mut d := Doc{
		input_path: os.real_path(input_path)
		table: table.new_table()
		head: DocNode{}
		contents: map[string]DocNode
		time_generated: time.now()
	}
	d.fmt = fmt.Fmt{
		indent: 0
		is_debug: false
		table: d.table
	}
	d.checker = checker.new_checker(d.table, d.prefs)
	return d
}

pub fn (mut nodes []DocNode) sort_by_name() {
	nodes.sort_with_compare(compare_nodes_by_name)
}

pub fn (mut nodes []DocNode) sort_by_kind() {
	nodes.sort_with_compare(compare_nodes_by_kind)
}

fn compare_nodes_by_kind(a &DocNode, b &DocNode) int {
	ak := int((*a).kind)
	bk := int((*b).kind)
	if ak < bk {
		return -1
	}
	if ak > bk {
		return 1
	}
	return 0
}

fn compare_nodes_by_name(a &DocNode, b &DocNode) int {
	al := a.name.to_lower()
	bl := b.name.to_lower()
	return compare_strings(al, bl)
}

pub fn (cnts map[string]DocNode) arr() []DocNode {
	mut contents := cnts.keys().map(cnts[it])
	contents.sort_by_name()
	contents.sort_by_kind()
	return contents
}

// get_parent_mod - return the parent mod name, in dot format.
// It works by climbing up the folder hierarchy, until a folder,
// that either contains main .v files, or a v.mod file is reached.
// For example, given something like /languages/v/vlib/x/websocket/tests/autobahn
// it returns `x.websocket.tests`, because /languages/v/ has v.mod file in it.
// NB: calling this is expensive, so keep the result, instead of recomputing it.
fn get_parent_mod(input_dir string) ?string {
	$if windows {
		// windows root path is C: or D:
		if input_dir.len <= 2 {
			return error('root folder reached')
		}
	} $else {
		if input_dir.len == 0 {
			return error('root folder reached')
		}
	}
	base_dir := os.dir(input_dir)
	input_dir_name := os.file_name(base_dir)
	prefs := new_vdoc_preferences()
	fentries := os.ls(base_dir) or {
		[]string{}
	}
	files := fentries.filter(!os.is_dir(it))
	if 'v.mod' in files {
		// the top level is reached, no point in climbing up further
		return ''
	}
	v_files := prefs.should_compile_filtered_files(base_dir, files)
	if v_files.len == 0 {
		parent_mod := get_parent_mod(base_dir) or {
			return input_dir_name
		}
		if parent_mod.len > 0 {
			return parent_mod + '.' + input_dir_name
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
		return input_dir_name
	}
	if parent_mod.len > 0 {
		return '${parent_mod}.${file_ast.mod.name}'
	}
	return file_ast.mod.name
}

pub fn (mut d Doc) generate_from_ast(file_ast ast.File) map[string]DocNode {
	mut contents := map[string]DocNode
	orig_mod_name := file_ast.mod.name
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
			if !d.with_head {
				continue
			}
			// the previous comments were probably a copyright/license one
			module_comment := get_comment_block_right_before(prev_comments)
			prev_comments = []
			if !d.is_vlib && !module_comment.starts_with('Copyright (c)') {
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
			if d.with_head {
				import_comments := merge_comments(prev_comments)
				if d.head.comment != '' {
					d.head.comment += '\n'
				}
				d.head.comment += import_comments
			}
			prev_comments = []
			imports_section = false
		}
		if stmt is ast.Import {
			continue
		}
		mut node := DocNode{
			name: d.get_name(stmt)
			content: d.get_signature(stmt, file_ast)
			comment: ''
			pos: convert_pos(file_ast.path, d.get_pos(stmt))
			file_path: file_ast.path
		}
		if (!node.content.starts_with('pub') && d.pub_only) || stmt is ast.GlobalDecl {
			prev_comments = []
			continue
		}
		if node.name.starts_with(orig_mod_name + '.') {
			node.name = node.name.all_after(orig_mod_name + '.')
		}
		if node.name.len == 0 && node.comment.len == 0 && node.content.len == 0 {
			continue
		}
		match stmt {
			ast.ConstDecl {
				node.kind = .const_group
				node.parent_name = 'Constants'
				if node.parent_name !in contents {
					contents[node.parent_name] = DocNode{
						name: 'Constants'
						kind: .const_group
					}
				}
			}
			ast.EnumDecl {
				node.kind = .enum_
			}
			ast.InterfaceDecl {
				node.kind = .interface_
			}
			ast.StructDecl {
				node.kind = .struct_
			}
			ast.TypeDecl {
				node.kind = .typedef
			}
			ast.FnDecl {
				node.deprecated = stmt.is_deprecated
				node.kind = .function
				if stmt.receiver.typ !in [0,1] {
					method_parent := d.fmt.table.type_to_str(stmt.receiver.typ).trim_left('&')
					if method_parent != 'void' && method_parent !in contents {
						contents[method_parent] = DocNode{
							name: method_parent
							kind: .none_
						}
					}
					node.kind = .method
					node.parent_name = method_parent
				}
			}
			else {}
		}
		if d.with_comments && (prev_comments.len > 0) {
			// last_comment := contents[contents.len - 1].comment
			// cmt := last_comment + '\n' + get_comment_block_right_before(prev_comments)
			cmt := get_comment_block_right_before(prev_comments)
			node.comment = cmt
		}
		prev_comments = []
		if node.parent_name.len > 0 {
			parent_name := node.parent_name
			if node.parent_name == 'Constants' {
				node.parent_name = ''
			}
			contents[parent_name].children << node
		} else {
			contents[node.name] = node
		}
	}
	d.fmt.mod2alias = map[string]string{}
	return contents
}

pub fn (mut d Doc) generate_from_ast_with_pos(file_ast ast.File, pos int) map[string]DocNode {
	lscope := file_ast.scope.innermost(pos)
	mut contents := map[string]DocNode
	for name, val in lscope.objects {
		if val !is ast.Var {
			continue
		}
		vr_data := val as ast.Var
		l_node := DocNode{
			name: name
			pos: convert_pos(file_ast.path, vr_data.pos)
			file_path: file_ast.path
			from_scope: true
			kind: .variable
			parent_name: d.expr_typ_to_string(vr_data.expr)
		}
		contents[l_node.name] = l_node
	}
	return contents
}

fn (mut d Doc) expr_typ_to_string(ex ast.Expr) string {
	expr_typ := d.checker.expr(ex)
	return d.fmt.table.type_to_str(expr_typ)
}

fn (mut d Doc) generate() ?Doc {
	// get all files
	base_path := if os.is_dir(d.input_path) { d.input_path } else { os.real_path(os.dir(d.input_path)) }
	d.is_vlib = 'vlib' !in base_path
	project_files := os.ls(base_path) or {
		return error_with_code(err, 0)
	}
	v_files := d.prefs.should_compile_filtered_files(base_path, project_files)
	if v_files.len == 0 {
		return error_with_code('vdoc: No valid V files were found.', 1)
	}
	// parse files
	mut comments_mode := scanner.CommentsMode.skip_comments
	if d.with_comments {
		comments_mode = .toplevel_comments
	}

	global_scope := &ast.Scope{parent: 0}
	mut parent_mod_name := ''
	mut fname_has_set := false
	mut orig_mod_name := ''
	for i, file in v_files {
		file_ast := parser.parse_file(file, d.table, comments_mode, d.prefs, global_scope)
		if d.filename.len > 0 && d.filename in file && !fname_has_set {
			d.filename = file
			fname_has_set = true
		}

		if i == 0 {
			parent_mod_name = get_parent_mod(base_path) or {
				''
			}
			mut module_name := file_ast.mod.name
			orig_mod_name = module_name
			if module_name != 'main' && parent_mod_name.len > 0 {
				module_name = parent_mod_name + '.' + module_name
			}
			if d.with_head {
				d.head = DocNode{
					name: module_name
					content: 'module $module_name'
					kind: .none_
				}
			}
		} else if file_ast.mod.name != orig_mod_name {
			continue
		}
		if file_ast.path == d.filename {
			d.checker.check(file_ast)
			d.scoped_contents = d.generate_from_ast_with_pos(file_ast, d.pos)
		}
		contents := d.generate_from_ast(file_ast)
		for name, node in contents {
			if name in d.contents && (d.contents[name].kind != .none_ || node.kind == .none_) {
				d.contents[name].children << node.children
				d.contents[name].children.sort_by_name()
				continue
			}

			d.contents[name] = node
		}
	}
	d.time_generated = time.now()
	return *d
}

pub fn generate_from_pos(input_path string, filename string, pos int) ?Doc {
	mut doc := new(input_path)
	doc.pub_only = false
	doc.with_comments = true
	doc.with_pos = true
	doc.filename = filename
	doc.pos = pos
	return doc.generate()
}

pub fn generate(input_path string, pub_only bool, with_comments bool) ?Doc {
	mut doc := new(input_path)
	doc.pub_only = pub_only
	doc.with_comments = with_comments
	return doc.generate()
}
