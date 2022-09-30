module doc

import os
import time
import v.ast
import v.checker
import v.fmt
import v.parser
import v.pref
import v.scanner
import v.token

// SymbolKind categorizes the symbols it documents.
// The names are intentionally not in order as a guide when sorting the nodes.
pub enum SymbolKind {
	none_
	const_group
	constant
	variable
	function
	method
	interface_
	typedef
	enum_
	enum_field
	struct_
	struct_field
}

pub enum Platform {
	auto
	ios
	macos
	linux
	windows
	freebsd
	openbsd
	netbsd
	dragonfly
	js // for interoperability in prefs.OS
	android
	termux // like android, but note that termux is running on devices natively, not cross compiling from other platforms
	solaris
	serenity
	vinix
	haiku
	raw
	cross // TODO: add functionality for v doc -os cross whenever possible
}

// copy of pref.os_from_string
pub fn platform_from_string(platform_str string) ?Platform {
	match platform_str {
		'all', 'cross' { return .cross }
		'linux' { return .linux }
		'windows' { return .windows }
		'ios' { return .ios }
		'macos' { return .macos }
		'freebsd' { return .freebsd }
		'openbsd' { return .openbsd }
		'netbsd' { return .netbsd }
		'dragonfly' { return .dragonfly }
		'js' { return .js }
		'solaris' { return .solaris }
		'serenity' { return .serenity }
		'vinix' { return .vinix }
		'android' { return .android }
		'termux' { return .termux }
		'haiku' { return .haiku }
		'nix' { return .linux }
		'' { return .auto }
		else { return error('vdoc: invalid platform `$platform_str`') }
	}
}

pub fn platform_from_filename(filename string) Platform {
	suffix := filename.all_after_last('_').all_before('.c.v')
	mut platform := platform_from_string(suffix) or { Platform.cross }
	if platform == .auto {
		platform = .cross
	}
	return platform
}

pub fn (sk SymbolKind) str() string {
	return match sk {
		.const_group { 'Constants' }
		.function, .method { 'fn' }
		.interface_ { 'interface' }
		.typedef { 'type' }
		.enum_ { 'enum' }
		.struct_ { 'struct' }
		else { '' }
	}
}

[minify]
pub struct Doc {
pub mut:
	prefs     &pref.Preferences = new_vdoc_preferences()
	base_path string
	table     &ast.Table      = ast.new_table()
	checker   checker.Checker = checker.Checker{
		table: 0
		pref: 0
	}
	fmt                 fmt.Fmt
	filename            string
	pos                 int
	pub_only            bool = true
	with_comments       bool = true
	with_pos            bool
	with_head           bool = true
	is_vlib             bool
	time_generated      time.Time
	head                DocNode
	contents            map[string]DocNode
	scoped_contents     map[string]DocNode
	parent_mod_name     string
	orig_mod_name       string
	extract_vars        bool
	filter_symbol_names []string
	common_symbols      []string
	platform            Platform
}

[minify]
pub struct DocNode {
pub mut:
	name        string
	content     string
	comments    []DocComment
	pos         token.Pos
	file_path   string
	kind        SymbolKind
	tags        []string
	parent_name string
	return_type string
	children    []DocNode
	attrs       map[string]string [json: attributes]
	from_scope  bool
	is_pub      bool              [json: public]
	platform    Platform
}

// new_vdoc_preferences creates a new instance of pref.Preferences tailored for v.doc.
pub fn new_vdoc_preferences() &pref.Preferences {
	// vdoc should be able to parse as much user code as possible
	// so its preferences should be permissive:
	mut pref := &pref.Preferences{
		enable_globals: true
		is_fmt: true
	}
	pref.fill_with_defaults()
	return pref
}

// new creates a new instance of a `Doc` struct.
pub fn new(input_path string) Doc {
	mut d := Doc{
		base_path: os.real_path(input_path)
		table: ast.new_table()
		head: DocNode{}
		contents: map[string]DocNode{}
		time_generated: time.now()
	}
	d.fmt = fmt.Fmt{
		pref: d.prefs
		indent: 0
		is_debug: false
		table: d.table
	}
	d.checker = checker.new_checker(d.table, d.prefs)
	return d
}

// stmt reads the data of an `ast.Stmt` node and returns a `DocNode`.
// An option error is thrown if the symbol is not exposed to the public
// (when `pub_only` is enabled) or the content's of the AST node is empty.
pub fn (mut d Doc) stmt(stmt ast.Stmt, filename string) ?DocNode {
	mut name := d.stmt_name(stmt)
	if name in d.common_symbols {
		return error('already documented')
	}
	if name.starts_with(d.orig_mod_name + '.') {
		name = name.all_after(d.orig_mod_name + '.')
	}
	mut node := DocNode{
		name: name
		content: d.stmt_signature(stmt)
		pos: stmt.pos
		file_path: os.join_path(d.base_path, filename)
		is_pub: d.stmt_pub(stmt)
		platform: platform_from_filename(filename)
	}
	if (!node.is_pub && d.pub_only) || stmt is ast.GlobalDecl {
		return error('symbol $node.name not public')
	}
	if node.name.starts_with(d.orig_mod_name + '.') {
		node.name = node.name.all_after(d.orig_mod_name + '.')
	}
	if node.name.len == 0 && node.comments.len == 0 && node.content.len == 0 {
		return error('empty stmt')
	}
	match stmt {
		ast.ConstDecl {
			node.kind = .const_group
			node.parent_name = 'Constants'
			if d.extract_vars {
				for field in stmt.fields {
					ret_type := if field.typ == 0 {
						d.expr_typ_to_string(field.expr)
					} else {
						d.type_to_str(field.typ)
					}
					node.children << DocNode{
						name: field.name.all_after(d.orig_mod_name + '.')
						kind: .constant
						pos: field.pos
						return_type: ret_type
					}
				}
			}
		}
		ast.EnumDecl {
			node.kind = .enum_
			if d.extract_vars {
				for field in stmt.fields {
					ret_type := if field.has_expr { d.expr_typ_to_string(field.expr) } else { 'int' }
					node.children << DocNode{
						name: field.name
						kind: .enum_field
						parent_name: node.name
						pos: field.pos
						return_type: ret_type
					}
				}
			}
		}
		ast.InterfaceDecl {
			node.kind = .interface_
		}
		ast.StructDecl {
			node.kind = .struct_
			if d.extract_vars {
				for field in stmt.fields {
					ret_type := if field.typ == 0 && field.has_default_expr {
						d.expr_typ_to_string(field.default_expr)
					} else {
						d.type_to_str(field.typ)
					}
					node.children << DocNode{
						name: field.name
						kind: .struct_field
						parent_name: node.name
						pos: field.pos
						return_type: ret_type
					}
				}
			}
		}
		ast.TypeDecl {
			node.kind = .typedef
		}
		ast.FnDecl {
			if stmt.is_deprecated {
				for sa in stmt.attrs {
					if sa.name.starts_with('deprecated') {
						node.tags << sa.str()
					}
				}
			}
			if stmt.is_unsafe {
				node.tags << 'unsafe'
			}
			node.kind = .function
			node.return_type = d.type_to_str(stmt.return_type)
			if stmt.receiver.typ !in [0, 1] {
				method_parent := d.type_to_str(stmt.receiver.typ)
				node.kind = .method
				node.parent_name = method_parent
			}
			if d.extract_vars {
				for param in stmt.params {
					node.children << DocNode{
						name: param.name
						kind: .variable
						parent_name: node.name
						pos: param.pos
						attrs: {
							'mut': param.is_mut.str()
						}
						return_type: d.type_to_str(param.typ)
					}
				}
			}
		}
		else {
			return error('invalid stmt type to document')
		}
	}
	included := node.name in d.filter_symbol_names || node.parent_name in d.filter_symbol_names
	if d.filter_symbol_names.len != 0 && !included {
		return error('not included in the list of symbol names')
	}
	if d.prefs.os == .all {
		d.common_symbols << node.name
	}
	return node
}

// file_ast reads the contents of `ast.File` and returns a map of `DocNode`s.
pub fn (mut d Doc) file_ast(file_ast ast.File) map[string]DocNode {
	mut contents := map[string]DocNode{}
	stmts := file_ast.stmts
	d.fmt.file = file_ast
	d.fmt.set_current_module_name(d.orig_mod_name)
	d.fmt.process_file_imports(file_ast)
	mut last_import_stmt_idx := 0
	for sidx, stmt in stmts {
		if stmt is ast.Import {
			last_import_stmt_idx = sidx
		}
	}
	mut preceeding_comments := []DocComment{}
	// mut imports_section := true
	for sidx, stmt in stmts {
		if stmt is ast.ExprStmt {
			// Collect comments
			if stmt.expr is ast.Comment {
				preceeding_comments << ast_comment_to_doc_comment(stmt.expr)
				continue
			}
		}
		// TODO: Fetch head comment once
		if stmt is ast.Module {
			if !d.with_head {
				continue
			}
			// the previous comments were probably a copyright/license one
			module_comment := merge_doc_comments(preceeding_comments)
			preceeding_comments = []
			if !d.is_vlib && !module_comment.starts_with('Copyright (c)') {
				if module_comment == '' {
					continue
				}
				d.head.comments << preceeding_comments
			}
			continue
		}
		if last_import_stmt_idx > 0 && sidx == last_import_stmt_idx {
			// the accumulated comments were interspersed before/between the imports;
			// just add them all to the module comments:
			if d.with_head {
				d.head.comments << preceeding_comments
			}
			preceeding_comments = []
			// imports_section = false
		}
		if stmt is ast.Import {
			continue
		}
		mut node := d.stmt(stmt, os.base(file_ast.path)) or {
			preceeding_comments = []
			continue
		}
		if node.parent_name !in contents {
			parent_node_kind := if node.parent_name == 'Constants' {
				SymbolKind.const_group
			} else {
				SymbolKind.typedef
			}
			contents[node.parent_name] = DocNode{
				name: node.parent_name
				kind: parent_node_kind
			}
		}
		if d.with_comments && (preceeding_comments.len > 0) {
			node.comments << preceeding_comments
		}
		preceeding_comments = []
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
	if contents[''].kind != .const_group {
		contents.delete('')
	}
	return contents
}

// file_ast_with_pos has the same function as the `file_ast` but
// instead returns a list of variables in a given offset-based position.
pub fn (mut d Doc) file_ast_with_pos(file_ast ast.File, pos int) map[string]DocNode {
	lscope := file_ast.scope.innermost(pos)
	mut contents := map[string]DocNode{}
	for name, val in lscope.objects {
		if val !is ast.Var {
			continue
		}
		vr_data := val as ast.Var
		l_node := DocNode{
			name: name
			pos: vr_data.pos
			file_path: file_ast.path
			from_scope: true
			kind: .variable
			return_type: d.expr_typ_to_string(vr_data.expr)
		}
		contents[l_node.name] = l_node
	}
	return contents
}

// generate is a `Doc` method that will start documentation
// process based on a file path provided.
pub fn (mut d Doc) generate() ? {
	// get all files
	d.base_path = if os.is_dir(d.base_path) {
		d.base_path
	} else {
		os.real_path(os.dir(d.base_path))
	}
	d.is_vlib = !d.base_path.contains('vlib')
	project_files := os.ls(d.base_path) or { return err }
	v_files := d.prefs.should_compile_filtered_files(d.base_path, project_files)
	if v_files.len == 0 {
		return error_with_code('vdoc: No valid V files were found.', 1)
	}
	// parse files
	mut comments_mode := scanner.CommentsMode.skip_comments
	if d.with_comments {
		comments_mode = .parse_comments
	}
	mut file_asts := []ast.File{}
	for i, file_path in v_files {
		if i == 0 {
			d.parent_mod_name = get_parent_mod(d.base_path) or { '' }
		}
		file_asts << parser.parse_file(file_path, d.table, comments_mode, d.prefs)
	}
	return d.file_asts(file_asts)
}

// file_asts has the same function as the `file_ast` function but
// accepts an array of `ast.File` and throws an error if necessary.
pub fn (mut d Doc) file_asts(file_asts []ast.File) ? {
	mut fname_has_set := false
	d.orig_mod_name = file_asts[0].mod.name
	for i, file_ast in file_asts {
		if d.filename.len > 0 && file_ast.path.contains(d.filename) && !fname_has_set {
			d.filename = file_ast.path
			fname_has_set = true
		}
		if d.with_head && i == 0 {
			mut module_name := file_ast.mod.name
			// if module_name != 'main' && d.parent_mod_name.len > 0 {
			// 	module_name = d.parent_mod_name + '.' + module_name
			// }
			d.head = DocNode{
				name: module_name
				content: 'module $module_name'
				kind: .none_
			}
		} else if file_ast.mod.name != d.orig_mod_name {
			continue
		}
		if file_ast.path == d.filename {
			d.checker.check(file_ast)
			d.scoped_contents = d.file_ast_with_pos(file_ast, d.pos)
		}
		contents := d.file_ast(file_ast)
		for name, node in contents {
			if name !in d.contents {
				d.contents[name] = node
				continue
			}
			if d.contents[name].kind == .typedef && node.kind !in [.typedef, .none_] {
				old_children := d.contents[name].children.clone()
				d.contents[name] = node
				d.contents[name].children = old_children
			}
			if d.contents[name].kind != .none_ || node.kind == .none_ {
				d.contents[name].children << node.children
				d.contents[name].children.sort_by_name()
				d.contents[name].children.sort_by_kind()
			}
		}
	}
	if d.filter_symbol_names.len != 0 && d.contents.len != 0 {
		for filter_name in d.filter_symbol_names {
			if filter_name !in d.contents {
				return error('vdoc: `$filter_name` symbol in module `$d.orig_mod_name` not found')
			}
		}
	}
	d.time_generated = time.now()
}

// generate documents a certain file directory and returns an
// instance of `Doc` if it is successful. Otherwise, it will  throw an error.
pub fn generate(input_path string, pub_only bool, with_comments bool, platform Platform, filter_symbol_names ...string) ?Doc {
	if platform == .js {
		return error('vdoc: Platform `$platform` is not supported.')
	}
	mut doc := new(input_path)
	doc.pub_only = pub_only
	doc.with_comments = with_comments
	doc.filter_symbol_names = filter_symbol_names.filter(it.len != 0)
	doc.prefs.os = if platform == .auto {
		pref.get_host_os()
	} else {
		unsafe { pref.OS(int(platform)) }
	}
	doc.generate()?
	return doc
}

// generate_with_pos has the same function as the `generate` function but
// accepts an offset-based position and enables the comments by default.
pub fn generate_with_pos(input_path string, filename string, pos int) ?Doc {
	mut doc := new(input_path)
	doc.pub_only = false
	doc.with_comments = true
	doc.with_pos = true
	doc.filename = filename
	doc.pos = pos
	doc.generate()?
	return doc
}
