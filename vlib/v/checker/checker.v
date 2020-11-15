// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import os
import v.ast
import v.vmod
import v.table
import v.token
import v.pref
import v.util
import v.errors
import v.pkgconfig

const (
	max_nr_errors                 = 300
	match_exhaustive_cutoff_limit = 10
	int_min                       = int(0x80000000)
	int_max                       = 0x7FFFFFFF
)

const (
	valid_comp_if_os        = ['windows', 'ios', 'macos', 'mach', 'darwin', 'hpux', 'gnu', 'qnx',
		'linux', 'freebsd', 'openbsd', 'netbsd', 'bsd', 'dragonfly', 'android', 'solaris', 'haiku', 'linux_or_macos']
	valid_comp_if_compilers = ['gcc', 'tinyc', 'clang', 'mingw', 'msvc', 'cplusplus']
	valid_comp_if_platforms = ['amd64', 'aarch64', 'x64', 'x32', 'little_endian', 'big_endian']
	valid_comp_if_other     = ['js', 'debug', 'test', 'glibc', 'prealloc', 'no_bounds_checking']
)

pub struct Checker {
	pref                             &pref.Preferences // Preferences shared from V struct
pub mut:
	table                            &table.Table
	file                             &ast.File = 0
	nr_errors                        int
	nr_warnings                      int
	errors                           []errors.Error
	warnings                         []errors.Warning
	error_lines                      []int // to avoid printing multiple errors for the same line
	expected_type                    table.Type
	cur_fn                           &ast.FnDecl // current function
	const_decl                       string
	const_deps                       []string
	const_names                      []string
	global_names                     []string
	locked_names                     []string // vars that are currently locked
	rlocked_names                    []string // vars that are currently read-locked
	in_for_count                     int // if checker is currently in a for loop
	// checked_ident  string // to avoid infinite checker loops
	returns                          bool
	scope_returns                    bool
	mod                              string // current module name
	is_builtin_mod                   bool // are we in `builtin`?
	inside_unsafe                    bool
	skip_flags                       bool // should `#flag` and `#include` be skipped
	cur_generic_type                 table.Type
mut:
	expr_level                       int // to avoid infinite recursion segfaults due to compiler bugs
	inside_sql                       bool // to handle sql table fields pseudo variables
	cur_orm_ts                       table.TypeSymbol
	error_details                    []string
	generic_funcs                    []&ast.FnDecl
	vmod_file_content                string // needed for @VMOD_FILE, contents of the file, *NOT its path**
	vweb_gen_types                   []table.Type // vweb route checks
	prevent_sum_type_unwrapping_once bool // needed for assign new values to sum type, stopping unwrapping then
}

pub fn new_checker(table &table.Table, pref &pref.Preferences) Checker {
	return Checker{
		table: table
		pref: pref
		cur_fn: 0
	}
}

pub fn (mut c Checker) check(ast_file &ast.File) {
	c.file = ast_file
	for i, ast_import in ast_file.imports {
		for j in 0 .. i {
			if ast_import.mod == ast_file.imports[j].mod {
				c.error('module name `$ast_import.mod` duplicate', ast_import.pos)
			}
		}
	}
	for stmt in ast_file.stmts {
		c.expr_level = 0
		c.stmt(stmt)
	}
	c.check_scope_vars(c.file.scope)
	c.post_process_generic_fns()
}

pub fn (mut c Checker) check_scope_vars(sc &ast.Scope) {
	for _, obj in sc.objects {
		match obj {
			ast.Var {
				if !c.pref.is_repl {
					if !obj.is_used && obj.name[0] != `_` {
						c.warn('unused variable: `$obj.name`', obj.pos)
					}
				}
				if obj.is_mut && !obj.is_changed && !c.is_builtin_mod && obj.name != 'it' {
					// if obj.is_mut && !obj.is_changed && !c.is_builtin {  //TODO C error bad field not checked
					// c.warn('`$obj.name` is declared as mutable, but it was never changed',
					// obj.pos)
				}
			}
			else {}
		}
	}
	for _, child in sc.children {
		c.check_scope_vars(child)
	}
}

// not used right now
pub fn (mut c Checker) check2(ast_file &ast.File) []errors.Error {
	c.file = ast_file
	for stmt in ast_file.stmts {
		c.stmt(stmt)
	}
	return c.errors
}

pub fn (mut c Checker) check_files(ast_files []ast.File) {
	mut has_main_mod_file := false
	mut has_main_fn := false
	mut files_from_main_module := []&ast.File{}
	for i in 0 .. ast_files.len {
		file := unsafe {&ast_files[i]}
		c.check(file)
		if file.mod.name == 'main' {
			files_from_main_module << file
			has_main_mod_file = true
			if c.check_file_in_main(file) {
				has_main_fn = true
			}
		}
	}
	if has_main_mod_file && !has_main_fn && files_from_main_module.len > 0 {
		if c.pref.is_script && !c.pref.is_test {
			mut first_main_file := files_from_main_module[0]
			first_main_file.stmts << ast.FnDecl{
				name: 'main.main'
				mod: 'main'
				file: first_main_file.path
				return_type: table.void_type
			}
			has_main_fn = true
		}
	}
	c.verify_all_vweb_routes()
	// Make sure fn main is defined in non lib builds
	if c.pref.build_mode == .build_module || c.pref.is_test {
		return
	}
	if c.pref.is_shared {
		// shared libs do not need to have a main
		return
	}
	if !has_main_mod_file {
		c.error('project must include a `main` module or be a shared library (compile with `v -shared`)',
			token.Position{})
	} else if !has_main_fn {
		c.error('function `main` must be declared in the main module', token.Position{})
	}
}

const (
	no_pub_in_main_warning = 'in module main cannot be declared public'
)

// do checks specific to files in main module
// returns `true` if a main function is in the file
fn (mut c Checker) check_file_in_main(file ast.File) bool {
	mut has_main_fn := false
	for stmt in file.stmts {
		match stmt {
			ast.ConstDecl {
				if stmt.is_pub {
					c.warn('const $no_pub_in_main_warning', stmt.pos)
				}
			}
			/*
			// TODO not a Stmt
			ast.ConstField {
				if stmt.is_pub {
					c.warn('const field `$stmt.name` $no_pub_in_main_warning', stmt.pos)
				}
			}
			*/
			ast.EnumDecl {
				if stmt.is_pub {
					c.warn('enum `$stmt.name` $no_pub_in_main_warning', stmt.pos)
				}
			}
			ast.FnDecl {
				if stmt.name == 'main.main' {
					if has_main_fn {
						c.error('function `main` is already defined', stmt.pos)
					}
					has_main_fn = true
					if stmt.is_pub {
						c.error('function `main` cannot be declared public', stmt.pos)
					}
					if stmt.params.len > 0 {
						c.error('function `main` cannot have arguments', stmt.pos)
					}
					if stmt.return_type != table.void_type {
						c.error('function `main` cannot return values', stmt.pos)
					}
				} else {
					if stmt.is_pub && !stmt.is_method {
						c.warn('function `$stmt.name` $no_pub_in_main_warning', stmt.pos)
					}
				}
				if stmt.return_type != table.void_type {
					for attr in stmt.attrs {
						if attr.is_ctdefine {
							c.error('only functions that do NOT return values can have `[if $attr.name]` tags',
								stmt.pos)
							break
						}
					}
				}
			}
			ast.StructDecl {
				if stmt.is_pub {
					c.warn('struct `$stmt.name` $no_pub_in_main_warning', stmt.pos)
				}
			}
			ast.TypeDecl {
				if stmt is ast.AliasTypeDecl {
					if stmt.is_pub {
						c.warn('type alias `$stmt.name` $no_pub_in_main_warning', stmt.pos)
					}
				} else if stmt is ast.SumTypeDecl {
					if stmt.is_pub {
						c.warn('sum type `$stmt.name` $no_pub_in_main_warning', stmt.pos)
					}
				} else if stmt is ast.FnTypeDecl {
					if stmt.is_pub {
						c.warn('type alias `$stmt.name` $no_pub_in_main_warning', stmt.pos)
					}
				}
			}
			else {}
		}
	}
	return has_main_fn
}

fn (mut c Checker) check_valid_snake_case(name string, identifier string, pos token.Position) {
	if !c.pref.is_vweb && (name[0] == `_` || name.contains('._')) {
		c.error('$identifier `$name` cannot start with `_`', pos)
	}
	if !c.pref.experimental && !c.pref.translated && util.contains_capital(name) {
		c.error('$identifier `$name` cannot contain uppercase letters, use snake_case instead',
			pos)
	}
}

fn stripped_name(name string) string {
	idx := name.last_index('.') or {
		-1
	}
	return name[(idx + 1)..]
}

fn (mut c Checker) check_valid_pascal_case(name string, identifier string, pos token.Position) {
	sname := stripped_name(name)
	if !sname[0].is_capital() && !c.pref.translated {
		c.error('$identifier `$name` must begin with capital letter', pos)
	}
}

pub fn (mut c Checker) type_decl(node ast.TypeDecl) {
	match node {
		ast.AliasTypeDecl {
			// TODO Replace `c.file.mod.name != 'time'` by `it.language != .v` once available
			if c.file.mod.name != 'time' && c.file.mod.name != 'builtin' {
				c.check_valid_pascal_case(node.name, 'type alias', node.pos)
			}
			typ_sym := c.table.get_type_symbol(node.parent_type)
			if typ_sym.kind == .placeholder {
				c.error("type `$typ_sym.source_name` doesn't exist", node.pos)
			} else if typ_sym.kind == .alias {
				orig_sym := c.table.get_type_symbol((typ_sym.info as table.Alias).parent_type)
				c.error('type `$typ_sym.str()` is an alias, use the original alias type `$orig_sym.source_name` instead',
					node.pos)
			} else if typ_sym.kind == .chan {
				c.error('aliases of `chan` types are not allowed.', node.pos)
			}
		}
		ast.FnTypeDecl {
			c.check_valid_pascal_case(node.name, 'fn type', node.pos)
			typ_sym := c.table.get_type_symbol(node.typ)
			fn_typ_info := typ_sym.info as table.FnType
			fn_info := fn_typ_info.func
			ret_sym := c.table.get_type_symbol(fn_info.return_type)
			if ret_sym.kind == .placeholder {
				c.error("type `$ret_sym.source_name` doesn't exist", node.pos)
			}
			for arg in fn_info.params {
				arg_sym := c.table.get_type_symbol(arg.typ)
				if arg_sym.kind == .placeholder {
					c.error("type `$arg_sym.source_name` doesn't exist", node.pos)
				}
			}
		}
		ast.SumTypeDecl {
			c.check_valid_pascal_case(node.name, 'sum type', node.pos)
			for typ in node.sub_types {
				typ_sym := c.table.get_type_symbol(typ)
				if typ_sym.kind == .placeholder {
					c.error("type `$typ_sym.source_name` doesn't exist", node.pos)
				} else if typ_sym.kind == .interface_ {
					c.error('sum type cannot hold an interface', node.pos)
				}
			}
		}
		ast.UnionSumTypeDecl {
			c.check_valid_pascal_case(node.name, 'sum type', node.pos)
			for typ in node.sub_types {
				mut sym := c.table.get_type_symbol(typ)
				if sym.kind == .placeholder {
					c.error("type `$sym.source_name` doesn't exist", node.pos)
				} else if sym.kind == .interface_ {
					c.error('sum type cannot hold an interface', node.pos)
				}
			}
		}
	}
}

pub fn (mut c Checker) interface_decl(decl ast.InterfaceDecl) {
	c.check_valid_pascal_case(decl.name, 'interface name', decl.pos)
	for method in decl.methods {
		c.check_valid_snake_case(method.name, 'method name', method.pos)
	}
}

pub fn (mut c Checker) struct_decl(decl ast.StructDecl) {
	if decl.language == .v && !c.is_builtin_mod {
		c.check_valid_pascal_case(decl.name, 'struct name', decl.pos)
	}
	struct_sym := c.table.find_type(decl.name) or {
		table.TypeSymbol{}
	}
	mut struct_info := struct_sym.info as table.Struct
	for i, field in decl.fields {
		if decl.language == .v && !field.is_embed {
			c.check_valid_snake_case(field.name, 'field name', field.pos)
		}
		sym := c.table.get_type_symbol(field.typ)
		if field.is_embed {
			if sym.info is table.Struct as sym_info {
				for embed_field in sym_info.fields {
					already_exists := struct_info.fields.filter(it.name == embed_field.name).len > 0
					if !already_exists {
						struct_info.fields << {
							embed_field |
							embed_alias_for: field.name
						}
					}
				}
			} else {
				c.error('`$sym.name` is not a struct', field.pos)
			}
		}
		for j in 0 .. i {
			if field.name == decl.fields[j].name {
				c.error('field name `$field.name` duplicate', field.pos)
			}
		}
		if sym.kind == .placeholder && decl.language != .c && !sym.name.starts_with('C.') {
			c.error(util.new_suggestion(sym.source_name, c.table.known_type_names()).say('unknown type `$sym.source_name`'),
				field.type_pos)
		}
		if sym.kind == .array {
			array_info := sym.array_info()
			elem_sym := c.table.get_type_symbol(array_info.elem_type)
			if elem_sym.kind == .placeholder {
				c.error(util.new_suggestion(elem_sym.source_name, c.table.known_type_names()).say('unknown type `$elem_sym.source_name`'),
					field.type_pos)
			}
		}
		if sym.kind == .struct_ {
			info := sym.info as table.Struct
			if info.is_ref_only && !field.typ.is_ptr() {
				c.error('`$sym.source_name` type can only be used as a reference: `&$sym.source_name`',
					field.type_pos)
			}
		}
		if sym.kind == .map {
			info := sym.map_info()
			key_sym := c.table.get_type_symbol(info.key_type)
			value_sym := c.table.get_type_symbol(info.value_type)
			if key_sym.kind == .placeholder {
				c.error('unknown type `$key_sym.source_name`', field.type_pos)
			}
			if value_sym.kind == .placeholder {
				c.error('unknown type `$value_sym.source_name`', field.type_pos)
			}
		}
		if field.has_default_expr {
			c.expected_type = field.typ
			field_expr_type := c.expr(field.default_expr)
			c.check_expected(field_expr_type, field.typ) or {
				c.error('incompatible initializer for field `$field.name`: $err', field.default_expr.position())
			}
			// Check for unnecessary inits like ` = 0` and ` = ''`
			if field.typ.is_ptr() {
				continue
			}
			if field.default_expr is ast.IntegerLiteral as lit {
				if lit.val == '0' {
					c.warn('unnecessary default value of `0`: struct fields are zeroed by default',
						lit.pos)
				}
			} else if field.default_expr is ast.StringLiteral as lit {
				if lit.val == '' {
					c.warn("unnecessary default value of '': struct fields are zeroed by default",
						lit.pos)
				}
			} else if field.default_expr is ast.BoolLiteral as lit {
				if lit.val == false {
					c.warn('unnecessary default value `false`: struct fields are zeroed by default',
						lit.pos)
				}
			}
		}
	}
}

pub fn (mut c Checker) struct_init(mut struct_init ast.StructInit) table.Type {
	// typ := c.table.find_type(struct_init.typ.typ.name) or {
	// c.error('unknown struct: $struct_init.typ.typ.name', struct_init.pos)
	// panic('')
	// }
	if struct_init.typ == table.void_type {
		// Short syntax `({foo: bar})`
		if c.expected_type == table.void_type {
			c.error('unexpected short struct syntax', struct_init.pos)
			return table.void_type
		}
		struct_init.typ = c.expected_type
	}
	if struct_init.typ == 0 {
		c.error('unknown type', struct_init.pos)
	}
	type_sym := c.table.get_type_symbol(struct_init.typ)
	if type_sym.kind == .sum_type && struct_init.fields.len == 1 {
		sexpr := struct_init.fields[0].expr.str()
		c.error('cast to sum type using `${type_sym.source_name}($sexpr)` not `$type_sym.source_name{$sexpr}`',
			struct_init.pos)
	}
	if type_sym.kind == .interface_ {
		c.error('cannot instantiate interface `$type_sym.source_name`', struct_init.pos)
	}
	if type_sym.kind == .alias {
		info := type_sym.info as table.Alias
		if info.parent_type.is_number() {
			c.error('cannot instantiate number type alias `$type_sym.source_name`', struct_init.pos)
			return table.void_type
		}
	}
	if !type_sym.is_public && type_sym.kind != .placeholder && type_sym.mod != c.mod &&
		type_sym.language != .c {
		c.error('type `$type_sym.source_name` is private', struct_init.pos)
	}
	match type_sym.kind {
		.placeholder {
			c.error('unknown struct: $type_sym.source_name', struct_init.pos)
		}
		// string & array are also structs but .kind of string/array
		.struct_, .string, .array, .alias {
			mut info := table.Struct{}
			if type_sym.kind == .alias {
				info_t := type_sym.info as table.Alias
				sym := c.table.get_type_symbol(info_t.parent_type)
				if sym.kind == .placeholder { // pending import symbol did not resolve
					c.error('unknown struct: $type_sym.source_name', struct_init.pos)
					return table.void_type
				}
				if sym.kind != .struct_ {
					c.error('alias type name: $sym.source_name is not struct type', struct_init.pos)
				}
				info = sym.info as table.Struct
			} else {
				info = type_sym.info as table.Struct
			}
			if struct_init.is_short {
				exp_len := info.fields.len
				got_len := struct_init.fields.len
				if exp_len != got_len {
					amount := if exp_len < got_len { 'many' } else { 'few' }
					c.error('too $amount fields in `$type_sym.source_name` literal (expecting $exp_len, got $got_len)',
						struct_init.pos)
				}
			}
			mut inited_fields := []string{}
			for i, field in struct_init.fields {
				mut info_field := table.Field{}
				mut field_name := ''
				if struct_init.is_short {
					if i >= info.fields.len {
						// It doesn't make sense to check for fields that don't exist.
						// We should just stop here.
						break
					}
					info_field = info.fields[i]
					field_name = info_field.name
					struct_init.fields[i].name = field_name
				} else {
					field_name = field.name
					mut exists := false
					for f in info.fields {
						if f.name == field_name {
							info_field = f
							exists = true
							break
						}
					}
					/*
					if c.pref.is_verbose {
						for f in info.fields {
							if f.name == field_name {
								if f.embed_alias_for.len != 0 {
									mut has_embed_init := false
									for embedding in struct_init.fields {
										if embedding.name == f.embed_alias_for {
											has_embed_init = true
										}
									}
									if !has_embed_init {
										n := {
											f |
											embed_alias_for: ''
										}
										println(field)
										// struct_init.fields << { f | embed_alias_for: '' }
									}
								}
								break
							}
						}
					}
					*/
					if !exists {
						c.error('unknown field `$field.name` in struct literal of type `$type_sym.source_name`',
							field.pos)
						continue
					}
					if field_name in inited_fields {
						c.error('duplicate field name in struct literal: `$field_name`',
							field.pos)
						continue
					}
				}
				inited_fields << field_name
				c.expected_type = info_field.typ
				expr_type := c.expr(field.expr)
				expr_type_sym := c.table.get_type_symbol(expr_type)
				if expr_type != table.void_type && expr_type_sym.kind != .placeholder {
					c.check_expected(expr_type, info_field.typ) or {
						c.error('cannot assign to field `$info_field.name`: $err', field.pos)
					}
				}
				if info_field.typ.is_ptr() && !expr_type.is_ptr() && !expr_type.is_pointer() &&
					!expr_type.is_number() {
					c.error('ref', field.pos)
				}
				struct_init.fields[i].typ = expr_type
				struct_init.fields[i].expected_type = info_field.typ
			}
			// Check uninitialized refs
			for field in info.fields {
				if field.has_default_expr || field.name in inited_fields || field.embed_alias_for !=
					'' {
					continue
				}
				if field.typ.is_ptr() && !c.pref.translated {
					c.warn('reference field `${type_sym.source_name}.$field.name` must be initialized',
						struct_init.pos)
				}
				// Check for `[required]` struct attr
				if field.attrs.contains('required') && !struct_init.is_short {
					mut found := false
					for init_field in struct_init.fields {
						if field.name == init_field.name {
							found = true
							break
						}
					}
					if !found {
						c.error('field `${type_sym.source_name}.$field.name` is required',
							struct_init.pos)
					}
				}
			}
		}
		else {}
	}
	return struct_init.typ
}

pub fn (mut c Checker) infix_expr(mut infix_expr ast.InfixExpr) table.Type {
	// println('checker: infix expr(op $infix_expr.op.str())')
	former_expected_type := c.expected_type
	defer {
		c.expected_type = former_expected_type
	}
	c.expected_type = table.void_type
	left_type := c.expr(infix_expr.left)
	// left_type = c.unwrap_genric(c.expr(infix_expr.left))
	infix_expr.left_type = left_type
	c.expected_type = left_type
	right_type := c.expr(infix_expr.right)
	// right_type = c.unwrap_genric(c.expr(infix_expr.right))
	infix_expr.right_type = right_type
	mut right := c.table.get_type_symbol(right_type)
	mut left := c.table.get_type_symbol(left_type)
	left_pos := infix_expr.left.position()
	right_pos := infix_expr.right.position()
	if (left_type.is_ptr() || left.is_pointer()) &&
		infix_expr.op in [.plus, .minus] && !c.inside_unsafe {
		c.warn('pointer arithmetic is only allowed in `unsafe` blocks', left_pos)
	}
	mut return_type := left_type
	// Single side check
	// Place these branches according to ops' usage frequency to accelerate.
	// TODO: First branch includes ops where single side check is not needed, or needed but hasn't been implemented.
	// TODO: Some of the checks are not single side. Should find a better way to organize them.
	match infix_expr.op {
		// .eq, .ne, .gt, .lt, .ge, .le, .and, .logical_or, .dot, .key_as, .right_shift {}
		.key_in, .not_in {
			match right.kind {
				.array {
					elem_type := right.array_info().elem_type
					// if left_default.kind != right_sym.kind {
					c.check_expected(left_type, elem_type) or {
						c.error('left operand to `$infix_expr.op` does not match the array element type: $err',
							infix_expr.pos)
					}
				}
				.map {
					elem_type := right.map_info().key_type
					c.check_expected(left_type, elem_type) or {
						c.error('left operand to `$infix_expr.op` does not match the map key type: $err',
							infix_expr.pos)
					}
				}
				.string {
					c.check_expected(left_type, right_type) or {
						c.error('left operand to `$infix_expr.op` does not match: $err',
							infix_expr.pos)
					}
				}
				else {
					c.error('`$infix_expr.op.str()` can only be used with an array/map/string',
						infix_expr.pos)
				}
			}
			return table.bool_type
		}
		.plus, .minus, .mul, .div, .mod, .xor, .amp, .pipe { // binary operators that expect matching types
			if right.info is table.Alias &&
				(right.info as table.Alias).language != .c && c.mod == c.table.type_to_str(right_type).split('.')[0] {
				right = c.table.get_type_symbol((right.info as table.Alias).parent_type)
			}
			if left.info is table.Alias &&
				(left.info as table.Alias).language != .c && c.mod == c.table.type_to_str(left_type).split('.')[0] {
				left = c.table.get_type_symbol((left.info as table.Alias).parent_type)
			}
			if left.kind in [.array, .array_fixed, .map, .struct_] {
				if left.has_method(infix_expr.op.str()) {
					if method := left.find_method(infix_expr.op.str()) {
						return_type = method.return_type
					} else {
						return_type = left_type
					}
				} else {
					left_name := c.table.type_to_str(left_type)
					right_name := c.table.type_to_str(right_type)
					if left_name == right_name {
						c.error('operation `$left_name` $infix_expr.op.str() `$right_name` does not exist, please define it',
							left_pos)
					} else {
						c.error('mismatched types `$left_name` and `$right_name`', left_pos)
					}
				}
			} else if right.kind in [.array, .array_fixed, .map, .struct_] {
				if right.has_method(infix_expr.op.str()) {
					if method := right.find_method(infix_expr.op.str()) {
						return_type = method.return_type
					} else {
						return_type = right_type
					}
				} else {
					left_name := c.table.type_to_str(left_type)
					right_name := c.table.type_to_str(right_type)
					if left_name == right_name {
						c.error('operation `$left_name` $infix_expr.op.str() `$right_name` does not exist, please define it',
							right_pos)
					} else {
						c.error('mismatched types `$left_name` and `$right_name`', right_pos)
					}
				}
			} else {
				promoted_type := c.promote(c.table.unalias_num_type(left_type), c.table.unalias_num_type(right_type))
				if promoted_type.idx() == table.void_type_idx {
					left_name := c.table.type_to_str(left_type)
					right_name := c.table.type_to_str(right_type)
					c.error('mismatched types `$left_name` and `$right_name`', infix_expr.pos)
				} else if promoted_type.has_flag(.optional) {
					s := c.table.type_to_str(promoted_type)
					c.error('`$infix_expr.op` cannot be used with `$s`', infix_expr.pos)
				} else if promoted_type.is_float() {
					if infix_expr.op in [.mod, .xor, .amp, .pipe] {
						side := if left_type == promoted_type { 'left' } else { 'right' }
						pos := if left_type == promoted_type { left_pos } else { right_pos }
						name := if left_type == promoted_type { left.name } else { right.name }
						if infix_expr.op == .mod {
							c.error('float modulo not allowed, use math.fmod() instead',
								pos)
						} else {
							c.error('$side type of `$infix_expr.op.str()` cannot be non-integer type $name',
								pos)
						}
					}
				}
				if infix_expr.op in [.div, .mod] {
					match infix_expr.right as infix_right {
						ast.FloatLiteral {
							if infix_right.val.f64() == 0.0 {
								oper := if infix_expr.op == .div { 'division' } else { 'modulo' }
								c.error('$oper by zero', infix_right.pos)
							}
						}
						ast.IntegerLiteral {
							if infix_right.val.int() == 0 {
								oper := if infix_expr.op == .div { 'division' } else { 'modulo' }
								c.error('$oper by zero', infix_right.pos)
							}
						}
						else {}
					}
				}
				return_type = promoted_type
			}
		}
		.gt, .lt, .ge, .le {
			if left.kind in [.array, .array_fixed] && right.kind in [.array, .array_fixed] {
				c.error('only `==` and `!=` are defined on arrays', infix_expr.pos)
			}
		}
		.left_shift {
			if left.kind == .array {
				// `array << elm`
				infix_expr.auto_locked, _ = c.fail_if_immutable(infix_expr.left)
				left_value_type := c.table.value_type(left_type)
				left_value_sym := c.table.get_type_symbol(left_value_type)
				if left_value_sym.kind == .interface_ {
					if right.kind != .array {
						// []Animal << Cat
						c.type_implements(right_type, left_value_type, right_pos)
					} else {
						// []Animal << Cat
						c.type_implements(c.table.value_type(right_type), left_value_type,
							right_pos)
					}
					return table.void_type
				}
				// the expressions have different types (array_x and x)
				if c.check_types(right_type, left_value_type) { // , right_type) {
					// []T << T
					return table.void_type
				}
				if right.kind == .array &&
					c.check_types(left_value_type, c.table.value_type(right_type)) {
					// []T << []T
					return table.void_type
				}
				c.error('cannot append `$right.source_name` to `$left.source_name`', right_pos)
				return table.void_type
			} else {
				return c.check_shift(left_type, right_type, left_pos, right_pos)
			}
		}
		.right_shift {
			return c.check_shift(left_type, right_type, left_pos, right_pos)
		}
		.key_is, .not_is {
			type_expr := infix_expr.right as ast.Type
			typ_sym := c.table.get_type_symbol(type_expr.typ)
			if typ_sym.kind == .placeholder {
				c.error('$infix_expr.op.str(): type `$typ_sym.source_name` does not exist',
					type_expr.pos)
			}
			if left.kind !in [.interface_, .sum_type, .union_sum_type] {
				c.error('`$infix_expr.op.str()` can only be used with interfaces and sum types',
					infix_expr.pos)
			} else if left.kind == .union_sum_type {
				info := left.info as table.UnionSumType
				if type_expr.typ !in info.variants {
					c.error('`$left.source_name` has no variant `$right.source_name`',
						infix_expr.pos)
				}
			}
			return table.bool_type
		}
		.arrow { // `chan <- elem`
			if left.kind == .chan {
				chan_info := left.chan_info()
				elem_type := chan_info.elem_type
				if !c.check_types(right_type, elem_type) {
					c.error('cannot push `$right.name` on `$left.name`', right_pos)
				}
				if chan_info.is_mut {
					// TODO: The error message of the following could be more specific...
					c.fail_if_immutable(infix_expr.right)
				}
				if elem_type.is_ptr() && !right_type.is_ptr() {
					c.error('cannot push non-reference `$right.source_name` on `$left.source_name`',
						right_pos)
				}
			} else {
				c.error('cannot push on non-channel `$left.name`', left_pos)
			}
			return table.void_type
		}
		else {
			if infix_expr.op in [.and, .logical_or] {
				if infix_expr.left_type != table.bool_type_idx {
					c.error('left operand for `$infix_expr.op` is not a boolean', infix_expr.left.position())
				}
				if infix_expr.right_type != table.bool_type_idx {
					c.error('right operand for `$infix_expr.op` is not a boolean', infix_expr.right.position())
				}
			}
			// use `()` to make the boolean expression clear error
			// for example: `(a && b) || c` instead of `a && b || c`
			if infix_expr.op in [.logical_or, .and] {
				if infix_expr.left is ast.InfixExpr {
					e := infix_expr.left as ast.InfixExpr
					if e.op in [.logical_or, .and] && e.op != infix_expr.op {
						c.error('use `()` to make the boolean expression clear', infix_expr.pos)
					}
				}
			}
		}
	}
	// TODO: Absorb this block into the above single side check block to accelerate.
	if left_type == table.bool_type && infix_expr.op !in [.eq, .ne, .logical_or, .and] {
		c.error('bool types only have the following operators defined: `==`, `!=`, `||`, and `&&`',
			infix_expr.pos)
	} else if left_type == table.string_type &&
		infix_expr.op !in [.plus, .eq, .ne, .lt, .gt, .le, .ge] {
		// TODO broken !in
		c.error('string types only have the following operators defined: `==`, `!=`, `<`, `>`, `<=`, `>=`, and `+`',
			infix_expr.pos)
	}
	// Dual sides check (compatibility check)
	if !c.symmetric_check(right_type, left_type) && !c.pref.translated {
		// for type-unresolved consts
		if left_type == table.void_type || right_type == table.void_type {
			return table.void_type
		}
		c.error('infix expr: cannot use `$right.name` (right expression) as `$left.name`',
			infix_expr.pos)
	}
	/*
	if (infix_expr.left is ast.InfixExpr &&
		(infix_expr.left as ast.InfixExpr).op == .inc) ||
		(infix_expr.right is ast.InfixExpr && (infix_expr.right as ast.InfixExpr).op == .inc) {
		c.warn('`++` and `--` are statements, not expressions', infix_expr.pos)
	}
	*/
	return if infix_expr.op.is_relational() {
		table.bool_type
	} else {
		return_type
	}
}

// returns name and position of variable that needs write lock
// also sets `is_changed` to true (TODO update the name to reflect this?)
fn (mut c Checker) fail_if_immutable(expr ast.Expr) (string, token.Position) {
	mut to_lock := '' // name of variable that needs lock
	mut pos := token.Position{} // and its position
	mut explicit_lock_needed := false
	match mut expr {
		ast.CastExpr {
			// TODO
			return '', pos
		}
		ast.Ident {
			if expr.obj is ast.Var {
				mut v := expr.obj as ast.Var
				if !v.is_mut && !c.pref.translated && !c.inside_unsafe {
					c.error('`$expr.name` is immutable, declare it with `mut` to make it mutable',
						expr.pos)
				}
				v.is_changed = true
				if v.typ.share() == .shared_t {
					if expr.name !in c.locked_names {
						to_lock = expr.name
						pos = expr.pos
					}
				}
			} else if expr.name in c.const_names {
				c.error('cannot modify constant `$expr.name`', expr.pos)
			}
		}
		ast.IndexExpr {
			to_lock, pos = c.fail_if_immutable(expr.left)
		}
		ast.ParExpr {
			to_lock, pos = c.fail_if_immutable(expr.expr)
		}
		ast.PrefixExpr {
			to_lock, pos = c.fail_if_immutable(expr.right)
		}
		ast.SelectorExpr {
			// retrieve table.Field
			if expr.expr_type == 0 {
				c.error('0 type in SelectorExpr', expr.pos)
				return '', pos
			}
			mut typ_sym := c.table.get_type_symbol(c.unwrap_generic(expr.expr_type))
			if typ_sym.kind == .alias {
				alias_info := typ_sym.info as table.Alias
				typ_sym = c.table.get_type_symbol(alias_info.parent_type)
			}
			match typ_sym.kind {
				.struct_ {
					struct_info := typ_sym.info as table.Struct
					field_info := struct_info.find_field(expr.field_name) or {
						type_str := c.table.type_to_str(expr.expr_type)
						c.error('unknown field `${type_str}.$expr.field_name`', expr.pos)
						return '', pos
					}
					if !field_info.is_mut && !c.pref.translated {
						type_str := c.table.type_to_str(expr.expr_type)
						c.error('field `$expr.field_name` of struct `$type_str` is immutable',
							expr.pos)
					}
					to_lock, pos = c.fail_if_immutable(expr.expr)
					if to_lock != '' {
						// No automatic lock for struct access
						explicit_lock_needed = true
					}
				}
				.array, .string {
					// This should only happen in `builtin`
					// TODO Remove `crypto.rand` when possible (see vlib/crypto/rand/rand.v,
					// if `c_array_to_bytes_tmp` doesn't exist, then it's safe to remove it)
					if c.file.mod.name !in ['builtin', 'crypto.rand'] {
						c.error('`$typ_sym.kind` can not be modified', expr.pos)
					}
				}
				else {
					c.error('unexpected symbol `$typ_sym.kind`', expr.pos)
				}
			}
		}
		ast.CallExpr {
			// TODO: should only work for builtin method
			if expr.name == 'slice' {
				to_lock, pos = c.fail_if_immutable(expr.left)
				if to_lock != '' {
					// No automatic lock for array slicing (yet(?))
					explicit_lock_needed = true
				}
			} else {
				c.error('cannot use function call as mut', expr.pos)
			}
		}
		ast.ArrayInit {
			return '', pos
		}
		else {
			c.error('unexpected expression `${typeof(expr)}`', expr.position())
		}
	}
	if explicit_lock_needed {
		c.error('`$to_lock` is `shared` and needs explicit lock for `${typeof(expr)}`',
			pos)
		to_lock = ''
	}
	return to_lock, pos
}

pub fn (mut c Checker) call_expr(mut call_expr ast.CallExpr) table.Type {
	c.stmts(call_expr.or_block.stmts)
	// First check everything that applies to both fns and methods
	// TODO merge logic from call_method and call_fn
	/*
	for i, call_arg in call_expr.args {
		if call_arg.is_mut {
			c.fail_if_immutable(call_arg.expr)
			if !arg.is_mut {
				tok := call_arg.share.str()
				c.error('`$call_expr.name` parameter `$arg.name` is not `$tok`, `$tok` is not needed`',
					call_arg.expr.position())
			} else if arg.typ.share() != call_arg.share {
				c.error('wrong shared type', call_arg.expr.position())
			}
		} else {
			if arg.is_mut && (!call_arg.is_mut || arg.typ.share() != call_arg.share) {
				tok := call_arg.share.str()
				c.error('`$call_expr.name` parameter `$arg.name` is `$tok`, you need to provide `$tok` e.g. `$tok arg${i+1}`',
					call_arg.expr.position())
			}
		}
	}
	*/
	// Now call `call_method` or `call_fn` for specific checks.
	typ := if call_expr.is_method { c.call_method(mut call_expr) } else { c.call_fn(mut call_expr) }
	// autofree: mark args that have to be freed (after saving them in tmp exprs)
	free_tmp_arg_vars := c.pref.autofree && c.pref.experimental && !c.is_builtin_mod &&
		call_expr.args.len > 0 && !call_expr.args[0].typ.has_flag(.optional)
	if free_tmp_arg_vars {
		for i, arg in call_expr.args {
			if arg.typ != table.string_type {
				continue
			}
			if arg.expr is ast.Ident ||
				arg.expr is ast.StringLiteral || arg.expr is ast.SelectorExpr {
				// Simple expressions like variables, string literals, selector expressions
				// (`x.field`) can't result in allocations and don't need to be assigned to
				// temporary vars.
				// Only expressions like `str + 'b'` need to be freed.
				continue
			}
			call_expr.args[i].is_tmp_autofree = true
		}
		if call_expr.receiver_type == table.string_type && !(call_expr.left is ast.Ident ||
			call_expr.left is ast.StringLiteral) {
			call_expr.free_receiver = true
		}
	}
	return typ
}

fn (mut c Checker) check_map_and_filter(is_map bool, elem_typ table.Type, call_expr ast.CallExpr) {
	elem_sym := c.table.get_type_symbol(elem_typ)
	match call_expr.args[0].expr as arg_expr {
		ast.AnonFn {
			if arg_expr.decl.params.len > 1 {
				c.error('function needs exactly 1 argument', call_expr.pos)
			} else if is_map &&
				(arg_expr.decl.return_type != elem_typ || arg_expr.decl.params[0].typ != elem_typ) {
				c.error('type mismatch, should use `fn(a $elem_sym.source_name) $elem_sym.source_name {...}`',
					call_expr.pos)
			} else if !is_map &&
				(arg_expr.decl.return_type != table.bool_type || arg_expr.decl.params[0].typ != elem_typ) {
				c.error('type mismatch, should use `fn(a $elem_sym.source_name) bool {...}`',
					call_expr.pos)
			}
		}
		ast.Ident {
			if arg_expr.kind == .function {
				func := c.table.find_fn(arg_expr.name) or {
					c.error('$arg_expr.name is not exist', arg_expr.pos)
					return
				}
				if func.params.len > 1 {
					c.error('function needs exactly 1 argument', call_expr.pos)
				} else if is_map && (func.return_type != elem_typ || func.params[0].typ != elem_typ) {
					c.error('type mismatch, should use `fn(a $elem_sym.source_name) $elem_sym.source_name {...}`',
						call_expr.pos)
				} else if !is_map &&
					(func.return_type != table.bool_type || func.params[0].typ != elem_typ) {
					c.error('type mismatch, should use `fn(a $elem_sym.source_name) bool {...}`',
						call_expr.pos)
				}
			}
		}
		else {}
	}
}

pub fn (mut c Checker) call_method(mut call_expr ast.CallExpr) table.Type {
	left_type := c.expr(call_expr.left)
	is_generic := left_type.has_flag(.generic)
	call_expr.left_type = left_type
	// Set default values for .return_type & .receiver_type too,
	// or there will be hard to diagnose 0 type panics in cgen.
	call_expr.return_type = left_type
	call_expr.receiver_type = left_type
	left_type_sym := c.table.get_type_symbol(c.unwrap_generic(left_type))
	method_name := call_expr.name
	mut unknown_method_msg := 'unknown method: `${left_type_sym.source_name}.$method_name`'
	if left_type.has_flag(.optional) {
		c.error('optional type cannot be called directly', call_expr.left.position())
		return table.void_type
	}
	if left_type_sym.kind == .sum_type && method_name == 'type_name' {
		return table.string_type
	}
	// TODO: remove this for actual methods, use only for compiler magic
	// FIXME: Argument count != 1 will break these
	if left_type_sym.kind == .array &&
		method_name in ['filter', 'clone', 'repeat', 'reverse', 'map', 'slice', 'sort'] {
		mut elem_typ := table.void_type
		is_filter_map := method_name in ['filter', 'map']
		is_sort := method_name == 'sort'
		if is_filter_map || is_sort {
			array_info := left_type_sym.info as table.Array
			mut scope := c.file.scope.innermost(call_expr.pos.pos)
			if is_filter_map {
				scope.update_var_type('it', array_info.elem_type)
			} else if is_sort {
				c.fail_if_immutable(call_expr.left)
				scope.update_var_type('a', array_info.elem_type)
				scope.update_var_type('b', array_info.elem_type)
				// Verify `.sort(a < b)`
				if call_expr.args.len > 0 {
					if call_expr.args[0].expr !is ast.InfixExpr {
						c.error('`.sort()` requires a `<` or `>` comparison as the first and only argument' +
							'\ne.g. `users.sort(a.id < b.id)`', call_expr.pos)
					}
				}
			}
			elem_typ = array_info.elem_type
		}
		// map/filter are supposed to have 1 arg only
		mut arg_type := left_type
		for arg in call_expr.args {
			arg_type = c.expr(arg.expr)
		}
		if method_name == 'map' {
			// check fn
			c.check_map_and_filter(true, elem_typ, call_expr)
			arg_sym := c.table.get_type_symbol(arg_type)
			// FIXME: match expr failed for now
			mut ret_type := 0
			match arg_sym.info as info {
				table.FnType { ret_type = info.func.return_type }
				else { ret_type = arg_type }
			}
			call_expr.return_type = c.table.find_or_register_array(ret_type, 1, c.mod)
		} else if method_name == 'filter' {
			// check fn
			c.check_map_and_filter(false, elem_typ, call_expr)
		} else if method_name == 'clone' {
			// need to return `array_xxx` instead of `array`
			// in ['clone', 'str'] {
			call_expr.receiver_type = left_type.to_ptr()
			// call_expr.return_type = call_expr.receiver_type
		} else if method_name == 'sort' {
			call_expr.return_type = table.void_type
		}
		return call_expr.return_type
	} else if left_type_sym.kind == .map && method_name == 'clone' {
		call_expr.return_type = left_type
		call_expr.receiver_type = left_type.to_ptr()
		return call_expr.return_type
	} else if left_type_sym.kind == .array && method_name in ['first', 'last', 'pop'] {
		info := left_type_sym.info as table.Array
		call_expr.return_type = info.elem_type
		if method_name == 'pop' {
			call_expr.receiver_type = left_type.to_ptr()
		} else {
			call_expr.receiver_type = left_type
		}
		return call_expr.return_type
	} else if left_type_sym.kind == .array && method_name in ['insert', 'prepend'] {
		array_info := left_type_sym.info as table.Array
		elem_sym := c.table.get_type_symbol(array_info.elem_type)
		arg_expr := if method_name == 'insert' { call_expr.args[1].expr } else { call_expr.args[0].expr }
		arg_sym := c.table.get_type_symbol(c.expr(arg_expr))
		if arg_sym.kind == .array {
			info := arg_sym.info as table.Array
			sym := c.table.get_type_symbol(info.elem_type)
			if sym.kind != elem_sym.kind &&
				((elem_sym.kind == .int && sym.kind != .any_int) ||
				(elem_sym.kind == .f64 && sym.kind != .any_float)) {
				c.error('type mismatch, should use `$elem_sym.source_name[]`', arg_expr.position())
			}
		} else {
			if arg_sym.kind != elem_sym.kind &&
				((elem_sym.kind == .int && arg_sym.kind != .any_int) ||
				(elem_sym.kind == .f64 && arg_sym.kind != .any_float)) {
				c.error('type mismatch, should use `$elem_sym.source_name`', arg_expr.position())
			}
		}
	}
	if method := c.table.type_find_method(left_type_sym, method_name) {
		if !method.is_pub && !c.is_builtin_mod && !c.pref.is_test && left_type_sym.mod != c.mod &&
			left_type_sym.mod != '' { // method.mod != c.mod {
			// If a private method is called outside of the module
			// its receiver type is defined in, show an error.
			// println('warn $method_name lef.mod=$left_type_sym.mod c.mod=$c.mod')
			c.error('method `${left_type_sym.source_name}.$method_name` is private', call_expr.pos)
		}
		if method.params[0].is_mut {
			c.fail_if_immutable(call_expr.left)
			// call_expr.is_mut = true
		}
		if method.return_type == table.void_type &&
			method.ctdefine.len > 0 && method.ctdefine !in c.pref.compile_defines {
			call_expr.should_be_skipped = true
		}
		nr_args := if method.params.len == 0 { 0 } else { method.params.len - 1 }
		min_required_args := method.params.len - if method.is_variadic && method.params.len >
			1 { 2 } else { 1 }
		if call_expr.args.len < min_required_args {
			c.error('too few arguments in call to `${left_type_sym.source_name}.$method_name` ($call_expr.args.len instead of $min_required_args)',
				call_expr.pos)
		} else if !method.is_variadic && call_expr.args.len > nr_args {
			c.error('too many arguments in call to `${left_type_sym.source_name}.$method_name` ($call_expr.args.len instead of $nr_args)',
				call_expr.pos)
			return method.return_type
		}
		// if method_name == 'clone' {
		// println('CLONE nr args=$method.args.len')
		// }
		// call_expr.args << method.args[0].typ
		// call_expr.exp_arg_types << method.args[0].typ
		for i, arg in call_expr.args {
			exp_arg_typ := if method.is_variadic && i >= method.params.len - 1 { method.params[method.params.len -
					1].typ } else { method.params[i + 1].typ }
			exp_arg_sym := c.table.get_type_symbol(exp_arg_typ)
			c.expected_type = exp_arg_typ
			got_arg_typ := c.expr(arg.expr)
			call_expr.args[i].typ = got_arg_typ
			if method.is_variadic && got_arg_typ.has_flag(.variadic) && call_expr.args.len - 1 > i {
				c.error('when forwarding a varg variable, it must be the final argument',
					call_expr.pos)
			}
			if exp_arg_sym.kind == .interface_ {
				c.type_implements(got_arg_typ, exp_arg_typ, arg.expr.position())
				continue
			}
			if !c.check_types(got_arg_typ, exp_arg_typ) {
				got_arg_sym := c.table.get_type_symbol(got_arg_typ)
				// str method, allow type with str method if fn arg is string
				// if exp_arg_sym.kind == .string && got_arg_sym.has_method('str') {
				// continue
				// }
				// same ancestor? let it be
				if exp_arg_sym.parent_idx == got_arg_sym.parent_idx {
					if got_arg_sym.parent_idx != 0 {
						continue
					}
				}
				if got_arg_typ != table.void_type {
					c.error('cannot use type `$got_arg_sym.source_name` as type `$exp_arg_sym.source_name` in argument ${i +
						1} to `${left_type_sym.source_name}.$method_name`', call_expr.pos)
				}
			}
			param := if method.is_variadic && i >= method.params.len - 1 { method.params[method.params.len -
					1] } else { method.params[i + 1] }
			if arg.is_mut {
				c.fail_if_immutable(arg.expr)
				if !param.is_mut {
					tok := arg.share.str()
					c.error('`$call_expr.name` parameter `$param.name` is not `$tok`, `$tok` is not needed`',
						arg.expr.position())
				} else if param.typ.share() != arg.share {
					c.error('wrong shared type', arg.expr.position())
				}
			} else {
				if param.is_mut && (!arg.is_mut || param.typ.share() != arg.share) {
					tok := arg.share.str()
					c.error('`$call_expr.name` parameter `$param.name` is `$tok`, you need to provide `$tok` e.g. `$tok arg${i +
						1}`', arg.expr.position())
				}
			}
		}
		if method.is_unsafe && !c.inside_unsafe {
			c.warn('method `${left_type_sym.source_name}.$method_name` must be called from an `unsafe` block',
				call_expr.pos)
		}
		// TODO: typ optimize.. this node can get processed more than once
		if call_expr.expected_arg_types.len == 0 {
			for i in 1 .. method.params.len {
				call_expr.expected_arg_types << method.params[i].typ
			}
		}
		if is_generic {
			// We need the receiver to be T in cgen.
			// TODO: cant we just set all these to the concrete type in checker? then no need in gen
			call_expr.receiver_type = left_type.derive(method.params[0].typ).set_flag(.generic)
		} else {
			call_expr.receiver_type = method.params[0].typ
		}
		call_expr.return_type = method.return_type
		return method.return_type
	} else {
		if left_type_sym.kind == .aggregate {
			// the error message contains the problematic type
			unknown_method_msg = err
		}
	}
	// TODO: str methods
	if method_name == 'str' {
		if left_type_sym.kind == .interface_ {
			iname := left_type_sym.source_name
			c.error('interface `$iname` does not have a .str() method. Use typeof() instead',
				call_expr.pos)
		}
		call_expr.receiver_type = left_type
		call_expr.return_type = table.string_type
		if call_expr.args.len > 0 {
			c.error('.str() method calls should have no arguments', call_expr.pos)
		}
		return table.string_type
	}
	// call struct field fn type
	// TODO: can we use SelectorExpr for all? this dosent really belong here
	if field := c.table.struct_find_field(left_type_sym, method_name) {
		field_type_sym := c.table.get_type_symbol(field.typ)
		if field_type_sym.kind == .function {
			// call_expr.is_method = false
			call_expr.is_field = true
			info := field_type_sym.info as table.FnType
			call_expr.return_type = info.func.return_type
			// TODO: check args (do it once for all of the above)
			for arg in call_expr.args {
				c.expr(arg.expr)
			}
			return info.func.return_type
		}
	}
	if left_type != table.void_type {
		suggestion := util.new_suggestion(method_name, left_type_sym.methods.map(it.name))
		c.error(suggestion.say(unknown_method_msg), call_expr.pos)
	}
	return table.void_type
}

pub fn (mut c Checker) call_fn(mut call_expr ast.CallExpr) table.Type {
	fn_name := call_expr.name
	if fn_name == 'main' {
		c.error('the `main` function cannot be called in the program', call_expr.pos)
	}
	if fn_name == 'typeof' {
		// TODO: impl typeof properly (probably not going to be a fn call)
		return table.string_type
	}
	if call_expr.generic_type.has_flag(.generic) {
		if c.mod != '' {
			// Need to prepend the module when adding a generic type to a function
			// `fn_gen_types['mymod.myfn'] == ['string', 'int']`
			c.table.register_fn_gen_type(c.mod + '.' + fn_name, c.cur_generic_type)
		} else {
			c.table.register_fn_gen_type(fn_name, c.cur_generic_type)
		}
		// call_expr.generic_type = c.unwrap_generic(call_expr.generic_type)
	}
	// if c.fileis('json_test.v') {
	// println(fn_name)
	// }
	if fn_name == 'json.encode' {
	} else if fn_name == 'json.decode' {
		expr := call_expr.args[0].expr
		if expr !is ast.Type {
			typ := typeof(expr)
			c.error('json.decode: first argument needs to be a type, got `$typ`', call_expr.pos)
			return table.void_type
		}
		c.expected_type = table.string_type
		call_expr.args[1].typ = c.expr(call_expr.args[1].expr)
		if call_expr.args[1].typ != table.string_type {
			c.error('json.decode: second argument needs to be a string', call_expr.pos)
		}
		typ := expr as ast.Type
		ret_type := typ.typ.set_flag(.optional)
		call_expr.return_type = ret_type
		return ret_type
	}
	// look for function in format `mod.fn` or `fn` (builtin)
	mut f := table.Fn{}
	mut found := false
	mut found_in_args := false
	// anon fn direct call
	if call_expr.left is ast.AnonFn {
		// it was set to anon for checker errors, clear for gen
		call_expr.name = ''
		c.expr(call_expr.left)
		anon_fn := call_expr.left as ast.AnonFn
		anon_fn_sym := c.table.get_type_symbol(anon_fn.typ)
		f = (anon_fn_sym.info as table.FnType).func
		found = true
	}
	// try prefix with current module as it would have never gotten prefixed
	if !found && !fn_name.contains('.') && call_expr.mod != 'builtin' {
		name_prefixed := '${call_expr.mod}.$fn_name'
		if f1 := c.table.find_fn(name_prefixed) {
			call_expr.name = name_prefixed
			found = true
			f = f1
		}
	}
	// already prefixed (mod.fn) or C/builtin/main
	if !found {
		if f1 := c.table.find_fn(fn_name) {
			found = true
			f = f1
		}
	}
	if c.pref.is_script && !found {
		os_name := 'os.$fn_name'
		if f1 := c.table.find_fn(os_name) {
			call_expr.name = os_name
			found = true
			f = f1
		}
	}
	// check for arg (var) of fn type
	if !found {
		scope := c.file.scope.innermost(call_expr.pos.pos)
		if v := scope.find_var(fn_name) {
			if v.typ != 0 {
				vts := c.table.get_type_symbol(v.typ)
				if vts.kind == .function {
					info := vts.info as table.FnType
					f = info.func
					found = true
					found_in_args = true
				}
			}
		}
	}
	if !found {
		c.error('unknown function: $fn_name', call_expr.pos)
		return table.void_type
	}
	if !found_in_args {
		scope := c.file.scope.innermost(call_expr.pos.pos)
		if _ := scope.find_var(fn_name) {
			c.error('ambiguous call to: `$fn_name`, may refer to fn `$fn_name` or variable `$fn_name`',
				call_expr.pos)
		}
	}
	if !f.is_pub && f.language == .v && f.name.len > 0 && f.mod.len > 0 && f.mod != c.mod {
		c.error('function `$f.name` is private. curmod=$c.mod fmod=$f.mod', call_expr.pos)
	}
	if f.is_deprecated {
		c.warn('function `$f.name` has been deprecated', call_expr.pos)
	}
	if f.is_unsafe && !c.inside_unsafe && f.language == .c && f.name[2] in [`m`, `s`] &&
		f.mod == 'builtin' {
		// builtin C.m*, C.s* only - temp
		c.warn('function `$f.name` must be called from an `unsafe` block', call_expr.pos)
	}
	if f.is_generic && f.return_type.has_flag(.generic) {
		rts := c.table.get_type_symbol(f.return_type)
		if rts.kind == .struct_ {
			rts_info := rts.info as table.Struct
			if rts_info.generic_types.len > 0 {
				// TODO: multiple generic types
				// for gt in rts_info.generic_types {
				// gtss := c.table.get_type_symbol(gt)
				// }
				gts := c.table.get_type_symbol(call_expr.generic_type)
				nrt := '$rts.name<$gts.name>'
				idx := c.table.type_idxs[nrt]
				if idx == 0 {
					c.error('unknown type: $nrt', call_expr.pos)
				}
				call_expr.return_type = table.new_type(idx).derive(f.return_type)
			}
		}
	} else {
		call_expr.return_type = f.return_type
	}
	if f.return_type == table.void_type &&
		f.ctdefine.len > 0 && f.ctdefine !in c.pref.compile_defines {
		call_expr.should_be_skipped = true
	}
	if f.language != .v || call_expr.language != .v {
		for arg in call_expr.args {
			c.expr(arg.expr)
		}
		return f.return_type
	}
	min_required_args := if f.is_variadic { f.params.len - 1 } else { f.params.len }
	if call_expr.args.len < min_required_args {
		c.error('too few arguments in call to `$fn_name` ($call_expr.args.len instead of $min_required_args)',
			call_expr.pos)
	} else if !f.is_variadic && call_expr.args.len > f.params.len {
		c.error('too many arguments in call to `$fn_name` ($call_expr.args.len instead of $f.params.len)',
			call_expr.pos)
		return f.return_type
	}
	// println can print anything
	if fn_name in ['println', 'print'] && call_expr.args.len > 0 {
		c.expected_type = table.string_type
		call_expr.args[0].typ = c.expr(call_expr.args[0].expr)
		// check optional argument
		if call_expr.args[0].typ.has_flag(.optional) {
			c.error('cannot print optional type', call_expr.args[0].expr.position())
		}
		/*
		// TODO: optimize `struct T{} fn (t &T) str() string {return 'abc'} mut a := []&T{} a << &T{} println(a[0])`
		// It currently generates:
		// `println(T_str_no_ptr(*(*(T**)array_get(a, 0))));`
		// ... which works, but could be just:
		// `println(T_str(*(T**)array_get(a, 0)));`
		prexpr := call_expr.args[0].expr
		prtyp := call_expr.args[0].typ
		prtyp_sym := c.table.get_type_symbol(prtyp)
		prtyp_is_ptr := prtyp.is_ptr()
		prhas_str, prexpects_ptr, prnr_args := prtyp_sym.str_method_info()
		eprintln('>>> println hack typ: ${prtyp} | sym.source_name: ${prtyp_sym.source_name} | is_ptr: $prtyp_is_ptr | has_str: $prhas_str | expects_ptr: $prexpects_ptr | nr_args: $prnr_args | expr: ${prexpr.str()} ')
		*/
		return f.return_type
	}
	// TODO: typ optimize.. this node can get processed more than once
	if call_expr.expected_arg_types.len == 0 {
		for param in f.params {
			call_expr.expected_arg_types << param.typ
		}
	}
	for i, call_arg in call_expr.args {
		arg := if f.is_variadic && i >= f.params.len - 1 { f.params[f.params.len - 1] } else { f.params[i] }
		c.expected_type = arg.typ
		typ := c.expr(call_arg.expr)
		call_expr.args[i].typ = typ
		typ_sym := c.table.get_type_symbol(typ)
		arg_typ_sym := c.table.get_type_symbol(arg.typ)
		if f.is_variadic && typ.has_flag(.variadic) && call_expr.args.len - 1 > i {
			c.error('when forwarding a varg variable, it must be the final argument',
				call_expr.pos)
		}
		if call_arg.is_mut {
			c.fail_if_immutable(call_arg.expr)
			if !arg.is_mut {
				tok := call_arg.share.str()
				c.error('`$call_expr.name` parameter `$arg.name` is not `$tok`, `$tok` is not needed`',
					call_arg.expr.position())
			} else if arg.typ.share() != call_arg.share {
				c.error('wrong shared type', call_arg.expr.position())
			}
		} else {
			if arg.is_mut && (!call_arg.is_mut || arg.typ.share() != call_arg.share) {
				tok := call_arg.share.str()
				c.error('`$call_expr.name` parameter `$arg.name` is `$tok`, you need to provide `$tok` e.g. `$tok arg${i +
					1}`', call_arg.expr.position())
			}
		}
		// Handle expected interface
		if arg_typ_sym.kind == .interface_ {
			c.type_implements(typ, arg.typ, call_arg.expr.position())
			continue
		}
		// Handle expected interface array
		/*
		if exp_type_sym.kind == .array && t.get_type_symbol(t.value_type(exp_idx)).kind == .interface_ {
			return true
		}
		*/
		c.check_expected(typ, arg.typ) or {
			// str method, allow type with str method if fn arg is string
			// Passing an int or a string array produces a c error here
			// Deleting this condition results in propper V error messages
			// if arg_typ_sym.kind == .string && typ_sym.has_method('str') {
			// continue
			// }
			if typ_sym.kind == .void && arg_typ_sym.kind == .string {
				continue
			}
			if f.is_generic {
				continue
			}
			c.error('invalid argument ${i + 1} to `$fn_name`: $err', call_arg.pos)
		}
	}
	if f.is_generic && call_expr.generic_type == table.void_type {
		// no type arguments given in call, attempt implicit instantiation
		c.infer_fn_types(f, mut call_expr)
	}
	if call_expr.generic_type != table.void_type && f.return_type != 0 { // table.t_type {
		// Handle `foo<T>() T` => `foo<int>() int` => return int
		return_sym := c.table.get_type_symbol(f.return_type)
		if return_sym.source_name == 'T' {
			mut typ := call_expr.generic_type
			typ = typ.set_nr_muls(f.return_type.nr_muls())
			if f.return_type.has_flag(.optional) {
				typ = typ.set_flag(.optional)
			}
			call_expr.return_type = typ
			return typ
		} else if return_sym.kind == .array {
			elem_info := return_sym.info as table.Array
			elem_sym := c.table.get_type_symbol(elem_info.elem_type)
			if elem_sym.source_name == 'T' {
				idx := c.table.find_or_register_array(call_expr.generic_type, 1, return_sym.mod)
				return table.new_type(idx)
			}
		}
	}
	if call_expr.generic_type.is_full() && !f.is_generic {
		c.error('a non generic function called like a generic one', call_expr.generic_list_pos)
	}
	if f.is_generic {
		return call_expr.return_type
	}
	return f.return_type
}

fn (mut c Checker) type_implements(typ table.Type, inter_typ table.Type, pos token.Position) bool {
	typ_sym := c.table.get_type_symbol(typ)
	inter_sym := c.table.get_type_symbol(inter_typ)
	styp := c.table.type_to_str(typ)
	for imethod in inter_sym.methods {
		if method := typ_sym.find_method(imethod.name) {
			if !imethod.is_same_method_as(method) {
				sig := c.table.fn_signature(imethod, {
					skip_receiver: true
				})
				c.error('`$styp` incorrectly implements method `$imethod.name` of interface `$inter_sym.source_name`, expected `$sig`',
					pos)
				return false
			}
			continue
		}
		c.error("`$styp` doesn't implement method `$imethod.name`", pos)
	}
	mut inter_info := inter_sym.info as table.Interface
	if typ !in inter_info.types && typ_sym.kind != .interface_ {
		inter_info.types << typ
	}
	return true
}

// return the actual type of the expression, once the optional is handled
pub fn (mut c Checker) check_expr_opt_call(expr ast.Expr, ret_type table.Type) table.Type {
	if expr is ast.CallExpr {
		if expr.return_type.has_flag(.optional) {
			if expr.or_block.kind == .absent {
				if ret_type != table.void_type {
					c.error('${expr.name}() returns an option but is missing an `or {}` block',
						expr.pos)
				}
			} else {
				c.check_or_expr(expr.or_block, ret_type, expr.return_type.clear_flag(.optional))
			}
			// remove optional flag
			// return ret_type.clear_flag(.optional)
			// TODO: currently unwrapped in assign, would need to refactor assign to unwrap here
			return ret_type
		} else if expr.or_block.kind == .block {
			c.error('unexpected `or` block, the function `$expr.name` does not return an optional',
				expr.or_block.pos)
		} else if expr.or_block.kind == .propagate {
			c.error('unexpected `?`, the function `$expr.name` does not return an optional',
				expr.or_block.pos)
		}
	}
	return ret_type
}

pub fn (mut c Checker) check_or_expr(or_expr ast.OrExpr, ret_type table.Type, expr_return_type table.Type) {
	if or_expr.kind == .propagate {
		if !c.cur_fn.return_type.has_flag(.optional) && c.cur_fn.name != 'main.main' {
			c.error('to propagate the optional call, `$c.cur_fn.name` must return an optional',
				or_expr.pos)
		}
		return
	}
	stmts_len := or_expr.stmts.len
	if stmts_len == 0 {
		if ret_type != table.void_type {
			// x := f() or {}
			c.error('assignment requires a non empty `or {}` block', or_expr.pos)
			return
		}
		// allow `f() or {}`
		return
	}
	last_stmt := or_expr.stmts[stmts_len - 1]
	if ret_type != table.void_type {
		match last_stmt {
			ast.ExprStmt {
				last_stmt_typ := c.expr(last_stmt.expr)
				type_fits := c.check_types(last_stmt_typ, ret_type)
				is_panic_or_exit := is_expr_panic_or_exit(last_stmt.expr)
				if type_fits || is_panic_or_exit {
					return
				}
				expected_type_name := c.table.type_to_str(ret_type.clear_flag(.optional))
				if last_stmt.typ == table.void_type {
					c.error('`or` block must provide a default value of type `$expected_type_name`, or return/exit/continue/break/panic',
						last_stmt.pos)
				} else {
					type_name := c.table.type_to_str(last_stmt_typ)
					c.error('wrong return type `$type_name` in the `or {}` block, expected `$expected_type_name`',
						last_stmt.pos)
				}
				return
			}
			ast.BranchStmt {
				if last_stmt.kind !in [.key_continue, .key_break] {
					c.error('only break/continue is allowed as a branch statement in the end of an `or {}` block',
						last_stmt.pos)
					return
				}
			}
			ast.Return {}
			else {
				expected_type_name := c.table.type_to_str(ret_type.clear_flag(.optional))
				c.error('last statement in the `or {}` block should be an expression of type `$expected_type_name` or exit parent scope',
					or_expr.pos)
				return
			}
		}
	} else {
		match last_stmt {
			ast.ExprStmt {
				if last_stmt.typ == table.void_type {
					return
				}
				if is_expr_panic_or_exit(last_stmt.expr) {
					return
				}
				if c.check_types(last_stmt.typ, expr_return_type) {
					return
				}
				// opt_returning_string() or { ... 123 }
				type_name := c.table.type_to_str(last_stmt.typ)
				expr_return_type_name := c.table.type_to_str(expr_return_type)
				c.error('the default expression type in the `or` block should be `$expr_return_type_name`, instead you gave a value of type `$type_name`',
					last_stmt.expr.position())
			}
			else {}
		}
	}
}

fn is_expr_panic_or_exit(expr ast.Expr) bool {
	match expr {
		ast.CallExpr { return expr.name in ['panic', 'exit'] }
		else { return false }
	}
}

pub fn (mut c Checker) selector_expr(mut selector_expr ast.SelectorExpr) table.Type {
	prevent_sum_type_unwrapping_once := c.prevent_sum_type_unwrapping_once
	c.prevent_sum_type_unwrapping_once = false
	// T.name, typeof(expr).name
	mut name_type := 0
	match selector_expr.expr as left {
		ast.Ident {
			if left.name == 'T' {
				name_type = table.Type(c.table.find_type_idx('T')).set_flag(.generic)
			}
		}
		// Note: in future typeof() should be a type known at compile-time
		// sum types should not be handled dynamically
		ast.TypeOf {
			name_type = c.expr(left.expr)
		}
		else {}
	}
	if name_type > 0 {
		if selector_expr.field_name != 'name' {
			c.error('invalid field `.$selector_expr.field_name` for type `$selector_expr.expr`',
				selector_expr.pos)
		}
		selector_expr.name_type = name_type
		return table.string_type
	}
	typ := c.expr(selector_expr.expr)
	if typ == table.void_type_idx {
		c.error('unknown selector expression', selector_expr.pos)
		return table.void_type
	}
	selector_expr.expr_type = typ
	field_name := selector_expr.field_name
	utyp := c.unwrap_generic(typ)
	sym := c.table.get_type_symbol(utyp)
	if typ.has_flag(.variadic) || sym.kind == .array_fixed || sym.kind == .chan {
		if field_name == 'len' || (sym.kind == .chan && field_name == 'cap') {
			selector_expr.typ = table.int_type
			return table.int_type
		}
	}
	mut unknown_field_msg := 'type `$sym.source_name` has no field or method `$field_name`'
	if field := c.table.struct_find_field(sym, field_name) {
		if sym.mod != c.mod && !field.is_pub && sym.language != .c {
			c.error('field `${sym.source_name}.$field_name` is not public', selector_expr.pos)
		}
		field_sym := c.table.get_type_symbol(field.typ)
		if field_sym.kind == .union_sum_type {
			if !prevent_sum_type_unwrapping_once {
				scope := c.file.scope.innermost(selector_expr.pos.pos)
				if scope_field := scope.find_struct_field(utyp, field_name) {
					return scope_field.sum_type_cast
				}
			}
		}
		selector_expr.typ = field.typ
		return field.typ
	} else {
		if sym.kind == .aggregate {
			unknown_field_msg = err
		}
	}
	if sym.kind !in [.struct_, .aggregate] {
		if sym.kind != .placeholder {
			c.error('`$sym.source_name` is not a struct', selector_expr.pos)
		}
	} else {
		if sym.kind == .struct_ {
			sss := sym.info as table.Struct
			suggestion := util.new_suggestion(field_name, sss.fields.map(it.name))
			c.error(suggestion.say(unknown_field_msg), selector_expr.pos)
		}
		c.error(unknown_field_msg, selector_expr.pos)
	}
	return table.void_type
}

// TODO: non deferred
pub fn (mut c Checker) return_stmt(mut return_stmt ast.Return) {
	c.expected_type = c.cur_fn.return_type
	if return_stmt.exprs.len > 0 && c.expected_type == table.void_type {
		c.error('too many arguments to return, current function does not return anything',
			return_stmt.pos)
		return
	} else if return_stmt.exprs.len == 0 && !(c.expected_type == table.void_type ||
		c.table.get_type_symbol(c.expected_type).kind == .void) {
		c.error('too few arguments to return', return_stmt.pos)
		return
	}
	if return_stmt.exprs.len == 0 {
		return
	}
	expected_type := c.unwrap_generic(c.expected_type)
	expected_type_sym := c.table.get_type_symbol(expected_type)
	exp_is_optional := expected_type.has_flag(.optional)
	mut expected_types := [expected_type]
	if expected_type_sym.kind == .multi_return {
		mr_info := expected_type_sym.info as table.MultiReturn
		expected_types = mr_info.types
	}
	mut got_types := []table.Type{}
	for expr in return_stmt.exprs {
		typ := c.expr(expr)
		// Unpack multi return types
		sym := c.table.get_type_symbol(typ)
		if sym.kind == .multi_return {
			for t in sym.mr_info().types {
				got_types << t
			}
		} else {
			got_types << typ
		}
	}
	return_stmt.types = got_types
	// allow `none` & `error (Option)` return types for function that returns optional
	if exp_is_optional && got_types[0].idx() in [table.none_type_idx, c.table.type_idxs['Option']] {
		return
	}
	if expected_types.len > 0 && expected_types.len != got_types.len {
		c.error('wrong number of return arguments', return_stmt.pos)
		return
	}
	for i, exp_type in expected_types {
		got_typ := c.unwrap_generic(got_types[i])
		if got_typ.has_flag(.optional) &&
			(!exp_type.has_flag(.optional) || c.table.type_to_str(got_typ) != c.table.type_to_str(exp_type)) {
			pos := return_stmt.exprs[i].position()
			c.error('cannot use `${c.table.type_to_str(got_typ)}` as type `${c.table.type_to_str(exp_type)}` in return argument',
				pos)
		}
		if !c.check_types(got_typ, exp_type) {
			got_typ_sym := c.table.get_type_symbol(got_typ)
			mut exp_typ_sym := c.table.get_type_symbol(exp_type)
			pos := return_stmt.exprs[i].position()
			if exp_typ_sym.kind == .interface_ {
				c.type_implements(got_typ, exp_type, return_stmt.pos)
				continue
			}
			c.error('cannot use `$got_typ_sym.source_name` as type `$exp_typ_sym.source_name` in return argument',
				pos)
		}
		if got_typ.is_ptr() && !exp_type.is_ptr() {
			pos := return_stmt.exprs[i].position()
			c.error('fn `$c.cur_fn.name` expects you to return a non reference type `${c.table.type_to_str(exp_type)}`, but you are returning `${c.table.type_to_str(got_typ)}` instead',
				pos)
		}
	}
}

pub fn (mut c Checker) enum_decl(decl ast.EnumDecl) {
	c.check_valid_pascal_case(decl.name, 'enum name', decl.pos)
	mut seen := []int{}
	for i, field in decl.fields {
		if !c.pref.experimental && util.contains_capital(field.name) {
			// TODO C2V uses hundreds of enums with capitals, remove -experimental check once it's handled
			c.error('field name `$field.name` cannot contain uppercase letters, use snake_case instead',
				field.pos)
		}
		for j in 0 .. i {
			if field.name == decl.fields[j].name {
				c.error('field name `$field.name` duplicate', field.pos)
			}
		}
		if field.has_expr {
			match field.expr as field_expr {
				ast.IntegerLiteral {
					val := field_expr.val.i64()
					if val < int_min || val > int_max {
						c.error('enum value `$val` overflows int', field_expr.pos)
					} else if !decl.is_multi_allowed && int(val) in seen {
						c.error('enum value `$val` already exists', field_expr.pos)
					}
					seen << int(val)
				}
				ast.PrefixExpr {}
				else {
					if field.expr is ast.Ident {
						expr := field.expr as ast.Ident
						if expr.language == .c {
							continue
						}
					}
					mut pos := field.expr.position()
					if pos.pos == 0 {
						pos = field.pos
					}
					c.error('default value for enum has to be an integer', pos)
				}
			}
		} else {
			if seen.len > 0 {
				last := seen[seen.len - 1]
				if last == int_max {
					c.error('enum value overflows', field.pos)
				}
				seen << last + 1
			} else {
				seen << 0
			}
		}
	}
}

pub fn (mut c Checker) assign_stmt(mut assign_stmt ast.AssignStmt) {
	c.expected_type = table.none_type // TODO a hack to make `x := if ... work`
	defer {
		c.expected_type = table.void_type
	}
	right_first := assign_stmt.right[0]
	mut right_len := assign_stmt.right.len
	mut right_type0 := table.void_type
	if right_first is ast.CallExpr || right_first is ast.IfExpr || right_first is ast.MatchExpr {
		right_type0 = c.expr(right_first)
		assign_stmt.right_types = [
			c.check_expr_opt_call(right_first, right_type0),
		]
		right_type_sym0 := c.table.get_type_symbol(right_type0)
		if right_type_sym0.kind == .multi_return {
			assign_stmt.right_types = right_type_sym0.mr_info().types
			right_len = assign_stmt.right_types.len
		} else if right_type0 == table.void_type {
			right_len = 0
		}
	}
	if assign_stmt.left.len != right_len {
		if right_first is ast.CallExpr {
			c.error('assignment mismatch: $assign_stmt.left.len variable(s) but `${right_first.name}()` returns $right_len value(s)',
				assign_stmt.pos)
		} else {
			c.error('assignment mismatch: $assign_stmt.left.len variable(s) $right_len value(s)',
				assign_stmt.pos)
		}
		return
	}
	// Check `x := &y` and `mut x := <-ch`
	if right_first is ast.PrefixExpr {
		node := right_first
		left_first := assign_stmt.left[0]
		if left_first is ast.Ident {
			assigned_var := left_first
			if node.right is ast.Ident {
				ident := node.right as ast.Ident
				scope := c.file.scope.innermost(node.pos.pos)
				if v := scope.find_var(ident.name) {
					right_type0 = v.typ
					if node.op == .amp {
						if !v.is_mut && assigned_var.is_mut && !c.inside_unsafe {
							c.error('`$ident.name` is immutable, cannot have a mutable reference to it',
								node.pos)
						}
					}
				}
			}
			if node.op == .arrow {
				if assigned_var.is_mut {
					right_sym := c.table.get_type_symbol(right_type0)
					if right_sym.kind == .chan {
						chan_info := right_sym.chan_info()
						if chan_info.elem_type.is_ptr() && !chan_info.is_mut {
							c.error('cannot have a mutable reference to object from `$right_sym.source_name`',
								node.pos)
						}
					}
				}
			}
		}
	}
	//
	is_decl := assign_stmt.op == .decl_assign
	for i, left in assign_stmt.left {
		if left is ast.CallExpr {
			c.error('cannot call function `${left.name}()` on the left side of an assignment',
				left.pos)
		}
		is_blank_ident := left.is_blank_ident()
		mut left_type := table.void_type
		if !is_decl && !is_blank_ident {
			if left is ast.Ident || left is ast.SelectorExpr {
				c.prevent_sum_type_unwrapping_once = true
			}
			left_type = c.expr(left)
			c.expected_type = c.unwrap_generic(left_type)
		}
		if assign_stmt.right_types.len < assign_stmt.left.len { // first type or multi return types added above
			right_type := c.expr(assign_stmt.right[i])
			if assign_stmt.right_types.len == i {
				assign_stmt.right_types << c.check_expr_opt_call(assign_stmt.right[i], right_type)
			}
		}
		right := if i < assign_stmt.right.len { assign_stmt.right[i] } else { assign_stmt.right[0] }
		mut right_type := assign_stmt.right_types[i]
		if is_decl {
			left_type = c.table.mktyp(right_type)
			if left_type == table.int_type {
				mut expr := right
				mut negative := false
				if right is ast.PrefixExpr {
					expr = right.right
					if right.op == .minus {
						negative = true
					}
				}
				if expr is ast.IntegerLiteral {
					mut is_large := false
					if expr.val.len > 8 {
						val := expr.val.i64()
						if (!negative && val > int_max) || (negative && -val < int_min) {
							is_large = true
						}
					}
					if is_large {
						c.error('overflow in implicit type `int`, use explicit type casting instead',
							expr.pos)
					}
				}
			}
			// we are unwrapping here instead if check_expr_opt_call currently
			if left_type.has_flag(.optional) {
				left_type = left_type.clear_flag(.optional)
			}
		} else {
			// Make sure the variable is mutable
			c.fail_if_immutable(left)
			// left_type = c.expr(left)
		}
		assign_stmt.left_types << left_type
		match mut left {
			ast.Ident {
				if left.kind == .blank_ident {
					left_type = right_type
					assign_stmt.left_types[i] = right_type
					if assign_stmt.op !in [.assign, .decl_assign] {
						c.error('cannot modify blank `_` identifier', left.pos)
					}
				} else if left.info !is ast.IdentVar {
					c.error('cannot assign to $left.kind `$left.name`', left.pos)
				} else {
					if is_decl {
						c.check_valid_snake_case(left.name, 'variable name', left.pos)
					}
					mut ident_var_info := left.info as ast.IdentVar
					if ident_var_info.share == .shared_t {
						left_type = left_type.set_flag(.shared_f)
					}
					if ident_var_info.share == .atomic_t {
						left_type = left_type.set_flag(.atomic_f)
					}
					assign_stmt.left_types[i] = left_type
					ident_var_info.typ = left_type
					left.info = ident_var_info
					if left_type != 0 {
						match mut left.obj as v {
							ast.Var { v.typ = left_type }
							ast.GlobalField { v.typ = left_type }
							else {}
						}
						/*
						if left.obj is ast.Var as v {
							v.typ = left_type
						} else if left.obj is ast.GlobalDecl as v {
							v.typ = left_type
						}
						*/
					}
				}
			}
			ast.PrefixExpr {
				// Do now allow `*x = y` outside `unsafe`
				if left.op == .mul && !c.inside_unsafe {
					c.error('modifying variables via dereferencing can only be done in `unsafe` blocks',
						assign_stmt.pos)
				}
				if is_decl {
					c.error('non-name on the left side of `:=`', left.pos)
				}
			}
			else {
				if is_decl {
					c.error('non-name `$left` on left side of `:=`', left.position())
				}
			}
		}
		left_type_unwrapped := c.unwrap_generic(left_type)
		right_type_unwrapped := c.unwrap_generic(right_type)
		if right_type_unwrapped == 0 {
			// right type was a generic `T`
			continue
		}
		left_sym := c.table.get_type_symbol(left_type_unwrapped)
		right_sym := c.table.get_type_symbol(right_type_unwrapped)
		left_is_ptr := left_type.is_ptr() || left_sym.is_pointer()
		right_is_ptr := right_type.is_ptr() || right_sym.is_pointer()
		if left_is_ptr && assign_stmt.op !in [.assign, .decl_assign] && !c.inside_unsafe {
			// ptr op=
			c.warn('pointer arithmetic is only allowed in `unsafe` blocks', assign_stmt.pos)
		}
		if c.pref.translated {
			// TODO fix this in C2V instead, for example cast enums to int before using `|` on them.
			// TODO replace all c.pref.translated checks with `$if !translated` for performance
			continue
		}
		if left_is_ptr && (right is ast.StructInit || !right_is_ptr) && !right_sym.is_number() {
			left_name := c.table.type_to_str(left_type_unwrapped)
			mut rtype := right_type_unwrapped
			if rtype.is_ptr() {
				rtype = rtype.deref()
			}
			right_name := c.table.type_to_str(rtype)
			c.error('mismatched types `$left_name` and `$right_name`', assign_stmt.pos)
		}
		// Single side check
		match assign_stmt.op {
			.assign {} // No need to do single side check for =. But here put it first for speed.
			.plus_assign {
				if !left_sym.is_number() && left_type != table.string_type && !left_sym.is_pointer() {
					c.error('operator += not defined on left operand type `$left_sym.source_name`',
						left.position())
				} else if !right_sym.is_number() && right_type != table.string_type && !right_sym.is_pointer() {
					c.error('operator += not defined on right operand type `$right_sym.source_name`',
						right.position())
				}
				if right is ast.IntegerLiteral && right.str().int() == 1 {
					c.error('use `++` instead of `+= 1`', assign_stmt.pos)
				}
			}
			.minus_assign {
				if !left_sym.is_number() && !left_sym.is_pointer() {
					c.error('operator -= not defined on left operand type `$left_sym.source_name`',
						left.position())
				} else if !right_sym.is_number() && !right_sym.is_pointer() {
					c.error('operator -= not defined on right operand type `$right_sym.source_name`',
						right.position())
				}
				if right is ast.IntegerLiteral && right.str().int() == 1 {
					c.error('use `--` instead of `-= 1`', assign_stmt.pos)
				}
			}
			.mult_assign, .div_assign {
				if !left_sym.is_number() &&
					!c.table.get_final_type_symbol(left_type_unwrapped).is_int() {
					c.error('operator $assign_stmt.op.str() not defined on left operand type `$left_sym.source_name`',
						left.position())
				} else if !right_sym.is_number() &&
					!c.table.get_final_type_symbol(left_type_unwrapped).is_int() {
					c.error('operator $assign_stmt.op.str() not defined on right operand type `$right_sym.source_name`',
						right.position())
				}
			}
			.and_assign, .or_assign, .xor_assign, .mod_assign, .left_shift_assign, .right_shift_assign {
				if !left_sym.is_int() &&
					!c.table.get_final_type_symbol(left_type_unwrapped).is_int() {
					c.error('operator $assign_stmt.op.str() not defined on left operand type `$left_sym.source_name`',
						left.position())
				} else if !right_sym.is_int() &&
					!c.table.get_final_type_symbol(right_type_unwrapped).is_int() {
					c.error('operator $assign_stmt.op.str() not defined on right operand type `$right_sym.source_name`',
						right.position())
				}
			}
			else {}
		}
		if !is_blank_ident && right_sym.kind != .placeholder {
			// Dual sides check (compatibility check)
			c.check_expected(right_type_unwrapped, left_type_unwrapped) or {
				c.error('cannot assign to `$left`: $err', right.position())
			}
		}
	}
}

fn (mut c Checker) open_scope(mut parent ast.Scope, start_pos int) &ast.Scope {
	mut s := ast.new_scope(parent, start_pos)
	s.end_pos = parent.end_pos
	parent.children << s
	return s
}

fn (mut c Checker) check_array_init_para_type(para string, expr ast.Expr, pos token.Position) {
	sym := c.table.get_type_symbol(c.expr(expr))
	if sym.kind !in [.int, .any_int] {
		c.error('array $para needs to be an int', pos)
	}
}

pub fn (mut c Checker) array_init(mut array_init ast.ArrayInit) table.Type {
	// println('checker: array init $array_init.pos.line_nr $c.file.path')
	mut elem_type := table.void_type
	// []string - was set in parser
	if array_init.typ != table.void_type {
		if array_init.exprs.len == 0 {
			if array_init.has_cap {
				c.check_array_init_para_type('cap', array_init.cap_expr, array_init.pos)
			}
			if array_init.has_len {
				c.check_array_init_para_type('len', array_init.len_expr, array_init.pos)
			}
		}
		sym := c.table.get_type_symbol(array_init.elem_type)
		if array_init.has_default {
			c.expr(array_init.default_expr)
		}
		if sym.kind == .placeholder {
			c.error('unknown type `$sym.source_name`', array_init.elem_type_pos)
		}
		return array_init.typ
	}
	// a = []
	if array_init.exprs.len == 0 {
		type_sym := c.table.get_type_symbol(c.expected_type)
		if type_sym.kind != .array {
			c.error('array_init: no type specified (maybe: `[]Type{}` instead of `[]`)',
				array_init.pos)
			return table.void_type
		}
		// TODO: seperate errors once bug is fixed with `x := if expr { ... } else { ... }`
		// if c.expected_type == table.void_type {
		// c.error('array_init: use `[]Type{}` instead of `[]`', array_init.pos)
		// return table.void_type
		// }
		array_info := type_sym.array_info()
		array_init.elem_type = array_info.elem_type
		return c.expected_type
	}
	// [1,2,3]
	if array_init.exprs.len > 0 && array_init.elem_type == table.void_type {
		mut expected_value_type := table.void_type
		mut expecting_interface_array := false
		cap := array_init.exprs.len
		mut interface_types := []table.Type{cap: cap}
		if c.expected_type != 0 {
			expected_value_type = c.table.value_type(c.expected_type)
			if c.table.get_type_symbol(expected_value_type).kind == .interface_ {
				// Array of interfaces? (`[dog, cat]`) Save the interface type (`Animal`)
				expecting_interface_array = true
				array_init.interface_type = expected_value_type
				array_init.is_interface = true
			}
		}
		// expecting_interface_array := c.expected_type != 0 &&
		// c.table.get_type_symbol(c.table.value_type(c.expected_type)).kind ==			.interface_
		//
		// if expecting_interface_array {
		// println('ex $c.expected_type')
		// }
		for i, expr in array_init.exprs {
			typ := c.expr(expr)
			if expecting_interface_array {
				if i == 0 {
					elem_type = expected_value_type
					c.expected_type = elem_type
				}
				interface_types << typ
				continue
			}
			// The first element's type
			if i == 0 {
				elem_type = c.table.mktyp(typ)
				c.expected_type = elem_type
				continue
			}
			if !c.check_types(typ, elem_type) {
				elem_type_sym := c.table.get_type_symbol(elem_type)
				c.error('expected array element with type `$elem_type_sym.source_name`',
					array_init.pos)
			}
		}
		if expecting_interface_array {
			array_init.interface_types = interface_types
		}
		if array_init.is_fixed {
			idx := c.table.find_or_register_array_fixed(elem_type, array_init.exprs.len,
				1)
			array_init.typ = table.new_type(idx)
		} else {
			sym := c.table.get_type_symbol(elem_type)
			idx := c.table.find_or_register_array(elem_type, 1, sym.mod)
			array_init.typ = table.new_type(idx)
		}
		array_init.elem_type = elem_type
	} else if array_init.is_fixed && array_init.exprs.len == 1 && array_init.elem_type != table.void_type {
		// [50]byte
		mut fixed_size := 1
		match array_init.exprs[0] as init_expr {
			ast.IntegerLiteral {
				fixed_size = init_expr.val.int()
			}
			ast.Ident {
				// if obj := c.file.global_scope.find_const(init_expr.name) {
				// if  obj := scope.find(init_expr.name) {
				// scope := c.file.scope.innermost(array_init.pos.pos)
				// eprintln('scope: ${scope.str()}')
				// scope.find(init_expr.name) or {
				// c.error('undefined ident: `$init_expr.name`', array_init.pos)
				// }
				mut full_const_name := init_expr.mod + '.' + init_expr.name
				if obj := c.file.global_scope.find_const(full_const_name) {
					if cint := const_int_value(obj) {
						fixed_size = cint
					}
				} else {
					c.error('non existent integer const $full_const_name while initializing the size of a static array',
						array_init.pos)
				}
			}
			else {
				c.error('expecting `int` for fixed size', array_init.pos)
			}
		}
		idx := c.table.find_or_register_array_fixed(array_init.elem_type, fixed_size,
			1)
		array_type := table.new_type(idx)
		array_init.typ = array_type
	}
	return array_init.typ
}

fn const_int_value(cfield ast.ConstField) ?int {
	if cint := is_const_integer(cfield) {
		return cint.val.int()
	}
	return none
}

fn is_const_integer(cfield ast.ConstField) ?ast.IntegerLiteral {
	match cfield.expr {
		ast.IntegerLiteral { return *it }
		else {}
	}
	return none
}

fn (mut c Checker) stmt(node ast.Stmt) {
	$if trace_checker ? {
		stmt_pos := node.position()
		eprintln('checking file: ${c.file.path:-30} | stmt pos: ${stmt_pos.str():-45} | stmt')
	}
	// c.expected_type = table.void_type
	match mut node {
		ast.AssertStmt {
			cur_exp_typ := c.expected_type
			assert_type := c.expr(node.expr)
			if assert_type != table.bool_type_idx {
				atype_name := c.table.get_type_symbol(assert_type).name
				c.error('assert can be used only with `bool` expressions, but found `$atype_name` instead',
					node.pos)
			}
			c.expected_type = cur_exp_typ
		}
		ast.AssignStmt {
			c.assign_stmt(mut node)
		}
		ast.Block {
			if node.is_unsafe {
				assert !c.inside_unsafe
				c.inside_unsafe = true
				c.stmts(node.stmts)
				c.inside_unsafe = false
			} else {
				c.stmts(node.stmts)
			}
		}
		ast.BranchStmt {
			if c.in_for_count == 0 {
				c.error('$node.kind.str() statement not within a loop', node.pos)
			}
		}
		ast.CompFor {
			// node.typ = c.expr(node.expr)
			c.stmts(node.stmts)
		}
		ast.ConstDecl {
			mut field_names := []string{}
			mut field_order := []int{}
			for i, field in node.fields {
				// TODO Check const name once the syntax is decided
				if field.name in c.const_names {
					c.error('duplicate const `$field.name`', field.pos)
				}
				c.const_names << field.name
				field_names << field.name
				field_order << i
			}
			mut needs_order := false
			mut done_fields := []int{}
			for i, field in node.fields {
				c.const_decl = field.name
				c.const_deps << field.name
				typ := c.expr(field.expr)
				node.fields[i].typ = c.table.mktyp(typ)
				for cd in c.const_deps {
					for j, f in node.fields {
						if j != i && cd in field_names && cd == f.name && j !in done_fields {
							needs_order = true
							x := field_order[j]
							field_order[j] = field_order[i]
							field_order[i] = x
							break
						}
					}
				}
				done_fields << i
				c.const_deps = []
			}
			if needs_order {
				mut ordered_fields := []ast.ConstField{}
				for order in field_order {
					ordered_fields << node.fields[order]
				}
				node.fields = ordered_fields
			}
		}
		ast.DeferStmt {
			c.stmts(node.stmts)
		}
		ast.EnumDecl {
			c.enum_decl(node)
		}
		ast.ExprStmt {
			node.typ = c.expr(node.expr)
			c.expected_type = table.void_type
			c.check_expr_opt_call(node.expr, table.void_type)
			// TODO This should work, even if it's prolly useless .-.
			// node.typ = c.check_expr_opt_call(node.expr, table.void_type)
		}
		ast.FnDecl {
			c.fn_decl(mut node)
		}
		ast.ForCStmt {
			c.in_for_count++
			c.stmt(node.init)
			c.expr(node.cond)
			c.stmt(node.inc)
			c.stmts(node.stmts)
			c.in_for_count--
		}
		ast.ForInStmt {
			c.in_for_count++
			typ := c.expr(node.cond)
			typ_idx := typ.idx()
			if node.key_var.len > 0 && node.key_var != '_' {
				c.check_valid_snake_case(node.key_var, 'variable name', node.pos)
			}
			if node.val_var.len > 0 && node.val_var != '_' {
				c.check_valid_snake_case(node.val_var, 'variable name', node.pos)
			}
			if node.is_range {
				high_type := c.expr(node.high)
				high_type_idx := high_type.idx()
				if typ_idx in table.integer_type_idxs && high_type_idx !in table.integer_type_idxs {
					c.error('range types do not match', node.cond.position())
				} else if typ_idx in table.float_type_idxs || high_type_idx in table.float_type_idxs {
					c.error('range type can not be float', node.cond.position())
				} else if typ_idx == table.bool_type_idx || high_type_idx == table.bool_type_idx {
					c.error('range type can not be bool', node.cond.position())
				} else if typ_idx == table.string_type_idx || high_type_idx == table.string_type_idx {
					c.error('range type can not be string', node.cond.position())
				}
			} else {
				mut scope := c.file.scope.innermost(node.pos.pos)
				sym := c.table.get_type_symbol(typ)
				if sym.kind == .map && !(node.key_var.len > 0 && node.val_var.len > 0) {
					c.error('declare a key and a value variable when ranging a map: `for key, val in map {`\n' +
						'use `_` if you do not need the variable', node.pos)
				}
				if node.key_var.len > 0 {
					key_type := match sym.kind {
						.map { sym.map_info().key_type }
						else { table.int_type }
					}
					node.key_type = key_type
					scope.update_var_type(node.key_var, key_type)
				}
				mut value_type := c.table.value_type(typ)
				if value_type == table.void_type || typ.has_flag(.optional) {
					if typ != table.void_type {
						c.error('for in: cannot index `${c.table.type_to_str(typ)}`',
							node.cond.position())
					}
				}
				if node.val_is_mut {
					value_type = value_type.to_ptr()
				}
				node.cond_type = typ
				node.kind = sym.kind
				node.val_type = value_type
				scope.update_var_type(node.val_var, value_type)
			}
			c.stmts(node.stmts)
			c.in_for_count--
		}
		ast.ForStmt {
			c.in_for_count++
			c.expected_type = table.bool_type
			typ := c.expr(node.cond)
			if !node.is_inf && typ.idx() != table.bool_type_idx && !c.pref.translated {
				c.error('non-bool used as for condition', node.pos)
			}
			// TODO: update loop var type
			// how does this work currenly?
			c.stmts(node.stmts)
			c.in_for_count--
		}
		ast.GlobalDecl {
			for field in node.fields {
				c.check_valid_snake_case(field.name, 'global name', field.pos)
				if field.name in c.global_names {
					c.error('duplicate global `$field.name`', field.pos)
				}
				c.global_names << field.name
			}
		}
		ast.GoStmt {
			if node.call_expr !is ast.CallExpr {
				c.error('expression in `go` must be a function call', node.call_expr.position())
			}
			c.expr(node.call_expr)
			if node.call_expr is ast.CallExpr {
				call_expr := node.call_expr as ast.CallExpr
				// Make sure there are no mutable arguments
				for arg in call_expr.args {
					if arg.is_mut && !arg.typ.is_ptr() {
						c.error('function in `go` statement cannot contain mutable non-reference arguments',
							arg.expr.position())
					}
				}
				if call_expr.is_method && call_expr.receiver_type.is_ptr() && !call_expr.left_type.is_ptr() {
					c.error('method in `go` statement cannot have non-reference mutable receiver',
						call_expr.left.position())
				}
			}
		}
		ast.GotoLabel {}
		ast.GotoStmt {}
		ast.HashStmt {
			c.hash_stmt(mut node)
		}
		ast.Import {
			c.import_stmt(node)
		}
		ast.InterfaceDecl {
			c.interface_decl(node)
		}
		ast.Module {
			c.mod = node.name
			c.is_builtin_mod = node.name in ['builtin', 'os', 'strconv']
			c.check_valid_snake_case(node.name, 'module name', node.pos)
		}
		ast.Return {
			// c.returns = true
			c.return_stmt(mut node)
			c.scope_returns = true
		}
		ast.SqlStmt {
			c.sql_stmt(mut node)
		}
		ast.StructDecl {
			c.struct_decl(node)
		}
		ast.TypeDecl {
			c.type_decl(node)
		}
	}
}

fn (mut c Checker) hash_stmt(mut node ast.HashStmt) {
	if c.skip_flags {
		return
	}
	if c.pref.backend == .js {
		if !c.file.path.ends_with('.js.v') {
			c.error('Hash statements are only allowed in backend specific files such "x.js.v"',
				node.pos)
		}
		if c.mod == 'main' {
			c.error('Hash statements are not allowed in the main module. Please place them in a separate module.',
				node.pos)
		}
		return
	}
	if node.kind == 'include' {
		mut flag := node.main
		if flag.contains('@VROOT') {
			vroot := util.resolve_vroot(flag, c.file.path) or {
				c.error(err, node.pos)
				return
			}
			node.val = 'include $vroot'
			node.main = vroot
		}
		flag_no_comment := flag.all_before('//').trim_space()
		if !((flag_no_comment.starts_with('"') && flag_no_comment.ends_with('"')) ||
			(flag_no_comment.starts_with('<') && flag_no_comment.ends_with('>'))) {
			c.error('including C files should use either `"header_file.h"` or `<header_file.h>` quoting',
				node.pos)
		}
	} else if node.kind == 'pkgconfig' {
		args := if node.main.contains('--') { node.main.split(' ') } else { '--cflags --libs $node.main'.split(' ') }
		mut m := pkgconfig.main(args) or {
			c.error(err, node.pos)
			return
		}
		cflags := m.run() or {
			c.error(err, node.pos)
			return
		}
		c.table.parse_cflag(cflags, c.mod, c.pref.compile_defines_all) or {
			c.error(err, node.pos)
			return
		}
	} else if node.kind == 'flag' {
		// #flag linux -lm
		mut flag := node.main
		// expand `@VROOT` to its absolute path
		if flag.contains('@VROOT') {
			flag = util.resolve_vroot(flag, c.file.path) or {
				c.error(err, node.pos)
				return
			}
		}
		for deprecated in ['@VMOD', '@VMODULE', '@VPATH', '@VLIB_PATH'] {
			if flag.contains(deprecated) {
				c.error('$deprecated had been deprecated, use @VROOT instead.', node.pos)
			}
		}
		// println('adding flag "$flag"')
		c.table.parse_cflag(flag, c.mod, c.pref.compile_defines_all) or {
			c.error(err, node.pos)
		}
	} else {
		if node.kind != 'define' {
			c.warn('expected `#define`, `#flag`, `#include` or `#pkgconfig` not $node.val',
				node.pos)
		}
	}
}

fn (mut c Checker) import_stmt(imp ast.Import) {
	for sym in imp.syms {
		name := '$imp.mod\.$sym.name'
		if sym.kind == .fn_ {
			c.table.find_fn(name) or {
				c.error('module `$imp.mod` has no public fn named `$sym.name\()`', sym.pos)
			}
		}
		if sym.kind == .type_ {
			if type_sym := c.table.find_type(name) {
				if type_sym.kind == .placeholder || !type_sym.is_public {
					c.error('module `$imp.mod` has no public type `$sym.name\{}`', sym.pos)
				}
			} else {
				c.error('module `$imp.mod` has no public type `$sym.name\{}`', sym.pos)
			}
		}
	}
}

fn (mut c Checker) stmts(stmts []ast.Stmt) {
	mut unreachable := token.Position{
		line_nr: -1
	}
	c.expected_type = table.void_type
	for stmt in stmts {
		if c.scope_returns {
			if unreachable.line_nr == -1 {
				unreachable = stmt.position()
			}
		}
		c.stmt(stmt)
	}
	if unreachable.line_nr >= 0 {
		c.error('unreachable code', unreachable)
	}
	c.scope_returns = false
	c.expected_type = table.void_type
}

[inline]
pub fn (c &Checker) unwrap_generic(typ table.Type) table.Type {
	if typ.has_flag(.generic) {
		// return c.cur_generic_type
		return c.cur_generic_type.derive(typ).clear_flag(.generic)
	}
	return typ
}

// TODO node must be mut
pub fn (mut c Checker) expr(node ast.Expr) table.Type {
	c.expr_level++
	defer {
		c.expr_level--
	}
	if c.expr_level > 200 {
		c.error('checker: too many expr levels: $c.expr_level ', node.position())
		return table.void_type
	}
	match mut node {
		ast.CTempVar {
			return node.typ
		}
		ast.AnonFn {
			keep_fn := c.cur_fn
			c.cur_fn = &node.decl
			c.stmts(node.decl.stmts)
			c.cur_fn = keep_fn
			return node.typ
		}
		ast.ArrayInit {
			return c.array_init(mut node)
		}
		ast.AsCast {
			node.expr_type = c.expr(node.expr)
			expr_type_sym := c.table.get_type_symbol(node.expr_type)
			type_sym := c.table.get_type_symbol(node.typ)
			if expr_type_sym.kind == .sum_type || expr_type_sym.kind == .union_sum_type {
				if type_sym.kind == .placeholder {
					// Unknown type used in the right part of `as`
					c.error('unknown type `$type_sym.source_name`', node.pos)
				}
				if !c.table.sumtype_has_variant(node.expr_type, node.typ) {
					c.error('cannot cast `$expr_type_sym.source_name` to `$type_sym.source_name`',
						node.pos)
					// c.error('only $info.variants can be casted to `$typ`', node.pos)
				}
			} else {
				mut s := 'cannot cast non-sum type `$expr_type_sym.source_name` using `as`'
				if type_sym.kind == .sum_type || expr_type_sym.kind == .union_sum_type {
					s += ' - use e.g. `${type_sym.source_name}(some_expr)` instead.'
				}
				c.error(s, node.pos)
			}
			if expr_type_sym.kind == .union_sum_type {
				return node.typ
			}
			return node.typ.to_ptr()
		}
		ast.Assoc {
			scope := c.file.scope.innermost(node.pos.pos)
			v := scope.find_var(node.var_name) or {
				panic(err)
			}
			for i, _ in node.fields {
				c.expr(node.exprs[i])
			}
			node.typ = v.typ
			return v.typ
		}
		ast.BoolLiteral {
			return table.bool_type
		}
		ast.CastExpr {
			return c.cast_expr(mut node)
		}
		ast.CallExpr {
			return c.call_expr(mut node)
		}
		ast.ChanInit {
			return c.chan_init(mut node)
		}
		ast.CharLiteral {
			// return any_int, not rune, so that we can do "bytes << `A`" without a cast etc
			// return table.any_int_type
			return table.rune_type
			// return table.byte_type
		}
		ast.Comment {
			return table.void_type
		}
		ast.AtExpr {
			return c.at_expr(mut node)
		}
		ast.ComptimeCall {
			node.sym = c.table.get_type_symbol(c.unwrap_generic(c.expr(node.left)))
			if node.is_vweb {
				// TODO assoc parser bug
				pref := *c.pref
				pref2 := {
					pref |
					is_vweb: true
				}
				mut c2 := new_checker(c.table, pref2)
				c2.check(node.vweb_tmpl)
				c.warnings << c2.warnings
				c.errors << c2.errors
				c.nr_warnings += c2.nr_warnings
				c.nr_errors += c2.nr_errors
			}
			return c.table.find_type_idx('vweb.Result')
			// return table.void_type
		}
		ast.ConcatExpr {
			return c.concat_expr(mut node)
		}
		ast.EnumVal {
			return c.enum_val(mut node)
		}
		ast.FloatLiteral {
			return table.any_flt_type
		}
		ast.Ident {
			// c.checked_ident = node.name
			res := c.ident(mut node)
			// c.checked_ident = ''
			return res
		}
		ast.IfExpr {
			return c.if_expr(mut node)
		}
		ast.IfGuardExpr {
			node.expr_type = c.expr(node.expr)
			if !node.expr_type.has_flag(.optional) {
				c.error('expression should return an option', node.expr.position())
			}
			return table.bool_type
		}
		ast.IndexExpr {
			return c.index_expr(mut node)
		}
		ast.InfixExpr {
			return c.infix_expr(mut node)
		}
		ast.IntegerLiteral {
			return table.any_int_type
		}
		ast.LockExpr {
			return c.lock_expr(mut node)
		}
		ast.MapInit {
			return c.map_init(mut node)
		}
		ast.MatchExpr {
			return c.match_expr(mut node)
		}
		ast.PostfixExpr {
			return c.postfix_expr(mut node)
		}
		ast.PrefixExpr {
			right_type := c.expr(node.right)
			node.right_type = right_type
			// TODO: testing ref/deref strategy
			if node.op == .amp && !right_type.is_ptr() {
				if node.right is ast.IntegerLiteral {
					c.error('cannot take the address of an int', node.pos)
				}
				if node.right is ast.StringLiteral || node.right is ast.StringInterLiteral {
					c.error('cannot take the address of a string', node.pos)
				}
				if node.right is ast.IndexExpr as index {
					typ_sym := c.table.get_type_symbol(index.left_type)
					mut is_mut := false
					if index.left is ast.Ident as ident {
						if ident.obj is ast.Var {
							v := ident.obj as ast.Var
							is_mut = v.is_mut
						}
					}
					if !c.inside_unsafe && is_mut {
						if typ_sym.kind == .map {
							c.error('cannot take the address of mutable map values outside unsafe blocks',
								index.pos)
						}
						if typ_sym.kind == .array {
							c.error('cannot take the address of mutable array elements outside unsafe blocks',
								index.pos)
						}
					}
				}
				return right_type.to_ptr()
			} else if node.op == .amp && node.right !is ast.CastExpr {
				return right_type.to_ptr()
			}
			if node.op == .mul {
				if right_type.is_ptr() {
					return right_type.deref()
				}
				if !right_type.is_pointer() {
					s := c.table.type_to_str(right_type)
					c.error('invalid indirect of `$s`', node.pos)
				}
			}
			if node.op == .bit_not && !right_type.is_int() && !c.pref.translated {
				c.error('operator ~ only defined on int types', node.pos)
			}
			if node.op == .not && right_type != table.bool_type_idx && !c.pref.translated {
				c.error('! operator can only be used with bool types', node.pos)
			}
			if node.op == .arrow {
				right := c.table.get_type_symbol(right_type)
				if right.kind == .chan {
					c.stmts(node.or_block.stmts)
					return right.chan_info().elem_type
				} else {
					c.error('<- operator can only be used with `chan` types', node.pos)
				}
			}
			return right_type
		}
		ast.None {
			return table.none_type
		}
		ast.OrExpr {
			// never happens
			return table.void_type
		}
		// ast.OrExpr2 {
		// return node.typ
		// }
		ast.ParExpr {
			return c.expr(node.expr)
		}
		ast.RangeExpr {
			// never happens
			return table.void_type
		}
		ast.SelectExpr {
			return c.select_expr(mut node)
		}
		ast.SelectorExpr {
			return c.selector_expr(mut node)
		}
		ast.SizeOf {
			return table.u32_type
		}
		ast.SqlExpr {
			return c.sql_expr(mut node)
		}
		ast.StringLiteral {
			if node.language == .c {
				return table.byteptr_type
			}
			return table.string_type
		}
		ast.StringInterLiteral {
			return c.string_inter_lit(mut node)
		}
		ast.StructInit {
			return c.struct_init(mut node)
		}
		ast.Type {
			return node.typ
		}
		ast.TypeOf {
			node.expr_type = c.expr(node.expr)
			return table.string_type
		}
		ast.UnsafeExpr {
			return c.unsafe_expr(mut node)
		}
		ast.Likely {
			ltype := c.expr(node.expr)
			if !c.check_types(ltype, table.bool_type) {
				ltype_sym := c.table.get_type_symbol(ltype)
				lname := if node.is_likely { '_likely_' } else { '_unlikely_' }
				c.error('`${lname}()` expects a boolean expression, instead it got `$ltype_sym.source_name`',
					node.pos)
			}
			return table.bool_type
		}
	}
	return table.void_type
}

pub fn (mut c Checker) cast_expr(mut node ast.CastExpr) table.Type {
	node.expr_type = c.expr(node.expr)
	from_type_sym := c.table.get_type_symbol(node.expr_type)
	to_type_sym := c.table.get_type_symbol(node.typ)
	expr_is_ptr := node.expr_type.is_ptr() || node.expr_type.idx() in table.pointer_type_idxs
	if expr_is_ptr && to_type_sym.kind == .string && !node.in_prexpr {
		if node.has_arg {
			c.warn('to convert a C string buffer pointer to a V string, please use x.vstring_with_len(len) instead of string(x,len)',
				node.pos)
		} else {
			c.warn('to convert a C string buffer pointer to a V string, please use x.vstring() instead of string(x)',
				node.pos)
		}
	}
	if node.expr_type == table.byte_type && to_type_sym.kind == .string {
		c.error('can not cast type `byte` to string, use `${node.expr.str()}.str()` instead.',
			node.pos)
	}
	if to_type_sym.kind == .sum_type || to_type_sym.kind == .union_sum_type {
		if node.expr_type in [table.any_int_type, table.any_flt_type] {
			node.expr_type = c.promote_num(node.expr_type, if node.expr_type == table.any_int_type { table.int_type } else { table.f64_type })
		}
		if !c.table.sumtype_has_variant(node.typ, node.expr_type) {
			c.error('cannot cast `$from_type_sym.source_name` to `$to_type_sym.source_name`',
				node.pos)
		}
	} else if to_type_sym.info is table.Alias as alias_info {
		if !c.check_types(node.expr_type, alias_info.parent_type) {
			parent_type_sym := c.table.get_type_symbol(alias_info.parent_type)
			c.error('cannot convert type `$from_type_sym.source_name` to `$to_type_sym.source_name` (alias to `$parent_type_sym.source_name`)',
				node.pos)
		}
	} else if node.typ == table.string_type &&
		(from_type_sym.kind in [.any_int, .int, .byte, .byteptr] ||
		(from_type_sym.kind == .array && from_type_sym.name == 'array_byte')) {
		type_name := c.table.type_to_str(node.expr_type)
		c.error('cannot cast type `$type_name` to string, use `x.str()` instead', node.pos)
	} else if node.expr_type == table.string_type {
		if to_type_sym.kind != .alias {
			mut error_msg := 'cannot cast a string'
			if node.expr is ast.StringLiteral {
				str_lit := node.expr as ast.StringLiteral
				if str_lit.val.len == 1 {
					error_msg += ", for denoting characters use `$str_lit.val` instead of '$str_lit.val'"
				}
			}
			c.error(error_msg, node.pos)
		}
	} else if to_type_sym.kind == .byte &&
		node.expr_type != table.voidptr_type && from_type_sym.kind != .enum_ && !node.expr_type.is_int() &&
		!node.expr_type.is_float() && !node.expr_type.is_ptr() {
		type_name := c.table.type_to_str(node.expr_type)
		c.error('cannot cast type `$type_name` to `byte`', node.pos)
	} else if to_type_sym.kind == .struct_ && !node.typ.is_ptr() && !(to_type_sym.info as table.Struct).is_typedef {
		// For now we ignore C typedef because of `C.Window(C.None)` in vlib/clipboard
		if from_type_sym.kind == .struct_ && !node.expr_type.is_ptr() {
			from_type_info := from_type_sym.info as table.Struct
			to_type_info := to_type_sym.info as table.Struct
			if !c.check_struct_signature(from_type_info, to_type_info) {
				c.error('cannot convert struct `$from_type_sym.source_name` to struct `$to_type_sym.source_name`',
					node.pos)
			}
		} else {
			type_name := c.table.type_to_str(node.expr_type)
			c.error('cannot cast `$type_name` to struct', node.pos)
		}
	} else if node.typ == table.bool_type {
		c.error('cannot cast to bool - use e.g. `some_int != 0` instead', node.pos)
	} else if node.expr_type == table.none_type {
		type_name := c.table.type_to_str(node.typ)
		c.error('cannot cast `none` to `$type_name`', node.pos)
	}
	if node.has_arg {
		c.expr(node.arg)
	}
	node.typname = c.table.get_type_symbol(node.typ).name
	return node.typ
}

fn (mut c Checker) at_expr(mut node ast.AtExpr) table.Type {
	match node.kind {
		.fn_name {
			node.val = c.cur_fn.name.all_after_last('.')
		}
		.mod_name {
			node.val = c.cur_fn.mod
		}
		.struct_name {
			if c.cur_fn.is_method {
				node.val = c.table.type_to_str(c.cur_fn.receiver.typ).all_after_last('.')
			} else {
				node.val = ''
			}
		}
		.vexe_path {
			node.val = pref.vexe_path()
		}
		.file_path {
			node.val = os.real_path(c.file.path)
		}
		.line_nr {
			node.val = (node.pos.line_nr + 1).str()
		}
		.column_nr {
			_, column := util.filepath_pos_to_source_and_column(c.file.path, node.pos)
			node.val = (column + 1).str()
		}
		.vhash {
			node.val = util.vhash()
		}
		.vmod_file {
			if c.vmod_file_content.len == 0 {
				mut mcache := vmod.get_cache()
				vmod_file_location := mcache.get_by_file(c.file.path)
				if vmod_file_location.vmod_file.len == 0 {
					c.error('@VMOD_FILE can be used only in projects, that have v.mod file',
						node.pos)
				}
				vmod_content := os.read_file(vmod_file_location.vmod_file) or {
					''
				}
				$if windows {
					c.vmod_file_content = vmod_content.replace('\r\n', '\n')
				} $else {
					c.vmod_file_content = vmod_content
				}
			}
			node.val = c.vmod_file_content
		}
		.unknown {
			c.error('unknown @ identifier: ${node.name}. Available identifiers: $token.valid_at_tokens',
				node.pos)
		}
	}
	return table.string_type
}

pub fn (mut c Checker) ident(mut ident ast.Ident) table.Type {
	// TODO: move this
	if c.const_deps.len > 0 {
		mut name := ident.name
		if !name.contains('.') && ident.mod != 'builtin' {
			name = '${ident.mod}.$ident.name'
		}
		if name == c.const_decl {
			c.error('cycle in constant `$c.const_decl`', ident.pos)
			return table.void_type
		}
		c.const_deps << name
	}
	if ident.kind == .blank_ident {
		if ident.tok_kind !in [.assign, .decl_assign] {
			c.error('undefined ident: `_` (may only be used in assignments)', ident.pos)
		}
		return table.void_type
	}
	// second use
	if ident.kind in [.constant, .global, .variable] {
		info := ident.info as ast.IdentVar
		// if info.typ == table.t_type {
		// Got a var with type T, return current generic type
		// return c.cur_generic_type
		// }
		return info.typ
	} else if ident.kind == .function {
		info := ident.info as ast.IdentFn
		return info.typ
	} else if ident.kind == .unresolved {
		// first use
		if ident.tok_kind == .assign && ident.is_mut {
			c.error('`mut` not allowed with `=` (use `:=` to declare a variable)', ident.pos)
		}
		start_scope := c.file.scope.innermost(ident.pos.pos)
		if obj1 := start_scope.find(ident.name) {
			match mut obj1 as obj {
				ast.GlobalField {
					ident.kind = .global
					ident.info = ast.IdentVar{
						typ: obj.typ
					}
					ident.obj = obj1
					return obj.typ
				}
				ast.Var {
					// incase var was not marked as used yet (vweb tmpl)
					obj.is_used = true
					if ident.pos.pos < obj.pos.pos {
						c.error('undefined variable `$ident.name` (used before declaration)',
							ident.pos)
					}
					is_sum_type_cast := obj.sum_type_cast != 0 && !c.prevent_sum_type_unwrapping_once
					c.prevent_sum_type_unwrapping_once = false
					mut typ := if is_sum_type_cast { obj.sum_type_cast } else { obj.typ }
					if typ == 0 {
						if obj.expr is ast.Ident {
							inner_ident := obj.expr as ast.Ident
							if inner_ident.kind == .unresolved {
								c.error('unresolved variable: `$ident.name`', ident.pos)
								return table.void_type
							}
						}
						typ = c.expr(obj.expr)
					}
					is_optional := typ.has_flag(.optional)
					ident.kind = .variable
					ident.info = ast.IdentVar{
						typ: typ
						is_optional: is_optional
					}
					// if typ == table.t_type {
					// sym := c.table.get_type_symbol(c.cur_generic_type)
					// println('IDENT T unresolved $ident.name typ=$sym.source_name')
					// Got a var with type T, return current generic type
					// typ = c.cur_generic_type
					// }
					// } else {
					if !is_sum_type_cast {
						obj.typ = typ
					}
					ident.obj = obj1
					// unwrap optional (`println(x)`)
					if is_optional {
						return typ.clear_flag(.optional)
					}
					return typ
				}
				else {}
			}
		}
		// prepend mod to look for fn call or const
		mut name := ident.name
		if !name.contains('.') && ident.mod != 'builtin' {
			name = '${ident.mod}.$ident.name'
		}
		if obj1 := c.file.global_scope.find(name) {
			match mut obj1 as obj {
				ast.ConstField {
					mut typ := obj.typ
					if typ == 0 {
						typ = c.expr(obj.expr)
					}
					ident.name = name
					ident.kind = .constant
					ident.info = ast.IdentVar{
						typ: typ
					}
					obj.typ = typ
					ident.obj = obj1
					return typ
				}
				else {}
			}
		}
		// Non-anon-function object (not a call), e.g. `onclick(my_click)`
		if func := c.table.find_fn(name) {
			fn_type := table.new_type(c.table.find_or_register_fn_type(ident.mod, func,
				false, true))
			ident.name = name
			ident.kind = .function
			ident.info = ast.IdentFn{
				typ: fn_type
			}
			return fn_type
		}
	}
	if ident.language == .c {
		return table.int_type
	}
	if c.inside_sql {
		if field := c.table.struct_find_field(c.cur_orm_ts, ident.name) {
			return field.typ
		}
	}
	if ident.kind == .unresolved && ident.mod != 'builtin' {
		// search in the `builtin` idents, for example
		// main.compare_f32 may actually be builtin.compare_f32
		saved_mod := ident.mod
		ident.mod = 'builtin'
		builtin_type := c.ident(mut ident)
		if builtin_type != table.void_type {
			return builtin_type
		}
		ident.mod = saved_mod
	}
	if ident.tok_kind == .assign {
		c.error('undefined ident: `$ident.name` (use `:=` to declare a variable)', ident.pos)
	} else {
		c.error('undefined ident: `$ident.name`', ident.pos)
	}
	if c.table.known_type(ident.name) {
		// e.g. `User`  in `json.decode(User, '...')`
		return table.void_type
	}
	return table.void_type
}

pub fn (mut c Checker) concat_expr(mut concat_expr ast.ConcatExpr) table.Type {
	mut mr_types := []table.Type{}
	for expr in concat_expr.vals {
		mr_types << c.expr(expr)
	}
	if concat_expr.vals.len == 1 {
		typ := mr_types[0]
		concat_expr.return_type = typ
		return typ
	} else {
		typ := c.table.find_or_register_multi_return(mr_types)
		table.new_type(typ)
		concat_expr.return_type = typ
		return typ
	}
}

pub fn (mut c Checker) match_expr(mut node ast.MatchExpr) table.Type {
	node.is_expr = c.expected_type != table.void_type
	node.expected_type = c.expected_type
	cond_type := c.expr(node.cond)
	// we setting this here rather than at the end of the method
	// since it is used in c.match_exprs() it saves checking twice
	node.cond_type = cond_type
	if cond_type == 0 {
		c.error('compiler bug: match 0 cond type', node.pos)
	}
	cond_type_sym := c.table.get_type_symbol(cond_type)
	if cond_type_sym.kind !in [.sum_type, .interface_, .union_sum_type] {
		node.is_sum_type = false
	}
	c.match_exprs(mut node, cond_type_sym)
	c.expected_type = cond_type
	mut ret_type := table.void_type
	mut nbranches_with_return := 0
	mut nbranches_without_return := 0
	for branch in node.branches {
		c.stmts(branch.stmts)
		if node.is_expr && branch.stmts.len > 0 {
			// ignore last statement - workaround
			// currently the last statement in a match branch does not have an
			// expected value set, so e.g. IfExpr.is_expr is not set.
			// probably any mismatch will be caught by not producing a value instead
			for st in branch.stmts[0..branch.stmts.len - 1] {
				// must not contain C statements
				st.check_c_expr() or {
					c.error('`match` expression branch has $err', st.position())
				}
			}
		}
		// If the last statement is an expression, return its type
		if branch.stmts.len > 0 {
			match mut branch.stmts[branch.stmts.len - 1] as stmt {
				ast.ExprStmt {
					ret_type = c.expr(stmt.expr)
					stmt.typ = ret_type
				}
				else {
					// TODO: ask alex about this
					// typ := c.expr(stmt.expr)
					// type_sym := c.table.get_type_symbol(typ)
					// p.warn('match expr ret $type_sym.source_name')
					// node.typ = typ
					// return typ
				}
			}
		}
		if has_return := c.has_return(branch.stmts) {
			if has_return {
				nbranches_with_return++
			} else {
				nbranches_without_return++
			}
		}
	}
	if nbranches_with_return > 0 {
		if nbranches_with_return == node.branches.len {
			// an exhaustive match, and all branches returned
			c.returns = true
		}
		if nbranches_without_return > 0 {
			// some of the branches did not return
			c.returns = false
		}
	}
	// if ret_type != table.void_type {
	// node.is_expr = c.expected_type != table.void_type
	// node.expected_type = c.expected_type
	// }
	node.return_type = ret_type
	if node.is_mut {
		// Mark `x` in `match mut x {` as changed, and ensure it's mutable
		// TODO2 enable when code is fixed
		// c.fail_if_immutable(node.cond)
	}
	return ret_type
}

fn (mut c Checker) match_exprs(mut node ast.MatchExpr, type_sym table.TypeSymbol) {
	// branch_exprs is a histogram of how many times
	// an expr was used in the match
	mut branch_exprs := map[string]int{}
	cond_type_sym := c.table.get_type_symbol(node.cond_type)
	for branch in node.branches {
		for expr in branch.exprs {
			mut key := ''
			if expr is ast.RangeExpr {
				mut low := 0
				mut high := 0
				c.expected_type = node.expected_type
				low_expr := expr.low
				high_expr := expr.high
				if low_expr is ast.IntegerLiteral {
					if high_expr is ast.IntegerLiteral {
						low = low_expr.val.int()
						high = high_expr.val.int()
					} else {
						c.error('mismatched range types', low_expr.pos)
					}
				} else if low_expr is ast.CharLiteral {
					if high_expr is ast.CharLiteral {
						low = low_expr.val[0]
						high = high_expr.val[0]
					} else {
						c.error('mismatched range types', low_expr.pos)
					}
				} else {
					typ := c.table.type_to_str(c.expr(expr.low))
					c.error('cannot use type `$typ` in match range', branch.pos)
				}
				for i in low .. high + 1 {
					key = i.str()
					val := if key in branch_exprs { branch_exprs[key] } else { 0 }
					if val == 1 {
						c.error('match case `$key` is handled more than once', branch.pos)
					}
					branch_exprs[key] = val + 1
				}
				continue
			}
			match expr {
				ast.Type {
					key = c.table.type_to_str(expr.typ)
					// smart cast only if one type is given (currently) // TODO make this work if types have same fields
					if branch.exprs.len == 1 && cond_type_sym.kind == .union_sum_type {
						mut scope := c.file.scope.innermost(branch.pos.pos)
						match node.cond as node_cond {
							ast.SelectorExpr { scope.register_struct_field(ast.ScopeStructField{
									struct_type: node_cond.expr_type
									name: node_cond.field_name
									typ: node.cond_type
									sum_type_cast: expr.typ
									pos: node_cond.pos
								}) }
							ast.Ident { scope.register(node.var_name, ast.Var{
									name: node.var_name
									typ: node.cond_type
									pos: node_cond.pos
									is_used: true
									is_mut: node.is_mut
									sum_type_cast: expr.typ
								}) }
							else {}
						}
					}
				}
				ast.EnumVal {
					key = expr.val
				}
				else {
					key = expr.str()
				}
			}
			val := if key in branch_exprs { branch_exprs[key] } else { 0 }
			if val == 1 {
				c.error('match case `$key` is handled more than once', branch.pos)
			}
			c.expected_type = node.cond_type
			expr_type := c.expr(expr)
			if cond_type_sym.kind == .interface_ {
				// TODO
				// This generates a memory issue with TCC
				// Needs to be checked later when TCC errors are fixed
				// Current solution is to move expr.position() to its own statement
				// c.type_implements(expr_type, c.expected_type, expr.position())
				expr_pos := expr.position()
				c.type_implements(expr_type, c.expected_type, expr_pos)
			} else if cond_type_sym.info is table.UnionSumType as info {
				if expr_type !in info.variants {
					expr_str := c.table.type_to_str(expr_type)
					expect_str := c.table.type_to_str(c.expected_type)
					c.error('`$expect_str` has no variant `$expr_str`', expr.position())
				}
			} else if !c.check_types(expr_type, c.expected_type) {
				expr_str := c.table.type_to_str(expr_type)
				expect_str := c.table.type_to_str(c.expected_type)
				c.error('cannot match `$expr_str` with `$expect_str` condition', expr.position())
			}
			branch_exprs[key] = val + 1
		}
	}
	// check that expressions are exhaustive
	// this is achieved either by putting an else
	// or, when the match is on a sum type or an enum
	// by listing all variants or values
	mut is_exhaustive := true
	mut unhandled := []string{}
	match type_sym.info as info {
		table.SumType {
			for v in info.variants {
				v_str := c.table.type_to_str(v)
				if v_str !in branch_exprs {
					is_exhaustive = false
					unhandled << '`$v_str`'
				}
			}
		}
		table.UnionSumType {
			for v in info.variants {
				v_str := c.table.type_to_str(v)
				if v_str !in branch_exprs {
					is_exhaustive = false
					unhandled << '`$v_str`'
				}
			}
		}
		//
		table.Enum {
			for v in info.vals {
				if v !in branch_exprs {
					is_exhaustive = false
					unhandled << '`.$v`'
				}
			}
		}
		else {
			is_exhaustive = false
		}
	}
	mut else_branch := node.branches[node.branches.len - 1]
	mut has_else := else_branch.is_else
	if !has_else {
		for i, branch in node.branches {
			if branch.is_else && i != node.branches.len - 1 {
				c.error('`else` must be the last branch of `match`', branch.pos)
				else_branch = branch
				has_else = true
			}
		}
	}
	if is_exhaustive {
		if has_else {
			c.error('match expression is exhaustive, `else` is unnecessary', else_branch.pos)
		}
		return
	}
	if has_else {
		return
	}
	mut err_details := 'match must be exhaustive'
	if unhandled.len > 0 {
		err_details += ' (add match branches for: '
		if unhandled.len < match_exhaustive_cutoff_limit {
			err_details += unhandled.join(', ')
		} else {
			remaining := unhandled.len - match_exhaustive_cutoff_limit
			err_details += unhandled[0..match_exhaustive_cutoff_limit].join(', ')
			err_details += ', and $remaining others ...'
		}
		err_details += ' or `else {}` at the end)'
	} else {
		err_details += ' (add `else {}` at the end)'
	}
	c.error(err_details, node.pos)
}

pub fn (mut c Checker) select_expr(mut node ast.SelectExpr) table.Type {
	node.is_expr = c.expected_type != table.void_type
	node.expected_type = c.expected_type
	for branch in node.branches {
		c.stmt(branch.stmt)
		match branch.stmt as stmt {
			ast.ExprStmt {
				if branch.is_timeout {
					if !stmt.typ.is_int() {
						tsym := c.table.get_type_symbol(stmt.typ)
						c.error('invalid type `$tsym.name` for timeout - expected integer type aka `time.Duration`',
							stmt.pos)
					}
				} else {
					if stmt.expr is ast.InfixExpr as expr {
						if expr.left !is ast.Ident &&
							expr.left !is ast.SelectorExpr && expr.left !is ast.IndexExpr {
							c.error('channel in `select` key must be predefined', expr.left.position())
						}
					} else {
						c.error('invalid expression for `select` key', stmt.expr.position())
					}
				}
			}
			ast.AssignStmt {
				match stmt.right[0] as expr {
					ast.PrefixExpr {
						if expr.right !is ast.Ident &&
							expr.right !is ast.SelectorExpr && expr.right !is ast.IndexExpr {
							c.error('channel in `select` key must be predefined', expr.right.position())
						}
						if expr.or_block.kind != .absent {
							err_prefix := if expr.or_block.kind == .block { 'or block' } else { 'error propagation' }
							c.error('$err_prefix not allowed in `select` key', expr.or_block.pos)
						}
					}
					else {
						c.error('`<-` receive expression expected', stmt.right[0].position())
					}
				}
			}
			else {
				if !branch.is_else {
					c.error('receive or send statement expected as `select` key', branch.stmt.position())
				}
			}
		}
		c.stmts(branch.stmts)
	}
	return table.bool_type
}

pub fn (mut c Checker) lock_expr(mut node ast.LockExpr) table.Type {
	for i in 0 .. node.lockeds.len {
		c.ident(mut node.lockeds[i])
		id := node.lockeds[i]
		if id.obj is ast.Var as v {
			if v.typ.share() != .shared_t {
				c.error('`$id.name` must be declared `shared` to be locked', id.pos)
			}
		} else {
			c.error('`$id.name` is not a variable and cannot be locked', id.pos)
		}
		if id.name in c.locked_names {
			c.error('`$id.name` is already locked', id.pos)
		} else if id.name in c.rlocked_names {
			c.error('`$id.name` is already read-locked', id.pos)
		}
		if node.is_rlock {
			c.rlocked_names << id.name
		} else {
			c.locked_names << id.name
		}
	}
	c.stmts(node.stmts)
	if node.is_rlock {
		c.rlocked_names = c.rlocked_names[..c.rlocked_names.len - node.lockeds.len]
	} else {
		c.locked_names = c.locked_names[..c.locked_names.len - node.lockeds.len]
	}
	// void for now... maybe sometime `x := lock a { a.getval() }`
	return table.void_type
}

pub fn (mut c Checker) unsafe_expr(mut node ast.UnsafeExpr) table.Type {
	assert !c.inside_unsafe
	c.inside_unsafe = true
	t := c.expr(node.expr)
	c.inside_unsafe = false
	return t
}

pub fn (mut c Checker) if_expr(mut node ast.IfExpr) table.Type {
	if_kind := if node.is_comptime { '\$if' } else { 'if' }
	expr_required := c.expected_type != table.void_type
	former_expected_type := c.expected_type
	node.typ = table.void_type
	mut nbranches_with_return := 0
	mut nbranches_without_return := 0
	mut should_skip := false // Whether the current branch should be skipped
	mut found_branch := false // Whether a matching branch was found- skip the rest
	for i in 0 .. node.branches.len {
		mut branch := node.branches[i]
		if branch.cond is ast.ParExpr {
			c.error('unnecessary `()` in `$if_kind` condition, use `$if_kind expr {` instead of `$if_kind (expr) {`.',
				branch.pos)
		}
		if !node.has_else || i < node.branches.len - 1 {
			if node.is_comptime {
				should_skip = c.comp_if_branch(branch.cond, branch.pos)
			} else {
				// check condition type is boolean
				c.expected_type = table.bool_type
				cond_typ := c.expr(branch.cond)
				if cond_typ.idx() !in [table.bool_type_idx, table.void_type_idx] && !c.pref.translated {
					// void types are skipped, because they mean the var was initialized incorrectly
					// (via missing function etc)
					typ_sym := c.table.get_type_symbol(cond_typ)
					c.error('non-bool type `$typ_sym.source_name` used as if condition',
						branch.pos)
				}
			}
		}
		// smartcast sumtypes and interfaces when using `is`
		if !node.is_comptime && branch.cond is ast.InfixExpr {
			infix := branch.cond as ast.InfixExpr
			if infix.op == .key_is {
				right_expr := infix.right as ast.Type
				left_sym := c.table.get_type_symbol(infix.left_type)
				expr_type := c.expr(infix.left)
				if left_sym.kind == .interface_ {
					c.type_implements(right_expr.typ, expr_type, branch.pos)
				} else if !c.check_types(expr_type, right_expr.typ) {
					expect_str := c.table.type_to_str(right_expr.typ)
					expr_str := c.table.type_to_str(expr_type)
					c.error('cannot use type `$expect_str` as type `$expr_str`', branch.pos)
				}
				if (infix.left is ast.Ident ||
					infix.left is ast.SelectorExpr) &&
					infix.right is ast.Type {
					is_variable := if infix.left is ast.Ident { (infix.left as ast.Ident).kind ==
							.variable } else { true }
					// Register shadow variable or `as` variable with actual type
					if is_variable {
						if left_sym.kind in [.sum_type, .interface_, .union_sum_type] {
							mut is_mut := false
							mut scope := c.file.scope.innermost(branch.body_pos.pos)
							if infix.left is ast.Ident as infix_left {
								if v := scope.find_var(infix_left.name) {
									is_mut = v.is_mut
								}
								if !is_mut && left_sym.kind == .union_sum_type {
									scope.register(branch.left_as_name, ast.Var{
										name: branch.left_as_name
										typ: infix.left_type
										sum_type_cast: right_expr.typ
										pos: infix.left.position()
										is_used: true
										is_mut: is_mut
									})
								}
							} else if infix.left is ast.SelectorExpr as selector {
								expr_sym := c.table.get_type_symbol(selector.expr_type)
								field := c.table.struct_find_field(expr_sym, selector.field_name) or {
									table.Field{}
								}
								is_mut = field.is_mut
								is_root_mut := scope.is_selector_root_mutable(c.table,
									selector)
								if !is_root_mut && !is_mut && left_sym.kind == .union_sum_type {
									scope.register_struct_field(ast.ScopeStructField{
										struct_type: selector.expr_type
										name: selector.field_name
										typ: infix.left_type
										sum_type_cast: right_expr.typ
										pos: infix.left.position()
									})
								}
							}
							if left_sym.kind != .union_sum_type && branch.left_as_name.len > 0 {
								scope.register(branch.left_as_name, ast.Var{
									name: branch.left_as_name
									typ: right_expr.typ.to_ptr()
									pos: infix.left.position()
									is_used: true
									is_mut: is_mut
								})
								node.branches[i].smartcast = true
							}
						}
					}
				}
			}
		}
		if node.is_comptime { // Skip checking if needed
			cur_skip_flags := c.skip_flags
			if found_branch {
				c.skip_flags = true
			} else if should_skip {
				c.skip_flags = true
				should_skip = false // Reset the value of `should_skip` for the next branch
			} else {
				found_branch = true // If a branch wasn't skipped, the rest must be
			}
			if !c.skip_flags || c.pref.output_cross_c {
				c.stmts(branch.stmts)
			} else {
				node.branches[i].stmts = []
			}
			c.skip_flags = cur_skip_flags
		} else {
			c.stmts(branch.stmts)
		}
		if expr_required {
			if branch.stmts.len > 0 && branch.stmts[branch.stmts.len - 1] is ast.ExprStmt {
				mut last_expr := branch.stmts[branch.stmts.len - 1] as ast.ExprStmt
				c.expected_type = former_expected_type
				last_expr.typ = c.expr(last_expr.expr)
				// if last_expr.typ != node.typ {
				// if !c.check_types(node.typ, last_expr.typ) {
				if !c.check_types(last_expr.typ, node.typ) {
					if node.typ == table.void_type {
						// first branch of if expression
						node.is_expr = true
						node.typ = last_expr.typ
						continue
					} else if node.typ in [table.any_flt_type, table.any_int_type] {
						if node.typ == table.any_int_type {
							if last_expr.typ.is_int() || last_expr.typ.is_float() {
								node.typ = last_expr.typ
								continue
							}
						} else { // node.typ == any_float
							if last_expr.typ.is_float() {
								node.typ = last_expr.typ
								continue
							}
						}
					}
					if last_expr.typ in [table.any_flt_type, table.any_int_type] {
						if last_expr.typ == table.any_int_type {
							if node.typ.is_int() || node.typ.is_float() {
								continue
							}
						} else { // expr_type == any_float
							if node.typ.is_float() {
								continue
							}
						}
					}
					c.error('mismatched types `${c.table.type_to_str(node.typ)}` and `${c.table.type_to_str(last_expr.typ)}`',
						node.pos)
				}
			} else {
				c.error('`$if_kind` expression requires an expression as the last statement of every branch',
					branch.pos)
			}
			for st in branch.stmts {
				// must not contain C statements
				st.check_c_expr() or {
					c.error('`if` expression branch has $err', st.position())
				}
			}
		}
		// Also check for returns inside a comp.if's statements, even if its contents aren't parsed
		if has_return := c.has_return(branch.stmts) {
			if has_return {
				nbranches_with_return++
			} else {
				nbranches_without_return++
			}
		}
	}
	if nbranches_with_return > 0 {
		if nbranches_with_return == node.branches.len {
			// if/else... where all branches returned
			c.returns = true
		}
		if !node.has_else {
			// `if cond { return ... }` means that when cond is false, execution continues
			c.returns = false
		}
		if nbranches_without_return > 0 {
			// some of the branches did not return
			c.returns = false
		}
	}
	// if only untyped literals were given default to int/f64
	if node.typ == table.any_int_type {
		node.typ = table.int_type
	} else if node.typ == table.any_flt_type {
		node.typ = table.f64_type
	}
	if expr_required {
		if !node.has_else {
			d := if node.is_comptime { '$' } else { '' }
			c.error('`$if_kind` expression needs `${d}else` clause', node.pos)
		}
		return node.typ
	}
	return table.bool_type
}

// comp_if_branch checks the condition of a compile-time `if` branch. It returns a `bool` that
// saying whether that branch's contents should be skipped (targets a different os for example)
fn (mut c Checker) comp_if_branch(cond ast.Expr, pos token.Position) bool {
	// TODO: better error messages here
	match cond {
		ast.ParExpr {
			return c.comp_if_branch(cond.expr, pos)
		}
		ast.PrefixExpr {
			if cond.op != .not {
				c.error('invalid `\$if` condition', cond.pos)
			}
			return !c.comp_if_branch(cond.right, cond.pos)
		}
		ast.PostfixExpr {
			if cond.op != .question {
				c.error('invalid \$if postfix operator', cond.pos)
			} else if cond.expr is ast.Ident as ident {
				return ident.name !in c.pref.compile_defines_all
			} else {
				c.error('invalid `\$if` condition', cond.pos)
			}
		}
		ast.InfixExpr {
			match cond.op {
				.and {
					l := c.comp_if_branch(cond.left, cond.pos)
					r := c.comp_if_branch(cond.right, cond.pos)
					return l || r // skip (return true) if at least one should be skipped
				}
				.logical_or {
					l := c.comp_if_branch(cond.left, cond.pos)
					r := c.comp_if_branch(cond.right, cond.pos)
					return l && r // skip (return true) only if both should be skipped
				}
				.key_is, .not_is {
					// $if method.@type is string
					// TODO better checks here, will be done in comp. for PR
					if cond.left !is ast.SelectorExpr || cond.right !is ast.Type {
						c.error('invalid `\$if` condition', cond.pos)
					}
				}
				.eq, .ne {
					// $if method.args.len == 1
					// TODO better checks here, will be done in comp. for PR
					if cond.left !is ast.SelectorExpr || cond.right !is ast.IntegerLiteral {
						c.error('invalid `\$if` condition', cond.pos)
					}
				}
				else {
					c.error('invalid `\$if` condition', cond.pos)
				}
			}
		}
		ast.Ident {
			if cond.name in valid_comp_if_os {
				return cond.name != c.pref.os.str().to_lower() // TODO hack
			} else if cond.name in valid_comp_if_compilers {
				return pref.cc_from_string(cond.name) != c.pref.ccompiler_type
			} else if cond.name in valid_comp_if_platforms {
				return false // TODO
			} else if cond.name in valid_comp_if_other {
				// TODO: This should probably be moved
				match cond.name as name {
					'js' { return c.pref.backend != .js }
					'debug' { return !c.pref.is_debug }
					'test' { return !c.pref.is_test }
					'glibc' { return false } // TODO
					'prealloc' { return !c.pref.prealloc }
					'no_bounds_checking' { return cond.name !in c.pref.compile_defines_all }
					else { return false }
				}
			} else {
				if cond.name !in c.pref.compile_defines_all {
					c.error('unknown \$if value', pos)
				}
			}
		}
		else {
			c.error('invalid `\$if` condition', pos)
		}
	}
	return false
}

fn (c &Checker) has_return(stmts []ast.Stmt) ?bool {
	// complexity means either more match or ifs
	mut has_complexity := false
	for s in stmts {
		if s is ast.ExprStmt {
			if s.expr is ast.IfExpr || s.expr is ast.MatchExpr {
				has_complexity = true
				break
			}
		}
	}
	// if the inner complexity covers all paths with returns there is no need for further checks
	if !has_complexity || !c.returns {
		return has_top_return(stmts)
	}
	return none
}

pub fn (mut c Checker) postfix_expr(mut node ast.PostfixExpr) table.Type {
	typ := c.expr(node.expr)
	typ_sym := c.table.get_type_symbol(typ)
	// if !typ.is_number() {
	if !typ_sym.is_number() {
		c.error('invalid operation: $node.op.str() (non-numeric type `$typ_sym.source_name`)',
			node.pos)
	} else {
		node.auto_locked, _ = c.fail_if_immutable(node.expr)
	}
	if (typ.is_ptr() || typ_sym.is_pointer()) && !c.inside_unsafe {
		c.warn('pointer arithmetic is only allowed in `unsafe` blocks', node.pos)
	}
	return typ
}

fn (mut c Checker) check_index_type(typ_sym &table.TypeSymbol, index_type table.Type, pos token.Position) {
	index_type_sym := c.table.get_type_symbol(index_type)
	// println('index expr left=$typ_sym.source_name $node.pos.line_nr')
	// if typ_sym.kind == .array && (!(table.type_idx(index_type) in table.number_type_idxs) &&
	// index_type_sym.kind != .enum_) {
	if typ_sym.kind in [.array, .array_fixed, .string, .ustring] {
		if !(index_type.is_number() || index_type_sym.kind == .enum_) {
			type_str := if typ_sym.kind in [.string, .ustring] { 'non-integer string index `$index_type_sym.source_name`' } else { 'non-integer index `$index_type_sym.source_name` (array type `$typ_sym.source_name`)' }
			c.error('$type_str', pos)
		}
		if index_type.has_flag(.optional) {
			type_str := if typ_sym.kind in [.string, .ustring] { '(type `$typ_sym.source_name`)' } else { '(array type `$typ_sym.source_name`)' }
			c.error('cannot use optional as index $type_str', pos)
		}
	}
}

pub fn (mut c Checker) index_expr(mut node ast.IndexExpr) table.Type {
	typ := c.expr(node.left)
	node.left_type = typ
	typ_sym := c.table.get_type_symbol(typ)
	if typ_sym.kind !in [.array, .array_fixed, .string, .map] && !typ.is_ptr() && !(!typ_sym.name[0].is_capital() &&
		typ_sym.name.ends_with('ptr')) && !typ.has_flag(.variadic) { // byteptr, charptr etc
		c.error('type `$typ_sym.source_name` does not support indexing', node.pos)
	}
	if typ_sym.kind == .string && !typ.is_ptr() && node.is_setter {
		c.error('cannot assign to s[i] since V strings are immutable\n' +
			'(note, that variables may be mutable but string values are always immutable, like in Go and Java)',
			node.pos)
	}
	if !c.inside_unsafe && (typ.is_ptr() || typ.is_pointer()) {
		mut is_ok := false
		if node.left is ast.Ident {
			ident := node.left as ast.Ident
			scope := c.file.scope.innermost(ident.pos.pos)
			if v := scope.find_var(ident.name) {
				// `mut param []T` function parameter
				is_ok = v.is_mut && v.is_arg && !typ.deref().is_ptr()
			}
		}
		if !is_ok && !c.pref.translated {
			c.warn('pointer indexing is only allowed in `unsafe` blocks', node.pos)
		}
	}
	if node.index is ast.RangeExpr as range { // [1..2]
		if range.has_low {
			index_type := c.expr(range.low)
			c.check_index_type(typ_sym, index_type, node.pos)
		}
		if range.has_high {
			index_type := c.expr(range.high)
			c.check_index_type(typ_sym, index_type, node.pos)
		}
		// array[1..2] => array
		// fixed_array[1..2] => array
		if typ_sym.kind == .array_fixed {
			elem_type := c.table.value_type(typ)
			idx := c.table.find_or_register_array(elem_type, 1, c.mod)
			return table.new_type(idx)
		}
		return typ.set_nr_muls(0)
	} else { // [1]
		index_type := c.expr(node.index)
		c.check_index_type(typ_sym, index_type, node.pos)
		if typ_sym.kind == .map && index_type.idx() != table.string_type_idx {
			c.error('non-string map index (map type `$typ_sym.source_name`)', node.pos)
		}
		value_type := c.table.value_type(typ)
		if value_type != table.void_type {
			return value_type
		}
	}
	return typ
}

// `.green` or `Color.green`
// If a short form is used, `expected_type` needs to be an enum
// with this value.
pub fn (mut c Checker) enum_val(mut node ast.EnumVal) table.Type {
	typ_idx := if node.enum_name == '' {
		c.expected_type.idx()
	} else { //
		c.table.find_type_idx(node.enum_name)
	}
	// println('checker: enum_val: $node.enum_name typeidx=$typ_idx')
	if typ_idx == 0 {
		c.error('not an enum (name=$node.enum_name) (type_idx=0)', node.pos)
		return table.void_type
	}
	mut typ := table.new_type(typ_idx)
	if c.pref.translated {
		// TODO make more strict
		node.typ = typ
		return typ
	}
	if typ == table.void_type {
		c.error('not an enum', node.pos)
		return table.void_type
	}
	mut typ_sym := c.table.get_type_symbol(typ)
	// println('tname=$typ_sym.source_name $node.pos.line_nr $c.file.path')
	if typ_sym.kind == .array && node.enum_name.len == 0 {
		array_info := typ_sym.info as table.Array
		typ = array_info.elem_type
		typ_sym = c.table.get_type_symbol(typ)
	}
	if typ_sym.kind != .enum_ && !c.pref.translated {
		// TODO in C int fields can be compared to enums, need to handle that in C2V
		c.error('expected type is not an enum (`$typ_sym.source_name`)', node.pos)
		return table.void_type
	}
	if typ_sym.info !is table.Enum {
		c.error('not an enum', node.pos)
		return table.void_type
	}
	// info := typ_sym.info as table.Enum
	info := typ_sym.enum_info()
	// rintln('checker: x = $info.x enum val $c.expected_type $typ_sym.source_name')
	// println(info.vals)
	if node.val !in info.vals {
		c.error('enum `$typ_sym.source_name` does not have a value `$node.val`', node.pos)
	}
	node.typ = typ
	return typ
}

pub fn (mut c Checker) chan_init(mut node ast.ChanInit) table.Type {
	if node.typ != 0 {
		info := c.table.get_type_symbol(node.typ).chan_info()
		node.elem_type = info.elem_type
		if node.has_cap {
			c.check_array_init_para_type('cap', node.cap_expr, node.pos)
		}
		return node.typ
	} else {
		c.error('`chan` of unknown type', node.pos)
		return node.typ
	}
}

pub fn (mut c Checker) map_init(mut node ast.MapInit) table.Type {
	// `x := map[string]string` - set in parser
	if node.typ != 0 {
		info := c.table.get_type_symbol(node.typ).map_info()
		key_sym := c.table.get_type_symbol(info.key_type)
		value_sym := c.table.get_type_symbol(info.value_type)
		if key_sym.kind == .placeholder {
			c.error('unknown type `$key_sym.source_name`', node.pos)
		}
		if value_sym.kind == .placeholder {
			c.error('unknown type `$value_sym.source_name`', node.pos)
		}
		node.key_type = info.key_type
		node.value_type = info.value_type
		return node.typ
	}
	// `{'age': 20}`
	key0_type := c.table.mktyp(c.expr(node.keys[0]))
	val0_type := c.table.mktyp(c.expr(node.vals[0]))
	for i, key in node.keys {
		key_i := key as ast.StringLiteral
		for j in 0 .. i {
			key_j := node.keys[j] as ast.StringLiteral
			if key_i.val == key_j.val {
				c.error('duplicate key "$key_i.val" in map literal', key.position())
			}
		}
		if i == 0 {
			continue
		}
		val := node.vals[i]
		key_type := c.expr(key)
		val_type := c.expr(val)
		if !c.check_types(key_type, key0_type) {
			key0_type_sym := c.table.get_type_symbol(key0_type)
			key_type_sym := c.table.get_type_symbol(key_type)
			c.error('map init: cannot use `$key_type_sym.source_name` as `$key0_type_sym.source_name` for map key',
				node.pos)
		}
		if !c.check_types(val_type, val0_type) {
			val0_type_sym := c.table.get_type_symbol(val0_type)
			val_type_sym := c.table.get_type_symbol(val_type)
			c.error('map init: cannot use `$val_type_sym.source_name` as `$val0_type_sym.source_name` for map value',
				node.pos)
		}
	}
	map_type := table.new_type(c.table.find_or_register_map(key0_type, val0_type))
	node.typ = map_type
	node.key_type = key0_type
	node.value_type = val0_type
	return map_type
}

pub fn (mut c Checker) add_error_detail(s string) {
	c.error_details << s
}

pub fn (mut c Checker) warn(s string, pos token.Position) {
	allow_warnings := !(c.pref.is_prod || c.pref.warns_are_errors) // allow warnings only in dev builds
	c.warn_or_error(s, pos, allow_warnings) // allow warnings only in dev builds
}

pub fn (mut c Checker) error(message string, pos token.Position) {
	if c.pref.translated && message.starts_with('mismatched types') {
		// TODO move this
		return
	}
	if c.pref.is_verbose {
		print_backtrace()
	}
	msg := message.replace('`array_', '`[]')
	c.warn_or_error(msg, pos, false)
}

// check_struct_signature checks if both structs has the same signature / fields for casting
fn (c Checker) check_struct_signature(from table.Struct, to table.Struct) bool {
	if from.fields.len != to.fields.len {
		return false
	}
	for _, field in from.fields {
		filtered := to.fields.filter(it.name == field.name)
		if filtered.len != 1 {
			// field doesn't exist
			return false
		}
		counterpart := filtered[0]
		if field.typ != counterpart.typ {
			// field has different tye
			return false
		}
		if field.is_pub != counterpart.is_pub {
			// field is not public while the other one is
			return false
		}
		if field.is_mut != counterpart.is_mut {
			// field is not mutable while the other one is
			return false
		}
	}
	return true
}

fn (mut c Checker) warn_or_error(message string, pos token.Position, warn bool) {
	// add backtrace to issue struct, how?
	// if c.pref.is_verbose {
	// print_backtrace()
	// }
	mut details := ''
	if c.error_details.len > 0 {
		details = c.error_details.join('\n')
		c.error_details = []
	}
	if warn && !c.pref.skip_warnings {
		c.nr_warnings++
		wrn := errors.Warning{
			reporter: errors.Reporter.checker
			pos: pos
			file_path: c.file.path
			message: message
			details: details
		}
		c.file.warnings << wrn
		c.warnings << wrn
		return
	}
	if !warn {
		c.nr_errors++
		if pos.line_nr !in c.error_lines {
			err := errors.Error{
				reporter: errors.Reporter.checker
				pos: pos
				file_path: c.file.path
				message: message
				details: details
			}
			c.file.errors << err
			c.errors << err
			c.error_lines << pos.line_nr
		}
	}
}

// for debugging only
fn (c &Checker) fileis(s string) bool {
	return c.file.path.contains(s)
}

fn (mut c Checker) sql_expr(mut node ast.SqlExpr) table.Type {
	c.inside_sql = true
	defer {
		c.inside_sql = false
	}
	sym := c.table.get_type_symbol(node.table_type)
	if sym.kind == .placeholder {
		c.error('orm: unknown type `$sym.source_name`', node.pos)
		return table.void_type
	}
	c.cur_orm_ts = sym
	info := sym.info as table.Struct
	fields := c.fetch_and_verify_orm_fields(info, node.pos, node.table_name)
	node.fields = fields
	node.table_name = sym.name
	if node.has_where {
		c.expr(node.where_expr)
	}
	if node.has_offset {
		c.expr(node.offset_expr)
	}
	if node.has_limit {
		c.expr(node.limit_expr)
	}
	if node.has_order {
		c.expr(node.order_expr)
	}
	c.expr(node.db_expr)
	return node.typ
}

fn (mut c Checker) sql_stmt(mut node ast.SqlStmt) table.Type {
	c.inside_sql = true
	defer {
		c.inside_sql = false
	}
	if node.table_type == 0 {
		c.error('orm: unknown type `$node.table_name`', node.pos)
	}
	sym := c.table.get_type_symbol(node.table_type)
	if sym.kind == .placeholder {
		c.error('orm: unknown type `$sym.source_name`', node.pos)
		return table.void_type
	}
	c.cur_orm_ts = sym
	info := sym.info as table.Struct
	fields := c.fetch_and_verify_orm_fields(info, node.pos, node.table_name)
	node.fields = fields
	c.expr(node.db_expr)
	if node.kind == .update {
		for expr in node.update_exprs {
			c.expr(expr)
		}
	}
	c.expr(node.where_expr)
	return table.void_type
}

fn (mut c Checker) fetch_and_verify_orm_fields(info table.Struct, pos token.Position, table_name string) []table.Field {
	fields := info.fields.filter(it.typ in
		[table.string_type, table.int_type, table.bool_type] && !it.attrs.contains('skip'))
	if fields.len == 0 {
		c.error('V orm: select: empty fields in `$table_name`', pos)
	}
	if fields[0].name != 'id' {
		c.error('V orm: `id int` must be the first field in `$table_name`', pos)
	}
	return fields
}

fn (mut c Checker) post_process_generic_fns() {
	// Loop thru each generic function concrete type.
	// Check each specific fn instantiation.
	for i in 0 .. c.generic_funcs.len {
		if c.table.fn_gen_types.len == 0 {
			// no concrete types, so just skip:
			continue
		}
		mut node := c.generic_funcs[i]
		for gen_type in c.table.fn_gen_types[node.name] {
			c.cur_generic_type = gen_type
			c.fn_decl(mut node)
			if node.name in ['vweb.run_app', 'vweb.run'] {
				c.vweb_gen_types << gen_type
			}
		}
		c.cur_generic_type = 0
		c.generic_funcs[i] = 0
	}
	// The generic funtions for each file/mod should be
	// postprocessed just once in the checker, while the file/mod
	// context is still the same.
	c.generic_funcs = []
}

fn (mut c Checker) fn_decl(mut node ast.FnDecl) {
	c.returns = false
	if node.is_generic && c.cur_generic_type == 0 {
		// Just remember the generic function for now.
		// It will be processed later in c.post_process_generic_fns,
		// after all other normal functions are processed.
		// This is done so that all generic function calls can
		// have a chance to populate c.table.fn_gen_types with
		// the correct concrete types.
		c.generic_funcs << node
		return
	}
	if node.language == .v && !c.is_builtin_mod {
		c.check_valid_snake_case(node.name, 'function name', node.pos)
	}
	if node.is_method {
		mut sym := c.table.get_type_symbol(node.receiver.typ)
		if sym.kind == .interface_ {
			c.error('interfaces cannot be used as method receiver', node.receiver_pos)
		}
		if sym.kind == .sum_type && node.name == 'type_name' {
			c.error('method overrides built-in sum type method', node.pos)
		}
		// if sym.has_method(node.name) {
		// c.warn('duplicate method `$node.name`', node.pos)
		// }
		// Do not allow to modify types from other modules
		if sym.mod != c.mod && !c.is_builtin_mod && sym.mod != '' { // TODO remove != ''
			// remove the method to hide other related errors (`method is private` etc)
			mut idx := 0
			for i, m in sym.methods {
				if m.name == node.name {
					idx = i
					break
				}
			}
			sym.methods.delete(idx)
			//
			c.error('cannot define new methods on non-local `$sym.source_name` (' +
				'current module is `$c.mod`, `$sym.source_name` is from `$sym.mod`)', node.pos)
		}
		// needed for proper error reporting during vweb route checking
		sym.methods[node.method_idx].source_fn = voidptr(node)
	}
	if node.language == .v {
		// Make sure all types are valid
		for arg in node.params {
			sym := c.table.get_type_symbol(arg.typ)
			if sym.kind == .placeholder {
				c.error('unknown type `$sym.source_name`', node.pos)
			}
		}
	}
	if node.language == .v && node.is_method && node.name == 'str' {
		if node.return_type != table.string_type {
			c.error('.str() methods should return `string`', node.pos)
		}
		if node.params.len != 1 {
			c.error('.str() methods should have 0 arguments', node.pos)
		}
	}
	// TODO c.pref.is_vet
	if node.language == .v && !node.is_method && node.params.len == 0 && node.return_type == table.void_type_idx &&
		node.name.after('.').starts_with('test_') && !c.file.path.ends_with('_test.v') {
		// simple heuristic
		for st in node.stmts {
			if st is ast.AssertStmt {
				c.warn('tests will not be run because filename does not end with `_test.v`',
					node.pos)
				break
			}
		}
	}
	c.expected_type = table.void_type
	c.cur_fn = node
	// Add return if `fn(...) ? {...}` have no return at end
	if node.return_type != table.void_type && node.return_type.has_flag(.optional) &&
		(node.stmts.len == 0 || node.stmts[node.stmts.len - 1] !is ast.Return) {
		sym := c.table.get_type_symbol(node.return_type)
		if sym.kind == .void {
			node.stmts << ast.Return{
				pos: node.pos
			}
		} else {
			node.stmts << ast.Return{
				pos: node.pos
				exprs: [ast.Expr(ast.None{
					pos: node.pos
				})]
			}
		}
	}
	c.stmts(node.stmts)
	returns := c.returns || has_top_return(node.stmts)
	if node.language == .v && !node.no_body && node.return_type != table.void_type && !returns &&
		node.name !in ['panic', 'exit'] {
		c.error('missing return at end of function `$node.name`', node.pos)
	}
	c.returns = false
	node.source_file = c.file
}

fn has_top_return(stmts []ast.Stmt) bool {
	for stmt in stmts {
		if stmt is ast.Return {
			return true
		} else if stmt is ast.Block {
			if has_top_return(stmt.stmts) {
				return true
			}
		} else if stmt is ast.ExprStmt {
			if stmt.expr is ast.CallExpr as ce {
				if ce.name in ['panic', 'exit'] {
					return true
				}
			}
		}
	}
	return false
}

fn (mut c Checker) verify_vweb_params_for_method(m table.Fn) (bool, int, int) {
	margs := m.params.len - 1 // first arg is the receiver/this
	if m.attrs.len == 0 {
		// allow non custom routed methods, with 1:1 mapping
		return true, -1, margs
	}
	mut route_attributes := 0
	for a in m.attrs {
		if a.name.starts_with('/') {
			route_attributes += a.name.count(':')
		}
	}
	return route_attributes == margs, route_attributes, margs
}

fn (mut c Checker) verify_all_vweb_routes() {
	if c.vweb_gen_types.len == 0 {
		return
	}
	typ_vweb_result := c.table.find_type_idx('vweb.Result')
	for vgt in c.vweb_gen_types {
		sym_app := c.table.get_type_symbol(vgt)
		for m in sym_app.methods {
			if m.return_type_source_name == 'vweb.Result' {
				is_ok, nroute_attributes, nargs := c.verify_vweb_params_for_method(m)
				if !is_ok {
					f := &ast.FnDecl(m.source_fn)
					if isnil(f) {
						continue
					}
					if f.return_type == typ_vweb_result &&
						f.receiver.typ == m.params[0].typ && f.name == m.name {
						c.file = f.source_file // setup of file path for the warning
						c.warn('mismatched parameters count between vweb method `${sym_app.name}.$m.name` ($nargs) and route attribute $m.attrs ($nroute_attributes)',
							f.pos)
					}
				}
			}
		}
	}
}
