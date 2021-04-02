// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import os
import strings
import time
import v.ast
import v.vmod
import v.token
import v.pref
import v.util
import v.errors
import v.pkgconfig

const int_min = int(0x80000000)

const int_max = int(0x7FFFFFFF)

const (
	valid_comp_if_os        = ['windows', 'ios', 'macos', 'mach', 'darwin', 'hpux', 'gnu', 'qnx',
		'linux', 'freebsd', 'openbsd', 'netbsd', 'bsd', 'dragonfly', 'android', 'solaris', 'haiku',
		'linux_or_macos',
	]
	valid_comp_if_compilers = ['gcc', 'tinyc', 'clang', 'mingw', 'msvc', 'cplusplus']
	valid_comp_if_platforms = ['amd64', 'aarch64', 'x64', 'x32', 'little_endian', 'big_endian']
	valid_comp_if_other     = ['js', 'debug', 'prod', 'test', 'glibc', 'prealloc',
		'no_bounds_checking',
	]
	array_builtin_methods   = ['filter', 'clone', 'repeat', 'reverse', 'map', 'slice', 'sort',
		'contains', 'index', 'wait', 'any', 'all', 'first', 'last', 'pop']
)

pub struct Checker {
	pref &pref.Preferences // Preferences shared from V struct
pub mut:
	table            &ast.Table
	file             &ast.File = 0
	nr_errors        int
	nr_warnings      int
	nr_notices       int
	errors           []errors.Error
	warnings         []errors.Warning
	notices          []errors.Notice
	error_lines      []int // to avoid printing multiple errors for the same line
	expected_type    ast.Type
	expected_or_type ast.Type    // fn() or { 'this type' } eg. string. expected or block type
	cur_fn           &ast.FnDecl // current function
	const_decl       string
	const_deps       []string
	const_names      []string
	global_names     []string
	locked_names     []string // vars that are currently locked
	rlocked_names    []string // vars that are currently read-locked
	in_for_count     int      // if checker is currently in a for loop
	// checked_ident  string // to avoid infinite checker loops
	returns           bool
	scope_returns     bool
	mod               string // current module name
	is_builtin_mod    bool   // are we in `builtin`?
	inside_unsafe     bool
	inside_const      bool
	inside_anon_fn    bool
	inside_ref_lit    bool
	skip_flags        bool // should `#flag` and `#include` be skipped
	cur_generic_types []ast.Type
mut:
	files                            []ast.File
	expr_level                       int  // to avoid infinite recursion segfaults due to compiler bugs
	inside_sql                       bool // to handle sql table fields pseudo variables
	cur_orm_ts                       ast.TypeSymbol
	error_details                    []string
	vmod_file_content                string     // needed for @VMOD_FILE, contents of the file, *NOT its path**
	vweb_gen_types                   []ast.Type // vweb route checks
	prevent_sum_type_unwrapping_once bool       // needed for assign new values to sum type, stopping unwrapping then
	loop_label                       string     // set when inside a labelled for loop
	timers                           &util.Timers = util.new_timers(false)
	comptime_fields_type             map[string]ast.Type
	fn_scope                         &ast.Scope = voidptr(0)
	used_fns                         map[string]bool // used_fns['println'] == true
	main_fn_decl_node                ast.FnDecl
	match_exhaustive_cutoff_limit    int = 10
	// TODO: these are here temporarily and used for deprecations; remove soon
	using_new_err_struct bool
	inside_selector_expr bool
	inside_println_arg   bool
}

pub fn new_checker(table &ast.Table, pref &pref.Preferences) Checker {
	mut timers_should_print := false
	$if time_checking ? {
		timers_should_print = true
	}
	return Checker{
		table: table
		pref: pref
		cur_fn: 0
		timers: util.new_timers(timers_should_print)
		match_exhaustive_cutoff_limit: pref.checker_match_exhaustive_cutoff_limit
	}
}

pub fn (mut c Checker) check(ast_file &ast.File) {
	c.file = ast_file
	for i, ast_import in ast_file.imports {
		for j in 0 .. i {
			if ast_import.mod == ast_file.imports[j].mod {
				c.error('module name `$ast_import.mod` duplicate', ast_import.mod_pos)
			}
		}
	}
	for stmt in ast_file.stmts {
		c.expr_level = 0
		c.stmt(stmt)
	}
	c.check_scope_vars(c.file.scope)
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
	for child in sc.children {
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
	// c.files = ast_files
	mut has_main_mod_file := false
	mut has_main_fn := false
	mut files_from_main_module := []&ast.File{}
	for i in 0 .. ast_files.len {
		file := unsafe { &ast_files[i] }
		c.timers.start('checker_check $file.path')
		c.check(file)
		if file.mod.name == 'main' {
			files_from_main_module << file
			has_main_mod_file = true
			if c.file_has_main_fn(file) {
				has_main_fn = true
			}
		}
		c.timers.show('checker_check $file.path')
	}
	if has_main_mod_file && !has_main_fn && files_from_main_module.len > 0 {
		if c.pref.is_script && !c.pref.is_test {
			// files_from_main_module contain preludes at the start
			mut the_main_file := files_from_main_module.last()
			the_main_file.stmts << ast.FnDecl{
				name: 'main.main'
				mod: 'main'
				is_main: true
				file: the_main_file.path
				return_type: ast.void_type
				scope: &ast.Scope{
					parent: 0
				}
			}
			has_main_fn = true
		}
	}
	c.timers.start('checker_post_process_generic_fns')
	last_file := c.file
	last_mod := c.mod
	// post process generic functions. must be done after all files have been
	// checked, to eunsure all generic calls are processed as this information
	// is needed when the generic type is auto inferred from the call argument
	for i in 0 .. ast_files.len {
		file := unsafe { &ast_files[i] }
		if file.generic_fns.len > 0 {
			c.file = file
			c.mod = file.mod.name
			c.post_process_generic_fns()
		}
	}
	// restore the original c.file && c.mod after post processing
	c.file = last_file
	c.mod = last_mod
	c.timers.show('checker_post_process_generic_fns')
	//
	c.timers.start('checker_verify_all_vweb_routes')
	c.verify_all_vweb_routes()
	c.timers.show('checker_verify_all_vweb_routes')
	//
	if c.pref.is_test {
		mut n_test_fns := 0
		for _, f in c.table.fns {
			if f.is_test {
				n_test_fns++
			}
		}
		if n_test_fns == 0 {
			c.add_error_detail('The name of a test function in V, should start with `test_`.')
			c.add_error_detail('The test function should take 0 parameters, and no return type. Example:')
			c.add_error_detail('fn test_xyz(){ assert 2 + 2 == 4 }')
			c.error('a _test.v file should have *at least* one `test_` function', token.Position{})
		}
	}
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

// do checks specific to files in main module
// returns `true` if a main function is in the file
fn (mut c Checker) file_has_main_fn(file ast.File) bool {
	mut has_main_fn := false
	for stmt in file.stmts {
		if stmt is ast.FnDecl {
			if stmt.name == 'main.main' {
				if has_main_fn {
					c.error('function `main` is already defined', stmt.pos)
				}
				has_main_fn = true
				if stmt.params.len > 0 {
					c.error('function `main` cannot have arguments', stmt.pos)
				}
				if stmt.return_type != ast.void_type {
					c.error('function `main` cannot return values', stmt.pos)
				}
				if stmt.no_body {
					c.error('function `main` must declare a body', stmt.pos)
				}
			} else if stmt.attrs.contains('console') {
				c.error('only `main` can have the `[console]` attribute', stmt.pos)
			}
		}
	}
	return has_main_fn
}

fn (mut c Checker) check_valid_snake_case(name string, identifier string, pos token.Position) {
	if !c.pref.is_vweb && name.len > 0 && (name[0] == `_` || name.contains('._')) {
		c.error('$identifier `$name` cannot start with `_`', pos)
	}
	if !c.pref.experimental && !c.pref.translated && util.contains_capital(name) {
		c.error('$identifier `$name` cannot contain uppercase letters, use snake_case instead',
			pos)
	}
}

fn stripped_name(name string) string {
	idx := name.last_index('.') or { -1 }
	return name[(idx + 1)..]
}

fn (mut c Checker) check_valid_pascal_case(name string, identifier string, pos token.Position) {
	sname := stripped_name(name)
	if sname.len > 0 && !sname[0].is_capital() && !c.pref.translated {
		c.error('$identifier `$name` must begin with capital letter', pos)
	}
}

pub fn (mut c Checker) type_decl(node ast.TypeDecl) {
	match node {
		ast.AliasTypeDecl { c.alias_type_decl(node) }
		ast.FnTypeDecl { c.fn_type_decl(node) }
		ast.SumTypeDecl { c.sum_type_decl(node) }
	}
}

pub fn (mut c Checker) alias_type_decl(node ast.AliasTypeDecl) {
	// TODO Replace `c.file.mod.name != 'time'` by `it.language != .v` once available
	if c.file.mod.name != 'time' && c.file.mod.name != 'builtin' {
		c.check_valid_pascal_case(node.name, 'type alias', node.pos)
	}
	typ_sym := c.table.get_type_symbol(node.parent_type)
	if typ_sym.kind in [.placeholder, .int_literal, .float_literal] {
		c.error("type `$typ_sym.name` doesn't exist", node.pos)
	} else if typ_sym.kind == .alias {
		orig_sym := c.table.get_type_symbol((typ_sym.info as ast.Alias).parent_type)
		c.error('type `$typ_sym.str()` is an alias, use the original alias type `$orig_sym.name` instead',
			node.pos)
	} else if typ_sym.kind == .chan {
		c.error('aliases of `chan` types are not allowed.', node.pos)
	}
}

pub fn (mut c Checker) fn_type_decl(node ast.FnTypeDecl) {
	c.check_valid_pascal_case(node.name, 'fn type', node.pos)
	typ_sym := c.table.get_type_symbol(node.typ)
	fn_typ_info := typ_sym.info as ast.FnType
	fn_info := fn_typ_info.func
	ret_sym := c.table.get_type_symbol(fn_info.return_type)
	if ret_sym.kind == .placeholder {
		c.error("type `$ret_sym.name` doesn't exist", node.pos)
	}
	for arg in fn_info.params {
		arg_sym := c.table.get_type_symbol(arg.typ)
		if arg_sym.kind == .placeholder {
			c.error("type `$arg_sym.name` doesn't exist", node.pos)
		}
	}
}

pub fn (mut c Checker) sum_type_decl(node ast.SumTypeDecl) {
	c.check_valid_pascal_case(node.name, 'sum type', node.pos)
	mut names_used := []string{}
	for variant in node.variants {
		if variant.typ.is_ptr() {
			c.error('sum type cannot hold a reference type', variant.pos)
		}
		mut sym := c.table.get_type_symbol(variant.typ)
		if sym.name in names_used {
			c.error('sum type $node.name cannot hold the type `$sym.name` more than once',
				variant.pos)
		} else if sym.kind in [.placeholder, .int_literal, .float_literal] {
			c.error("type `$sym.name` doesn't exist", variant.pos)
		} else if sym.kind == .interface_ {
			c.error('sum type cannot hold an interface', variant.pos)
		}
		names_used << sym.name
	}
}

pub fn (mut c Checker) interface_decl(decl ast.InterfaceDecl) {
	c.check_valid_pascal_case(decl.name, 'interface name', decl.pos)
	for method in decl.methods {
		c.check_valid_snake_case(method.name, 'method name', method.pos)
		if method.return_type != ast.Type(0) {
			c.ensure_type_exists(method.return_type, method.pos) or { return }
		}
		for param in method.params {
			c.ensure_type_exists(param.typ, param.pos) or { return }
		}
	}
	for i, field in decl.fields {
		c.check_valid_snake_case(field.name, 'field name', field.pos)
		if field.typ != ast.Type(0) {
			c.ensure_type_exists(field.typ, field.pos) or { return }
		}
		for j in 0 .. i {
			if field.name == decl.fields[j].name {
				c.error('field name `$field.name` duplicate', field.pos)
			}
		}
	}
}

pub fn (mut c Checker) struct_decl(mut decl ast.StructDecl) {
	if decl.language == .v && !c.is_builtin_mod {
		c.check_valid_pascal_case(decl.name, 'struct name', decl.pos)
	}
	mut struct_sym := c.table.find_type(decl.name) or { ast.TypeSymbol{} }
	if mut struct_sym.info is ast.Struct {
		for embed in decl.embeds {
			embed_sym := c.table.get_type_symbol(embed.typ)
			if embed_sym.kind != .struct_ {
				c.error('`$embed_sym.name` is not a struct', embed.pos)
			} else {
				info := embed_sym.info as ast.Struct
				if info.is_heap && !embed.typ.is_ptr() {
					struct_sym.info.is_heap = true
				}
			}
		}
		for attr in decl.attrs {
			if attr.name == 'typedef' && decl.language != .c {
				c.error('`typedef` attribute can only be used with C structs', decl.pos)
			}
		}
		for i, field in decl.fields {
			c.ensure_type_exists(field.typ, field.type_pos) or { return }
			if decl.language == .v {
				c.check_valid_snake_case(field.name, 'field name', field.pos)
			}
			sym := c.table.get_type_symbol(field.typ)
			for j in 0 .. i {
				if field.name == decl.fields[j].name {
					c.error('field name `$field.name` duplicate', field.pos)
				}
			}
			if sym.kind == .struct_ {
				info := sym.info as ast.Struct
				if info.is_heap && !field.typ.is_ptr() {
					struct_sym.info.is_heap = true
				}
			}
			if field.has_default_expr {
				c.expected_type = field.typ
				mut field_expr_type := c.expr(field.default_expr)
				if !field.typ.has_flag(.optional) {
					c.check_expr_opt_call(field.default_expr, field_expr_type)
				}
				struct_sym.info.fields[i].default_expr_typ = field_expr_type
				c.check_expected(field_expr_type, field.typ) or {
					if !(sym.kind == .interface_
						&& c.type_implements(field_expr_type, field.typ, field.pos)) {
						c.error('incompatible initializer for field `$field.name`: $err.msg',
							field.default_expr.position())
					}
				}
				// Check for unnecessary inits like ` = 0` and ` = ''`
				if field.typ.is_ptr() {
					continue
				}
				if field.default_expr is ast.IntegerLiteral {
					if field.default_expr.val == '0' {
						c.warn('unnecessary default value of `0`: struct fields are zeroed by default',
							field.default_expr.pos)
					}
				} else if field.default_expr is ast.StringLiteral {
					if field.default_expr.val == '' {
						c.warn("unnecessary default value of '': struct fields are zeroed by default",
							field.default_expr.pos)
					}
				} else if field.default_expr is ast.BoolLiteral {
					if field.default_expr.val == false {
						c.warn('unnecessary default value `false`: struct fields are zeroed by default',
							field.default_expr.pos)
					}
				}
			}
		}
	}
}

pub fn (mut c Checker) struct_init(mut struct_init ast.StructInit) ast.Type {
	// typ := c.table.find_type(struct_init.typ.typ.name) or {
	// c.error('unknown struct: $struct_init.typ.typ.name', struct_init.pos)
	// panic('')
	// }
	if struct_init.typ == ast.void_type {
		// Short syntax `({foo: bar})`
		if c.expected_type == ast.void_type {
			c.error('unexpected short struct syntax', struct_init.pos)
			return ast.void_type
		}
		sym := c.table.get_type_symbol(c.expected_type)
		if sym.kind == .array {
			struct_init.typ = c.table.value_type(c.expected_type)
		} else {
			struct_init.typ = c.expected_type
		}
	}
	utyp := c.unwrap_generic(struct_init.typ)
	c.ensure_type_exists(utyp, struct_init.pos) or {}
	type_sym := c.table.get_type_symbol(utyp)
	if !c.inside_unsafe && type_sym.kind == .sum_type {
		c.note('direct sum type init (`x := SumType{}`) will be removed soon', struct_init.pos)
	}
	// Make sure the first letter is capital, do not allow e.g. `x := string{}`,
	// but `x := T{}` is ok.
	if !c.is_builtin_mod && !c.inside_unsafe && type_sym.language == .v
		&& c.cur_generic_types.len == 0 {
		pos := type_sym.name.last_index('.') or { -1 }
		first_letter := type_sym.name[pos + 1]
		if !first_letter.is_capital() {
			c.error('cannot initialize builtin type `$type_sym.name`', struct_init.pos)
		}
	}
	if type_sym.kind == .sum_type && struct_init.fields.len == 1 {
		sexpr := struct_init.fields[0].expr.str()
		c.error('cast to sum type using `${type_sym.name}($sexpr)` not `$type_sym.name{$sexpr}`',
			struct_init.pos)
	}
	if type_sym.kind == .interface_ {
		c.error('cannot instantiate interface `$type_sym.name`', struct_init.pos)
	}
	if type_sym.info is ast.Alias {
		if type_sym.info.parent_type.is_number() {
			c.error('cannot instantiate number type alias `$type_sym.name`', struct_init.pos)
			return ast.void_type
		}
	}
	// allow init structs from generic if they're private except the type is from builtin module
	if !type_sym.is_public && type_sym.kind != .placeholder && type_sym.language != .c
		&& (type_sym.mod != c.mod && !(struct_init.typ.has_flag(.generic)
		&& type_sym.mod != 'builtin')) {
		c.error('type `$type_sym.name` is private', struct_init.pos)
	}
	if type_sym.kind == .struct_ {
		info := type_sym.info as ast.Struct
		if info.attrs.len > 0 && info.attrs[0].name == 'noinit' && type_sym.mod != c.mod {
			c.error('struct `$type_sym.name` is declared with a `[noinit]` attribute, so ' +
				'it cannot be initialized with `$type_sym.name{}`', struct_init.pos)
		}
		if info.is_heap && !c.inside_ref_lit && !c.inside_unsafe && !struct_init.typ.is_ptr() {
			c.error('`$type_sym.name` type can only be used as a reference `&$type_sym.name` or inside a `struct` reference',
				struct_init.pos)
		}
	}
	if type_sym.name.len == 1 && c.cur_fn.generic_params.len == 0 {
		c.error('unknown struct `$type_sym.name`', struct_init.pos)
		return 0
	}
	match type_sym.kind {
		.placeholder {
			c.error('unknown struct: $type_sym.name', struct_init.pos)
			return ast.void_type
		}
		// string & array are also structs but .kind of string/array
		.struct_, .string, .array, .alias {
			mut info := ast.Struct{}
			if type_sym.kind == .alias {
				info_t := type_sym.info as ast.Alias
				sym := c.table.get_type_symbol(info_t.parent_type)
				if sym.kind == .placeholder { // pending import symbol did not resolve
					c.error('unknown struct: $type_sym.name', struct_init.pos)
					return ast.void_type
				}
				if sym.kind != .struct_ {
					c.error('alias type name: $sym.name is not struct type', struct_init.pos)
				}
				info = sym.info as ast.Struct
			} else {
				info = type_sym.info as ast.Struct
			}
			if struct_init.is_short {
				exp_len := info.fields.len
				got_len := struct_init.fields.len
				if exp_len != got_len {
					amount := if exp_len < got_len { 'many' } else { 'few' }
					c.error('too $amount fields in `$type_sym.name` literal (expecting $exp_len, got $got_len)',
						struct_init.pos)
				}
			}
			mut inited_fields := []string{}
			for i, field in struct_init.fields {
				mut info_field := ast.StructField{}
				mut embed_type := ast.Type(0)
				mut is_embed := false
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
					if !exists {
						for embed in info.embeds {
							embed_sym := c.table.get_type_symbol(embed)
							if embed_sym.embed_name() == field_name {
								exists = true
								embed_type = embed
								is_embed = true
								break
							}
						}
					}
					if !exists {
						c.error('unknown field `$field.name` in struct literal of type `$type_sym.name`',
							field.pos)
						continue
					}
					if field_name in inited_fields {
						c.error('duplicate field name in struct literal: `$field_name`',
							field.pos)
						continue
					}
				}
				if is_embed {
					c.expected_type = embed_type
					expr_type := c.expr(field.expr)
					expr_type_sym := c.table.get_type_symbol(expr_type)
					if expr_type != ast.void_type && expr_type_sym.kind != .placeholder {
						c.check_expected(expr_type, embed_type) or {
							c.error('cannot assign to field `$info_field.name`: $err.msg',
								field.pos)
						}
					}
					struct_init.fields[i].typ = expr_type
					struct_init.fields[i].expected_type = embed_type
				} else {
					inited_fields << field_name
					field_type_sym := c.table.get_type_symbol(info_field.typ)
					c.expected_type = info_field.typ
					mut expr_type := c.expr(field.expr)
					if !info_field.typ.has_flag(.optional) {
						expr_type = c.check_expr_opt_call(field.expr, expr_type)
					}
					expr_type_sym := c.table.get_type_symbol(expr_type)
					if field_type_sym.kind == .interface_ {
						c.type_implements(expr_type, info_field.typ, field.pos)
					} else if expr_type != ast.void_type && expr_type_sym.kind != .placeholder {
						c.check_expected(expr_type, info_field.typ) or {
							c.error('cannot assign to field `$info_field.name`: $err.msg',
								field.pos)
						}
					}
					if info_field.typ.has_flag(.shared_f) {
						if !expr_type.has_flag(.shared_f) && expr_type.is_ptr() {
							c.error('`shared` field must be initialized with `shared` or value',
								field.pos)
						}
					} else {
						if info_field.typ.is_ptr() && !expr_type.is_ptr() && !expr_type.is_pointer()
							&& !expr_type.is_number() {
							c.error('reference field must be initialized with reference',
								field.pos)
						}
					}
					struct_init.fields[i].typ = expr_type
					struct_init.fields[i].expected_type = info_field.typ
				}
			}
			// Check uninitialized refs/sum types
			for field in info.fields {
				if field.has_default_expr || field.name in inited_fields {
					continue
				}
				if field.typ.is_ptr() && !struct_init.has_update_expr && !c.pref.translated {
					c.error('reference field `${type_sym.name}.$field.name` must be initialized',
						struct_init.pos)
				}
				// Do not allow empty uninitialized sum types
				/*
				sym := c.table.get_type_symbol(field.typ)
				if sym.kind == .sum_type {
					c.warn('sum type field `${type_sym.name}.$field.name` must be initialized',
						struct_init.pos)
				}
				*/
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
						c.error('field `${type_sym.name}.$field.name` must be initialized',
							struct_init.pos)
					}
				}
			}
		}
		else {}
	}
	if struct_init.has_update_expr {
		update_type := c.expr(struct_init.update_expr)
		struct_init.update_expr_type = update_type
		if c.table.type_kind(update_type) != .struct_ {
			s := c.table.type_to_str(update_type)
			c.error('expected struct, found `$s`', struct_init.update_expr.position())
		} else if update_type != struct_init.typ {
			from_sym := c.table.get_type_symbol(update_type)
			to_sym := c.table.get_type_symbol(struct_init.typ)
			from_info := from_sym.info as ast.Struct
			to_info := to_sym.info as ast.Struct
			// TODO this check is too strict
			if !c.check_struct_signature(from_info, to_info) {
				c.error('struct `$from_sym.name` is not compatible with struct `$to_sym.name`',
					struct_init.update_expr.position())
			}
		}
		if !struct_init.update_expr.is_lvalue() {
			// cgen will repeat `update_expr` for each field
			// so enforce an lvalue for efficiency
			c.error('expression is not an lvalue', struct_init.update_expr.position())
		}
	}
	return struct_init.typ
}

fn (mut c Checker) check_div_mod_by_zero(expr ast.Expr, op_kind token.Kind) {
	match mut expr {
		ast.FloatLiteral {
			if expr.val.f64() == 0.0 {
				oper := if op_kind == .div { 'division' } else { 'modulo' }
				c.error('$oper by zero', expr.pos)
			}
		}
		ast.IntegerLiteral {
			if expr.val.int() == 0 {
				oper := if op_kind == .div { 'division' } else { 'modulo' }
				c.error('$oper by zero', expr.pos)
			}
		}
		ast.CastExpr {
			c.check_div_mod_by_zero(expr.expr, op_kind)
		}
		else {}
	}
}

pub fn (mut c Checker) infix_expr(mut infix_expr ast.InfixExpr) ast.Type {
	// println('checker: infix expr(op $infix_expr.op.str())')
	former_expected_type := c.expected_type
	defer {
		c.expected_type = former_expected_type
	}
	left_type := c.expr(infix_expr.left)
	// left_type = c.unwrap_genric(c.expr(infix_expr.left))
	infix_expr.left_type = left_type
	c.expected_type = left_type
	right_type := c.expr(infix_expr.right)
	// right_type = c.unwrap_genric(c.expr(infix_expr.right))
	infix_expr.right_type = right_type
	mut right := c.table.get_type_symbol(right_type)
	right_final := c.table.get_final_type_symbol(right_type)
	mut left := c.table.get_type_symbol(left_type)
	left_final := c.table.get_final_type_symbol(left_type)
	left_pos := infix_expr.left.position()
	right_pos := infix_expr.right.position()
	left_right_pos := left_pos.extend(right_pos)
	if (left_type.is_ptr() || left.is_pointer()) && infix_expr.op in [.plus, .minus] {
		if !c.inside_unsafe && !infix_expr.left.is_auto_deref_var()
			&& !infix_expr.right.is_auto_deref_var() {
			c.warn('pointer arithmetic is only allowed in `unsafe` blocks', left_pos)
		}
		if left_type == ast.voidptr_type {
			c.error('`$infix_expr.op` cannot be used with `voidptr`', left_pos)
		}
	}
	mut return_type := left_type
	if infix_expr.op != .key_is {
		match mut infix_expr.left {
			ast.Ident, ast.SelectorExpr {
				if infix_expr.left.is_mut {
					c.error('remove unnecessary `mut`', infix_expr.left.mut_pos)
				}
			}
			else {}
		}
	}
	eq_ne := infix_expr.op in [.eq, .ne]
	// Single side check
	// Place these branches according to ops' usage frequency to accelerate.
	// TODO: First branch includes ops where single side check is not needed, or needed but hasn't been implemented.
	// TODO: Some of the checks are not single side. Should find a better way to organize them.
	match infix_expr.op {
		// .eq, .ne, .gt, .lt, .ge, .le, .and, .logical_or, .dot, .key_as, .right_shift {}
		.eq, .ne {
			is_mismatch := (left.kind == .alias && right.kind in [.struct_, .array, .sum_type])
				|| (right.kind == .alias && left.kind in [.struct_, .array, .sum_type])
			if is_mismatch {
				c.error('possible type mismatch of compared values of `$infix_expr.op` operation',
					left_right_pos)
			}
		}
		.key_in, .not_in {
			match right.kind {
				.array {
					elem_type := right.array_info().elem_type
					// if left_default.kind != right_sym.kind {
					c.check_expected(left_type, elem_type) or {
						c.error('left operand to `$infix_expr.op` does not match the array element type: $err.msg',
							left_right_pos)
					}
				}
				.map {
					map_info := right.map_info()
					c.check_expected(left_type, map_info.key_type) or {
						c.error('left operand to `$infix_expr.op` does not match the map key type: $err.msg',
							left_right_pos)
					}
					infix_expr.left_type = map_info.key_type
				}
				.string {
					c.warn('use `str.contains(substr)` instead of `substr in str`', left_right_pos)
					c.check_expected(left_type, right_type) or {
						c.error('left operand to `$infix_expr.op` does not match: $err.msg',
							left_right_pos)
					}
				}
				else {
					c.error('`$infix_expr.op.str()` can only be used with an array/map/string',
						infix_expr.pos)
				}
			}
			return ast.bool_type
		}
		.plus, .minus, .mul, .div, .mod, .xor, .amp, .pipe { // binary operators that expect matching types
			if right.info is ast.Alias && (right.info as ast.Alias).language != .c
				&& c.mod == c.table.type_to_str(right_type).split('.')[0]
				&& c.table.get_type_symbol((right.info as ast.Alias).parent_type).is_primitive() {
				right = c.table.get_type_symbol((right.info as ast.Alias).parent_type)
			}
			if left.info is ast.Alias && (left.info as ast.Alias).language != .c
				&& c.mod == c.table.type_to_str(left_type).split('.')[0]
				&& c.table.get_type_symbol((left.info as ast.Alias).parent_type).is_primitive() {
				left = c.table.get_type_symbol((left.info as ast.Alias).parent_type)
			}
			// Check if the alias type is not a primitive then allow using operator overloading for aliased `arrays` and `maps`
			if left.kind == .alias && left.info is ast.Alias
				&& !(c.table.get_type_symbol((left.info as ast.Alias).parent_type).is_primitive()) {
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
						c.error('undefined operation `$left_name` $infix_expr.op.str() `$right_name`',
							left_right_pos)
					} else {
						c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
					}
				}
			} else if right.kind == .alias && right.info is ast.Alias
				&& !(c.table.get_type_symbol((right.info as ast.Alias).parent_type).is_primitive()) {
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
						c.error('undefined operation `$left_name` $infix_expr.op.str() `$right_name`',
							left_right_pos)
					} else {
						c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
					}
				}
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
						c.error('undefined operation `$left_name` $infix_expr.op.str() `$right_name`',
							left_right_pos)
					} else {
						c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
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
						c.error('undefined operation `$left_name` $infix_expr.op.str() `$right_name`',
							left_right_pos)
					} else {
						c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
					}
				}
			} else {
				promoted_type := c.promote(c.table.unalias_num_type(left_type), c.table.unalias_num_type(right_type))
				if promoted_type.idx() == ast.void_type_idx {
					left_name := c.table.type_to_str(left_type)
					right_name := c.table.type_to_str(right_type)
					c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
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
							c.error('$side type of `$infix_expr.op.str()` cannot be non-integer type `$name`',
								pos)
						}
					}
				}
				if infix_expr.op in [.div, .mod] {
					c.check_div_mod_by_zero(infix_expr.right, infix_expr.op)
				}
				return_type = promoted_type
			}
		}
		.gt, .lt, .ge, .le {
			if left.kind in [.array, .array_fixed] && right.kind in [.array, .array_fixed] {
				c.error('only `==` and `!=` are defined on arrays', infix_expr.pos)
			} else if left.kind == .struct_ && right.kind == .struct_ && infix_expr.op in [.eq, .lt] {
				if !(left.has_method(infix_expr.op.str()) && right.has_method(infix_expr.op.str())) {
					left_name := c.table.type_to_str(left_type)
					right_name := c.table.type_to_str(right_type)
					if left_name == right_name {
						c.error('undefined operation `$left_name` $infix_expr.op.str() `$right_name`',
							left_right_pos)
					} else {
						c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
					}
				}
			}
			if left.kind == .struct_ && right.kind == .struct_ {
				if !left.has_method('<') && infix_expr.op in [.ge, .le] {
					c.error('cannot use `$infix_expr.op` as `<` operator method is not defined',
						left_right_pos)
				} else if !left.has_method('<') && infix_expr.op == .gt {
					c.error('cannot use `>` as `<=` operator method is not defined', left_right_pos)
				}
			}
		}
		.left_shift {
			if left_final.kind == .array {
				if !infix_expr.is_stmt {
					c.error('array append cannot be used in an expression', infix_expr.pos)
				}
				// `array << elm`
				c.check_expr_opt_call(infix_expr.right, right_type)
				infix_expr.auto_locked, _ = c.fail_if_immutable(infix_expr.left)
				left_value_type := c.table.value_type(left_type)
				left_value_sym := c.table.get_type_symbol(left_value_type)
				if left_value_sym.kind == .interface_ {
					if right_final.kind != .array {
						// []Animal << Cat
						c.type_implements(right_type, left_value_type, right_pos)
					} else {
						// []Animal << []Cat
						c.type_implements(c.table.value_type(right_type), left_value_type,
							right_pos)
					}
					return ast.void_type
				}
				// the expressions have different types (array_x and x)
				if c.check_types(right_type, left_value_type) { // , right_type) {
					// []T << T
					return ast.void_type
				}
				if right_final.kind == .array
					&& c.check_types(left_value_type, c.table.value_type(right_type)) {
					// []T << []T
					return ast.void_type
				}
				c.error('cannot append `$right.name` to `$left.name`', right_pos)
				return ast.void_type
			} else {
				return c.check_shift(left_type, right_type, left_pos, right_pos)
			}
		}
		.right_shift {
			return c.check_shift(left_type, right_type, left_pos, right_pos)
		}
		.key_is, .not_is {
			right_expr := infix_expr.right
			mut typ := match right_expr {
				ast.TypeNode {
					right_expr.typ
				}
				ast.None {
					ast.none_type_idx
				}
				else {
					c.error('invalid type `$right_expr`', right_expr.position())
					ast.Type(0)
				}
			}
			typ_sym := c.table.get_type_symbol(typ)
			op := infix_expr.op.str()
			if typ_sym.kind == .placeholder {
				c.error('$op: type `$typ_sym.name` does not exist', right_expr.position())
			}
			if left.kind !in [.interface_, .sum_type] {
				c.error('`$op` can only be used with interfaces and sum types', infix_expr.pos)
			} else if mut left.info is ast.SumType {
				if typ !in left.info.variants {
					c.error('`$left.name` has no variant `$right.name`', infix_expr.pos)
				}
			}
			return ast.bool_type
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
					c.error('cannot push non-reference `$right.name` on `$left.name`',
						right_pos)
				}
				c.stmts(infix_expr.or_block.stmts)
			} else {
				c.error('cannot push on non-channel `$left.name`', left_pos)
			}
			return ast.void_type
		}
		.and, .logical_or {
			if infix_expr.left_type != ast.bool_type_idx {
				c.error('left operand for `$infix_expr.op` is not a boolean', infix_expr.left.position())
			}
			if infix_expr.right_type != ast.bool_type_idx {
				c.error('right operand for `$infix_expr.op` is not a boolean', infix_expr.right.position())
			}
			// use `()` to make the boolean expression clear error
			// for example: `(a && b) || c` instead of `a && b || c`
			if mut infix_expr.left is ast.InfixExpr {
				if infix_expr.left.op != infix_expr.op && infix_expr.left.op in [.logical_or, .and] {
					c.error('use `()` to make the boolean expression clear', infix_expr.pos)
				}
			}
		}
		else {}
	}
	// TODO: Absorb this block into the above single side check block to accelerate.
	if left_type == ast.bool_type && infix_expr.op !in [.eq, .ne, .logical_or, .and] {
		c.error('bool types only have the following operators defined: `==`, `!=`, `||`, and `&&`',
			infix_expr.pos)
	} else if left_type == ast.string_type
		&& infix_expr.op !in [.plus, .eq, .ne, .lt, .gt, .le, .ge] {
		// TODO broken !in
		c.error('string types only have the following operators defined: `==`, `!=`, `<`, `>`, `<=`, `>=`, and `+`',
			infix_expr.pos)
	} else if left.kind == .enum_ && right.kind == .enum_ && !eq_ne {
		left_enum := left.info as ast.Enum
		right_enum := right.info as ast.Enum
		if left_enum.is_flag && right_enum.is_flag {
			// `[flag]` tagged enums are a special case that allow also `|` and `&` binary operators
			if infix_expr.op !in [.pipe, .amp] {
				c.error('only `==`, `!=`, `|` and `&` are defined on `[flag]` tagged `enum`, use an explicit cast to `int` if needed',
					infix_expr.pos)
			}
		} else {
			// Regular enums
			c.error('only `==` and `!=` are defined on `enum`, use an explicit cast to `int` if needed',
				infix_expr.pos)
		}
	}
	// sum types can't have any infix operation except of `is`, `eq`, `ne`.
	// `is` is checked before and doesn't reach this.
	if c.table.type_kind(left_type) == .sum_type && !eq_ne {
		c.error('cannot use operator `$infix_expr.op` with `$left.name`', infix_expr.pos)
	} else if c.table.type_kind(right_type) == .sum_type && !eq_ne {
		c.error('cannot use operator `$infix_expr.op` with `$right.name`', infix_expr.pos)
	}
	// TODO move this to symmetric_check? Right now it would break `return 0` for `fn()?int `
	left_is_optional := left_type.has_flag(.optional)
	right_is_optional := right_type.has_flag(.optional)
	if (left_is_optional && !right_is_optional) || (!left_is_optional && right_is_optional) {
		c.error('unwrapped optional cannot be used in an infix expression', left_right_pos)
	}
	// Dual sides check (compatibility check)
	if !c.symmetric_check(right_type, left_type) && !c.pref.translated {
		// for type-unresolved consts
		if left_type == ast.void_type || right_type == ast.void_type {
			return ast.void_type
		}
		c.error('infix expr: cannot use `$right.name` (right expression) as `$left.name`',
			left_right_pos)
	}
	/*
	if (infix_expr.left is ast.InfixExpr &&
		(infix_expr.left as ast.InfixExpr).op == .inc) ||
		(infix_expr.right is ast.InfixExpr && (infix_expr.right as ast.InfixExpr).op == .inc) {
		c.warn('`++` and `--` are statements, not expressions', infix_expr.pos)
	}
	*/
	return if infix_expr.op.is_relational() { ast.bool_type } else { return_type }
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
		ast.ComptimeSelector {
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
						if c.locked_names.len > 0 || c.rlocked_names.len > 0 {
							if expr.name in c.rlocked_names {
								c.error('$expr.name has an `rlock` but needs a `lock`',
									expr.pos)
							} else {
								c.error('$expr.name must be added to the `lock` list above',
									expr.pos)
							}
						}
						to_lock = expr.name
						pos = expr.pos
					}
				}
			} else if expr.obj is ast.ConstField && expr.name in c.const_names {
				c.error('cannot modify constant `$expr.name`', expr.pos)
			}
		}
		ast.IndexExpr {
			left_sym := c.table.get_type_symbol(expr.left_type)
			mut elem_type := ast.Type(0)
			mut kind := ''
			match left_sym.info {
				ast.Array {
					elem_type, kind = left_sym.info.elem_type, 'array'
				}
				ast.ArrayFixed {
					elem_type, kind = left_sym.info.elem_type, 'fixed array'
				}
				ast.Map {
					elem_type, kind = left_sym.info.value_type, 'map'
				}
				else {}
			}
			if elem_type.has_flag(.shared_f) {
				c.error('you have to create a handle and `lock` it to modify `shared` $kind element',
					expr.left.position().extend(expr.pos))
			}
			to_lock, pos = c.fail_if_immutable(expr.left)
		}
		ast.ParExpr {
			to_lock, pos = c.fail_if_immutable(expr.expr)
		}
		ast.PrefixExpr {
			to_lock, pos = c.fail_if_immutable(expr.right)
		}
		ast.SelectorExpr {
			if expr.expr_type == 0 {
				return '', pos
			}
			// retrieve ast.Field
			c.ensure_type_exists(expr.expr_type, expr.pos) or { return '', pos }
			mut typ_sym := c.table.get_final_type_symbol(c.unwrap_generic(expr.expr_type))
			match typ_sym.kind {
				.struct_ {
					struct_info := typ_sym.info as ast.Struct
					mut has_field := true
					mut field_info := struct_info.find_field(expr.field_name) or {
						has_field = false
						ast.StructField{}
					}
					if !has_field {
						for embed in struct_info.embeds {
							embed_sym := c.table.get_type_symbol(embed)
							embed_struct_info := embed_sym.info as ast.Struct
							if embed_field_info := embed_struct_info.find_field(expr.field_name) {
								has_field = true
								field_info = embed_field_info
								break
							}
						}
					}
					if !has_field {
						type_str := c.table.type_to_str(expr.expr_type)
						c.error('unknown field `${type_str}.$expr.field_name`', expr.pos)
						return '', pos
					}
					if !field_info.is_mut && !c.pref.translated {
						type_str := c.table.type_to_str(expr.expr_type)
						c.error('field `$expr.field_name` of struct `$type_str` is immutable',
							expr.pos)
					}
					if field_info.typ.has_flag(.shared_f) {
						type_str := c.table.type_to_str(expr.expr_type)
						c.error('you have to create a handle and `lock` it to modify `shared` field `$expr.field_name` of struct `$type_str`',
							expr.pos)
					}
					to_lock, pos = c.fail_if_immutable(expr.expr)
					if to_lock != '' {
						// No automatic lock for struct access
						explicit_lock_needed = true
					}
				}
				.interface_ {
					interface_info := typ_sym.info as ast.Interface
					mut field_info := interface_info.find_field(expr.field_name) or {
						type_str := c.table.type_to_str(expr.expr_type)
						c.error('unknown field `${type_str}.$expr.field_name`', expr.pos)
						return '', pos
					}
					if !field_info.is_mut {
						type_str := c.table.type_to_str(expr.expr_type)
						c.error('field `$expr.field_name` of interface `$type_str` is immutable',
							expr.pos)
					}
					c.fail_if_immutable(expr.expr)
				}
				.array, .string {
					// This should only happen in `builtin`
					// TODO Remove `crypto.rand` when possible (see vlib/crypto/rand/rand.v,
					// if `c_array_to_bytes_tmp` doesn't exist, then it's safe to remove it)
					if c.file.mod.name !in ['builtin', 'crypto.rand'] {
						c.error('`$typ_sym.kind` can not be modified', expr.pos)
					}
				}
				.aggregate {
					c.fail_if_immutable(expr.expr)
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
			}
		}
		ast.ArrayInit {
			c.error('array literal can not be modified', expr.pos)
			return '', pos
		}
		ast.StructInit {
			return '', pos
		}
		else {
			c.error('unexpected expression `$expr.type_name()`', expr.position())
		}
	}
	if explicit_lock_needed {
		c.error('`$to_lock` is `shared` and needs explicit lock for `$expr.type_name()`',
			pos)
		to_lock = ''
	}
	return to_lock, pos
}

pub fn (mut c Checker) call_expr(mut call_expr ast.CallExpr) ast.Type {
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
	free_tmp_arg_vars := c.pref.autofree && !c.is_builtin_mod && call_expr.args.len > 0
		&& !call_expr.args[0].typ.has_flag(.optional)
	if free_tmp_arg_vars && !c.inside_const {
		for i, arg in call_expr.args {
			if arg.typ != ast.string_type {
				continue
			}
			if arg.expr is ast.Ident || arg.expr is ast.StringLiteral
				|| arg.expr is ast.SelectorExpr {
				// Simple expressions like variables, string literals, selector expressions
				// (`x.field`) can't result in allocations and don't need to be assigned to
				// temporary vars.
				// Only expressions like `str + 'b'` need to be freed.
				continue
			}
			call_expr.args[i].is_tmp_autofree = true
		}
		// TODO copy pasta from above
		if call_expr.receiver_type == ast.string_type && !(call_expr.left is ast.Ident
			|| call_expr.left is ast.StringLiteral
			|| call_expr.left is ast.SelectorExpr) {
			call_expr.free_receiver = true
		}
	}
	c.expected_or_type = call_expr.return_type.clear_flag(.optional)
	c.stmts(call_expr.or_block.stmts)
	c.expected_or_type = ast.void_type
	if call_expr.or_block.kind == .propagate && !c.cur_fn.return_type.has_flag(.optional)
		&& !c.inside_const {
		if !c.cur_fn.is_main {
			c.error('to propagate the optional call, `$c.cur_fn.name` must return an optional',
				call_expr.or_block.pos)
		}
	}
	return typ
}

fn (mut c Checker) check_map_and_filter(is_map bool, elem_typ ast.Type, call_expr ast.CallExpr) {
	if call_expr.args.len != 1 {
		c.error('expected 1 argument, but got $call_expr.args.len', call_expr.pos)
		// Finish early so that it doesn't fail later
		return
	}
	elem_sym := c.table.get_type_symbol(elem_typ)
	arg_expr := call_expr.args[0].expr
	match arg_expr {
		ast.AnonFn {
			if arg_expr.decl.params.len > 1 {
				c.error('function needs exactly 1 argument', arg_expr.decl.pos)
			} else if is_map && (arg_expr.decl.return_type == ast.void_type
				|| arg_expr.decl.params[0].typ != elem_typ) {
				c.error('type mismatch, should use `fn(a $elem_sym.name) T {...}`', arg_expr.decl.pos)
			} else if !is_map && (arg_expr.decl.return_type != ast.bool_type
				|| arg_expr.decl.params[0].typ != elem_typ) {
				c.error('type mismatch, should use `fn(a $elem_sym.name) bool {...}`',
					arg_expr.decl.pos)
			}
		}
		ast.Ident {
			if arg_expr.kind == .function {
				func := c.table.find_fn(arg_expr.name) or {
					c.error('$arg_expr.name does not exist', arg_expr.pos)
					return
				}
				if func.params.len > 1 {
					c.error('function needs exactly 1 argument', call_expr.pos)
				} else if is_map
					&& (func.return_type == ast.void_type || func.params[0].typ != elem_typ) {
					c.error('type mismatch, should use `fn(a $elem_sym.name) T {...}`',
						arg_expr.pos)
				} else if !is_map
					&& (func.return_type != ast.bool_type || func.params[0].typ != elem_typ) {
					c.error('type mismatch, should use `fn(a $elem_sym.name) bool {...}`',
						arg_expr.pos)
				}
			} else if arg_expr.kind == .variable {
				if arg_expr.obj is ast.Var {
					expr := arg_expr.obj.expr
					if expr is ast.AnonFn {
						// copied from above
						if expr.decl.params.len > 1 {
							c.error('function needs exactly 1 argument', expr.decl.pos)
						} else if is_map && (expr.decl.return_type == ast.void_type
							|| expr.decl.params[0].typ != elem_typ) {
							c.error('type mismatch, should use `fn(a $elem_sym.name) T {...}`',
								expr.decl.pos)
						} else if !is_map && (expr.decl.return_type != ast.bool_type
							|| expr.decl.params[0].typ != elem_typ) {
							c.error('type mismatch, should use `fn(a $elem_sym.name) bool {...}`',
								expr.decl.pos)
						}
						return
					}
				}
				if !is_map && arg_expr.info.typ != ast.bool_type {
					c.error('type mismatch, should be bool', arg_expr.pos)
				}
			}
		}
		ast.CallExpr {
			if is_map && arg_expr.return_type == ast.void_type {
				c.error('type mismatch, `$arg_expr.name` does not return anything', arg_expr.pos)
			} else if !is_map && arg_expr.return_type != ast.bool_type {
				c.error('type mismatch, `$arg_expr.name` must return a bool', arg_expr.pos)
			}
		}
		else {}
	}
}

pub fn (mut c Checker) call_method(mut call_expr ast.CallExpr) ast.Type {
	left_type := c.expr(call_expr.left)
	c.expected_type = left_type
	is_generic := left_type.has_flag(.generic)
	call_expr.left_type = left_type
	// Set default values for .return_type & .receiver_type too,
	// or there will be hard to diagnose 0 type panics in cgen.
	call_expr.return_type = left_type
	call_expr.receiver_type = left_type
	left_type_sym := c.table.get_type_symbol(c.unwrap_generic(left_type))
	method_name := call_expr.name
	mut unknown_method_msg := 'unknown method: `${left_type_sym.name}.$method_name`'
	if left_type.has_flag(.optional) {
		c.error('optional type cannot be called directly', call_expr.left.position())
		return ast.void_type
	}
	if left_type_sym.kind in [.sum_type, .interface_] && method_name == 'type_name' {
		return ast.string_type
	}
	mut has_generic_generic := false // x.foo<T>() instead of x.foo<int>()
	mut generic_types := []ast.Type{}
	for generic_type in call_expr.generic_types {
		if generic_type.has_flag(.generic) {
			has_generic_generic = true
			generic_types << c.unwrap_generic(generic_type)
		} else {
			generic_types << generic_type
		}
	}
	if has_generic_generic {
		if c.mod != '' {
			// Need to prepend the module when adding a generic type to a function
			c.table.register_fn_gen_type(c.mod + '.' + call_expr.name, generic_types)
		} else {
			c.table.register_fn_gen_type(call_expr.name, generic_types)
		}
	}
	// TODO: remove this for actual methods, use only for compiler magic
	// FIXME: Argument count != 1 will break these
	if left_type_sym.kind == .array && method_name in checker.array_builtin_methods {
		return c.call_array_builtin_method(mut call_expr, left_type, left_type_sym)
	} else if left_type_sym.kind == .map && method_name in ['clone', 'keys', 'move'] {
		return c.call_map_builtin_method(mut call_expr, left_type, left_type_sym)
	} else if left_type_sym.kind == .array && method_name in ['insert', 'prepend'] {
		info := left_type_sym.info as ast.Array
		arg_expr := if method_name == 'insert' {
			call_expr.args[1].expr
		} else {
			call_expr.args[0].expr
		}
		arg_type := c.expr(arg_expr)
		arg_sym := c.table.get_type_symbol(arg_type)
		if !c.check_types(arg_type, info.elem_type) && !c.check_types(left_type, arg_type) {
			c.error('cannot $method_name `$arg_sym.name` to `$left_type_sym.name`', arg_expr.position())
		}
	} else if left_type_sym.kind == .thread && method_name == 'wait' {
		info := left_type_sym.info as ast.Thread
		if call_expr.args.len > 0 {
			c.error('wait() does not have any arguments', call_expr.args[0].pos)
		}
		call_expr.return_type = info.return_type
		return info.return_type
	}
	mut method := ast.Fn{}
	mut has_method := false
	mut is_method_from_embed := false
	if m := c.table.type_find_method(left_type_sym, method_name) {
		method = m
		has_method = true
	} else {
		// can this logic be moved to ast.type_find_method() so it can be used from anywhere
		if left_type_sym.info is ast.Struct {
			mut found_methods := []ast.Fn{}
			mut embed_of_found_methods := []ast.Type{}
			for embed in left_type_sym.info.embeds {
				embed_sym := c.table.get_type_symbol(embed)
				if m := c.table.type_find_method(embed_sym, method_name) {
					found_methods << m
					embed_of_found_methods << embed
				}
			}
			if found_methods.len == 1 {
				method = found_methods[0]
				has_method = true
				is_method_from_embed = true
				call_expr.from_embed_type = embed_of_found_methods[0]
			} else if found_methods.len > 1 {
				c.error('ambiguous method `$method_name`', call_expr.pos)
			}
		}
		if left_type_sym.kind == .aggregate {
			// the error message contains the problematic type
			unknown_method_msg = err.msg
		}
	}
	if has_method {
		if !method.is_pub && !c.pref.is_test && method.mod != c.mod {
			// If a private method is called outside of the module
			// its receiver type is defined in, show an error.
			// println('warn $method_name lef.mod=$left_type_sym.mod c.mod=$c.mod')
			c.error('method `${left_type_sym.name}.$method_name` is private', call_expr.pos)
		}
		rec_share := method.params[0].typ.share()
		if rec_share == .shared_t && (c.locked_names.len > 0 || c.rlocked_names.len > 0) {
			c.error('method with `shared` receiver cannot be called inside `lock`/`rlock` block',
				call_expr.pos)
		}
		if method.params[0].is_mut {
			to_lock, pos := c.fail_if_immutable(call_expr.left)
			// call_expr.is_mut = true
			if to_lock != '' && rec_share != .shared_t {
				c.error('$to_lock is `shared` and must be `lock`ed to be passed as `mut`',
					pos)
			}
		} else {
			c.fail_if_unreadable(call_expr.left, left_type, 'receiver')
		}
		if (!left_type_sym.is_builtin() && method.mod != 'builtin') && method.language == .v
			&& method.no_body {
			c.error('cannot call a method that does not have a body', call_expr.pos)
		}
		if method.return_type == ast.void_type && method.ctdefine.len > 0
			&& method.ctdefine !in c.pref.compile_defines {
			call_expr.should_be_skipped = true
		}
		nr_args := if method.params.len == 0 { 0 } else { method.params.len - 1 }
		min_required_args := method.params.len - if method.is_variadic && method.params.len > 1 {
			2
		} else {
			1
		}
		if call_expr.args.len < min_required_args {
			c.error('expected $min_required_args arguments, but got $call_expr.args.len',
				call_expr.pos)
		} else if !method.is_variadic && call_expr.args.len > nr_args {
			unexpected_arguments := call_expr.args[min_required_args..]
			unexpected_arguments_pos := unexpected_arguments[0].pos.extend(unexpected_arguments.last().pos)
			c.error('expected $nr_args arguments, but got $call_expr.args.len', unexpected_arguments_pos)
			return method.return_type
		}

		// if method_name == 'clone' {
		// println('CLONE nr args=$method.args.len')
		// }
		// call_expr.args << method.args[0].typ
		// call_expr.exp_arg_types << method.args[0].typ
		for i, arg in call_expr.args {
			exp_arg_typ := if method.is_variadic && i >= method.params.len - 1 {
				method.params[method.params.len - 1].typ
			} else {
				method.params[i + 1].typ
			}
			exp_arg_sym := c.table.get_type_symbol(exp_arg_typ)
			c.expected_type = exp_arg_typ
			got_arg_typ := c.check_expr_opt_call(arg.expr, c.expr(arg.expr))
			call_expr.args[i].typ = got_arg_typ
			if method.is_variadic && got_arg_typ.has_flag(.variadic) && call_expr.args.len - 1 > i {
				c.error('when forwarding a variadic variable, it must be the final argument',
					arg.pos)
			}
			if exp_arg_sym.kind == .interface_ {
				c.type_implements(got_arg_typ, exp_arg_typ, arg.expr.position())
				continue
			}
			if method.generic_names.len > 0 {
				continue
			}
			c.check_expected_call_arg(got_arg_typ, exp_arg_typ, call_expr.language) or {
				// str method, allow type with str method if fn arg is string
				// Passing an int or a string array produces a c error here
				// Deleting this condition results in propper V error messages
				// if arg_typ_sym.kind == .string && typ_sym.has_method('str') {
				// continue
				// }
				if got_arg_typ != ast.void_type {
					c.error('$err.msg in argument ${i + 1} to `${left_type_sym.name}.$method_name`',
						arg.pos)
				}
			}
			param := if method.is_variadic && i >= method.params.len - 1 {
				method.params[method.params.len - 1]
			} else {
				method.params[i + 1]
			}
			param_share := param.typ.share()
			if param_share == .shared_t && (c.locked_names.len > 0 || c.rlocked_names.len > 0) {
				c.error('method with `shared` arguments cannot be called inside `lock`/`rlock` block',
					arg.pos)
			}
			if arg.is_mut {
				to_lock, pos := c.fail_if_immutable(arg.expr)
				if !param.is_mut {
					tok := arg.share.str()
					c.error('`$call_expr.name` parameter `$param.name` is not `$tok`, `$tok` is not needed`',
						arg.expr.position())
				} else {
					if param.typ.share() != arg.share {
						c.error('wrong shared type', arg.expr.position())
					}
					if to_lock != '' && param_share != .shared_t {
						c.error('$to_lock is `shared` and must be `lock`ed to be passed as `mut`',
							pos)
					}
				}
			} else {
				if param.is_mut {
					tok := arg.share.str()
					c.error('`$call_expr.name` parameter `$param.name` is `$tok`, you need to provide `$tok` e.g. `$tok arg${
						i + 1}`', arg.expr.position())
				} else {
					c.fail_if_unreadable(arg.expr, got_arg_typ, 'argument')
				}
			}
		}
		if method.is_unsafe && !c.inside_unsafe {
			c.warn('method `${left_type_sym.name}.$method_name` must be called from an `unsafe` block',
				call_expr.pos)
		}
		if !c.cur_fn.is_deprecated && method.is_deprecated {
			c.deprecate_fnmethod('method', '${left_type_sym.name}.$method.name', method,
				call_expr)
		}
		// TODO: typ optimize.. this node can get processed more than once
		if call_expr.expected_arg_types.len == 0 {
			for i in 1 .. method.params.len {
				call_expr.expected_arg_types << method.params[i].typ
			}
		}
		if is_method_from_embed {
			call_expr.receiver_type = call_expr.from_embed_type.derive(method.params[0].typ)
		} else if is_generic {
			// We need the receiver to be T in cgen.
			// TODO: cant we just set all these to the concrete type in checker? then no need in gen
			call_expr.receiver_type = left_type.derive(method.params[0].typ).set_flag(.generic)
		} else {
			call_expr.receiver_type = method.params[0].typ
		}
		call_expr.return_type = method.return_type
		if method.generic_names.len != call_expr.generic_types.len {
			// no type arguments given in call, attempt implicit instantiation
			c.infer_fn_types(method, mut call_expr)
		}
		if call_expr.generic_types.len > 0 && method.return_type != 0 {
			if typ := c.table.resolve_generic_by_names(method.return_type, method.generic_names,
				call_expr.generic_types)
			{
				call_expr.return_type = typ
				return typ
			}
		}
		if call_expr.generic_types.len > 0 && method.generic_names.len == 0 {
			c.error('a non generic function called like a generic one', call_expr.generic_list_pos)
		}
		if call_expr.generic_types.len > method.generic_names.len {
			c.error('too many generic parameters got $call_expr.generic_types.len, expected $method.generic_names.len',
				call_expr.generic_list_pos)
		}
		if method.generic_names.len > 0 {
			return call_expr.return_type
		}
		return method.return_type
	}
	// TODO: str methods
	if method_name == 'str' {
		if left_type_sym.kind == .interface_ {
			iname := left_type_sym.name
			c.error('interface `$iname` does not have a .str() method. Use typeof() instead',
				call_expr.pos)
		}
		call_expr.receiver_type = left_type
		call_expr.return_type = ast.string_type
		if call_expr.args.len > 0 {
			c.error('.str() method calls should have no arguments', call_expr.pos)
		}
		c.fail_if_unreadable(call_expr.left, left_type, 'receiver')
		return ast.string_type
	}
	// call struct field fn type
	// TODO: can we use SelectorExpr for all? this dosent really belong here
	if field := c.table.find_field(left_type_sym, method_name) {
		field_type_sym := c.table.get_type_symbol(field.typ)
		if field_type_sym.kind == .function {
			// call_expr.is_method = false
			call_expr.is_field = true
			info := field_type_sym.info as ast.FnType
			call_expr.return_type = info.func.return_type
			mut earg_types := []ast.Type{}
			for mut arg in call_expr.args {
				targ := c.check_expr_opt_call(arg.expr, c.expr(arg.expr))
				arg.typ = targ
				earg_types << targ
			}
			call_expr.expected_arg_types = earg_types
			return info.func.return_type
		}
	}
	if left_type != ast.void_type {
		suggestion := util.new_suggestion(method_name, left_type_sym.methods.map(it.name))
		c.error(suggestion.say(unknown_method_msg), call_expr.pos)
	}
	return ast.void_type
}

fn (mut c Checker) call_map_builtin_method(mut call_expr ast.CallExpr, left_type ast.Type, left_type_sym ast.TypeSymbol) ast.Type {
	method_name := call_expr.name
	mut ret_type := ast.void_type
	match method_name {
		'clone', 'move' {
			if method_name[0] == `m` {
				c.fail_if_immutable(call_expr.left)
			}
			if call_expr.left.is_auto_deref_var() {
				ret_type = left_type.deref()
			} else {
				ret_type = left_type
			}
		}
		'keys' {
			info := left_type_sym.info as ast.Map
			typ := c.table.find_or_register_array(info.key_type)
			ret_type = ast.Type(typ)
		}
		else {}
	}
	call_expr.receiver_type = left_type.to_ptr()
	call_expr.return_type = ret_type
	return call_expr.return_type
}

fn (mut c Checker) call_array_builtin_method(mut call_expr ast.CallExpr, left_type ast.Type, left_type_sym ast.TypeSymbol) ast.Type {
	method_name := call_expr.name
	mut elem_typ := ast.void_type
	if method_name == 'slice' && !c.is_builtin_mod {
		c.error('.slice() is a private method, use `x[start..end]` instead', call_expr.pos)
	}
	array_info := left_type_sym.info as ast.Array
	elem_typ = array_info.elem_type
	if method_name in ['filter', 'map', 'any', 'all'] {
		// position of `it` doesn't matter
		scope_register_it(mut call_expr.scope, call_expr.pos, elem_typ)
	} else if method_name == 'sort' {
		c.fail_if_immutable(call_expr.left)
		// position of `a` and `b` doesn't matter, they're the same
		scope_register_a_b(mut call_expr.scope, call_expr.pos, elem_typ)

		if call_expr.args.len > 1 {
			c.error('expected 0 or 1 argument, but got $call_expr.args.len', call_expr.pos)
		} else if call_expr.args.len == 1 {
			if call_expr.args[0].expr is ast.InfixExpr {
				if call_expr.args[0].expr.op !in [.gt, .lt] {
					c.error('`.sort()` can only use `<` or `>` comparison', call_expr.pos)
				}
				left_name := '${call_expr.args[0].expr.left}'[0]
				right_name := '${call_expr.args[0].expr.right}'[0]
				if left_name !in [`a`, `b`] || right_name !in [`a`, `b`] {
					c.error('`.sort()` can only use `a` or `b` as argument, e.g. `arr.sort(a < b)`',
						call_expr.pos)
				} else if left_name == right_name {
					c.error('`.sort()` cannot use same argument', call_expr.pos)
				}
			} else {
				c.error(
					'`.sort()` requires a `<` or `>` comparison as the first and only argument' +
					'\ne.g. `users.sort(a.id < b.id)`', call_expr.pos)
			}
		}
	} else if method_name == 'wait' {
		elem_sym := c.table.get_type_symbol(elem_typ)
		if elem_sym.kind == .thread {
			if call_expr.args.len != 0 {
				c.error('`.wait()` does not have any arguments', call_expr.args[0].pos)
			}
			thread_ret_type := elem_sym.thread_info().return_type
			if thread_ret_type.has_flag(.optional) {
				c.error('`.wait()` cannot be called for an array when thread functions return optionals. Iterate over the arrays elements instead and handle each returned optional with `or`.',
					call_expr.pos)
			}
			call_expr.return_type = c.table.find_or_register_array(thread_ret_type)
		} else {
			c.error('`$left_type_sym.name` has no method `wait()` (only thread handles and arrays of them have)',
				call_expr.left.position())
		}
	}
	// map/filter are supposed to have 1 arg only
	mut arg_type := left_type
	for arg in call_expr.args {
		arg_type = c.check_expr_opt_call(arg.expr, c.expr(arg.expr))
	}
	if method_name == 'map' {
		// check fn
		c.check_map_and_filter(true, elem_typ, call_expr)
		arg_sym := c.table.get_type_symbol(arg_type)
		ret_type := match arg_sym.info {
			ast.FnType { arg_sym.info.func.return_type }
			else { arg_type }
		}
		call_expr.return_type = c.table.find_or_register_array(ret_type)
	} else if method_name == 'filter' {
		// check fn
		c.check_map_and_filter(false, elem_typ, call_expr)
	} else if method_name in ['any', 'all'] {
		c.check_map_and_filter(false, elem_typ, call_expr)
		call_expr.return_type = ast.bool_type
	} else if method_name == 'clone' {
		// need to return `array_xxx` instead of `array`
		// in ['clone', 'str'] {
		call_expr.receiver_type = left_type.to_ptr()
		if call_expr.left.is_auto_deref_var() {
			call_expr.return_type = left_type.deref()
		} else {
			call_expr.return_type = call_expr.receiver_type.set_nr_muls(0)
		}
	} else if method_name == 'sort' {
		call_expr.return_type = ast.void_type
	} else if method_name == 'contains' {
		call_expr.return_type = ast.bool_type
	} else if method_name == 'index' {
		call_expr.return_type = ast.int_type
	} else if method_name in ['first', 'last', 'pop'] {
		call_expr.return_type = array_info.elem_type
		if method_name == 'pop' {
			call_expr.receiver_type = left_type.to_ptr()
		} else {
			call_expr.receiver_type = left_type
		}
	}
	return call_expr.return_type
}

pub fn (mut c Checker) call_fn(mut call_expr ast.CallExpr) ast.Type {
	fn_name := call_expr.name
	if fn_name == 'main' {
		c.error('the `main` function cannot be called in the program', call_expr.pos)
	}
	if fn_name == 'typeof' {
		// TODO: impl typeof properly (probably not going to be a fn call)
		return ast.string_type
	}
	mut has_generic_generic := false // foo<T>() instead of foo<int>()
	mut generic_types := []ast.Type{}
	for generic_type in call_expr.generic_types {
		if generic_type.has_flag(.generic) {
			has_generic_generic = true
			generic_types << c.unwrap_generic(generic_type)
		} else {
			generic_types << generic_type
		}
	}
	if has_generic_generic {
		if c.mod != '' {
			// Need to prepend the module when adding a generic type to a function
			c.table.register_fn_gen_type(c.mod + '.' + fn_name, generic_types)
		} else {
			c.table.register_fn_gen_type(fn_name, generic_types)
		}
	}
	if fn_name == 'json.encode' {
	} else if fn_name == 'json.decode' && call_expr.args.len > 0 {
		if call_expr.args.len != 2 {
			c.error("json.decode expects 2 arguments, a type and a string (e.g `json.decode(T, '')`)",
				call_expr.pos)
			return ast.void_type
		}
		expr := call_expr.args[0].expr
		if expr !is ast.TypeNode {
			typ := expr.type_name()
			c.error('json.decode: first argument needs to be a type, got `$typ`', call_expr.pos)
			return ast.void_type
		}
		c.expected_type = ast.string_type
		call_expr.args[1].typ = c.expr(call_expr.args[1].expr)
		if call_expr.args[1].typ != ast.string_type {
			c.error('json.decode: second argument needs to be a string', call_expr.pos)
		}
		typ := expr as ast.TypeNode
		ret_type := typ.typ.set_flag(.optional)
		call_expr.return_type = ret_type
		return ret_type
	}
	// look for function in format `mod.fn` or `fn` (builtin)
	mut f := ast.Fn{}
	mut found := false
	mut found_in_args := false
	// anon fn direct call
	if mut call_expr.left is ast.AnonFn {
		// it was set to anon for checker errors, clear for gen
		call_expr.name = ''
		c.expr(call_expr.left)
		anon_fn_sym := c.table.get_type_symbol(call_expr.left.typ)
		f = (anon_fn_sym.info as ast.FnType).func
		found = true
	}
	// try prefix with current module as it would have never gotten prefixed
	if !found && !fn_name.contains('.') && call_expr.mod != 'builtin' {
		name_prefixed := '${call_expr.mod}.$fn_name'
		if f1 := c.table.find_fn(name_prefixed) {
			call_expr.name = name_prefixed
			found = true
			f = f1
			c.table.fns[name_prefixed].usages++
		}
	}
	if !found && call_expr.left is ast.IndexExpr {
		c.expr(call_expr.left)
		expr := call_expr.left as ast.IndexExpr
		sym := c.table.get_type_symbol(expr.left_type)
		if sym.kind == .array {
			info := sym.info as ast.Array
			elem_typ := c.table.get_type_symbol(info.elem_type)
			if elem_typ.info is ast.FnType {
				return elem_typ.info.func.return_type
			}
		} else if sym.kind == .map {
			info := sym.info as ast.Map
			value_typ := c.table.get_type_symbol(info.value_type)
			if value_typ.info is ast.FnType {
				return value_typ.info.func.return_type
			}
		} else if sym.kind == .array_fixed {
			info := sym.info as ast.ArrayFixed
			elem_typ := c.table.get_type_symbol(info.elem_type)
			if elem_typ.info is ast.FnType {
				return elem_typ.info.func.return_type
			}
		}
		found = true
		return ast.string_type
	}
	// already prefixed (mod.fn) or C/builtin/main
	if !found {
		if f1 := c.table.find_fn(fn_name) {
			found = true
			f = f1
			c.table.fns[fn_name].usages++
		}
	}
	if c.pref.is_script && !found {
		os_name := 'os.$fn_name'
		if f1 := c.table.find_fn(os_name) {
			if f1.generic_names.len == call_expr.generic_types.len {
				c.table.fn_gen_types[os_name] = c.table.fn_gen_types['${call_expr.mod}.$call_expr.name']
			}
			call_expr.name = os_name
			found = true
			f = f1
			c.table.fns[os_name].usages++
		}
	}
	// check for arg (var) of fn type
	if !found {
		if v := call_expr.scope.find_var(fn_name) {
			if v.typ != 0 {
				vts := c.table.get_type_symbol(v.typ)
				if vts.kind == .function {
					info := vts.info as ast.FnType
					f = info.func
					found = true
					found_in_args = true
				}
			}
		}
	}
	if !found {
		c.error('unknown function: $fn_name', call_expr.pos)
		return ast.void_type
	}
	if !found_in_args {
		if _ := call_expr.scope.find_var(fn_name) {
			c.error('ambiguous call to: `$fn_name`, may refer to fn `$fn_name` or variable `$fn_name`',
				call_expr.pos)
		}
	}
	if !f.is_pub && f.language == .v && f.name.len > 0 && f.mod.len > 0 && f.mod != c.mod {
		c.error('function `$f.name` is private', call_expr.pos)
	}
	if !c.cur_fn.is_deprecated && f.is_deprecated {
		c.deprecate_fnmethod('function', f.name, f, call_expr)
	}
	if f.is_unsafe && !c.inside_unsafe
		&& (f.language != .c || (f.name[2] in [`m`, `s`] && f.mod == 'builtin')) {
		// builtin C.m*, C.s* only - temp
		c.warn('function `$f.name` must be called from an `unsafe` block', call_expr.pos)
	}
	if f.mod != 'builtin' && f.language == .v && f.no_body && !c.pref.translated && !f.is_unsafe {
		c.error('cannot call a function that does not have a body', call_expr.pos)
	}
	for generic_type in call_expr.generic_types {
		c.ensure_type_exists(generic_type, call_expr.generic_list_pos) or {}
	}
	if f.generic_names.len > 0 && f.return_type.has_flag(.generic) {
		rts := c.table.get_type_symbol(f.return_type)
		if rts.info is ast.Struct {
			if rts.info.generic_types.len > 0 {
				gts := c.table.get_type_symbol(call_expr.generic_types[0])
				nrt := '$rts.name<$gts.name>'
				c_nrt := '${rts.name}_T_$gts.name'
				idx := c.table.type_idxs[nrt]
				if idx != 0 {
					c.ensure_type_exists(idx, call_expr.pos) or {}
					call_expr.return_type = ast.new_type(idx).derive(f.return_type)
				} else {
					mut fields := rts.info.fields.clone()
					if rts.info.generic_types.len == generic_types.len {
						for i, _ in fields {
							if t_typ := c.table.resolve_generic_by_types(fields[i].typ,
								rts.info.generic_types, generic_types)
							{
								fields[i].typ = t_typ
							}
						}
						mut info := rts.info
						info.generic_types = []
						info.fields = fields
						stru_idx := c.table.register_type_symbol(ast.TypeSymbol{
							kind: .struct_
							name: nrt
							cname: util.no_dots(c_nrt)
							mod: c.mod
							info: info
						})
						call_expr.return_type = ast.new_type(stru_idx)
					}
				}
			}
		}
	} else {
		call_expr.return_type = f.return_type
	}
	if f.return_type == ast.void_type && f.ctdefine.len > 0 && f.ctdefine !in c.pref.compile_defines {
		call_expr.should_be_skipped = true
	}
	// dont check number of args for JS functions since arguments are not required
	if call_expr.language != .js {
		min_required_args := if f.is_variadic { f.params.len - 1 } else { f.params.len }
		if call_expr.args.len < min_required_args {
			c.error('expected $min_required_args arguments, but got $call_expr.args.len',
				call_expr.pos)
		} else if !f.is_variadic && call_expr.args.len > f.params.len {
			unexpected_arguments := call_expr.args[min_required_args..]
			unexpected_arguments_pos := unexpected_arguments[0].pos.extend(unexpected_arguments.last().pos)
			c.error('expected $min_required_args arguments, but got $call_expr.args.len',
				unexpected_arguments_pos)
			return f.return_type
		}
	}
	// println / eprintln / panic can print anything
	if fn_name in ['println', 'print', 'eprintln', 'eprint', 'panic'] && call_expr.args.len > 0 {
		c.inside_println_arg = true
		c.expected_type = ast.string_type
		call_expr.args[0].typ = c.expr(call_expr.args[0].expr)
		if call_expr.args[0].typ.is_void() {
			c.error('`$fn_name` can not print void expressions', call_expr.pos)
		}
		c.fail_if_unreadable(call_expr.args[0].expr, call_expr.args[0].typ, 'argument to print')
		c.inside_println_arg = false
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
		eprintln('>>> println hack typ: ${prtyp} | sym.name: ${prtyp_sym.name} | is_ptr: $prtyp_is_ptr | has_str: $prhas_str | expects_ptr: $prexpects_ptr | nr_args: $prnr_args | expr: ${prexpr.str()} ')
		*/
		return f.return_type
	}
	// `return error(err)` -> `return err`
	if fn_name == 'error' {
		arg := call_expr.args[0]
		call_expr.args[0].typ = c.expr(arg.expr)
		if call_expr.args[0].typ == ast.error_type {
			c.warn('`error($arg)` can be shortened to just `$arg`', call_expr.pos)
		}
	}
	// TODO: typ optimize.. this node can get processed more than once
	if call_expr.expected_arg_types.len == 0 {
		for param in f.params {
			call_expr.expected_arg_types << param.typ
		}
	}
	for i, call_arg in call_expr.args {
		param := if f.is_variadic && i >= f.params.len - 1 {
			f.params[f.params.len - 1]
		} else {
			f.params[i]
		}
		if f.is_variadic && call_arg.expr is ast.ArrayDecompose {
			if i > f.params.len - 1 {
				c.error('too many arguments in call to `$f.name`', call_expr.pos)
			}
		}
		c.expected_type = param.typ
		typ := c.check_expr_opt_call(call_arg.expr, c.expr(call_arg.expr))
		call_expr.args[i].typ = typ
		typ_sym := c.table.get_type_symbol(typ)
		arg_typ_sym := c.table.get_type_symbol(param.typ)
		if f.is_variadic && typ.has_flag(.variadic) && call_expr.args.len - 1 > i {
			c.error('when forwarding a variadic variable, it must be the final argument',
				call_arg.pos)
		}
		arg_share := param.typ.share()
		if arg_share == .shared_t && (c.locked_names.len > 0 || c.rlocked_names.len > 0) {
			c.error('function with `shared` arguments cannot be called inside `lock`/`rlock` block',
				call_arg.pos)
		}
		if call_arg.is_mut && f.language == .v {
			to_lock, pos := c.fail_if_immutable(call_arg.expr)
			if !param.is_mut {
				tok := call_arg.share.str()
				c.error('`$call_expr.name` parameter `$param.name` is not `$tok`, `$tok` is not needed`',
					call_arg.expr.position())
			} else {
				if param.typ.share() != call_arg.share {
					c.error('wrong shared type', call_arg.expr.position())
				}
				if to_lock != '' && !param.typ.has_flag(.shared_f) {
					c.error('$to_lock is `shared` and must be `lock`ed to be passed as `mut`',
						pos)
				}
			}
		} else {
			if param.is_mut {
				tok := call_arg.share.str()
				c.error('`$call_expr.name` parameter `$param.name` is `$tok`, you need to provide `$tok` e.g. `$tok arg${
					i + 1}`', call_arg.expr.position())
			} else {
				c.fail_if_unreadable(call_arg.expr, typ, 'argument')
			}
		}
		// Handle expected interface
		if arg_typ_sym.kind == .interface_ {
			c.type_implements(typ, param.typ, call_arg.expr.position())
			continue
		}
		c.check_expected_call_arg(typ, param.typ, call_expr.language) or {
			// str method, allow type with str method if fn arg is string
			// Passing an int or a string array produces a c error here
			// Deleting this condition results in propper V error messages
			// if arg_typ_sym.kind == .string && typ_sym.has_method('str') {
			// continue
			// }
			if typ_sym.kind == .void && arg_typ_sym.kind == .string {
				continue
			}
			if f.generic_names.len > 0 {
				if param.typ.has_flag(.generic)
					&& f.generic_names.len == call_expr.generic_types.len {
					if unwrap_typ := c.table.resolve_generic_by_names(param.typ, f.generic_names,
						call_expr.generic_types)
					{
						if (unwrap_typ.idx() == typ.idx())
							|| (unwrap_typ.is_int() && typ.is_int())
							|| (unwrap_typ.is_float() && typ.is_float()) {
							continue
						}
						expected_sym := c.table.get_type_symbol(unwrap_typ)
						got_sym := c.table.get_type_symbol(typ)
						c.error('argument ${i + 1} got `$got_sym.name`, expected `$expected_sym.name`',
							call_arg.pos)
					} else {
						continue
					}
				} else {
					continue
				}
			}
			c.error('$err.msg in argument ${i + 1} to `$fn_name`', call_arg.pos)
		}
		// Warn about automatic (de)referencing, which will be removed soon.
		if f.language != .c && !c.inside_unsafe && typ.nr_muls() != param.typ.nr_muls()
			&& !(call_arg.is_mut && param.is_mut) && !(!call_arg.is_mut && !param.is_mut)
			&& param.typ !in [ast.byteptr_type, ast.charptr_type, ast.voidptr_type] {
			// sym := c.table.get_type_symbol(typ)
			c.warn('automatic referencing/dereferencing is deprecated and will be removed soon (got: $typ.nr_muls() references, expected: $param.typ.nr_muls() references)',
				call_arg.pos)
		}
	}
	if f.generic_names.len != call_expr.generic_types.len {
		// no type arguments given in call, attempt implicit instantiation
		c.infer_fn_types(f, mut call_expr)
	}
	if call_expr.generic_types.len > 0 && f.return_type != 0 {
		if typ := c.table.resolve_generic_by_names(f.return_type, f.generic_names, call_expr.generic_types) {
			call_expr.return_type = typ
			return typ
		}
	}
	if call_expr.generic_types.len > 0 && f.generic_names.len == 0 {
		c.error('a non generic function called like a generic one', call_expr.generic_list_pos)
	}

	if call_expr.generic_types.len > f.generic_names.len {
		c.error('too many generic parameters got $call_expr.generic_types.len, expected $f.generic_names.len',
			call_expr.generic_list_pos)
	}
	if f.generic_names.len > 0 {
		return call_expr.return_type
	}
	return f.return_type
}

fn (mut c Checker) deprecate_fnmethod(kind string, name string, the_fn ast.Fn, call_expr ast.CallExpr) {
	start_message := '$kind `$name`'
	mut deprecation_message := ''
	now := time.now()
	mut after_time := now
	for attr in the_fn.attrs {
		if attr.name == 'deprecated' && attr.arg != '' {
			deprecation_message = attr.arg
		}
		if attr.name == 'deprecated_after' && attr.arg != '' {
			after_time = time.parse_iso8601(attr.arg) or {
				c.error('invalid time format', attr.pos)
				time.now()
			}
		}
	}
	if after_time < now {
		c.warn(semicolonize('$start_message has been deprecated since $after_time.ymmdd()',
			deprecation_message), call_expr.pos)
	} else if after_time == now {
		c.warn(semicolonize('$start_message has been deprecated', deprecation_message),
			call_expr.pos)
	} else {
		c.note(semicolonize('$start_message will be deprecated after $after_time.ymmdd()',
			deprecation_message), call_expr.pos)
	}
}

fn semicolonize(main string, details string) string {
	if details == '' {
		return main
	}
	return '$main; $details'
}

fn (mut c Checker) type_implements(typ ast.Type, inter_typ ast.Type, pos token.Position) bool {
	$if debug_interface_type_implements ? {
		eprintln('> type_implements typ: $typ.debug() | inter_typ: $inter_typ.debug()')
	}
	utyp := c.unwrap_generic(typ)
	typ_sym := c.table.get_type_symbol(utyp)
	mut inter_sym := c.table.get_type_symbol(inter_typ)
	// do not check the same type more than once
	if mut inter_sym.info is ast.Interface {
		for t in inter_sym.info.types {
			if t.idx() == utyp.idx() {
				return true
			}
		}
	}
	styp := c.table.type_to_str(utyp)
	if utyp.idx() == inter_typ.idx() {
		// same type -> already casted to the interface
		return true
	}
	if inter_typ.idx() == ast.error_type_idx && utyp.idx() == ast.none_type_idx {
		// `none` "implements" the Error interface
		return true
	}
	if typ_sym.kind == .interface_ && inter_sym.kind == .interface_ {
		c.error('cannot implement interface `$inter_sym.name` with a different interface `$styp`',
			pos)
	}
	imethods := if inter_sym.kind == .interface_ {
		(inter_sym.info as ast.Interface).methods
	} else {
		inter_sym.methods
	}
	for imethod in imethods {
		if method := typ_sym.find_method(imethod.name) {
			msg := c.table.is_same_method(imethod, method)
			if msg.len > 0 {
				sig := c.table.fn_signature(imethod, skip_receiver: true)
				c.add_error_detail('$inter_sym.name has `$sig`')
				c.error('`$styp` incorrectly implements method `$imethod.name` of interface `$inter_sym.name`: $msg',
					pos)
				return false
			}
			continue
		}
		c.error("`$styp` doesn't implement method `$imethod.name` of interface `$inter_sym.name`",
			pos)
	}
	if mut inter_sym.info is ast.Interface {
		for ifield in inter_sym.info.fields {
			if field := c.table.find_field_with_embeds(typ_sym, ifield.name) {
				if ifield.typ != field.typ {
					exp := c.table.type_to_str(ifield.typ)
					got := c.table.type_to_str(field.typ)
					c.error('`$styp` incorrectly implements field `$ifield.name` of interface `$inter_sym.name`, expected `$exp`, got `$got`',
						pos)
					return false
				} else if ifield.is_mut && !(field.is_mut || field.is_global) {
					c.error('`$styp` incorrectly implements interface `$inter_sym.name`, field `$ifield.name` must be mutable',
						pos)
					return false
				}
				continue
			}
			c.error("`$styp` doesn't implement field `$ifield.name` of interface `$inter_sym.name`",
				pos)
		}
		if utyp !in inter_sym.info.types && typ_sym.kind != .interface_ {
			inter_sym.info.types << utyp
		}
	}
	return true
}

// return the actual type of the expression, once the optional is handled
pub fn (mut c Checker) check_expr_opt_call(expr ast.Expr, ret_type ast.Type) ast.Type {
	if expr is ast.CallExpr {
		if expr.return_type.has_flag(.optional) {
			if expr.or_block.kind == .absent {
				c.error('${expr.name}() returns an option, so it should have either an `or {}` block, or `?` at the end',
					expr.pos)
			} else {
				c.check_or_expr(expr.or_block, ret_type, expr.return_type.clear_flag(.optional))
			}
			return ret_type.clear_flag(.optional)
		} else if expr.or_block.kind == .block {
			c.error('unexpected `or` block, the function `$expr.name` does not return an optional',
				expr.or_block.pos)
		} else if expr.or_block.kind == .propagate {
			c.error('unexpected `?`, the function `$expr.name` does not return an optional',
				expr.or_block.pos)
		}
	} else if expr is ast.IndexExpr {
		if expr.or_expr.kind != .absent {
			c.check_or_expr(expr.or_expr, ret_type, ret_type)
		}
	}
	return ret_type
}

pub fn (mut c Checker) check_or_expr(or_expr ast.OrExpr, ret_type ast.Type, expr_return_type ast.Type) {
	if or_expr.kind == .propagate {
		if !c.cur_fn.return_type.has_flag(.optional) && c.cur_fn.name != 'main.main'
			&& !c.inside_const {
			c.error('to propagate the optional call, `$c.cur_fn.name` must return an optional',
				or_expr.pos)
		}
		return
	}
	stmts_len := or_expr.stmts.len
	if stmts_len == 0 {
		if ret_type != ast.void_type {
			// x := f() or {}
			c.error('assignment requires a non empty `or {}` block', or_expr.pos)
			return
		}
		// allow `f() or {}`
		return
	}
	last_stmt := or_expr.stmts[stmts_len - 1]
	if ret_type != ast.void_type {
		match last_stmt {
			ast.ExprStmt {
				c.expected_type = ret_type
				c.expected_or_type = ret_type.clear_flag(.optional)
				last_stmt_typ := c.expr(last_stmt.expr)
				c.expected_or_type = ast.void_type
				type_fits := c.check_types(last_stmt_typ, ret_type)
					&& last_stmt_typ.nr_muls() == ret_type.nr_muls()
				is_panic_or_exit := is_expr_panic_or_exit(last_stmt.expr)
				if type_fits || is_panic_or_exit {
					return
				}
				expected_type_name := c.table.type_to_str(ret_type.clear_flag(.optional))
				if last_stmt.typ == ast.void_type {
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
				if last_stmt.typ == ast.void_type {
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

pub fn (mut c Checker) selector_expr(mut selector_expr ast.SelectorExpr) ast.Type {
	prevent_sum_type_unwrapping_once := c.prevent_sum_type_unwrapping_once
	c.prevent_sum_type_unwrapping_once = false

	using_new_err_struct_save := c.using_new_err_struct
	// TODO remove; this avoids a breaking change in syntax
	if '$selector_expr.expr' == 'err' {
		c.using_new_err_struct = true
	}

	// T.name, typeof(expr).name
	mut name_type := 0
	match mut selector_expr.expr {
		ast.Ident {
			name := selector_expr.expr.name
			valid_generic := util.is_generic_type_name(name)
				&& c.cur_fn.generic_params.filter(it.name == name).len != 0
			if valid_generic {
				name_type = ast.Type(c.table.find_type_idx(name)).set_flag(.generic)
			}
		}
		// Note: in future typeof() should be a type known at compile-time
		// sum types should not be handled dynamically
		ast.TypeOf {
			name_type = c.expr(selector_expr.expr.expr)
		}
		else {}
	}
	if name_type > 0 {
		if selector_expr.field_name != 'name' {
			c.error('invalid field `.$selector_expr.field_name` for type `$selector_expr.expr`',
				selector_expr.pos)
		}
		selector_expr.name_type = name_type
		return ast.string_type
	}
	//
	old_selector_expr := c.inside_selector_expr
	c.inside_selector_expr = true
	typ := c.expr(selector_expr.expr)
	c.inside_selector_expr = old_selector_expr
	//
	c.using_new_err_struct = using_new_err_struct_save
	if typ == ast.void_type_idx {
		c.error('unknown selector expression', selector_expr.pos)
		return ast.void_type
	}
	selector_expr.expr_type = typ
	if selector_expr.expr_type.has_flag(.optional) && !((selector_expr.expr is ast.Ident
		&& (selector_expr.expr as ast.Ident).kind == .constant)) {
		c.error('cannot access fields of an optional, handle the error with `or {...}` or propagate it with `?`',
			selector_expr.pos)
	}
	field_name := selector_expr.field_name
	utyp := c.unwrap_generic(typ)
	sym := c.table.get_type_symbol(utyp)
	if typ.has_flag(.variadic) || sym.kind == .array_fixed || sym.kind == .chan {
		if field_name == 'len' || (sym.kind == .chan && field_name == 'cap') {
			selector_expr.typ = ast.int_type
			return ast.int_type
		}
		if sym.kind == .chan && field_name == 'closed' {
			selector_expr.typ = ast.bool_type
			return ast.bool_type
		}
	}
	mut unknown_field_msg := 'type `$sym.name` has no field or method `$field_name`'
	mut has_field := false
	mut field := ast.StructField{}
	if field_name.len > 0 && field_name[0].is_capital() && sym.info is ast.Struct
		&& sym.language == .v {
		// x.Foo.y => access the embedded struct
		sym_info := sym.info as ast.Struct
		for embed in sym_info.embeds {
			embed_sym := c.table.get_type_symbol(embed)
			if embed_sym.embed_name() == field_name {
				selector_expr.typ = embed
				return embed
			}
		}
	} else {
		if f := c.table.find_field(sym, field_name) {
			has_field = true
			field = f
		} else {
			// look for embedded field
			if sym.info is ast.Struct {
				mut found_fields := []ast.StructField{}
				mut embed_of_found_fields := []ast.Type{}
				for embed in sym.info.embeds {
					embed_sym := c.table.get_type_symbol(embed)
					if f := c.table.find_field(embed_sym, field_name) {
						found_fields << f
						embed_of_found_fields << embed
					}
				}
				if found_fields.len == 1 {
					field = found_fields[0]
					has_field = true
					selector_expr.from_embed_type = embed_of_found_fields[0]
				} else if found_fields.len > 1 {
					c.error('ambiguous field `$field_name`', selector_expr.pos)
				}
			}
			if sym.kind in [.aggregate, .sum_type] {
				unknown_field_msg = err.msg
			}
		}
		if !c.inside_unsafe {
			if sym.info is ast.Struct {
				if sym.info.is_union && selector_expr.next_token !in token.assign_tokens {
					c.warn('reading a union field (or its address) requires `unsafe`',
						selector_expr.pos)
				}
			}
		}
	}
	if has_field {
		if sym.mod != c.mod && !field.is_pub && sym.language != .c {
			c.error('field `${sym.name}.$field_name` is not public', selector_expr.pos)
		}
		field_sym := c.table.get_type_symbol(field.typ)
		if field_sym.kind == .sum_type {
			if !prevent_sum_type_unwrapping_once {
				if scope_field := selector_expr.scope.find_struct_field(utyp, field_name) {
					return scope_field.sum_type_casts.last()
				}
			}
		}
		selector_expr.typ = field.typ
		return field.typ
	}
	if sym.kind !in [.struct_, .aggregate, .interface_, .sum_type] {
		if sym.kind != .placeholder {
			c.error('`$sym.name` has no property `$selector_expr.field_name`', selector_expr.pos)
		}
	} else {
		if sym.info is ast.Struct {
			suggestion := util.new_suggestion(field_name, sym.info.fields.map(it.name))
			c.error(suggestion.say(unknown_field_msg), selector_expr.pos)
		}
		c.error(unknown_field_msg, selector_expr.pos)
	}
	return ast.void_type
}

// TODO: non deferred
pub fn (mut c Checker) return_stmt(mut return_stmt ast.Return) {
	c.expected_type = c.cur_fn.return_type
	expected_type := c.unwrap_generic(c.expected_type)
	expected_type_sym := c.table.get_type_symbol(expected_type)
	if return_stmt.exprs.len > 0 && c.cur_fn.return_type == ast.void_type {
		c.error('unexpected argument, current function does not return anything', return_stmt.exprs[0].position())
		return
	} else if return_stmt.exprs.len == 0 && !(c.expected_type == ast.void_type
		|| expected_type_sym.kind == .void) {
		stype := c.table.type_to_str(expected_type)
		arg := if expected_type_sym.kind == .multi_return { 'arguments' } else { 'argument' }
		c.error('expected `$stype` $arg', return_stmt.pos)
		return
	}
	if return_stmt.exprs.len == 0 {
		return
	}
	exp_is_optional := expected_type.has_flag(.optional)
	mut expected_types := [expected_type]
	if expected_type_sym.info is ast.MultiReturn {
		expected_types = expected_type_sym.info.types
		if c.cur_generic_types.len > 0 {
			expected_types = expected_types.map(c.unwrap_generic(it))
		}
	}
	mut got_types := []ast.Type{}
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
	// allow `none` & `error` return types for function that returns optional
	if exp_is_optional
		&& got_types[0].idx() in [ast.none_type_idx, ast.error_type_idx, c.table.type_idxs['Option']] {
		return
	}
	if expected_types.len > 0 && expected_types.len != got_types.len {
		arg := if expected_types.len == 1 { 'argument' } else { 'arguments' }
		c.error('expected $expected_types.len $arg, but got $got_types.len', return_stmt.pos)
		return
	}
	for i, exp_type in expected_types {
		got_typ := c.unwrap_generic(got_types[i])
		if got_typ.has_flag(.optional) && (!exp_type.has_flag(.optional)
			|| c.table.type_to_str(got_typ) != c.table.type_to_str(exp_type)) {
			pos := return_stmt.exprs[i].position()
			c.error('cannot use `${c.table.type_to_str(got_typ)}` as type `${c.table.type_to_str(exp_type)}` in return argument',
				pos)
		}
		if !c.check_types(got_typ, exp_type) {
			got_typ_sym := c.table.get_type_symbol(got_typ)
			mut exp_typ_sym := c.table.get_type_symbol(exp_type)
			pos := return_stmt.exprs[i].position()
			if return_stmt.exprs[i].is_auto_deref_var() {
				continue
			}
			if exp_typ_sym.kind == .interface_ {
				c.type_implements(got_typ, exp_type, return_stmt.pos)
				continue
			}
			c.error('cannot use `$got_typ_sym.name` as type `$exp_typ_sym.name` in return argument',
				pos)
		}
		if (got_typ.is_ptr() || got_typ.is_pointer())
			&& (!exp_type.is_ptr() && !exp_type.is_pointer()) {
			pos := return_stmt.exprs[i].position()
			if return_stmt.exprs[i].is_auto_deref_var() {
				continue
			}
			c.error('fn `$c.cur_fn.name` expects you to return a non reference type `${c.table.type_to_str(exp_type)}`, but you are returning `${c.table.type_to_str(got_typ)}` instead',
				pos)
		}
		if (exp_type.is_ptr() || exp_type.is_pointer())
			&& (!got_typ.is_ptr() && !got_typ.is_pointer()) && got_typ != ast.int_literal_type {
			pos := return_stmt.exprs[i].position()
			if return_stmt.exprs[i].is_auto_deref_var() {
				continue
			}
			c.error('fn `$c.cur_fn.name` expects you to return a reference type `${c.table.type_to_str(exp_type)}`, but you are returning `${c.table.type_to_str(got_typ)}` instead',
				pos)
		}
	}
	if exp_is_optional && return_stmt.exprs.len > 0 {
		expr0 := return_stmt.exprs[0]
		if expr0 is ast.CallExpr {
			if expr0.or_block.kind == .propagate {
				c.error('`?` is not needed, use `return ${expr0.name}()`', expr0.pos)
			}
		}
	}
}

pub fn (mut c Checker) const_decl(mut node ast.ConstDecl) {
	mut field_names := []string{}
	mut field_order := []int{}
	if node.fields.len == 0 {
		c.warn('const block must have at least 1 declaration', node.pos)
	}
	for i, field in node.fields {
		// TODO Check const name once the syntax is decided
		if field.name in c.const_names {
			name_pos := token.Position{
				...field.pos
				len: util.no_cur_mod(field.name, c.mod).len
			}
			c.error('duplicate const `$field.name`', name_pos)
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
		typ := c.check_expr_opt_call(field.expr, c.expr(field.expr))
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

pub fn (mut c Checker) enum_decl(decl ast.EnumDecl) {
	c.check_valid_pascal_case(decl.name, 'enum name', decl.pos)
	mut seen := []i64{}
	if decl.fields.len == 0 {
		c.error('enum cannot be empty', decl.pos)
	}
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
			match field.expr {
				ast.IntegerLiteral {
					val := field.expr.val.i64()
					if val < checker.int_min || val > checker.int_max {
						c.error('enum value `$val` overflows int', field.expr.pos)
					} else if !decl.is_multi_allowed && i64(val) in seen {
						c.error('enum value `$val` already exists', field.expr.pos)
					}
					seen << i64(val)
				}
				ast.PrefixExpr {}
				else {
					if field.expr is ast.Ident {
						if field.expr.language == .c {
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
				if last == checker.int_max {
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
	c.expected_type = ast.none_type // TODO a hack to make `x := if ... work`
	defer {
		c.expected_type = ast.void_type
	}
	right_first := assign_stmt.right[0]
	mut right_len := assign_stmt.right.len
	mut right_type0 := ast.void_type
	for i, right in assign_stmt.right {
		if right is ast.CallExpr || right is ast.IfExpr || right is ast.LockExpr
			|| right is ast.MatchExpr {
			right_type := c.expr(right)
			if i == 0 {
				right_type0 = right_type
				assign_stmt.right_types = [
					c.check_expr_opt_call(right, right_type0),
				]
			}
			right_type_sym := c.table.get_type_symbol(right_type)
			if right_type_sym.kind == .multi_return {
				if assign_stmt.right.len > 1 {
					c.error('cannot use multi-value $right_type_sym.name in single-value context',
						right.position())
				}
				assign_stmt.right_types = right_type_sym.mr_info().types
				right_len = assign_stmt.right_types.len
			} else if right_type == ast.void_type {
				right_len = 0
			}
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
	//
	is_decl := assign_stmt.op == .decl_assign
	for i, left in assign_stmt.left {
		if left is ast.CallExpr {
			c.error('cannot call function `${left.name}()` on the left side of an assignment',
				left.pos)
		}
		is_blank_ident := left.is_blank_ident()
		mut left_type := ast.void_type
		if !is_decl && !is_blank_ident {
			if left is ast.Ident || left is ast.SelectorExpr {
				c.prevent_sum_type_unwrapping_once = true
			}
			left_type = c.expr(left)
			c.expected_type = c.unwrap_generic(left_type)
			// `map = {}`
			sym := c.table.get_type_symbol(left_type)
			if sym.kind == .map && assign_stmt.right[i] is ast.StructInit {
				c.warn('assigning a struct literal to a map is deprecated - use `map{}` instead',
					assign_stmt.right[i].position())
				assign_stmt.right[i] = ast.MapInit{}
			}
		}
		if assign_stmt.right_types.len < assign_stmt.left.len { // first type or multi return types added above
			old_inside_ref_lit := c.inside_ref_lit
			if left is ast.Ident {
				if left.info is ast.IdentVar {
					c.inside_ref_lit = c.inside_ref_lit || left.info.share == .shared_t
				}
			}
			right_type := c.expr(assign_stmt.right[i])
			c.inside_ref_lit = old_inside_ref_lit
			if assign_stmt.right_types.len == i {
				assign_stmt.right_types << c.check_expr_opt_call(assign_stmt.right[i],
					right_type)
			}
		}
		right := if i < assign_stmt.right.len { assign_stmt.right[i] } else { assign_stmt.right[0] }
		mut right_type := assign_stmt.right_types[i]
		if is_decl {
			if right.is_auto_deref_var() {
				left_type = c.table.mktyp(right_type.deref())
			} else {
				left_type = c.table.mktyp(right_type)
			}
			if left_type == ast.int_type {
				if right is ast.IntegerLiteral {
					mut is_large := right.val.len > 13
					if !is_large && right.val.len > 8 {
						val := right.val.i64()
						is_large = val > checker.int_max || val < checker.int_min
					}
					if is_large {
						c.error('overflow in implicit type `int`, use explicit type casting instead',
							right.pos)
					}
				}
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
						if is_decl {
							if left_type.nr_muls() > 1 {
								c.error('shared cannot be multi level reference', left.pos)
							}
							left_type = left_type.set_nr_muls(1)
						}
					} else if left_type.has_flag(.shared_f) {
						left_type = left_type.clear_flag(.shared_f)
					}
					if ident_var_info.share == .atomic_t {
						left_type = left_type.set_flag(.atomic_f)
					}
					assign_stmt.left_types[i] = left_type
					ident_var_info.typ = left_type
					left.info = ident_var_info
					if left_type != 0 {
						match mut left.obj {
							ast.Var { left.obj.typ = left_type }
							ast.GlobalField { left.obj.typ = left_type }
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
					if is_decl {
						full_name := '${left.mod}.$left.name'
						if obj := c.file.global_scope.find(full_name) {
							if obj is ast.ConstField {
								c.warn('duplicate of a const name `$full_name`', left.pos)
							}
						}
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
				if mut left is ast.IndexExpr {
					// eprintln('>>> left.is_setter: ${left.is_setter:10} | left.is_map: ${left.is_map:10} | left.is_array: ${left.is_array:10}')
					if left.is_map && left.is_setter {
						left.recursive_mapset_is_setter(true)
					}
				}
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
		if c.pref.translated {
			// TODO fix this in C2V instead, for example cast enums to int before using `|` on them.
			// TODO replace all c.pref.translated checks with `$if !translated` for performance
			continue
		}
		if left_sym.kind == .array && !c.inside_unsafe && assign_stmt.op in [.assign, .decl_assign]
			&& right_sym.kind == .array && (left is ast.Ident && !left.is_blank_ident())
			&& right is ast.Ident {
			// Do not allow `a = b`, only `a = b.clone()`
			c.error('use `array2 $assign_stmt.op.str() array1.clone()` instead of `array2 $assign_stmt.op.str() array1` (or use `unsafe`)',
				assign_stmt.pos)
		}
		if left_sym.kind == .map && assign_stmt.op in [.assign, .decl_assign]
			&& right_sym.kind == .map && ((right is ast.Ident && right.is_auto_deref_var())
			|| !right_type.is_ptr()) && !left.is_blank_ident() && right.is_lvalue() {
			// Do not allow `a = b`
			c.error('cannot copy map: call `move` or `clone` method (or use a reference)',
				right.position())
		}
		left_is_ptr := left_type.is_ptr() || left_sym.is_pointer()
		if left_is_ptr && !left.is_auto_deref_var() {
			if !c.inside_unsafe && assign_stmt.op !in [.assign, .decl_assign] {
				// ptr op=
				c.warn('pointer arithmetic is only allowed in `unsafe` blocks', assign_stmt.pos)
			}
			right_is_ptr := right_type.is_ptr() || right_sym.is_pointer()
			if !right_is_ptr && assign_stmt.op == .assign && right_type_unwrapped.is_number() {
				c.error('cannot assign to `$left`: ' +
					c.expected_msg(right_type_unwrapped, left_type_unwrapped), right.position())
			}
			if (right is ast.StructInit || !right_is_ptr) && !(right_sym.is_number()
				|| left_type.has_flag(.shared_f)) {
				left_name := c.table.type_to_str(left_type_unwrapped)
				mut rtype := right_type_unwrapped
				if rtype.is_ptr() {
					rtype = rtype.deref()
				}
				right_name := c.table.type_to_str(rtype)
				c.error('mismatched types `$left_name` and `$right_name`', assign_stmt.pos)
			}
		}
		// Single side check
		match assign_stmt.op {
			.assign {} // No need to do single side check for =. But here put it first for speed.
			.plus_assign, .minus_assign {
				if left_type == ast.string_type {
					if assign_stmt.op != .plus_assign {
						c.error('operator `$assign_stmt.op` not defined on left operand type `$left_sym.name`',
							left.position())
					}
					if right_type != ast.string_type {
						c.error('invalid right operand: $left_sym.name $assign_stmt.op $right_sym.name',
							right.position())
					}
				} else if !left_sym.is_number()
					&& left_sym.kind !in [.byteptr, .charptr, .struct_, .alias] {
					c.error('operator `$assign_stmt.op` not defined on left operand type `$left_sym.name`',
						left.position())
				} else if !right_sym.is_number()
					&& left_sym.kind !in [.byteptr, .charptr, .struct_, .alias] {
					c.error('invalid right operand: $left_sym.name $assign_stmt.op $right_sym.name',
						right.position())
				}
			}
			.mult_assign, .div_assign {
				if !left_sym.is_number()
					&& !c.table.get_final_type_symbol(left_type_unwrapped).is_int()
					&& left_sym.kind !in [.struct_, .alias] {
					c.error('operator $assign_stmt.op.str() not defined on left operand type `$left_sym.name`',
						left.position())
				} else if !right_sym.is_number()
					&& !c.table.get_final_type_symbol(left_type_unwrapped).is_int()
					&& left_sym.kind !in [.struct_, .alias] {
					c.error('operator $assign_stmt.op.str() not defined on right operand type `$right_sym.name`',
						right.position())
				}
			}
			.and_assign, .or_assign, .xor_assign, .mod_assign, .left_shift_assign, .right_shift_assign
			 {
				if !left_sym.is_int()
					&& !c.table.get_final_type_symbol(left_type_unwrapped).is_int() {
					c.error('operator $assign_stmt.op.str() not defined on left operand type `$left_sym.name`',
						left.position())
				} else if !right_sym.is_int()
					&& !c.table.get_final_type_symbol(right_type_unwrapped).is_int() {
					c.error('operator $assign_stmt.op.str() not defined on right operand type `$right_sym.name`',
						right.position())
				}
			}
			else {}
		}
		if assign_stmt.op in [.plus_assign, .minus_assign, .mod_assign, .mult_assign, .div_assign]
			&& ((left_sym.kind == .struct_ && right_sym.kind == .struct_)
			|| left_sym.kind == .alias) {
			left_name := c.table.type_to_str(left_type)
			right_name := c.table.type_to_str(right_type)
			parent_sym := c.table.get_final_type_symbol(left_type)
			if left_sym.kind == .alias && right_sym.kind != .alias {
				c.error('mismatched types `$left_name` and `$right_name`', assign_stmt.pos)
			}
			extracted_op := match assign_stmt.op {
				.plus_assign { '+' }
				.minus_assign { '-' }
				.div_assign { '/' }
				.mod_assign { '%' }
				.mult_assign { '*' }
				else { 'unknown op' }
			}
			if method := left_sym.find_method(extracted_op) {
				if method.return_type != left_type {
					c.error('operator `$extracted_op` must return `$left_name` to be used as an assignment operator',
						assign_stmt.pos)
				}
			} else {
				if parent_sym.is_primitive() {
					c.error('cannot use operator methods on type alias for `$parent_sym.name`',
						assign_stmt.pos)
				}
				if left_name == right_name {
					c.error('undefined operation `$left_name` $extracted_op `$right_name`',
						assign_stmt.pos)
				} else {
					c.error('mismatched types `$left_name` and `$right_name`', assign_stmt.pos)
				}
			}
		}
		if !is_blank_ident && !right.is_auto_deref_var() && right_sym.kind != .placeholder
			&& left_sym.kind != .interface_ {
			// Dual sides check (compatibility check)
			c.check_expected(right_type_unwrapped, left_type_unwrapped) or {
				c.error('cannot assign to `$left`: $err.msg', right.position())
			}
		}
		if left_sym.kind == .interface_ {
			c.type_implements(right_type, left_type, right.position())
		}
	}
	// this needs to run after the assign stmt left exprs have been run through checker so that ident.obj is set
	// Check `x := &y` and `mut x := <-ch`
	if right_first is ast.PrefixExpr {
		node := right_first
		left_first := assign_stmt.left[0]
		if left_first is ast.Ident {
			assigned_var := left_first
			mut is_shared := false
			if left_first.info is ast.IdentVar {
				is_shared = left_first.info.share == .shared_t
			}
			old_inside_ref_lit := c.inside_ref_lit
			c.inside_ref_lit = (c.inside_ref_lit || node.op == .amp || is_shared)
			c.expr(node.right)
			c.inside_ref_lit = old_inside_ref_lit
			if node.right is ast.Ident {
				if node.right.obj is ast.Var {
					v := node.right.obj
					right_type0 = v.typ
					if node.op == .amp {
						if !v.is_mut && assigned_var.is_mut && !c.inside_unsafe {
							c.error('`$node.right.name` is immutable, cannot have a mutable reference to it',
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
							c.error('cannot have a mutable reference to object from `$right_sym.name`',
								node.pos)
						}
					}
				}
			}
		}
	}
	// right_sym := c.table.get_type_symbol(right_type_unwrapped)
}

fn scope_register_it(mut s ast.Scope, pos token.Position, typ ast.Type) {
	s.register(ast.Var{
		name: 'it'
		pos: pos
		typ: typ
		is_used: true
	})
}

fn scope_register_a_b(mut s ast.Scope, pos token.Position, typ ast.Type) {
	s.register(ast.Var{
		name: 'a'
		pos: pos
		typ: typ.to_ptr()
		is_used: true
	})
	s.register(ast.Var{
		name: 'b'
		pos: pos
		typ: typ.to_ptr()
		is_used: true
	})
}

fn (mut c Checker) check_array_init_para_type(para string, expr ast.Expr, pos token.Position) {
	sym := c.table.get_type_symbol(c.expr(expr))
	if sym.kind !in [.int, .int_literal] {
		c.error('array $para needs to be an int', pos)
	}
}

pub fn (mut c Checker) ensure_sumtype_array_has_default_value(array_init ast.ArrayInit) {
	sym := c.table.get_type_symbol(array_init.elem_type)
	if sym.kind == .sum_type && !array_init.has_default {
		c.error('cannot initialize sum type array without default value', array_init.pos)
	}
}

pub fn (mut c Checker) array_init(mut array_init ast.ArrayInit) ast.Type {
	// println('checker: array init $array_init.pos.line_nr $c.file.path')
	mut elem_type := ast.void_type
	// []string - was set in parser
	if array_init.typ != ast.void_type {
		if array_init.exprs.len == 0 {
			if array_init.has_cap {
				c.check_array_init_para_type('cap', array_init.cap_expr, array_init.pos)
			}
			if array_init.has_len {
				c.check_array_init_para_type('len', array_init.len_expr, array_init.pos)
			}
		}
		if array_init.has_default {
			default_expr := array_init.default_expr
			default_typ := c.check_expr_opt_call(default_expr, c.expr(default_expr))
			c.check_expected(default_typ, array_init.elem_type) or {
				c.error(err.msg, default_expr.position())
			}
		}
		if array_init.has_len {
			c.ensure_sumtype_array_has_default_value(array_init)
		}
		c.ensure_type_exists(array_init.elem_type, array_init.elem_type_pos) or {}
		return array_init.typ
	}
	if array_init.is_fixed {
		c.ensure_sumtype_array_has_default_value(array_init)
		c.ensure_type_exists(array_init.elem_type, array_init.elem_type_pos) or {}
	}
	// a = []
	if array_init.exprs.len == 0 {
		// a := fn_returing_opt_array() or { [] }
		if c.expected_type == ast.void_type && c.expected_or_type != ast.void_type {
			c.expected_type = c.expected_or_type
		}
		mut type_sym := c.table.get_type_symbol(c.expected_type)
		if type_sym.kind != .array {
			c.error('array_init: no type specified (maybe: `[]Type{}` instead of `[]`)',
				array_init.pos)
			return ast.void_type
		}
		// TODO: seperate errors once bug is fixed with `x := if expr { ... } else { ... }`
		// if c.expected_type == ast.void_type {
		// c.error('array_init: use `[]Type{}` instead of `[]`', array_init.pos)
		// return ast.void_type
		// }
		array_info := type_sym.array_info()
		array_init.elem_type = array_info.elem_type
		// clear optional flag incase of: `fn opt_arr ?[]int { return [] }`
		return c.expected_type.clear_flag(.optional)
	}
	// [1,2,3]
	if array_init.exprs.len > 0 && array_init.elem_type == ast.void_type {
		mut expected_value_type := ast.void_type
		mut expecting_interface_array := false
		if c.expected_type != 0 {
			expected_value_type = c.table.value_type(c.expected_type)
			if c.table.get_type_symbol(expected_value_type).kind == .interface_ {
				// Array of interfaces? (`[dog, cat]`) Save the interface type (`Animal`)
				expecting_interface_array = true
			}
		}
		// expecting_interface_array := c.expected_type != 0 &&
		// c.table.get_type_symbol(c.table.value_type(c.expected_type)).kind ==			.interface_
		//
		// if expecting_interface_array {
		// println('ex $c.expected_type')
		// }
		for i, expr in array_init.exprs {
			typ := c.check_expr_opt_call(expr, c.expr(expr))
			array_init.expr_types << typ
			// The first element's type
			if expecting_interface_array {
				if i == 0 {
					elem_type = expected_value_type
					c.expected_type = elem_type
					c.type_implements(typ, elem_type, expr.position())
				}
				continue
			}
			// The first element's type
			if i == 0 {
				if expr.is_auto_deref_var() {
					elem_type = c.table.mktyp(typ.deref())
				} else {
					elem_type = c.table.mktyp(typ)
				}
				c.expected_type = elem_type
				continue
			}
			c.check_expected(typ, elem_type) or {
				c.error('invalid array element: $err.msg', expr.position())
			}
		}
		if array_init.is_fixed {
			idx := c.table.find_or_register_array_fixed(elem_type, array_init.exprs.len)
			array_init.typ = ast.new_type(idx)
		} else {
			idx := c.table.find_or_register_array(elem_type)
			array_init.typ = ast.new_type(idx)
		}
		array_init.elem_type = elem_type
	} else if array_init.is_fixed && array_init.exprs.len == 1
		&& array_init.elem_type != ast.void_type {
		// [50]byte
		mut fixed_size := 0
		init_expr := array_init.exprs[0]
		c.expr(init_expr)
		match init_expr {
			ast.IntegerLiteral {
				fixed_size = init_expr.val.int()
			}
			ast.Ident {
				if init_expr.obj is ast.ConstField {
					if cint := eval_int_expr(init_expr.obj.expr, 0) {
						fixed_size = cint
					}
				} else {
					c.error('non-constant array bound `$init_expr.name`', init_expr.pos)
				}
			}
			ast.InfixExpr {
				if cint := eval_int_expr(init_expr, 0) {
					fixed_size = cint
				}
			}
			else {
				c.error('expecting `int` for fixed size', array_init.pos)
			}
		}
		if fixed_size <= 0 {
			c.error('fixed size cannot be zero or negative', init_expr.position())
		}
		idx := c.table.find_or_register_array_fixed(array_init.elem_type, fixed_size)
		array_type := ast.new_type(idx)
		array_init.typ = array_type
		if array_init.has_default {
			c.expr(array_init.default_expr)
		}
	}
	return array_init.typ
}

fn eval_int_expr(expr ast.Expr, nlevel int) ?int {
	if nlevel > 100 {
		// protect against a too deep comptime eval recursion:
		return none
	}
	match expr {
		ast.IntegerLiteral {
			return expr.val.int()
		}
		ast.InfixExpr {
			left := eval_int_expr(expr.left, nlevel + 1) ?
			right := eval_int_expr(expr.right, nlevel + 1) ?
			match expr.op {
				.plus { return left + right }
				.minus { return left - right }
				.mul { return left * right }
				.div { return left / right }
				.mod { return left % right }
				.xor { return left ^ right }
				.pipe { return left | right }
				.amp { return left & right }
				.left_shift { return left << right }
				.right_shift { return left >> right }
				else { return none }
			}
		}
		ast.Ident {
			if expr.obj is ast.ConstField {
				// an int constant?
				cint := eval_int_expr(expr.obj.expr, nlevel + 1) ?
				return cint
			}
		}
		else {
			// dump(expr)
			return none
		}
	}
	return none
}

[inline]
fn (mut c Checker) check_loop_label(label string, pos token.Position) {
	if label.len == 0 {
		// ignore
		return
	}
	if c.loop_label.len != 0 {
		c.error('nesting of labelled `for` loops is not supported', pos)
		return
	}
	c.loop_label = label
}

fn (mut c Checker) stmt(node ast.Stmt) {
	$if trace_checker ? {
		stmt_pos := node.pos
		eprintln('checking file: ${c.file.path:-30} | stmt pos: ${stmt_pos.str():-45} | stmt')
	}
	// c.expected_type = ast.void_type
	match mut node {
		ast.EmptyStmt {
			print_backtrace()
			eprintln('Checker.stmt() EmptyStmt')
		}
		ast.NodeError {}
		ast.AsmStmt {
			c.asm_stmt(mut node)
		}
		ast.AssertStmt {
			c.assert_stmt(node)
		}
		ast.AssignStmt {
			c.assign_stmt(mut node)
		}
		ast.Block {
			c.block(node)
		}
		ast.BranchStmt {
			c.branch_stmt(node)
		}
		ast.CompFor {
			typ := c.unwrap_generic(node.typ)
			sym := c.table.get_type_symbol(typ)
			if sym.kind == .placeholder || typ.has_flag(.generic) {
				c.error('unknown type `$sym.name`', node.typ_pos)
			}
			c.stmts(node.stmts)
		}
		ast.ConstDecl {
			c.inside_const = true
			c.const_decl(mut node)
			c.inside_const = false
		}
		ast.DeferStmt {
			if node.idx_in_fn < 0 {
				node.idx_in_fn = c.cur_fn.defer_stmts.len
				c.cur_fn.defer_stmts << &node
			}
			c.stmts(node.stmts)
		}
		ast.EnumDecl {
			c.enum_decl(node)
		}
		ast.ExprStmt {
			node.typ = c.expr(node.expr)
			c.expected_type = ast.void_type
			c.check_expr_opt_call(node.expr, ast.void_type)
			// TODO This should work, even if it's prolly useless .-.
			// node.typ = c.check_expr_opt_call(node.expr, ast.void_type)
		}
		ast.FnDecl {
			c.fn_decl(mut node)
		}
		ast.ForCStmt {
			c.for_c_stmt(node)
		}
		ast.ForInStmt {
			c.for_in_stmt(mut node)
		}
		ast.ForStmt {
			c.for_stmt(mut node)
		}
		ast.GlobalDecl {
			c.global_decl(node)
		}
		ast.GoStmt {
			c.go_stmt(mut node)
			if node.call_expr.or_block.kind != .absent {
				c.error('optional handling cannot be done in `go` call. Do it when calling `.wait()`',
					node.call_expr.or_block.pos)
			}
		}
		ast.GotoLabel {}
		ast.GotoStmt {
			if !c.inside_unsafe {
				c.warn('`goto` requires `unsafe` (consider using labelled break/continue)',
					node.pos)
			}
			if node.name !in c.cur_fn.label_names {
				c.error('unknown label `$node.name`', node.pos)
			}
			// TODO: check label doesn't bypass variable declarations
		}
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
			c.struct_decl(mut node)
		}
		ast.TypeDecl {
			c.type_decl(node)
		}
	}
}

fn (mut c Checker) assert_stmt(node ast.AssertStmt) {
	cur_exp_typ := c.expected_type
	assert_type := c.check_expr_opt_call(node.expr, c.expr(node.expr))
	if assert_type != ast.bool_type_idx {
		atype_name := c.table.get_type_symbol(assert_type).name
		c.error('assert can be used only with `bool` expressions, but found `$atype_name` instead',
			node.pos)
	}
	c.expected_type = cur_exp_typ
}

fn (mut c Checker) block(node ast.Block) {
	if node.is_unsafe {
		assert !c.inside_unsafe
		c.inside_unsafe = true
		c.stmts(node.stmts)
		c.inside_unsafe = false
	} else {
		c.stmts(node.stmts)
	}
}

fn (mut c Checker) branch_stmt(node ast.BranchStmt) {
	if c.in_for_count == 0 {
		c.error('$node.kind.str() statement not within a loop', node.pos)
	}
	if node.label.len > 0 {
		if node.label != c.loop_label {
			c.error('invalid label name `$node.label`', node.pos)
		}
	}
}

fn (mut c Checker) for_c_stmt(node ast.ForCStmt) {
	c.in_for_count++
	prev_loop_label := c.loop_label
	if node.has_init {
		c.stmt(node.init)
	}
	c.expr(node.cond)
	if node.has_inc {
		c.stmt(node.inc)
	}
	c.check_loop_label(node.label, node.pos)
	c.stmts(node.stmts)
	c.loop_label = prev_loop_label
	c.in_for_count--
}

fn (mut c Checker) for_in_stmt(mut node ast.ForInStmt) {
	c.in_for_count++
	prev_loop_label := c.loop_label
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
		if typ_idx in ast.integer_type_idxs && high_type_idx !in ast.integer_type_idxs {
			c.error('range types do not match', node.cond.position())
		} else if typ_idx in ast.float_type_idxs || high_type_idx in ast.float_type_idxs {
			c.error('range type can not be float', node.cond.position())
		} else if typ_idx == ast.bool_type_idx || high_type_idx == ast.bool_type_idx {
			c.error('range type can not be bool', node.cond.position())
		} else if typ_idx == ast.string_type_idx || high_type_idx == ast.string_type_idx {
			c.error('range type can not be string', node.cond.position())
		}
	} else {
		sym := c.table.get_type_symbol(typ)
		if sym.kind == .struct_ {
			// iterators
			next_fn := sym.find_method('next') or {
				c.error('a struct must have a `next()` method to be an iterator', node.cond.position())
				return
			}
			if !next_fn.return_type.has_flag(.optional) {
				c.error('iterator method `next()` must return an optional', node.cond.position())
			}
			// the receiver
			if next_fn.params.len != 1 {
				c.error('iterator method `next()` must have 0 parameters', node.cond.position())
			}
			val_type := next_fn.return_type.clear_flag(.optional)
			node.cond_type = typ
			node.kind = sym.kind
			node.val_type = val_type
			node.scope.update_var_type(node.val_var, val_type)
		} else {
			if sym.kind == .map && !(node.key_var.len > 0 && node.val_var.len > 0) {
				c.error(
					'declare a key and a value variable when ranging a map: `for key, val in map {`\n' +
					'use `_` if you do not need the variable', node.pos)
			}
			if node.key_var.len > 0 {
				key_type := match sym.kind {
					.map { sym.map_info().key_type }
					else { ast.int_type }
				}
				node.key_type = key_type
				node.scope.update_var_type(node.key_var, key_type)
			}
			mut value_type := c.table.value_type(typ)
			if value_type == ast.void_type || typ.has_flag(.optional) {
				if typ != ast.void_type {
					c.error('for in: cannot index `${c.table.type_to_str(typ)}`', node.cond.position())
				}
			}
			if node.val_is_mut {
				value_type = value_type.to_ptr()
				match node.cond {
					ast.Ident {
						if node.cond.obj is ast.Var {
							obj := node.cond.obj as ast.Var
							if !obj.is_mut {
								c.error('`$obj.name` is immutable, it cannot be changed',
									node.cond.pos)
							}
						}
					}
					ast.ArrayInit {
						c.error('array literal is immutable, it cannot be changed', node.cond.pos)
					}
					ast.MapInit {
						c.error('map literal is immutable, it cannot be changed', node.cond.pos)
					}
					else {}
				}
			}
			node.cond_type = typ
			node.kind = sym.kind
			node.val_type = value_type
			node.scope.update_var_type(node.val_var, value_type)
		}
	}
	c.check_loop_label(node.label, node.pos)
	c.stmts(node.stmts)
	c.loop_label = prev_loop_label
	c.in_for_count--
}

fn (mut c Checker) for_stmt(mut node ast.ForStmt) {
	c.in_for_count++
	prev_loop_label := c.loop_label
	c.expected_type = ast.bool_type
	typ := c.expr(node.cond)
	if !node.is_inf && typ.idx() != ast.bool_type_idx && !c.pref.translated {
		c.error('non-bool used as for condition', node.pos)
	}
	if node.cond is ast.InfixExpr {
		infix := node.cond
		if infix.op == .key_is {
			if (infix.left is ast.Ident || infix.left is ast.SelectorExpr)
				&& infix.right is ast.TypeNode {
				right_expr := infix.right as ast.TypeNode
				is_variable := if mut infix.left is ast.Ident {
					infix.left.kind == .variable
				} else {
					true
				}
				left_type := c.expr(infix.left)
				left_sym := c.table.get_type_symbol(left_type)
				if is_variable {
					if left_sym.kind == .sum_type {
						c.smartcast_sumtype(infix.left, infix.left_type, right_expr.typ, mut
							node.scope)
					}
				}
			}
		}
	}
	// TODO: update loop var type
	// how does this work currenly?
	c.check_loop_label(node.label, node.pos)
	c.stmts(node.stmts)
	c.loop_label = prev_loop_label
	c.in_for_count--
}

fn (mut c Checker) global_decl(node ast.GlobalDecl) {
	for field in node.fields {
		c.check_valid_snake_case(field.name, 'global name', field.pos)
		if field.name in c.global_names {
			c.error('duplicate global `$field.name`', field.pos)
		}
		c.global_names << field.name
	}
}

fn (mut c Checker) go_stmt(mut node ast.GoStmt) {
	c.call_expr(mut node.call_expr)
	// Make sure there are no mutable arguments
	for arg in node.call_expr.args {
		if arg.is_mut && !arg.typ.is_ptr() {
			c.error('function in `go` statement cannot contain mutable non-reference arguments',
				arg.expr.position())
		}
	}
	if node.call_expr.is_method && node.call_expr.receiver_type.is_ptr()
		&& !node.call_expr.left_type.is_ptr() {
		c.error('method in `go` statement cannot have non-reference mutable receiver',
			node.call_expr.left.position())
	}
}

fn (mut c Checker) asm_stmt(mut stmt ast.AsmStmt) {
	if stmt.is_goto {
		c.warn('inline assembly goto is not supported, it will most likely not work',
			stmt.pos)
	}
	if c.pref.backend == .js {
		c.error('inline assembly is not supported in js backend', stmt.pos)
	}
	if c.pref.backend == .c && c.pref.ccompiler_type == .msvc {
		c.error('msvc compiler does not support inline assembly', stmt.pos)
	}
	mut aliases := c.asm_ios(stmt.output, mut stmt.scope, true)
	aliases2 := c.asm_ios(stmt.input, mut stmt.scope, false)
	aliases << aliases2
	for template in stmt.templates {
		if template.is_directive {
			/*
			align n[,value]
			.skip n[,value]
			.space n[,value]
			.byte value1[,...]
			.word value1[,...]
			.short value1[,...]
			.int value1[,...]
			.long value1[,...]
			.quad immediate_value1[,...]
			.globl symbol
			.global symbol
			.section section
			.text
			.data
			.bss
			.fill repeat[,size[,value]]
			.org n
			.previous
			.string string[,...]
			.asciz string[,...]
			.ascii string[,...]
			*/
			if template.name !in ['skip', 'space', 'byte', 'word', 'short', 'int', 'long', 'quad',
				'globl', 'global', 'section', 'text', 'data', 'bss', 'fill', 'org', 'previous',
				'string', 'asciz', 'ascii'] { // all tcc supported assembler directive
				c.error('unknown assembler directive: `$template.name`', template.pos)
			}
			// if c.file in  {

			// }
		}
		for mut arg in template.args {
			c.asm_arg(arg, stmt, aliases)
		}
	}
	for mut clob in stmt.clobbered {
		c.asm_arg(clob.reg, stmt, aliases)
	}
}

fn (mut c Checker) asm_arg(arg ast.AsmArg, stmt ast.AsmStmt, aliases []string) {
	match mut arg {
		ast.AsmAlias {
			name := arg.name
			if name !in aliases && name !in stmt.local_labels && name !in c.file.global_labels {
				mut possible := aliases.clone()
				possible << stmt.local_labels
				possible << c.file.global_labels
				suggestion := util.new_suggestion(name, possible)
				c.error(suggestion.say('alias or label `$arg.name` does not exist'), arg.pos)
			}
		}
		ast.AsmAddressing {
			if arg.scale !in [-1, 1, 2, 4, 8] {
				c.error('scale must be one of 1, 2, 4, or 8', arg.pos)
			}
			c.asm_arg(arg.displacement, stmt, aliases)
			c.asm_arg(arg.base, stmt, aliases)
			c.asm_arg(arg.index, stmt, aliases)
		}
		ast.BoolLiteral {} // all of these are guarented to be correct.
		ast.FloatLiteral {}
		ast.CharLiteral {}
		ast.IntegerLiteral {}
		ast.AsmRegister {} // if the register is not found, the parser will register it as an alias
		ast.AsmDisp {}
		string {}
	}
}

fn (mut c Checker) asm_ios(ios []ast.AsmIO, mut scope ast.Scope, output bool) []string {
	mut aliases := []string{}
	for io in ios {
		typ := c.expr(io.expr)
		if output {
			c.fail_if_immutable(io.expr)
		}
		if io.alias != '' {
			aliases << io.alias
			if io.alias in scope.objects {
				scope.objects[io.alias] = ast.Var{
					name: io.alias
					expr: io.expr
					is_arg: true
					typ: typ
					orig_type: typ
					pos: io.pos
				}
			}
		}
	}

	return aliases
}

fn (mut c Checker) hash_stmt(mut node ast.HashStmt) {
	if c.skip_flags {
		return
	}
	if c.pref.backend == .js {
		if !c.file.path.ends_with('.js.v') {
			c.error('hash statements are only allowed in backend specific files such "x.js.v"',
				node.pos)
		}
		if c.mod == 'main' {
			c.error('hash statements are not allowed in the main module. Please place them in a separate module.',
				node.pos)
		}
		return
	}
	match node.kind {
		'include' {
			mut flag := node.main
			if flag.contains('@VROOT') {
				vroot := util.resolve_vroot(flag, c.file.path) or {
					c.error(err.msg, node.pos)
					return
				}
				node.val = 'include $vroot'
				node.main = vroot
				flag = vroot
			}
			if flag.contains('\$env(') {
				env := util.resolve_env_value(flag, true) or {
					c.error(err.msg, node.pos)
					return
				}
				node.main = env
			}
			flag_no_comment := flag.all_before('//').trim_space()
			if !((flag_no_comment.starts_with('"') && flag_no_comment.ends_with('"'))
				|| (flag_no_comment.starts_with('<') && flag_no_comment.ends_with('>'))) {
				c.error('including C files should use either `"header_file.h"` or `<header_file.h>` quoting',
					node.pos)
			}
		}
		'pkgconfig' {
			args := if node.main.contains('--') {
				node.main.split(' ')
			} else {
				'--cflags --libs $node.main'.split(' ')
			}
			mut m := pkgconfig.main(args) or {
				c.error(err.msg, node.pos)
				return
			}
			cflags := m.run() or {
				c.error(err.msg, node.pos)
				return
			}
			c.table.parse_cflag(cflags, c.mod, c.pref.compile_defines_all) or {
				c.error(err.msg, node.pos)
				return
			}
		}
		'flag' {
			// #flag linux -lm
			mut flag := node.main
			// expand `@VROOT` to its absolute path
			if flag.contains('@VROOT') {
				flag = util.resolve_vroot(flag, c.file.path) or {
					c.error(err.msg, node.pos)
					return
				}
			}
			if flag.contains('\$env(') {
				flag = util.resolve_env_value(flag, true) or {
					c.error(err.msg, node.pos)
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
				c.error(err.msg, node.pos)
			}
		}
		else {
			if node.kind != 'define' {
				c.error('expected `#define`, `#flag`, `#include` or `#pkgconfig` not $node.val',
					node.pos)
			}
		}
	}
}

fn (mut c Checker) import_stmt(imp ast.Import) {
	for sym in imp.syms {
		name := '${imp.mod}.$sym.name'
		if sym.name[0].is_capital() {
			if type_sym := c.table.find_type(name) {
				if type_sym.kind != .placeholder {
					if !type_sym.is_public {
						c.error('module `$imp.mod` type `$sym.name` is private', sym.pos)
					}
					continue
				}
			}
			c.error('module `$imp.mod` has no type `$sym.name`', sym.pos)
			continue
		}
		if func := c.table.find_fn(name) {
			if !func.is_pub {
				c.error('module `$imp.mod` function `${sym.name}()` is private', sym.pos)
			}
			continue
		}
		if _ := c.file.global_scope.find_const(name) {
			continue
		}
		c.error('module `$imp.mod` has no constant or function `$sym.name`', sym.pos)
	}
}

fn (mut c Checker) stmts(stmts []ast.Stmt) {
	mut unreachable := token.Position{
		line_nr: -1
	}
	c.expected_type = ast.void_type
	for stmt in stmts {
		if c.scope_returns {
			if unreachable.line_nr == -1 {
				unreachable = stmt.pos
			}
		}
		c.stmt(stmt)
	}
	if unreachable.line_nr >= 0 {
		c.error('unreachable code', unreachable)
	}
	c.scope_returns = false
	c.expected_type = ast.void_type
}

pub fn (c &Checker) unwrap_generic(typ ast.Type) ast.Type {
	if typ.has_flag(.generic) {
		sym := c.table.get_type_symbol(typ)
		for i, generic_param in c.cur_fn.generic_params {
			if generic_param.name == sym.name {
				return c.cur_generic_types[i].derive(typ).clear_flag(.generic)
			}
		}
	}
	return typ
}

// TODO node must be mut
pub fn (mut c Checker) expr(node ast.Expr) ast.Type {
	c.expr_level++
	defer {
		c.expr_level--
	}
	if c.expr_level > 200 {
		c.error('checker: too many expr levels: $c.expr_level ', node.position())
		return ast.void_type
	}
	match mut node {
		ast.NodeError {}
		ast.EmptyExpr {
			print_backtrace()
			c.error('checker.expr(): unhandled EmptyExpr', token.Position{})
		}
		ast.CTempVar {
			return node.typ
		}
		ast.AnonFn {
			c.inside_anon_fn = true
			keep_fn := c.cur_fn
			c.cur_fn = &node.decl
			c.stmts(node.decl.stmts)
			c.fn_decl(mut node.decl)
			c.cur_fn = keep_fn
			c.inside_anon_fn = false
			return node.typ
		}
		ast.ArrayDecompose {
			typ := c.expr(node.expr)
			type_sym := c.table.get_type_symbol(typ)
			if type_sym.kind != .array {
				c.error('decomposition can only be used on arrays', node.expr.position())
				return ast.void_type
			}
			array_info := type_sym.info as ast.Array
			elem_type := array_info.elem_type.set_flag(.variadic)
			node.expr_type = typ
			node.arg_type = elem_type
			return elem_type
		}
		ast.ArrayInit {
			return c.array_init(mut node)
		}
		ast.AsCast {
			node.expr_type = c.expr(node.expr)
			expr_type_sym := c.table.get_type_symbol(node.expr_type)
			type_sym := c.table.get_type_symbol(node.typ)
			if expr_type_sym.kind == .sum_type {
				c.ensure_type_exists(node.typ, node.pos) or {}
				if !c.table.sumtype_has_variant(node.expr_type, node.typ) {
					c.error('cannot cast `$expr_type_sym.name` to `$type_sym.name`', node.pos)
					// c.error('only $info.variants can be casted to `$typ`', node.pos)
				}
			} else {
				mut s := 'cannot cast non-sum type `$expr_type_sym.name` using `as`'
				if type_sym.kind == .sum_type {
					s += ' - use e.g. `${type_sym.name}(some_expr)` instead.'
				}
				c.error(s, node.pos)
			}
			if expr_type_sym.kind == .sum_type {
				return node.typ
			}
			return node.typ.to_ptr()
		}
		ast.Assoc {
			v := node.scope.find_var(node.var_name) or { panic(err) }
			for i, _ in node.fields {
				c.expr(node.exprs[i])
			}
			node.typ = v.typ
			return v.typ
		}
		ast.BoolLiteral {
			return ast.bool_type
		}
		ast.CastExpr {
			return c.cast_expr(mut node)
		}
		ast.CallExpr {
			mut ret_type := c.call_expr(mut node)
			if !ret_type.has_flag(.optional) {
				if node.or_block.kind == .block {
					c.error('unexpected `or` block, the function `$node.name` does not return an optional',
						node.or_block.pos)
				} else if node.or_block.kind == .propagate {
					c.error('unexpected `?`, the function `$node.name` does not return an optional',
						node.or_block.pos)
				}
			}
			if ret_type.has_flag(.optional) && node.or_block.kind != .absent {
				ret_type = ret_type.clear_flag(.optional)
			}
			return ret_type
		}
		ast.GoExpr {
			mut ret_type := c.call_expr(mut node.go_stmt.call_expr)
			if node.go_stmt.call_expr.or_block.kind != .absent {
				c.error('optional handling cannot be done in `go` call. Do it when calling `.wait()`',
					node.go_stmt.call_expr.or_block.pos)
			}
			return c.table.find_or_register_thread(ret_type)
		}
		ast.ChanInit {
			return c.chan_init(mut node)
		}
		ast.CharLiteral {
			// return int_literal, not rune, so that we can do "bytes << `A`" without a cast etc
			// return ast.int_literal_type
			return ast.rune_type
			// return ast.byte_type
		}
		ast.Comment {
			return ast.void_type
		}
		ast.AtExpr {
			return c.at_expr(mut node)
		}
		ast.ComptimeCall {
			return c.comptime_call(mut node)
		}
		ast.ComptimeSelector {
			node.left_type = c.unwrap_generic(c.expr(node.left))
			expr_type := c.unwrap_generic(c.expr(node.field_expr))
			expr_sym := c.table.get_type_symbol(expr_type)
			if expr_type != ast.string_type {
				c.error('expected `string` instead of `$expr_sym.name` (e.g. `field.name`)',
					node.field_expr.position())
			}
			if node.field_expr is ast.SelectorExpr {
				left_pos := node.field_expr.expr.position()
				if c.comptime_fields_type.len == 0 {
					c.error('compile time field access can only be used when iterating over `T.fields`',
						left_pos)
				}
				expr_name := node.field_expr.expr.str()
				if expr_name in c.comptime_fields_type {
					return c.comptime_fields_type[expr_name]
				}
				c.error('unknown `\$for` variable `$expr_name`', left_pos)
			} else {
				c.error('expected selector expression e.g. `$(field.name)`', node.field_expr.position())
			}
			return ast.void_type
		}
		ast.ConcatExpr {
			return c.concat_expr(mut node)
		}
		ast.DumpExpr {
			node.expr_type = c.expr(node.expr)
			if node.expr_type.idx() == ast.void_type_idx {
				c.error('dump expression can not be void', node.expr.position())
				return ast.void_type
			}
			tsym := c.table.get_type_symbol(node.expr_type)
			c.table.dumps[int(node.expr_type)] = tsym.cname
			node.cname = tsym.cname
			return node.expr_type
		}
		ast.EnumVal {
			return c.enum_val(mut node)
		}
		ast.FloatLiteral {
			return ast.float_literal_type
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
				mut no_opt := true
				match mut node.expr {
					ast.IndexExpr {
						no_opt = false
						node.expr_type = node.expr_type.set_flag(.optional)
						node.expr.is_option = true
					}
					ast.PrefixExpr {
						if node.expr.op == .arrow {
							no_opt = false
							node.expr_type = node.expr_type.set_flag(.optional)
							node.expr.is_option = true
						}
					}
					else {}
				}
				if no_opt {
					c.error('expression should return an option', node.expr.position())
				}
			}
			return ast.bool_type
		}
		ast.IndexExpr {
			return c.index_expr(mut node)
		}
		ast.InfixExpr {
			return c.infix_expr(mut node)
		}
		ast.IntegerLiteral {
			return ast.int_literal_type
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
			return c.prefix_expr(mut node)
		}
		ast.None {
			return ast.none_type
		}
		ast.OrExpr {
			// never happens
			return ast.void_type
		}
		// ast.OrExpr2 {
		// return node.typ
		// }
		ast.ParExpr {
			return c.expr(node.expr)
		}
		ast.RangeExpr {
			// never happens
			return ast.void_type
		}
		ast.SelectExpr {
			return c.select_expr(mut node)
		}
		ast.SelectorExpr {
			return c.selector_expr(mut node)
		}
		ast.SizeOf {
			if !node.is_type {
				node.typ = c.expr(node.expr)
			}
			return ast.u32_type
		}
		ast.OffsetOf {
			return c.offset_of(node)
		}
		ast.SqlExpr {
			return c.sql_expr(mut node)
		}
		ast.StringLiteral {
			if node.language == .c {
				return ast.byteptr_type
			}
			return ast.string_type
		}
		ast.StringInterLiteral {
			return c.string_inter_lit(mut node)
		}
		ast.StructInit {
			if node.unresolved {
				return c.expr(ast.resolve_init(node, c.unwrap_generic(node.typ), c.table))
			}
			return c.struct_init(mut node)
		}
		ast.TypeNode {
			return node.typ
		}
		ast.TypeOf {
			node.expr_type = c.expr(node.expr)
			return ast.string_type
		}
		ast.UnsafeExpr {
			return c.unsafe_expr(mut node)
		}
		ast.Likely {
			ltype := c.expr(node.expr)
			if !c.check_types(ltype, ast.bool_type) {
				ltype_sym := c.table.get_type_symbol(ltype)
				lname := if node.is_likely { '_likely_' } else { '_unlikely_' }
				c.error('`${lname}()` expects a boolean expression, instead it got `$ltype_sym.name`',
					node.pos)
			}
			return ast.bool_type
		}
	}
	return ast.void_type
}

// pub fn (mut c Checker) asm_reg(mut node ast.AsmRegister) ast.Type {
// 	name := node.name

// 	for bit_size, array in ast.x86_no_number_register_list {
// 		if name in array {
// 			return c.table.bitsize_to_type(bit_size)
// 		}
// 	}
// 	for bit_size, array in ast.x86_with_number_register_list {
// 		if name in array {
// 			return c.table.bitsize_to_type(bit_size)
// 		}
// 	}
// 	c.error('invalid register name: `$name`', node.pos)
// 	return ast.void_type
// }

pub fn (mut c Checker) cast_expr(mut node ast.CastExpr) ast.Type {
	node.expr_type = c.expr(node.expr) // type to be casted
	from_type_sym := c.table.get_type_symbol(node.expr_type)
	to_type_sym := c.table.get_type_symbol(node.typ) // type to be used as cast
	if to_type_sym.language != .c {
		c.ensure_type_exists(node.typ, node.pos) or {}
	}
	expr_is_ptr := node.expr_type.is_ptr() || node.expr_type.idx() in ast.pointer_type_idxs
	if expr_is_ptr && to_type_sym.kind == .string && !node.in_prexpr {
		if node.has_arg {
			c.warn('to convert a C string buffer pointer to a V string, please use x.vstring_with_len(len) instead of string(x,len)',
				node.pos)
		} else {
			c.warn('to convert a C string buffer pointer to a V string, please use x.vstring() instead of string(x)',
				node.pos)
		}
	}
	if node.expr_type == ast.byte_type && to_type_sym.kind == .string {
		c.error('can not cast type `byte` to string, use `${node.expr.str()}.str()` instead.',
			node.pos)
	}
	if to_type_sym.kind == .sum_type {
		if node.expr_type in [ast.int_literal_type, ast.float_literal_type] {
			node.expr_type = c.promote_num(node.expr_type, if node.expr_type == ast.int_literal_type {
				ast.int_type
			} else {
				ast.f64_type
			})
		}
		if !c.table.sumtype_has_variant(node.typ, node.expr_type) && !node.typ.has_flag(.optional) {
			c.error('cannot cast `$from_type_sym.name` to `$to_type_sym.name`', node.pos)
		}
	} else if mut to_type_sym.info is ast.Alias {
		if !c.check_types(node.expr_type, to_type_sym.info.parent_type) {
			parent_type_sym := c.table.get_type_symbol(to_type_sym.info.parent_type)
			c.error('cannot convert type `$from_type_sym.name` to `$to_type_sym.name` (alias to `$parent_type_sym.name`)',
				node.pos)
		}
	} else if node.typ == ast.string_type
		&& (from_type_sym.kind in [.int_literal, .int, .byte, .byteptr, .bool]
		|| (from_type_sym.kind == .array && from_type_sym.name == 'array_byte')) {
		type_name := c.table.type_to_str(node.expr_type)
		c.error('cannot cast type `$type_name` to string, use `x.str()` instead', node.pos)
	} else if node.expr_type == ast.string_type {
		if to_type_sym.kind != .alias {
			mut error_msg := 'cannot cast a string'
			if mut node.expr is ast.StringLiteral {
				if node.expr.val.len == 1 {
					error_msg += ", for denoting characters use `$node.expr.val` instead of '$node.expr.val'"
				}
			}
			c.error(error_msg, node.pos)
		}
	} else if to_type_sym.kind == .byte && node.expr_type != ast.voidptr_type
		&& from_type_sym.kind != .enum_ && !node.expr_type.is_int() && !node.expr_type.is_float()
		&& !node.expr_type.is_ptr() {
		type_name := c.table.type_to_str(node.expr_type)
		c.error('cannot cast type `$type_name` to `byte`', node.pos)
	} else if to_type_sym.kind == .struct_ && !node.typ.is_ptr()
		&& !(to_type_sym.info as ast.Struct).is_typedef {
		// For now we ignore C typedef because of `C.Window(C.None)` in vlib/clipboard
		if from_type_sym.kind == .struct_ && !node.expr_type.is_ptr() {
			c.warn('casting to struct is deprecated, use e.g. `Struct{...expr}` instead',
				node.pos)
			from_type_info := from_type_sym.info as ast.Struct
			to_type_info := to_type_sym.info as ast.Struct
			if !c.check_struct_signature(from_type_info, to_type_info) {
				c.error('cannot convert struct `$from_type_sym.name` to struct `$to_type_sym.name`',
					node.pos)
			}
		} else {
			type_name := c.table.type_to_str(node.expr_type)
			c.error('cannot cast `$type_name` to struct', node.pos)
		}
	} else if to_type_sym.kind == .interface_ {
		c.type_implements(node.expr_type, node.typ, node.pos)
	} else if node.typ == ast.bool_type {
		c.error('cannot cast to bool - use e.g. `some_int != 0` instead', node.pos)
	} else if node.expr_type == ast.none_type && !node.typ.has_flag(.optional) {
		type_name := c.table.type_to_str(node.typ)
		c.error('cannot cast `none` to `$type_name`', node.pos)
	} else if from_type_sym.kind == .struct_ && !node.expr_type.is_ptr() {
		if (node.typ.is_ptr() || to_type_sym.kind !in [.sum_type, .interface_]) && !c.is_builtin_mod {
			type_name := c.table.type_to_str(node.typ)
			c.error('cannot cast struct to `$type_name`', node.pos)
		}
	} else if node.expr_type.has_flag(.optional) || node.expr_type.has_flag(.variadic) {
		// variadic case can happen when arrays are converted into variadic
		msg := if node.expr_type.has_flag(.optional) { 'an optional' } else { 'a variadic' }
		c.error('cannot type cast $msg', node.pos)
	} else if !c.inside_unsafe && node.typ.is_ptr() && node.expr_type.is_ptr() {
		ft := c.table.type_to_str(node.expr_type)
		tt := c.table.type_to_str(node.typ)
		c.warn('casting `$ft` to `$tt` is only allowed in `unsafe` code', node.pos)
	} else if from_type_sym.kind == .array_fixed && !node.expr_type.is_ptr() {
		c.warn('cannot cast a fixed array (use e.g. `&arr[0]` instead)', node.pos)
	}
	if node.has_arg {
		c.expr(node.arg)
	}
	node.typname = c.table.get_type_symbol(node.typ).name
	return node.typ
}

fn (mut c Checker) comptime_call(mut node ast.ComptimeCall) ast.Type {
	node.sym = c.table.get_type_symbol(c.unwrap_generic(c.expr(node.left)))
	if node.is_env {
		env_value := util.resolve_env_value("\$env('$node.args_var')", false) or {
			c.error(err.msg, node.env_pos)
			return ast.string_type
		}
		node.env_value = env_value
		return ast.string_type
	}
	if node.is_embed {
		c.file.embedded_files << node.embed_file
		return c.table.find_type_idx('v.embed_file.EmbedFileData')
	}
	if node.is_vweb {
		// TODO assoc parser bug
		pref_ := *c.pref
		pref2 := &pref.Preferences{
			...pref_
			is_vweb: true
		}
		mut c2 := new_checker(c.table, pref2)
		c2.check(node.vweb_tmpl)
		mut i := 0 // tmp counter var for skipping first three tmpl vars
		for k, _ in c2.file.scope.children[0].objects {
			if i < 2 {
				// Skip first three because they are tmpl vars see vlib/vweb/tmpl/tmpl.v
				i++
				continue
			}
			if k in c.fn_scope.objects && c.fn_scope.objects[k] is ast.Var {
				mut vsc := c.fn_scope.objects[k] as ast.Var
				vsc.is_used = true
				c.fn_scope.objects[k] = vsc
			}
		}
		c.warnings << c2.warnings
		c.errors << c2.errors
		c.notices << c2.notices
		c.nr_warnings += c2.nr_warnings
		c.nr_errors += c2.nr_errors
		c.nr_notices += c2.nr_notices
	}
	if node.method_name == 'html' {
		rtyp := c.table.find_type_idx('vweb.Result')
		node.result_type = rtyp
		return rtyp
	}
	if node.method_name == 'method' {
		for i, arg in node.args {
			// check each arg expression
			node.args[i].typ = c.expr(arg.expr)
		}
		// assume string for now
		return ast.string_type
	}
	if node.is_vweb {
		return ast.string_type
	}
	// s.$my_str()
	v := node.scope.find_var(node.method_name) or {
		c.error('unknown identifier `$node.method_name`', node.method_pos)
		return ast.void_type
	}
	if v.typ != ast.string_type {
		s := c.expected_msg(v.typ, ast.string_type)
		c.error('invalid string method call: $s', node.method_pos)
		return ast.void_type
	}
	// note: we should use a compile-time evaluation function rather than handle here
	// mut variables will not work after init
	mut method_name := ''
	if v.expr is ast.StringLiteral {
		method_name = v.expr.val
	} else {
		c.error('todo: not a string literal', node.method_pos)
	}
	f := node.sym.find_method(method_name) or {
		c.error('could not find method `$method_name`', node.method_pos)
		return ast.void_type
	}
	// println(f.name + ' ' + c.table.type_to_str(f.return_type))
	node.result_type = f.return_type
	return f.return_type
}

fn (mut c Checker) at_expr(mut node ast.AtExpr) ast.Type {
	match node.kind {
		.fn_name {
			node.val = c.cur_fn.name.all_after_last('.')
		}
		.method_name {
			fname := c.cur_fn.name.all_after_last('.')
			if c.cur_fn.is_method {
				node.val = c.table.type_to_str(c.cur_fn.receiver.typ).all_after_last('.') + '.' +
					fname
			} else {
				node.val = fname
			}
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
			node.val = (node.pos.col + 1).str()
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
				vmod_content := os.read_file(vmod_file_location.vmod_file) or { '' }
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
	return ast.string_type
}

pub fn (mut c Checker) ident(mut ident ast.Ident) ast.Type {
	// TODO: move this
	if c.const_deps.len > 0 {
		mut name := ident.name
		if !name.contains('.') && ident.mod != 'builtin' {
			name = '${ident.mod}.$ident.name'
		}
		if name == c.const_decl {
			c.error('cycle in constant `$c.const_decl`', ident.pos)
			return ast.void_type
		}
		c.const_deps << name
	}
	if ident.kind == .blank_ident {
		if ident.tok_kind !in [.assign, .decl_assign] {
			c.error('undefined ident: `_` (may only be used in assignments)', ident.pos)
		}
		return ast.void_type
	}
	// second use
	if ident.kind in [.constant, .global, .variable] {
		info := ident.info as ast.IdentVar
		// Got a var with type T, return current generic type
		return info.typ
	} else if ident.kind == .function {
		info := ident.info as ast.IdentFn
		return info.typ
	} else if ident.kind == .unresolved {
		// first use
		if ident.tok_kind == .assign && ident.is_mut {
			c.error('`mut` not allowed with `=` (use `:=` to declare a variable)', ident.pos)
		}
		if obj := ident.scope.find(ident.name) {
			match mut obj {
				ast.GlobalField {
					ident.kind = .global
					ident.info = ast.IdentVar{
						typ: obj.typ
					}
					ident.obj = obj
					return obj.typ
				}
				ast.Var {
					// incase var was not marked as used yet (vweb tmpl)
					obj.is_used = true
					if ident.pos.pos < obj.pos.pos {
						c.error('undefined variable `$ident.name` (used before declaration)',
							ident.pos)
					}
					is_sum_type_cast := obj.sum_type_casts.len != 0
						&& !c.prevent_sum_type_unwrapping_once
					c.prevent_sum_type_unwrapping_once = false
					mut typ := if is_sum_type_cast { obj.sum_type_casts.last() } else { obj.typ }
					if typ == 0 {
						if mut obj.expr is ast.Ident {
							if obj.expr.kind == .unresolved {
								c.error('unresolved variable: `$ident.name`', ident.pos)
								return ast.void_type
							}
						}
						if mut obj.expr is ast.IfGuardExpr {
							// new variable from if guard shouldn't have the optional flag for further use
							// a temp variable will be generated which unwraps it
							if_guard_var_type := c.expr(obj.expr.expr)
							typ = if_guard_var_type.clear_flag(.optional)
						} else {
							typ = c.expr(obj.expr)
						}
					}
					is_optional := typ.has_flag(.optional)
					ident.kind = .variable
					ident.info = ast.IdentVar{
						typ: typ
						is_optional: is_optional
					}
					if typ == ast.error_type && c.expected_type == ast.string_type
						&& !c.using_new_err_struct && !c.inside_selector_expr
						&& !c.inside_println_arg && !c.file.mod.name.contains('v.')
						&& !c.is_builtin_mod {
						//                          ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ <- TODO: remove; this prevents a failure in the `performance-regressions` CI job
						c.warn('string errors are deprecated; use `err.msg` instead',
							ident.pos)
					}
					// if typ == ast.t_type {
					// sym := c.table.get_type_symbol(c.cur_generic_type)
					// println('IDENT T unresolved $ident.name typ=$sym.name')
					// Got a var with type T, return current generic type
					// typ = c.cur_generic_type
					// }
					// } else {
					if !is_sum_type_cast {
						obj.typ = typ
					}
					ident.obj = obj
					// unwrap optional (`println(x)`)
					if is_optional {
						return typ.clear_flag(.optional)
					}
					return typ
				}
				else {}
			}
		}
		mut name := ident.name
		// check for imported symbol
		if name in c.file.imported_symbols {
			name = c.file.imported_symbols[name]
		}
		// prepend mod to look for fn call or const
		else if !name.contains('.') && ident.mod != 'builtin' {
			name = '${ident.mod}.$ident.name'
		}
		if obj := c.file.global_scope.find(name) {
			match mut obj {
				ast.ConstField {
					if !(obj.is_pub || obj.mod == c.mod || c.pref.is_test) {
						c.error('constant `$obj.name` is private', ident.pos)
					}
					mut typ := obj.typ
					if typ == 0 {
						c.inside_const = true
						typ = c.expr(obj.expr)
						c.inside_const = false
						if obj.expr is ast.CallExpr {
							if obj.expr.or_block.kind != .absent {
								typ = typ.clear_flag(.optional)
							}
						}
					}
					ident.name = name
					ident.kind = .constant
					ident.info = ast.IdentVar{
						typ: typ
					}
					obj.typ = typ
					ident.obj = obj
					return typ
				}
				else {}
			}
		}
		// Non-anon-function object (not a call), e.g. `onclick(my_click)`
		if func := c.table.find_fn(name) {
			fn_type := ast.new_type(c.table.find_or_register_fn_type(ident.mod, func,
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
		if ident.name == 'C.NULL' {
			return ast.voidptr_type
		}
		return ast.int_type
	}
	if c.inside_sql {
		if field := c.table.find_field(c.cur_orm_ts, ident.name) {
			return field.typ
		}
	}
	if ident.kind == .unresolved && ident.mod != 'builtin' {
		// search in the `builtin` idents, for example
		// main.compare_f32 may actually be builtin.compare_f32
		saved_mod := ident.mod
		ident.mod = 'builtin'
		builtin_type := c.ident(mut ident)
		if builtin_type != ast.void_type {
			return builtin_type
		}
		ident.mod = saved_mod
	}
	if ident.tok_kind == .assign {
		c.error('undefined ident: `$ident.name` (use `:=` to declare a variable)', ident.pos)
	} else if ident.name == 'errcode' {
		c.error('undefined ident: `errcode`; did you mean `err.code`?', ident.pos)
	} else {
		c.error('undefined ident: `$ident.name`', ident.pos)
	}
	if c.table.known_type(ident.name) {
		// e.g. `User`  in `json.decode(User, '...')`
		return ast.void_type
	}
	return ast.void_type
}

pub fn (mut c Checker) concat_expr(mut concat_expr ast.ConcatExpr) ast.Type {
	mut mr_types := []ast.Type{}
	for expr in concat_expr.vals {
		mr_types << c.expr(expr)
	}
	if concat_expr.vals.len == 1 {
		typ := mr_types[0]
		concat_expr.return_type = typ
		return typ
	} else {
		typ := c.table.find_or_register_multi_return(mr_types)
		ast.new_type(typ)
		concat_expr.return_type = typ
		return typ
	}
}

pub fn (mut c Checker) match_expr(mut node ast.MatchExpr) ast.Type {
	node.is_expr = c.expected_type != ast.void_type
	node.expected_type = c.expected_type
	cond_type := c.expr(node.cond)
	// we setting this here rather than at the end of the method
	// since it is used in c.match_exprs() it saves checking twice
	node.cond_type = c.table.mktyp(cond_type)
	c.ensure_type_exists(node.cond_type, node.pos) or { return ast.void_type }
	cond_type_sym := c.table.get_type_symbol(cond_type)
	if cond_type_sym.kind !in [.interface_, .sum_type] {
		node.is_sum_type = false
	}
	c.match_exprs(mut node, cond_type_sym)
	c.expected_type = cond_type
	mut ret_type := ast.void_type
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
				st.check_c_expr() or { c.error('`match` expression branch has $err.msg', st.pos) }
			}
		}
		// If the last statement is an expression, return its type
		if branch.stmts.len > 0 {
			mut stmt := branch.stmts[branch.stmts.len - 1]
			match mut stmt {
				ast.ExprStmt {
					expr_type := c.expr(stmt.expr)
					if ret_type == ast.void_type {
						ret_type = expr_type
						stmt.typ = ret_type
					} else if node.is_expr && ret_type != expr_type {
						if !c.check_types(ret_type, expr_type) {
							ret_sym := c.table.get_type_symbol(ret_type)
							c.error('return type mismatch, it should be `$ret_sym.name`',
								stmt.expr.position())
						}
					}
				}
				else {
					// TODO: ask alex about this
					// typ := c.expr(stmt.expr)
					// type_sym := c.table.get_type_symbol(typ)
					// p.warn('match expr ret $type_sym.name')
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
	// if ret_type != ast.void_type {
	// node.is_expr = c.expected_type != ast.void_type
	// node.expected_type = c.expected_type
	// }
	node.return_type = ret_type
	return ret_type
}

fn (mut c Checker) match_exprs(mut node ast.MatchExpr, cond_type_sym ast.TypeSymbol) {
	// branch_exprs is a histogram of how many times
	// an expr was used in the match
	mut branch_exprs := map[string]int{}
	for branch_i, _ in node.branches {
		mut branch := node.branches[branch_i]
		mut expr_types := []ast.TypeNode{}
		for expr in branch.exprs {
			mut key := ''
			if expr is ast.RangeExpr {
				mut low := i64(0)
				mut high := i64(0)
				c.expected_type = node.expected_type
				low_expr := expr.low
				high_expr := expr.high
				if low_expr is ast.IntegerLiteral {
					if high_expr is ast.IntegerLiteral {
						low = low_expr.val.i64()
						high = high_expr.val.i64()
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
				high_low_cutoff := 1000
				if high - low > high_low_cutoff {
					c.warn('more than $high_low_cutoff possibilities ($low ... $high) in match range',
						branch.pos)
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
				ast.TypeNode {
					key = c.table.type_to_str(expr.typ)
					expr_types << expr
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
			if expr_type.idx() == 0 {
				// parser failed, stop checking
				return
			}
			if cond_type_sym.kind == .interface_ {
				// TODO
				// This generates a memory issue with TCC
				// Needs to be checked later when TCC errors are fixed
				// Current solution is to move expr.position() to its own statement
				// c.type_implements(expr_type, c.expected_type, expr.position())
				expr_pos := expr.position()
				c.type_implements(expr_type, c.expected_type, expr_pos)
			} else if mut cond_type_sym.info is ast.SumType {
				if expr_type !in cond_type_sym.info.variants {
					expr_str := c.table.type_to_str(expr_type)
					expect_str := c.table.type_to_str(node.cond_type)
					c.error('`$expect_str` has no variant `$expr_str`', expr.position())
				}
			} else if !c.check_types(expr_type, node.cond_type) {
				expr_str := c.table.type_to_str(expr_type)
				expect_str := c.table.type_to_str(node.cond_type)
				c.error('cannot match `$expr_str` with `$expect_str` condition', expr.position())
			}
			branch_exprs[key] = val + 1
		}
		// when match is sum type matching, then register smart cast for every branch
		if expr_types.len > 0 {
			if cond_type_sym.kind == .sum_type {
				mut expr_type := ast.Type(0)
				if expr_types.len > 1 {
					mut agg_name := strings.new_builder(20)
					mut agg_cname := strings.new_builder(20)
					agg_name.write_string('(')
					for i, expr in expr_types {
						if i > 0 {
							agg_name.write_string(' | ')
							agg_cname.write_string('___')
						}
						type_str := c.table.type_to_str(expr.typ)
						name := if c.is_builtin_mod { type_str } else { '${c.mod}.$type_str' }
						agg_name.write_string(name)
						agg_cname.write_string(util.no_dots(name))
					}
					agg_name.write_string(')')
					name := agg_name.str()
					existing_idx := c.table.type_idxs[name]
					if existing_idx > 0 {
						expr_type = existing_idx
					} else {
						expr_type = c.table.register_type_symbol(ast.TypeSymbol{
							name: name
							cname: agg_cname.str()
							kind: .aggregate
							mod: c.mod
							info: ast.Aggregate{
								types: expr_types.map(it.typ)
							}
						})
					}
				} else {
					expr_type = expr_types[0].typ
				}
				c.smartcast_sumtype(node.cond, node.cond_type, expr_type, mut branch.scope)
			}
		}
	}
	// check that expressions are exhaustive
	// this is achieved either by putting an else
	// or, when the match is on a sum type or an enum
	// by listing all variants or values
	mut is_exhaustive := true
	mut unhandled := []string{}
	if node.cond_type == ast.bool_type {
		variants := ['true', 'false']
		for v in variants {
			if v !in branch_exprs {
				is_exhaustive = false
				unhandled << '`$v`'
			}
		}
	} else {
		match mut cond_type_sym.info {
			ast.SumType {
				for v in cond_type_sym.info.variants {
					v_str := c.table.type_to_str(v)
					if v_str !in branch_exprs {
						is_exhaustive = false
						unhandled << '`$v_str`'
					}
				}
			}
			//
			ast.Enum {
				for v in cond_type_sym.info.vals {
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
		if unhandled.len < c.match_exhaustive_cutoff_limit {
			err_details += unhandled.join(', ')
		} else {
			remaining := unhandled.len - c.match_exhaustive_cutoff_limit
			err_details += unhandled[0..c.match_exhaustive_cutoff_limit].join(', ')
			err_details += ', and $remaining others ...'
		}
		err_details += ' or `else {}` at the end)'
	} else {
		err_details += ' (add `else {}` at the end)'
	}
	c.error(err_details, node.pos)
}

// smartcast takes the expression with the current type which should be smartcasted to the target type in the given scope
fn (c &Checker) smartcast_sumtype(expr ast.Expr, cur_type ast.Type, to_type ast.Type, mut scope ast.Scope) {
	match mut expr {
		ast.SelectorExpr {
			mut is_mut := false
			mut sum_type_casts := []ast.Type{}
			expr_sym := c.table.get_type_symbol(expr.expr_type)
			mut orig_type := 0
			if field := c.table.find_field(expr_sym, expr.field_name) {
				if field.is_mut {
					root_ident := expr.root_ident()
					if v := scope.find_var(root_ident.name) {
						is_mut = v.is_mut
					}
				}
				if orig_type == 0 {
					orig_type = field.typ
				}
			}
			if field := scope.find_struct_field(expr.expr_type, expr.field_name) {
				sum_type_casts << field.sum_type_casts
			}
			// smartcast either if the value is immutable or if the mut argument is explicitly given
			if !is_mut || expr.is_mut {
				sum_type_casts << to_type
				scope.register_struct_field(ast.ScopeStructField{
					struct_type: expr.expr_type
					name: expr.field_name
					typ: cur_type
					sum_type_casts: sum_type_casts
					pos: expr.pos
					orig_type: orig_type
				})
			}
		}
		ast.Ident {
			mut is_mut := false
			mut sum_type_casts := []ast.Type{}
			mut is_already_casted := false
			mut orig_type := 0
			if mut expr.obj is ast.Var {
				is_mut = expr.obj.is_mut
				sum_type_casts << expr.obj.sum_type_casts
				is_already_casted = expr.obj.pos.pos == expr.pos.pos
				if orig_type == 0 {
					orig_type = expr.obj.typ
				}
			}
			// smartcast either if the value is immutable or if the mut argument is explicitly given
			if (!is_mut || expr.is_mut) && !is_already_casted {
				sum_type_casts << to_type
				scope.register(ast.Var{
					name: expr.name
					typ: cur_type
					pos: expr.pos
					is_used: true
					is_mut: expr.is_mut
					sum_type_casts: sum_type_casts
					orig_type: orig_type
				})
			}
		}
		else {}
	}
}

pub fn (mut c Checker) select_expr(mut node ast.SelectExpr) ast.Type {
	node.is_expr = c.expected_type != ast.void_type
	node.expected_type = c.expected_type
	for branch in node.branches {
		c.stmt(branch.stmt)
		match branch.stmt {
			ast.ExprStmt {
				if branch.is_timeout {
					if !branch.stmt.typ.is_int() {
						tsym := c.table.get_type_symbol(branch.stmt.typ)
						c.error('invalid type `$tsym.name` for timeout - expected integer type aka `time.Duration`',
							branch.stmt.pos)
					}
				} else {
					if branch.stmt.expr is ast.InfixExpr {
						if branch.stmt.expr.left !is ast.Ident
							&& branch.stmt.expr.left !is ast.SelectorExpr
							&& branch.stmt.expr.left !is ast.IndexExpr {
							c.error('channel in `select` key must be predefined', branch.stmt.expr.left.position())
						}
					} else {
						c.error('invalid expression for `select` key', branch.stmt.expr.position())
					}
				}
			}
			ast.AssignStmt {
				expr := branch.stmt.right[0]
				match expr {
					ast.PrefixExpr {
						if expr.right !is ast.Ident && expr.right !is ast.SelectorExpr
							&& expr.right !is ast.IndexExpr {
							c.error('channel in `select` key must be predefined', expr.right.position())
						}
						if expr.or_block.kind != .absent {
							err_prefix := if expr.or_block.kind == .block {
								'or block'
							} else {
								'error propagation'
							}
							c.error('$err_prefix not allowed in `select` key', expr.or_block.pos)
						}
					}
					else {
						c.error('`<-` receive expression expected', branch.stmt.right[0].position())
					}
				}
			}
			else {
				if !branch.is_else {
					c.error('receive or send statement expected as `select` key', branch.stmt.pos)
				}
			}
		}
		c.stmts(branch.stmts)
	}
	return ast.bool_type
}

pub fn (mut c Checker) lock_expr(mut node ast.LockExpr) ast.Type {
	if c.rlocked_names.len > 0 || c.locked_names.len > 0 {
		c.error('nested `lock`/`rlock` not allowed', node.pos)
	}
	for i in 0 .. node.lockeds.len {
		c.ident(mut node.lockeds[i])
		id := node.lockeds[i]
		if mut id.obj is ast.Var {
			if id.obj.typ.share() != .shared_t {
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
		if node.is_rlock[i] {
			c.rlocked_names << id.name
		} else {
			c.locked_names << id.name
		}
	}
	c.stmts(node.stmts)
	c.rlocked_names = []
	c.locked_names = []
	// handle `x := rlock a { a.getval() }`
	mut ret_type := ast.void_type
	if node.stmts.len > 0 {
		last_stmt := node.stmts[node.stmts.len - 1]
		if last_stmt is ast.ExprStmt {
			ret_type = last_stmt.typ
		}
	}
	if ret_type != ast.void_type {
		node.is_expr = true
	}
	node.typ = ret_type
	return ret_type
}

pub fn (mut c Checker) unsafe_expr(mut node ast.UnsafeExpr) ast.Type {
	assert !c.inside_unsafe
	c.inside_unsafe = true
	t := c.expr(node.expr)
	c.inside_unsafe = false
	return t
}

pub fn (mut c Checker) if_expr(mut node ast.IfExpr) ast.Type {
	if_kind := if node.is_comptime { '\$if' } else { 'if' }
	expr_required := c.expected_type != ast.void_type
	former_expected_type := c.expected_type
	node.typ = ast.void_type
	mut nbranches_with_return := 0
	mut nbranches_without_return := 0
	mut should_skip := false // Whether the current branch should be skipped
	mut found_branch := false // Whether a matching branch was found- skip the rest
	mut is_comptime_type_is_expr := false // if `$if T is string`
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
				c.expected_type = ast.bool_type
				cond_typ := c.expr(branch.cond)
				if cond_typ.idx() != ast.bool_type_idx && !c.pref.translated {
					typ_sym := c.table.get_type_symbol(cond_typ)
					c.error('non-bool type `$typ_sym.name` used as if condition', branch.cond.position())
				}
			}
		}
		if node.is_comptime { // Skip checking if needed
			// smartcast field type on comptime if
			mut comptime_field_name := ''
			if branch.cond is ast.InfixExpr {
				if branch.cond.op == .key_is {
					if branch.cond.right !is ast.TypeNode {
						c.error('invalid `\$if` condition: expected a type', branch.cond.right.position())
						return 0
					}
					got_type := c.unwrap_generic((branch.cond.right as ast.TypeNode).typ)
					sym := c.table.get_type_symbol(got_type)
					if sym.kind == .placeholder || got_type.has_flag(.generic) {
						c.error('unknown type `$sym.name`', branch.cond.right.position())
					}
					left := branch.cond.left
					if left is ast.SelectorExpr {
						comptime_field_name = left.expr.str()
						c.comptime_fields_type[comptime_field_name] = got_type
						is_comptime_type_is_expr = true
					} else if left is ast.TypeNode {
						is_comptime_type_is_expr = true
						left_type := c.unwrap_generic(left.typ)
						if left_type != got_type {
							should_skip = true
						}
					}
				}
			}
			cur_skip_flags := c.skip_flags
			if found_branch {
				c.skip_flags = true
			} else if should_skip {
				c.skip_flags = true
				should_skip = false // Reset the value of `should_skip` for the next branch
			} else if !is_comptime_type_is_expr {
				found_branch = true // If a branch wasn't skipped, the rest must be
			}
			if !c.skip_flags || c.pref.output_cross_c {
				c.stmts(branch.stmts)
			} else if !is_comptime_type_is_expr {
				node.branches[i].stmts = []
			}
			if comptime_field_name.len > 0 {
				c.comptime_fields_type.delete(comptime_field_name)
			}
			c.skip_flags = cur_skip_flags
		} else {
			// smartcast sumtypes and interfaces when using `is`
			pos := branch.cond.position()
			if branch.cond is ast.InfixExpr {
				if branch.cond.op == .key_is {
					right_expr := branch.cond.right as ast.TypeNode
					left_sym := c.table.get_type_symbol(branch.cond.left_type)
					expr_type := c.expr(branch.cond.left)
					if left_sym.kind == .interface_ {
						c.type_implements(right_expr.typ, expr_type, pos)
					} else if !c.check_types(right_expr.typ, expr_type) {
						expect_str := c.table.type_to_str(right_expr.typ)
						expr_str := c.table.type_to_str(expr_type)
						c.error('cannot use type `$expect_str` as type `$expr_str`', pos)
					}
					if (branch.cond.left is ast.Ident || branch.cond.left is ast.SelectorExpr)
						&& branch.cond.right is ast.TypeNode {
						is_variable := if mut branch.cond.left is ast.Ident {
							branch.cond.left.kind == .variable
						} else {
							true
						}
						if is_variable {
							if left_sym.kind in [.interface_, .sum_type] {
								if branch.cond.left is ast.Ident && left_sym.kind == .interface_ {
									// TODO: rewrite interface smartcast
									left := branch.cond.left as ast.Ident
									mut is_mut := false
									mut sum_type_casts := []ast.Type{}
									if v := branch.scope.find_var(left.name) {
										is_mut = v.is_mut
										sum_type_casts << v.sum_type_casts
									}
									branch.scope.register(ast.Var{
										name: left.name
										typ: right_expr.typ.to_ptr()
										sum_type_casts: sum_type_casts
										pos: left.pos
										is_used: true
										is_mut: is_mut
									})
									// TODO: needs to be removed
									node.branches[i].smartcast = true
								} else {
									c.smartcast_sumtype(branch.cond.left, branch.cond.left_type,
										right_expr.typ, mut branch.scope)
								}
							}
						}
					}
				}
			}
			c.stmts(branch.stmts)
		}
		if expr_required {
			if branch.stmts.len > 0 && branch.stmts[branch.stmts.len - 1] is ast.ExprStmt {
				mut last_expr := branch.stmts[branch.stmts.len - 1] as ast.ExprStmt
				c.expected_type = former_expected_type
				if c.expected_type.has_flag(.optional) {
					if node.typ == ast.void_type {
						node.is_expr = true
						node.typ = c.expected_type
					}
					continue
				}
				if c.expected_type.has_flag(.generic) {
					if node.typ == ast.void_type {
						node.is_expr = true
						node.typ = c.unwrap_generic(c.expected_type)
					}
					continue
				}
				last_expr.typ = c.expr(last_expr.expr)
				if !c.check_types(last_expr.typ, node.typ) {
					if node.typ == ast.void_type {
						// first branch of if expression
						node.is_expr = true
						node.typ = last_expr.typ
						continue
					} else if node.typ in [ast.float_literal_type, ast.int_literal_type] {
						if node.typ == ast.int_literal_type {
							if last_expr.typ.is_int() || last_expr.typ.is_float() {
								node.typ = last_expr.typ
								continue
							}
						} else { // node.typ == float_literal
							if last_expr.typ.is_float() {
								node.typ = last_expr.typ
								continue
							}
						}
					}
					if last_expr.typ in [ast.float_literal_type, ast.int_literal_type] {
						if last_expr.typ == ast.int_literal_type {
							if node.typ.is_int() || node.typ.is_float() {
								continue
							}
						} else { // expr_type == float_literal
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
				st.check_c_expr() or { c.error('`if` expression branch has $err.msg', st.pos) }
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
	if node.typ == ast.int_literal_type {
		node.typ = ast.int_type
	} else if node.typ == ast.float_literal_type {
		node.typ = ast.f64_type
	}
	if expr_required && !node.has_else {
		d := if node.is_comptime { '$' } else { '' }
		c.error('`$if_kind` expression needs `${d}else` clause', node.pos)
	}
	return node.typ
}

// comp_if_branch checks the condition of a compile-time `if` branch. It returns a `bool` that
// saying whether that branch's contents should be skipped (targets a different os for example)
fn (mut c Checker) comp_if_branch(cond ast.Expr, pos token.Position) bool {
	// TODO: better error messages here
	match cond {
		ast.BoolLiteral {
			return !cond.val
		}
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
			} else if cond.expr is ast.Ident {
				return cond.expr.name !in c.pref.compile_defines_all
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
					if cond.left is ast.SelectorExpr || cond.left is ast.TypeNode {
						// $if method.@type is string
						c.expr(cond.left)
						return false
					} else {
						c.error('invalid `\$if` condition: expected a type or selector expression',
							cond.left.position())
					}
				}
				.eq, .ne {
					if cond.left is ast.SelectorExpr && cond.right is ast.IntegerLiteral {
						// $if method.args.len == 1
					} else if cond.left is ast.Ident {
						// $if version == 2
						left_type := c.expr(cond.left)
						right_type := c.expr(cond.right)
						expr := c.find_definition(cond.left) or {
							c.error(err.msg, cond.left.pos)
							return false
						}
						if !c.check_types(right_type, left_type) {
							left_name := c.table.type_to_str(left_type)
							right_name := c.table.type_to_str(right_type)
							c.error('mismatched types `$left_name` and `$right_name`',
								cond.pos)
						}
						// :)
						// until `v.eval` is stable, I can't think of a better way to do this
						different := expr.str() != cond.right.str()
						return if cond.op == .eq { different } else { !different }
					} else {
						c.error('invalid `\$if` condition: ${cond.left.type_name()}1',
							cond.pos)
					}
				}
				else {
					c.error('invalid `\$if` condition', cond.pos)
				}
			}
		}
		ast.Ident {
			if cond.name in checker.valid_comp_if_os {
				return cond.name != c.pref.os.str().to_lower() // TODO hack
			} else if cond.name in checker.valid_comp_if_compilers {
				return pref.cc_from_string(cond.name) != c.pref.ccompiler_type
			} else if cond.name in checker.valid_comp_if_platforms {
				return false // TODO
			} else if cond.name in checker.valid_comp_if_other {
				// TODO: This should probably be moved
				match cond.name {
					'js' { return c.pref.backend != .js }
					'debug' { return !c.pref.is_debug }
					'prod' { return !c.pref.is_prod }
					'test' { return !c.pref.is_test }
					'glibc' { return false } // TODO
					'prealloc' { return !c.pref.prealloc }
					'no_bounds_checking' { return cond.name !in c.pref.compile_defines_all }
					else { return false }
				}
			} else if cond.name !in c.pref.compile_defines_all {
				// `$if some_var {}`
				typ := c.expr(cond)
				if cond.obj !is ast.Var && cond.obj !is ast.ConstField
					&& cond.obj !is ast.GlobalField {
					c.error('unknown var: `$cond.name`', pos)
					return false
				}
				expr := c.find_obj_definition(cond.obj) or {
					c.error(err.msg, cond.pos)
					return false
				}
				if !c.check_types(typ, ast.bool_type) {
					type_name := c.table.type_to_str(typ)
					c.error('non-bool type `$type_name` used as \$if condition', cond.pos)
				}
				// :)
				// until `v.eval` is stable, I can't think of a better way to do this
				return !(expr as ast.BoolLiteral).val
			}
		}
		else {
			c.error('invalid `\$if` condition', pos)
		}
	}
	return false
}

fn (mut c Checker) find_definition(ident ast.Ident) ?ast.Expr {
	match ident.kind {
		.unresolved, .blank_ident { return none }
		.variable, .constant { return c.find_obj_definition(ident.obj) }
		.global { return error('$ident.name is a global variable') }
		.function { return error('$ident.name is a function') }
	}
}

fn (mut c Checker) find_obj_definition(obj ast.ScopeObject) ?ast.Expr {
	// TODO: remove once we have better type inference
	mut name := ''
	match obj {
		ast.Var, ast.ConstField, ast.GlobalField, ast.AsmRegister { name = obj.name }
	}
	mut expr := ast.empty_expr()
	if obj is ast.Var {
		if obj.is_mut {
			return error('`$name` is mut and may have changed since its definition')
		}
		expr = obj.expr
	} else if obj is ast.ConstField {
		expr = obj.expr
	} else {
		return error('`$name` is a global variable and is unknown at compile time')
	}
	if expr is ast.Ident {
		return c.find_definition(expr as ast.Ident) // TODO: smartcast
	}
	if !expr.is_lit() {
		return error('definition of `$name` is unknown at compile time')
	}
	return expr
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

pub fn (mut c Checker) postfix_expr(mut node ast.PostfixExpr) ast.Type {
	typ := c.expr(node.expr)
	typ_sym := c.table.get_type_symbol(typ)
	is_non_void_pointer := (typ.is_ptr() || typ.is_pointer()) && typ_sym.kind != .voidptr
	if !c.inside_unsafe && is_non_void_pointer && !node.expr.is_auto_deref_var() {
		c.warn('pointer arithmetic is only allowed in `unsafe` blocks', node.pos)
	}
	if !(typ_sym.is_number() || (c.inside_unsafe && is_non_void_pointer)) {
		c.error('invalid operation: $node.op.str() (non-numeric type `$typ_sym.name`)',
			node.pos)
	} else {
		node.auto_locked, _ = c.fail_if_immutable(node.expr)
	}
	return typ
}

pub fn (mut c Checker) prefix_expr(mut node ast.PrefixExpr) ast.Type {
	old_inside_ref_lit := c.inside_ref_lit
	c.inside_ref_lit = c.inside_ref_lit || node.op == .amp
	right_type := c.expr(node.right)
	c.inside_ref_lit = old_inside_ref_lit
	node.right_type = right_type
	// TODO: testing ref/deref strategy
	if node.op == .amp && !right_type.is_ptr() {
		mut expr := node.right
		// if ParExpr get the innermost expr
		for mut expr is ast.ParExpr {
			expr = expr.expr
		}
		match expr {
			ast.BoolLiteral, ast.CallExpr, ast.CharLiteral, ast.FloatLiteral, ast.IntegerLiteral,
			ast.InfixExpr, ast.StringLiteral, ast.StringInterLiteral {
				c.error('cannot take the address of $expr', node.pos)
			}
			else {}
		}
		if mut node.right is ast.IndexExpr {
			typ_sym := c.table.get_type_symbol(node.right.left_type)
			mut is_mut := false
			if mut node.right.left is ast.Ident {
				ident := node.right.left
				// TODO: temporary, remove this
				ident_obj := ident.obj
				if ident_obj is ast.Var {
					is_mut = ident_obj.is_mut
				}
			}
			if typ_sym.kind == .map {
				c.error('cannot take the address of map values', node.right.pos)
			}
			if !c.inside_unsafe {
				if typ_sym.kind == .array && is_mut {
					c.error('cannot take the address of mutable array elements outside unsafe blocks',
						node.right.pos)
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
	if node.op == .not && right_type != ast.bool_type_idx && !c.pref.translated {
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

fn (mut c Checker) check_index(typ_sym &ast.TypeSymbol, index ast.Expr, index_type ast.Type, pos token.Position, range_index bool) {
	index_type_sym := c.table.get_type_symbol(index_type)
	// println('index expr left=$typ_sym.name $node.pos.line_nr')
	// if typ_sym.kind == .array && (!(ast.type_idx(index_type) in ast.number_type_idxs) &&
	// index_type_sym.kind != .enum_) {
	if typ_sym.kind in [.array, .array_fixed, .string, .ustring] {
		if !(index_type.is_int() || index_type_sym.kind == .enum_) {
			type_str := if typ_sym.kind in [.string, .ustring] {
				'non-integer string index `$index_type_sym.name`'
			} else {
				'non-integer index `$index_type_sym.name` (array type `$typ_sym.name`)'
			}
			c.error('$type_str', pos)
		}
		if index is ast.IntegerLiteral {
			if index.val[0] == `-` {
				c.error('negative index `$index.val`', index.pos)
			} else if typ_sym.kind == .array_fixed {
				i := index.val.int()
				info := typ_sym.info as ast.ArrayFixed
				if (!range_index && i >= info.size) || (range_index && i > info.size) {
					c.error('index out of range (index: $i, len: $info.size)', index.pos)
				}
			}
		}
		if index_type.has_flag(.optional) {
			type_str := if typ_sym.kind in [.string, .ustring] {
				'(type `$typ_sym.name`)'
			} else {
				'(array type `$typ_sym.name`)'
			}
			c.error('cannot use optional as index $type_str', pos)
		}
	}
}

pub fn (mut c Checker) index_expr(mut node ast.IndexExpr) ast.Type {
	mut typ := c.expr(node.left)
	node.left_type = typ
	typ_sym := c.table.get_final_type_symbol(typ)
	match typ_sym.kind {
		.map {
			node.is_map = true
		}
		.array {
			node.is_array = true
		}
		.array_fixed {
			node.is_farray = true
		}
		else {}
	}
	if typ_sym.kind !in [.array, .array_fixed, .string, .map] && !typ.is_ptr()
		&& typ !in [ast.byteptr_type, ast.charptr_type] && !typ.has_flag(.variadic) {
		c.error('type `$typ_sym.name` does not support indexing', node.pos)
	}
	if typ_sym.kind == .string && !typ.is_ptr() && node.is_setter {
		c.error('cannot assign to s[i] since V strings are immutable\n' +
			'(note, that variables may be mutable but string values are always immutable, like in Go and Java)',
			node.pos)
	}
	if !c.inside_unsafe && ((typ.is_ptr() && !node.left.is_auto_deref_var()) || typ.is_pointer()) {
		mut is_ok := false
		if mut node.left is ast.Ident {
			if node.left.obj is ast.Var {
				v := node.left.obj as ast.Var
				// `mut param []T` function parameter
				is_ok = ((v.is_mut && v.is_arg) || v.share == .shared_t) && !typ.deref().is_ptr()
			}
		}
		if !is_ok && !c.pref.translated {
			c.warn('pointer indexing is only allowed in `unsafe` blocks', node.pos)
		}
	}
	if mut node.index is ast.RangeExpr { // [1..2]
		if node.index.has_low {
			index_type := c.expr(node.index.low)
			c.check_index(typ_sym, node.index.low, index_type, node.pos, true)
		}
		if node.index.has_high {
			index_type := c.expr(node.index.high)
			c.check_index(typ_sym, node.index.high, index_type, node.pos, true)
		}
		// array[1..2] => array
		// fixed_array[1..2] => array
		if typ_sym.kind == .array_fixed {
			elem_type := c.table.value_type(typ)
			idx := c.table.find_or_register_array(elem_type)
			typ = ast.new_type(idx)
		} else {
			typ = typ.set_nr_muls(0)
		}
	} else { // [1]
		index_type := c.expr(node.index)
		if typ_sym.kind == .map {
			info := typ_sym.info as ast.Map
			if !c.check_types(index_type, info.key_type) {
				err := c.expected_msg(index_type, info.key_type)
				c.error('invalid key: $err', node.pos)
			}
		} else {
			c.check_index(typ_sym, node.index, index_type, node.pos, false)
		}
		value_type := c.table.value_type(typ)
		if value_type != ast.void_type {
			typ = value_type
		}
	}
	c.stmts(node.or_expr.stmts)
	return typ
}

// `.green` or `Color.green`
// If a short form is used, `expected_type` needs to be an enum
// with this value.
pub fn (mut c Checker) enum_val(mut node ast.EnumVal) ast.Type {
	typ_idx := if node.enum_name == '' {
		c.expected_type.idx()
	} else { //
		c.table.find_type_idx(node.enum_name)
	}
	// println('checker: enum_val: $node.enum_name typeidx=$typ_idx')
	if typ_idx == 0 {
		c.error('not an enum (name=$node.enum_name) (type_idx=0)', node.pos)
		return ast.void_type
	}
	mut typ := ast.new_type(typ_idx)
	if c.pref.translated {
		// TODO make more strict
		node.typ = typ
		return typ
	}
	if typ == ast.void_type {
		c.error('not an enum', node.pos)
		return ast.void_type
	}
	mut typ_sym := c.table.get_type_symbol(typ)
	// println('tname=$typ_sym.name $node.pos.line_nr $c.file.path')
	if typ_sym.kind == .array && node.enum_name.len == 0 {
		array_info := typ_sym.info as ast.Array
		typ = array_info.elem_type
		typ_sym = c.table.get_type_symbol(typ)
	}
	if typ_sym.kind != .enum_ && !c.pref.translated {
		// TODO in C int fields can be compared to enums, need to handle that in C2V
		c.error('expected type is not an enum (`$typ_sym.name`)', node.pos)
		return ast.void_type
	}
	if typ_sym.info !is ast.Enum {
		c.error('not an enum', node.pos)
		return ast.void_type
	}
	// info := typ_sym.info as ast.Enum
	info := typ_sym.enum_info()
	// rintln('checker: x = $info.x enum val $c.expected_type $typ_sym.name')
	// println(info.vals)
	if node.val !in info.vals {
		suggestion := util.new_suggestion(node.val, info.vals)
		c.error(suggestion.say('enum `$typ_sym.name` does not have a value `$node.val`'),
			node.pos)
	}
	node.typ = typ
	return typ
}

pub fn (mut c Checker) chan_init(mut node ast.ChanInit) ast.Type {
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

pub fn (mut c Checker) offset_of(node ast.OffsetOf) ast.Type {
	sym := c.table.get_final_type_symbol(node.struct_type)
	if sym.kind != .struct_ {
		c.error('first argument of __offsetof must be struct', node.pos)
		return ast.u32_type
	}
	if !c.table.struct_has_field(sym, node.field) {
		c.error('struct `$sym.name` has no field called `$node.field`', node.pos)
	}
	return ast.u32_type
}

pub fn (mut c Checker) check_dup_keys(node &ast.MapInit, i int) {
	key_i := node.keys[i]
	if key_i is ast.StringLiteral {
		for j in 0 .. i {
			key_j := node.keys[j]
			if key_j is ast.StringLiteral {
				if key_i.val == key_j.val {
					c.error('duplicate key "$key_i.val" in map literal', key_i.pos)
				}
			}
		}
	} else if key_i is ast.IntegerLiteral {
		for j in 0 .. i {
			key_j := node.keys[j]
			if key_j is ast.IntegerLiteral {
				if key_i.val == key_j.val {
					c.error('duplicate key "$key_i.val" in map literal', key_i.pos)
				}
			}
		}
	}
}

pub fn (mut c Checker) map_init(mut node ast.MapInit) ast.Type {
	// `map = {}`
	if node.keys.len == 0 && node.vals.len == 0 && node.typ == 0 {
		sym := c.table.get_type_symbol(c.expected_type)
		if sym.kind == .map {
			info := sym.map_info()
			node.typ = c.expected_type
			node.key_type = info.key_type
			node.value_type = info.value_type
			return node.typ
		} else {
			c.error('invalid empty map initilization syntax, use e.g. map[string]int{} instead',
				node.pos)
		}
	}
	// `x := map[string]string` - set in parser
	if node.typ != 0 {
		info := c.table.get_type_symbol(node.typ).map_info()
		c.ensure_type_exists(info.key_type, node.pos) or {}
		c.ensure_type_exists(info.value_type, node.pos) or {}
		node.key_type = info.key_type
		node.value_type = info.value_type
		return node.typ
	}
	if node.keys.len > 0 && node.vals.len > 0 {
		// `{'age': 20}`
		mut key0_type := c.table.mktyp(c.expr(node.keys[0]))
		if node.keys[0].is_auto_deref_var() {
			key0_type = key0_type.deref()
		}
		mut val0_type := c.table.mktyp(c.expr(node.vals[0]))
		if node.vals[0].is_auto_deref_var() {
			val0_type = val0_type.deref()
		}
		mut same_key_type := true
		for i, key in node.keys {
			if i == 0 {
				continue
			}
			val := node.vals[i]
			key_type := c.expr(key)
			c.expected_type = val0_type
			val_type := c.expr(val)
			if !c.check_types(key_type, key0_type) {
				msg := c.expected_msg(key_type, key0_type)
				c.error('invalid map key: $msg', key.position())
				same_key_type = false
			}
			if !c.check_types(val_type, val0_type) {
				msg := c.expected_msg(val_type, val0_type)
				c.error('invalid map value: $msg', val.position())
			}
		}
		if same_key_type {
			for i in 1 .. node.keys.len {
				c.check_dup_keys(node, i)
			}
		}
		map_type := ast.new_type(c.table.find_or_register_map(key0_type, val0_type))
		node.typ = map_type
		node.key_type = key0_type
		node.value_type = val0_type
		return map_type
	}
	return node.typ
}

// call this *before* calling error or warn
pub fn (mut c Checker) add_error_detail(s string) {
	c.error_details << s
}

pub fn (mut c Checker) warn(s string, pos token.Position) {
	allow_warnings := !(c.pref.is_prod || c.pref.warns_are_errors) // allow warnings only in dev builds
	c.warn_or_error(s, pos, allow_warnings)
}

pub fn (mut c Checker) error(message string, pos token.Position) {
	if c.pref.translated && message.starts_with('mismatched types') {
		// TODO move this
		return
	}
	if c.pref.is_verbose {
		print_backtrace()
	}
	msg := message.replace('`Array_', '`[]')
	c.warn_or_error(msg, pos, false)
}

// check `to` has all fields of `from`
fn (c &Checker) check_struct_signature(from ast.Struct, to ast.Struct) bool {
	// Note: `to` can have extra fields
	if from.fields.len == 0 {
		return false
	}
	for field in from.fields {
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

pub fn (mut c Checker) note(message string, pos token.Position) {
	mut details := ''
	if c.error_details.len > 0 {
		details = c.error_details.join('\n')
		c.error_details = []
	}
	wrn := errors.Notice{
		reporter: errors.Reporter.checker
		pos: pos
		file_path: c.file.path
		message: message
		details: details
	}
	c.file.notices << wrn
	c.notices << wrn
	c.nr_notices++
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
		if c.pref.fatal_errors {
			exit(1)
		}
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

fn (mut c Checker) sql_expr(mut node ast.SqlExpr) ast.Type {
	c.inside_sql = true
	defer {
		c.inside_sql = false
	}
	sym := c.table.get_type_symbol(node.table_expr.typ)
	c.ensure_type_exists(node.table_expr.typ, node.pos) or { return ast.void_type }
	c.cur_orm_ts = sym
	info := sym.info as ast.Struct
	fields := c.fetch_and_verify_orm_fields(info, node.table_expr.pos, sym.name)
	mut sub_structs := map[int]ast.SqlExpr{}
	for f in fields.filter(c.table.type_symbols[int(it.typ)].kind == .struct_) {
		mut n := ast.SqlExpr{
			pos: node.pos
			has_where: true
			typ: f.typ
			db_expr: node.db_expr
			table_expr: ast.TypeNode{
				pos: node.table_expr.pos
				typ: f.typ
			}
		}
		tmp_inside_sql := c.inside_sql
		c.sql_expr(mut n)
		c.inside_sql = tmp_inside_sql
		n.where_expr = ast.InfixExpr{
			op: .eq
			pos: n.pos
			left: ast.Ident{
				language: .v
				tok_kind: .eq
				scope: c.fn_scope
				obj: ast.Var{}
				mod: 'main'
				name: 'id'
				is_mut: false
				kind: .unresolved
				info: ast.IdentVar{}
			}
			right: ast.Ident{
				language: .c
				mod: 'main'
				tok_kind: .eq
				obj: ast.Var{}
				is_mut: false
				scope: c.fn_scope
				info: ast.IdentVar{
					typ: ast.int_type
				}
			}
			left_type: ast.int_type
			right_type: ast.int_type
			auto_locked: ''
			or_block: ast.OrExpr{}
		}

		sub_structs[int(f.typ)] = n
	}
	node.fields = fields
	node.sub_structs = sub_structs.move()
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

fn (mut c Checker) sql_stmt(mut node ast.SqlStmt) ast.Type {
	c.inside_sql = true
	defer {
		c.inside_sql = false
	}
	c.ensure_type_exists(node.table_expr.typ, node.pos) or { return ast.void_type }
	table_sym := c.table.get_type_symbol(node.table_expr.typ)
	c.cur_orm_ts = table_sym
	info := table_sym.info as ast.Struct
	fields := c.fetch_and_verify_orm_fields(info, node.table_expr.pos, table_sym.name)
	mut sub_structs := map[int]ast.SqlStmt{}
	for f in fields.filter(c.table.type_symbols[int(it.typ)].kind == .struct_) {
		mut n := ast.SqlStmt{
			pos: node.pos
			db_expr: node.db_expr
			kind: node.kind
			table_expr: ast.TypeNode{
				pos: node.table_expr.pos
				typ: f.typ
			}
			object_var_name: '${node.object_var_name}.$f.name'
		}
		tmp_inside_sql := c.inside_sql
		c.sql_stmt(mut n)
		c.inside_sql = tmp_inside_sql
		sub_structs[int(f.typ)] = n
	}
	node.fields = fields
	node.sub_structs = sub_structs.move()
	c.expr(node.db_expr)
	if node.kind == .update {
		for expr in node.update_exprs {
			c.expr(expr)
		}
	}
	if node.where_expr !is ast.EmptyExpr {
		c.expr(node.where_expr)
	}

	return ast.void_type
}

fn (mut c Checker) fetch_and_verify_orm_fields(info ast.Struct, pos token.Position, table_name string) []ast.StructField {
	fields := info.fields.filter((it.typ in [ast.string_type, ast.int_type, ast.bool_type]
		|| c.table.type_symbols[int(it.typ)].kind == .struct_) && !it.attrs.contains('skip'))
	if fields.len == 0 {
		c.error('V orm: select: empty fields in `$table_name`', pos)
		return []ast.StructField{}
	}
	if fields[0].name != 'id' {
		c.error('V orm: `id int` must be the first field in `$table_name`', pos)
	}
	return fields
}

fn (mut c Checker) post_process_generic_fns() {
	// Loop thru each generic function concrete type.
	// Check each specific fn instantiation.
	for i in 0 .. c.file.generic_fns.len {
		if c.table.fn_gen_types.len == 0 {
			// no concrete types, so just skip:
			continue
		}
		mut node := c.file.generic_fns[i]
		c.mod = node.mod
		for gen_types in c.table.fn_gen_types[node.name] {
			c.cur_generic_types = gen_types
			c.fn_decl(mut node)
			if node.name in ['vweb.run_app', 'vweb.run'] {
				c.vweb_gen_types << gen_types
			}
		}
		c.cur_generic_types = []
	}
}

fn (mut c Checker) fn_decl(mut node ast.FnDecl) {
	c.returns = false
	if node.generic_params.len > 0 && c.cur_generic_types.len == 0 {
		// Just remember the generic function for now.
		// It will be processed later in c.post_process_generic_fns,
		// after all other normal functions are processed.
		// This is done so that all generic function calls can
		// have a chance to populate c.table.fn_gen_types with
		// the correct concrete types.
		c.file.generic_fns << node
		return
	}
	if node.language == .v && !c.is_builtin_mod {
		c.check_valid_snake_case(node.name, 'function name', node.pos)
	}
	if node.name == 'main.main' {
		c.main_fn_decl_node = node
	}
	if node.return_type != ast.void_type {
		for attr in node.attrs {
			if attr.is_comptime_define {
				c.error('only functions that do NOT return values can have `[if $attr.name]` tags',
					node.pos)
				break
			}
		}
	}
	if node.is_method {
		mut sym := c.table.get_type_symbol(node.receiver.typ)
		if sym.kind == .array && !c.is_builtin_mod && node.name == 'map' {
			// TODO `node.map in array_builtin_methods`
			c.error('method overrides built-in array method', node.pos)
		} else if sym.kind == .sum_type && node.name == 'type_name' {
			c.error('method overrides built-in sum type method', node.pos)
		} else if sym.kind == .multi_return {
			c.error('cannot define method on multi-value', node.method_type_pos)
		}
		if sym.name.len == 1 {
			// One letter types are reserved for generics.
			c.error('unknown type `$sym.name`', node.receiver_pos)
			return
		}
		// make sure interface does not implement its own interface methods
		if sym.kind == .interface_ && sym.has_method(node.name) {
			if sym.info is ast.Interface {
				info := sym.info as ast.Interface
				// if the method is in info.methods then it is an interface method
				if info.has_method(node.name) {
					c.error('interface `$sym.name` cannot implement its own interface method `$node.name`',
						node.pos)
				}
			}
		}
		// needed for proper error reporting during vweb route checking
		sym.methods[node.method_idx].source_fn = voidptr(node)
	}
	if node.language == .v {
		// Make sure all types are valid
		for arg in node.params {
			c.ensure_type_exists(arg.typ, node.pos) or { return }
		}
	}
	if node.language == .v && node.name.after_char(`.`) == 'init' && !node.is_method
		&& node.params.len == 0 {
		if node.is_pub {
			c.error('fn `init` must not be public', node.pos)
		}
		if node.return_type != ast.void_type {
			c.error('fn `init` cannot have a return type', node.pos)
		}
	}
	if node.return_type != ast.Type(0) {
		c.ensure_type_exists(node.return_type, node.pos) or { return }
		if node.language == .v && node.is_method && node.name == 'str' {
			if node.return_type != ast.string_type {
				c.error('.str() methods should return `string`', node.pos)
			}
			if node.params.len != 1 {
				c.error('.str() methods should have 0 arguments', node.pos)
			}
		}
		if node.language == .v && node.is_method
			&& node.name in ['+', '-', '*', '%', '/', '<', '=='] {
			if node.params.len != 2 {
				c.error('operator methods should have exactly 1 argument', node.pos)
			} else {
				receiver_sym := c.table.get_type_symbol(node.receiver.typ)
				param_sym := c.table.get_type_symbol(node.params[1].typ)
				if param_sym.kind !in [.struct_, .alias] || receiver_sym.kind !in [.struct_, .alias] {
					c.error('operator methods are only allowed for struct and type alias',
						node.pos)
				} else {
					parent_sym := c.table.get_final_type_symbol(node.receiver.typ)
					if node.rec_mut {
						c.error('receiver cannot be `mut` for operator overloading', node.receiver_pos)
					} else if node.params[1].is_mut {
						c.error('argument cannot be `mut` for operator overloading', node.pos)
					} else if node.receiver.typ != node.params[1].typ {
						c.error('expected `$receiver_sym.name` not `$param_sym.name` - both operands must be the same type for operator overloading',
							node.params[1].type_pos)
					} else if node.name in ['<', '=='] && node.return_type != ast.bool_type {
						c.error('operator comparison methods should return `bool`', node.pos)
					} else if parent_sym.is_primitive() {
						c.error('cannot define operator methods on type alias for `$parent_sym.name`',
							node.pos)
					}
				}
			}
		}
	}
	// TODO c.pref.is_vet
	if node.language == .v && !node.is_method && node.params.len == 0 && node.is_test {
		if !c.pref.is_test {
			// simple heuristic
			for st in node.stmts {
				if st is ast.AssertStmt {
					c.warn('tests will not be run, because filename does not end with `_test.v`',
						node.pos)
					break
				}
			}
		}
		if node.return_type != ast.void_type_idx
			&& node.return_type.clear_flag(.optional) != ast.void_type_idx {
			c.error('test functions should either return nothing at all, or be marked to return `?`',
				node.pos)
		}
	}
	c.expected_type = ast.void_type
	c.cur_fn = node
	// Add return if `fn(...) ? {...}` have no return at end
	if node.return_type != ast.void_type && node.return_type.has_flag(.optional)
		&& (node.stmts.len == 0 || node.stmts[node.stmts.len - 1] !is ast.Return) {
		sym := c.table.get_type_symbol(node.return_type)
		if sym.kind == .void {
			node.stmts << ast.Return{
				pos: node.pos
			}
		}
	}
	c.fn_scope = node.scope
	c.stmts(node.stmts)
	node.has_return = c.returns || has_top_return(node.stmts)
	if node.language == .v && !node.no_body && node.return_type != ast.void_type && !node.has_return
		&& node.name !in ['panic', 'exit'] {
		if c.inside_anon_fn {
			c.error('missing return at the end of an anonymous function', node.pos)
		} else {
			c.error('missing return at end of function `$node.name`', node.pos)
		}
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
			if stmt.expr is ast.CallExpr {
				if stmt.expr.name in ['panic', 'exit'] {
					return true
				}
			}
		}
	}
	return false
}

fn (mut c Checker) verify_vweb_params_for_method(m ast.Fn) (bool, int, int) {
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
			if m.return_type == typ_vweb_result {
				is_ok, nroute_attributes, nargs := c.verify_vweb_params_for_method(m)
				if !is_ok {
					f := &ast.FnDecl(m.source_fn)
					if isnil(f) {
						continue
					}
					if f.return_type == typ_vweb_result && f.receiver.typ == m.params[0].typ
						&& f.name == m.name {
						c.file = f.source_file // setup of file path for the warning
						c.warn('mismatched parameters count between vweb method `${sym_app.name}.$m.name` ($nargs) and route attribute $m.attrs ($nroute_attributes)',
							f.pos)
					}
				}
			}
		}
	}
}

fn (mut c Checker) trace(fbase string, message string) {
	if c.file.path_base == fbase {
		println('> c.trace | ${fbase:-10s} | $message')
	}
}

fn (mut c Checker) ensure_type_exists(typ ast.Type, pos token.Position) ? {
	if typ == 0 {
		c.error('unknown type', pos)
	}
	sym := c.table.get_type_symbol(typ)
	match sym.kind {
		.placeholder {
			if sym.language == .v && !sym.name.starts_with('C.') {
				c.error(util.new_suggestion(sym.name, c.table.known_type_names()).say('unknown type `$sym.name`'),
					pos)
				return none
			}
		}
		.int_literal, .float_literal {
			// Separate error condition for `int_literal` and `float_literal` because `util.suggestion` may give different
			// suggestions due to f32 comparision issue.
			if !c.is_builtin_mod {
				msg := if sym.kind == .int_literal {
					'unknown type `$sym.name`.\nDid you mean `int`?'
				} else {
					'unknown type `$sym.name`.\nDid you mean `f64`?'
				}
				c.error(msg, pos)
				return none
			}
		}
		.array {
			c.ensure_type_exists((sym.info as ast.Array).elem_type, pos) ?
		}
		.map {
			info := sym.info as ast.Map
			c.ensure_type_exists(info.key_type, pos) ?
			c.ensure_type_exists(info.value_type, pos) ?
		}
		else {}
	}
}
