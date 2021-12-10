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
import v.util.version
import v.errors
import v.pkgconfig
import v.gen.native

const int_min = int(0x80000000)

const int_max = int(0x7FFFFFFF)

const (
	valid_comptime_if_os             = ['windows', 'ios', 'macos', 'mach', 'darwin', 'hpux', 'gnu',
		'qnx', 'linux', 'freebsd', 'openbsd', 'netbsd', 'bsd', 'dragonfly', 'android', 'solaris',
		'haiku', 'serenity', 'vinix']
	valid_comptime_compression_types = ['none', 'zlib']
	valid_comptime_if_compilers      = ['gcc', 'tinyc', 'clang', 'mingw', 'msvc', 'cplusplus']
	valid_comptime_if_platforms      = ['amd64', 'i386', 'aarch64', 'arm64', 'arm32', 'rv64', 'rv32']
	valid_comptime_if_cpu_features   = ['x64', 'x32', 'little_endian', 'big_endian']
	valid_comptime_if_other          = ['js', 'debug', 'prod', 'test', 'glibc', 'prealloc',
		'no_bounds_checking', 'freestanding', 'threads', 'js_node', 'js_browser', 'js_freestanding',
		'interpreter']
	valid_comptime_not_user_defined  = all_valid_comptime_idents()
	array_builtin_methods            = ['filter', 'clone', 'repeat', 'reverse', 'map', 'slice',
		'sort', 'contains', 'index', 'wait', 'any', 'all', 'first', 'last', 'pop']
	reserved_type_names              = ['bool', 'char', 'i8', 'i16', 'int', 'i64', 'byte', 'u16',
		'u32', 'u64', 'f32', 'f64', 'map', 'string', 'rune']
	vroot_is_deprecated_message      = '@VROOT is deprecated, use @VMODROOT or @VEXEROOT instead'
)

fn all_valid_comptime_idents() []string {
	mut res := []string{}
	res << checker.valid_comptime_if_os
	res << checker.valid_comptime_if_compilers
	res << checker.valid_comptime_if_platforms
	res << checker.valid_comptime_if_cpu_features
	res << checker.valid_comptime_if_other
	return res
}

[heap]
pub struct Checker {
	pref &pref.Preferences // Preferences shared from V struct
pub mut:
	table            &ast.Table
	file             &ast.File = 0
	nr_errors        int
	nr_warnings      int
	nr_notices       int
	should_abort     bool // when too many errors/warnings/notices are accumulated, .should_abort becomes true. It is checked in statement/expression loops, so the checker can return early, instead of wasting time.
	errors           []errors.Error
	warnings         []errors.Warning
	notices          []errors.Notice
	error_lines      []int // to avoid printing multiple errors for the same line
	expected_type    ast.Type
	expected_or_type ast.Type // fn() or { 'this type' } eg. string. expected or block type
	const_decl       string
	const_deps       []string
	const_names      []string
	global_names     []string
	locked_names     []string // vars that are currently locked
	rlocked_names    []string // vars that are currently read-locked
	in_for_count     int      // if checker is currently in a for loop
	// checked_ident  string // to avoid infinite checker loops
	returns        bool
	scope_returns  bool
	mod            string // current module name
	is_builtin_mod bool   // true inside the 'builtin', 'os' or 'strconv' modules; TODO: remove the need for special casing this
	inside_unsafe  bool   // true inside `unsafe {}` blocks
	inside_const   bool   // true inside `const ( ... )` blocks
	inside_anon_fn bool   // true inside `fn() { ... }()`
	inside_ref_lit bool   // true inside `a := &something`
	inside_defer   bool   // true inside `defer {}` blocks
	inside_fn_arg  bool   // `a`, `b` in `a.f(b)`
	inside_ct_attr bool   // true inside `[if expr]`
	skip_flags     bool   // should `#flag` and `#include` be skipped
	fn_level       int    // 0 for the top level, 1 for `fn abc() {}`, 2 for a nested fn, etc
	ct_cond_stack  []ast.Expr
mut:
	stmt_level int // the nesting level inside each stmts list;
	// .stmt_level is used to check for `evaluated but not used` ExprStmts like `1 << 1`
	// 1 for statements directly at each inner scope level;
	// increases for `x := if cond { statement_list1} else {statement_list2}`;
	// increases for `x := optfn() or { statement_list3 }`;
	is_last_stmt                     bool
	files                            []ast.File
	expr_level                       int  // to avoid infinite recursion segfaults due to compiler bugs
	inside_sql                       bool // to handle sql table fields pseudo variables
	cur_orm_ts                       ast.TypeSymbol
	error_details                    []string
	vmod_file_content                string     // needed for @VMOD_FILE, contents of the file, *NOT its path**
	vweb_gen_types                   []ast.Type // vweb route checks
	prevent_sum_type_unwrapping_once bool       // needed for assign new values to sum type, stopping unwrapping then
	loop_label                       string     // set when inside a labelled for loop
	timers                           &util.Timers = util.get_timers()
	comptime_fields_type             map[string]ast.Type
	fn_scope                         &ast.Scope = voidptr(0)
	main_fn_decl_node                ast.FnDecl
	match_exhaustive_cutoff_limit    int = 10
	// TODO: these are here temporarily and used for deprecations; remove soon
	using_new_err_struct     bool
	inside_selector_expr     bool
	inside_println_arg       bool
	inside_decl_rhs          bool
	inside_if_guard          bool // true inside the guard condition of `if x := opt() {}`
	need_recheck_generic_fns bool // need recheck generic fns because there are cascaded nested generic fn
}

pub fn new_checker(table &ast.Table, pref &pref.Preferences) &Checker {
	mut timers_should_print := false
	$if time_checking ? {
		timers_should_print = true
	}
	return &Checker{
		table: table
		pref: pref
		timers: util.new_timers(should_print: timers_should_print, label: 'checker')
		match_exhaustive_cutoff_limit: pref.checker_match_exhaustive_cutoff_limit
	}
}

fn (mut c Checker) reset_checker_state_at_start_of_new_file() {
	c.expected_type = ast.void_type
	c.expected_or_type = ast.void_type
	c.const_decl = ''
	c.in_for_count = 0
	c.returns = false
	c.scope_returns = false
	c.mod = ''
	c.is_builtin_mod = false
	c.inside_unsafe = false
	c.inside_const = false
	c.inside_anon_fn = false
	c.inside_ref_lit = false
	c.inside_defer = false
	c.inside_fn_arg = false
	c.inside_ct_attr = false
	c.skip_flags = false
	c.fn_level = 0
	c.expr_level = 0
	c.stmt_level = 0
	c.inside_sql = false
	c.cur_orm_ts = ast.TypeSymbol{}
	c.prevent_sum_type_unwrapping_once = false
	c.loop_label = ''
	c.using_new_err_struct = false
	c.inside_selector_expr = false
	c.inside_println_arg = false
	c.inside_decl_rhs = false
	c.inside_if_guard = false
}

pub fn (mut c Checker) check(ast_file_ &ast.File) {
	mut ast_file := ast_file_
	c.reset_checker_state_at_start_of_new_file()
	c.change_current_file(ast_file)
	for i, ast_import in ast_file.imports {
		for sym in ast_import.syms {
			full_name := ast_import.mod + '.' + sym.name
			if full_name in c.const_names {
				c.error('cannot selectively import constant `$sym.name` from `$ast_import.mod`, import `$ast_import.mod` and use `$full_name` instead',
					sym.pos)
			}
		}
		for j in 0 .. i {
			if ast_import.mod == ast_file.imports[j].mod {
				c.error('`$ast_import.mod` was already imported on line ${
					ast_file.imports[j].mod_pos.line_nr + 1}', ast_import.mod_pos)
			}
		}
	}
	c.stmt_level = 0
	for mut stmt in ast_file.stmts {
		if stmt in [ast.ConstDecl, ast.ExprStmt] {
			c.expr_level = 0
			c.stmt(stmt)
		}
		if c.should_abort {
			return
		}
	}
	//
	c.stmt_level = 0
	for mut stmt in ast_file.stmts {
		if stmt is ast.GlobalDecl {
			c.expr_level = 0
			c.stmt(stmt)
		}
		if c.should_abort {
			return
		}
	}
	//
	c.stmt_level = 0
	for mut stmt in ast_file.stmts {
		if stmt !is ast.ConstDecl && stmt !is ast.GlobalDecl && stmt !is ast.ExprStmt {
			c.expr_level = 0
			c.stmt(stmt)
		}
		if c.should_abort {
			return
		}
	}
	//
	c.check_scope_vars(c.file.scope)
}

pub fn (mut c Checker) check_scope_vars(sc &ast.Scope) {
	if !c.pref.is_repl && !c.file.is_test {
		for _, obj in sc.objects {
			match obj {
				ast.Var {
					if !obj.is_used && obj.name[0] != `_` {
						c.warn('unused variable: `$obj.name`', obj.pos)
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
	}
	for child in sc.children {
		c.check_scope_vars(child)
	}
}

// not used right now
pub fn (mut c Checker) check2(ast_file &ast.File) []errors.Error {
	c.change_current_file(ast_file)
	for stmt in ast_file.stmts {
		c.stmt(stmt)
	}
	return c.errors
}

pub fn (mut c Checker) change_current_file(file &ast.File) {
	c.file = unsafe { file }
	c.vmod_file_content = ''
	c.mod = file.mod.name
}

pub fn (mut c Checker) check_files(ast_files []&ast.File) {
	// c.files = ast_files
	mut has_main_mod_file := false
	mut has_main_fn := false
	mut files_from_main_module := []&ast.File{}
	for i in 0 .. ast_files.len {
		mut file := unsafe { ast_files[i] }
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
	// post process generic functions. must be done after all files have been
	// checked, to eunsure all generic calls are processed as this information
	// is needed when the generic type is auto inferred from the call argument
	// Check more times if there are more new registered fn concrete types
	for {
		for file in ast_files {
			if file.generic_fns.len > 0 {
				c.change_current_file(file)
				c.post_process_generic_fns()
			}
		}
		if !c.need_recheck_generic_fns {
			break
		}
		c.need_recheck_generic_fns = false
	}
	// restore the original c.file && c.mod after post processing
	c.change_current_file(last_file)
	c.timers.show('checker_post_process_generic_fns')

	c.timers.start('checker_verify_all_vweb_routes')
	c.verify_all_vweb_routes()
	c.timers.show('checker_verify_all_vweb_routes')

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
	if c.pref.no_builtin {
		// `v -no-builtin module/` do not necessarily need to have a `main` function
		// This is useful for compiling linux kernel modules for example.
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
fn (mut c Checker) file_has_main_fn(file &ast.File) bool {
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
	if !c.pref.is_vweb && !c.pref.translated && name.len > 0
		&& (name[0] == `_` || name.contains('._')) {
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
	// TODO Remove when `u8` isn't an alias in builtin anymore
	if c.file.mod.name != 'builtin' {
		c.check_valid_pascal_case(node.name, 'type alias', node.pos)
	}
	c.ensure_type_exists(node.parent_type, node.type_pos) or { return }
	typ_sym := c.table.get_type_symbol(node.parent_type)
	if typ_sym.kind in [.placeholder, .int_literal, .float_literal] {
		c.error('unknown type `$typ_sym.name`', node.type_pos)
	} else if typ_sym.kind == .alias {
		orig_sym := c.table.get_type_symbol((typ_sym.info as ast.Alias).parent_type)
		c.error('type `$typ_sym.str()` is an alias, use the original alias type `$orig_sym.name` instead',
			node.type_pos)
	} else if typ_sym.kind == .chan {
		c.error('aliases of `chan` types are not allowed.', node.type_pos)
	}
}

pub fn (mut c Checker) fn_type_decl(node ast.FnTypeDecl) {
	c.check_valid_pascal_case(node.name, 'fn type', node.pos)
	typ_sym := c.table.get_type_symbol(node.typ)
	fn_typ_info := typ_sym.info as ast.FnType
	fn_info := fn_typ_info.func
	c.ensure_type_exists(fn_info.return_type, fn_info.return_type_pos) or {}
	ret_sym := c.table.get_type_symbol(fn_info.return_type)
	if ret_sym.kind == .placeholder {
		c.error('unknown type `$ret_sym.name`', fn_info.return_type_pos)
	}
	for arg in fn_info.params {
		c.ensure_type_exists(arg.typ, arg.type_pos) or { return }
		arg_sym := c.table.get_type_symbol(arg.typ)
		if arg_sym.kind == .placeholder {
			c.error('unknown type `$arg_sym.name`', arg.type_pos)
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
		c.ensure_type_exists(variant.typ, variant.pos) or {}
		mut sym := c.table.get_type_symbol(variant.typ)
		if sym.name in names_used {
			c.error('sum type $node.name cannot hold the type `$sym.name` more than once',
				variant.pos)
		} else if sym.kind in [.placeholder, .int_literal, .float_literal] {
			c.error('unknown type `$sym.name`', variant.pos)
		} else if sym.kind == .interface_ && sym.language != .js {
			c.error('sum type cannot hold an interface', variant.pos)
		} else if sym.kind == .struct_ && sym.language == .js {
			c.error('sum type cannot hold an JS struct', variant.pos)
		}
		if sym.name.trim_prefix(sym.mod + '.') == node.name {
			c.error('sum type cannot hold itself', variant.pos)
		}
		names_used << sym.name
	}
}

pub fn (mut c Checker) expand_iface_embeds(idecl &ast.InterfaceDecl, level int, iface_embeds []ast.InterfaceEmbedding) []ast.InterfaceEmbedding {
	// eprintln('> expand_iface_embeds: idecl.name: $idecl.name | level: $level | iface_embeds.len: $iface_embeds.len')
	if level > 100 {
		c.error('too many interface embedding levels: $level, for interface `$idecl.name`',
			idecl.pos)
		return []
	}
	if iface_embeds.len == 0 {
		return []
	}
	mut res := map[int]ast.InterfaceEmbedding{}
	mut ares := []ast.InterfaceEmbedding{}
	for ie in iface_embeds {
		if iface_decl := c.table.interfaces[ie.typ] {
			mut list := iface_decl.ifaces
			if !iface_decl.are_ifaces_expanded {
				list = c.expand_iface_embeds(idecl, level + 1, iface_decl.ifaces)
				c.table.interfaces[ie.typ].ifaces = list
				c.table.interfaces[ie.typ].are_ifaces_expanded = true
			}
			for partial in list {
				res[partial.typ] = partial
			}
		}
		res[ie.typ] = ie
	}
	for _, v in res {
		ares << v
	}
	return ares
}

pub fn (mut c Checker) interface_decl(mut node ast.InterfaceDecl) {
	c.check_valid_pascal_case(node.name, 'interface name', node.pos)
	mut decl_sym := c.table.get_type_symbol(node.typ)
	is_js := node.language == .js
	if mut decl_sym.info is ast.Interface {
		if node.ifaces.len > 0 {
			all_ifaces := c.expand_iface_embeds(node, 0, node.ifaces)
			// eprintln('> node.name: $node.name | node.ifaces.len: $node.ifaces.len | all_ifaces: $all_ifaces.len')
			node.ifaces = all_ifaces
			mut emnames := map[string]int{}
			mut emnames_ds := map[string]bool{}
			mut emnames_ds_info := map[string]bool{}
			mut efnames := map[string]int{}
			mut efnames_ds_info := map[string]bool{}
			for i, m in node.methods {
				emnames[m.name] = i
				emnames_ds[m.name] = true
				emnames_ds_info[m.name] = true
			}
			for i, f in node.fields {
				efnames[f.name] = i
				efnames_ds_info[f.name] = true
			}
			//
			for iface in all_ifaces {
				isym := c.table.get_type_symbol(iface.typ)
				if isym.kind != .interface_ {
					c.error('interface `$node.name` tries to embed `$isym.name`, but `$isym.name` is not an interface, but `$isym.kind`',
						iface.pos)
					continue
				}
				isym_info := isym.info as ast.Interface
				for f in isym_info.fields {
					if !efnames_ds_info[f.name] {
						efnames_ds_info[f.name] = true
						decl_sym.info.fields << f
					}
				}
				for m in isym_info.methods {
					if !emnames_ds_info[m.name] {
						emnames_ds_info[m.name] = true
						decl_sym.info.methods << m.new_method_with_receiver_type(node.typ)
					}
				}
				for m in isym.methods {
					if !emnames_ds[m.name] {
						emnames_ds[m.name] = true
						decl_sym.methods << m.new_method_with_receiver_type(node.typ)
					}
				}
				if iface_decl := c.table.interfaces[iface.typ] {
					for f in iface_decl.fields {
						if f.name in efnames {
							// already existing method name, check for conflicts
							ifield := node.fields[efnames[f.name]]
							if field := c.table.find_field_with_embeds(isym, f.name) {
								if ifield.typ != field.typ {
									exp := c.table.type_to_str(ifield.typ)
									got := c.table.type_to_str(field.typ)
									c.error('embedded interface `$iface_decl.name` conflicts existing field: `$ifield.name`, expecting type: `$exp`, got type: `$got`',
										ifield.pos)
								}
							}
						} else {
							efnames[f.name] = node.fields.len
							node.fields << f
						}
					}
					for m in iface_decl.methods {
						if m.name in emnames {
							// already existing field name, check for conflicts
							imethod := node.methods[emnames[m.name]]
							if em_fn := decl_sym.find_method(imethod.name) {
								if m_fn := isym.find_method(m.name) {
									msg := c.table.is_same_method(m_fn, em_fn)
									if msg.len > 0 {
										em_sig := c.table.fn_signature(em_fn, skip_receiver: true)
										m_sig := c.table.fn_signature(m_fn, skip_receiver: true)
										c.error('embedded interface `$iface_decl.name` causes conflict: $msg, for interface method `$em_sig` vs `$m_sig`',
											imethod.pos)
									}
								}
							}
						} else {
							emnames[m.name] = node.methods.len
							mut new_method := m.new_method_with_receiver_type(node.typ)
							new_method.pos = iface.pos
							node.methods << new_method
						}
					}
				}
			}
		}
		for i, method in node.methods {
			if node.language == .v {
				c.check_valid_snake_case(method.name, 'method name', method.pos)
			}
			c.ensure_type_exists(method.return_type, method.return_type_pos) or { return }
			if is_js {
				mtyp := c.table.get_type_symbol(method.return_type)
				if !mtyp.is_js_compatible() {
					c.error('method $method.name returns non JS type', method.pos)
				}
			}
			for j, param in method.params {
				if j == 0 && is_js {
					continue // no need to check first param
				}
				c.ensure_type_exists(param.typ, param.pos) or { return }
				if param.name in checker.reserved_type_names {
					c.error('invalid use of reserved type `$param.name` as a parameter name',
						param.pos)
				}
				if is_js {
					ptyp := c.table.get_type_symbol(param.typ)
					if !ptyp.is_js_compatible() && !(j == method.params.len - 1
						&& method.is_variadic) {
						c.error('method `$method.name` accepts non JS type as parameter',
							method.pos)
					}
				}
			}
			for field in node.fields {
				field_sym := c.table.get_type_symbol(field.typ)
				if field.name == method.name && field_sym.kind == .function {
					c.error('type `$decl_sym.name` has both field and method named `$method.name`',
						method.pos)
				}
			}
			for j in 0 .. i {
				if method.name == node.methods[j].name {
					c.error('duplicate method name `$method.name`', method.pos)
				}
			}
		}
		for i, field in node.fields {
			if node.language == .v {
				c.check_valid_snake_case(field.name, 'field name', field.pos)
			}
			c.ensure_type_exists(field.typ, field.pos) or { return }
			if is_js {
				tsym := c.table.get_type_symbol(field.typ)
				if !tsym.is_js_compatible() {
					c.error('field `$field.name` uses non JS type', field.pos)
				}
			}
			if field.typ == node.typ && node.language != .js {
				c.error('recursive interface fields are not allowed because they cannot be initialised',
					field.type_pos)
			}
			for j in 0 .. i {
				if field.name == node.fields[j].name {
					c.error('field name `$field.name` duplicate', field.pos)
				}
			}
		}
	}
}

pub fn (mut c Checker) struct_decl(mut node ast.StructDecl) {
	if node.language == .v && !c.is_builtin_mod {
		c.check_valid_pascal_case(node.name, 'struct name', node.pos)
	}
	mut struct_sym := c.table.find_type(node.name) or { ast.invalid_type_symbol }
	mut has_generic_types := false
	if mut struct_sym.info is ast.Struct {
		for embed in node.embeds {
			if embed.typ.has_flag(.generic) {
				has_generic_types = true
			}
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
		for attr in node.attrs {
			if attr.name == 'typedef' && node.language != .c {
				c.error('`typedef` attribute can only be used with C structs', node.pos)
			}
		}
		for i, field in node.fields {
			if field.typ == ast.any_type {
				c.error('struct field cannot be the `any` type, use generics instead',
					field.type_pos)
			}
			c.ensure_type_exists(field.typ, field.type_pos) or { return }
			if field.typ.has_flag(.generic) {
				has_generic_types = true
			}
			if node.language == .v {
				c.check_valid_snake_case(field.name, 'field name', field.pos)
			}
			sym := c.table.get_type_symbol(field.typ)
			for j in 0 .. i {
				if field.name == node.fields[j].name {
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
					if sym.kind == .interface_
						&& c.type_implements(field_expr_type, field.typ, field.pos) {
						if !field_expr_type.is_ptr() && !field_expr_type.is_pointer()
							&& !c.inside_unsafe {
							field_expr_type_sym := c.table.get_type_symbol(field_expr_type)
							if field_expr_type_sym.kind != .interface_ {
								c.mark_as_referenced(mut &node.fields[i].default_expr,
									true)
							}
						}
					} else {
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
		if node.generic_types.len == 0 && has_generic_types {
			c.error('generic struct declaration must specify the generic type names, e.g. Foo<T>',
				node.pos)
		}
	}
}

pub fn (mut c Checker) struct_init(mut node ast.StructInit) ast.Type {
	if node.typ == ast.void_type {
		// Short syntax `({foo: bar})`
		if c.expected_type == ast.void_type {
			c.error('unexpected short struct syntax', node.pos)
			return ast.void_type
		}
		sym := c.table.get_type_symbol(c.expected_type)
		if sym.kind == .array {
			node.typ = c.table.value_type(c.expected_type)
		} else {
			node.typ = c.expected_type
		}
	}
	struct_sym := c.table.get_type_symbol(node.typ)
	if struct_sym.info is ast.Struct {
		if struct_sym.info.generic_types.len > 0 && struct_sym.info.concrete_types.len == 0
			&& c.table.cur_concrete_types.len == 0 {
			c.error('generic struct init must specify type parameter, e.g. Foo<int>',
				node.pos)
		}
	} else if struct_sym.info is ast.Alias {
		parent_sym := c.table.get_type_symbol(struct_sym.info.parent_type)
		// e.g. ´x := MyMapAlias{}´, should be a cast to alias type ´x := MyMapAlias(map[...]...)´
		if parent_sym.kind == .map {
			alias_str := c.table.type_to_str(node.typ)
			map_str := c.table.type_to_str(struct_sym.info.parent_type)
			c.error('direct map alias init is not possible, use `${alias_str}($map_str{})` instead',
				node.pos)
			return ast.void_type
		}
	}
	// register generic struct type when current fn is generic fn
	if c.table.cur_fn.generic_names.len > 0 {
		c.table.unwrap_generic_type(node.typ, c.table.cur_fn.generic_names, c.table.cur_concrete_types)
	}
	c.ensure_type_exists(node.typ, node.pos) or {}
	type_sym := c.table.get_type_symbol(node.typ)
	if !c.inside_unsafe && type_sym.kind == .sum_type {
		c.note('direct sum type init (`x := SumType{}`) will be removed soon', node.pos)
	}
	// Make sure the first letter is capital, do not allow e.g. `x := string{}`,
	// but `x := T{}` is ok.
	if !c.is_builtin_mod && !c.inside_unsafe && type_sym.language == .v
		&& c.table.cur_concrete_types.len == 0 {
		pos := type_sym.name.last_index('.') or { -1 }
		first_letter := type_sym.name[pos + 1]
		if !first_letter.is_capital() {
			c.error('cannot initialize builtin type `$type_sym.name`', node.pos)
		}
	}
	if type_sym.kind == .sum_type && node.fields.len == 1 {
		sexpr := node.fields[0].expr.str()
		c.error('cast to sum type using `${type_sym.name}($sexpr)` not `$type_sym.name{$sexpr}`',
			node.pos)
	}
	if type_sym.kind == .interface_ && type_sym.language != .js {
		c.error('cannot instantiate interface `$type_sym.name`', node.pos)
	}
	if type_sym.info is ast.Alias {
		if type_sym.info.parent_type.is_number() {
			c.error('cannot instantiate number type alias `$type_sym.name`', node.pos)
			return ast.void_type
		}
	}
	// allow init structs from generic if they're private except the type is from builtin module
	if !type_sym.is_public && type_sym.kind != .placeholder && type_sym.language != .c
		&& (type_sym.mod != c.mod && !(node.typ.has_flag(.generic) && type_sym.mod != 'builtin')) {
		c.error('type `$type_sym.name` is private', node.pos)
	}
	if type_sym.kind == .struct_ {
		info := type_sym.info as ast.Struct
		if info.attrs.len > 0 && info.attrs[0].name == 'noinit' && type_sym.mod != c.mod {
			c.error('struct `$type_sym.name` is declared with a `[noinit]` attribute, so ' +
				'it cannot be initialized with `$type_sym.name{}`', node.pos)
		}
	}
	if type_sym.name.len == 1 && c.table.cur_fn.generic_names.len == 0 {
		c.error('unknown struct `$type_sym.name`', node.pos)
		return 0
	}
	match type_sym.kind {
		.placeholder {
			c.error('unknown struct: $type_sym.name', node.pos)
			return ast.void_type
		}
		// string & array are also structs but .kind of string/array
		.struct_, .string, .array, .alias {
			mut info := ast.Struct{}
			if type_sym.kind == .alias {
				info_t := type_sym.info as ast.Alias
				sym := c.table.get_type_symbol(info_t.parent_type)
				if sym.kind == .placeholder { // pending import symbol did not resolve
					c.error('unknown struct: $type_sym.name', node.pos)
					return ast.void_type
				}
				if sym.kind == .struct_ {
					info = sym.info as ast.Struct
				} else {
					c.error('alias type name: $sym.name is not struct type', node.pos)
				}
			} else {
				info = type_sym.info as ast.Struct
			}
			if node.is_short {
				exp_len := info.fields.len
				got_len := node.fields.len
				if exp_len != got_len {
					amount := if exp_len < got_len { 'many' } else { 'few' }
					c.error('too $amount fields in `$type_sym.name` literal (expecting $exp_len, got $got_len)',
						node.pos)
				}
			}
			mut inited_fields := []string{}
			for i, mut field in node.fields {
				mut field_info := ast.StructField{}
				mut field_name := ''
				if node.is_short {
					if i >= info.fields.len {
						// It doesn't make sense to check for fields that don't exist.
						// We should just stop here.
						break
					}
					field_info = info.fields[i]
					field_name = field_info.name
					node.fields[i].name = field_name
				} else {
					field_name = field.name
					mut exists := true
					field_info = c.table.find_field_with_embeds(type_sym, field_name) or {
						exists = false
						ast.StructField{}
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
				mut expr_type := ast.Type(0)
				mut expected_type := ast.Type(0)
				inited_fields << field_name
				field_type_sym := c.table.get_type_symbol(field_info.typ)
				expected_type = field_info.typ
				c.expected_type = expected_type
				expr_type = c.expr(field.expr)
				if !field_info.typ.has_flag(.optional) {
					expr_type = c.check_expr_opt_call(field.expr, expr_type)
				}
				expr_type_sym := c.table.get_type_symbol(expr_type)
				if field_type_sym.kind == .interface_ {
					if c.type_implements(expr_type, field_info.typ, field.pos) {
						if !expr_type.is_ptr() && !expr_type.is_pointer()
							&& expr_type_sym.kind != .interface_ && !c.inside_unsafe {
							c.mark_as_referenced(mut &field.expr, true)
						}
					}
				} else if expr_type != ast.void_type && expr_type_sym.kind != .placeholder {
					c.check_expected(c.unwrap_generic(expr_type), c.unwrap_generic(field_info.typ)) or {
						c.error('cannot assign to field `$field_info.name`: $err.msg',
							field.pos)
					}
				}
				if field_info.typ.has_flag(.shared_f) {
					if !expr_type.has_flag(.shared_f) && expr_type.is_ptr() {
						c.error('`shared` field must be initialized with `shared` or value',
							field.pos)
					}
				} else {
					if field_info.typ.is_ptr() && !expr_type.is_ptr() && !expr_type.is_pointer()
						&& !expr_type.is_number() {
						c.error('reference field must be initialized with reference',
							field.pos)
					}
				}
				node.fields[i].typ = expr_type
				node.fields[i].expected_type = field_info.typ

				if field_info.typ.has_flag(.optional) {
					c.error('field `$field_info.name` is optional, but initialization of optional fields currently unsupported',
						field.pos)
				}
				if expr_type.is_ptr() && expected_type.is_ptr() {
					if mut field.expr is ast.Ident {
						if mut field.expr.obj is ast.Var {
							mut obj := unsafe { &field.expr.obj }
							if c.fn_scope != voidptr(0) {
								obj = c.fn_scope.find_var(obj.name) or { obj }
							}
							if obj.is_stack_obj && !c.inside_unsafe {
								sym := c.table.get_type_symbol(obj.typ.set_nr_muls(0))
								if !sym.is_heap() && !c.pref.translated {
									suggestion := if sym.kind == .struct_ {
										'declaring `$sym.name` as `[heap]`'
									} else {
										'wrapping the `$sym.name` object in a `struct` declared as `[heap]`'
									}
									c.error('`$field.expr.name` cannot be assigned outside `unsafe` blocks as it might refer to an object stored on stack. Consider ${suggestion}.',
										field.expr.pos)
								}
							}
						}
					}
				}
			}
			// Check uninitialized refs/sum types
			for field in info.fields {
				if field.has_default_expr || field.name in inited_fields {
					continue
				}
				if field.typ.is_ptr() && !field.typ.has_flag(.shared_f) && !node.has_update_expr
					&& !c.pref.translated {
					c.error('reference field `${type_sym.name}.$field.name` must be initialized',
						node.pos)
				}
				// Do not allow empty uninitialized interfaces
				sym := c.table.get_type_symbol(field.typ)
				if sym.kind == .interface_ {
					// TODO: should be an error instead, but first `ui` needs updating.
					c.note('interface field `${type_sym.name}.$field.name` must be initialized',
						node.pos)
				}
				// Do not allow empty uninitialized sum types
				/*
				sym := c.table.get_type_symbol(field.typ)
				if sym.kind == .sum_type {
					c.warn('sum type field `${type_sym.name}.$field.name` must be initialized',
						node.pos)
				}
				*/
				// Check for `[required]` struct attr
				if field.attrs.contains('required') && !node.is_short && !node.has_update_expr {
					mut found := false
					for init_field in node.fields {
						if field.name == init_field.name {
							found = true
							break
						}
					}
					if !found {
						c.error('field `${type_sym.name}.$field.name` must be initialized',
							node.pos)
					}
				}
			}
		}
		else {}
	}
	if node.has_update_expr {
		update_type := c.expr(node.update_expr)
		node.update_expr_type = update_type
		if c.table.type_kind(update_type) != .struct_ {
			s := c.table.type_to_str(update_type)
			c.error('expected struct, found `$s`', node.update_expr.position())
		} else if update_type != node.typ {
			from_sym := c.table.get_type_symbol(update_type)
			to_sym := c.table.get_type_symbol(node.typ)
			from_info := from_sym.info as ast.Struct
			to_info := to_sym.info as ast.Struct
			// TODO this check is too strict
			if !c.check_struct_signature(from_info, to_info) {
				c.error('struct `$from_sym.name` is not compatible with struct `$to_sym.name`',
					node.update_expr.position())
			}
		}
		if !node.update_expr.is_lvalue() {
			// cgen will repeat `update_expr` for each field
			// so enforce an lvalue for efficiency
			c.error('expression is not an lvalue', node.update_expr.position())
		}
	}
	return node.typ
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

pub fn (mut c Checker) infix_expr(mut node ast.InfixExpr) ast.Type {
	former_expected_type := c.expected_type
	defer {
		c.expected_type = former_expected_type
	}
	left_type := c.expr(node.left)
	node.left_type = left_type
	c.expected_type = left_type
	right_type := c.expr(node.right)
	node.right_type = right_type
	if left_type.is_number() && !left_type.is_ptr()
		&& right_type in [ast.int_literal_type, ast.float_literal_type] {
		node.right_type = left_type
	}
	if right_type.is_number() && !right_type.is_ptr()
		&& left_type in [ast.int_literal_type, ast.float_literal_type] {
		node.left_type = right_type
	}
	mut right_sym := c.table.get_type_symbol(right_type)
	right_final := c.table.get_final_type_symbol(right_type)
	mut left_sym := c.table.get_type_symbol(left_type)
	left_final := c.table.get_final_type_symbol(left_type)
	left_pos := node.left.position()
	right_pos := node.right.position()
	left_right_pos := left_pos.extend(right_pos)
	if left_type.is_any_kind_of_pointer()
		&& node.op in [.plus, .minus, .mul, .div, .mod, .xor, .amp, .pipe] {
		if (right_type.is_any_kind_of_pointer() && node.op != .minus)
			|| (!right_type.is_any_kind_of_pointer() && node.op !in [.plus, .minus]) {
			left_name := c.table.type_to_str(left_type)
			right_name := c.table.type_to_str(right_type)
			c.error('invalid operator `$node.op` to `$left_name` and `$right_name`', left_right_pos)
		} else if node.op in [.plus, .minus] {
			if !c.inside_unsafe && !node.left.is_auto_deref_var() && !node.right.is_auto_deref_var() {
				c.warn('pointer arithmetic is only allowed in `unsafe` blocks', left_right_pos)
			}
			if left_type == ast.voidptr_type {
				c.error('`$node.op` cannot be used with `voidptr`', left_pos)
			}
		}
	}
	mut return_type := left_type
	if node.op != .key_is {
		match mut node.left {
			ast.Ident, ast.SelectorExpr {
				if node.left.is_mut {
					c.error('remove unnecessary `mut`', node.left.mut_pos)
				}
			}
			else {}
		}
	}
	eq_ne := node.op in [.eq, .ne]
	// Single side check
	// Place these branches according to ops' usage frequency to accelerate.
	// TODO: First branch includes ops where single side check is not needed, or needed but hasn't been implemented.
	// TODO: Some of the checks are not single side. Should find a better way to organize them.
	match node.op {
		// .eq, .ne, .gt, .lt, .ge, .le, .and, .logical_or, .dot, .key_as, .right_shift {}
		.eq, .ne {
			is_mismatch :=
				(left_sym.kind == .alias && right_sym.kind in [.struct_, .array, .sum_type])
				|| (right_sym.kind == .alias && left_sym.kind in [.struct_, .array, .sum_type])
			if is_mismatch {
				c.error('possible type mismatch of compared values of `$node.op` operation',
					left_right_pos)
			}
		}
		.key_in, .not_in {
			match right_final.kind {
				.array {
					if left_sym.kind !in [.sum_type, .interface_] {
						elem_type := right_final.array_info().elem_type
						c.check_expected(left_type, elem_type) or {
							c.error('left operand to `$node.op` does not match the array element type: $err.msg',
								left_right_pos)
						}
					}
				}
				.map {
					map_info := right_final.map_info()
					c.check_expected(left_type, map_info.key_type) or {
						c.error('left operand to `$node.op` does not match the map key type: $err.msg',
							left_right_pos)
					}
					node.left_type = map_info.key_type
				}
				else {
					c.error('`$node.op.str()` can only be used with an array/map/string',
						node.pos)
				}
			}
			return ast.bool_type
		}
		.plus, .minus, .mul, .div, .mod, .xor, .amp, .pipe { // binary operators that expect matching types
			if right_sym.info is ast.Alias && (right_sym.info as ast.Alias).language != .c
				&& c.mod == c.table.type_to_str(right_type).split('.')[0]
				&& c.table.get_type_symbol((right_sym.info as ast.Alias).parent_type).is_primitive() {
				right_sym = c.table.get_type_symbol((right_sym.info as ast.Alias).parent_type)
			}
			if left_sym.info is ast.Alias && (left_sym.info as ast.Alias).language != .c
				&& c.mod == c.table.type_to_str(left_type).split('.')[0]
				&& c.table.get_type_symbol((left_sym.info as ast.Alias).parent_type).is_primitive() {
				left_sym = c.table.get_type_symbol((left_sym.info as ast.Alias).parent_type)
			}
			// Check if the alias type is not a primitive then allow using operator overloading for aliased `arrays` and `maps`
			if left_sym.kind == .alias && left_sym.info is ast.Alias
				&& !(c.table.get_type_symbol((left_sym.info as ast.Alias).parent_type).is_primitive()) {
				if left_sym.has_method(node.op.str()) {
					if method := left_sym.find_method(node.op.str()) {
						return_type = method.return_type
					} else {
						return_type = left_type
					}
				} else {
					left_name := c.table.type_to_str(left_type)
					right_name := c.table.type_to_str(right_type)
					if left_name == right_name {
						c.error('undefined operation `$left_name` $node.op.str() `$right_name`',
							left_right_pos)
					} else {
						c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
					}
				}
			} else if right_sym.kind == .alias && right_sym.info is ast.Alias
				&& !(c.table.get_type_symbol((right_sym.info as ast.Alias).parent_type).is_primitive()) {
				if right_sym.has_method(node.op.str()) {
					if method := right_sym.find_method(node.op.str()) {
						return_type = method.return_type
					} else {
						return_type = right_type
					}
				} else {
					left_name := c.table.type_to_str(left_type)
					right_name := c.table.type_to_str(right_type)
					if left_name == right_name {
						c.error('undefined operation `$left_name` $node.op.str() `$right_name`',
							left_right_pos)
					} else {
						c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
					}
				}
			}
			if left_sym.kind in [.array, .array_fixed, .map, .struct_] {
				if left_sym.has_method(node.op.str()) {
					if method := left_sym.find_method(node.op.str()) {
						return_type = method.return_type
					} else {
						return_type = left_type
					}
				} else {
					if left_sym.kind == .struct_
						&& (left_sym.info as ast.Struct).generic_types.len > 0 {
						return_type = left_type
					} else {
						left_name := c.table.type_to_str(left_type)
						right_name := c.table.type_to_str(right_type)
						if left_name == right_name {
							c.error('undefined operation `$left_name` $node.op.str() `$right_name`',
								left_right_pos)
						} else {
							c.error('mismatched types `$left_name` and `$right_name`',
								left_right_pos)
						}
					}
				}
			} else if right_sym.kind in [.array, .array_fixed, .map, .struct_] {
				if right_sym.has_method(node.op.str()) {
					if method := right_sym.find_method(node.op.str()) {
						return_type = method.return_type
					} else {
						return_type = right_type
					}
				} else {
					left_name := c.table.type_to_str(left_type)
					right_name := c.table.type_to_str(right_type)
					if left_name == right_name {
						c.error('undefined operation `$left_name` $node.op.str() `$right_name`',
							left_right_pos)
					} else {
						c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
					}
				}
			} else if node.left.is_auto_deref_var() || node.right.is_auto_deref_var() {
				deref_left_type := if node.left.is_auto_deref_var() {
					left_type.deref()
				} else {
					left_type
				}
				deref_right_type := if node.right.is_auto_deref_var() {
					right_type.deref()
				} else {
					right_type
				}
				left_name := c.table.type_to_str(c.table.mktyp(deref_left_type))
				right_name := c.table.type_to_str(c.table.mktyp(deref_right_type))
				if left_name != right_name {
					c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
				}
			} else {
				unaliased_left_type := c.table.unalias_num_type(left_type)
				unalias_right_type := c.table.unalias_num_type(right_type)
				mut promoted_type := c.promote(unaliased_left_type, unalias_right_type)
				// substract pointers is allowed in unsafe block
				is_allowed_pointer_arithmetic := left_type.is_any_kind_of_pointer()
					&& right_type.is_any_kind_of_pointer() && node.op == .minus
				if is_allowed_pointer_arithmetic {
					promoted_type = ast.int_type
				}
				if promoted_type.idx() == ast.void_type_idx {
					left_name := c.table.type_to_str(left_type)
					right_name := c.table.type_to_str(right_type)
					c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
				} else if promoted_type.has_flag(.optional) {
					s := c.table.type_to_str(promoted_type)
					c.error('`$node.op` cannot be used with `$s`', node.pos)
				} else if promoted_type.is_float() {
					if node.op in [.mod, .xor, .amp, .pipe] {
						side := if left_type == promoted_type { 'left' } else { 'right' }
						pos := if left_type == promoted_type { left_pos } else { right_pos }
						name := if left_type == promoted_type {
							left_sym.name
						} else {
							right_sym.name
						}
						if node.op == .mod {
							c.error('float modulo not allowed, use math.fmod() instead',
								pos)
						} else {
							c.error('$side type of `$node.op.str()` cannot be non-integer type `$name`',
								pos)
						}
					}
				}
				if node.op in [.div, .mod] {
					c.check_div_mod_by_zero(node.right, node.op)
				}
				return_type = promoted_type
			}
		}
		.gt, .lt, .ge, .le {
			if left_sym.kind in [.array, .array_fixed] && right_sym.kind in [.array, .array_fixed] {
				c.error('only `==` and `!=` are defined on arrays', node.pos)
			} else if left_sym.kind == .struct_
				&& (left_sym.info as ast.Struct).generic_types.len > 0 {
				return ast.bool_type
			} else if left_sym.kind == .struct_ && right_sym.kind == .struct_
				&& node.op in [.eq, .lt] {
				if !(left_sym.has_method(node.op.str()) && right_sym.has_method(node.op.str())) {
					left_name := c.table.type_to_str(left_type)
					right_name := c.table.type_to_str(right_type)
					if left_name == right_name {
						c.error('undefined operation `$left_name` $node.op.str() `$right_name`',
							left_right_pos)
					} else {
						c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
					}
				}
			}
			if left_sym.kind == .struct_ && right_sym.kind == .struct_ {
				if !left_sym.has_method('<') && node.op in [.ge, .le] {
					c.error('cannot use `$node.op` as `<` operator method is not defined',
						left_right_pos)
				} else if !left_sym.has_method('<') && node.op == .gt {
					c.error('cannot use `>` as `<=` operator method is not defined', left_right_pos)
				}
			}
		}
		.left_shift {
			if left_final.kind == .array {
				if !node.is_stmt {
					c.error('array append cannot be used in an expression', node.pos)
				}
				// `array << elm`
				c.check_expr_opt_call(node.right, right_type)
				node.auto_locked, _ = c.fail_if_immutable(node.left)
				left_value_type := c.table.value_type(c.unwrap_generic(left_type))
				left_value_sym := c.table.get_type_symbol(c.unwrap_generic(left_value_type))
				if left_value_sym.kind == .interface_ {
					if right_final.kind != .array {
						// []Animal << Cat
						if c.type_implements(right_type, left_value_type, right_pos) {
							if !right_type.is_ptr() && !right_type.is_pointer() && !c.inside_unsafe
								&& right_sym.kind != .interface_ {
								c.mark_as_referenced(mut &node.right, true)
							}
						}
					} else {
						// []Animal << []Cat
						c.type_implements(c.table.value_type(right_type), left_value_type,
							right_pos)
					}
					return ast.void_type
				}
				// []T << T or []T << []T
				unwrapped_right_type := c.unwrap_generic(right_type)
				if c.check_types(unwrapped_right_type, left_value_type) {
					// []&T << T is wrong: we check for that, !(T.is_ptr()) && ?(&T).is_ptr()
					if !(!unwrapped_right_type.is_ptr() && left_value_type.is_ptr()
						&& left_value_type.share() == .mut_t) {
						return ast.void_type
					}
				} else if c.check_types(unwrapped_right_type, c.unwrap_generic(left_type)) {
					return ast.void_type
				}
				c.error('cannot append `$right_sym.name` to `$left_sym.name`', right_pos)
				return ast.void_type
			} else {
				return c.check_shift(mut node, left_type, right_type)
			}
		}
		.right_shift {
			return c.check_shift(mut node, left_type, right_type)
		}
		.unsigned_right_shift {
			modified_left_type := if !left_type.is_int() {
				c.error('invalid operation: shift on type `${c.table.get_type_symbol(left_type).name}`',
					left_pos)
				ast.void_type_idx
			} else if left_type.is_int_literal() {
				// int literal => i64
				ast.u32_type_idx
			} else if left_type.is_unsigned() {
				left_type
			} else {
				// signed types' idx adds with 5 will get correct relative unsigned type
				// i8 		=> byte
				// i16 		=> u16
				// int  	=> u32
				// i64  	=> u64
				// isize	=> usize
				// i128 	=> u128 NOT IMPLEMENTED YET
				left_type.idx() + ast.u32_type_idx - ast.int_type_idx
			}

			if modified_left_type == 0 {
				return ast.void_type
			}

			node = ast.InfixExpr{
				left: ast.CastExpr{
					expr: node.left
					typ: modified_left_type
					typname: c.table.type_str(modified_left_type)
					pos: node.pos
				}
				left_type: left_type
				op: .right_shift
				right: node.right
				right_type: right_type
				is_stmt: false
				pos: node.pos
				auto_locked: node.auto_locked
				or_block: node.or_block
			}

			return c.check_shift(mut node, left_type, right_type)
		}
		.key_is, .not_is {
			right_expr := node.right
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
			if typ != ast.Type(0) {
				typ_sym := c.table.get_type_symbol(typ)
				op := node.op.str()
				if typ_sym.kind == .placeholder {
					c.error('$op: type `$typ_sym.name` does not exist', right_expr.position())
				}
				if left_sym.kind !in [.interface_, .sum_type] {
					c.error('`$op` can only be used with interfaces and sum types', node.pos)
				} else if mut left_sym.info is ast.SumType {
					if typ !in left_sym.info.variants {
						c.error('`$left_sym.name` has no variant `$right_sym.name`', node.pos)
					}
				}
			}
			return ast.bool_type
		}
		.arrow { // `chan <- elem`
			if left_sym.kind == .chan {
				chan_info := left_sym.chan_info()
				elem_type := chan_info.elem_type
				if !c.check_types(right_type, elem_type) {
					c.error('cannot push `$right_sym.name` on `$left_sym.name`', right_pos)
				}
				if chan_info.is_mut {
					// TODO: The error message of the following could be more specific...
					c.fail_if_immutable(node.right)
				}
				if elem_type.is_ptr() && !right_type.is_ptr() {
					c.error('cannot push non-reference `$right_sym.name` on `$left_sym.name`',
						right_pos)
				}
				c.stmts_ending_with_expression(node.or_block.stmts)
			} else {
				c.error('cannot push on non-channel `$left_sym.name`', left_pos)
			}
			return ast.void_type
		}
		.and, .logical_or {
			if !c.pref.translated {
				if node.left_type != ast.bool_type_idx {
					c.error('left operand for `$node.op` is not a boolean', node.left.position())
				}
				if node.right_type != ast.bool_type_idx {
					c.error('right operand for `$node.op` is not a boolean', node.right.position())
				}
			}
			if mut node.left is ast.InfixExpr {
				if node.left.op != node.op && node.left.op in [.logical_or, .and] {
					// for example: `(a && b) || c` instead of `a && b || c`
					c.error('ambiguous boolean expression. use `()` to ensure correct order of operations',
						node.pos)
				}
			}
		}
		else {}
	}
	// TODO: Absorb this block into the above single side check block to accelerate.
	if left_type == ast.bool_type && node.op !in [.eq, .ne, .logical_or, .and] {
		c.error('bool types only have the following operators defined: `==`, `!=`, `||`, and `&&`',
			node.pos)
	} else if left_type == ast.string_type && node.op !in [.plus, .eq, .ne, .lt, .gt, .le, .ge] {
		// TODO broken !in
		c.error('string types only have the following operators defined: `==`, `!=`, `<`, `>`, `<=`, `>=`, and `+`',
			node.pos)
	} else if left_sym.kind == .enum_ && right_sym.kind == .enum_ && !eq_ne {
		left_enum := left_sym.info as ast.Enum
		right_enum := right_sym.info as ast.Enum
		if left_enum.is_flag && right_enum.is_flag {
			// `[flag]` tagged enums are a special case that allow also `|` and `&` binary operators
			if node.op !in [.pipe, .amp] {
				c.error('only `==`, `!=`, `|` and `&` are defined on `[flag]` tagged `enum`, use an explicit cast to `int` if needed',
					node.pos)
			}
		} else if !c.pref.translated {
			// Regular enums
			c.error('only `==` and `!=` are defined on `enum`, use an explicit cast to `int` if needed',
				node.pos)
		}
	}
	// sum types can't have any infix operation except of `is`, `eq`, `ne`.
	// `is` is checked before and doesn't reach this.
	if c.table.type_kind(left_type) == .sum_type && !eq_ne {
		c.error('cannot use operator `$node.op` with `$left_sym.name`', node.pos)
	} else if c.table.type_kind(right_type) == .sum_type && !eq_ne {
		c.error('cannot use operator `$node.op` with `$right_sym.name`', node.pos)
	}
	// TODO move this to symmetric_check? Right now it would break `return 0` for `fn()?int `
	left_is_optional := left_type.has_flag(.optional)
	right_is_optional := right_type.has_flag(.optional)
	if (left_is_optional && !right_is_optional) || (!left_is_optional && right_is_optional) {
		c.error('unwrapped optional cannot be used in an infix expression', left_right_pos)
	}
	// Dual sides check (compatibility check)
	if !(c.symmetric_check(left_type, right_type) && c.symmetric_check(right_type, left_type))
		&& !c.pref.translated && !node.left.is_auto_deref_var() && !node.right.is_auto_deref_var() {
		// for type-unresolved consts
		if left_type == ast.void_type || right_type == ast.void_type {
			return ast.void_type
		}
		if left_type.nr_muls() > 0 && right_type.is_int() {
			// pointer arithmetic is fine, it is checked in other places
			return return_type
		}
		c.error('infix expr: cannot use `$right_sym.name` (right expression) as `$left_sym.name`',
			left_right_pos)
	}
	/*
	if (node.left is ast.InfixExpr &&
		(node.left as ast.InfixExpr).op == .inc) ||
		(node.right is ast.InfixExpr && (node.right as ast.InfixExpr).op == .inc) {
		c.warn('`++` and `--` are statements, not expressions', node.pos)
	}
	*/
	return if node.op.is_relational() { ast.bool_type } else { return_type }
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
					mut has_field := true
					mut field_info := c.table.find_field_with_embeds(typ_sym, expr.field_name) or {
						has_field = false
						ast.StructField{}
					}
					if !has_field {
						type_str := c.table.type_to_str(expr.expr_type)
						c.error('unknown field `${type_str}.$expr.field_name`', expr.pos)
						return '', pos
					}
					if field_info.typ.has_flag(.shared_f) {
						expr_name := '${expr.expr}.$expr.field_name'
						if expr_name !in c.locked_names {
							if c.locked_names.len > 0 || c.rlocked_names.len > 0 {
								if expr_name in c.rlocked_names {
									c.error('$expr_name has an `rlock` but needs a `lock`',
										expr.pos)
								} else {
									c.error('$expr_name must be added to the `lock` list above',
										expr.pos)
								}
								return '', expr.pos
							}
							to_lock = expr_name
							pos = expr.pos
						}
					} else {
						if !field_info.is_mut && !c.pref.translated {
							type_str := c.table.type_to_str(expr.expr_type)
							c.error('field `$expr.field_name` of struct `$type_str` is immutable',
								expr.pos)
						}
						to_lock, pos = c.fail_if_immutable(expr.expr)
					}
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
						return '', expr.pos
					}
					c.fail_if_immutable(expr.expr)
				}
				.sum_type {
					sumtype_info := typ_sym.info as ast.SumType
					mut field_info := sumtype_info.find_field(expr.field_name) or {
						type_str := c.table.type_to_str(expr.expr_type)
						c.error('unknown field `${type_str}.$expr.field_name`', expr.pos)
						return '', pos
					}
					if !field_info.is_mut {
						type_str := c.table.type_to_str(expr.expr_type)
						c.error('field `$expr.field_name` of sumtype `$type_str` is immutable',
							expr.pos)
						return '', expr.pos
					}
					c.fail_if_immutable(expr.expr)
				}
				.array, .string {
					// should only happen in `builtin` and unsafe blocks
					inside_builtin := c.file.mod.name == 'builtin'
					if !inside_builtin && !c.inside_unsafe {
						c.error('`$typ_sym.kind` can not be modified', expr.pos)
						return '', expr.pos
					}
				}
				.aggregate, .placeholder {
					c.fail_if_immutable(expr.expr)
				}
				else {
					c.error('unexpected symbol `$typ_sym.kind`', expr.pos)
					return '', expr.pos
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
		ast.InfixExpr {
			return '', pos
		}
		else {
			if !expr.is_lit() {
				c.error('unexpected expression `$expr.type_name()`', expr.position())
				return '', pos
			}
		}
	}
	if explicit_lock_needed {
		c.error('`$to_lock` is `shared` and needs explicit lock for `$expr.type_name()`',
			pos)
		to_lock = ''
	}
	return to_lock, pos
}

pub fn (mut c Checker) call_expr(mut node ast.CallExpr) ast.Type {
	// First check everything that applies to both fns and methods
	// TODO merge logic from method_call and fn_call
	/*
	for i, call_arg in node.args {
		if call_arg.is_mut {
			c.fail_if_immutable(call_arg.expr)
			if !arg.is_mut {
				tok := call_arg.share.str()
				c.error('`$node.name` parameter `$arg.name` is not `$tok`, `$tok` is not needed`',
					call_arg.expr.position())
			} else if arg.typ.share() != call_arg.share {
				c.error('wrong shared type', call_arg.expr.position())
			}
		} else {
			if arg.is_mut && (!call_arg.is_mut || arg.typ.share() != call_arg.share) {
				tok := call_arg.share.str()
				c.error('`$node.name` parameter `$arg.name` is `$tok`, you need to provide `$tok` e.g. `$tok arg${i+1}`',
					call_arg.expr.position())
			}
		}
	}
	*/
	// Now call `method_call` or `fn_call` for specific checks.
	old_inside_fn_arg := c.inside_fn_arg
	c.inside_fn_arg = true
	mut continue_check := true
	typ := if node.is_method {
		c.method_call(mut node)
	} else {
		c.fn_call(mut node, mut continue_check)
	}
	if !continue_check {
		return ast.void_type
	}
	c.inside_fn_arg = old_inside_fn_arg
	// autofree: mark args that have to be freed (after saving them in tmp exprs)
	free_tmp_arg_vars := c.pref.autofree && !c.is_builtin_mod && node.args.len > 0
		&& !node.args[0].typ.has_flag(.optional)
	if free_tmp_arg_vars && !c.inside_const {
		for i, arg in node.args {
			if arg.typ != ast.string_type {
				continue
			}
			if arg.expr in [ast.Ident, ast.StringLiteral, ast.SelectorExpr] {
				// Simple expressions like variables, string literals, selector expressions
				// (`x.field`) can't result in allocations and don't need to be assigned to
				// temporary vars.
				// Only expressions like `str + 'b'` need to be freed.
				continue
			}
			node.args[i].is_tmp_autofree = true
		}
		// TODO copy pasta from above
		if node.receiver_type == ast.string_type
			&& node.left !in [ast.Ident, ast.StringLiteral, ast.SelectorExpr] {
			node.free_receiver = true
		}
	}
	c.expected_or_type = node.return_type.clear_flag(.optional)
	c.stmts_ending_with_expression(node.or_block.stmts)
	c.expected_or_type = ast.void_type
	if node.or_block.kind == .propagate && !c.table.cur_fn.return_type.has_flag(.optional)
		&& !c.inside_const {
		if !c.table.cur_fn.is_main {
			c.error('to propagate the optional call, `$c.table.cur_fn.name` must return an optional',
				node.or_block.pos)
		}
	}
	return typ
}

fn (mut c Checker) check_map_and_filter(is_map bool, elem_typ ast.Type, node ast.CallExpr) {
	if node.args.len != 1 {
		c.error('expected 1 argument, but got $node.args.len', node.pos)
		// Finish early so that it doesn't fail later
		return
	}
	elem_sym := c.table.get_type_symbol(elem_typ)
	arg_expr := node.args[0].expr
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
					c.error('function needs exactly 1 argument', node.pos)
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
				// NOTE: bug accessing typ field on sumtype variant (not cast properly).
				// leaving this here as the resulting issue is notoriously hard to debug.
				// if !is_map && arg_expr.info.typ != ast.bool_type {
				if !is_map && arg_expr.var_info().typ != ast.bool_type {
					c.error('type mismatch, should be bool', arg_expr.pos)
				}
			}
		}
		ast.CallExpr {
			if is_map && arg_expr.return_type in [ast.void_type, 0] {
				c.error('type mismatch, `$arg_expr.name` does not return anything', arg_expr.pos)
			} else if !is_map && arg_expr.return_type != ast.bool_type {
				c.error('type mismatch, `$arg_expr.name` must return a bool', arg_expr.pos)
			}
		}
		else {}
	}
}

pub fn (mut c Checker) check_expected_arg_count(mut node ast.CallExpr, f &ast.Fn) ? {
	nr_args := node.args.len
	nr_params := if node.is_method && f.params.len > 0 { f.params.len - 1 } else { f.params.len }
	mut min_required_params := f.params.len
	if node.is_method {
		min_required_params--
	}
	if f.is_variadic {
		min_required_params--
	}
	if min_required_params < 0 {
		min_required_params = 0
	}
	if nr_args < min_required_params {
		if min_required_params == nr_args + 1 {
			last_typ := f.params.last().typ
			last_sym := c.table.get_type_symbol(last_typ)
			if last_sym.info is ast.Struct {
				is_params := last_sym.info.attrs.filter(it.name == 'params' && !it.has_arg).len > 0
				if is_params {
					// allow empty trailing struct syntax arg (`f()` where `f` is `fn(ConfigStruct)`)
					node.args << ast.CallArg{
						expr: ast.StructInit{
							typ: last_typ
						}
					}
					return
				}
			}
		}
		c.error('expected $min_required_params arguments, but got $nr_args', node.pos)
		return error('')
	} else if !f.is_variadic && nr_args > nr_params {
		unexpected_args_pos := node.args[min_required_params].pos.extend(node.args.last().pos)
		c.error('expected $min_required_params arguments, but got $nr_args', unexpected_args_pos)
		return error('')
	}
}

pub fn (mut c Checker) method_call(mut node ast.CallExpr) ast.Type {
	left_type := c.expr(node.left)
	c.expected_type = left_type
	mut is_generic := left_type.has_flag(.generic)
	// x is Bar<T>, x.foo() -> x.foo<T>()
	if is_generic && node.concrete_types.len == 0 {
		rec_sym := c.table.get_type_symbol(left_type)
		if rec_sym.info is ast.Struct {
			node.concrete_types = rec_sym.info.generic_types
		}
	}
	node.left_type = left_type
	// Set default values for .return_type & .receiver_type too,
	// or there will be hard tRo diagnose 0 type panics in cgen.
	node.return_type = left_type
	node.receiver_type = left_type

	if c.table.cur_fn.generic_names.len > 0 {
		c.table.unwrap_generic_type(left_type, c.table.cur_fn.generic_names, c.table.cur_concrete_types)
	}
	unwrapped_left_type := c.unwrap_generic(left_type)
	left_sym := c.table.get_type_symbol(unwrapped_left_type)
	final_left_sym := c.table.get_final_type_symbol(unwrapped_left_type)

	method_name := node.name
	mut unknown_method_msg := if field := c.table.find_field(left_sym, method_name) {
		'unknown method `$field.name` did you mean to access the field with the same name instead?'
	} else {
		'unknown method or field: `${left_sym.name}.$method_name`'
	}
	if left_type.has_flag(.optional) {
		c.error('optional type cannot be called directly', node.left.position())
		return ast.void_type
	}
	if left_sym.kind in [.sum_type, .interface_] {
		if method_name == 'type_name' {
			return ast.string_type
		}
		if method_name == 'type_idx' {
			return ast.int_type
		}
	}
	if left_type == ast.void_type {
		// No need to print this error, since this means that the variable is unknown,
		// and there already was an error before.
		// c.error('`void` type has no methods', node.left.position())
		return ast.void_type
	}
	mut concrete_types := []ast.Type{}
	for concrete_type in node.concrete_types {
		if concrete_type.has_flag(.generic) {
			concrete_types << c.unwrap_generic(concrete_type)
		} else {
			concrete_types << concrete_type
		}
	}
	if concrete_types.len > 0 {
		if c.table.register_fn_concrete_types(node.name, concrete_types) {
			c.need_recheck_generic_fns = true
		}
	}
	// TODO: remove this for actual methods, use only for compiler magic
	// FIXME: Argument count != 1 will break these
	if left_sym.kind == .array && method_name in checker.array_builtin_methods {
		return c.array_builtin_method_call(mut node, left_type, left_sym)
	} else if (left_sym.kind == .map || final_left_sym.kind == .map)
		&& method_name in ['clone', 'keys', 'move', 'delete'] {
		if left_sym.kind == .map {
			return c.map_builtin_method_call(mut node, left_type, left_sym)
		} else {
			parent_type := (left_sym.info as ast.Alias).parent_type
			return c.map_builtin_method_call(mut node, parent_type, final_left_sym)
		}
	} else if left_sym.kind == .array && method_name in ['insert', 'prepend'] {
		if method_name == 'insert' {
			if node.args.len != 2 {
				c.error('`array.insert()` should have 2 arguments, e.g. `insert(1, val)`',
					node.pos)
				return ast.void_type
			} else {
				arg_type := c.expr(node.args[0].expr)
				if arg_type !in [ast.int_type, ast.int_literal_type] {
					c.error('the first argument of `array.insert()` should be integer',
						node.args[0].expr.position())
					return ast.void_type
				}
			}
		} else {
			if node.args.len != 1 {
				c.error('`array.prepend()` should have 1 argument, e.g. `prepend(val)`',
					node.pos)
				return ast.void_type
			}
		}
		info := left_sym.info as ast.Array
		arg_expr := if method_name == 'insert' { node.args[1].expr } else { node.args[0].expr }
		arg_type := c.expr(arg_expr)
		arg_sym := c.table.get_type_symbol(arg_type)
		if !c.check_types(arg_type, info.elem_type) && !c.check_types(left_type, arg_type) {
			c.error('cannot $method_name `$arg_sym.name` to `$left_sym.name`', arg_expr.position())
		}
	} else if final_left_sym.kind == .array && method_name in ['first', 'last', 'pop'] {
		if final_left_sym.info is ast.Array {
			node.return_type = final_left_sym.info.elem_type
			return node.return_type
		}
	} else if left_sym.kind == .thread && method_name == 'wait' {
		info := left_sym.info as ast.Thread
		if node.args.len > 0 {
			c.error('wait() does not have any arguments', node.args[0].pos)
		}
		node.return_type = info.return_type
		return info.return_type
	} else if left_sym.kind == .char && left_type.nr_muls() == 0 && method_name == 'str' {
		c.error('calling `.str()` on type `char` is not allowed, use its address or cast it to an integer instead',
			node.left.position().extend(node.pos))
		return ast.void_type
	}
	mut method := ast.Fn{}
	mut has_method := false
	mut is_method_from_embed := false
	if m := c.table.find_method(left_sym, method_name) {
		method = m
		has_method = true
	} else {
		if left_sym.kind in [.struct_, .sum_type, .interface_] {
			mut parent_type := ast.void_type
			if left_sym.info is ast.Struct {
				parent_type = left_sym.info.parent_type
			} else if left_sym.info is ast.SumType {
				parent_type = left_sym.info.parent_type
			} else if left_sym.info is ast.Interface {
				parent_type = left_sym.info.parent_type
			}
			if parent_type != 0 {
				type_sym := c.table.get_type_symbol(parent_type)
				if m := c.table.find_method(type_sym, method_name) {
					method = m
					has_method = true
					is_generic = true
				}
			}
		}
		if !has_method {
			has_method = true
			mut embed_types := []ast.Type{}
			method, embed_types = c.table.find_method_from_embeds(left_sym, method_name) or {
				if err.msg != '' {
					c.error(err.msg, node.pos)
				}
				has_method = false
				ast.Fn{}, []ast.Type{}
			}
			if embed_types.len != 0 {
				is_method_from_embed = true
				node.from_embed_types = embed_types
			}
		}
		if left_sym.kind == .aggregate {
			// the error message contains the problematic type
			unknown_method_msg = err.msg
		}
	}
	if has_method {
		node.is_noreturn = method.is_noreturn
		node.is_ctor_new = method.is_ctor_new
		if !method.is_pub && !c.pref.is_test && method.mod != c.mod {
			// If a private method is called outside of the module
			// its receiver type is defined in, show an error.
			// println('warn $method_name lef.mod=$left_type_sym.mod c.mod=$c.mod')
			c.error('method `${left_sym.name}.$method_name` is private', node.pos)
		}
		rec_share := method.params[0].typ.share()
		if rec_share == .shared_t && (c.locked_names.len > 0 || c.rlocked_names.len > 0) {
			c.error('method with `shared` receiver cannot be called inside `lock`/`rlock` block',
				node.pos)
		}
		if method.params[0].is_mut {
			to_lock, pos := c.fail_if_immutable(node.left)
			if !node.left.is_lvalue() {
				c.error('cannot pass expression as `mut`', node.left.position())
			}
			// node.is_mut = true
			if to_lock != '' && rec_share != .shared_t {
				c.error('$to_lock is `shared` and must be `lock`ed to be passed as `mut`',
					pos)
			}
		} else {
			c.fail_if_unreadable(node.left, left_type, 'receiver')
		}
		if left_sym.language != .js && (!left_sym.is_builtin() && method.mod != 'builtin')
			&& method.language == .v && method.no_body {
			c.error('cannot call a method that does not have a body', node.pos)
		}
		if method.return_type == ast.void_type && method.is_conditional && method.ctdefine_idx != -1 {
			node.should_be_skipped = c.evaluate_once_comptime_if_attribute(mut method.attrs[method.ctdefine_idx])
		}
		c.check_expected_arg_count(mut node, method) or { return method.return_type }
		mut exp_arg_typ := ast.Type(0) // type of 1st arg for special builtin methods
		mut param_is_mut := false
		mut no_type_promotion := false
		if left_sym.kind == .chan {
			elem_typ := (left_sym.info as ast.Chan).elem_type
			if method_name == 'try_push' {
				exp_arg_typ = elem_typ.ref()
			} else if method_name == 'try_pop' {
				exp_arg_typ = elem_typ
				param_is_mut = true
				no_type_promotion = true
			}
		}
		// if method_name == 'clone' {
		// println('CLONE nr args=$method.args.len')
		// }
		// node.args << method.args[0].typ
		// node.exp_arg_types << method.args[0].typ
		for i, mut arg in node.args {
			if i > 0 || exp_arg_typ == ast.Type(0) {
				exp_arg_typ = if method.is_variadic && i >= method.params.len - 1 {
					method.params[method.params.len - 1].typ
				} else {
					method.params[i + 1].typ
				}
				param_is_mut = false
				no_type_promotion = false
			}
			exp_arg_sym := c.table.get_type_symbol(exp_arg_typ)
			c.expected_type = exp_arg_typ
			got_arg_typ := c.check_expr_opt_call(arg.expr, c.expr(arg.expr))
			node.args[i].typ = got_arg_typ
			if no_type_promotion {
				if got_arg_typ != exp_arg_typ {
					c.error('cannot use `${c.table.get_type_symbol(got_arg_typ).name}` as argument for `$method.name` (`$exp_arg_sym.name` expected)',
						arg.pos)
				}
			}
			if method.is_variadic && got_arg_typ.has_flag(.variadic) && node.args.len - 1 > i {
				c.error('when forwarding a variadic variable, it must be the final argument',
					arg.pos)
			}
			mut final_arg_sym := exp_arg_sym
			mut final_arg_typ := exp_arg_typ
			if method.is_variadic && exp_arg_sym.info is ast.Array {
				final_arg_typ = exp_arg_sym.array_info().elem_type
				final_arg_sym = c.table.get_type_symbol(final_arg_typ)
			}
			// Handle expected interface
			if final_arg_sym.kind == .interface_ {
				if c.type_implements(got_arg_typ, final_arg_typ, arg.expr.position()) {
					if !got_arg_typ.is_ptr() && !got_arg_typ.is_pointer() && !c.inside_unsafe {
						got_arg_typ_sym := c.table.get_type_symbol(got_arg_typ)
						if got_arg_typ_sym.kind != .interface_ {
							c.mark_as_referenced(mut &arg.expr, true)
						}
					}
				}
				continue
			}
			if exp_arg_typ.has_flag(.generic) {
				continue
			}
			c.check_expected_call_arg(got_arg_typ, c.unwrap_generic(exp_arg_typ), node.language,
				arg) or {
				// str method, allow type with str method if fn arg is string
				// Passing an int or a string array produces a c error here
				// Deleting this condition results in propper V error messages
				// if arg_typ_sym.kind == .string && typ_sym.has_method('str') {
				// continue
				// }
				if got_arg_typ != ast.void_type {
					c.error('$err.msg in argument ${i + 1} to `${left_sym.name}.$method_name`',
						arg.pos)
				}
			}
			param := if method.is_variadic && i >= method.params.len - 1 {
				method.params[method.params.len - 1]
			} else {
				method.params[i + 1]
			}
			param_is_mut = param_is_mut || param.is_mut
			param_share := param.typ.share()
			if param_share == .shared_t && (c.locked_names.len > 0 || c.rlocked_names.len > 0) {
				c.error('method with `shared` arguments cannot be called inside `lock`/`rlock` block',
					arg.pos)
			}
			if arg.is_mut {
				to_lock, pos := c.fail_if_immutable(arg.expr)
				if !param_is_mut {
					tok := arg.share.str()
					c.error('`$node.name` parameter `$param.name` is not `$tok`, `$tok` is not needed`',
						arg.expr.position())
				} else {
					if param_share != arg.share {
						c.error('wrong shared type', arg.expr.position())
					}
					if to_lock != '' && param_share != .shared_t {
						c.error('$to_lock is `shared` and must be `lock`ed to be passed as `mut`',
							pos)
					}
				}
			} else {
				if param_is_mut {
					tok := arg.share.str()
					c.error('`$node.name` parameter `$param.name` is `$tok`, you need to provide `$tok` e.g. `$tok arg${
						i + 1}`', arg.expr.position())
				} else {
					c.fail_if_unreadable(arg.expr, got_arg_typ, 'argument')
				}
			}
		}
		if method.is_unsafe && !c.inside_unsafe {
			c.warn('method `${left_sym.name}.$method_name` must be called from an `unsafe` block',
				node.pos)
		}
		if !c.table.cur_fn.is_deprecated && method.is_deprecated {
			c.deprecate_fnmethod('method', '${left_sym.name}.$method.name', method, node)
		}
		// TODO: typ optimize.. this node can get processed more than once
		if node.expected_arg_types.len == 0 {
			for i in 1 .. method.params.len {
				node.expected_arg_types << method.params[i].typ
			}
		}
		if is_method_from_embed {
			node.receiver_type = node.from_embed_types.last().derive(method.params[0].typ)
		} else if is_generic {
			// We need the receiver to be T in cgen.
			// TODO: cant we just set all these to the concrete type in checker? then no need in gen
			node.receiver_type = left_type.derive(method.params[0].typ).set_flag(.generic)
		} else {
			node.receiver_type = method.params[0].typ
		}
		if method.generic_names.len != node.concrete_types.len {
			// no type arguments given in call, attempt implicit instantiation
			c.infer_fn_generic_types(method, mut node)
			concrete_types = node.concrete_types
		}
		// resolve return generics struct to concrete type
		if method.generic_names.len > 0 && method.return_type.has_flag(.generic)
			&& c.table.cur_fn.generic_names.len == 0 {
			node.return_type = c.table.unwrap_generic_type(method.return_type, method.generic_names,
				concrete_types)
		} else {
			node.return_type = method.return_type
		}
		if node.concrete_types.len > 0 && method.return_type != 0
			&& c.table.cur_fn.generic_names.len == 0 {
			if typ := c.table.resolve_generic_to_concrete(method.return_type, method.generic_names,
				concrete_types)
			{
				node.return_type = typ
				return typ
			}
		}
		if node.concrete_types.len > 0 && method.generic_names.len == 0 {
			c.error('a non generic function called like a generic one', node.concrete_list_pos)
		}
		if node.concrete_types.len > method.generic_names.len {
			c.error('too many generic parameters got $node.concrete_types.len, expected $method.generic_names.len',
				node.concrete_list_pos)
		}
		if method.generic_names.len > 0 {
			return node.return_type
		}
		return method.return_type
	}
	// TODO: str methods
	if method_name == 'str' {
		if left_sym.kind == .interface_ {
			iname := left_sym.name
			c.error('interface `$iname` does not have a .str() method. Use typeof() instead',
				node.pos)
		}
		node.receiver_type = left_type
		node.return_type = ast.string_type
		if node.args.len > 0 {
			c.error('.str() method calls should have no arguments', node.pos)
		}
		c.fail_if_unreadable(node.left, left_type, 'receiver')
		return ast.string_type
	} else if method_name == 'free' {
		return ast.void_type
	}
	// call struct field fn type
	// TODO: can we use SelectorExpr for all? this dosent really belong here
	if field := c.table.find_field(left_sym, method_name) {
		field_sym := c.table.get_type_symbol(c.unwrap_generic(field.typ))
		if field_sym.kind == .function {
			node.is_method = false
			node.is_field = true
			info := field_sym.info as ast.FnType

			c.check_expected_arg_count(mut node, info.func) or { return info.func.return_type }
			node.return_type = info.func.return_type
			mut earg_types := []ast.Type{}

			for i, mut arg in node.args {
				targ := c.check_expr_opt_call(arg.expr, c.expr(arg.expr))
				arg.typ = targ
				earg_types << targ

				if i < info.func.params.len {
					exp_arg_typ := info.func.params[i].typ
					c.check_expected_call_arg(targ, c.unwrap_generic(exp_arg_typ), node.language,
						arg) or {
						if targ != ast.void_type {
							c.error('$err.msg in argument ${i + 1} to `${left_sym.name}.$method_name`',
								arg.pos)
						}
					}
				}
			}
			node.expected_arg_types = earg_types
			node.is_method = true
			return info.func.return_type
		}
	}
	if left_type != ast.void_type {
		suggestion := util.new_suggestion(method_name, left_sym.methods.map(it.name))
		c.error(suggestion.say(unknown_method_msg), node.pos)
	}
	return ast.void_type
}

fn (mut c Checker) map_builtin_method_call(mut node ast.CallExpr, left_type ast.Type, left_sym ast.TypeSymbol) ast.Type {
	method_name := node.name
	mut ret_type := ast.void_type
	match method_name {
		'clone', 'move' {
			if method_name[0] == `m` {
				c.fail_if_immutable(node.left)
			}
			if node.left.is_auto_deref_var() {
				ret_type = left_type.deref()
			} else {
				ret_type = left_type
			}
		}
		'keys' {
			info := left_sym.info as ast.Map
			typ := c.table.find_or_register_array(info.key_type)
			ret_type = ast.Type(typ)
		}
		'delete' {
			c.fail_if_immutable(node.left)
			if node.args.len != 1 {
				c.error('expected 1 argument, but got $node.args.len', node.pos)
			}
			info := left_sym.info as ast.Map
			arg_type := c.expr(node.args[0].expr)
			c.check_expected_call_arg(arg_type, info.key_type, node.language, node.args[0]) or {
				c.error('$err.msg in argument 1 to `Map.delete`', node.args[0].pos)
			}
		}
		else {}
	}
	node.receiver_type = left_type.ref()
	node.return_type = ret_type
	return node.return_type
}

fn (mut c Checker) array_builtin_method_call(mut node ast.CallExpr, left_type ast.Type, left_sym ast.TypeSymbol) ast.Type {
	method_name := node.name
	mut elem_typ := ast.void_type
	if method_name == 'slice' && !c.is_builtin_mod {
		c.error('.slice() is a private method, use `x[start..end]` instead', node.pos)
	}
	array_info := left_sym.info as ast.Array
	elem_typ = array_info.elem_type
	if method_name in ['filter', 'map', 'any', 'all'] {
		// position of `it` doesn't matter
		scope_register_it(mut node.scope, node.pos, elem_typ)
	} else if method_name == 'sort' {
		if node.left is ast.CallExpr {
			c.error('the `sort()` method can be called only on mutable receivers, but `$node.left` is a call expression',
				node.pos)
		}
		c.fail_if_immutable(node.left)
		// position of `a` and `b` doesn't matter, they're the same
		scope_register_a_b(mut node.scope, node.pos, elem_typ)

		if node.args.len > 1 {
			c.error('expected 0 or 1 argument, but got $node.args.len', node.pos)
		} else if node.args.len == 1 {
			if node.args[0].expr is ast.InfixExpr {
				if node.args[0].expr.op !in [.gt, .lt] {
					c.error('`.sort()` can only use `<` or `>` comparison', node.pos)
				}
				left_name := '${node.args[0].expr.left}'[0]
				right_name := '${node.args[0].expr.right}'[0]
				if left_name !in [`a`, `b`] || right_name !in [`a`, `b`] {
					c.error('`.sort()` can only use `a` or `b` as argument, e.g. `arr.sort(a < b)`',
						node.pos)
				} else if left_name == right_name {
					c.error('`.sort()` cannot use same argument', node.pos)
				}
				if (node.args[0].expr.left !is ast.Ident
					&& node.args[0].expr.left !is ast.SelectorExpr
					&& node.args[0].expr.left !is ast.IndexExpr)
					|| (node.args[0].expr.right !is ast.Ident
					&& node.args[0].expr.right !is ast.SelectorExpr
					&& node.args[0].expr.right !is ast.IndexExpr) {
					c.error('`.sort()` can only use ident, index or selector as argument, \ne.g. `arr.sort(a < b)`, `arr.sort(a.id < b.id)`, `arr.sort(a[0] < b[0])`',
						node.pos)
				}
			} else {
				c.error(
					'`.sort()` requires a `<` or `>` comparison as the first and only argument' +
					'\ne.g. `users.sort(a.id < b.id)`', node.pos)
			}
		} else if !(c.table.get_type_symbol(elem_typ).has_method('<')
			|| c.table.unalias_num_type(elem_typ) in [ast.int_type, ast.int_type.ref(), ast.string_type, ast.string_type.ref(), ast.i8_type, ast.i16_type, ast.i64_type, ast.byte_type, ast.rune_type, ast.u16_type, ast.u32_type, ast.u64_type, ast.f32_type, ast.f64_type, ast.char_type, ast.bool_type, ast.float_literal_type, ast.int_literal_type]) {
			c.error('custom sorting condition must be supplied for type `${c.table.type_to_str(elem_typ)}`',
				node.pos)
		}
	} else if method_name == 'wait' {
		elem_sym := c.table.get_type_symbol(elem_typ)
		if elem_sym.kind == .thread {
			if node.args.len != 0 {
				c.error('`.wait()` does not have any arguments', node.args[0].pos)
			}
			thread_ret_type := elem_sym.thread_info().return_type
			if thread_ret_type.has_flag(.optional) {
				c.error('`.wait()` cannot be called for an array when thread functions return optionals. Iterate over the arrays elements instead and handle each returned optional with `or`.',
					node.pos)
			}
			node.return_type = c.table.find_or_register_array(thread_ret_type)
		} else {
			c.error('`$left_sym.name` has no method `wait()` (only thread handles and arrays of them have)',
				node.left.position())
		}
	}
	// map/filter are supposed to have 1 arg only
	mut arg_type := left_type
	for arg in node.args {
		arg_type = c.check_expr_opt_call(arg.expr, c.expr(arg.expr))
	}
	if method_name == 'map' {
		// check fn
		c.check_map_and_filter(true, elem_typ, node)
		arg_sym := c.table.get_type_symbol(arg_type)
		ret_type := match arg_sym.info {
			ast.FnType { arg_sym.info.func.return_type }
			else { arg_type }
		}
		node.return_type = c.table.find_or_register_array(c.unwrap_generic(ret_type))
	} else if method_name == 'filter' {
		// check fn
		c.check_map_and_filter(false, elem_typ, node)
	} else if method_name in ['any', 'all'] {
		c.check_map_and_filter(false, elem_typ, node)
		node.return_type = ast.bool_type
	} else if method_name == 'clone' {
		// need to return `array_xxx` instead of `array`
		// in ['clone', 'str'] {
		node.receiver_type = left_type.ref()
		if node.left.is_auto_deref_var() {
			node.return_type = left_type.deref()
		} else {
			node.return_type = node.receiver_type.set_nr_muls(0)
		}
	} else if method_name == 'sort' {
		node.return_type = ast.void_type
	} else if method_name == 'contains' {
		// c.warn('use `value in arr` instead of `arr.contains(value)`', node.pos)
		node.return_type = ast.bool_type
	} else if method_name == 'index' {
		node.return_type = ast.int_type
	} else if method_name in ['first', 'last', 'pop'] {
		node.return_type = array_info.elem_type
		if method_name == 'pop' {
			c.fail_if_immutable(node.left)
			node.receiver_type = left_type.ref()
		} else {
			node.receiver_type = left_type
		}
	}
	return node.return_type
}

pub fn (mut c Checker) fn_call(mut node ast.CallExpr, mut continue_check &bool) ast.Type {
	fn_name := node.name
	if fn_name == 'main' {
		c.error('the `main` function cannot be called in the program', node.pos)
	}
	mut has_generic := false // foo<T>() instead of foo<int>()
	mut concrete_types := []ast.Type{}
	for concrete_type in node.concrete_types {
		if concrete_type.has_flag(.generic) {
			has_generic = true
			concrete_types << c.unwrap_generic(concrete_type)
		} else {
			concrete_types << concrete_type
		}
	}
	if !isnil(c.table.cur_fn) && c.table.cur_concrete_types.len == 0 && has_generic {
		c.error('generic fn using generic types cannot be called outside of generic fn',
			node.pos)
	}
	if concrete_types.len > 0 {
		mut no_exists := true
		if fn_name.contains('.') {
			no_exists = c.table.register_fn_concrete_types(fn_name, concrete_types)
		} else {
			no_exists = c.table.register_fn_concrete_types(c.mod + '.' + fn_name, concrete_types)
			// if the generic fn does not exist in the current fn calling module, continue
			// to look in builtin module
			if !no_exists {
				no_exists = c.table.register_fn_concrete_types(fn_name, concrete_types)
			}
		}
		if no_exists {
			c.need_recheck_generic_fns = true
		}
	}
	if fn_name == 'JS.await' {
		if node.args.len > 1 {
			c.error('JS.await expects 1 argument, a promise value (e.g `JS.await(fs.read())`',
				node.pos)
			return ast.void_type
		}

		typ := c.expr(node.args[0].expr)
		tsym := c.table.get_type_symbol(typ)

		if !tsym.name.starts_with('Promise<') {
			c.error('JS.await: first argument must be a promise, got `$tsym.name`', node.pos)
			return ast.void_type
		}
		c.table.cur_fn.has_await = true
		match tsym.info {
			ast.Struct {
				mut ret_type := tsym.info.concrete_types[0]
				ret_type = ret_type.set_flag(.optional)
				node.return_type = ret_type
				return ret_type
			}
			else {
				c.error('JS.await: Promise must be a struct type', node.pos)
				return ast.void_type
			}
		}
		panic('unreachable')
	}
	if fn_name == 'json.encode' {
	} else if fn_name == 'json.decode' && node.args.len > 0 {
		if node.args.len != 2 {
			c.error("json.decode expects 2 arguments, a type and a string (e.g `json.decode(T, '')`)",
				node.pos)
			return ast.void_type
		}
		expr := node.args[0].expr
		if expr is ast.TypeNode {
			sym := c.table.get_type_symbol(expr.typ)
			if !c.table.known_type(sym.name) {
				c.error('json.decode: unknown type `$sym.name`', node.pos)
			}
		} else {
			// if expr !is ast.TypeNode {
			typ := expr.type_name()
			c.error('json.decode: first argument needs to be a type, got `$typ`', node.pos)
			return ast.void_type
		}
		c.expected_type = ast.string_type
		node.args[1].typ = c.expr(node.args[1].expr)
		if node.args[1].typ != ast.string_type {
			c.error('json.decode: second argument needs to be a string', node.pos)
		}
		typ := expr as ast.TypeNode
		ret_type := typ.typ.set_flag(.optional)
		node.return_type = ret_type
		return ret_type
	}
	// look for function in format `mod.fn` or `fn` (builtin)
	mut func := ast.Fn{}
	mut found := false
	mut found_in_args := false
	// anon fn direct call
	if mut node.left is ast.AnonFn {
		// it was set to anon for checker errors, clear for gen
		node.name = ''
		c.expr(node.left)
		if node.left.typ != ast.Type(0) {
			anon_fn_sym := c.table.get_type_symbol(node.left.typ)
			func = (anon_fn_sym.info as ast.FnType).func
			found = true
		}
	}
	// try prefix with current module as it would have never gotten prefixed
	if !found && !fn_name.contains('.') && node.mod != 'builtin' {
		name_prefixed := '${node.mod}.$fn_name'
		if f := c.table.find_fn(name_prefixed) {
			node.name = name_prefixed
			found = true
			func = f
			c.table.fns[name_prefixed].usages++
		}
	}
	if !found && node.left is ast.IndexExpr {
		c.expr(node.left)
		expr := node.left as ast.IndexExpr
		sym := c.table.get_type_symbol(expr.left_type)
		if sym.kind == .array {
			info := sym.info as ast.Array
			elem_typ := c.table.get_type_symbol(info.elem_type)
			if elem_typ.info is ast.FnType {
				return elem_typ.info.func.return_type
			} else {
				c.error('cannot call the element of the array, it is not a function',
					node.pos)
			}
		} else if sym.kind == .map {
			info := sym.info as ast.Map
			value_typ := c.table.get_type_symbol(info.value_type)
			if value_typ.info is ast.FnType {
				return value_typ.info.func.return_type
			} else {
				c.error('cannot call the value of the map, it is not a function', node.pos)
			}
		} else if sym.kind == .array_fixed {
			info := sym.info as ast.ArrayFixed
			elem_typ := c.table.get_type_symbol(info.elem_type)
			if elem_typ.info is ast.FnType {
				return elem_typ.info.func.return_type
			} else {
				c.error('cannot call the element of the array, it is not a function',
					node.pos)
			}
		} else {
			// TODO: assert? is this possible?
		}
		found = true
		return ast.string_type
	}
	// already prefixed (mod.fn) or C/builtin/main
	if !found {
		if f := c.table.find_fn(fn_name) {
			found = true
			func = f
			c.table.fns[fn_name].usages++
		}
	}
	mut is_native_builtin := false
	if !found && c.pref.backend == .native {
		if fn_name in native.builtins {
			c.table.fns[fn_name].usages++
			found = true
			func = c.table.fns[fn_name]
			is_native_builtin = true
		}
	}
	if !found && c.pref.is_vsh {
		os_name := 'os.$fn_name'
		if f := c.table.find_fn(os_name) {
			if f.generic_names.len == node.concrete_types.len {
				c.table.fn_generic_types[os_name] = c.table.fn_generic_types['${node.mod}.$node.name']
			}
			node.name = os_name
			found = true
			func = f
			c.table.fns[os_name].usages++
		}
	}
	if is_native_builtin {
		return ast.void_type
	}
	// check for arg (var) of fn type
	if !found {
		mut typ := 0
		if obj := node.scope.find(node.name) {
			match obj {
				ast.GlobalField {
					typ = obj.typ
				}
				ast.Var {
					typ = if obj.smartcasts.len != 0 { obj.smartcasts.last() } else { obj.typ }
				}
				else {}
			}
		}

		if typ != 0 {
			generic_vts := c.table.get_final_type_symbol(typ)
			if generic_vts.kind == .function {
				info := generic_vts.info as ast.FnType
				func = info.func
				found = true
				found_in_args = true
			} else {
				vts := c.table.get_type_symbol(c.unwrap_generic(typ))
				if vts.kind == .function {
					info := vts.info as ast.FnType
					func = info.func
					found = true
					found_in_args = true
				}
			}
		}
	}
	// global fn?
	if !found {
		if obj := c.file.global_scope.find(fn_name) {
			sym := c.table.get_type_symbol(obj.typ)
			if sym.kind == .function {
				found = true
				func = (sym.info as ast.FnType).func
			}
		}
	}
	if !found {
		continue_check = false
		c.error('unknown function: $fn_name', node.pos)
		return ast.void_type
	}

	node.is_noreturn = func.is_noreturn
	node.is_ctor_new = func.is_ctor_new
	if !found_in_args {
		if node.scope.known_var(fn_name) {
			c.error('ambiguous call to: `$fn_name`, may refer to fn `$fn_name` or variable `$fn_name`',
				node.pos)
		}
	}
	if !func.is_pub && func.language == .v && func.name.len > 0 && func.mod.len > 0
		&& func.mod != c.mod {
		c.error('function `$func.name` is private', node.pos)
	}
	if !isnil(c.table.cur_fn) && !c.table.cur_fn.is_deprecated && func.is_deprecated {
		c.deprecate_fnmethod('function', func.name, func, node)
	}
	if func.is_unsafe && !c.inside_unsafe
		&& (func.language != .c || (func.name[2] in [`m`, `s`] && func.mod == 'builtin')) {
		// builtin C.m*, C.s* only - temp
		c.warn('function `$func.name` must be called from an `unsafe` block', node.pos)
	}
	node.is_keep_alive = func.is_keep_alive
	if func.mod != 'builtin' && func.language == .v && func.no_body && !c.pref.translated
		&& !func.is_unsafe {
		c.error('cannot call a function that does not have a body', node.pos)
	}
	for concrete_type in node.concrete_types {
		c.ensure_type_exists(concrete_type, node.concrete_list_pos) or {}
	}
	if func.generic_names.len > 0 && node.args.len == 0 && node.concrete_types.len == 0 {
		c.error('no argument generic function must add concrete types, e.g. foo<int>()',
			node.pos)
		return func.return_type
	}
	if func.return_type == ast.void_type && func.is_conditional && func.ctdefine_idx != -1 {
		node.should_be_skipped = c.evaluate_once_comptime_if_attribute(mut func.attrs[func.ctdefine_idx])
	}
	// dont check number of args for JS functions since arguments are not required
	if node.language != .js {
		c.check_expected_arg_count(mut node, func) or { return func.return_type }
	}
	// println / eprintln / panic can print anything
	if fn_name in ['println', 'print', 'eprintln', 'eprint', 'panic'] && node.args.len > 0 {
		c.inside_println_arg = true
		c.expected_type = ast.string_type
		node.args[0].typ = c.expr(node.args[0].expr)
		arg := node.args[0]
		c.check_expr_opt_call(arg.expr, arg.typ)
		if arg.typ.is_void() {
			c.error('`$fn_name` can not print void expressions', node.pos)
		} else if arg.typ == ast.char_type && arg.typ.nr_muls() == 0 {
			c.error('`$fn_name` cannot print type `char` directly, print its address or cast it to an integer instead',
				node.pos)
		}
		c.fail_if_unreadable(arg.expr, arg.typ, 'argument to print')
		c.inside_println_arg = false
		node.return_type = ast.void_type
		/*
		// TODO: optimize `struct T{} fn (t &T) str() string {return 'abc'} mut a := []&T{} a << &T{} println(a[0])`
		// It currently generates:
		// `println(T_str_no_ptr(*(*(T**)array_get(a, 0))));`
		// ... which works, but could be just:
		// `println(T_str(*(T**)array_get(a, 0)));`
		prexpr := node.args[0].expr
		prtyp := node.args[0].typ
		prtyp_sym := c.table.get_type_symbol(prtyp)
		prtyp_is_ptr := prtyp.is_ptr()
		prhas_str, prexpects_ptr, prnr_args := prtyp_sym.str_method_info()
		eprintln('>>> println hack typ: ${prtyp} | sym.name: ${prtyp_sym.name} | is_ptr: $prtyp_is_ptr | has_str: $prhas_str | expects_ptr: $prexpects_ptr | nr_args: $prnr_args | expr: ${prexpr.str()} ')
		*/
		return func.return_type
	}
	// `return error(err)` -> `return err`
	if fn_name == 'error' && node.args.len == 1 {
		arg := node.args[0]
		node.args[0].typ = c.expr(arg.expr)
		if node.args[0].typ == ast.error_type {
			c.warn('`error($arg)` can be shortened to just `$arg`', node.pos)
		}
	}
	// TODO: typ optimize.. this node can get processed more than once
	if node.expected_arg_types.len == 0 {
		for param in func.params {
			node.expected_arg_types << param.typ
		}
	}
	for i, mut call_arg in node.args {
		param := if func.is_variadic && i >= func.params.len - 1 {
			func.params[func.params.len - 1]
		} else {
			func.params[i]
		}
		if func.is_variadic && call_arg.expr is ast.ArrayDecompose {
			if i > func.params.len - 1 {
				c.error('too many arguments in call to `$func.name`', node.pos)
			}
		}
		c.expected_type = param.typ

		e_sym := c.table.get_type_symbol(c.expected_type)
		if call_arg.expr is ast.MapInit && e_sym.kind == .struct_ {
			c.error('cannot initialize a struct with a map', call_arg.pos)
			continue
		} else if call_arg.expr is ast.StructInit && e_sym.kind == .map {
			c.error('cannot initialize a map with a struct', call_arg.pos)
			continue
		}

		typ := c.check_expr_opt_call(call_arg.expr, c.expr(call_arg.expr))
		node.args[i].typ = typ
		typ_sym := c.table.get_type_symbol(typ)
		param_typ_sym := c.table.get_type_symbol(param.typ)
		if func.is_variadic && typ.has_flag(.variadic) && node.args.len - 1 > i {
			c.error('when forwarding a variadic variable, it must be the final argument',
				call_arg.pos)
		}
		arg_share := param.typ.share()
		if arg_share == .shared_t && (c.locked_names.len > 0 || c.rlocked_names.len > 0) {
			c.error('function with `shared` arguments cannot be called inside `lock`/`rlock` block',
				call_arg.pos)
		}
		if call_arg.is_mut {
			to_lock, pos := c.fail_if_immutable(call_arg.expr)
			if !call_arg.expr.is_lvalue() {
				c.error('cannot pass expression as `mut`', call_arg.expr.position())
			}
			if !param.is_mut {
				tok := call_arg.share.str()
				c.error('`$node.name` parameter `$param.name` is not `$tok`, `$tok` is not needed`',
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
				c.error('`$node.name` parameter `$param.name` is `$tok`, you need to provide `$tok` e.g. `$tok arg${
					i + 1}`', call_arg.expr.position())
			} else {
				c.fail_if_unreadable(call_arg.expr, typ, 'argument')
			}
		}
		mut final_param_sym := param_typ_sym
		mut final_param_typ := param.typ
		if func.is_variadic && param_typ_sym.info is ast.Array {
			final_param_typ = param_typ_sym.array_info().elem_type
			final_param_sym = c.table.get_type_symbol(final_param_typ)
		}
		// NB: Casting to voidptr is used as an escape mechanism, so:
		// 1. allow passing *explicit* voidptr (native or through cast) to functions
		// expecting voidptr or ...voidptr
		// ... but 2. disallow passing non-pointers - that is very rarely what the user wanted,
		// it can lead to codegen errors (except for 'magic' functions like `json.encode` that,
		// the compiler has special codegen support for), so it should be opt in, that is it
		// shoould require an explicit voidptr(x) cast (and probably unsafe{} ?) .
		if call_arg.typ != param.typ
			&& (param.typ == ast.voidptr_type || final_param_sym.idx == ast.voidptr_type_idx)
			&& !call_arg.typ.is_any_kind_of_pointer() && func.language == .v
			&& !call_arg.expr.is_lvalue() && func.name != 'json.encode' && !c.pref.translated {
			c.error('expression cannot be passed as `voidptr`', call_arg.expr.position())
		}
		// Handle expected interface
		if final_param_sym.kind == .interface_ {
			if c.type_implements(typ, final_param_typ, call_arg.expr.position()) {
				if !typ.is_ptr() && !typ.is_pointer() && !c.inside_unsafe
					&& typ_sym.kind != .interface_ {
					c.mark_as_referenced(mut &call_arg.expr, true)
				}
			}
			continue
		}
		c.check_expected_call_arg(typ, c.unwrap_generic(param.typ), node.language, call_arg) or {
			// str method, allow type with str method if fn arg is string
			// Passing an int or a string array produces a c error here
			// Deleting this condition results in propper V error messages
			// if arg_typ_sym.kind == .string && typ_sym.has_method('str') {
			// continue
			// }
			if typ_sym.kind == .void && param_typ_sym.kind == .string {
				continue
			}
			if param.typ.has_flag(.generic) {
				continue
			}
			if c.pref.translated {
				// Allow enums to be used as ints and vice versa in translated code
				if param.typ == ast.int_type && typ_sym.kind == .enum_ {
					continue
				}
				if typ == ast.int_type && param_typ_sym.kind == .enum_ {
					continue
				}
				// In C unsafe number casts are used all the time (e.g. `char*` where
				// `int*` is expected etc), so just allow them all.
				mut param_is_number := param.typ.is_number()
				if param.typ.is_ptr() {
					param_is_number = param.typ.deref().is_number()
				}
				mut typ_is_number := typ.is_number()
				if typ.is_ptr() {
					typ_is_number = typ.deref().is_number()
				}
				if param_is_number && typ_is_number {
					continue
				}
				// Allow voidptrs for everything
				if param.typ == ast.voidptr_type_idx || typ == ast.voidptr_type_idx {
					continue
				}
				// Allow `[32]i8` as `&i8` etc
				if (typ_sym.kind == .array_fixed && param_is_number)
					|| (param_typ_sym.kind == .array_fixed && typ_is_number) {
					continue
				}
			}
			c.error('$err.msg in argument ${i + 1} to `$fn_name`', call_arg.pos)
		}
		// Warn about automatic (de)referencing, which will be removed soon.
		if func.language != .c && !c.inside_unsafe && typ.nr_muls() != param.typ.nr_muls()
			&& !(call_arg.is_mut && param.is_mut) && !(!call_arg.is_mut && !param.is_mut)
			&& param.typ !in [ast.byteptr_type, ast.charptr_type, ast.voidptr_type] {
			// sym := c.table.get_type_symbol(typ)
			c.warn('automatic referencing/dereferencing is deprecated and will be removed soon (got: $typ.nr_muls() references, expected: $param.typ.nr_muls() references)',
				call_arg.pos)
		}
	}
	if func.generic_names.len != node.concrete_types.len {
		// no type arguments given in call, attempt implicit instantiation
		c.infer_fn_generic_types(func, mut node)
		concrete_types = node.concrete_types
	}
	if func.generic_names.len > 0 {
		for i, mut call_arg in node.args {
			param := if func.is_variadic && i >= func.params.len - 1 {
				func.params[func.params.len - 1]
			} else {
				func.params[i]
			}
			c.expected_type = param.typ
			typ := c.check_expr_opt_call(call_arg.expr, c.expr(call_arg.expr))

			if param.typ.has_flag(.generic) && func.generic_names.len == node.concrete_types.len {
				if unwrap_typ := c.table.resolve_generic_to_concrete(param.typ, func.generic_names,
					concrete_types)
				{
					utyp := c.unwrap_generic(typ)
					unwrap_sym := c.table.get_type_symbol(unwrap_typ)
					if unwrap_sym.kind == .interface_ {
						if c.type_implements(utyp, unwrap_typ, call_arg.expr.position()) {
							if !utyp.is_ptr() && !utyp.is_pointer() && !c.inside_unsafe
								&& c.table.get_type_symbol(utyp).kind != .interface_ {
								c.mark_as_referenced(mut &call_arg.expr, true)
							}
						}
						continue
					}
					c.check_expected_call_arg(utyp, unwrap_typ, node.language, call_arg) or {
						c.error('$err.msg in argument ${i + 1} to `$fn_name`', call_arg.pos)
					}
				}
			}
		}
	}
	// resolve return generics struct to concrete type
	if func.generic_names.len > 0 && func.return_type.has_flag(.generic)
		&& c.table.cur_fn.generic_names.len == 0 {
		node.return_type = c.table.unwrap_generic_type(func.return_type, func.generic_names,
			concrete_types)
	} else {
		node.return_type = func.return_type
	}
	if node.concrete_types.len > 0 && func.return_type != 0 && c.table.cur_fn.generic_names.len == 0 {
		if typ := c.table.resolve_generic_to_concrete(func.return_type, func.generic_names,
			concrete_types)
		{
			node.return_type = typ
			return typ
		}
	}
	if node.concrete_types.len > 0 && func.generic_names.len == 0 {
		c.error('a non generic function called like a generic one', node.concrete_list_pos)
	}

	if node.concrete_types.len > func.generic_names.len {
		c.error('too many generic parameters got $node.concrete_types.len, expected $func.generic_names.len',
			node.concrete_list_pos)
	}
	if func.generic_names.len > 0 {
		return node.return_type
	}
	return func.return_type
}

fn (mut c Checker) deprecate_fnmethod(kind string, name string, the_fn ast.Fn, node ast.CallExpr) {
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
	error_time := after_time.add_days(180)
	if error_time < now {
		c.error(semicolonize('$start_message has been deprecated since $after_time.ymmdd()',
			deprecation_message), node.pos)
	} else if after_time < now {
		c.warn(semicolonize('$start_message has been deprecated since $after_time.ymmdd(), it will be an error after $error_time.ymmdd()',
			deprecation_message), node.pos)
	} else if after_time == now {
		c.warn(semicolonize('$start_message has been deprecated', deprecation_message),
			node.pos)
	} else {
		c.note(semicolonize('$start_message will be deprecated after $after_time.ymmdd(), and will become an error after $error_time.ymmdd()',
			deprecation_message), node.pos)
	}
}

fn semicolonize(main string, details string) string {
	if details == '' {
		return main
	}
	return '$main; $details'
}

fn (mut c Checker) resolve_generic_interface(typ ast.Type, interface_type ast.Type, pos token.Position) ast.Type {
	utyp := c.unwrap_generic(typ)
	typ_sym := c.table.get_type_symbol(utyp)
	mut inter_sym := c.table.get_type_symbol(interface_type)

	if mut inter_sym.info is ast.Interface {
		if inter_sym.info.is_generic {
			mut inferred_types := []ast.Type{}
			generic_names := inter_sym.info.generic_types.map(c.table.get_type_name(it))
			// inferring interface generic types
			for gt_name in generic_names {
				mut inferred_type := ast.void_type
				for ifield in inter_sym.info.fields {
					if ifield.typ.has_flag(.generic) && c.table.get_type_name(ifield.typ) == gt_name {
						if field := c.table.find_field_with_embeds(typ_sym, ifield.name) {
							inferred_type = field.typ
						}
					}
				}
				for imethod in inter_sym.info.methods {
					method := typ_sym.find_method(imethod.name) or {
						typ_sym.find_method_with_generic_parent(imethod.name) or { ast.Fn{} }
					}
					if imethod.return_type.has_flag(.generic) {
						imret_sym := c.table.get_type_symbol(imethod.return_type)
						mret_sym := c.table.get_type_symbol(method.return_type)
						if imret_sym.info is ast.MultiReturn && mret_sym.info is ast.MultiReturn {
							for i, mr_typ in imret_sym.info.types {
								if mr_typ.has_flag(.generic)
									&& c.table.get_type_name(mr_typ) == gt_name {
									inferred_type = mret_sym.info.types[i]
								}
							}
						} else if c.table.get_type_name(imethod.return_type) == gt_name {
							mut ret_typ := method.return_type
							if imethod.return_type.has_flag(.optional) {
								ret_typ = ret_typ.clear_flag(.optional)
							}
							inferred_type = ret_typ
						}
					}
					for i, iparam in imethod.params {
						param := method.params[i] or { ast.Param{} }
						if iparam.typ.has_flag(.generic)
							&& c.table.get_type_name(iparam.typ) == gt_name {
							inferred_type = param.typ
						}
					}
				}
				if inferred_type == ast.void_type {
					c.error('could not infer generic type `$gt_name` in interface', pos)
					return interface_type
				}
				inferred_types << inferred_type
			}
			// add concrete types to method
			for imethod in inter_sym.info.methods {
				if inferred_types !in c.table.fn_generic_types[imethod.name] {
					c.table.fn_generic_types[imethod.name] << inferred_types
				}
			}
			inter_sym.info.concrete_types = inferred_types
			return c.table.unwrap_generic_type(interface_type, generic_names, inter_sym.info.concrete_types)
		}
	}
	return interface_type
}

fn (mut c Checker) type_implements(typ ast.Type, interface_type ast.Type, pos token.Position) bool {
	$if debug_interface_type_implements ? {
		eprintln('> type_implements typ: $typ.debug() (`${c.table.type_to_str(typ)}`) | inter_typ: $interface_type.debug() (`${c.table.type_to_str(interface_type)}`)')
	}
	utyp := c.unwrap_generic(typ)
	typ_sym := c.table.get_type_symbol(utyp)
	mut inter_sym := c.table.get_type_symbol(interface_type)

	// small hack for JS.Any type. Since `any` in regular V is getting deprecated we have our own JS.Any type for JS backend.
	if typ_sym.name == 'JS.Any' {
		return true
	}
	if mut inter_sym.info is ast.Interface {
		mut generic_type := interface_type
		mut generic_info := inter_sym.info
		if inter_sym.info.parent_type.has_flag(.generic) {
			parent_sym := c.table.get_type_symbol(inter_sym.info.parent_type)
			if parent_sym.info is ast.Interface {
				generic_type = inter_sym.info.parent_type
				generic_info = parent_sym.info
			}
		}
		mut inferred_type := interface_type
		if generic_info.is_generic {
			inferred_type = c.resolve_generic_interface(typ, generic_type, pos)
			if inferred_type == 0 {
				return false
			}
		}
		if inter_sym.info.is_generic {
			return c.type_implements(typ, inferred_type, pos)
		}
	}
	// do not check the same type more than once
	if mut inter_sym.info is ast.Interface {
		for t in inter_sym.info.types {
			if t.idx() == utyp.idx() {
				return true
			}
		}
	}
	styp := c.table.type_to_str(utyp)
	if utyp.idx() == interface_type.idx() {
		// same type -> already casted to the interface
		return true
	}
	if interface_type.idx() == ast.error_type_idx && utyp.idx() == ast.none_type_idx {
		// `none` "implements" the Error interface
		return true
	}
	if typ_sym.kind == .interface_ && inter_sym.kind == .interface_ && !styp.starts_with('JS.')
		&& !inter_sym.name.starts_with('JS.') {
		c.error('cannot implement interface `$inter_sym.name` with a different interface `$styp`',
			pos)
	}
	imethods := if inter_sym.kind == .interface_ {
		(inter_sym.info as ast.Interface).methods
	} else {
		inter_sym.methods
	}
	// voidptr is an escape hatch, it should be allowed to be passed
	if utyp != ast.voidptr_type {
		// Verify methods
		for imethod in imethods {
			method := typ_sym.find_method(imethod.name) or {
				typ_sym.find_method_with_generic_parent(imethod.name) or {
					c.error("`$styp` doesn't implement method `$imethod.name` of interface `$inter_sym.name`",
						pos)
					continue
				}
			}
			msg := c.table.is_same_method(imethod, method)
			if msg.len > 0 {
				sig := c.table.fn_signature(imethod, skip_receiver: false)
				typ_sig := c.table.fn_signature(method, skip_receiver: false)
				c.add_error_detail('$inter_sym.name has `$sig`')
				c.add_error_detail('         $typ_sym.name has `$typ_sig`')
				c.error('`$styp` incorrectly implements method `$imethod.name` of interface `$inter_sym.name`: $msg',
					pos)
				return false
			}
		}
	}
	// Verify fields
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
			// voidptr is an escape hatch, it should be allowed to be passed
			if utyp != ast.voidptr_type {
				c.error("`$styp` doesn't implement field `$ifield.name` of interface `$inter_sym.name`",
					pos)
			}
		}
		inter_sym.info.types << utyp
	}
	return true
}

// return the actual type of the expression, once the optional is handled
pub fn (mut c Checker) check_expr_opt_call(expr ast.Expr, ret_type ast.Type) ast.Type {
	if expr is ast.CallExpr {
		if expr.return_type.has_flag(.optional) {
			if expr.or_block.kind == .absent {
				if c.inside_defer {
					c.error('${expr.name}() returns an option, so it should have an `or {}` block at the end',
						expr.pos)
				} else {
					c.error('${expr.name}() returns an option, so it should have either an `or {}` block, or `?` at the end',
						expr.pos)
				}
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

pub fn (mut c Checker) check_or_expr(node ast.OrExpr, ret_type ast.Type, expr_return_type ast.Type) {
	if node.kind == .propagate {
		if !c.table.cur_fn.return_type.has_flag(.optional) && c.table.cur_fn.name != 'main.main'
			&& !c.inside_const {
			c.error('to propagate the optional call, `$c.table.cur_fn.name` must return an optional',
				node.pos)
		}
		return
	}
	stmts_len := node.stmts.len
	if stmts_len == 0 {
		if ret_type != ast.void_type {
			// x := f() or {}
			c.error('assignment requires a non empty `or {}` block', node.pos)
		}
		// allow `f() or {}`
		return
	}
	last_stmt := node.stmts[stmts_len - 1]
	if ret_type != ast.void_type {
		match last_stmt {
			ast.ExprStmt {
				c.expected_type = ret_type
				c.expected_or_type = ret_type.clear_flag(.optional)
				last_stmt_typ := c.expr(last_stmt.expr)
				c.expected_or_type = ast.void_type
				type_fits := c.check_types(last_stmt_typ, ret_type)
					&& last_stmt_typ.nr_muls() == ret_type.nr_muls()
				is_noreturn := is_noreturn_callexpr(last_stmt.expr)
				if type_fits || is_noreturn {
					return
				}
				expected_type_name := c.table.type_to_str(ret_type.clear_flag(.optional))
				if last_stmt.typ == ast.void_type {
					c.error('`or` block must provide a default value of type `$expected_type_name`, or return/continue/break or call a [noreturn] function like panic(err) or exit(1)',
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
					node.pos)
				return
			}
		}
	} else {
		match last_stmt {
			ast.ExprStmt {
				if last_stmt.typ == ast.void_type {
					return
				}
				if is_noreturn_callexpr(last_stmt.expr) {
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

fn is_noreturn_callexpr(expr ast.Expr) bool {
	if expr is ast.CallExpr {
		return expr.is_noreturn
	}
	return false
}

pub fn (mut c Checker) selector_expr(mut node ast.SelectorExpr) ast.Type {
	prevent_sum_type_unwrapping_once := c.prevent_sum_type_unwrapping_once
	c.prevent_sum_type_unwrapping_once = false

	using_new_err_struct_save := c.using_new_err_struct
	// TODO remove; this avoids a breaking change in syntax
	if '$node.expr' == 'err' {
		c.using_new_err_struct = true
	}

	// T.name, typeof(expr).name
	mut name_type := 0
	match mut node.expr {
		ast.Ident {
			name := node.expr.name
			valid_generic := util.is_generic_type_name(name) && name in c.table.cur_fn.generic_names
			if valid_generic {
				name_type = ast.Type(c.table.find_type_idx(name)).set_flag(.generic)
			}
		}
		// Note: in future typeof() should be a type known at compile-time
		// sum types should not be handled dynamically
		ast.TypeOf {
			name_type = c.expr(node.expr.expr)
		}
		else {}
	}
	if name_type > 0 {
		node.name_type = name_type
		match node.gkind_field {
			.name {
				return ast.string_type
			}
			.typ {
				return ast.int_type
			}
			else {
				if node.field_name == 'name' {
					return ast.string_type
				} else if node.field_name == 'idx' {
					return ast.int_type
				}
				c.error('invalid field `.$node.field_name` for type `$node.expr`', node.pos)
				return ast.string_type
			}
		}
	}

	old_selector_expr := c.inside_selector_expr
	c.inside_selector_expr = true
	mut typ := c.expr(node.expr)
	if node.expr.is_auto_deref_var() {
		if mut node.expr is ast.Ident {
			if mut node.expr.obj is ast.Var {
				typ = node.expr.obj.typ
			}
		}
	}
	c.inside_selector_expr = old_selector_expr
	c.using_new_err_struct = using_new_err_struct_save
	if typ == ast.void_type_idx {
		c.error('`void` type has no fields', node.pos)
		return ast.void_type
	}
	node.expr_type = typ
	if node.expr_type.has_flag(.optional) && !(node.expr is ast.Ident
		&& (node.expr as ast.Ident).kind == .constant) {
		c.error('cannot access fields of an optional, handle the error with `or {...}` or propagate it with `?`',
			node.pos)
	}
	field_name := node.field_name
	sym := c.table.get_type_symbol(typ)
	if (typ.has_flag(.variadic) || sym.kind == .array_fixed) && field_name == 'len' {
		node.typ = ast.int_type
		return ast.int_type
	}
	if sym.kind == .chan {
		if field_name == 'closed' {
			node.typ = ast.bool_type
			return ast.bool_type
		} else if field_name in ['len', 'cap'] {
			node.typ = ast.u32_type
			return ast.u32_type
		}
	}
	mut unknown_field_msg := 'type `$sym.name` has no field named `$field_name`'
	mut has_field := false
	mut field := ast.StructField{}
	if field_name.len > 0 && field_name[0].is_capital() && sym.info is ast.Struct
		&& sym.language == .v {
		// x.Foo.y => access the embedded struct
		for embed in sym.info.embeds {
			embed_sym := c.table.get_type_symbol(embed)
			if embed_sym.embed_name() == field_name {
				node.typ = embed
				return embed
			}
		}
	} else {
		if f := c.table.find_field(sym, field_name) {
			has_field = true
			field = f
		} else {
			// look for embedded field
			has_field = true
			mut embed_types := []ast.Type{}
			field, embed_types = c.table.find_field_from_embeds(sym, field_name) or {
				if err.msg != '' {
					c.error(err.msg, node.pos)
				}
				has_field = false
				ast.StructField{}, []ast.Type{}
			}
			node.from_embed_types = embed_types
			if sym.kind in [.aggregate, .sum_type] {
				unknown_field_msg = err.msg
			}
		}
		if !c.inside_unsafe {
			if sym.info is ast.Struct {
				if sym.info.is_union && node.next_token !in token.assign_tokens {
					c.warn('reading a union field (or its address) requires `unsafe`',
						node.pos)
				}
			}
		}
		if typ.has_flag(.generic) && !has_field {
			gs := c.table.get_type_symbol(c.unwrap_generic(typ))
			if f := c.table.find_field(gs, field_name) {
				has_field = true
				field = f
			} else {
				// look for embedded field
				has_field = true
				mut embed_types := []ast.Type{}
				field, embed_types = c.table.find_field_from_embeds(gs, field_name) or {
					if err.msg != '' {
						c.error(err.msg, node.pos)
					}
					has_field = false
					ast.StructField{}, []ast.Type{}
				}
				node.from_embed_types = embed_types
			}
		}
	}
	if has_field {
		if sym.mod != c.mod && !field.is_pub && sym.language != .c {
			unwrapped_sym := c.table.get_type_symbol(c.unwrap_generic(typ))
			c.error('field `${unwrapped_sym.name}.$field_name` is not public', node.pos)
		}
		field_sym := c.table.get_type_symbol(field.typ)
		if field_sym.kind in [.sum_type, .interface_] {
			if !prevent_sum_type_unwrapping_once {
				if scope_field := node.scope.find_struct_field(node.expr.str(), typ, field_name) {
					return scope_field.smartcasts.last()
				}
			}
		}
		node.typ = field.typ
		return field.typ
	}
	if sym.kind !in [.struct_, .aggregate, .interface_, .sum_type] {
		if sym.kind != .placeholder {
			unwrapped_sym := c.table.get_type_symbol(c.unwrap_generic(typ))
			c.error('`$unwrapped_sym.name` has no property `$node.field_name`', node.pos)
		}
	} else {
		if sym.info is ast.Struct {
			suggestion := util.new_suggestion(field_name, sym.info.fields.map(it.name))
			c.error(suggestion.say(unknown_field_msg), node.pos)
		}
		c.error(unknown_field_msg, node.pos)
	}
	return ast.void_type
}

// TODO: non deferred
pub fn (mut c Checker) return_stmt(mut node ast.Return) {
	c.expected_type = c.table.cur_fn.return_type
	mut expected_type := c.unwrap_generic(c.expected_type)
	expected_type_sym := c.table.get_type_symbol(expected_type)
	if node.exprs.len > 0 && c.table.cur_fn.return_type == ast.void_type {
		c.error('unexpected argument, current function does not return anything', node.exprs[0].position())
		return
	} else if node.exprs.len == 0 && !(c.expected_type == ast.void_type
		|| expected_type_sym.kind == .void) {
		stype := c.table.type_to_str(expected_type)
		arg := if expected_type_sym.kind == .multi_return { 'arguments' } else { 'argument' }
		c.error('expected `$stype` $arg', node.pos)
		return
	}
	if node.exprs.len == 0 {
		return
	}
	exp_is_optional := expected_type.has_flag(.optional)
	mut expected_types := [expected_type]
	if expected_type_sym.info is ast.MultiReturn {
		expected_types = expected_type_sym.info.types
		if c.table.cur_concrete_types.len > 0 {
			expected_types = expected_types.map(c.unwrap_generic(it))
		}
	}
	mut got_types := []ast.Type{}
	for expr in node.exprs {
		typ := c.expr(expr)
		if typ == ast.void_type {
			c.error('`$expr` used as value', node.pos)
		}
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
	node.types = got_types
	$if debug_manualfree ? {
		cfn := c.table.cur_fn
		if cfn.is_manualfree {
			pnames := cfn.params.map(it.name)
			for expr in node.exprs {
				if expr is ast.Ident {
					if expr.name in pnames {
						c.note('returning a parameter in a fn marked with `[manualfree]` can cause double freeing in the caller',
							node.pos)
					}
				}
			}
		}
	}
	// allow `none` & `error` return types for function that returns optional
	option_type_idx := c.table.type_idxs['Option']
	got_types_0_idx := got_types[0].idx()
	if exp_is_optional
		&& got_types_0_idx in [ast.none_type_idx, ast.error_type_idx, option_type_idx] {
		if got_types_0_idx == ast.none_type_idx && expected_type == ast.ovoid_type {
			c.error('returning `none` in functions, that have a `?` result type is not allowed anymore, either `return error(message)` or just `return` instead',
				node.pos)
		}
		return
	}
	if expected_types.len > 0 && expected_types.len != got_types.len {
		arg := if expected_types.len == 1 { 'argument' } else { 'arguments' }
		c.error('expected $expected_types.len $arg, but got $got_types.len', node.pos)
		return
	}
	for i, exp_type in expected_types {
		got_typ := c.unwrap_generic(got_types[i])
		if got_typ.has_flag(.optional) && (!exp_type.has_flag(.optional)
			|| c.table.type_to_str(got_typ) != c.table.type_to_str(exp_type)) {
			pos := node.exprs[i].position()
			c.error('cannot use `${c.table.type_to_str(got_typ)}` as type `${c.table.type_to_str(exp_type)}` in return argument',
				pos)
		}
		if !c.check_types(got_typ, exp_type) {
			got_typ_sym := c.table.get_type_symbol(got_typ)
			mut exp_typ_sym := c.table.get_type_symbol(exp_type)
			if exp_typ_sym.kind == .interface_ {
				if c.type_implements(got_typ, exp_type, node.pos) {
					if !got_typ.is_ptr() && !got_typ.is_pointer() && got_typ_sym.kind != .interface_
						&& !c.inside_unsafe {
						c.mark_as_referenced(mut &node.exprs[i], true)
					}
				}
				continue
			}
			pos := node.exprs[i].position()
			c.error('cannot use `$got_typ_sym.name` as type `${c.table.type_to_str(exp_type)}` in return argument',
				pos)
		}
		if (got_typ.is_ptr() || got_typ.is_pointer())
			&& (!exp_type.is_ptr() && !exp_type.is_pointer()) {
			pos := node.exprs[i].position()
			if node.exprs[i].is_auto_deref_var() {
				continue
			}
			c.error('fn `$c.table.cur_fn.name` expects you to return a non reference type `${c.table.type_to_str(exp_type)}`, but you are returning `${c.table.type_to_str(got_typ)}` instead',
				pos)
		}
		if (exp_type.is_ptr() || exp_type.is_pointer())
			&& (!got_typ.is_ptr() && !got_typ.is_pointer()) && got_typ != ast.int_literal_type {
			pos := node.exprs[i].position()
			if node.exprs[i].is_auto_deref_var() {
				continue
			}
			c.error('fn `$c.table.cur_fn.name` expects you to return a reference type `${c.table.type_to_str(exp_type)}`, but you are returning `${c.table.type_to_str(got_typ)}` instead',
				pos)
		}
		if exp_type.is_ptr() && got_typ.is_ptr() {
			mut r_expr := &node.exprs[i]
			if mut r_expr is ast.Ident {
				if mut r_expr.obj is ast.Var {
					mut obj := unsafe { &r_expr.obj }
					if c.fn_scope != voidptr(0) {
						obj = c.fn_scope.find_var(r_expr.obj.name) or { obj }
					}
					if obj.is_stack_obj && !c.inside_unsafe {
						type_sym := c.table.get_type_symbol(obj.typ.set_nr_muls(0))
						if !type_sym.is_heap() && !c.pref.translated {
							suggestion := if type_sym.kind == .struct_ {
								'declaring `$type_sym.name` as `[heap]`'
							} else {
								'wrapping the `$type_sym.name` object in a `struct` declared as `[heap]`'
							}
							c.error('`$r_expr.name` cannot be returned outside `unsafe` blocks as it might refer to an object stored on stack. Consider ${suggestion}.',
								r_expr.pos)
						}
					}
				}
			}
		}
	}
	if exp_is_optional && node.exprs.len > 0 {
		expr0 := node.exprs[0]
		if expr0 is ast.CallExpr {
			if expr0.or_block.kind == .propagate {
				c.error('`?` is not needed, use `return ${expr0.name}()`', expr0.pos)
			}
		}
	}
}

pub fn (mut c Checker) const_decl(mut node ast.ConstDecl) {
	if node.fields.len == 0 {
		c.warn('const block must have at least 1 declaration', node.pos)
	}
	for field in node.fields {
		// TODO Check const name once the syntax is decided
		if field.name in c.const_names {
			name_pos := token.Position{
				...field.pos
				len: util.no_cur_mod(field.name, c.mod).len
			}
			c.error('duplicate const `$field.name`', name_pos)
		}
		c.const_names << field.name
	}
	for i, mut field in node.fields {
		c.const_decl = field.name
		c.const_deps << field.name
		mut typ := c.check_expr_opt_call(field.expr, c.expr(field.expr))
		if ct_value := c.eval_comptime_const_expr(field.expr, 0) {
			field.comptime_expr_value = ct_value
			if ct_value is u64 {
				typ = ast.u64_type
			}
		}
		node.fields[i].typ = c.table.mktyp(typ)
		c.const_deps = []
	}
}

pub fn (mut c Checker) enum_decl(mut node ast.EnumDecl) {
	c.check_valid_pascal_case(node.name, 'enum name', node.pos)
	mut seen := []i64{cap: node.fields.len}
	if node.fields.len == 0 {
		c.error('enum cannot be empty', node.pos)
	}
	/*
	if node.is_pub && c.mod == 'builtin' {
		c.error('`builtin` module cannot have enums', node.pos)
	}
	*/
	for i, mut field in node.fields {
		if !c.pref.experimental && util.contains_capital(field.name) {
			// TODO C2V uses hundreds of enums with capitals, remove -experimental check once it's handled
			c.error('field name `$field.name` cannot contain uppercase letters, use snake_case instead',
				field.pos)
		}
		for j in 0 .. i {
			if field.name == node.fields[j].name {
				c.error('field name `$field.name` duplicate', field.pos)
			}
		}
		if field.has_expr {
			match mut field.expr {
				ast.IntegerLiteral {
					val := field.expr.val.i64()
					if val < checker.int_min || val > checker.int_max {
						c.error('enum value `$val` overflows int', field.expr.pos)
					} else if !c.pref.translated && !node.is_multi_allowed && i64(val) in seen {
						c.error('enum value `$val` already exists', field.expr.pos)
					}
					seen << i64(val)
				}
				ast.PrefixExpr {}
				ast.InfixExpr {
					// Handle `enum Foo { x = 1 + 2 }`
					c.infix_expr(mut field.expr)
				}
				// ast.ParExpr {} // TODO allow `.x = (1+2)`
				else {
					if field.expr is ast.Ident {
						x := field.expr as ast.Ident
						// TODO sum type bug, remove temp var
						// if field.expr.language == .c {
						if x.language == .c {
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
				} else if !c.pref.translated && !node.is_multi_allowed && last + 1 in seen {
					c.error('enum value `${last + 1}` already exists', field.pos)
				}
				seen << last + 1
			} else {
				seen << 0
			}
		}
	}
}

pub fn (mut c Checker) assign_stmt(mut node ast.AssignStmt) {
	c.expected_type = ast.none_type // TODO a hack to make `x := if ... work`
	defer {
		c.expected_type = ast.void_type
	}
	right_first := node.right[0]
	node.left_types = []
	mut right_len := node.right.len
	mut right_type0 := ast.void_type
	for i, right in node.right {
		if right in [ast.CallExpr, ast.IfExpr, ast.LockExpr, ast.MatchExpr] {
			right_type := c.expr(right)
			if i == 0 {
				right_type0 = right_type
				node.right_types = [
					c.check_expr_opt_call(right, right_type0),
				]
			}
			right_type_sym := c.table.get_type_symbol(right_type)
			if right_type_sym.kind == .multi_return {
				if node.right.len > 1 {
					c.error('cannot use multi-value $right_type_sym.name in single-value context',
						right.position())
				}
				node.right_types = right_type_sym.mr_info().types
				right_len = node.right_types.len
			} else if right_type == ast.void_type {
				right_len = 0
			}
		}
		if right is ast.InfixExpr {
			if right.op == .arrow {
				c.error('cannot use `<-` on the right-hand side of an assignment, as it does not return any values',
					right.pos)
			}
		}
		if right is ast.Ident {
			if right.is_mut {
				c.error('unexpected `mut` on right-hand side of assignment', right.mut_pos)
			}
		}
	}
	if node.left.len != right_len {
		if right_first is ast.CallExpr {
			if node.left_types.len > 0 && node.left_types[0] == ast.void_type {
				// If it's a void type, it's an unknown variable, already had an error earlier.
				return
			}
			c.error('assignment mismatch: $node.left.len variable(s) but `${right_first.name}()` returns $right_len value(s)',
				node.pos)
		} else {
			c.error('assignment mismatch: $node.left.len variable(s) $right_len value(s)',
				node.pos)
		}
		return
	}

	is_decl := node.op == .decl_assign
	for i, left in node.left {
		if left is ast.CallExpr {
			// ban `foo() = 10`
			c.error('cannot call function `${left.name}()` on the left side of an assignment',
				left.pos)
		} else if left is ast.PrefixExpr {
			// ban `*foo() = 10`
			if left.right is ast.CallExpr && left.op == .mul {
				c.error('cannot dereference a function call on the left side of an assignment, use a temporary variable',
					left.pos)
			}
		} else if left is ast.IndexExpr {
			if left.index is ast.RangeExpr {
				c.error('cannot reassign using range expression on the left side of an assignment',
					left.pos)
			}
		}
		is_blank_ident := left.is_blank_ident()
		mut left_type := ast.void_type
		if !is_decl && !is_blank_ident {
			if left in [ast.Ident, ast.SelectorExpr] {
				c.prevent_sum_type_unwrapping_once = true
			}
			left_type = c.expr(left)
			c.expected_type = c.unwrap_generic(left_type)
			// `map = {}`
			if left_type != 0 {
				sym := c.table.get_type_symbol(left_type)
				if sym.kind == .map {
					if node.right.len <= i {
						// `map_1, map_2, map_3 = f()`, where f returns (map[int]int, map[int]int, map[int]int)
						// i.e. 3 left parts of the assignment, but just 1 right part
					} else {
						if node.right[i] is ast.StructInit {
							c.warn('assigning a struct literal to a map is deprecated - use `map{}` instead',
								node.right[i].position())
							node.right[i] = ast.MapInit{}
						}
					}
				}
			}
		}
		if node.right_types.len < node.left.len { // first type or multi return types added above
			old_inside_ref_lit := c.inside_ref_lit
			if left is ast.Ident {
				if left.info is ast.IdentVar {
					c.inside_ref_lit = c.inside_ref_lit || left.info.share == .shared_t
				}
			}
			c.inside_decl_rhs = is_decl
			right_type := c.expr(node.right[i])
			c.inside_decl_rhs = false
			c.inside_ref_lit = old_inside_ref_lit
			if node.right_types.len == i {
				node.right_types << c.check_expr_opt_call(node.right[i], right_type)
			}
		}
		right := if i < node.right.len { node.right[i] } else { node.right[0] }
		mut right_type := node.right_types[i]
		if right is ast.Ident {
			right_sym := c.table.get_type_symbol(right_type)
			if right_sym.info is ast.Struct {
				if right_sym.info.generic_types.len > 0 {
					if obj := right.scope.find(right.name) {
						right_type = obj.typ
					}
				}
			}
		}
		if is_decl {
			// check generic struct init and return unwrap generic struct type
			if right is ast.StructInit {
				if right.typ.has_flag(.generic) {
					c.expr(right)
					right_type = right.typ
				}
			} else if right is ast.PrefixExpr {
				if right.op == .amp && right.right is ast.StructInit {
					right_type = c.expr(right)
				}
			}
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
		if right_type.is_ptr() && left_type.is_ptr() {
			if mut right is ast.Ident {
				if mut right.obj is ast.Var {
					mut obj := unsafe { &right.obj }
					if c.fn_scope != voidptr(0) {
						obj = c.fn_scope.find_var(right.obj.name) or { obj }
					}
					if obj.is_stack_obj && !c.inside_unsafe {
						type_sym := c.table.get_type_symbol(obj.typ.set_nr_muls(0))
						if !type_sym.is_heap() && !c.pref.translated {
							suggestion := if type_sym.kind == .struct_ {
								'declaring `$type_sym.name` as `[heap]`'
							} else {
								'wrapping the `$type_sym.name` object in a `struct` declared as `[heap]`'
							}
							c.error('`$right.name` cannot be assigned outside `unsafe` blocks as it might refer to an object stored on stack. Consider ${suggestion}.',
								right.pos)
						}
					}
				}
			}
		}
		// Do not allow `a := 0; b := 0; a = &b`
		if !is_decl && left is ast.Ident && !is_blank_ident && !left_type.is_real_pointer()
			&& right_type.is_real_pointer() {
			left_sym := c.table.get_type_symbol(left_type)
			if left_sym.kind != .function {
				c.warn(
					'cannot assign a reference to a value (this will be an error soon) left=${c.table.type_str(left_type)} $left_type.is_ptr() ' +
					'right=${c.table.type_str(right_type)} $right_type.is_real_pointer() ptr=$right_type.is_ptr()',
					node.pos)
			}
		}
		node.left_types << left_type
		match mut left {
			ast.Ident {
				if left.kind == .blank_ident {
					left_type = right_type
					node.left_types[i] = right_type
					if node.op !in [.assign, .decl_assign] {
						c.error('cannot modify blank `_` identifier', left.pos)
					}
				} else if left.info !is ast.IdentVar {
					c.error('cannot assign to $left.kind `$left.name`', left.pos)
				} else {
					if is_decl {
						c.check_valid_snake_case(left.name, 'variable name', left.pos)
						if left.name in checker.reserved_type_names {
							c.error('invalid use of reserved type `$left.name` as a variable name',
								left.pos)
						}
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
					node.left_types[i] = left_type
					ident_var_info.typ = left_type
					left.info = ident_var_info
					if left_type != 0 {
						match mut left.obj {
							ast.Var {
								left.obj.typ = left_type
								if left.obj.is_auto_deref {
									left.obj.is_used = true
								}
								if !left_type.is_ptr() {
									if c.table.get_type_symbol(left_type).is_heap() {
										left.obj.is_auto_heap = true
									}
								}
								if left_type in ast.unsigned_integer_type_idxs {
									if right is ast.IntegerLiteral {
										if right.val[0] == `-` {
											c.error('Cannot assign negative value to unsigned integer type',
												right.pos)
										}
									}
								}
							}
							ast.GlobalField {
								left.obj.typ = left_type
							}
							else {}
						}
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
				if left.op == .mul {
					if !c.inside_unsafe && !c.pref.translated {
						c.error('modifying variables via dereferencing can only be done in `unsafe` blocks',
							node.pos)
					} else {
						// mark `p` in `*p = val` as used:
						match mut left.right {
							ast.Ident {
								match mut left.right.obj {
									ast.Var {
										left.right.obj.is_used = true
									}
									else {}
								}
							}
							else {}
						}
					}
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
		if c.pref.translated {
			// TODO fix this in C2V instead, for example cast enums to int before using `|` on them.
			// TODO replace all c.pref.translated checks with `$if !translated` for performance
			continue
		}
		if left_type_unwrapped == 0 {
			continue
		}
		left_sym := c.table.get_type_symbol(left_type_unwrapped)
		right_sym := c.table.get_type_symbol(right_type_unwrapped)
		if left_sym.kind == .array && !c.inside_unsafe && node.op in [.assign, .decl_assign]
			&& right_sym.kind == .array && (left is ast.Ident && !left.is_blank_ident())
			&& right is ast.Ident {
			// Do not allow `a = b`, only `a = b.clone()`
			c.error('use `array2 $node.op.str() array1.clone()` instead of `array2 $node.op.str() array1` (or use `unsafe`)',
				node.pos)
		}
		if left_sym.kind == .array_fixed && !c.inside_unsafe && node.op in [.assign, .decl_assign]
			&& right_sym.kind == .array_fixed && (left is ast.Ident && !left.is_blank_ident())
			&& right is ast.Ident {
			if right_sym.info is ast.ArrayFixed {
				if right_sym.info.elem_type.is_ptr() {
					c.error('assignment from one fixed array to another with a pointer element type is prohibited outside of `unsafe`',
						node.pos)
				}
			}
		}
		if left_sym.kind == .map && node.op in [.assign, .decl_assign] && right_sym.kind == .map
			&& ((right is ast.Ident && right.is_auto_deref_var())
			|| !right_type.is_ptr()) && !left.is_blank_ident() && right.is_lvalue() {
			// Do not allow `a = b`
			c.error('cannot copy map: call `move` or `clone` method (or use a reference)',
				right.position())
		}
		left_is_ptr := left_type.is_ptr() || left_sym.is_pointer()
		if left_is_ptr && !left.is_auto_deref_var() {
			if !c.inside_unsafe && node.op !in [.assign, .decl_assign] {
				// ptr op=
				c.warn('pointer arithmetic is only allowed in `unsafe` blocks', node.pos)
			}
			right_is_ptr := right_type.is_ptr() || right_sym.is_pointer()
			if !right_is_ptr && node.op == .assign && right_type_unwrapped.is_number() {
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
				c.error('mismatched types `$left_name` and `$right_name`', node.pos)
			}
		}
		// Single side check
		match node.op {
			.assign {} // No need to do single side check for =. But here put it first for speed.
			.plus_assign, .minus_assign {
				if left_type == ast.string_type {
					if node.op != .plus_assign {
						c.error('operator `$node.op` not defined on left operand type `$left_sym.name`',
							left.position())
					}
					if right_type != ast.string_type {
						c.error('invalid right operand: $left_sym.name $node.op $right_sym.name',
							right.position())
					}
				} else if !left_sym.is_number()
					&& left_sym.kind !in [.byteptr, .charptr, .struct_, .alias] {
					c.error('operator `$node.op` not defined on left operand type `$left_sym.name`',
						left.position())
				} else if !right_sym.is_number()
					&& left_sym.kind !in [.byteptr, .charptr, .struct_, .alias] {
					c.error('invalid right operand: $left_sym.name $node.op $right_sym.name',
						right.position())
				}
			}
			.mult_assign, .div_assign {
				if !left_sym.is_number()
					&& !c.table.get_final_type_symbol(left_type_unwrapped).is_int()
					&& left_sym.kind !in [.struct_, .alias] {
					c.error('operator $node.op.str() not defined on left operand type `$left_sym.name`',
						left.position())
				} else if !right_sym.is_number()
					&& !c.table.get_final_type_symbol(left_type_unwrapped).is_int()
					&& left_sym.kind !in [.struct_, .alias] {
					c.error('operator $node.op.str() not defined on right operand type `$right_sym.name`',
						right.position())
				}
			}
			.and_assign, .or_assign, .xor_assign, .mod_assign, .left_shift_assign,
			.right_shift_assign {
				if !left_sym.is_int()
					&& !c.table.get_final_type_symbol(left_type_unwrapped).is_int() {
					c.error('operator $node.op.str() not defined on left operand type `$left_sym.name`',
						left.position())
				} else if !right_sym.is_int()
					&& !c.table.get_final_type_symbol(right_type_unwrapped).is_int() {
					c.error('operator $node.op.str() not defined on right operand type `$right_sym.name`',
						right.position())
				}
			}
			.unsigned_right_shift_assign {
				if node.left.len != 1 || node.right.len != 1 {
					c.error('unsupported operation: unable to lower expression for unsigned shift assignment.',
						node.pos)
				}

				modified_left_type := if !left_type.is_int() {
					c.error('invalid operation: shift on type `${c.table.get_type_symbol(left_type).name}`',
						node.pos)
					ast.void_type_idx
				} else if left_type.is_int_literal() {
					// int literal => i64
					ast.u32_type_idx
				} else if left_type.is_unsigned() {
					left_type
				} else {
					// signed types' idx adds with 5 will get correct relative unsigned type
					// i8 		=> byte
					// i16 		=> u16
					// int  	=> u32
					// i64  	=> u64
					// isize	=> usize
					// i128 	=> u128 NOT IMPLEMENTED YET
					left_type.idx() + ast.u32_type_idx - ast.int_type_idx
				}

				node = ast.AssignStmt{
					op: .assign
					pos: node.pos
					comments: node.comments
					end_comments: node.end_comments
					left: node.left
					right: [
						ast.Expr(ast.InfixExpr{
							left: ast.CastExpr{
								expr: node.left[0]
								typ: modified_left_type
								typname: c.table.type_str(modified_left_type)
								pos: node.pos
							}
							op: .right_shift
							right: node.right[0]
							left_type: modified_left_type
							right_type: right_type
							pos: node.pos
						}),
					]
					left_types: node.left_types
					right_types: node.right_types
					is_static: node.is_static
					is_simple: node.is_simple
					has_cross_var: node.has_cross_var
				}
			}
			else {}
		}
		if node.op in [.plus_assign, .minus_assign, .mod_assign, .mult_assign, .div_assign]
			&& ((left_sym.kind == .struct_ && right_sym.kind == .struct_)
			|| left_sym.kind == .alias) {
			left_name := c.table.type_to_str(left_type_unwrapped)
			right_name := c.table.type_to_str(right_type_unwrapped)
			parent_sym := c.table.get_final_type_symbol(left_type_unwrapped)
			if left_sym.kind == .alias && right_sym.kind != .alias {
				c.error('mismatched types `$left_name` and `$right_name`', node.pos)
			}
			extracted_op := match node.op {
				.plus_assign { '+' }
				.minus_assign { '-' }
				.div_assign { '/' }
				.mod_assign { '%' }
				.mult_assign { '*' }
				else { 'unknown op' }
			}
			if left_sym.kind == .struct_ && (left_sym.info as ast.Struct).generic_types.len > 0 {
				continue
			}
			if method := left_sym.find_method(extracted_op) {
				if method.return_type != left_type_unwrapped {
					c.error('operator `$extracted_op` must return `$left_name` to be used as an assignment operator',
						node.pos)
				}
			} else {
				if parent_sym.is_primitive() {
					c.error('cannot use operator methods on type alias for `$parent_sym.name`',
						node.pos)
				}
				if left_name == right_name {
					c.error('undefined operation `$left_name` $extracted_op `$right_name`',
						node.pos)
				} else {
					c.error('mismatched types `$left_name` and `$right_name`', node.pos)
				}
			}
		}
		if !is_blank_ident && !left.is_auto_deref_var() && !right.is_auto_deref_var()
			&& right_sym.kind != .placeholder && left_sym.kind != .interface_
			&& !right_type.has_flag(.generic) && !left_type.has_flag(.generic) {
			// Dual sides check (compatibility check)
			c.check_expected(right_type_unwrapped, left_type_unwrapped) or {
				// allow for ptr += 2
				if left_type_unwrapped.is_ptr() && right_type_unwrapped.is_int()
					&& node.op in [.plus_assign, .minus_assign] {
					if !c.inside_unsafe {
						c.warn('pointer arithmetic is only allowed in `unsafe` blocks',
							node.pos)
					}
				} else {
					c.error('cannot assign to `$left`: $err.msg', right.position())
				}
			}
		}
		if left_sym.kind == .interface_ {
			if c.type_implements(right_type, left_type, right.position()) {
				if !right_type.is_ptr() && !right_type.is_pointer() && right_sym.kind != .interface_
					&& !c.inside_unsafe {
					c.mark_as_referenced(mut &node.right[i], true)
				}
			}
		}
	}
	// this needs to run after the assign stmt left exprs have been run through checker
	// so that ident.obj is set
	// Check `x := &y` and `mut x := <-ch`
	if right_first is ast.PrefixExpr {
		right_node := right_first
		left_first := node.left[0]
		if left_first is ast.Ident {
			assigned_var := left_first
			mut is_shared := false
			if left_first.info is ast.IdentVar {
				is_shared = left_first.info.share == .shared_t
			}
			old_inside_ref_lit := c.inside_ref_lit
			c.inside_ref_lit = (c.inside_ref_lit || right_node.op == .amp || is_shared)
			c.expr(right_node.right)
			c.inside_ref_lit = old_inside_ref_lit
			if right_node.op == .amp {
				if right_node.right is ast.Ident {
					if right_node.right.obj is ast.Var {
						v := right_node.right.obj
						right_type0 = v.typ
						if !v.is_mut && assigned_var.is_mut && !c.inside_unsafe {
							c.error('`$right_node.right.name` is immutable, cannot have a mutable reference to it',
								right_node.pos)
						}
					} else if right_node.right.obj is ast.ConstField {
						if assigned_var.is_mut && !c.inside_unsafe {
							c.error('`$right_node.right.name` is immutable, cannot have a mutable reference to it',
								right_node.pos)
						}
					}
				}
			}
			if right_node.op == .arrow {
				if assigned_var.is_mut {
					right_sym := c.table.get_type_symbol(right_type0)
					if right_sym.kind == .chan {
						chan_info := right_sym.chan_info()
						if chan_info.elem_type.is_ptr() && !chan_info.is_mut {
							c.error('cannot have a mutable reference to object from `$right_sym.name`',
								right_node.pos)
						}
					}
				}
			}
		}
	}
	if node.left_types.len != node.left.len {
		c.error('assign statement left type number mismatch', node.pos)
	}
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
		typ: typ.ref()
		is_used: true
	})
	s.register(ast.Var{
		name: 'b'
		pos: pos
		typ: typ.ref()
		is_used: true
	})
}

fn (mut c Checker) check_array_init_para_type(para string, expr ast.Expr, pos token.Position) {
	sym := c.table.get_type_symbol(c.expr(expr))
	if sym.kind !in [.int, .int_literal] {
		c.error('array $para needs to be an int', pos)
	}
}

pub fn (mut c Checker) ensure_sumtype_array_has_default_value(node ast.ArrayInit) {
	sym := c.table.get_type_symbol(node.elem_type)
	if sym.kind == .sum_type && !node.has_default {
		c.error('cannot initialize sum type array without default value', node.pos)
	}
}

pub fn (mut c Checker) array_init(mut node ast.ArrayInit) ast.Type {
	mut elem_type := ast.void_type
	// []string - was set in parser
	if node.typ != ast.void_type {
		if node.exprs.len == 0 {
			if node.has_cap {
				c.check_array_init_para_type('cap', node.cap_expr, node.pos)
			}
			if node.has_len {
				c.check_array_init_para_type('len', node.len_expr, node.pos)
			}
		}
		if node.has_default {
			default_expr := node.default_expr
			default_typ := c.check_expr_opt_call(default_expr, c.expr(default_expr))
			c.check_expected(default_typ, node.elem_type) or {
				c.error(err.msg, default_expr.position())
			}
		}
		if node.has_len {
			if node.has_len && !node.has_default {
				elem_type_sym := c.table.get_type_symbol(node.elem_type)
				if elem_type_sym.kind == .interface_ {
					c.error('cannot instantiate an array of interfaces without also giving a default `init:` value',
						node.len_expr.position())
				}
			}
			c.ensure_sumtype_array_has_default_value(node)
		}
		c.ensure_type_exists(node.elem_type, node.elem_type_pos) or {}
		if node.typ.has_flag(.generic) && c.table.cur_fn.generic_names.len == 0 {
			c.error('generic struct cannot use in non-generic function', node.pos)
		}
		return node.typ
	}
	if node.is_fixed {
		c.ensure_sumtype_array_has_default_value(node)
		c.ensure_type_exists(node.elem_type, node.elem_type_pos) or {}
	}
	// a = []
	if node.exprs.len == 0 {
		// a := fn_returing_opt_array() or { [] }
		if c.expected_type == ast.void_type && c.expected_or_type != ast.void_type {
			c.expected_type = c.expected_or_type
		}
		mut type_sym := c.table.get_type_symbol(c.expected_type)
		if type_sym.kind != .array || type_sym.array_info().elem_type == ast.void_type {
			c.error('array_init: no type specified (maybe: `[]Type{}` instead of `[]`)',
				node.pos)
			return ast.void_type
		}
		// TODO: seperate errors once bug is fixed with `x := if expr { ... } else { ... }`
		// if c.expected_type == ast.void_type {
		// c.error('array_init: use `[]Type{}` instead of `[]`', node.pos)
		// return ast.void_type
		// }
		array_info := type_sym.array_info()
		node.elem_type = array_info.elem_type
		// clear optional flag incase of: `fn opt_arr ?[]int { return [] }`
		return c.expected_type.clear_flag(.optional)
	}
	// [1,2,3]
	if node.exprs.len > 0 && node.elem_type == ast.void_type {
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
		for i, mut expr in node.exprs {
			typ := c.check_expr_opt_call(expr, c.expr(expr))
			node.expr_types << typ
			// The first element's type
			if expecting_interface_array {
				if i == 0 {
					elem_type = expected_value_type
					c.expected_type = elem_type
					c.type_implements(typ, elem_type, expr.position())
				}
				if !typ.is_ptr() && !typ.is_pointer() && !c.inside_unsafe {
					typ_sym := c.table.get_type_symbol(typ)
					if typ_sym.kind != .interface_ {
						c.mark_as_referenced(mut &expr, true)
					}
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
			if expr !is ast.TypeNode {
				c.check_expected(typ, elem_type) or {
					c.error('invalid array element: $err.msg', expr.position())
				}
			}
		}
		if node.is_fixed {
			idx := c.table.find_or_register_array_fixed(elem_type, node.exprs.len, ast.empty_expr())
			if elem_type.has_flag(.generic) {
				node.typ = ast.new_type(idx).set_flag(.generic)
			} else {
				node.typ = ast.new_type(idx)
			}
		} else {
			idx := c.table.find_or_register_array(elem_type)
			if elem_type.has_flag(.generic) {
				node.typ = ast.new_type(idx).set_flag(.generic)
			} else {
				node.typ = ast.new_type(idx)
			}
		}
		node.elem_type = elem_type
	} else if node.is_fixed && node.exprs.len == 1 && node.elem_type != ast.void_type {
		// [50]byte
		mut fixed_size := i64(0)
		init_expr := node.exprs[0]
		c.expr(init_expr)
		match init_expr {
			ast.IntegerLiteral {
				fixed_size = init_expr.val.int()
			}
			ast.Ident {
				if init_expr.obj is ast.ConstField {
					if comptime_value := c.eval_comptime_const_expr(init_expr.obj.expr,
						0)
					{
						fixed_size = comptime_value.i64() or { fixed_size }
					}
				} else {
					c.error('non-constant array bound `$init_expr.name`', init_expr.pos)
				}
			}
			ast.InfixExpr {
				if comptime_value := c.eval_comptime_const_expr(init_expr, 0) {
					fixed_size = comptime_value.i64() or { fixed_size }
				}
			}
			else {
				c.error('expecting `int` for fixed size', node.pos)
			}
		}
		if fixed_size <= 0 {
			c.error('fixed size cannot be zero or negative (fixed_size: $fixed_size)',
				init_expr.position())
		}
		idx := c.table.find_or_register_array_fixed(node.elem_type, int(fixed_size), init_expr)
		if node.elem_type.has_flag(.generic) {
			node.typ = ast.new_type(idx).set_flag(.generic)
		} else {
			node.typ = ast.new_type(idx)
		}
		if node.has_default {
			c.expr(node.default_expr)
		}
	}
	return node.typ
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
		ntype := typeof(node).replace('v.ast.', '')
		eprintln('checking: ${c.file.path:-30} | pos: ${node.pos.line_str():-39} | node: $ntype | $node')
	}
	c.expected_type = ast.void_type
	match mut node {
		ast.EmptyStmt {
			if c.pref.is_verbose {
				eprintln('Checker.stmt() EmptyStmt')
				print_backtrace()
			}
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
		ast.ComptimeFor {
			c.comptime_for(node)
		}
		ast.ConstDecl {
			c.inside_const = true
			c.const_decl(mut node)
			c.inside_const = false
		}
		ast.DeferStmt {
			if node.idx_in_fn < 0 {
				node.idx_in_fn = c.table.cur_fn.defer_stmts.len
				c.table.cur_fn.defer_stmts << unsafe { &node }
			}
			if c.locked_names.len != 0 || c.rlocked_names.len != 0 {
				c.error('defers are not allowed in lock statements', node.pos)
			}
			for i, ident in node.defer_vars {
				mut id := ident
				if id.info is ast.IdentVar {
					if id.comptime && id.name in checker.valid_comptime_not_user_defined {
						node.defer_vars[i] = ast.Ident{
							scope: 0
							name: ''
						}
						continue
					}
					mut info := id.info as ast.IdentVar
					typ := c.ident(mut id)
					if typ == ast.error_type_idx {
						continue
					}
					info.typ = typ
					id.info = info
					node.defer_vars[i] = id
				}
			}
			c.inside_defer = true
			c.stmts(node.stmts)
			c.inside_defer = false
		}
		ast.EnumDecl {
			c.enum_decl(mut node)
		}
		ast.ExprStmt {
			node.typ = c.expr(node.expr)
			c.expected_type = ast.void_type
			mut or_typ := ast.void_type
			match node.expr {
				ast.IndexExpr {
					if node.expr.or_expr.kind != .absent {
						node.is_expr = true
						or_typ = node.typ
					}
				}
				ast.PrefixExpr {
					if node.expr.or_block.kind != .absent {
						node.is_expr = true
						or_typ = node.typ
					}
				}
				else {}
			}
			if !c.pref.is_repl && (c.stmt_level == 1 || (c.stmt_level > 1 && !c.is_last_stmt)) {
				if node.expr is ast.InfixExpr {
					if node.expr.op == .left_shift {
						left_sym := c.table.get_final_type_symbol(node.expr.left_type)
						if left_sym.kind != .array {
							c.error('unused expression', node.pos)
						}
					}
				}
			}
			c.check_expr_opt_call(node.expr, or_typ)
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
			c.global_decl(mut node)
		}
		ast.GotoLabel {}
		ast.GotoStmt {
			if c.inside_defer {
				c.error('goto is not allowed in defer statements', node.pos)
			}
			if !c.inside_unsafe {
				c.warn('`goto` requires `unsafe` (consider using labelled break/continue)',
					node.pos)
			}
			if node.name !in c.table.cur_fn.label_names {
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
			c.interface_decl(mut node)
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
		c.inside_unsafe = true
		c.stmts(node.stmts)
		c.inside_unsafe = false
	} else {
		c.stmts(node.stmts)
	}
}

fn (mut c Checker) branch_stmt(node ast.BranchStmt) {
	if c.inside_defer {
		c.error('`$node.kind.str()` is not allowed in defer statements', node.pos)
	}
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

fn (mut c Checker) comptime_for(node ast.ComptimeFor) {
	typ := c.unwrap_generic(node.typ)
	sym := c.table.get_type_symbol(typ)
	if sym.kind == .placeholder || typ.has_flag(.generic) {
		c.error('unknown type `$sym.name`', node.typ_pos)
	}
	if node.kind == .fields {
		c.comptime_fields_type[node.val_var] = node.typ
	}
	c.stmts(node.stmts)
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
		if high_type in [ast.int_type, ast.int_literal_type] {
			node.val_type = typ
		} else {
			node.val_type = high_type
		}
		node.high_type = high_type
		node.scope.update_var_type(node.val_var, node.val_type)
	} else {
		sym := c.table.get_final_type_symbol(typ)
		if sym.kind == .struct_ {
			// iterators
			next_fn := sym.find_method_with_generic_parent('next') or {
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
			mut val_type := next_fn.return_type.clear_flag(.optional)
			if node.val_is_mut {
				val_type = val_type.ref()
			}
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
				value_type = value_type.ref()
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
					ast.SelectorExpr {
						root_ident := node.cond.root_ident() or { node.cond.expr as ast.Ident }
						if !(root_ident.obj as ast.Var).is_mut {
							c.error('field `$node.cond.field_name` is immutable, it cannot be changed',
								node.cond.pos)
						}
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
			if infix.left in [ast.Ident, ast.SelectorExpr] && infix.right is ast.TypeNode {
				is_variable := if mut infix.left is ast.Ident {
					infix.left.kind == .variable
				} else {
					true
				}
				left_type := c.expr(infix.left)
				left_sym := c.table.get_type_symbol(left_type)
				if is_variable {
					if left_sym.kind in [.sum_type, .interface_] {
						c.smartcast(infix.left, infix.left_type, infix.right.typ, mut
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

fn (mut c Checker) global_decl(mut node ast.GlobalDecl) {
	for mut field in node.fields {
		c.check_valid_snake_case(field.name, 'global name', field.pos)
		if field.name in c.global_names {
			c.error('duplicate global `$field.name`', field.pos)
		}
		sym := c.table.get_type_symbol(field.typ)
		if sym.kind == .placeholder {
			c.error('unknown type `$sym.name`', field.typ_pos)
		}
		if field.has_expr {
			field.typ = c.expr(field.expr)
			mut v := c.file.global_scope.find_global(field.name) or {
				panic('internal compiler error - could not find global in scope')
			}
			v.typ = c.table.mktyp(field.typ)
		}
		c.global_names << field.name
	}
}

fn (mut c Checker) go_expr(mut node ast.GoExpr) ast.Type {
	ret_type := c.call_expr(mut node.call_expr)
	if node.call_expr.or_block.kind != .absent {
		c.error('optional handling cannot be done in `go` call. Do it when calling `.wait()`',
			node.call_expr.or_block.pos)
	}
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

	if c.pref.backend.is_js() {
		return c.table.find_or_register_promise(ret_type)
	} else {
		return c.table.find_or_register_thread(ret_type)
	}
}

fn (mut c Checker) asm_stmt(mut stmt ast.AsmStmt) {
	if stmt.is_goto {
		c.warn('inline assembly goto is not supported, it will most likely not work',
			stmt.pos)
	}
	if c.pref.backend.is_js() {
		c.error('inline assembly is not supported in the js backend', stmt.pos)
	}
	if c.pref.backend == .c && c.pref.ccompiler_type == .msvc {
		c.error('msvc compiler does not support inline assembly', stmt.pos)
	}
	mut aliases := c.asm_ios(stmt.output, mut stmt.scope, true)
	aliases2 := c.asm_ios(stmt.input, mut stmt.scope, false)
	aliases << aliases2
	for mut template in stmt.templates {
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
				'string', 'asciz', 'ascii'] { // all tcc-supported assembler directives
				c.error('unknown assembler directive: `$template.name`', template.pos)
			}
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
		ast.AsmAlias {}
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
	if c.ct_cond_stack.len > 0 {
		node.ct_conds = c.ct_cond_stack.clone()
	}
	if c.pref.backend.is_js() {
		if !c.file.path.ends_with('.js.v') {
			c.error('hash statements are only allowed in backend specific files such "x.js.v"',
				node.pos)
		}
		if c.mod == 'main' {
			c.error('hash statements are not allowed in the main module. Place them in a separate module.',
				node.pos)
		}
		return
	}
	match node.kind {
		'include' {
			mut flag := node.main
			if flag.contains('@VROOT') {
				// c.note(checker.vroot_is_deprecated_message, node.pos)
				vroot := util.resolve_vmodroot(flag.replace('@VROOT', '@VMODROOT'), c.file.path) or {
					c.error(err.msg, node.pos)
					return
				}
				node.val = 'include $vroot'
				node.main = vroot
				flag = vroot
			}
			if flag.contains('@VEXEROOT') {
				vroot := flag.replace('@VEXEROOT', os.dir(pref.vexe_path()))
				node.val = 'include $vroot'
				node.main = vroot
				flag = vroot
			}
			if flag.contains('@VMODROOT') {
				vroot := util.resolve_vmodroot(flag, c.file.path) or {
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
			if flag.contains('@VROOT') {
				// c.note(checker.vroot_is_deprecated_message, node.pos)
				flag = util.resolve_vmodroot(flag.replace('@VROOT', '@VMODROOT'), c.file.path) or {
					c.error(err.msg, node.pos)
					return
				}
			}
			if flag.contains('@VEXEROOT') {
				// expand `@VEXEROOT` to its absolute path
				flag = flag.replace('@VEXEROOT', os.dir(pref.vexe_path()))
			}
			if flag.contains('@VMODROOT') {
				flag = util.resolve_vmodroot(flag, c.file.path) or {
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
					if !flag.contains('@VMODROOT') {
						c.error('$deprecated had been deprecated, use @VMODROOT instead.',
							node.pos)
					}
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

fn (mut c Checker) import_stmt(node ast.Import) {
	c.check_valid_snake_case(node.alias, 'module alias', node.pos)
	for sym in node.syms {
		name := '${node.mod}.$sym.name'
		if sym.name[0].is_capital() {
			if type_sym := c.table.find_type(name) {
				if type_sym.kind != .placeholder {
					if !type_sym.is_public {
						c.error('module `$node.mod` type `$sym.name` is private', sym.pos)
					}
					continue
				}
			}
			c.error('module `$node.mod` has no type `$sym.name`', sym.pos)
			continue
		}
		if func := c.table.find_fn(name) {
			if !func.is_pub {
				c.error('module `$node.mod` function `${sym.name}()` is private', sym.pos)
			}
			continue
		}
		if _ := c.file.global_scope.find_const(name) {
			continue
		}
		c.error('module `$node.mod` has no constant or function `$sym.name`', sym.pos)
	}
}

// stmts should be used for processing normal statement lists (fn bodies, for loop bodies etc).
fn (mut c Checker) stmts(stmts []ast.Stmt) {
	old_stmt_level := c.stmt_level
	c.stmt_level = 0
	c.stmts_ending_with_expression(stmts)
	c.stmt_level = old_stmt_level
}

// stmts_ending_with_expression, should be used for processing list of statements, that can end with an expression.
// Examples for such lists are the bodies of `or` blocks, `if` expressions and `match` expressions:
//    `x := opt() or { stmt1 stmt2 ExprStmt }`,
//    `x := if cond { stmt1 stmt2 ExprStmt } else { stmt2 stmt3 ExprStmt }`,
//    `x := match expr { Type1 { stmt1 stmt2 ExprStmt } else { stmt2 stmt3 ExprStmt }`.
fn (mut c Checker) stmts_ending_with_expression(stmts []ast.Stmt) {
	mut unreachable := token.Position{
		line_nr: -1
	}
	c.expected_type = ast.void_type
	c.stmt_level++
	for i, stmt in stmts {
		c.is_last_stmt = i == stmts.len - 1
		if c.scope_returns {
			if unreachable.line_nr == -1 {
				unreachable = stmt.pos
			}
		}
		c.stmt(stmt)
		if stmt is ast.GotoLabel {
			unreachable = token.Position{
				line_nr: -1
			}
			c.scope_returns = false
		}
		if c.should_abort {
			return
		}
	}
	c.stmt_level--
	if unreachable.line_nr >= 0 {
		c.error('unreachable code', unreachable)
	}
	c.find_unreachable_statements_after_noreturn_calls(stmts)
	c.scope_returns = false
	c.expected_type = ast.void_type
}

pub fn (mut c Checker) find_unreachable_statements_after_noreturn_calls(stmts []ast.Stmt) {
	mut prev_stmt_was_noreturn_call := false
	for stmt in stmts {
		match stmt {
			ast.ExprStmt {
				if stmt.expr is ast.CallExpr {
					if prev_stmt_was_noreturn_call {
						c.error('unreachable code after a [noreturn] call', stmt.pos)
						return
					}
					prev_stmt_was_noreturn_call = stmt.expr.is_noreturn
				}
			}
			else {
				prev_stmt_was_noreturn_call = false
			}
		}
	}
}

pub fn (mut c Checker) unwrap_generic(typ ast.Type) ast.Type {
	if typ.has_flag(.generic) {
		if t_typ := c.table.resolve_generic_to_concrete(typ, c.table.cur_fn.generic_names,
			c.table.cur_concrete_types)
		{
			return t_typ
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

	// c.expr_level set to 150 so that stack overflow does not occur on windows
	if c.expr_level > 150 {
		c.error('checker: too many expr levels: $c.expr_level ', node.position())
		return ast.void_type
	}
	match mut node {
		ast.NodeError {}
		ast.EmptyExpr {
			c.error('checker.expr(): unhandled EmptyExpr', token.Position{})
		}
		ast.CTempVar {
			return node.typ
		}
		ast.AnonFn {
			return c.anon_fn(mut node)
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
				if !c.table.sumtype_has_variant(node.expr_type, node.typ, true) {
					addr := '&'.repeat(node.typ.nr_muls())
					c.error('cannot cast `$expr_type_sym.name` to `$addr$type_sym.name`',
						node.pos)
				}
			} else if expr_type_sym.kind == .interface_ && type_sym.kind == .interface_ {
				c.ensure_type_exists(node.typ, node.pos) or {}
			} else if node.expr_type != node.typ {
				mut s := 'cannot cast non-sum type `$expr_type_sym.name` using `as`'
				if type_sym.kind == .sum_type {
					s += ' - use e.g. `${type_sym.name}(some_expr)` instead.'
				}
				c.error(s, node.pos)
			}
			return node.typ
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
		ast.GoExpr {
			return c.go_expr(mut node)
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
			old_inside_if_guard := c.inside_if_guard
			c.inside_if_guard = true
			node.expr_type = c.expr(node.expr)
			c.inside_if_guard = old_inside_if_guard
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
			return c.int_lit(mut node)
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
			if node.expr is ast.ParExpr {
				c.warn('redundant parentheses are used', node.pos)
			}
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
		ast.IsRefType {
			if !node.is_type {
				node.typ = c.expr(node.expr)
			}
			return ast.bool_type
		}
		ast.OffsetOf {
			return c.offset_of(node)
		}
		ast.SqlExpr {
			return c.sql_expr(mut node)
		}
		ast.StringLiteral {
			if node.language == .c {
				// string literal starts with "c": `C.printf(c'hello')`
				return ast.byte_type.set_nr_muls(1)
			}
			return c.string_lit(mut node)
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
	// Given: `Outside( Inside(xyz) )`,
	//        node.expr_type: `Inside`
	//        node.typ: `Outside`
	node.expr_type = c.expr(node.expr) // type to be casted
	mut from_type := node.expr_type
	to_type := node.typ
	//
	from_type_sym := c.table.get_type_symbol(from_type)
	from_type_sym_final := c.table.get_final_type_symbol(from_type)
	to_type_sym := c.table.get_type_symbol(to_type) // type to be used as cast
	to_type_sym_final := c.table.get_final_type_symbol(to_type)
	//
	if (to_type_sym.is_number() && from_type_sym.name == 'JS.Number')
		|| (to_type_sym.is_number() && from_type_sym.name == 'JS.BigInt')
		|| (to_type_sym.is_string() && from_type_sym.name == 'JS.String')
		|| (to_type.is_bool() && from_type_sym.name == 'JS.Boolean')
		|| (from_type.is_bool() && to_type_sym.name == 'JS.Boolean')
		|| (from_type_sym.is_number() && to_type_sym.name == 'JS.Number')
		|| (from_type_sym.is_number() && to_type_sym.name == 'JS.BigInt')
		|| (from_type_sym.is_string() && to_type_sym.name == 'JS.String') {
		return to_type
	}

	if to_type_sym.language != .c {
		c.ensure_type_exists(to_type, node.pos) or {}
	}
	if from_type_sym.kind == .byte && from_type.is_ptr() && to_type_sym.kind == .string
		&& !to_type.is_ptr() {
		c.error('to convert a C string buffer pointer to a V string, use x.vstring() instead of string(x)',
			node.pos)
	}
	if from_type == ast.void_type {
		c.error('expression does not return a value so it cannot be cast', node.expr.position())
	}
	//
	if to_type_sym.kind == .sum_type {
		if from_type in [ast.int_literal_type, ast.float_literal_type] {
			xx := if from_type == ast.int_literal_type { ast.int_type } else { ast.f64_type }
			node.expr_type = c.promote_num(node.expr_type, xx)
			from_type = node.expr_type
		}
		if !c.table.sumtype_has_variant(to_type, from_type, false) && !to_type.has_flag(.optional) {
			c.error('cannot cast `$from_type_sym.name` to `$to_type_sym.name`', node.pos)
		}
	} else if mut to_type_sym.info is ast.Alias {
		if !c.check_types(from_type, to_type_sym.info.parent_type) {
			c.error('cannot convert type `$from_type_sym.name` to `$to_type_sym.name` (alias to `$to_type_sym_final.name`)',
				node.pos)
		}
	} else if to_type_sym.kind == .byte && from_type != ast.voidptr_type
		&& from_type_sym.kind != .enum_ && !from_type.is_int() && !from_type.is_float()
		&& from_type != ast.bool_type && !from_type.is_ptr() && from_type_sym.kind == .alias
		&& from_type_sym_final.name != 'byte' {
		type_name := c.table.type_to_str(from_type)
		c.error('cannot cast type `$type_name` to `byte`', node.pos)
	} else if to_type_sym.kind == .struct_ && !to_type.is_ptr()
		&& !(to_type_sym.info as ast.Struct).is_typedef {
		// For now we ignore C typedef because of `C.Window(C.None)` in vlib/clipboard
		if from_type_sym.kind == .struct_ && !from_type.is_ptr() {
			c.warn('casting to struct is deprecated, use e.g. `Struct{...expr}` instead',
				node.pos)
			from_type_info := from_type_sym.info as ast.Struct
			to_type_info := to_type_sym.info as ast.Struct
			if !c.check_struct_signature(from_type_info, to_type_info) {
				c.error('cannot convert struct `$from_type_sym.name` to struct `$to_type_sym.name`',
					node.pos)
			}
		} else {
			type_name := c.table.type_to_str(from_type)
			c.error('cannot cast `$type_name` to struct', node.pos)
		}
	} else if to_type_sym.kind == .interface_ {
		if c.type_implements(from_type, to_type, node.pos) {
			if !from_type.is_ptr() && !from_type.is_pointer() && from_type_sym.kind != .interface_
				&& !c.inside_unsafe {
				c.mark_as_referenced(mut &node.expr, true)
			}
		}
	} else if to_type == ast.bool_type && from_type != ast.bool_type && !c.inside_unsafe {
		c.error('cannot cast to bool - use e.g. `some_int != 0` instead', node.pos)
	} else if from_type == ast.none_type && !to_type.has_flag(.optional) {
		type_name := c.table.type_to_str(to_type)
		c.error('cannot cast `none` to `$type_name`', node.pos)
	} else if from_type_sym.kind == .struct_ && !from_type.is_ptr() {
		if (to_type.is_ptr() || to_type_sym.kind !in [.sum_type, .interface_]) && !c.is_builtin_mod {
			from_type_name := c.table.type_to_str(from_type)
			type_name := c.table.type_to_str(to_type)
			snexpr := node.expr.str()
			c.error('cannot cast struct `$from_type_name` to `$type_name`, use `${snexpr}.str()` instead.',
				node.pos)
		}
	} else if from_type.has_flag(.optional) || from_type.has_flag(.variadic) {
		// variadic case can happen when arrays are converted into variadic
		msg := if from_type.has_flag(.optional) { 'an optional' } else { 'a variadic' }
		c.error('cannot type cast $msg', node.pos)
	} else if !c.inside_unsafe && to_type.is_ptr() && from_type.is_ptr()
		&& to_type.deref() != ast.char_type && from_type.deref() != ast.char_type {
		ft := c.table.type_to_str(from_type)
		tt := c.table.type_to_str(to_type)
		c.warn('casting `$ft` to `$tt` is only allowed in `unsafe` code', node.pos)
	} else if from_type_sym.kind == .array_fixed && !from_type.is_ptr() {
		c.warn('cannot cast a fixed array (use e.g. `&arr[0]` instead)', node.pos)
	}

	if to_type == ast.string_type {
		if from_type in [ast.byte_type, ast.bool_type] {
			snexpr := node.expr.str()
			c.error('cannot cast type `$from_type_sym.name` to string, use `${snexpr}.str()` instead.',
				node.pos)
		} else if from_type.is_real_pointer() {
			snexpr := node.expr.str()
			c.error('cannot cast pointer type `$from_type_sym.name` to string, use `&byte($snexpr).vstring()` or `cstring_to_vstring($snexpr)` instead.',
				node.pos)
		} else if from_type.is_number() {
			snexpr := node.expr.str()
			c.error('cannot cast number to string, use `${snexpr}.str()` instead.', node.pos)
		} else if from_type_sym.kind == .alias && from_type_sym_final.name != 'string' {
			c.error('cannot cast type `$from_type_sym.name` to string, use `x.str()` instead.',
				node.pos)
		} else if from_type_sym_final.kind == .array {
			snexpr := node.expr.str()
			if from_type_sym_final.name == '[]byte' {
				c.error('cannot cast []byte to string, use `${snexpr}.bytestr()` or `${snexpr}.str()` instead.',
					node.pos)
			} else {
				first_elem_idx := '[0]'
				c.error('cannot cast array to string, use `$snexpr${first_elem_idx}.str()` instead.',
					node.pos)
			}
		} else if from_type_sym_final.kind == .enum_ {
			snexpr := node.expr.str()
			c.error('cannot cast enum to string, use ${snexpr}.str() instead.', node.pos)
		} else if from_type_sym_final.kind == .map {
			c.error('cannot cast map to string.', node.pos)
		} else if from_type_sym_final.kind == .sum_type {
			snexpr := node.expr.str()
			c.error('cannot cast sumtype `$from_type_sym.name` to string, use `${snexpr}.str()` instead.',
				node.pos)
		} else if to_type != ast.string_type && from_type == ast.string_type
			&& (!(to_type_sym.kind == .alias && to_type_sym_final.name == 'string')) {
			mut error_msg := 'cannot cast a string to a type `$to_type_sym_final.name`, that is not an alias of string'
			if mut node.expr is ast.StringLiteral {
				if node.expr.val.len == 1 {
					error_msg += ", for denoting characters use `$node.expr.val` instead of '$node.expr.val'"
				}
			}
			c.error(error_msg, node.pos)
		}
	}

	if node.has_arg {
		c.expr(node.arg)
	}

	// checks on int literal to enum cast if the value represents a value on the enum
	if to_type_sym.kind == .enum_ {
		if node.expr is ast.IntegerLiteral {
			enum_typ_name := c.table.get_type_name(to_type)
			node_val := (node.expr as ast.IntegerLiteral).val.int()

			if enum_decl := c.table.enum_decls[to_type_sym.name] {
				mut in_range := false
				mut enum_val := 0

				for enum_field in enum_decl.fields {
					// check if the field of the enum value is an integer literal
					if enum_field.expr is ast.IntegerLiteral {
						enum_val = enum_field.expr.val.int()
					}

					if node_val == enum_val {
						in_range = true
						break
					}

					enum_val += 1
				}

				if !in_range {
					c.warn('$node_val does not represents a value of enum $enum_typ_name',
						node.pos)
				}
			}
		}
	}

	node.typname = c.table.get_type_symbol(to_type).name

	return to_type
}

fn (mut c Checker) comptime_call(mut node ast.ComptimeCall) ast.Type {
	sym := c.table.get_type_symbol(c.unwrap_generic(c.expr(node.left)))
	node.sym = *sym
	if node.is_env {
		env_value := util.resolve_env_value("\$env('$node.args_var')", false) or {
			c.error(err.msg, node.env_pos)
			return ast.string_type
		}
		node.env_value = env_value
		return ast.string_type
	}
	if node.is_embed {
		// c.file.embedded_files << node.embed_file
		if node.embed_file.compression_type !in checker.valid_comptime_compression_types {
			supported := checker.valid_comptime_compression_types.map('.$it').join(', ')
			c.error('not supported compression type: .${node.embed_file.compression_type}. supported: $supported',
				node.pos)
		}
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
			if k in c.fn_scope.objects && unsafe { c.fn_scope.objects[k] } is ast.Var {
				mut vsc := unsafe { c.fn_scope.objects[k] } as ast.Var
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
			node.val = c.table.cur_fn.name.all_after_last('.')
		}
		.method_name {
			fname := c.table.cur_fn.name.all_after_last('.')
			if c.table.cur_fn.is_method {
				node.val = c.table.type_to_str(c.table.cur_fn.receiver.typ).all_after_last('.') +
					'.' + fname
			} else {
				node.val = fname
			}
		}
		.mod_name {
			node.val = c.table.cur_fn.mod
		}
		.struct_name {
			if c.table.cur_fn.is_method {
				node.val = c.table.type_to_str(c.table.cur_fn.receiver.typ).all_after_last('.')
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
			node.val = version.vhash()
		}
		.vmod_file {
			// cache the vmod content, do not read it many times
			if c.vmod_file_content.len == 0 {
				mut mcache := vmod.get_cache()
				vmod_file_location := mcache.get_by_file(c.file.path)
				if vmod_file_location.vmod_file.len == 0 {
					c.error('@VMOD_FILE can be used only in projects, that have v.mod file',
						node.pos)
				}
				vmod_content := os.read_file(vmod_file_location.vmod_file) or { '' }
				c.vmod_file_content = vmod_content.replace('\r\n', '\n') // normalise EOLs just in case
			}
			node.val = c.vmod_file_content
		}
		.vroot_path {
			node.val = os.dir(pref.vexe_path())
		}
		.vexeroot_path {
			node.val = os.dir(pref.vexe_path())
		}
		.vmodroot_path {
			mut mcache := vmod.get_cache()
			vmod_file_location := mcache.get_by_file(c.file.path)
			node.val = os.dir(vmod_file_location.vmod_file)
		}
		.unknown {
			c.error('unknown @ identifier: ${node.name}. Available identifiers: $token.valid_at_tokens',
				node.pos)
		}
	}
	return ast.string_type
}

pub fn (mut c Checker) ident(mut node ast.Ident) ast.Type {
	// TODO: move this
	if c.const_deps.len > 0 {
		mut name := node.name
		if !name.contains('.') && node.mod != 'builtin' {
			name = '${node.mod}.$node.name'
		}
		if name == c.const_decl {
			c.error('cycle in constant `$c.const_decl`', node.pos)
			return ast.void_type
		}
		c.const_deps << name
	}
	if node.kind == .blank_ident {
		if node.tok_kind !in [.assign, .decl_assign] {
			c.error('undefined ident: `_` (may only be used in assignments)', node.pos)
		}
		return ast.void_type
	}
	// second use
	if node.kind in [.constant, .global, .variable] {
		info := node.info as ast.IdentVar
		// Got a var with type T, return current generic type
		return info.typ
	} else if node.kind == .function {
		info := node.info as ast.IdentFn
		return info.typ
	} else if node.kind == .unresolved {
		// first use
		if node.tok_kind == .assign && node.is_mut {
			c.error('`mut` not allowed with `=` (use `:=` to declare a variable)', node.pos)
		}
		if obj := node.scope.find(node.name) {
			match mut obj {
				ast.GlobalField {
					node.kind = .global
					node.info = ast.IdentVar{
						typ: obj.typ
					}
					node.obj = obj
					return obj.typ
				}
				ast.Var {
					// incase var was not marked as used yet (vweb tmpl)
					// obj.is_used = true
					if node.pos.pos < obj.pos.pos {
						c.error('undefined variable `$node.name` (used before declaration)',
							node.pos)
					}
					is_sum_type_cast := obj.smartcasts.len != 0
						&& !c.prevent_sum_type_unwrapping_once
					c.prevent_sum_type_unwrapping_once = false
					mut typ := if is_sum_type_cast { obj.smartcasts.last() } else { obj.typ }
					if typ == 0 {
						if mut obj.expr is ast.Ident {
							if obj.expr.kind == .unresolved {
								c.error('unresolved variable: `$node.name`', node.pos)
								return ast.void_type
							}
						}
						if mut obj.expr is ast.IfGuardExpr {
							// new variable from if guard shouldn't have the optional flag for further use
							// a temp variable will be generated which unwraps it
							typ = obj.expr.expr_type.clear_flag(.optional)
						} else {
							typ = c.expr(obj.expr)
						}
					}
					is_optional := typ.has_flag(.optional)
					node.kind = .variable
					node.info = ast.IdentVar{
						typ: typ
						is_optional: is_optional
					}
					if typ == ast.error_type && c.expected_type == ast.string_type
						&& !c.using_new_err_struct && !c.inside_selector_expr
						&& !c.inside_println_arg && !c.file.mod.name.contains('v.')
						&& !c.is_builtin_mod {
						//                          ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ <- TODO: remove; this prevents a failure in the `performance-regressions` CI job
						c.warn('string errors are deprecated; use `err.msg` instead',
							node.pos)
					}
					// if typ == ast.t_type {
					// sym := c.table.get_type_symbol(c.cur_generic_type)
					// println('IDENT T unresolved $node.name typ=$sym.name')
					// Got a var with type T, return current generic type
					// typ = c.cur_generic_type
					// }
					// } else {
					if !is_sum_type_cast {
						obj.typ = typ
					}
					node.obj = obj
					// unwrap optional (`println(x)`)
					if is_optional {
						return typ.clear_flag(.optional)
					}
					return typ
				}
				else {}
			}
		}
		mut name := node.name
		// check for imported symbol
		if name in c.file.imported_symbols {
			name = c.file.imported_symbols[name]
		}
		// prepend mod to look for fn call or const
		else if !name.contains('.') && node.mod != 'builtin' {
			name = '${node.mod}.$node.name'
		}
		if obj := c.file.global_scope.find(name) {
			match mut obj {
				ast.ConstField {
					if !(obj.is_pub || obj.mod == c.mod || c.pref.is_test) {
						c.error('constant `$obj.name` is private', node.pos)
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
					node.name = name
					node.kind = .constant
					node.info = ast.IdentVar{
						typ: typ
					}
					obj.typ = typ
					node.obj = obj
					return typ
				}
				else {}
			}
		}
		// Non-anon-function object (not a call), e.g. `onclick(my_click)`
		if func := c.table.find_fn(name) {
			fn_type := ast.new_type(c.table.find_or_register_fn_type(node.mod, func, false,
				true))
			node.name = name
			node.kind = .function
			node.info = ast.IdentFn{
				typ: fn_type
			}
			return fn_type
		}
	}
	if node.language == .c {
		if node.name == 'C.NULL' {
			return ast.voidptr_type
		}
		return ast.int_type
	}
	if c.inside_sql {
		if field := c.table.find_field(c.cur_orm_ts, node.name) {
			return field.typ
		}
	}
	if node.kind == .unresolved && node.mod != 'builtin' {
		// search in the `builtin` idents, for example
		// main.compare_f32 may actually be builtin.compare_f32
		saved_mod := node.mod
		node.mod = 'builtin'
		builtin_type := c.ident(mut node)
		if builtin_type != ast.void_type {
			return builtin_type
		}
		node.mod = saved_mod
	}
	if node.tok_kind == .assign {
		c.error('undefined ident: `$node.name` (use `:=` to declare a variable)', node.pos)
	} else if node.name == 'errcode' {
		c.error('undefined ident: `errcode`; did you mean `err.code`?', node.pos)
	} else {
		if c.inside_ct_attr {
			c.note('`[if $node.name]` is deprecated. Use `[if $node.name?]` instead',
				node.pos)
		} else {
			c.error('undefined ident: `$node.name`', node.pos)
		}
	}
	if c.table.known_type(node.name) {
		// e.g. `User`  in `json.decode(User, '...')`
		return ast.void_type
	}
	return ast.void_type
}

pub fn (mut c Checker) concat_expr(mut node ast.ConcatExpr) ast.Type {
	mut mr_types := []ast.Type{}
	for expr in node.vals {
		mr_types << c.expr(expr)
	}
	if node.vals.len == 1 {
		typ := mr_types[0]
		node.return_type = typ
		return typ
	} else {
		typ := c.table.find_or_register_multi_return(mr_types)
		ast.new_type(typ)
		node.return_type = typ
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
	c.check_expr_opt_call(node.cond, cond_type)
	cond_type_sym := c.table.get_type_symbol(cond_type)
	node.is_sum_type = cond_type_sym.kind in [.interface_, .sum_type]
	c.match_exprs(mut node, cond_type_sym)
	c.expected_type = cond_type
	mut first_iteration := true
	mut ret_type := ast.void_type
	mut nbranches_with_return := 0
	mut nbranches_without_return := 0
	for branch in node.branches {
		if node.is_expr {
			c.stmts_ending_with_expression(branch.stmts)
		} else {
			c.stmts(branch.stmts)
		}
		if node.is_expr {
			if branch.stmts.len > 0 {
				// ignore last statement - workaround
				// currently the last statement in a match branch does not have an
				// expected value set, so e.g. IfExpr.is_expr is not set.
				// probably any mismatch will be caught by not producing a value instead
				for st in branch.stmts[0..branch.stmts.len - 1] {
					// must not contain C statements
					st.check_c_expr() or {
						c.error('`match` expression branch has $err.msg', st.pos)
					}
				}
			} else if ret_type != ast.void_type {
				c.error('`match` expression requires an expression as the last statement of every branch',
					branch.branch_pos)
			}
		}
		// If the last statement is an expression, return its type
		if branch.stmts.len > 0 {
			mut stmt := branch.stmts[branch.stmts.len - 1]
			match mut stmt {
				ast.ExprStmt {
					if node.is_expr {
						c.expected_type = node.expected_type
					}
					expr_type := c.expr(stmt.expr)
					if first_iteration {
						if node.is_expr && (node.expected_type.has_flag(.optional)
							|| c.table.type_kind(node.expected_type) == .sum_type) {
							ret_type = node.expected_type
						} else {
							ret_type = expr_type
						}
						stmt.typ = expr_type
					} else if node.is_expr && ret_type.idx() != expr_type.idx() {
						if !c.check_types(ret_type, expr_type)
							&& !c.check_types(expr_type, ret_type) {
							ret_sym := c.table.get_type_symbol(ret_type)
							is_noreturn := is_noreturn_callexpr(stmt.expr)
							if !(node.is_expr && ret_sym.kind == .sum_type) && !is_noreturn {
								c.error('return type mismatch, it should be `$ret_sym.name`',
									stmt.expr.position())
							}
						}
					}
				}
				else {
					if node.is_expr && ret_type != ast.void_type {
						c.error('`match` expression requires an expression as the last statement of every branch',
							stmt.pos)
					}
				}
			}
		}
		first_iteration = false
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
	cond_var := c.get_base_name(&node.cond)
	if cond_var != '' {
		mut cond_is_auto_heap := false
		for branch in node.branches {
			if v := branch.scope.find_var(cond_var) {
				if v.is_auto_heap {
					cond_is_auto_heap = true
					break
				}
			}
		}
		if cond_is_auto_heap {
			for branch in node.branches {
				mut v := branch.scope.find_var(cond_var) or { continue }
				v.is_auto_heap = true
			}
		}
	}
	return ret_type
}

fn (mut c Checker) match_exprs(mut node ast.MatchExpr, cond_type_sym ast.TypeSymbol) {
	// branch_exprs is a histogram of how many times
	// an expr was used in the match
	mut branch_exprs := map[string]int{}
	for branch_i, _ in node.branches {
		mut branch := node.branches[branch_i]
		mut expr_types := []ast.TypeNode{}
		for k, expr in branch.exprs {
			mut key := ''
			if expr is ast.RangeExpr {
				mut low := i64(0)
				mut high := i64(0)
				c.expected_type = node.expected_type
				low_expr := expr.low
				high_expr := expr.high
				if low_expr is ast.IntegerLiteral {
					if high_expr is ast.IntegerLiteral
						&& (cond_type_sym.is_int() || cond_type_sym.info is ast.Enum) {
						low = low_expr.val.i64()
						high = high_expr.val.i64()
						if low > high {
							c.error('start value is higher than end value', branch.pos)
						}
					} else {
						c.error('mismatched range types', low_expr.pos)
					}
				} else if low_expr is ast.CharLiteral {
					if high_expr is ast.CharLiteral && cond_type_sym.kind in [.byte, .char, .rune] {
						low = low_expr.val[0]
						high = high_expr.val[0]
						if low > high {
							c.error('start value is higher than end value', branch.pos)
						}
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
			expr_type_sym := c.table.get_type_symbol(expr_type)
			if cond_type_sym.kind == .interface_ {
				// TODO
				// This generates a memory issue with TCC
				// Needs to be checked later when TCC errors are fixed
				// Current solution is to move expr.position() to its own statement
				// c.type_implements(expr_type, c.expected_type, expr.position())
				expr_pos := expr.position()
				if c.type_implements(expr_type, c.expected_type, expr_pos) {
					if !expr_type.is_ptr() && !expr_type.is_pointer() && !c.inside_unsafe {
						if expr_type_sym.kind != .interface_ {
							c.mark_as_referenced(mut &branch.exprs[k], true)
						}
					}
				}
			} else if mut cond_type_sym.info is ast.SumType {
				if expr_type !in cond_type_sym.info.variants {
					expr_str := c.table.type_to_str(expr_type)
					expect_str := c.table.type_to_str(node.cond_type)
					c.error('`$expect_str` has no variant `$expr_str`', expr.position())
				}
			} else if cond_type_sym.info is ast.Alias && expr_type_sym.info is ast.Struct {
				expr_str := c.table.type_to_str(expr_type)
				expect_str := c.table.type_to_str(node.cond_type)
				c.error('cannot match alias type `$expect_str` with `$expr_str`', expr.position())
			} else if !c.check_types(expr_type, node.cond_type) {
				expr_str := c.table.type_to_str(expr_type)
				expect_str := c.table.type_to_str(node.cond_type)
				c.error('cannot match `$expect_str` with `$expr_str`', expr.position())
			}
			branch_exprs[key] = val + 1
		}
		// when match is type matching, then register smart cast for every branch
		if expr_types.len > 0 {
			if cond_type_sym.kind in [.sum_type, .interface_] {
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

				c.smartcast(node.cond, node.cond_type, expr_type, mut branch.scope)
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
		if has_else && !c.pref.translated {
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
			if remaining > 0 {
				err_details += ', and $remaining others ...'
			}
		}
		err_details += ' or `else {}` at the end)'
	} else {
		err_details += ' (add `else {}` at the end)'
	}
	c.error(err_details, node.pos)
}

// smartcast takes the expression with the current type which should be smartcasted to the target type in the given scope
fn (c Checker) smartcast(expr ast.Expr, cur_type ast.Type, to_type_ ast.Type, mut scope ast.Scope) {
	sym := c.table.get_type_symbol(cur_type)
	to_type := if sym.kind == .interface_ { to_type_.ref() } else { to_type_ }
	match expr {
		ast.SelectorExpr {
			mut is_mut := false
			mut smartcasts := []ast.Type{}
			expr_sym := c.table.get_type_symbol(expr.expr_type)
			mut orig_type := 0
			if field := c.table.find_field(expr_sym, expr.field_name) {
				if field.is_mut {
					if root_ident := expr.root_ident() {
						if v := scope.find_var(root_ident.name) {
							is_mut = v.is_mut
						}
					}
				}
				if orig_type == 0 {
					orig_type = field.typ
				}
			}
			if field := scope.find_struct_field(expr.expr.str(), expr.expr_type, expr.field_name) {
				smartcasts << field.smartcasts
			}
			// smartcast either if the value is immutable or if the mut argument is explicitly given
			if !is_mut || expr.is_mut {
				smartcasts << to_type
				scope.register_struct_field(expr.expr.str(), ast.ScopeStructField{
					struct_type: expr.expr_type
					name: expr.field_name
					typ: cur_type
					smartcasts: smartcasts
					pos: expr.pos
					orig_type: orig_type
				})
			}
		}
		ast.Ident {
			mut is_mut := false
			mut smartcasts := []ast.Type{}
			mut is_already_casted := false
			mut orig_type := 0
			if mut expr.obj is ast.Var {
				is_mut = expr.obj.is_mut
				smartcasts << expr.obj.smartcasts
				is_already_casted = expr.obj.pos.pos == expr.pos.pos
				if orig_type == 0 {
					orig_type = expr.obj.typ
				}
			}
			// smartcast either if the value is immutable or if the mut argument is explicitly given
			if (!is_mut || expr.is_mut) && !is_already_casted {
				smartcasts << to_type
				scope.register(ast.Var{
					name: expr.name
					typ: cur_type
					pos: expr.pos
					is_used: true
					is_mut: expr.is_mut
					smartcasts: smartcasts
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
						c.error('invalid type `$tsym.name` for timeout - expected integer number of nanoseconds aka `time.Duration`',
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
		e_typ := c.expr(node.lockeds[i])
		id_name := node.lockeds[i].str()
		if !e_typ.has_flag(.shared_f) {
			obj_type := if node.lockeds[i] is ast.Ident { 'variable' } else { 'struct element' }
			c.error('`$id_name` must be declared as `shared` $obj_type to be locked',
				node.lockeds[i].position())
		}
		if id_name in c.locked_names {
			c.error('`$id_name` is already locked', node.lockeds[i].position())
		} else if id_name in c.rlocked_names {
			c.error('`$id_name` is already read-locked', node.lockeds[i].position())
		}
		if node.is_rlock[i] {
			c.rlocked_names << id_name
		} else {
			c.locked_names << id_name
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
	c.inside_unsafe = true
	t := c.expr(node.expr)
	c.inside_unsafe = false
	return t
}

fn (mut c Checker) smartcast_if_conds(node ast.Expr, mut scope ast.Scope) {
	if node is ast.InfixExpr {
		if node.op == .and {
			c.smartcast_if_conds(node.left, mut scope)
			c.smartcast_if_conds(node.right, mut scope)
		} else if node.op == .key_is {
			right_expr := node.right
			mut right_type := match right_expr {
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
			right_type = c.unwrap_generic(right_type)
			if right_type != ast.Type(0) {
				left_sym := c.table.get_type_symbol(node.left_type)
				right_sym := c.table.get_type_symbol(right_type)
				expr_type := c.unwrap_generic(c.expr(node.left))
				if left_sym.kind == .interface_ {
					if right_sym.kind != .interface_ {
						c.type_implements(right_type, expr_type, node.pos)
					} else {
						return
					}
				} else if !c.check_types(right_type, expr_type) {
					expect_str := c.table.type_to_str(right_type)
					expr_str := c.table.type_to_str(expr_type)
					c.error('cannot use type `$expect_str` as type `$expr_str`', node.pos)
				}
				if node.left in [ast.Ident, ast.SelectorExpr] && node.right is ast.TypeNode {
					is_variable := if mut node.left is ast.Ident {
						node.left.kind == .variable
					} else {
						true
					}
					if is_variable {
						if left_sym.kind in [.interface_, .sum_type] {
							c.smartcast(node.left, node.left_type, right_type, mut scope)
						}
					}
				}
			}
		}
	} else if node is ast.Likely {
		c.smartcast_if_conds(node.expr, mut scope)
	}
}

pub fn (mut c Checker) if_expr(mut node ast.IfExpr) ast.Type {
	if_kind := if node.is_comptime { '\$if' } else { 'if' }
	mut node_is_expr := false
	if node.branches.len > 0 && node.has_else {
		stmts := node.branches[0].stmts
		if stmts.len > 0 && stmts[stmts.len - 1] is ast.ExprStmt
			&& (stmts[stmts.len - 1] as ast.ExprStmt).typ != ast.void_type {
			node_is_expr = true
		}
	}
	if c.expected_type == ast.void_type && node_is_expr {
		c.expected_type = c.expected_or_type
	}
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
				should_skip = c.comptime_if_branch(branch.cond, branch.pos)
				node.branches[i].pkg_exist = !should_skip
			} else {
				// check condition type is boolean
				c.expected_type = ast.bool_type
				cond_typ := c.expr(branch.cond)
				if (cond_typ.idx() != ast.bool_type_idx || cond_typ.has_flag(.optional))
					&& !c.pref.translated {
					c.error('non-bool type `${c.table.type_to_str(cond_typ)}` used as if condition',
						branch.cond.position())
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
					} else if branch.cond.right is ast.TypeNode && left is ast.TypeNode
						&& sym.kind == .interface_ {
						is_comptime_type_is_expr = true
						// is interface
						checked_type := c.unwrap_generic(left.typ)
						should_skip = !c.table.does_type_implement_interface(checked_type,
							got_type)
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
			if c.fn_level == 0 && c.pref.output_cross_c {
				// do not skip any of the branches for top level `$if OS {`
				// statements, in `-os cross` mode
				found_branch = false
				c.skip_flags = false
				c.ct_cond_stack << branch.cond
			}
			if !c.skip_flags {
				if node_is_expr {
					c.stmts_ending_with_expression(branch.stmts)
				} else {
					c.stmts(branch.stmts)
				}
			} else if c.pref.output_cross_c {
				mut is_freestanding_block := false
				if branch.cond is ast.Ident {
					if branch.cond.name == 'freestanding' {
						is_freestanding_block = true
					}
				}
				if is_freestanding_block {
					branch.stmts = []
					node.branches[i].stmts = []
				}
				if node_is_expr {
					c.stmts_ending_with_expression(branch.stmts)
				} else {
					c.stmts(branch.stmts)
				}
			} else if !is_comptime_type_is_expr {
				node.branches[i].stmts = []
			}
			if comptime_field_name.len > 0 {
				c.comptime_fields_type.delete(comptime_field_name)
			}
			c.skip_flags = cur_skip_flags
			if c.fn_level == 0 && c.pref.output_cross_c && c.ct_cond_stack.len > 0 {
				c.ct_cond_stack.delete_last()
			}
		} else {
			// smartcast sumtypes and interfaces when using `is`
			c.smartcast_if_conds(branch.cond, mut branch.scope)
			if node_is_expr {
				c.stmts_ending_with_expression(branch.stmts)
			} else {
				c.stmts(branch.stmts)
			}
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
					if node.is_expr
						&& c.table.get_type_symbol(former_expected_type).kind == .sum_type {
						continue
					}
					if is_noreturn_callexpr(last_expr.expr) {
						continue
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

// comptime_if_branch checks the condition of a compile-time `if` branch. It returns `true`
// if that branch's contents should be skipped (targets a different os for example)
fn (mut c Checker) comptime_if_branch(cond ast.Expr, pos token.Position) bool {
	// TODO: better error messages here
	match cond {
		ast.BoolLiteral {
			return !cond.val
		}
		ast.ParExpr {
			return c.comptime_if_branch(cond.expr, pos)
		}
		ast.PrefixExpr {
			if cond.op != .not {
				c.error('invalid `\$if` condition', cond.pos)
			}
			return !c.comptime_if_branch(cond.right, cond.pos)
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
					l := c.comptime_if_branch(cond.left, cond.pos)
					r := c.comptime_if_branch(cond.right, cond.pos)
					return l || r // skip (return true) if at least one should be skipped
				}
				.logical_or {
					l := c.comptime_if_branch(cond.left, cond.pos)
					r := c.comptime_if_branch(cond.right, cond.pos)
					return l && r // skip (return true) only if both should be skipped
				}
				.key_is, .not_is {
					if cond.left is ast.TypeNode && cond.right is ast.TypeNode {
						// `$if Foo is Interface {`
						sym := c.table.get_type_symbol(cond.right.typ)
						if sym.kind != .interface_ {
							c.expr(cond.left)
							// c.error('`$sym.name` is not an interface', cond.right.position())
						}
						return false
					} else if cond.left in [ast.SelectorExpr, ast.TypeNode] {
						// `$if method.@type is string`
						c.expr(cond.left)
						return false
					} else {
						c.error('invalid `\$if` condition: expected a type or a selector expression or an interface check',
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
			cname := cond.name
			if cname in checker.valid_comptime_if_os {
				mut is_os_target_different := false
				if !c.pref.output_cross_c {
					target_os := c.pref.os.str().to_lower()
					is_os_target_different = cname != target_os
				}
				return is_os_target_different
			} else if cname in checker.valid_comptime_if_compilers {
				return pref.cc_from_string(cname) != c.pref.ccompiler_type
			} else if cname in checker.valid_comptime_if_platforms {
				if cname == 'aarch64' {
					c.note('use `arm64` instead of `aarch64`', pos)
				}
				match cname {
					'amd64' { return c.pref.arch != .amd64 }
					'i386' { return c.pref.arch != .i386 }
					'aarch64' { return c.pref.arch != .arm64 }
					'arm64' { return c.pref.arch != .arm64 }
					'arm32' { return c.pref.arch != .arm32 }
					'rv64' { return c.pref.arch != .rv64 }
					'rv32' { return c.pref.arch != .rv32 }
					else { return false }
				}
			} else if cname in checker.valid_comptime_if_cpu_features {
				return false
			} else if cname in checker.valid_comptime_if_other {
				match cname {
					'js' { return !c.pref.backend.is_js() }
					'debug' { return !c.pref.is_debug }
					'prod' { return !c.pref.is_prod }
					'test' { return !c.pref.is_test }
					'glibc' { return false } // TODO
					'threads' { return c.table.gostmts == 0 }
					'prealloc' { return !c.pref.prealloc }
					'no_bounds_checking' { return cname !in c.pref.compile_defines_all }
					'freestanding' { return !c.pref.is_bare || c.pref.output_cross_c }
					'interpreter' { c.pref.backend != .interpret }
					else { return false }
				}
			} else if cname !in c.pref.compile_defines_all {
				if cname == 'linux_or_macos' {
					c.error('linux_or_macos is deprecated, use `\$if linux || macos {` instead',
						cond.pos)
					return false
				}
				// `$if some_var {}`, or `[if user_defined_tag] fn abc(){}`
				typ := c.expr(cond)
				if cond.obj !is ast.Var && cond.obj !is ast.ConstField
					&& cond.obj !is ast.GlobalField {
					if !c.inside_ct_attr {
						c.error('unknown var: `$cname`', pos)
					}
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
		ast.ComptimeCall {
			if cond.is_pkgconfig {
				mut m := pkgconfig.main([cond.args_var]) or {
					c.error(err.msg, cond.pos)
					return true
				}
				m.run() or { return true }
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
			if s.expr in [ast.IfExpr, ast.MatchExpr] {
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
	typ := c.unwrap_generic(c.expr(node.expr))
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

pub fn (mut c Checker) mark_as_referenced(mut node ast.Expr, as_interface bool) {
	match mut node {
		ast.Ident {
			if mut node.obj is ast.Var {
				mut obj := unsafe { &node.obj }
				if c.fn_scope != voidptr(0) {
					obj = c.fn_scope.find_var(node.obj.name) or { obj }
				}
				type_sym := c.table.get_type_symbol(obj.typ.set_nr_muls(0))
				if obj.is_stack_obj && !type_sym.is_heap() && !c.pref.translated {
					suggestion := if type_sym.kind == .struct_ {
						'declaring `$type_sym.name` as `[heap]`'
					} else {
						'wrapping the `$type_sym.name` object in a `struct` declared as `[heap]`'
					}
					if !c.pref.translated {
						mischief := if as_interface {
							'used as interface object'
						} else {
							'referenced'
						}
						c.error('`$node.name` cannot be $mischief outside `unsafe` blocks as it might be stored on stack. Consider ${suggestion}.',
							node.pos)
					}
				} else if type_sym.kind == .array_fixed {
					c.error('cannot reference fixed array `$node.name` outside `unsafe` blocks as it is supposed to be stored on stack',
						node.pos)
				} else {
					match type_sym.kind {
						.struct_ {
							info := type_sym.info as ast.Struct
							if !info.is_heap {
								node.obj.is_auto_heap = true
							}
						}
						else {
							node.obj.is_auto_heap = true
						}
					}
				}
			}
		}
		ast.SelectorExpr {
			if !node.expr_type.is_ptr() {
				c.mark_as_referenced(mut &node.expr, as_interface)
			}
		}
		ast.IndexExpr {
			c.mark_as_referenced(mut &node.left, as_interface)
		}
		else {}
	}
}

pub fn (mut c Checker) get_base_name(node &ast.Expr) string {
	match node {
		ast.Ident {
			return node.name
		}
		ast.SelectorExpr {
			return c.get_base_name(&node.expr)
		}
		ast.IndexExpr {
			return c.get_base_name(&node.left)
		}
		else {
			return ''
		}
	}
}

pub fn (mut c Checker) prefix_expr(mut node ast.PrefixExpr) ast.Type {
	old_inside_ref_lit := c.inside_ref_lit
	c.inside_ref_lit = c.inside_ref_lit || node.op == .amp
	right_type := c.expr(node.right)
	c.inside_ref_lit = old_inside_ref_lit
	node.right_type = right_type
	if node.op == .amp {
		if mut node.right is ast.PrefixExpr {
			if node.right.op == .amp {
				c.error('unexpected `&`, expecting expression', node.right.pos)
			}
		}
	}
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
		if !c.inside_fn_arg && !c.inside_unsafe {
			c.mark_as_referenced(mut &node.right, false)
		}
		return right_type.ref()
	} else if node.op == .amp && node.right !is ast.CastExpr {
		if !c.inside_fn_arg && !c.inside_unsafe {
			c.mark_as_referenced(mut &node.right, false)
		}
		if node.right.is_auto_deref_var() {
			return right_type
		} else {
			return right_type.ref()
		}
	}
	if node.op == .mul {
		if right_type.is_ptr() {
			return right_type.deref()
		}
		if !right_type.is_pointer() && !c.pref.translated {
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
	// FIXME
	// there are currently other issues to investigate if right_type
	// is unwraped directly as initialization, so do it here
	right_sym := c.table.get_final_type_symbol(c.unwrap_generic(right_type))
	if node.op == .minus && !right_sym.is_number() {
		c.error('- operator can only be used with numeric types', node.pos)
	}
	if node.op == .arrow {
		if right_sym.kind == .chan {
			c.stmts_ending_with_expression(node.or_block.stmts)
			return right_sym.chan_info().elem_type
		}
		c.error('<- operator can only be used with `chan` types', node.pos)
	}
	return right_type
}

fn (mut c Checker) check_index(typ_sym &ast.TypeSymbol, index ast.Expr, index_type ast.Type, pos token.Position, range_index bool) {
	index_type_sym := c.table.get_type_symbol(index_type)
	// println('index expr left=$typ_sym.name $node.pos.line_nr')
	// if typ_sym.kind == .array && (!(ast.type_idx(index_type) in ast.number_type_idxs) &&
	// index_type_sym.kind != .enum_) {
	if typ_sym.kind in [.array, .array_fixed, .string] {
		if !(index_type.is_int() || index_type_sym.kind == .enum_) {
			type_str := if typ_sym.kind == .string {
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
			type_str := if typ_sym.kind == .string {
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
	mut typ_sym := c.table.get_final_type_symbol(typ)
	node.left_type = typ
	for {
		match typ_sym.kind {
			.map {
				node.is_map = true
				break
			}
			.array {
				node.is_array = true
				break
			}
			.array_fixed {
				node.is_farray = true
				break
			}
			.any {
				gname := typ_sym.name
				typ = c.unwrap_generic(typ)
				node.left_type = typ
				typ_sym = c.table.get_final_type_symbol(typ)
				if typ.is_ptr() {
					continue
				} else {
					c.error('generic type $gname does not support indexing, pass an array, or a reference instead, e.g. []$gname or &$gname',
						node.pos)
				}
			}
			else {
				break
			}
		}
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
	if !c.inside_unsafe && ((typ.is_ptr() && !typ.has_flag(.shared_f)
		&& !node.left.is_auto_deref_var()) || typ.is_pointer()) {
		mut is_ok := false
		if mut node.left is ast.Ident {
			if node.left.obj is ast.Var {
				v := node.left.obj as ast.Var
				// `mut param []T` function parameter
				is_ok = v.is_mut && v.is_arg && !typ.deref().is_ptr()
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
		if typ_sym.kind == .map {
			info := typ_sym.info as ast.Map
			c.expected_type = info.key_type
			index_type := c.expr(node.index)
			if !c.check_types(index_type, info.key_type) {
				err := c.expected_msg(index_type, info.key_type)
				c.error('invalid key: $err', node.pos)
			}
			value_sym := c.table.get_type_symbol(info.value_type)
			if !node.is_setter && value_sym.kind == .sum_type && node.or_expr.kind == .absent
				&& !c.inside_unsafe && !c.inside_if_guard {
				c.warn('`or {}` block required when indexing a map with sum type value',
					node.pos)
			}
		} else {
			index_type := c.expr(node.index)
			c.check_index(typ_sym, node.index, index_type, node.pos, false)
		}
		value_type := c.table.value_type(typ)
		if value_type != ast.void_type {
			typ = value_type
		}
	}
	c.stmts_ending_with_expression(node.or_expr.stmts)
	c.check_expr_opt_call(node, typ)
	return typ
}

// `.green` or `Color.green`
// If a short form is used, `expected_type` needs to be an enum
// with this value.
pub fn (mut c Checker) enum_val(mut node ast.EnumVal) ast.Type {
	mut typ_idx := if node.enum_name == '' {
		c.expected_type.idx()
	} else {
		c.table.find_type_idx(node.enum_name)
	}
	if typ_idx == 0 {
		// Handle `builtin` enums like `ChanState`, so that `x := ChanState.closed` works.
		// In the checker the name for such enums was set to `main.ChanState` instead of
		// just `ChanState`.
		if node.enum_name.starts_with('${c.mod}.') {
			typ_idx = c.table.find_type_idx(node.enum_name['${c.mod}.'.len..])
			if typ_idx == 0 {
				c.error('unknown enum `$node.enum_name` (type_idx=0)', node.pos)
				return ast.void_type
			}
		}
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
	if !(typ_sym.is_public || typ_sym.mod == c.mod) {
		c.error('enum `$typ_sym.name` is private', node.pos)
	}
	info := typ_sym.enum_info()
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
			if sym.kind == .struct_ {
				c.error('`{}` can not be used for initialising empty structs any more. Use `${c.table.type_to_str(c.expected_type)}{}` instead.',
					node.pos)
			} else {
				c.error('invalid empty map initialisation syntax, use e.g. map[string]int{} instead',
					node.pos)
			}
			return ast.void_type
		}
	}
	// `x := map[string]string` - set in parser
	if node.typ != 0 {
		info := c.table.get_type_symbol(node.typ).map_info()
		c.ensure_type_exists(info.key_type, node.pos) or {}
		c.ensure_type_exists(info.value_type, node.pos) or {}
		node.key_type = c.unwrap_generic(info.key_type)
		node.value_type = c.unwrap_generic(info.value_type)
		return node.typ
	}
	if node.keys.len > 0 && node.vals.len > 0 {
		mut key0_type := ast.void_type
		mut val0_type := ast.void_type
		use_expected_type := c.expected_type != ast.void_type && !c.inside_const
			&& c.table.get_type_symbol(c.expected_type).kind == .map
		if use_expected_type {
			sym := c.table.get_type_symbol(c.expected_type)
			info := sym.map_info()
			key0_type = c.unwrap_generic(info.key_type)
			val0_type = c.unwrap_generic(info.value_type)
		} else {
			// `{'age': 20}`
			key0_type = c.table.mktyp(c.expr(node.keys[0]))
			if node.keys[0].is_auto_deref_var() {
				key0_type = key0_type.deref()
			}
			val0_type = c.table.mktyp(c.expr(node.vals[0]))
			if node.vals[0].is_auto_deref_var() {
				val0_type = val0_type.deref()
			}
		}
		mut same_key_type := true
		for i, key in node.keys {
			if i == 0 && !use_expected_type {
				continue
			}
			val := node.vals[i]
			c.expected_type = key0_type
			key_type := c.expr(key)
			c.expected_type = val0_type
			val_type := c.expr(val)
			if !c.check_types(key_type, key0_type) || (i == 0 && key_type.is_number()
				&& key0_type.is_number() && key0_type != c.table.mktyp(key_type)) {
				msg := c.expected_msg(key_type, key0_type)
				c.error('invalid map key: $msg', key.position())
				same_key_type = false
			}
			if !c.check_types(val_type, val0_type) || (i == 0 && val_type.is_number()
				&& val0_type.is_number() && val0_type != c.table.mktyp(val_type)) {
				msg := c.expected_msg(val_type, val0_type)
				c.error('invalid map value: $msg', val.position())
			}
		}
		if same_key_type {
			for i in 1 .. node.keys.len {
				c.check_dup_keys(node, i)
			}
		}
		key0_type = c.unwrap_generic(key0_type)
		val0_type = c.unwrap_generic(val0_type)
		mut map_type := ast.new_type(c.table.find_or_register_map(key0_type, val0_type))
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
	$if checker_exit_on_first_error ? {
		eprintln('\n\n>> checker error: $message, pos: $pos')
		print_backtrace()
		exit(1)
	}
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
	if c.pref.message_limit >= 0 && c.nr_notices >= c.pref.message_limit {
		c.should_abort = true
		return
	}
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
		if c.pref.message_limit >= 0 && c.nr_warnings >= c.pref.message_limit {
			c.should_abort = true
			return
		}
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
		if c.pref.message_limit >= 0 && c.errors.len >= c.pref.message_limit {
			c.should_abort = true
			return
		}
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
	c.cur_orm_ts = *sym
	if sym.info !is ast.Struct {
		c.error('The table symbol `$sym.name` has to be a struct', node.table_expr.pos)
		return ast.void_type
	}
	info := sym.info as ast.Struct
	fields := c.fetch_and_verify_orm_fields(info, node.table_expr.pos, sym.name)
	mut sub_structs := map[int]ast.SqlExpr{}
	for f in fields.filter((c.table.type_symbols[int(it.typ)].kind == .struct_
		|| (c.table.get_type_symbol(it.typ).kind == .array
		&& c.table.get_type_symbol(c.table.get_type_symbol(it.typ).array_info().elem_type).kind == .struct_))
		&& c.table.get_type_name(it.typ) != 'time.Time') {
		typ := if c.table.get_type_symbol(f.typ).kind == .struct_ {
			f.typ
		} else if c.table.get_type_symbol(f.typ).kind == .array {
			c.table.get_type_symbol(f.typ).array_info().elem_type
		} else {
			ast.Type(0)
		}
		mut n := ast.SqlExpr{
			pos: node.pos
			has_where: true
			typ: typ
			db_expr: node.db_expr
			table_expr: ast.TypeNode{
				pos: node.table_expr.pos
				typ: typ
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

		sub_structs[int(typ)] = n
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
	c.expr(node.db_expr)
	mut typ := ast.void_type
	for mut line in node.lines {
		a := c.sql_stmt_line(mut line)
		if a != ast.void_type {
			typ = a
		}
	}
	return typ
}

fn (mut c Checker) sql_stmt_line(mut node ast.SqlStmtLine) ast.Type {
	c.inside_sql = true
	defer {
		c.inside_sql = false
	}
	c.ensure_type_exists(node.table_expr.typ, node.pos) or { return ast.void_type }
	table_sym := c.table.get_type_symbol(node.table_expr.typ)
	c.cur_orm_ts = *table_sym
	if table_sym.info !is ast.Struct {
		c.error('unknown type `$table_sym.name`', node.pos)
		return ast.void_type
	}
	info := table_sym.info as ast.Struct
	fields := c.fetch_and_verify_orm_fields(info, node.table_expr.pos, table_sym.name)
	mut sub_structs := map[int]ast.SqlStmtLine{}
	for f in fields.filter(((c.table.type_symbols[int(it.typ)].kind == .struct_)
		|| (c.table.get_type_symbol(it.typ).kind == .array
		&& c.table.get_type_symbol(c.table.get_type_symbol(it.typ).array_info().elem_type).kind == .struct_))
		&& c.table.get_type_name(it.typ) != 'time.Time') {
		typ := if c.table.get_type_symbol(f.typ).kind == .struct_ {
			f.typ
		} else if c.table.get_type_symbol(f.typ).kind == .array {
			c.table.get_type_symbol(f.typ).array_info().elem_type
		} else {
			ast.Type(0)
		}
		mut object_var_name := '${node.object_var_name}.$f.name'
		if typ != f.typ {
			object_var_name = node.object_var_name
		}
		mut n := ast.SqlStmtLine{
			pos: node.pos
			kind: node.kind
			table_expr: ast.TypeNode{
				pos: node.table_expr.pos
				typ: typ
			}
			object_var_name: object_var_name
		}
		tmp_inside_sql := c.inside_sql
		c.sql_stmt_line(mut n)
		c.inside_sql = tmp_inside_sql
		sub_structs[typ] = n
	}
	node.fields = fields
	node.sub_structs = sub_structs.move()
	for i, column in node.updated_columns {
		x := node.fields.filter(it.name == column)
		if x.len == 0 {
			c.error('type `$table_sym.name` has no field named `$column`', node.pos)
			continue
		}
		field := x[0]
		node.updated_columns[i] = c.fetch_field_name(field)
	}
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
	fields := info.fields.filter(
		(it.typ in [ast.string_type, ast.bool_type] || int(it.typ) in ast.number_type_idxs
		|| c.table.type_symbols[int(it.typ)].kind == .struct_
		|| (c.table.get_type_symbol(it.typ).kind == .array
		&& c.table.get_type_symbol(c.table.get_type_symbol(it.typ).array_info().elem_type).kind == .struct_))
		&& !it.attrs.contains('skip'))
	if fields.len == 0 {
		c.error('V orm: select: empty fields in `$table_name`', pos)
		return []ast.StructField{}
	}
	if fields[0].name != 'id' {
		c.error('V orm: `id int` must be the first field in `$table_name`', pos)
	}
	return fields
}

fn (mut c Checker) fetch_field_name(field ast.StructField) string {
	mut name := field.name
	for attr in field.attrs {
		if attr.kind == .string && attr.name == 'sql' && attr.arg != '' {
			name = attr.arg
			break
		}
	}
	sym := c.table.get_type_symbol(field.typ)
	if sym.kind == .struct_ && sym.name != 'time.Time' {
		name = '${name}_id'
	}
	return name
}

fn (mut c Checker) post_process_generic_fns() {
	// Loop thru each generic function concrete type.
	// Check each specific fn instantiation.
	for i in 0 .. c.file.generic_fns.len {
		mut node := c.file.generic_fns[i]
		c.mod = node.mod
		for concrete_types in c.table.fn_generic_types[node.name] {
			c.table.cur_concrete_types = concrete_types
			c.fn_decl(mut node)
			if node.name == 'vweb.run' {
				for ct in concrete_types {
					if ct !in c.vweb_gen_types {
						c.vweb_gen_types << ct
					}
				}
			}
		}
		c.table.cur_concrete_types = []
	}
}

fn (mut c Checker) evaluate_once_comptime_if_attribute(mut node ast.Attr) bool {
	if node.ct_evaled {
		return node.ct_skip
	}
	if node.ct_expr is ast.Ident {
		if node.ct_opt {
			if node.ct_expr.name in checker.valid_comptime_not_user_defined {
				c.error('optional `[if expression ?]` tags, can be used only for user defined identifiers',
					node.pos)
				node.ct_skip = true
			} else {
				node.ct_skip = node.ct_expr.name !in c.pref.compile_defines
			}
			node.ct_evaled = true
			return node.ct_skip
		} else {
			if node.ct_expr.name !in checker.valid_comptime_not_user_defined {
				c.note('`[if $node.ct_expr.name]` is deprecated. Use `[if $node.ct_expr.name ?]` instead',
					node.pos)
				node.ct_skip = node.ct_expr.name !in c.pref.compile_defines
				node.ct_evaled = true
				return node.ct_skip
			} else {
				if node.ct_expr.name in c.pref.compile_defines {
					// explicitly allow custom user overrides with `-d linux` for example, for easier testing:
					node.ct_skip = false
					node.ct_evaled = true
					return node.ct_skip
				}
			}
		}
	}
	c.inside_ct_attr = true
	node.ct_skip = c.comptime_if_branch(node.ct_expr, node.pos)
	c.inside_ct_attr = false
	node.ct_evaled = true
	return node.ct_skip
}

fn (mut c Checker) fn_decl(mut node ast.FnDecl) {
	if node.generic_names.len > 0 && c.table.cur_concrete_types.len == 0 {
		// Just remember the generic function for now.
		// It will be processed later in c.post_process_generic_fns,
		// after all other normal functions are processed.
		// This is done so that all generic function calls can
		// have a chance to populate c.table.fn_generic_types with
		// the correct concrete types.
		c.file.generic_fns << node
		return
	}
	// save all the state that fn_decl or inner  statements/expressions
	// could potentially modify, since functions can be nested, due to
	// anonymous function support, and ensure that it is restored, when
	// fn_decl returns:
	prev_fn_scope := c.fn_scope
	prev_in_for_count := c.in_for_count
	prev_inside_defer := c.inside_defer
	prev_inside_unsafe := c.inside_unsafe
	prev_inside_anon_fn := c.inside_anon_fn
	prev_returns := c.returns
	prev_stmt_level := c.stmt_level
	c.fn_level++
	c.in_for_count = 0
	c.inside_defer = false
	c.inside_unsafe = false
	c.returns = false
	defer {
		c.stmt_level = prev_stmt_level
		c.fn_level--
		c.returns = prev_returns
		c.inside_anon_fn = prev_inside_anon_fn
		c.inside_unsafe = prev_inside_unsafe
		c.inside_defer = prev_inside_defer
		c.in_for_count = prev_in_for_count
		c.fn_scope = prev_fn_scope
	}
	// Check generics fn/method without generic type parameters
	mut need_generic_names := false
	if node.generic_names.len == 0 {
		if node.return_type.has_flag(.generic) {
			need_generic_names = true
		} else {
			for param in node.params {
				if param.typ.has_flag(.generic) {
					need_generic_names = true
					break
				}
			}
		}
		if need_generic_names {
			c.error('generic function declaration must specify generic type names, e.g. foo<T>',
				node.pos)
		}
	}
	if node.language == .v && !c.is_builtin_mod && !node.is_anon {
		c.check_valid_snake_case(node.name, 'function name', node.pos)
	}
	if node.name == 'main.main' {
		c.main_fn_decl_node = *node
	}
	if node.return_type != ast.void_type {
		if ct_attr_idx := node.attrs.find_comptime_define() {
			sexpr := node.attrs[ct_attr_idx].ct_expr.str()
			c.error('only functions that do NOT return values can have `[if $sexpr]` tags',
				node.pos)
		}
		if node.generic_names.len > 0 {
			gs := c.table.get_type_symbol(node.return_type)
			if gs.info is ast.Struct {
				if gs.info.is_generic && !node.return_type.has_flag(.generic) {
					c.error('return generic struct in fn declaration must specify the generic type names, e.g. Foo<T>',
						node.return_type_pos)
				}
			}
		}
		return_sym := c.table.get_type_symbol(node.return_type)
		if return_sym.info is ast.MultiReturn {
			for multi_type in return_sym.info.types {
				multi_sym := c.table.get_type_symbol(multi_type)
				if multi_type == ast.error_type {
					c.error('type `IError` cannot be used in multi-return, return an option instead',
						node.return_type_pos)
				} else if multi_type.has_flag(.optional) {
					c.error('option cannot be used in multi-return, return an option instead',
						node.return_type_pos)
				} else if multi_sym.kind == .array_fixed {
					c.error('fixed array cannot be used in multi-return', node.return_type_pos)
				}
			}
		} else if return_sym.kind == .array_fixed {
			c.error('fixed array cannot be returned by function', node.return_type_pos)
		}
	} else {
		for mut a in node.attrs {
			if a.kind == .comptime_define {
				node.should_be_skipped = c.evaluate_once_comptime_if_attribute(mut a)
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
		} else if sym.kind == .sum_type && node.name == 'type_idx' {
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
			if mut sym.info is ast.Interface {
				// if the method is in info.methods then it is an interface method
				if sym.info.has_method(node.name) {
					c.error('interface `$sym.name` cannot implement its own interface method `$node.name`',
						node.pos)
				}
			}
		}
		if mut sym.info is ast.Struct {
			if field := c.table.find_field(sym, node.name) {
				field_sym := c.table.get_type_symbol(field.typ)
				if field_sym.kind == .function {
					c.error('type `$sym.name` has both field and method named `$node.name`',
						node.pos)
				}
			}
			if node.name == 'free' {
				if node.return_type != ast.void_type {
					c.error('`.free()` methods should not have a return type', node.return_type_pos)
				}
				if !node.receiver.typ.is_ptr() {
					tname := sym.name.after_char(`.`)
					c.error('`.free()` methods should be defined on either a `(mut x &$tname)`, or a `(x &$tname)` receiver',
						node.receiver_pos)
				}
				if node.params.len != 1 {
					c.error('`.free()` methods should have 0 arguments', node.pos)
				}
			}
		}
		// needed for proper error reporting during vweb route checking
		if node.method_idx < sym.methods.len {
			sym.methods[node.method_idx].source_fn = voidptr(node)
		} else {
			c.error('method index: $node.method_idx >= sym.methods.len: $sym.methods.len',
				node.pos)
		}
	}
	if node.language == .v {
		// Make sure all types are valid
		for mut param in node.params {
			c.ensure_type_exists(param.typ, param.type_pos) or { return }
			if param.name in checker.reserved_type_names {
				c.error('invalid use of reserved type `$param.name` as a parameter name',
					param.pos)
			}
			if !param.typ.is_ptr() { // value parameter, i.e. on stack - check for `[heap]`
				arg_typ_sym := c.table.get_type_symbol(param.typ)
				if arg_typ_sym.kind == .struct_ {
					info := arg_typ_sym.info as ast.Struct
					if info.is_heap { // set auto_heap to promote value parameter
						mut v := node.scope.find_var(param.name) or { continue }
						v.is_auto_heap = true
					}
					if info.generic_types.len > 0 && !param.typ.has_flag(.generic)
						&& info.concrete_types.len == 0 {
						c.error('generic struct in fn declaration must specify the generic type names, e.g. Foo<T>',
							param.type_pos)
					}
				} else if arg_typ_sym.kind == .interface_ {
					info := arg_typ_sym.info as ast.Interface
					if info.generic_types.len > 0 && !param.typ.has_flag(.generic)
						&& info.concrete_types.len == 0 {
						c.error('generic interface in fn declaration must specify the generic type names, e.g. Foo<T>',
							param.type_pos)
					}
				} else if arg_typ_sym.kind == .sum_type {
					info := arg_typ_sym.info as ast.SumType
					if info.generic_types.len > 0 && !param.typ.has_flag(.generic)
						&& info.concrete_types.len == 0 {
						c.error('generic sumtype in fn declaration must specify the generic type names, e.g. Foo<T>',
							param.type_pos)
					}
				}
			}
			if c.pref.translated && node.is_variadic && node.params.len == 1 && param.typ.is_ptr() {
				// TODO c2v hack to fix `(const char *s, ...)`
				param.typ = ast.int_type.ref()
			}
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
		c.ensure_type_exists(node.return_type, node.return_type_pos) or { return }
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
				if param_sym.kind == .string && receiver_sym.kind == .string {
					// bypass check for strings
					// TODO there must be a better way to handle that
				} else if param_sym.kind !in [.struct_, .alias]
					|| receiver_sym.kind !in [.struct_, .alias] {
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
	if node.language == .v && !node.is_method && node.is_test {
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

		if node.params.len != 0 {
			c.error('test functions should take 0 parameters', node.pos)
		}
		if node.return_type != ast.void_type_idx
			&& node.return_type.clear_flag(.optional) != ast.void_type_idx {
			c.error('test functions should either return nothing at all, or be marked to return `?`',
				node.pos)
		}
	}
	c.expected_type = ast.void_type
	c.table.cur_fn = unsafe { node }
	// c.table.cur_fn = node
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
	node_has_top_return := has_top_return(node.stmts)
	node.has_return = c.returns || node_has_top_return
	c.check_noreturn_fn_decl(mut node)
	if node.language == .v && !node.no_body && node.return_type != ast.void_type && !node.has_return
		&& !node.is_noreturn {
		if c.inside_anon_fn {
			c.error('missing return at the end of an anonymous function', node.pos)
		} else if !node.attrs.contains('_naked') {
			c.error('missing return at end of function `$node.name`', node.pos)
		}
	}
	node.source_file = c.file
}

fn (mut c Checker) anon_fn(mut node ast.AnonFn) ast.Type {
	keep_fn := c.table.cur_fn
	keep_inside_anon := c.inside_anon_fn
	defer {
		c.table.cur_fn = keep_fn
		c.inside_anon_fn = keep_inside_anon
	}
	c.table.cur_fn = unsafe { &node.decl }
	c.inside_anon_fn = true
	for mut var in node.inherited_vars {
		parent_var := node.decl.scope.parent.find_var(var.name) or {
			panic('unexpected checker error: cannot find parent of inherited variable `$var.name`')
		}
		if var.is_mut && !parent_var.is_mut {
			c.error('original `$parent_var.name` is immutable, declare it with `mut` to make it mutable',
				var.pos)
		}
		var.typ = parent_var.typ
	}
	c.stmts(node.decl.stmts)
	c.fn_decl(mut node.decl)
	return node.typ
}

// NB: has_top_return/1 should be called on *already checked* stmts,
// which do have their stmt.expr.is_noreturn set properly:
fn has_top_return(stmts []ast.Stmt) bool {
	for stmt in stmts {
		match stmt {
			ast.Return {
				return true
			}
			ast.Block {
				if has_top_return(stmt.stmts) {
					return true
				}
			}
			ast.ExprStmt {
				if stmt.expr is ast.CallExpr {
					if stmt.expr.is_noreturn {
						return true
					}
				}
			}
			else {}
		}
	}
	return false
}

fn (mut c Checker) verify_vweb_params_for_method(node ast.Fn) (bool, int, int) {
	margs := node.params.len - 1 // first arg is the receiver/this
	if node.attrs.len == 0 {
		// allow non custom routed methods, with 1:1 mapping
		return true, -1, margs
	}
	if node.params.len > 1 {
		for param in node.params[1..] {
			param_sym := c.table.get_final_type_symbol(param.typ)
			if !(param_sym.is_string() || param_sym.is_number() || param_sym.is_float()
				|| param_sym.kind == .bool) {
				c.error('invalid type `$param_sym.name` for parameter `$param.name` in vweb app method `$node.name`',
					param.pos)
			}
		}
	}
	mut route_attributes := 0
	for a in node.attrs {
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
	c.table.used_vweb_types = c.vweb_gen_types
	typ_vweb_result := c.table.find_type_idx('vweb.Result')
	old_file := c.file
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
						&& f.name == m.name && !f.attrs.contains('post') {
						c.change_current_file(f.source_file) // setup of file path for the warning
						c.warn('mismatched parameters count between vweb method `${sym_app.name}.$m.name` ($nargs) and route attribute $m.attrs ($nroute_attributes)',
							f.pos)
					}
				}
			}
		}
	}
	c.change_current_file(old_file)
}

fn (mut c Checker) trace(fbase string, message string) {
	if c.file.path_base == fbase {
		println('> c.trace | ${fbase:-10s} | $message')
	}
}

fn (mut c Checker) ensure_type_exists(typ ast.Type, pos token.Position) ? {
	if typ == 0 {
		c.error('unknown type', pos)
		return
	}
	sym := c.table.get_type_symbol(typ)
	match sym.kind {
		.placeholder {
			if sym.language == .v && !sym.name.starts_with('C.') {
				c.error(util.new_suggestion(sym.name, c.table.known_type_names()).say('unknown type `$sym.name`'),
					pos)
				return
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
				return
			}
		}
		.array {
			c.ensure_type_exists((sym.info as ast.Array).elem_type, pos) ?
		}
		.array_fixed {
			c.ensure_type_exists((sym.info as ast.ArrayFixed).elem_type, pos) ?
		}
		.map {
			info := sym.info as ast.Map
			c.ensure_type_exists(info.key_type, pos) ?
			c.ensure_type_exists(info.value_type, pos) ?
		}
		else {}
	}
}

// comptime const eval
fn (mut c Checker) eval_comptime_const_expr(expr ast.Expr, nlevel int) ?ast.ComptTimeConstValue {
	if nlevel > 100 {
		// protect against a too deep comptime eval recursion
		return none
	}
	match expr {
		ast.ParExpr {
			return c.eval_comptime_const_expr(expr.expr, nlevel + 1)
		}
		// ast.EnumVal {
		//	c.note('>>>>>>>> expr: $expr', expr.pos)
		//	return expr.val.i64()
		// }
		ast.SizeOf {
			xtype := expr.typ
			if xtype.is_real_pointer() {
				if c.pref.m64 {
					return 8 // 64bit platform
				}
				return 4 // 32bit platform
			}
			if int(xtype) == xtype.idx() {
				match xtype {
					ast.char_type { return 1 }
					ast.i8_type { return 1 }
					ast.i16_type { return 2 }
					ast.int_type { return 4 }
					ast.i64_type { return 8 }
					//
					ast.byte_type { return 1 }
					ast.u8_type { return 1 }
					ast.u16_type { return 2 }
					ast.u32_type { return 4 }
					ast.u64_type { return 8 }
					else {}
				}
			}
			return none
		}
		ast.FloatLiteral {
			x := expr.val.f64()
			return x
		}
		ast.IntegerLiteral {
			x := expr.val.u64()
			if x > 9223372036854775807 {
				return x
			}
			return expr.val.i64()
		}
		ast.StringLiteral {
			return util.smart_quote(expr.val, expr.is_raw)
		}
		ast.CharLiteral {
			runes := expr.val.runes()
			if runes.len > 0 {
				return runes[0]
			}
			return none
		}
		ast.Ident {
			if expr.obj is ast.ConstField {
				// an existing constant?
				return c.eval_comptime_const_expr(expr.obj.expr, nlevel + 1)
			}
		}
		ast.CastExpr {
			cast_expr_value := c.eval_comptime_const_expr(expr.expr, nlevel + 1) or { return none }
			if expr.typ == ast.i8_type {
				return cast_expr_value.i8() or { return none }
			}
			if expr.typ == ast.i16_type {
				return cast_expr_value.i16() or { return none }
			}
			if expr.typ == ast.int_type {
				return cast_expr_value.int() or { return none }
			}
			if expr.typ == ast.i64_type {
				return cast_expr_value.i64() or { return none }
			}
			//
			if expr.typ == ast.byte_type {
				return cast_expr_value.byte() or { return none }
			}
			if expr.typ == ast.u16_type {
				return cast_expr_value.u16() or { return none }
			}
			if expr.typ == ast.u32_type {
				return cast_expr_value.u32() or { return none }
			}
			if expr.typ == ast.u64_type {
				return cast_expr_value.u64() or { return none }
			}
			//
			if expr.typ == ast.f32_type {
				return cast_expr_value.f32() or { return none }
			}
			if expr.typ == ast.f64_type {
				return cast_expr_value.f64() or { return none }
			}
		}
		ast.InfixExpr {
			left := c.eval_comptime_const_expr(expr.left, nlevel + 1) ?
			right := c.eval_comptime_const_expr(expr.right, nlevel + 1) ?
			if left is string && right is string {
				match expr.op {
					.plus {
						return left + right
					}
					else {
						return none
					}
				}
			} else if left is u64 && right is i64 {
				match expr.op {
					.plus { return i64(left) + i64(right) }
					.minus { return i64(left) - i64(right) }
					.mul { return i64(left) * i64(right) }
					.div { return i64(left) / i64(right) }
					.mod { return i64(left) % i64(right) }
					.xor { return i64(left) ^ i64(right) }
					.pipe { return i64(left) | i64(right) }
					.amp { return i64(left) & i64(right) }
					.left_shift { return i64(u64(left) << i64(right)) }
					.right_shift { return i64(u64(left) >> i64(right)) }
					.unsigned_right_shift { return i64(u64(left) >>> i64(right)) }
					else { return none }
				}
			} else if left is i64 && right is u64 {
				match expr.op {
					.plus { return i64(left) + i64(right) }
					.minus { return i64(left) - i64(right) }
					.mul { return i64(left) * i64(right) }
					.div { return i64(left) / i64(right) }
					.mod { return i64(left) % i64(right) }
					.xor { return i64(left) ^ i64(right) }
					.pipe { return i64(left) | i64(right) }
					.amp { return i64(left) & i64(right) }
					.left_shift { return i64(u64(left) << i64(right)) }
					.right_shift { return i64(u64(left) >> i64(right)) }
					.unsigned_right_shift { return i64(u64(left) >>> i64(right)) }
					else { return none }
				}
			} else if left is u64 && right is u64 {
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
					.unsigned_right_shift { return left >>> right }
					else { return none }
				}
			} else if left is i64 && right is i64 {
				match expr.op {
					.plus { return left + right }
					.minus { return left - right }
					.mul { return left * right }
					.div { return left / right }
					.mod { return left % right }
					.xor { return left ^ right }
					.pipe { return left | right }
					.amp { return left & right }
					.left_shift { return i64(u64(left) << right) }
					.right_shift { return i64(u64(left) >> right) }
					.unsigned_right_shift { return i64(u64(left) >>> right) }
					else { return none }
				}
			} else if left is byte && right is byte {
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
					.unsigned_right_shift { return left >>> right }
					else { return none }
				}
			}
		}
		// ast.ArrayInit {}
		// ast.PrefixExpr {
		//	c.note('prefixexpr: $expr', expr.pos)
		// }
		else {
			// eprintln('>>> nlevel: $nlevel | another $expr.type_name() | $expr ')
			return none
		}
	}
	return none
}

fn (mut c Checker) check_noreturn_fn_decl(mut node ast.FnDecl) {
	if !node.is_noreturn {
		return
	}
	if node.no_body {
		return
	}
	if node.return_type != ast.void_type {
		c.error('[noreturn] functions cannot have return types', node.pos)
	}
	if uses_return_stmt(node.stmts) {
		c.error('[noreturn] functions cannot use return statements', node.pos)
	}
	if node.stmts.len != 0 {
		mut is_valid_end_of_noreturn_fn := false
		last_stmt := node.stmts.last()
		match last_stmt {
			ast.ExprStmt {
				if last_stmt.expr is ast.CallExpr {
					if last_stmt.expr.should_be_skipped {
						c.error('[noreturn] functions cannot end with a skippable `[if ..]` call',
							last_stmt.pos)
						return
					}
					if last_stmt.expr.is_noreturn {
						is_valid_end_of_noreturn_fn = true
					}
				}
			}
			ast.ForStmt {
				if last_stmt.is_inf && last_stmt.stmts.len == 0 {
					is_valid_end_of_noreturn_fn = true
				}
			}
			else {}
		}
		if !is_valid_end_of_noreturn_fn {
			c.error('[noreturn] functions should end with a call to another [noreturn] function, or with an infinite `for {}` loop',
				last_stmt.pos)
			return
		}
	}
}

fn uses_return_stmt(stmts []ast.Stmt) bool {
	if stmts.len == 0 {
		return false
	}
	for stmt in stmts {
		match stmt {
			ast.Return {
				return true
			}
			ast.Block {
				if uses_return_stmt(stmt.stmts) {
					return true
				}
			}
			ast.ExprStmt {
				match stmt.expr {
					ast.CallExpr {
						if uses_return_stmt(stmt.expr.or_block.stmts) {
							return true
						}
					}
					ast.MatchExpr {
						for b in stmt.expr.branches {
							if uses_return_stmt(b.stmts) {
								return true
							}
						}
					}
					ast.SelectExpr {
						for b in stmt.expr.branches {
							if uses_return_stmt(b.stmts) {
								return true
							}
						}
					}
					ast.IfExpr {
						for b in stmt.expr.branches {
							if uses_return_stmt(b.stmts) {
								return true
							}
						}
					}
					else {}
				}
			}
			ast.ForStmt {
				if uses_return_stmt(stmt.stmts) {
					return true
				}
			}
			ast.ForCStmt {
				if uses_return_stmt(stmt.stmts) {
					return true
				}
			}
			ast.ForInStmt {
				if uses_return_stmt(stmt.stmts) {
					return true
				}
			}
			else {}
		}
	}
	return false
}
