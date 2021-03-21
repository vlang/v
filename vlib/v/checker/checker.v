// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
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

const int_min = int(0x80000000)

const int_max = int(0x7FFFFFFF)

const (
	valid_comp_if_os        = ['windows', 'ios', 'macos', 'mach', 'darwin', 'hpux', 'gnu', 'qnx',
		'linux', 'freebsd', 'openbsd', 'netbsd', 'bsd', 'dragonfly', 'android', 'solaris', 'haiku',
		'linux_or_macos',
	]
	valid_comp_if_compilers = ['gcc', 'tinyc', 'clang', 'mingw', 'msvc', 'cplusplus']
	valid_comp_if_platforms = ['amd64', 'aarch64', 'x64', 'x32', 'little_endian', 'big_endian']
	valid_comp_if_other     = ['js', 'debug', 'prod', 'test', 'glibc', 'prealloc', 'no_bounds_checking']
	array_builtin_methods   = ['filter', 'clone', 'repeat', 'reverse', 'map', 'slice', 'sort',
		'contains', 'index', 'wait', 'any', 'all', 'first', 'last', 'pop']
)

pub struct Checker {
	pref &pref.Preferences // Preferences shared from V struct
pub mut:
	table            &table.Table
	file             &ast.File = 0
	nr_errors        int
	nr_warnings      int
	errors           []errors.Error
	warnings         []errors.Warning
	error_lines      []int // to avoid printing multiple errors for the same line
	expected_type    table.Type
	expected_or_type table.Type  // fn() or { 'this type' } eg. string. expected or block type
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
	cur_generic_types []table.Type
mut:
	files                            []ast.File
	expr_level                       int  // to avoid infinite recursion segfaults due to compiler bugs
	inside_sql                       bool // to handle sql table fields pseudo variables
	cur_orm_ts                       table.TypeSymbol
	error_details                    []string
	vmod_file_content                string       // needed for @VMOD_FILE, contents of the file, *NOT its path**
	vweb_gen_types                   []table.Type // vweb route checks
	prevent_sum_type_unwrapping_once bool   // needed for assign new values to sum type, stopping unwrapping then
	loop_label                       string // set when inside a labelled for loop
	timers                           &util.Timers = util.new_timers(false)
	comptime_fields_type             map[string]table.Type
	fn_scope                         &ast.Scope = voidptr(0)
	used_fns                         map[string]bool // used_fns['println'] == true
	main_fn_decl_node                ast.FnDecl
	match_exhaustive_cutoff_limit    int = 10
	// TODO: these are here temporarily and used for deprecations; remove soon
	using_new_err_struct bool
	inside_selector_expr bool
	inside_println_arg   bool
}

pub fn new_checker(table &table.Table, pref &pref.Preferences) Checker {
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
				return_type: table.void_type
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
				if stmt.return_type != table.void_type {
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
		orig_sym := c.table.get_type_symbol((typ_sym.info as table.Alias).parent_type)
		c.error('type `$typ_sym.str()` is an alias, use the original alias type `$orig_sym.name` instead',
			node.pos)
	} else if typ_sym.kind == .chan {
		c.error('aliases of `chan` types are not allowed.', node.pos)
	}
}

pub fn (mut c Checker) fn_type_decl(node ast.FnTypeDecl) {
	c.check_valid_pascal_case(node.name, 'fn type', node.pos)
	typ_sym := c.table.get_type_symbol(node.typ)
	fn_typ_info := typ_sym.info as table.FnType
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
		if method.return_type != table.Type(0) {
			c.ensure_type_exists(method.return_type, method.pos) or { return }
		}
		for param in method.params {
			c.ensure_type_exists(param.typ, param.pos) or { return }
		}
	}
	for i, field in decl.fields {
		c.check_valid_snake_case(field.name, 'field name', field.pos)
		c.ensure_type_exists(field.typ, field.pos) or { return }
		for j in 0 .. i {
			if field.name == decl.fields[j].name {
				c.error('field name `$field.name` duplicate', field.pos)
			}
		}
	}
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

pub fn (mut c Checker) infix_expr(mut infix_expr ast.InfixExpr) table.Type {
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
		if left_type == table.voidptr_type {
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
	// Single side check
	// Place these branches according to ops' usage frequency to accelerate.
	// TODO: First branch includes ops where single side check is not needed, or needed but hasn't been implemented.
	// TODO: Some of the checks are not single side. Should find a better way to organize them.
	match infix_expr.op {
		// .eq, .ne, .gt, .lt, .ge, .le, .and, .logical_or, .dot, .key_as, .right_shift {}
		.eq, .ne {
			is_mismatch := (left.kind == .alias && right.kind in [.struct_, .array])
				|| (right.kind == .alias && left.kind in [.struct_, .array])
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
			return table.bool_type
		}
		.plus, .minus, .mul, .div, .mod, .xor, .amp, .pipe { // binary operators that expect matching types
			if right.info is table.Alias
				&& (right.info as table.Alias).language != .c && c.mod == c.table.type_to_str(right_type).split('.')[0] {
				right = c.table.get_type_symbol((right.info as table.Alias).parent_type)
			}
			if left.info is table.Alias
				&& (left.info as table.Alias).language != .c && c.mod == c.table.type_to_str(left_type).split('.')[0] {
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
				if promoted_type.idx() == table.void_type_idx {
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
					return table.void_type
				}
				// the expressions have different types (array_x and x)
				if c.check_types(right_type, left_value_type) { // , right_type) {
					// []T << T
					return table.void_type
				}
				if right_final.kind == .array
					&& c.check_types(left_value_type, c.table.value_type(right_type)) {
					// []T << []T
					return table.void_type
				}
				c.error('cannot append `$right.name` to `$left.name`', right_pos)
				return table.void_type
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
				ast.Type {
					right_expr.typ
				}
				ast.None {
					table.none_type_idx
				}
				else {
					c.error('invalid type `$right_expr`', right_expr.position())
					table.Type(0)
				}
			}
			typ_sym := c.table.get_type_symbol(typ)
			op := infix_expr.op.str()
			if typ_sym.kind == .placeholder {
				c.error('$op: type `$typ_sym.name` does not exist', right_expr.position())
			}
			if left.kind !in [.interface_, .sum_type] {
				c.error('`$op` can only be used with interfaces and sum types', infix_expr.pos)
			} else if mut left.info is table.SumType {
				if typ !in left.info.variants {
					c.error('`$left.name` has no variant `$right.name`', infix_expr.pos)
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
					c.error('cannot push non-reference `$right.name` on `$left.name`',
						right_pos)
				}
				c.stmts(infix_expr.or_block.stmts)
			} else {
				c.error('cannot push on non-channel `$left.name`', left_pos)
			}
			return table.void_type
		}
		.and, .logical_or {
			if infix_expr.left_type != table.bool_type_idx {
				c.error('left operand for `$infix_expr.op` is not a boolean', infix_expr.left.position())
			}
			if infix_expr.right_type != table.bool_type_idx {
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
	if left_type == table.bool_type && infix_expr.op !in [.eq, .ne, .logical_or, .and] {
		c.error('bool types only have the following operators defined: `==`, `!=`, `||`, and `&&`',
			infix_expr.pos)
	} else if left_type == table.string_type
		&& infix_expr.op !in [.plus, .eq, .ne, .lt, .gt, .le, .ge] {
		// TODO broken !in
		c.error('string types only have the following operators defined: `==`, `!=`, `<`, `>`, `<=`, `>=`, and `+`',
			infix_expr.pos)
	} else if left.kind == .enum_ && right.kind == .enum_ && infix_expr.op !in [.ne, .eq] {
		left_enum := left.info as table.Enum
		right_enum := right.info as table.Enum
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
	// sum types can't have any infix operation except of "is", is is checked before and doesn't reach this
	if c.table.type_kind(left_type) == .sum_type {
		c.error('cannot use operator `$infix_expr.op` with `$left.name`', infix_expr.pos)
	} else if c.table.type_kind(right_type) == .sum_type {
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
		if left_type == table.void_type || right_type == table.void_type {
			return table.void_type
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
	return if infix_expr.op.is_relational() { table.bool_type } else { return_type }
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
			mut elem_type := table.Type(0)
			mut kind := ''
			match left_sym.info {
				table.Array {
					elem_type, kind = left_sym.info.elem_type, 'array'
				}
				table.ArrayFixed {
					elem_type, kind = left_sym.info.elem_type, 'fixed array'
				}
				table.Map {
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
			// retrieve table.Field
			c.ensure_type_exists(expr.expr_type, expr.pos) or { return '', pos }
			mut typ_sym := c.table.get_final_type_symbol(c.unwrap_generic(expr.expr_type))
			match typ_sym.kind {
				.struct_ {
					struct_info := typ_sym.info as table.Struct
					mut has_field := true
					mut field_info := struct_info.find_field(expr.field_name) or {
						has_field = false
						table.Field{}
					}
					if !has_field {
						for embed in struct_info.embeds {
							embed_sym := c.table.get_type_symbol(embed)
							embed_struct_info := embed_sym.info as table.Struct
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
					interface_info := typ_sym.info as table.Interface
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

fn (mut c Checker) type_implements(typ table.Type, inter_typ table.Type, pos token.Position) bool {
	$if debug_interface_type_implements ? {
		eprintln('> type_implements typ: $typ.debug() | inter_typ: $inter_typ.debug()')
	}
	utyp := c.unwrap_generic(typ)
	typ_sym := c.table.get_type_symbol(utyp)
	mut inter_sym := c.table.get_type_symbol(inter_typ)
	// do not check the same type more than once
	if mut inter_sym.info is table.Interface {
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
	if inter_typ.idx() == table.error_type_idx && utyp.idx() == table.none_type_idx {
		// `none` "implements" the Error interface
		return true
	}
	if typ_sym.kind == .interface_ && inter_sym.kind == .interface_ {
		c.error('cannot implement interface `$inter_sym.name` with a different interface `$styp`',
			pos)
	}
	imethods := if inter_sym.kind == .interface_ {
		(inter_sym.info as table.Interface).methods
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
	if mut inter_sym.info is table.Interface {
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
pub fn (mut c Checker) check_expr_opt_call(expr ast.Expr, ret_type table.Type) table.Type {
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
				c.expected_type = ret_type
				c.expected_or_type = ret_type.clear_flag(.optional)
				last_stmt_typ := c.expr(last_stmt.expr)
				c.expected_or_type = table.void_type
				type_fits := c.check_types(last_stmt_typ, ret_type)
					&& last_stmt_typ.nr_muls() == ret_type.nr_muls()
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
				name_type = table.Type(c.table.find_type_idx(name)).set_flag(.generic)
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
		return table.string_type
	}
	//
	old_selector_expr := c.inside_selector_expr
	c.inside_selector_expr = true
	typ := c.expr(selector_expr.expr)
	c.inside_selector_expr = old_selector_expr
	//
	c.using_new_err_struct = using_new_err_struct_save
	if typ == table.void_type_idx {
		c.error('unknown selector expression', selector_expr.pos)
		return table.void_type
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
			selector_expr.typ = table.int_type
			return table.int_type
		}
		if sym.kind == .chan && field_name == 'closed' {
			selector_expr.typ = table.bool_type
			return table.bool_type
		}
	}
	mut unknown_field_msg := 'type `$sym.name` has no field or method `$field_name`'
	mut has_field := false
	mut field := table.Field{}
	if field_name.len > 0 && field_name[0].is_capital() && sym.info is table.Struct
		&& sym.language == .v {
		// x.Foo.y => access the embedded struct
		sym_info := sym.info as table.Struct
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
			if sym.info is table.Struct {
				mut found_fields := []table.Field{}
				mut embed_of_found_fields := []table.Type{}
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
			if sym.info is table.Struct {
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
		if sym.info is table.Struct {
			suggestion := util.new_suggestion(field_name, sym.info.fields.map(it.name))
			c.error(suggestion.say(unknown_field_msg), selector_expr.pos)
		}
		c.error(unknown_field_msg, selector_expr.pos)
	}
	return table.void_type
}

// TODO: non deferred
pub fn (mut c Checker) return_stmt(mut return_stmt ast.Return) {
	c.expected_type = c.cur_fn.return_type
	expected_type := c.unwrap_generic(c.expected_type)
	expected_type_sym := c.table.get_type_symbol(expected_type)
	if return_stmt.exprs.len > 0 && c.cur_fn.return_type == table.void_type {
		c.error('unexpected argument, current function does not return anything', return_stmt.exprs[0].position())
		return
	} else if return_stmt.exprs.len == 0 && !(c.expected_type == table.void_type
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
	if expected_type_sym.info is table.MultiReturn {
		expected_types = expected_type_sym.info.types
		if c.cur_generic_types.len > 0 {
			expected_types = expected_types.map(c.unwrap_generic(it))
		}
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
	// allow `none` & `error` return types for function that returns optional
	if exp_is_optional
		&& got_types[0].idx() in [table.none_type_idx, table.error_type_idx, c.table.type_idxs['Option']] {
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
			&& (!got_typ.is_ptr() && !got_typ.is_pointer()) && got_typ != table.int_literal_type {
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
		mut typ := c.expr(field.expr)
		if field.expr is ast.CallExpr {
			if field.expr.or_block.kind != .absent {
				typ = typ.clear_flag(.optional)
			}
		}
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

fn scope_register_it(mut s ast.Scope, pos token.Position, typ table.Type) {
	s.register(ast.Var{
		name: 'it'
		pos: pos
		typ: typ
		is_used: true
	})
}

fn scope_register_a_b(mut s ast.Scope, pos token.Position, typ table.Type) {
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
	// c.expected_type = table.void_type
	match mut node {
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
			c.expected_type = table.void_type
			c.check_expr_opt_call(node.expr, table.void_type)
			// TODO This should work, even if it's prolly useless .-.
			// node.typ = c.check_expr_opt_call(node.expr, table.void_type)
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
	assert_type := c.expr(node.expr)
	if assert_type != table.bool_type_idx {
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
		for arg in template.args {
			c.asm_arg(arg, stmt, aliases)
		}
	}
	for clob in stmt.clobbered {
		c.asm_arg(clob.reg, stmt, aliases)
	}
}

fn (mut c Checker) asm_arg(arg ast.AsmArg, stmt ast.AsmStmt, aliases []string) {
	match mut arg {
		ast.AsmAlias {
			name := arg.name
			if name !in aliases && name !in stmt.local_labels && name !in stmt.global_labels {
				suggestion := util.new_suggestion(name, aliases)
				c.error(suggestion.say('alias or label `$arg.name` does not exist'), arg.pos)
			}
		}
		ast.AsmAddressing {
			if arg.scale !in [-1, 1, 2, 4, 8] {
				c.error('scale must be one of 1, 2, 4, or 8', arg.pos)
			}
			c.asm_arg(arg.base, stmt, aliases)
			c.asm_arg(arg.index, stmt, aliases)
		}
		ast.BoolLiteral {} // all of these are guarented to be correct.
		ast.FloatLiteral {}
		ast.CharLiteral {}
		ast.IntegerLiteral {}
		ast.AsmRegister {} // if the register is not found, the parser will register it as an alias
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
	c.expected_type = table.void_type
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
	c.expected_type = table.void_type
}

pub fn (c &Checker) unwrap_generic(typ table.Type) table.Type {
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
				return table.void_type
			}
			array_info := type_sym.info as table.Array
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
			return table.bool_type
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
			// return table.int_literal_type
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
			return c.comptime_call(mut node)
		}
		ast.ComptimeSelector {
			node.left_type = c.unwrap_generic(c.expr(node.left))
			expr_type := c.unwrap_generic(c.expr(node.field_expr))
			expr_sym := c.table.get_type_symbol(expr_type)
			if expr_type != table.string_type {
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
			return table.void_type
		}
		ast.ConcatExpr {
			return c.concat_expr(mut node)
		}
		ast.DumpExpr {
			node.expr_type = c.expr(node.expr)
			if node.expr_type.idx() == table.void_type_idx {
				c.error('dump expression can not be void', node.expr.position())
				return table.void_type
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
			return table.float_literal_type
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
			return table.bool_type
		}
		ast.IndexExpr {
			return c.index_expr(mut node)
		}
		ast.InfixExpr {
			return c.infix_expr(mut node)
		}
		ast.IntegerLiteral {
			return table.int_literal_type
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
			if !node.is_type {
				node.typ = c.expr(node.expr)
			}
			return table.u32_type
		}
		ast.OffsetOf {
			return c.offset_of(node)
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
			if node.unresolved {
				return c.expr(ast.resolve_init(node, c.unwrap_generic(node.typ), c.table))
			}
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
				c.error('`${lname}()` expects a boolean expression, instead it got `$ltype_sym.name`',
					node.pos)
			}
			return table.bool_type
		}
	}
	return table.void_type
}

// pub fn (mut c Checker) asm_reg(mut node ast.AsmRegister) table.Type {
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
// 	return table.void_type
// }

pub fn (mut c Checker) cast_expr(mut node ast.CastExpr) table.Type {
	node.expr_type = c.expr(node.expr) // type to be casted
	from_type_sym := c.table.get_type_symbol(node.expr_type)
	to_type_sym := c.table.get_type_symbol(node.typ) // type to be used as cast
	if to_type_sym.language != .c {
		c.ensure_type_exists(node.typ, node.pos) or {}
	}
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
	if to_type_sym.kind == .sum_type {
		if node.expr_type in [table.int_literal_type, table.float_literal_type] {
			node.expr_type = c.promote_num(node.expr_type, if node.expr_type == table.int_literal_type {
				table.int_type
			} else {
				table.f64_type
			})
		}
		if !c.table.sumtype_has_variant(node.typ, node.expr_type) {
			c.error('cannot cast `$from_type_sym.name` to `$to_type_sym.name`', node.pos)
		}
	} else if mut to_type_sym.info is table.Alias {
		if !c.check_types(node.expr_type, to_type_sym.info.parent_type) {
			parent_type_sym := c.table.get_type_symbol(to_type_sym.info.parent_type)
			c.error('cannot convert type `$from_type_sym.name` to `$to_type_sym.name` (alias to `$parent_type_sym.name`)',
				node.pos)
		}
	} else if node.typ == table.string_type
		&& (from_type_sym.kind in [.int_literal, .int, .byte, .byteptr, .bool]
		|| (from_type_sym.kind == .array && from_type_sym.name == 'array_byte')) {
		type_name := c.table.type_to_str(node.expr_type)
		c.error('cannot cast type `$type_name` to string, use `x.str()` instead', node.pos)
	} else if node.expr_type == table.string_type {
		if to_type_sym.kind != .alias {
			mut error_msg := 'cannot cast a string'
			if mut node.expr is ast.StringLiteral {
				if node.expr.val.len == 1 {
					error_msg += ", for denoting characters use `$node.expr.val` instead of '$node.expr.val'"
				}
			}
			c.error(error_msg, node.pos)
		}
	} else if to_type_sym.kind == .byte && node.expr_type != table.voidptr_type
		&& from_type_sym.kind != .enum_ && !node.expr_type.is_int() && !node.expr_type.is_float()
		&& !node.expr_type.is_ptr() {
		type_name := c.table.type_to_str(node.expr_type)
		c.error('cannot cast type `$type_name` to `byte`', node.pos)
	} else if to_type_sym.kind == .struct_ && !node.typ.is_ptr()
		&& !(to_type_sym.info as table.Struct).is_typedef {
		// For now we ignore C typedef because of `C.Window(C.None)` in vlib/clipboard
		if from_type_sym.kind == .struct_ && !node.expr_type.is_ptr() {
			c.warn('casting to struct is deprecated, use e.g. `Struct{...expr}` instead',
				node.pos)
			from_type_info := from_type_sym.info as table.Struct
			to_type_info := to_type_sym.info as table.Struct
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
	} else if node.typ == table.bool_type {
		c.error('cannot cast to bool - use e.g. `some_int != 0` instead', node.pos)
	} else if node.expr_type == table.none_type {
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

fn (mut c Checker) comptime_call(mut node ast.ComptimeCall) table.Type {
	node.sym = c.table.get_type_symbol(c.unwrap_generic(c.expr(node.left)))
	if node.is_env {
		env_value := util.resolve_env_value("\$env('$node.args_var')", false) or {
			c.error(err.msg, node.env_pos)
			return table.string_type
		}
		node.env_value = env_value
		return table.string_type
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
		c.nr_warnings += c2.nr_warnings
		c.nr_errors += c2.nr_errors
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
		return table.string_type
	}
	if node.is_vweb {
		return table.string_type
	}
	// s.$my_str()
	v := node.scope.find_var(node.method_name) or {
		c.error('unknown identifier `$node.method_name`', node.method_pos)
		return table.void_type
	}
	if v.typ != table.string_type {
		s := c.expected_msg(v.typ, table.string_type)
		c.error('invalid string method call: $s', node.method_pos)
		return table.void_type
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
		return table.void_type
	}
	// println(f.name + ' ' + c.table.type_to_str(f.return_type))
	node.result_type = f.return_type
	return f.return_type
}

fn (mut c Checker) at_expr(mut node ast.AtExpr) table.Type {
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
								return table.void_type
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
					if typ == table.error_type && c.expected_type == table.string_type
						&& !c.using_new_err_struct && !c.inside_selector_expr
						&& !c.inside_println_arg && 'v.' !in c.file.mod.name && !c.is_builtin_mod {
						//                          ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ <- TODO: remove; this prevents a failure in the `performance-regressions` CI job
						c.warn('string errors are deprecated; use `err.msg` instead',
							ident.pos)
					}
					// if typ == table.t_type {
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
		if ident.name == 'C.NULL' {
			return table.voidptr_type
		}
		return table.int_type
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
		if builtin_type != table.void_type {
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

// smartcast takes the expression with the current type which should be smartcasted to the target type in the given scope
fn (c &Checker) smartcast_sumtype(expr ast.Expr, cur_type table.Type, to_type table.Type, mut scope ast.Scope) {
	match mut expr {
		ast.SelectorExpr {
			mut is_mut := false
			mut sum_type_casts := []table.Type{}
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
			mut sum_type_casts := []table.Type{}
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

pub fn (mut c Checker) select_expr(mut node ast.SelectExpr) table.Type {
	node.is_expr = c.expected_type != table.void_type
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
	return table.bool_type
}

pub fn (mut c Checker) lock_expr(mut node ast.LockExpr) table.Type {
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
	mut ret_type := table.void_type
	if node.stmts.len > 0 {
		last_stmt := node.stmts[node.stmts.len - 1]
		if last_stmt is ast.ExprStmt {
			ret_type = last_stmt.typ
		}
	}
	if ret_type != table.void_type {
		node.is_expr = true
	}
	node.typ = ret_type
	return ret_type
}

pub fn (mut c Checker) unsafe_expr(mut node ast.UnsafeExpr) table.Type {
	assert !c.inside_unsafe
	c.inside_unsafe = true
	t := c.expr(node.expr)
	c.inside_unsafe = false
	return t
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
	mut expr := ast.Expr{}
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

pub fn (mut c Checker) postfix_expr(mut node ast.PostfixExpr) table.Type {
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

pub fn (mut c Checker) prefix_expr(mut node ast.PrefixExpr) table.Type {
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

fn (mut c Checker) check_index(typ_sym &table.TypeSymbol, index ast.Expr, index_type table.Type, pos token.Position, range_index bool) {
	index_type_sym := c.table.get_type_symbol(index_type)
	// println('index expr left=$typ_sym.name $node.pos.line_nr')
	// if typ_sym.kind == .array && (!(table.type_idx(index_type) in table.number_type_idxs) &&
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
				info := typ_sym.info as table.ArrayFixed
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

pub fn (mut c Checker) index_expr(mut node ast.IndexExpr) table.Type {
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
		&& typ !in [table.byteptr_type, table.charptr_type] && !typ.has_flag(.variadic) {
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
			typ = table.new_type(idx)
		} else {
			typ = typ.set_nr_muls(0)
		}
	} else { // [1]
		index_type := c.expr(node.index)
		if typ_sym.kind == .map {
			info := typ_sym.info as table.Map
			if !c.check_types(index_type, info.key_type) {
				err := c.expected_msg(index_type, info.key_type)
				c.error('invalid key: $err', node.pos)
			}
		} else {
			c.check_index(typ_sym, node.index, index_type, node.pos, false)
		}
		value_type := c.table.value_type(typ)
		if value_type != table.void_type {
			typ = value_type
		}
	}
	c.stmts(node.or_expr.stmts)
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
	// println('tname=$typ_sym.name $node.pos.line_nr $c.file.path')
	if typ_sym.kind == .array && node.enum_name.len == 0 {
		array_info := typ_sym.info as table.Array
		typ = array_info.elem_type
		typ_sym = c.table.get_type_symbol(typ)
	}
	if typ_sym.kind != .enum_ && !c.pref.translated {
		// TODO in C int fields can be compared to enums, need to handle that in C2V
		c.error('expected type is not an enum (`$typ_sym.name`)', node.pos)
		return table.void_type
	}
	if typ_sym.info !is table.Enum {
		c.error('not an enum', node.pos)
		return table.void_type
	}
	// info := typ_sym.info as table.Enum
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

pub fn (mut c Checker) offset_of(node ast.OffsetOf) table.Type {
	sym := c.table.get_final_type_symbol(node.struct_type)
	if sym.kind != .struct_ {
		c.error('first argument of __offsetof must be struct', node.pos)
		return table.u32_type
	}
	if !c.table.struct_has_field(sym, node.field) {
		c.error('struct `$sym.name` has no field called `$node.field`', node.pos)
	}
	return table.u32_type
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

// call this *before* calling error or warn
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
	msg := message.replace('`Array_', '`[]')
	c.warn_or_error(msg, pos, false)
}

// check `to` has all fields of `from`
fn (c &Checker) check_struct_signature(from table.Struct, to table.Struct) bool {
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

fn (mut c Checker) sql_expr(mut node ast.SqlExpr) table.Type {
	c.inside_sql = true
	defer {
		c.inside_sql = false
	}
	sym := c.table.get_type_symbol(node.table_expr.typ)
	c.ensure_type_exists(node.table_expr.typ, node.pos) or { return table.void_type }
	c.cur_orm_ts = sym
	info := sym.info as table.Struct
	fields := c.fetch_and_verify_orm_fields(info, node.table_expr.pos, sym.name)
	mut sub_structs := map[int]ast.SqlExpr{}
	for f in fields.filter(c.table.type_symbols[int(it.typ)].kind == .struct_) {
		mut n := ast.SqlExpr{
			pos: node.pos
			has_where: true
			typ: f.typ
			db_expr: node.db_expr
			table_expr: ast.Type{
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
					typ: table.int_type
				}
			}
			left_type: table.int_type
			right_type: table.int_type
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

fn (mut c Checker) sql_stmt(mut node ast.SqlStmt) table.Type {
	c.inside_sql = true
	defer {
		c.inside_sql = false
	}
	c.ensure_type_exists(node.table_expr.typ, node.pos) or { return table.void_type }
	table_sym := c.table.get_type_symbol(node.table_expr.typ)
	c.cur_orm_ts = table_sym
	info := table_sym.info as table.Struct
	fields := c.fetch_and_verify_orm_fields(info, node.table_expr.pos, table_sym.name)
	mut sub_structs := map[int]ast.SqlStmt{}
	for f in fields.filter(c.table.type_symbols[int(it.typ)].kind == .struct_) {
		mut n := ast.SqlStmt{
			pos: node.pos
			db_expr: node.db_expr
			kind: node.kind
			table_expr: ast.Type{
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
	c.expr(node.where_expr)

	return table.void_type
}

fn (mut c Checker) fetch_and_verify_orm_fields(info table.Struct, pos token.Position, table_name string) []table.Field {
	fields := info.fields.filter((it.typ in [table.string_type, table.int_type, table.bool_type]
		|| c.table.type_symbols[int(it.typ)].kind == .struct_) && !it.attrs.contains('skip'))
	if fields.len == 0 {
		c.error('V orm: select: empty fields in `$table_name`', pos)
		return []table.Field{}
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

fn (mut c Checker) ensure_type_exists(typ table.Type, pos token.Position) ? {
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
			c.ensure_type_exists((sym.info as table.Array).elem_type, pos) ?
		}
		.map {
			info := sym.info as table.Map
			c.ensure_type_exists(info.key_type, pos) ?
			c.ensure_type_exists(info.value_type, pos) ?
		}
		else {}
	}
}
