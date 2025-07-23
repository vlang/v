// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import os
import strconv
import v.ast
import v.vmod
import v.token
import v.pref
import v.util
import v.util.version
import v.errors
import v.pkgconfig
import v.transformer
import v.type_resolver

// prevent stack overflows by restricting too deep recursion:
const expr_level_cutoff_limit = 40
const stmt_level_cutoff_limit = 40
const type_level_cutoff_limit = 40 // it is very rarely deeper than 4
const iface_level_cutoff_limit = 100
const generic_fn_cutoff_limit_per_fn = 10_000 // how many times post_process_generic_fns, can visit the same function before bailing out

const generic_fn_postprocess_iterations_cutoff_limit = 1_000_000

// array_builtin_methods contains a list of all methods on array, that return other typed arrays.
// i.e. that act as *pseudogeneric* methods, that need compiler support, so that the types of the results
// are properly checked.
// Note that methods that do not return anything, or that return known types, are not listed here, since they are just ordinary non generic methods.
pub const array_builtin_methods = ['filter', 'clone', 'repeat', 'reverse', 'map', 'slice', 'sort',
	'sort_with_compare', 'sorted', 'sorted_with_compare', 'contains', 'index', 'wait', 'any', 'all',
	'first', 'last', 'pop', 'delete', 'insert', 'prepend', 'count']
pub const array_builtin_methods_chk = token.new_keywords_matcher_from_array_trie(array_builtin_methods)
pub const fixed_array_builtin_methods = ['contains', 'index', 'any', 'all', 'wait', 'map', 'sort',
	'sorted', 'sort_with_compare', 'sorted_with_compare', 'reverse', 'reverse_in_place', 'count']
pub const fixed_array_builtin_methods_chk = token.new_keywords_matcher_from_array_trie(fixed_array_builtin_methods)
// TODO: remove `byte` from this list when it is no longer supported
pub const reserved_type_names = ['bool', 'char', 'i8', 'i16', 'int', 'i64', 'u8', 'u16', 'u32',
	'u64', 'f32', 'f64', 'map', 'string', 'rune', 'usize', 'isize', 'voidptr', 'thread']
pub const reserved_type_names_chk = token.new_keywords_matcher_from_array_trie(reserved_type_names)
pub const vroot_is_deprecated_message = '@VROOT is deprecated, use @VMODROOT or @VEXEROOT instead'

@[heap; minify]
pub struct Checker {
pub mut:
	pref &pref.Preferences = unsafe { nil } // Preferences shared from V struct

	table &ast.Table = unsafe { nil }
	file  &ast.File  = unsafe { nil }

	nr_errors     int
	nr_warnings   int
	nr_notices    int
	errors        []errors.Error
	warnings      []errors.Warning
	notices       []errors.Notice
	error_lines   map[string]bool // dedup errors
	warning_lines map[string]bool // dedup warns
	notice_lines  map[string]bool // dedup notices
	error_details []string
	should_abort  bool // when too many errors/warnings/notices are accumulated, .should_abort becomes true. It is checked in statement/expression loops, so the checker can return early, instead of wasting time.

	expected_type               ast.Type
	expected_or_type            ast.Type // fn() or { 'this type' } eg. string. expected or block type
	expected_expr_type          ast.Type // if/match is_expr: expected_type
	mod                         string   // current module name
	const_var                   &ast.ConstField = unsafe { nil } // the current constant, when checking const declarations
	const_deps                  []string
	const_names                 []string
	global_names                []string
	locked_names                []string // vars that are currently locked
	rlocked_names               []string // vars that are currently read-locked
	in_for_count                int      // if checker is currently in a for loop
	returns                     bool
	scope_returns               bool
	is_builtin_mod              bool        // true inside the 'builtin', 'os' or 'strconv' modules; TODO: remove the need for special casing this
	is_just_builtin_mod         bool        // true only inside 'builtin'
	is_generated                bool        // true for `@[generated] module xyz` .v files
	unresolved_fixed_sizes      []&ast.Stmt // funcs with unresolved array fixed size e.g. fn func() [const1]int
	inside_recheck              bool        // true when rechecking rhs assign statement
	inside_unsafe               bool        // true inside `unsafe {}` blocks
	inside_const                bool        // true inside `const ( ... )` blocks
	inside_anon_fn              bool        // true inside `fn() { ... }()`
	inside_lambda               bool        // true inside `|...| ...`
	inside_ref_lit              bool        // true inside `a := &something`
	inside_defer                bool        // true inside `defer {}` blocks
	inside_return               bool        // true inside `return ...` blocks
	inside_fn_arg               bool        // `a`, `b` in `a.f(b)`
	inside_ct_attr              bool        // true inside `[if expr]`
	inside_x_is_type            bool        // true inside the Type expression of `if x is Type {`
	inside_x_matches_type       bool        // true inside the match branch of `match x.type { Type {} }`
	anon_struct_should_be_mut   bool        // true when `mut var := struct { ... }` is used
	inside_generic_struct_init  bool
	inside_integer_literal_cast bool // true inside `int(123)`
	cur_struct_generic_types    []ast.Type
	cur_struct_concrete_types   []ast.Type
	skip_flags                  bool      // should `#flag` and `#include` be skipped
	fn_level                    int       // 0 for the top level, 1 for `fn abc() {}`, 2 for a nested fn, etc
	smartcast_mut_pos           token.Pos // match mut foo, if mut foo is Foo
	smartcast_cond_pos          token.Pos // match cond
	ct_cond_stack               []ast.Expr
	ct_user_defines             map[string]ComptimeBranchSkipState
	ct_system_defines           map[string]ComptimeBranchSkipState
mut:
	stmt_level int // the nesting level inside each stmts list;
	// .stmt_level is used to check for `evaluated but not used` ExprStmts like `1 << 1`
	// 1 for statements directly at each inner scope level;
	// increases for `x := if cond { statement_list1} else {statement_list2}`;
	// increases for `x := optfn() or { statement_list3 }`;
	// files                            []ast.File
	expr_level                       int // to avoid infinite recursion segfaults due to compiler bugs
	type_level                       int // to avoid infinite recursion segfaults due to compiler bugs in ensure_type_exists
	ensure_generic_type_level        int // to avoid infinite recursion segfaults in ensure_generic_type_specify_type_names
	cur_orm_ts                       ast.TypeSymbol
	cur_anon_fn                      &ast.AnonFn = unsafe { nil }
	vmod_file_content                string     // needed for @VMOD_FILE, contents of the file, *NOT its path**
	loop_labels                      []string   // filled, when inside labelled for loops: `a_label: for x in 0..10 {`
	vweb_gen_types                   []ast.Type // vweb route checks
	timers                           &util.Timers = util.get_timers()
	type_resolver                    type_resolver.TypeResolver
	comptime                         &type_resolver.ResolverInfo = unsafe { nil }
	fn_scope                         &ast.Scope                  = unsafe { nil }
	main_fn_decl_node                ast.FnDecl
	match_exhaustive_cutoff_limit    int = 10
	is_last_stmt                     bool
	prevent_sum_type_unwrapping_once bool // needed for assign new values to sum type, stopping unwrapping then
	using_new_err_struct             bool
	need_recheck_generic_fns         bool            // need recheck generic fns because there are cascaded nested generic fn
	generic_fns                      map[string]bool // register generic fns that needs recheck once
	inside_sql                       bool            // to handle sql table fields pseudo variables
	inside_selector_expr             bool
	inside_or_block_value            bool // true inside or-block where its value is used `f(g() or { true })`
	inside_interface_deref           bool
	inside_decl_rhs                  bool
	inside_if_guard                  bool // true inside the guard condition of `if x := opt() {}`
	inside_assign                    bool
	// doing_line_info                  int    // a quick single file run when called with v -line-info (contains line nr to inspect)
	// doing_line_path                  string // same, but stores the path being parsed
	is_index_assign   bool
	comptime_call_pos int                      // needed for correctly checking use before decl for templates
	goto_labels       map[string]ast.GotoLabel // to check for unused goto labels
	enum_data_type    ast.Type
	field_data_type   ast.Type
	variant_data_type ast.Type
	fn_return_type    ast.Type
	orm_table_fields  map[string][]ast.StructField // known table structs

	v_current_commit_hash string // same as old C.V_CURRENT_COMMIT_HASH
	assign_stmt_attr      string // for `x := [1,2,3] @[freed]`

	js_string ast.Type = ast.void_type // when `js"string literal"` is used, `js_string` will be equal to `JS.String`
}

pub fn new_checker(table &ast.Table, pref_ &pref.Preferences) &Checker {
	mut timers_should_print := false
	$if time_checking ? {
		timers_should_print = true
	}
	v_current_commit_hash := if pref_.building_v {
		version.githash(pref_.vroot) or { vcurrent_hash() }
	} else {
		vcurrent_hash()
	}
	mut checker := &Checker{
		table:                         table
		pref:                          pref_
		timers:                        util.new_timers(
			should_print: timers_should_print
			label:        'checker'
		)
		match_exhaustive_cutoff_limit: pref_.checker_match_exhaustive_cutoff_limit
		v_current_commit_hash:         v_current_commit_hash
	}
	checker.type_resolver = type_resolver.TypeResolver.new(table, checker)
	checker.comptime = &checker.type_resolver.info
	return checker
}

fn (mut c Checker) reset_checker_state_at_start_of_new_file() {
	c.expected_type = ast.void_type
	c.expected_or_type = ast.void_type
	c.const_var = unsafe { nil }
	c.in_for_count = 0
	c.returns = false
	c.scope_returns = false
	c.mod = ''
	c.is_builtin_mod = false
	c.is_just_builtin_mod = false
	c.inside_unsafe = false
	c.inside_const = false
	c.inside_anon_fn = false
	c.inside_ref_lit = false
	c.inside_defer = false
	c.inside_fn_arg = false
	c.inside_ct_attr = false
	c.inside_x_is_type = false
	c.inside_x_matches_type = false
	c.inside_integer_literal_cast = false
	c.skip_flags = false
	c.fn_level = 0
	c.expr_level = 0
	c.stmt_level = 0
	c.inside_sql = false
	c.cur_orm_ts = ast.TypeSymbol{}
	c.prevent_sum_type_unwrapping_once = false
	c.loop_labels = []
	c.using_new_err_struct = false
	c.inside_selector_expr = false
	c.inside_interface_deref = false
	c.inside_decl_rhs = false
	c.inside_if_guard = false
	c.error_details.clear()
}

pub fn (mut c Checker) check(mut ast_file ast.File) {
	$if trace_checker ? {
		eprintln('start checking file: ${ast_file.path}')
	}
	c.reset_checker_state_at_start_of_new_file()
	c.change_current_file(ast_file)
	for i, ast_import in ast_file.imports {
		// Imports with the same path and name (self-imports and module name conflicts with builtin module imports)
		if c.mod == ast_import.mod {
			c.error('cannot import `${ast_import.mod}` into a module with the same name',
				ast_import.mod_pos)
		}
		// Duplicates of regular imports with the default alias (modname) and `as` imports with a custom alias
		if c.mod == ast_import.alias {
			if c.mod == ast_import.mod.all_after_last('.') {
				c.error('cannot import `${ast_import.mod}` into a module with the same name',
					ast_import.mod_pos)
			}
			c.error('cannot import `${ast_import.mod}` as `${ast_import.alias}` into a module with the same name',
				ast_import.alias_pos)
		}
		for sym in ast_import.syms {
			full_name := ast_import.mod + '.' + sym.name
			if full_name in c.const_names {
				c.error('cannot selectively import constant `${sym.name}` from `${ast_import.mod}`, import `${ast_import.mod}` and use `${full_name}` instead',
					sym.pos)
			}
		}

		cmp_mod_name := if ast_import.mod != ast_import.alias && ast_import.alias != '_' {
			ast_import.alias
		} else {
			ast_import.mod
		}
		for j in 0 .. i {
			if cmp_mod_name == if ast_file.imports[j].mod != ast_file.imports[j].alias
				&& ast_file.imports[j].alias != '_' {
				ast_file.imports[j].alias
			} else {
				ast_file.imports[j].mod
			} {
				c.error('A module `${cmp_mod_name}` was already imported on line ${
					ast_file.imports[j].mod_pos.line_nr + 1}`.', ast_import.mod_pos)
			}
		}
	}
	c.stmt_level = 0
	for mut stmt in ast_file.stmts {
		if stmt in [ast.ConstDecl, ast.ExprStmt] {
			c.expr_level = 0
			c.stmt(mut stmt)
		}
		if c.should_abort {
			return
		}
	}

	c.stmt_level = 0
	for mut stmt in ast_file.stmts {
		if stmt is ast.GlobalDecl {
			c.expr_level = 0
			c.stmt(mut stmt)
		}
		if c.should_abort {
			return
		}
	}

	c.stmt_level = 0
	for mut stmt in ast_file.stmts {
		if stmt !in [ast.ConstDecl, ast.GlobalDecl, ast.ExprStmt] {
			c.expr_level = 0
			c.stmt(mut stmt)
		}
		if c.should_abort {
			return
		}
	}

	c.check_scope_vars(c.file.scope)
	c.check_unused_labels()
}

pub fn (mut c Checker) check_scope_vars(sc &ast.Scope) {
	if !c.pref.is_repl && !c.file.is_test {
		for _, obj in sc.objects {
			match obj {
				ast.Var {
					if !obj.is_used && obj.name[0] != `_` {
						if !c.pref.translated && !c.file.is_translated {
							if obj.is_arg {
								if c.pref.show_unused_params {
									c.note('unused parameter: `${obj.name}`', obj.pos)
								}
							} else {
								c.warn('unused variable: `${obj.name}`', obj.pos)
							}
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
	}
	for child in sc.children {
		c.check_scope_vars(child)
	}
}

pub fn (mut c Checker) change_current_file(file &ast.File) {
	c.file = unsafe { file }
	c.vmod_file_content = ''
	c.mod = file.mod.name
	c.is_generated = file.is_generated
}

pub fn (mut c Checker) check_files(ast_files []&ast.File) {
	// println('check_files')
	// c.files = ast_files
	mut has_main_mod_file := false
	mut has_no_main_mod_file := false
	mut has_main_fn := false
	unsafe {
		mut files_from_main_module := []&ast.File{}
		for i in 0 .. ast_files.len {
			mut file := ast_files[i]
			c.timers.start('checker_check ${file.path}')
			c.check(mut file)
			if file.mod.name == 'no_main' {
				has_no_main_mod_file = true
			}
			if file.mod.name == 'main' {
				files_from_main_module << file
				has_main_mod_file = true
				if c.file_has_main_fn(file) {
					has_main_fn = true
				}
			}
			c.timers.show('checker_check ${file.path}')
		}
		if has_main_mod_file && !has_main_fn && files_from_main_module.len > 0 {
			if c.pref.is_script && !c.pref.is_test {
				// files_from_main_module contain preludes at the start
				mut the_main_file := files_from_main_module.last()
				the_main_file.stmts << ast.FnDecl{
					name:        'main.main'
					mod:         'main'
					is_main:     true
					file:        the_main_file.path
					return_type: ast.void_type
					scope:       &ast.Scope{
						parent: nil
					}
				}
				has_main_fn = true
			}
		}
	}
	c.timers.start('checker_post_process_generic_fns')
	last_file := c.file
	// post process generic functions. must be done after all files have been
	// checked, to ensure all generic calls are processed, as this information
	// is needed when the generic type is auto inferred from the call argument.
	// we may have to loop several times, if there were more concrete types found.
	mut post_process_generic_fns_iterations := 0
	post_process_iterations_loop: for post_process_generic_fns_iterations <= generic_fn_postprocess_iterations_cutoff_limit {
		$if trace_post_process_generic_fns_loop ? {
			eprintln('>>>>>>>>> recheck_generic_fns loop iteration: ${post_process_generic_fns_iterations}')
		}
		for file in ast_files {
			if file.generic_fns.len > 0 {
				$if trace_post_process_generic_fns_loop ? {
					eprintln('>> file.path: ${file.path:-40} | file.generic_fns:' +
						file.generic_fns.map(it.name).str())
				}
				c.change_current_file(file)
				c.post_process_generic_fns() or { break post_process_iterations_loop }
			}
		}
		if !c.need_recheck_generic_fns {
			break
		}
		c.need_recheck_generic_fns = false
		post_process_generic_fns_iterations++
	}
	$if trace_post_process_generic_fns_loop ? {
		eprintln('>>>>>>>>> recheck_generic_fns loop done, iteration: ${post_process_generic_fns_iterations}')
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
			c.error('a _test.v file should have *at least* one `test_` function', token.Pos{})
		}
	}
	// After the main checker run, run the line info check, print line info, and exit (if it's present)
	if !c.pref.linfo.is_running && c.pref.line_info != '' { //'' && c.pref.linfo.line_nr == 0 {
		// c.do_line_info(c.pref.line_info, ast_files)
		println('setting is_running=true,  pref.path=${c.pref.linfo.path} curdir' + os.getwd())
		c.pref.linfo.is_running = true
		for i, file in ast_files {
			// println(file.path)
			if file.path == c.pref.linfo.path {
				println('running c.check_files')
				c.check_files([ast_files[i]])
				exit(0)
			} else if file.path.starts_with('./') {
				// Maybe it's a "./foo.v", linfo.path has an absolute path
				abs_path := os.join_path(os.getwd(), file.path).replace('/./', '/') // TODO: join_path shouldn't have /./
				if abs_path == c.pref.linfo.path {
					c.check_files([ast_files[i]])
					exit(0)
				}
			}
		}
		println('failed to find file "${c.pref.linfo.path}"')
		exit(0)
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
	if has_no_main_mod_file {
		return
	}
	if !has_main_mod_file {
		c.error('project must include a `main` module or be a shared library (compile with `v -shared`)',
			token.Pos{})
	} else if !has_main_fn && !c.pref.is_o {
		c.error('function `main` must be declared in the main module', token.Pos{})
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
				c.error('only `main` can have the `@[console]` attribute', stmt.pos)
			}
		}
	}
	return has_main_fn
}

@[direct_array_access]
fn (mut c Checker) check_valid_snake_case(name string, identifier string, pos token.Pos) {
	if c.pref.translated || c.file.is_translated {
		return
	}
	if !c.pref.is_vweb && name.len > 1 && (name[0] == `_` || name.contains('._')) {
		c.error('${identifier} `${name}` cannot start with `_`', pos)
	}
	if util.contains_capital(name) {
		c.error('${identifier} `${name}` cannot contain uppercase letters, use snake_case instead',
			pos)
	}
}

fn stripped_name(name string) string {
	idx := name.last_index('.') or { -1 }
	return name[(idx + 1)..]
}

fn (mut c Checker) check_valid_pascal_case(name string, identifier string, pos token.Pos) {
	if c.pref.translated || c.file.is_translated {
		return
	}
	sname := stripped_name(name)
	if sname.len > 0 && !sname[0].is_capital() {
		c.error('${identifier} `${name}` must begin with capital letter', pos)
	}
}

fn (mut c Checker) type_decl(mut node ast.TypeDecl) {
	if node.typ == ast.invalid_type && (node is ast.AliasTypeDecl || node is ast.SumTypeDecl) {
		typ_desc := if node is ast.AliasTypeDecl { 'alias' } else { 'sum type' }
		c.error('cannot register ${typ_desc} `${node.name}`, another type with this name exists',
			node.pos)
		return
	}
	match mut node {
		ast.AliasTypeDecl { c.alias_type_decl(mut node) }
		ast.FnTypeDecl { c.fn_type_decl(mut node) }
		ast.SumTypeDecl { c.sum_type_decl(mut node) }
	}
}

fn (mut c Checker) alias_type_decl(mut node ast.AliasTypeDecl) {
	if c.file.mod.name != 'builtin' && !node.name.starts_with('C.') {
		c.check_valid_pascal_case(node.name, 'type alias', node.pos)
	}
	if !c.ensure_type_exists(node.parent_type, node.type_pos) {
		return
	}
	mut parent_typ_sym := c.table.sym(node.parent_type)
	if node.parent_type.has_flag(.result) {
		c.add_error_detail('Result types cannot be stored and have to be unwrapped immediately')
		c.error('cannot make an alias of Result type', node.type_pos)
	}
	match parent_typ_sym.kind {
		.placeholder, .int_literal, .float_literal, .any {
			c.error('unknown aliased type `${parent_typ_sym.name}`', node.type_pos)
		}
		.alias {
			orig_sym := c.table.sym((parent_typ_sym.info as ast.Alias).parent_type)
			if !node.name.starts_with('C.') {
				c.error('type `${parent_typ_sym.str()}` is an alias, use the original alias type `${orig_sym.name}` instead',
					node.type_pos)
			}
		}
		.chan {
			c.error('aliases of `chan` types are not allowed', node.type_pos)
		}
		.thread {
			c.error('aliases of `thread` types are not allowed', node.type_pos)
		}
		.multi_return {
			c.error('aliases of function multi return types are not allowed', node.type_pos)
		}
		.void {
			c.error('aliases of the void type are not allowed', node.type_pos)
		}
		.function {
			orig_sym := c.table.type_to_str(node.parent_type)
			c.error('type `${parent_typ_sym.str()}` is an alias, use the original alias type `${orig_sym}` instead',
				node.type_pos)
		}
		.struct {
			if mut parent_typ_sym.info is ast.Struct {
				// check if the generic param types have been defined
				for ct in parent_typ_sym.info.concrete_types {
					ct_sym := c.table.sym(ct)
					if ct_sym.kind == .placeholder {
						c.error('unknown type `${ct_sym.name}`', node.type_pos)
					}
				}

				if parent_typ_sym.info.is_generic && parent_typ_sym.info.concrete_types.len == 0 {
					c.error('${parent_typ_sym.name} type is generic struct, must specify the generic type names, e.g. ${parent_typ_sym.name}[int]',
						node.type_pos)
				}

				// check if embed types are struct
				for embed_type in parent_typ_sym.info.embeds {
					final_embed_sym := c.table.final_sym(embed_type)
					if final_embed_sym.kind != .struct {
						c.error('cannot embed non-struct `${c.table.sym(embed_type).name}`',
							node.type_pos)
					}
				}
				if parent_typ_sym.info.is_anon {
					for field in parent_typ_sym.info.fields {
						mut is_embed := false
						field_sym := c.table.sym(field.typ)
						if field_sym.info is ast.Alias {
							if c.table.sym(field_sym.info.parent_type).kind != .struct {
								c.error('cannot embed non-struct `${field_sym.name}`',
									field.type_pos)
								is_embed = true
							}
						}
						if !is_embed {
							c.check_valid_snake_case(field.name, 'field name', field.pos)
						}
					}
				}
			}
		}
		.array {
			c.check_alias_vs_element_type_of_parent(node, (parent_typ_sym.info as ast.Array).elem_type,
				'array')
		}
		.array_fixed {
			array_fixed_info := parent_typ_sym.info as ast.ArrayFixed
			if c.array_fixed_has_unresolved_size(array_fixed_info) {
				c.unresolved_fixed_sizes << &ast.TypeDecl(node)
			}
			c.check_alias_vs_element_type_of_parent(node, array_fixed_info.elem_type,
				'fixed array')
		}
		.map {
			info := parent_typ_sym.info as ast.Map
			c.check_alias_vs_element_type_of_parent(node, info.key_type, 'map key')
			c.check_alias_vs_element_type_of_parent(node, info.value_type, 'map value')
			c.markused_used_maps(c.table.used_features.used_maps == 0)
		}
		.sum_type {
			// TODO: decide whether the following should be allowed. Note that it currently works,
			// while `type Sum = int | Sum` is explicitly disallowed:
			// type Sum = int | Alias
			// type Alias = Sum
		}
		.none {
			c.error('cannot create a type alias of `none` as it is a value', node.type_pos)
		}
		// The rest of the parent symbol kinds are also allowed, since they are either primitive types,
		// that in turn do not allow recursion, or are abstract enough so that they can not be checked at comptime:
		else {
			c.check_any_type(node.parent_type, parent_typ_sym, node.type_pos)
		}
		/*
		.voidptr, .byteptr, .charptr {}
		.char, .rune, .bool {}
		.string, .enum, .none, .any {}
		.i8, .i16, .int, .i64, .isize {}
		.u8, .u16, .u32, .u64, .usize {}
		.f32, .f64 {}
		.interface {}
		.generic_inst {}
		.aggregate {}
		*/
	}
}

fn (mut c Checker) check_alias_vs_element_type_of_parent(node ast.AliasTypeDecl, element_type_of_parent ast.Type,
	label string) {
	if node.typ.idx() != element_type_of_parent.idx() {
		return
	}
	c.error('recursive declarations of aliases are not allowed - the alias `${node.name}` is used in the ${label}',
		node.type_pos)
}

fn (mut c Checker) check_any_type(typ ast.Type, sym &ast.TypeSymbol, pos token.Pos) {
	if sym.kind == .any && !typ.has_flag(.generic) && sym.language != .js
		&& c.file.mod.name != 'builtin' {
		c.error('cannot use type `any` here', pos)
	}
}

fn (mut c Checker) fn_type_decl(mut node ast.FnTypeDecl) {
	c.check_valid_pascal_case(node.name, 'fn type', node.pos)
	typ_sym := c.table.sym(node.typ)
	fn_typ_info := typ_sym.info as ast.FnType
	fn_info := fn_typ_info.func
	c.ensure_type_exists(fn_info.return_type, fn_info.return_type_pos)
	ret_sym := c.table.sym(fn_info.return_type)
	if ret_sym.kind == .placeholder {
		c.error('unknown type `${ret_sym.name}`', fn_info.return_type_pos)
	}
	for arg in fn_info.params {
		if !c.ensure_type_exists(arg.typ, arg.type_pos) {
			return
		}
		arg_sym := c.table.sym(arg.typ)
		if arg_sym.kind == .placeholder {
			c.error('unknown type `${arg_sym.name}`', arg.type_pos)
		}
	}
}

fn (mut c Checker) sum_type_decl(mut node ast.SumTypeDecl) {
	c.check_valid_pascal_case(node.name, 'sum type', node.pos)
	mut names_used := []string{}
	for variant in node.variants {
		c.ensure_type_exists(variant.typ, variant.pos)
		sym := c.table.sym(variant.typ)
		if variant.typ.is_ptr() || (sym.info is ast.Alias && sym.info.parent_type.is_ptr()) {
			variant_name := sym.name.all_after_last('.')
			lb, rb := if sym.kind == .struct { '{', '}' } else { '(', ')' }
			msg := if sym.info is ast.Alias && sym.info.parent_type.is_ptr() {
				'alias as non-reference type'
			} else {
				'the sum type with non-reference types'
			}
			c.add_error_detail('declare ${msg}: `${node.name} = ${variant_name} | ...`
and use a reference to the sum type instead: `var := &${node.name}(${variant_name}${lb}val${rb})`')
			c.error('sum type cannot hold a reference type', variant.pos)
		}
		variant_name := c.table.type_to_str(variant.typ)
		if variant_name in names_used {
			c.error('sum type ${node.name} cannot hold the type `${sym.name}` more than once',
				variant.pos)
		} else if sym.kind in [.placeholder, .int_literal, .float_literal] {
			c.error('unknown type `${sym.name}`', variant.pos)
		} else if sym.kind == .interface && sym.language != .js {
			c.error('sum type cannot hold an interface', variant.pos)
		} else if sym.kind == .struct && sym.language == .js {
			c.error('sum type cannot hold a JS struct', variant.pos)
		} else if sym.info is ast.Struct {
			if sym.info.is_generic {
				if !variant.typ.has_flag(.generic) {
					c.error('generic struct `${sym.name}` must specify generic type names, e.g. ${sym.name}[T]',
						variant.pos)
				}
				if node.generic_types.len == 0 {
					c.error('generic sumtype `${node.name}` must specify generic type names, e.g. ${node.name}[T]',
						node.name_pos)
				} else {
					for typ in sym.info.generic_types {
						if typ !in node.generic_types {
							sumtype_type_names := node.generic_types.map(c.table.type_to_str(it)).join(', ')
							generic_sumtype_name := '${node.name}[${sumtype_type_names}]'
							variant_type_names := sym.info.generic_types.map(c.table.type_to_str(it)).join(', ')
							generic_variant_name := '${sym.name}[${variant_type_names}]'
							c.error('generic type name `${c.table.sym(typ).name}` of generic struct `${generic_variant_name}` is not mentioned in sumtype `${generic_sumtype_name}`',
								variant.pos)
						}
					}
				}
			}
		} else if sym.info is ast.FnType {
			if sym.info.func.generic_names.len > 0 {
				if !variant.typ.has_flag(.generic) {
					c.error('generic fntype `${sym.name}` must specify generic type names, e.g. ${sym.name}[T]',
						variant.pos)
				}
				if node.generic_types.len == 0 {
					c.error('generic sumtype `${node.name}` must specify generic type names, e.g. ${node.name}[T]',
						node.name_pos)
				}
			}
			if c.table.sym(sym.info.func.return_type).name.ends_with('.${node.name}') {
				c.error('sum type `${node.name}` cannot be defined recursively', variant.pos)
			}
			for param in sym.info.func.params {
				if c.table.sym(param.typ).name.ends_with('.${node.name}') {
					c.error('sum type `${node.name}` cannot be defined recursively', variant.pos)
				}
			}
		} else if variant.typ.has_flag(.result) {
			c.error('sum type cannot hold a Result type', variant.pos)
		}
		c.check_any_type(variant.typ, sym, variant.pos)

		if sym.name.trim_string_left(sym.mod + '.') == node.name {
			c.error('sum type cannot hold itself', variant.pos)
		}
		names_used << variant_name
	}
}

fn (mut c Checker) expand_iface_embeds(idecl &ast.InterfaceDecl, level int, iface_embeds []ast.InterfaceEmbedding) []ast.InterfaceEmbedding {
	// eprintln('> expand_iface_embeds: idecl.name: $idecl.name | level: $level | iface_embeds.len: $iface_embeds.len')
	if level > iface_level_cutoff_limit {
		c.error('too many interface embedding levels: ${level}, for interface `${idecl.name}`',
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
			mut list := iface_decl.embeds.clone()
			if !iface_decl.are_embeds_expanded {
				list = c.expand_iface_embeds(idecl, level + 1, iface_decl.embeds)
				unsafe {
					c.table.interfaces[ie.typ].embeds = list
				}
				unsafe {
					c.table.interfaces[ie.typ].are_embeds_expanded = true
				}
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

// fail_if_immutable_to_mutable checks if there is a immutable reference on right-side of assignment for mutable var
fn (mut c Checker) fail_if_immutable_to_mutable(left_type ast.Type, right_type ast.Type, right ast.Expr) bool {
	match right {
		ast.Ident {
			if c.inside_unsafe || c.pref.translated || c.file.is_translated {
				return true
			}
			if right.obj is ast.Var {
				if left_type.is_ptr() && !right.is_mut() && right_type.is_ptr() {
					c.note('`${right.name}` is immutable, cannot have a mutable reference to an immutable object',
						right.pos)
					return false
				}
				if !right.obj.is_mut
					&& c.table.final_sym(right_type).kind in [.array, .array_fixed, .map] {
					c.note('left-side of assignment expects a mutable reference, but variable `${right.name}` is immutable, declare it with `mut` to make it mutable or clone it',
						right.pos)
					return false
				}
			}
		}
		ast.IfExpr {
			if c.inside_unsafe || c.pref.translated || c.file.is_translated {
				return true
			}
			for branch in right.branches {
				stmts := branch.stmts.filter(it is ast.ExprStmt)
				if stmts.len > 0 {
					last_expr := stmts.last() as ast.ExprStmt
					c.fail_if_immutable_to_mutable(left_type, right_type, last_expr.expr)
				}
			}
		}
		ast.MatchExpr {
			if c.inside_unsafe || c.pref.translated || c.file.is_translated {
				return true
			}
			for branch in right.branches {
				stmts := branch.stmts.filter(it is ast.ExprStmt)
				if stmts.len > 0 {
					last_expr := stmts.last() as ast.ExprStmt
					c.fail_if_immutable_to_mutable(left_type, right_type, last_expr.expr)
				}
			}
		}
		ast.StructInit {
			typ_sym := c.table.sym(right.typ)
			for init_field in right.init_fields {
				if field_info := c.table.find_field_with_embeds(typ_sym, init_field.name) {
					if field_info.is_mut {
						if init_field.expr is ast.Ident && !init_field.expr.is_mut()
							&& init_field.typ.is_ptr() {
							c.note('`${init_field.expr.name}` is immutable, cannot have a mutable reference to an immutable object',
								init_field.pos)
						} else if init_field.expr is ast.PrefixExpr {
							if init_field.expr.op == .amp && init_field.expr.right is ast.Ident
								&& !init_field.expr.right.is_mut() {
								c.note('`${init_field.expr.right.name}` is immutable, cannot have a mutable reference to an immutable object',
									init_field.expr.right.pos)
							}
						}
					}
				}
			}
		}
		else {
			return true
		}
	}
	return true
}

// returns name and position of variable that needs write lock
// also sets `is_changed` to true (TODO update the name to reflect this?)
fn (mut c Checker) fail_if_immutable(mut expr ast.Expr) (string, token.Pos) {
	mut to_lock := '' // name of variable that needs lock
	mut pos := token.Pos{} // and its position
	mut explicit_lock_needed := false
	match mut expr {
		ast.CastExpr {
			// TODO
			return '', expr.pos
		}
		ast.ComptimeSelector {
			mut expr_left := expr.left
			if mut expr.left is ast.Ident && expr.left.obj is ast.Var {
				c.fail_if_immutable(mut expr_left)
			}
			return '', expr.pos
		}
		ast.Ident {
			if mut expr.obj is ast.Var {
				if !expr.obj.is_mut && !c.pref.translated && !c.file.is_translated
					&& !c.inside_unsafe {
					if c.inside_anon_fn {
						c.error('the closure copy of `${expr.name}` is immutable, declare it with `mut` to make it mutable',
							expr.pos)
					} else {
						c.error('`${expr.name}` is immutable, declare it with `mut` to make it mutable',
							expr.pos)
					}
				}
				expr.obj.is_changed = true
				if expr.obj.typ.share() == .shared_t {
					if expr.name !in c.locked_names {
						if c.locked_names.len > 0 || c.rlocked_names.len > 0 {
							if expr.name in c.rlocked_names {
								c.error('${expr.name} has an `rlock` but needs a `lock`',
									expr.pos)
							} else {
								c.error('${expr.name} must be added to the `lock` list above',
									expr.pos)
							}
						}
						to_lock = expr.name
						pos = expr.pos
					}
				}
			} else if expr.obj is ast.ConstField && expr.name in c.const_names {
				if !c.pref.translated {
					// TODO: fix this in c2v, do not allow modification of all consts
					// in translated code
					c.error('cannot modify constant `${expr.name}`', expr.pos)
				}
			}
		}
		ast.IndexExpr {
			if expr.left_type == 0 {
				return to_lock, pos
			}
			left_sym := c.table.sym(expr.left_type)
			mut elem_type := ast.no_type
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
				c.error('you have to create a handle and `lock` it to modify `shared` ${kind} element',
					expr.left.pos().extend(expr.pos))
			}
			to_lock, pos = c.fail_if_immutable(mut expr.left)
		}
		ast.ParExpr {
			to_lock, pos = c.fail_if_immutable(mut expr.expr)
		}
		ast.PrefixExpr {
			if expr.op == .mul && expr.right is ast.Ident {
				// Do not fail if dereference is immutable:
				// `*x = foo()` doesn't modify `x`
			} else {
				to_lock, pos = c.fail_if_immutable(mut expr.right)
			}
		}
		ast.PostfixExpr {
			to_lock, pos = c.fail_if_immutable(mut expr.expr)
		}
		ast.SelectorExpr {
			if expr.expr_type == 0 {
				return '', expr.pos
			}
			// retrieve ast.Field
			if !c.ensure_type_exists(expr.expr_type, expr.pos) {
				return '', expr.pos
			}
			mut typ_sym := c.table.final_sym(c.unwrap_generic(expr.expr_type))
			match typ_sym.kind {
				.struct {
					mut has_field := true
					mut field_info := c.table.find_field_with_embeds(typ_sym, expr.field_name) or {
						has_field = false
						ast.StructField{}
					}
					if !has_field {
						type_str := c.table.type_to_str(expr.expr_type)
						c.error('unknown field `${type_str}.${expr.field_name}`', expr.pos)
						return '', expr.pos
					}
					if field_info.typ.has_flag(.shared_f) {
						expr_name := '${expr.expr}.${expr.field_name}'
						if expr_name !in c.locked_names {
							if c.locked_names.len > 0 || c.rlocked_names.len > 0 {
								if expr_name in c.rlocked_names {
									c.error('${expr_name} has an `rlock` but needs a `lock`',
										expr.pos)
								} else {
									c.error('${expr_name} must be added to the `lock` list above',
										expr.pos)
								}
								return '', expr.pos
							}
							to_lock = expr_name
							pos = expr.pos
						}
					} else {
						if !field_info.is_mut && !c.pref.translated && !c.file.is_translated {
							type_str := c.table.type_to_str(expr.expr_type)
							c.error('field `${expr.field_name}` of struct `${type_str}` is immutable',
								expr.pos)
						}
						to_lock, pos = c.fail_if_immutable(mut expr.expr)
					}
					if to_lock != '' {
						// No automatic lock for struct access
						explicit_lock_needed = true
					}
				}
				.interface {
					interface_info := typ_sym.info as ast.Interface
					mut field_info := interface_info.find_field(expr.field_name) or {
						type_str := c.table.type_to_str(expr.expr_type)
						c.error('unknown field `${type_str}.${expr.field_name}`', expr.pos)
						return '', expr.pos
					}
					if !field_info.is_mut {
						type_str := c.table.type_to_str(expr.expr_type)
						c.error('field `${expr.field_name}` of interface `${type_str}` is immutable',
							expr.pos)
						return '', expr.pos
					}
					c.fail_if_immutable(mut expr.expr)
				}
				.sum_type {
					sumtype_info := typ_sym.info as ast.SumType
					mut field_info := sumtype_info.find_sum_type_field(expr.field_name) or {
						type_str := c.table.type_to_str(expr.expr_type)
						c.error('unknown field `${type_str}.${expr.field_name}`', expr.pos)
						return '', expr.pos
					}
					if !field_info.is_mut {
						type_str := c.table.type_to_str(expr.expr_type)
						c.error('field `${expr.field_name}` of sumtype `${type_str}` is immutable',
							expr.pos)
						return '', expr.pos
					}
					c.fail_if_immutable(mut expr.expr)
				}
				.array, .string {
					// should only happen in `builtin` and unsafe blocks
					inside_builtin := c.file.mod.name == 'builtin'
					if !inside_builtin && !c.inside_unsafe {
						c.error('`${typ_sym.kind}` can not be modified', expr.pos)
						return '', expr.pos
					}
				}
				.aggregate, .placeholder {
					c.fail_if_immutable(mut expr.expr)
				}
				else {
					c.error('unexpected symbol `${typ_sym.kind}`', expr.pos)
					return '', expr.pos
				}
			}
		}
		ast.CallExpr {
			// TODO: should only work for builtin method
			if expr.name == 'slice' {
				to_lock, pos = c.fail_if_immutable(mut expr.left)
				if to_lock != '' {
					// No automatic lock for array slicing (yet(?))
					explicit_lock_needed = true
				}
			}
		}
		ast.ArrayInit {
			c.error('array literal can not be modified', expr.pos)
			return '', expr.pos
		}
		ast.StructInit {
			return '', expr.pos
		}
		ast.InfixExpr {
			return '', expr.pos
		}
		ast.IfExpr {
			for mut branch in expr.branches {
				mut last_expr := (branch.stmts.last() as ast.ExprStmt).expr
				c.fail_if_immutable(mut last_expr)
			}
			return '', expr.pos
		}
		ast.AsCast {
			to_lock, pos = c.fail_if_immutable(mut expr.expr)
		}
		else {
			if !expr.is_pure_literal() {
				c.error('unexpected expression `${expr.type_name()}`', expr.pos())
				return '', expr.pos()
			}
		}
	}
	if explicit_lock_needed {
		c.error('`${to_lock}` is `shared` and needs explicit lock for `${expr.type_name()}`',
			pos)
		to_lock = ''
	}
	return to_lock, pos
}

fn (mut c Checker) type_implements(typ ast.Type, interface_type ast.Type, pos token.Pos) bool {
	if typ == interface_type {
		return true
	}
	$if debug_interface_type_implements ? {
		eprintln('> type_implements typ: ${typ.debug()} (`${c.table.type_to_str(typ)}`) | inter_typ: ${interface_type.debug()} (`${c.table.type_to_str(interface_type)}`)')
	}
	utyp := c.unwrap_generic(typ)
	styp := c.table.type_to_str(utyp)
	typ_sym := c.table.sym(utyp)
	mut inter_sym := c.table.sym(interface_type)
	if !inter_sym.is_pub && inter_sym.mod !in [typ_sym.mod, c.mod] && typ_sym.mod != 'builtin' {
		c.error('`${styp}` cannot implement private interface `${inter_sym.name}` of other module',
			pos)
		return false
	}

	// small hack for JS.Any type. Since `any` in regular V is getting deprecated we have our own JS.Any type for JS backend.
	if typ_sym.name == 'JS.Any' {
		return true
	}
	if mut inter_sym.info is ast.Interface {
		mut generic_type := interface_type
		mut generic_info := inter_sym.info
		if inter_sym.info.parent_type.has_flag(.generic) {
			parent_sym := c.table.sym(inter_sym.info.parent_type)
			if parent_sym.info is ast.Interface {
				generic_type = inter_sym.info.parent_type
				generic_info = parent_sym.info
			}
		}
		mut inferred_type := interface_type
		if generic_info.is_generic {
			inferred_type = c.unwrap_generic_interface(typ, generic_type, pos)
			if inferred_type == 0 {
				return false
			}
		}
		if inter_sym.info.is_generic {
			if inferred_type == interface_type {
				// terminate early, since otherwise we get an infinite recursion/segfault:
				return false
			}
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
	if utyp.idx() == interface_type.idx() {
		// same type -> already casted to the interface
		return true
	}
	if interface_type.idx() == ast.error_type_idx && utyp.idx() == ast.none_type_idx {
		// `none` "implements" the Error interface
		return true
	}
	if typ_sym.kind == .interface && inter_sym.kind == .interface && !styp.starts_with('JS.')
		&& !inter_sym.name.starts_with('JS.') {
		c.error('cannot implement interface `${inter_sym.name}` with a different interface `${styp}`',
			pos)
	}
	imethods := if inter_sym.kind == .interface {
		(inter_sym.info as ast.Interface).methods
	} else {
		inter_sym.methods
	}
	// voidptr is an escape hatch, it should be allowed to be passed
	if utyp != ast.voidptr_type && utyp != ast.nil_type && !(interface_type.has_flag(.option)
		&& utyp == ast.none_type) {
		mut are_methods_implemented := true

		// Verify methods
		for imethod in imethods {
			method := c.table.find_method_with_embeds(typ_sym, imethod.name) or {
				typ_sym.find_method_with_generic_parent(imethod.name) or {
					c.error("`${styp}` doesn't implement method `${imethod.name}` of interface `${inter_sym.name}`",
						pos)
					are_methods_implemented = false
					continue
				}
			}
			msg := c.table.is_same_method(imethod, method)
			if msg.len > 0 {
				sig := c.table.fn_signature(imethod, skip_receiver: false)
				typ_sig := c.table.fn_signature(method, skip_receiver: false)
				c.add_error_detail('${inter_sym.name} has `${sig}`')
				c.add_error_detail('         ${typ_sym.name} has `${typ_sig}`')
				c.error('`${styp}` incorrectly implements method `${imethod.name}` of interface `${inter_sym.name}`: ${msg}',
					pos)
				return false
			}
		}

		if !are_methods_implemented {
			return false
		}
	}
	// Verify fields
	if mut inter_sym.info is ast.Interface {
		for ifield in inter_sym.info.fields {
			if field := c.table.find_field_with_embeds(typ_sym, ifield.name) {
				if ifield.typ != field.typ {
					exp := c.table.type_to_str(ifield.typ)
					got := c.table.type_to_str(field.typ)
					c.error('`${styp}` incorrectly implements field `${ifield.name}` of interface `${inter_sym.name}`, expected `${exp}`, got `${got}`',
						pos)
					return false
				} else if ifield.is_mut && !(field.is_mut || field.is_global) {
					c.error('`${styp}` incorrectly implements interface `${inter_sym.name}`, field `${ifield.name}` must be mutable',
						pos)
					return false
				}
				continue
			}
			// voidptr is an escape hatch, it should be allowed to be passed
			if utyp != ast.voidptr_type && utyp != ast.nil_type {
				c.error("`${styp}` doesn't implement field `${ifield.name}` of interface `${inter_sym.name}`",
					pos)
			}
		}
		if utyp != ast.voidptr_type && utyp != ast.nil_type && utyp != ast.none_type
			&& !inter_sym.info.types.contains(utyp) {
			inter_sym.info.types << utyp
		}
		if !inter_sym.info.types.contains(ast.voidptr_type) {
			inter_sym.info.types << ast.voidptr_type
		}
	}
	return true
}

// helper for expr_or_block_err
fn is_field_to_description(expr_name string, is_field bool) string {
	return if is_field {
		'field `${expr_name}` is not'
	} else {
		'function `${expr_name}` does not return'
	}
}

fn (mut c Checker) expr_or_block_err(kind ast.OrKind, expr_name string, pos token.Pos, is_field bool) {
	match kind {
		.absent {
			// do nothing, most common case; do not be tempted to move the call to is_field_to_description above it, since that will slow it down
		}
		.block {
			obj_does_not_return_or_is_not := is_field_to_description(expr_name, is_field)
			c.error('unexpected `or` block, the ${obj_does_not_return_or_is_not} an Option or a Result',
				pos)
		}
		.propagate_option {
			obj_does_not_return_or_is_not := is_field_to_description(expr_name, is_field)
			c.error('unexpected `?`, the ${obj_does_not_return_or_is_not} an Option',
				pos)
		}
		.propagate_result {
			obj_does_not_return_or_is_not := is_field_to_description(expr_name, is_field)
			c.error('unexpected `!`, the ${obj_does_not_return_or_is_not} a Result', pos)
		}
	}
}

// return the actual type of the expression, once the result or option type is handled
fn (mut c Checker) check_expr_option_or_result_call(expr ast.Expr, ret_type ast.Type) ast.Type {
	match expr {
		ast.CallExpr {
			mut expr_ret_type := expr.return_type
			if expr_ret_type != 0 && c.table.sym(expr_ret_type).kind == .alias {
				unaliased_ret_type := c.table.unaliased_type(expr_ret_type)
				if unaliased_ret_type.has_option_or_result() {
					expr_ret_type = unaliased_ret_type
				}
			}
			// var with option function
			if expr.is_fn_var && expr.fn_var_type.has_option_or_result()
				&& expr.or_block.kind == .absent {
				ret_sym := c.table.sym(expr.fn_var_type)
				if expr.fn_var_type.has_flag(.option) {
					c.error('type `?${ret_sym.name}` is an Option, it must be unwrapped first',
						expr.pos)
				} else {
					c.error('type `?${ret_sym.name}` is an Result, it must be unwrapped first',
						expr.pos)
				}
			}
			if expr_ret_type.has_option_or_result() {
				if expr.or_block.kind == .absent {
					ret_sym := c.table.sym(expr_ret_type)
					if expr_ret_type.has_flag(.result)
						|| (expr_ret_type.has_flag(.option) && ret_sym.kind == .multi_return) {
						ret_typ_tok := if expr_ret_type.has_flag(.option) { '?' } else { '!' }
						if c.inside_defer {
							c.error('${expr.name}() returns `${ret_typ_tok}${ret_sym.name}`, so it should have an `or {}` block at the end',
								expr.pos)
						} else {
							c.error('${expr.name}() returns `${ret_typ_tok}${ret_sym.name}`, so it should have either an `or {}` block, or `${ret_typ_tok}` at the end',
								expr.pos)
						}
					}
				} else {
					c.check_or_expr(expr.or_block, ret_type, expr_ret_type, expr)
				}
				return ret_type.clear_flag(.result)
			} else {
				c.expr_or_block_err(expr.or_block.kind, expr.name, expr.or_block.pos,
					false)
			}
		}
		ast.SelectorExpr {
			if c.table.sym(ret_type).kind != .chan {
				if expr.typ.has_option_or_result() {
					with_modifier_kind := if expr.typ.has_flag(.option) {
						'an Option'
					} else {
						'a Result'
					}
					with_modifier := if expr.typ.has_flag(.option) { '?' } else { '!' }
					if expr.typ.has_flag(.result) && expr.or_block.kind == .absent {
						if c.inside_defer {
							c.error('field `${expr.field_name}` is ${with_modifier_kind}, so it should have an `or {}` block at the end',
								expr.pos)
						} else {
							c.error('field `${expr.field_name}` is ${with_modifier_kind}, so it should have either an `or {}` block, or `${with_modifier}` at the end',
								expr.pos)
						}
					} else {
						if expr.or_block.kind != .absent {
							c.check_or_expr(expr.or_block, ret_type, expr.typ, expr)
						}
					}
					return ret_type.clear_flag(.result)
				} else {
					c.expr_or_block_err(expr.or_block.kind, expr.field_name, expr.or_block.pos,
						true)
				}
			}
		}
		ast.IndexExpr {
			if expr.or_expr.kind != .absent {
				mut return_none_or_error := false
				if expr.or_expr.stmts.len > 0 {
					last_stmt := expr.or_expr.stmts.last()
					if last_stmt is ast.ExprStmt {
						if c.inside_return && last_stmt.typ in [ast.none_type, ast.error_type] {
							return_none_or_error = true
						}
					}
				}
				if return_none_or_error {
					c.check_expr_option_or_result_call(expr.or_expr, c.table.cur_fn.return_type)
				} else {
					c.check_or_expr(expr.or_expr, ret_type, ret_type.set_flag(.result),
						expr)
				}
			} else if expr.left is ast.SelectorExpr && expr.left_type.has_option_or_result() {
				with_modifier_kind := if expr.left_type.has_flag(.option) {
					'an Option'
				} else {
					'a Result'
				}
				with_modifier := if expr.left_type.has_flag(.option) { '?' } else { '!' }
				c.error('field `${expr.left.field_name}` is ${with_modifier_kind}, so it should have either an `or {}` block, or `${with_modifier}` at the end',
					expr.left.pos)
			}
		}
		ast.CastExpr {
			c.check_expr_option_or_result_call(expr.expr, ret_type)
		}
		ast.AsCast {
			c.check_expr_option_or_result_call(expr.expr, ret_type)
		}
		ast.ParExpr {
			c.check_expr_option_or_result_call(expr.expr, ret_type)
		}
		else {}
	}
	return ret_type
}

fn (mut c Checker) check_or_expr(node ast.OrExpr, ret_type ast.Type, expr_return_type ast.Type, expr ast.Expr) {
	if node.kind == .propagate_option {
		if c.table.cur_fn != unsafe { nil } && !c.table.cur_fn.return_type.has_flag(.option)
			&& !c.table.cur_fn.is_main && !c.table.cur_fn.is_test && !c.inside_const {
			c.add_instruction_for_option_type()
			if expr is ast.Ident {
				c.error('to propagate the Option, `${c.table.cur_fn.name}` must return an Option type',
					expr.pos)
			} else {
				c.error('to propagate the call, `${c.table.cur_fn.name}` must return an Option type',
					node.pos)
			}
		}
		if expr !in [ast.Ident, ast.SelectorExpr] && !expr_return_type.has_flag(.option) {
			if expr_return_type.has_flag(.result) {
				c.error('propagating a Result like an Option is deprecated, use `foo()!` instead of `foo()?`',
					node.pos)
			} else {
				c.error('to propagate an Option, the call must also return an Option type',
					node.pos)
			}
		}
		return
	}
	if node.kind == .propagate_result {
		if c.table.cur_fn != unsafe { nil } && !c.table.cur_fn.return_type.has_flag(.result)
			&& !c.table.cur_fn.is_main && !c.table.cur_fn.is_test && !c.inside_const {
			c.add_instruction_for_result_type()
			c.error('to propagate the call, `${c.table.cur_fn.name}` must return a Result type',
				node.pos)
		}
		if !expr_return_type.has_flag(.result) {
			c.error('to propagate a Result, the call must also return a Result type',
				node.pos)
		}
		return
	}
	if node.stmts.len == 0 {
		if expr is ast.CallExpr && expr.is_return_used {
			// x := f() or {}, || f() or {} etc
			c.error('expression requires a non empty `or {}` block', node.pos)
		} else if expr !is ast.CallExpr && ret_type != ast.void_type {
			// _ := sql db {... } or { }
			c.error('expression requires a non empty `or {}` block', node.pos)
		}
		// allow `f() or {}`
		return
	}
	mut valid_stmts := node.stmts.filter(it !is ast.SemicolonStmt)
	mut last_stmt := if valid_stmts.len > 0 { valid_stmts.last() } else { node.stmts.last() }
	if expr !is ast.CallExpr || (expr is ast.CallExpr && expr.is_return_used) {
		// requires a block returning an unwrapped type of expr return type
		c.check_or_last_stmt(mut last_stmt, ret_type, expr_return_type.clear_option_and_result())
	} else {
		// allow f() or { var = 123 }
		c.check_or_last_stmt(mut last_stmt, ast.void_type, expr_return_type.clear_option_and_result())
	}
}

fn (mut c Checker) check_or_last_stmt(mut stmt ast.Stmt, ret_type ast.Type, expr_return_type ast.Type) {
	if ret_type != ast.void_type {
		match mut stmt {
			ast.ExprStmt {
				c.expected_type = ret_type
				c.expected_or_type = ret_type.clear_option_and_result()
				if c.inside_or_block_value && stmt.expr is ast.None && ret_type.has_flag(.option) {
					// return call() or { none } where fn returns an Option type
					return
				}
				last_stmt_typ := c.expr(mut stmt.expr)
				stmt.typ = last_stmt_typ
				if last_stmt_typ.has_flag(.option) || last_stmt_typ == ast.none_type {
					if stmt.expr in [ast.Ident, ast.SelectorExpr, ast.CallExpr, ast.None, ast.CastExpr] {
						expected_type_name := c.table.type_to_str(ret_type.clear_option_and_result())
						got_type_name := c.table.type_to_str(last_stmt_typ)
						c.error('`or` block must provide a value of type `${expected_type_name}`, not `${got_type_name}`',
							stmt.expr.pos())
						return
					}
				}

				c.expected_or_type = ast.void_type
				type_fits := c.check_types(last_stmt_typ, ret_type)
					&& last_stmt_typ.nr_muls() == ret_type.nr_muls()
				is_noreturn := is_noreturn_callexpr(stmt.expr)
				if type_fits || is_noreturn {
					return
				}
				if stmt.typ == ast.void_type {
					if mut stmt.expr is ast.IfExpr {
						for mut branch in stmt.expr.branches {
							if branch.stmts.len > 0 {
								mut stmt_ := branch.stmts.last()
								c.check_or_last_stmt(mut stmt_, ret_type, expr_return_type)
							}
						}
						return
					} else if mut stmt.expr is ast.MatchExpr {
						for mut branch in stmt.expr.branches {
							if branch.stmts.len > 0 {
								mut stmt_ := branch.stmts.last()
								c.check_or_last_stmt(mut stmt_, ret_type, expr_return_type)
							}
						}
						return
					}
					expected_type_name := c.table.type_to_str(ret_type.clear_option_and_result())
					c.error('`or` block must provide a default value of type `${expected_type_name}`, or return/continue/break or call a @[noreturn] function like panic(err) or exit(1)',
						stmt.expr.pos())
				} else {
					if ret_type.is_ptr() && last_stmt_typ.is_pointer()
						&& c.table.sym(last_stmt_typ).kind == .voidptr {
						return
					}
					if last_stmt_typ == ast.none_type_idx && ret_type.has_flag(.option) {
						return
					}
					type_name := c.table.type_to_str(last_stmt_typ)
					expected_type_name := c.table.type_to_str(ret_type.clear_option_and_result())
					if ret_type.has_flag(.generic) {
						return
					}
					c.error('wrong return type `${type_name}` in the `or {}` block, expected `${expected_type_name}`',
						stmt.expr.pos())
				}
			}
			ast.BranchStmt {
				if stmt.kind !in [.key_continue, .key_break] {
					c.error('only break/continue is allowed as a branch statement in the end of an `or {}` block',
						stmt.pos)
					return
				}
			}
			ast.Return {}
			else {
				if stmt !is ast.AssertStmt || c.inside_or_block_value {
					expected_type_name := c.table.type_to_str(ret_type.clear_option_and_result())
					c.error('last statement in the `or {}` block should be an expression of type `${expected_type_name}` or exit parent scope',
						stmt.pos)
				}
			}
		}
	} else if mut stmt is ast.ExprStmt {
		match mut stmt.expr {
			ast.IfExpr {
				for mut branch in stmt.expr.branches {
					if branch.stmts.len > 0 {
						mut stmt_ := branch.stmts.last()
						c.check_or_last_stmt(mut stmt_, ret_type, expr_return_type)
					}
				}
			}
			ast.MatchExpr {
				for mut branch in stmt.expr.branches {
					if branch.stmts.len > 0 {
						mut stmt_ := branch.stmts.last()
						c.check_or_last_stmt(mut stmt_, ret_type, expr_return_type)
					}
				}
			}
			else {
				if stmt.typ == ast.void_type || expr_return_type == ast.void_type {
					return
				}
				if is_noreturn_callexpr(stmt.expr) {
					return
				}
				if c.check_types(stmt.typ, expr_return_type) {
					if stmt.typ.is_ptr() == expr_return_type.is_ptr()
						|| (expr_return_type.is_ptr() && stmt.typ.is_pointer()
						&& c.table.sym(stmt.typ).kind == .voidptr) {
						return
					}
				}
				if expr_return_type.has_flag(.generic) {
					return
				}
				// opt_returning_string() or { ... 123 }
				type_name := c.table.type_to_str(stmt.typ)
				expr_return_type_name := c.table.type_to_str(expr_return_type)
				c.error('the default expression type in the `or` block should be `${expr_return_type_name}`, instead you gave a value of type `${type_name}`',
					stmt.expr.pos())
			}
		}
	}
}

fn (mut c Checker) selector_expr(mut node ast.SelectorExpr) ast.Type {
	prevent_sum_type_unwrapping_once := c.prevent_sum_type_unwrapping_once
	c.prevent_sum_type_unwrapping_once = false

	using_new_err_struct_save := c.using_new_err_struct
	// TODO: remove; this avoids a breaking change in syntax
	if node.expr is ast.Ident && node.expr.name == 'err' {
		c.using_new_err_struct = true
	}

	// T.name, typeof(expr).name
	mut name_type := 0
	mut node_expr := node.expr
	match mut node.expr {
		ast.Ident {
			name := node.expr.name
			valid_generic := util.is_generic_type_name(name) && c.table.cur_fn != unsafe { nil }
				&& name in c.table.cur_fn.generic_names
			if valid_generic {
				name_type = c.table.find_type(name).set_flag(.generic)
			}
		}
		ast.TypeOf {
			// TODO: fix this weird case, since just `typeof(x)` is `string`, but `|typeof(x).| propertyname` should be the actual type,
			// so that we can get other metadata properties of the type, depending on `propertyname` (one of `name` or `idx` for now).
			// A better alternative would be a new `meta(x).propertyname`, that does not have a `meta(x)` case (an error),
			// or if it does, it should be a normal constant struct value, just filled at comptime.
			c.expr(mut node_expr)
			name_type = node.expr.typ
		}
		ast.AsCast {
			c.add_error_detail('for example `(${node.expr.expr} as ${c.table.type_to_str(node.expr.typ)}).${node.field_name}`')
			c.error('indeterminate `as` cast, use parenthesis to clarity', node.expr.pos)
		}
		else {}
	}
	if name_type > 0 {
		node.name_type = name_type
		match node.gkind_field {
			.name {
				return ast.string_type
			}
			.unaliased_typ, .typ, .indirections {
				return ast.int_type
			}
			else {
				if node.field_name == 'name' {
					return ast.string_type
				} else if node.field_name in ['idx', 'unaliased_typ', 'key_type', 'value_type',
					'element_type'] {
					return ast.int_type
				} else if node.field_name == 'indirections' {
					return ast.int_type
				}
				c.error('invalid field `.${node.field_name}` for type `${node.expr}`',
					node.pos)
				return ast.string_type
			}
		}
	}
	// evaluates comptime field.<name> (from T.fields)
	if c.comptime.check_comptime_is_field_selector(node) {
		if c.comptime.check_comptime_is_field_selector_bool(node) {
			node.expr_type = ast.bool_type
			return node.expr_type
		}
	}
	node.is_field_typ = node.is_field_typ || c.comptime.is_comptime_selector_type(node)
	old_selector_expr := c.inside_selector_expr
	c.inside_selector_expr = true
	mut typ := c.expr(mut node.expr)
	if node.expr.is_auto_deref_var() {
		if mut node.expr is ast.Ident && node.expr.obj is ast.Var {
			typ = node.expr.obj.typ
		}
	}
	c.inside_selector_expr = old_selector_expr
	c.using_new_err_struct = using_new_err_struct_save
	if typ == ast.void_type_idx {
		// This means that the field has an undefined type.
		// This error was handled before.
		c.error('`${node.expr}` does not return a value', node.pos)
		node.expr_type = ast.void_type
		return ast.void_type
	} else if c.comptime.inside_comptime_for && typ == c.enum_data_type
		&& node.field_name == 'value' {
		// for comp-time enum.values
		node.expr_type = c.type_resolver.get_ct_type_or_default('${c.comptime.comptime_for_enum_var}.typ',
			node.expr_type)
		node.typ = typ
		return node.expr_type
	}
	node.expr_type = typ
	if !(node.expr is ast.Ident && node.expr.kind == .constant) {
		if node.expr_type.has_flag(.option) {
			c.error('cannot access fields of an Option, handle the error with `or {...}` or propagate it with `?`',
				node.pos)
		} else if node.expr_type.has_flag(.result) {
			c.error('cannot access fields of a Result, handle the error with `or {...}` or propagate it with `!`',
				node.pos)
		}
	}
	field_name := node.field_name
	sym := c.table.sym(typ)
	final_sym := c.table.final_sym(typ)
	if (typ.has_flag(.variadic) || final_sym.kind == .array_fixed) && field_name == 'len' {
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
	mut unknown_field_msg := ''
	mut has_field := false
	mut field := ast.StructField{}
	if field_name.len > 0 && sym.info is ast.Struct && sym.language == .v
		&& field_name[0].is_capital() {
		// x.Foo.y => access the embedded struct
		for embed in sym.info.embeds {
			embed_sym := c.table.sym(embed)
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
				if err.msg() != '' {
					c.error(err.msg(), node.pos)
				}
				has_field = false
				ast.StructField{}, []ast.Type{}
			}
			node.from_embed_types = embed_types
			if sym.kind in [.aggregate, .sum_type] {
				unknown_field_msg = err.msg()
				// TODO need a better way to check that we need to display sum type variants info
				if unknown_field_msg.contains('does not exist or have the same type in all sumtype') {
					info := sym.info as ast.SumType
					missing_variants := c.table.find_missing_variants(info, field_name)
					unknown_field_msg += missing_variants
				}
			}
		}
		if !c.inside_unsafe {
			if sym.info is ast.Struct {
				if sym.info.is_union && node.next_token !in token.assign_tokens {
					if !c.pref.translated && !c.file.is_translated {
						c.warn('reading a union field (or its address) requires `unsafe`',
							node.pos)
					}
				}
			}
		}
		if typ.has_flag(.generic) && !has_field {
			gs := c.table.sym(c.unwrap_generic(typ))
			if f := c.table.find_field(gs, field_name) {
				has_field = true
				field = f
			} else {
				// look for embedded field
				has_field = true
				mut embed_types := []ast.Type{}
				field, embed_types = c.table.find_field_from_embeds(gs, field_name) or {
					if err.msg() != '' {
						c.error(err.msg(), node.pos)
					}
					has_field = false
					ast.StructField{}, []ast.Type{}
				}
				node.from_embed_types = embed_types
				node.generic_from_embed_types << embed_types
			}
		}
	}

	if has_field {
		is_used_outside := !c.inside_recheck && sym.mod != c.mod
		if is_used_outside && !field.is_pub && sym.language != .c {
			unwrapped_sym := c.table.sym(c.unwrap_generic(typ))
			c.error('field `${unwrapped_sym.name}.${field_name}` is not public', node.pos)
		}
		field_sym := c.table.sym(field.typ)
		if field.is_deprecated && is_used_outside {
			c.deprecate('field', field_name, field.attrs, node.pos)
		}
		if field_sym.kind in [.sum_type, .interface] || field.typ.has_flag(.option) {
			if !prevent_sum_type_unwrapping_once {
				scope_field := node.scope.find_struct_field(node.expr.str(), typ, field_name)
				if scope_field != unsafe { nil } {
					return scope_field.smartcasts.last()
				}
			}
		}
		node.typ = field.typ
		if node.or_block.kind != .absent {
			unwrapped_typ := c.unwrap_generic(node.typ)
			c.expected_or_type = unwrapped_typ.clear_option_and_result()
			c.stmts_ending_with_expression(mut node.or_block.stmts, c.expected_or_type)
			c.check_or_expr(node.or_block, unwrapped_typ, c.expected_or_type, node)
			c.expected_or_type = ast.void_type
		}
		return field.typ
	}
	if mut method := c.table.sym(c.unwrap_generic(typ)).find_method_with_generic_parent(field_name) {
		c.markused_comptime_call(typ.has_flag(.generic), '${int(method.params[0].typ)}.${field_name}')
		if c.expected_type != 0 && c.expected_type != ast.none_type {
			fn_type := ast.new_type(c.table.find_or_register_fn_type(method, false, true))
			// if the expected type includes the receiver, don't hide it behind a closure
			if c.check_types(fn_type, c.expected_type) {
				c.table.used_features.anon_fn = true
				return fn_type
			}
		}
		receiver := c.unwrap_generic(method.params[0].typ)
		if receiver.nr_muls() > 0 {
			if !c.inside_unsafe {
				rec_sym := c.table.sym(receiver.set_nr_muls(0))
				if !rec_sym.is_heap() {
					suggestion := if rec_sym.kind == .struct {
						'declaring `${rec_sym.name}` as `@[heap]`'
					} else {
						'wrapping the `${rec_sym.name}` object in a `struct` declared as `@[heap]`'
					}
					c.error('method `${c.table.type_to_str(receiver.idx_type())}.${method.name}` cannot be used as a variable outside `unsafe` blocks as its receiver might refer to an object stored on stack. Consider ${suggestion}.',
						node.expr.pos().extend(node.pos))
				}
			}
		}
		method.params = method.params[1..]
		node.has_hidden_receiver = true
		method.name = ''
		fn_type := ast.new_type(c.table.find_or_register_fn_type(method, false, true))
		node.typ = c.unwrap_generic(fn_type)
		c.table.used_features.anon_fn = true
		return fn_type
	}
	if sym.kind !in [.struct, .aggregate, .interface, .sum_type] {
		if sym.kind != .placeholder {
			unwrapped_sym := c.table.sym(c.unwrap_generic(typ))

			if unwrapped_sym.kind == .array_fixed && node.field_name == 'len' {
				node.typ = ast.int_type
				return ast.int_type
			}

			c.error('`${unwrapped_sym.name}` has no property `${node.field_name}`', node.pos)
		}
	} else {
		if unknown_field_msg == '' {
			unknown_field_msg = 'type `${sym.name}` has no field named `${field_name}`'
		}
		if sym.info is ast.Struct {
			if c.smartcast_mut_pos != token.Pos{} {
				c.note('smartcasting requires either an immutable value, or an explicit mut keyword before the value',
					c.smartcast_mut_pos)
			}
			suggestion := util.new_suggestion(field_name, sym.info.fields.map(it.name))
			c.error(suggestion.say(unknown_field_msg), node.pos)
			return ast.void_type
		}
		if c.smartcast_mut_pos != token.Pos{} {
			c.note('smartcasting requires either an immutable value, or an explicit mut keyword before the value',
				c.smartcast_mut_pos)
		}
		if c.smartcast_cond_pos != token.Pos{} {
			c.note('smartcast can only be used on the ident or selector, e.g. match foo, match foo.bar',
				c.smartcast_cond_pos)
		}
		c.error(unknown_field_msg, node.pos)
	}
	return ast.void_type
}

fn (mut c Checker) const_decl(mut node ast.ConstDecl) {
	if node.fields.len == 0 {
		c.warn('const block must have at least 1 declaration', node.pos)
	}
	if node.is_block {
		c.warn('const () groups will be an error after 2025-01-01 (`v fmt -w source.v` will fix that for you)',
			node.pos)
	}
	for mut field in node.fields {
		if reserved_type_names_chk.matches(util.no_cur_mod(field.name, c.mod)) {
			c.error('invalid use of reserved type `${field.name}` as a const name', field.pos)
		}
		// TODO: Check const name once the syntax is decided
		if field.name in c.const_names {
			name_pos := token.Pos{
				...field.pos
				len: util.no_cur_mod(field.name, c.mod).len
			}
			c.error('duplicate const `${field.name}`', name_pos)
		}
		if field.expr is ast.CallExpr {
			sym := c.table.sym(c.check_expr_option_or_result_call(field.expr, c.expr(mut field.expr)))
			if sym.kind == .multi_return {
				c.error('const declarations do not support multiple return values yet',
					field.expr.pos())
			}
		}
		const_name := field.name.all_after_last('.')
		if const_name == c.mod && const_name != 'main' {
			name_pos := token.Pos{
				...field.pos
				len: util.no_cur_mod(field.name, c.mod).len
			}
			c.error('duplicate of a module name `${field.name}`', name_pos)
		}
		if const_name == '_' {
			name_pos := token.Pos{
				...field.pos
				len: util.no_cur_mod(field.name, c.mod).len
			}
			c.error('cannot use `_` as a const name', name_pos)
			return
		}
		c.const_names << field.name
	}
	for i, mut field in node.fields {
		c.const_deps << field.name
		prev_const_var := c.const_var
		c.const_var = unsafe { field }
		mut typ := c.check_expr_option_or_result_call(field.expr, c.expr(mut field.expr))
		typ = c.cast_fixed_array_ret(typ, c.table.sym(typ))
		if ct_value := c.eval_comptime_const_expr(field.expr, 0) {
			field.comptime_expr_value = ct_value
			if ct_value is u64 {
				typ = ast.u64_type
			}
		}
		node.fields[i].typ = ast.mktyp(typ)
		if mut field.expr is ast.IfExpr {
			for branch in field.expr.branches {
				if branch.stmts.len > 0 && branch.stmts.last() is ast.ExprStmt
					&& branch.stmts.last().typ != ast.void_type {
					field.expr.is_expr = true
					field.expr.typ = (branch.stmts.last() as ast.ExprStmt).typ
					field.typ = field.expr.typ
					// update ConstField object's type in table
					if mut obj := c.file.global_scope.find_const(field.name) {
						obj.typ = field.typ
					}
					break
				}
			}
		}
		// Check for int overflow
		if field.typ == ast.int_type {
			if mut field.expr is ast.IntegerLiteral {
				val := field.expr.val.i64()
				if overflows_i32(val) {
					c.error('overflow in implicit type `int`, use explicit type casting instead',
						field.expr.pos)
				}
			}
		}
		c.const_deps = []
		c.const_var = prev_const_var
	}
}

fn (mut c Checker) enum_decl(mut node ast.EnumDecl) {
	c.check_valid_pascal_case(node.name, 'enum name', node.pos)
	if node.typ == ast.invalid_type {
		c.error('cannot register enum `${node.name}`, another type with this name exists',
			node.pos)
		return
	}
	mut useen := []u64{}
	mut iseen := []i64{}
	mut seen_enum_field_names := map[string]int{}
	if node.fields.len == 0 {
		c.error('enum cannot be empty', node.pos)
	}
	/*
	if node.is_pub && c.mod == 'builtin' {
		c.error('`builtin` module cannot have enums', node.pos)
	}
	*/
	mut enum_imin := i64(0)
	mut enum_imax := i64(0)
	mut enum_umin := u64(0)
	mut enum_umax := u64(0)
	mut signed := true
	senum_type := c.table.type_to_str(node.typ)
	match node.typ {
		ast.i8_type {
			signed, enum_imin, enum_imax = true, min_i8, max_i8
		}
		ast.i16_type {
			signed, enum_imin, enum_imax = true, min_i16, max_i16
		}
		ast.int_type {
			signed, enum_imin, enum_imax = true, min_i32, max_i32
		}
		ast.i64_type {
			signed, enum_imin, enum_imax = true, min_i64, max_i64
		}
		//
		ast.u8_type {
			signed, enum_umin, enum_umax = false, min_u8, max_u8
		}
		ast.u16_type {
			signed, enum_umin, enum_umax = false, min_u16, max_u16
		}
		ast.u32_type {
			signed, enum_umin, enum_umax = false, min_u32, max_u32
		}
		ast.u64_type {
			signed, enum_umin, enum_umax = false, min_u64, max_u64
		}
		else {
			if senum_type == 'i32' {
				signed, enum_imin, enum_imax = true, min_i32, max_i32
			} else {
				c.error('`${senum_type}` is not one of `i8`,`i16`,`i32`,`int`,`i64`,`u8`,`u16`,`u32`,`u64`',
					node.typ_pos)
			}
		}
	}
	if enum_imin > 0 {
		// ensure that the minimum value is negative, even with msvc, which has a bug that makes -2147483648 positive ...
		enum_imin *= -1
	}
	for i, mut field in node.fields {
		if !c.pref.experimental && util.contains_capital(field.name) {
			// TODO: C2V uses hundreds of enums with capitals, remove -experimental check once it's handled
			c.error('field name `${field.name}` cannot contain uppercase letters, use snake_case instead',
				field.pos)
		}
		if _ := seen_enum_field_names[field.name] {
			c.error('duplicate enum field name `${field.name}`', field.pos)
		}
		seen_enum_field_names[field.name] = i
		if field.has_expr {
			match mut field.expr {
				ast.IntegerLiteral {
					c.check_enum_field_integer_literal(field.expr, signed, node.is_multi_allowed,
						senum_type, field.expr.pos, mut useen, enum_umin, enum_umax, mut
						iseen, enum_imin, enum_imax)
				}
				ast.InfixExpr {
					// Handle `enum Foo { x = 1 + 2 }`
					c.infix_expr(mut field.expr)
					mut t := transformer.new_transformer_with_table(c.table, c.pref)
					folded_expr := t.infix_expr(mut field.expr)

					if folded_expr is ast.IntegerLiteral {
						c.check_enum_field_integer_literal(folded_expr, signed, node.is_multi_allowed,
							senum_type, field.expr.pos, mut useen, enum_umin, enum_umax, mut
							iseen, enum_imin, enum_imax)
					}
				}
				ast.ParExpr {
					c.expr(mut field.expr.expr)
					mut t := transformer.new_transformer_with_table(c.table, c.pref)
					folded_expr := t.expr(mut field.expr.expr)

					if folded_expr is ast.IntegerLiteral {
						c.check_enum_field_integer_literal(folded_expr, signed, node.is_multi_allowed,
							senum_type, field.expr.pos, mut useen, enum_umin, enum_umax, mut
							iseen, enum_imin, enum_imax)
					}
				}
				ast.CastExpr {
					fe_type := c.cast_expr(mut field.expr)
					if node.typ != fe_type {
						sfe_type := c.table.type_to_str(fe_type)
						c.error('the type of the enum value `${sfe_type}` != the enum type itself `${senum_type}`',
							field.expr.pos)
					}
					if !fe_type.is_pure_int() {
						c.error('the type of an enum value must be an integer type, like i8, u8, int, u64 etc.',
							field.expr.pos)
					}
					if mut field.expr.expr is ast.EnumVal {
						if field.expr.expr.enum_name == node.name {
							if field.expr.expr.val !in seen_enum_field_names {
								c.error('`${field.expr.expr.enum_name}.${field.expr.expr.val}` should be declared before using it',
									field.expr.pos)
							}
						}
					}
				}
				else {
					if mut field.expr is ast.Ident {
						if field.expr.language == .c {
							continue
						}
						if field.expr.kind == .unresolved {
							c.ident(mut field.expr)
						}
						if field.expr.kind == .constant && field.expr.obj.typ.is_int() {
							// accepts int constants as enum value
							if mut field.expr.obj is ast.ConstField {
								mut t := transformer.new_transformer_with_table(c.table,
									c.pref)
								folded_expr := t.expr(mut field.expr.obj.expr)

								if folded_expr is ast.IntegerLiteral {
									c.check_enum_field_integer_literal(folded_expr, signed,
										node.is_multi_allowed, senum_type, field.expr.pos, mut
										useen, enum_umin, enum_umax, mut iseen, enum_imin,
										enum_imax)
								}
							}
							continue
						}
					}
					mut pos := field.expr.pos()
					if pos.pos == 0 {
						pos = field.pos
					}
					c.error('the default value for an enum has to be an integer', pos)
				}
			}
		} else {
			if signed {
				if iseen.len > 0 {
					ilast := iseen.last()
					if ilast == enum_imax {
						c.error('enum value overflows type `${senum_type}`, which has a maximum value of ${enum_imax}',
							field.pos)
					} else if !c.pref.translated && !c.file.is_translated && !node.is_multi_allowed
						&& ilast + 1 in iseen {
						c.add_error_detail('use `@[_allow_multiple_values]` attribute to allow multiple enum values. Use only when it is needed')
						c.error('enum value `${ilast + 1}` already exists', field.pos)
					}
					iseen << ilast + 1
				} else {
					iseen << 0
				}
			} else {
				if useen.len > 0 {
					ulast := useen.last()
					if ulast == enum_umax {
						c.error('enum value overflows type `${senum_type}`, which has a maximum value of ${enum_umax}',
							field.pos)
					} else if !c.pref.translated && !c.file.is_translated && !node.is_multi_allowed
						&& ulast + 1 in useen {
						c.add_error_detail('use `@[_allow_multiple_values]` attribute to allow multiple enum values. Use only when it is needed')
						c.error('enum value `${ulast + 1}` already exists', field.pos)
					}
					useen << ulast + 1
				} else {
					useen << 0
				}
			}
		}
	}
}

fn (mut c Checker) check_enum_field_integer_literal(expr ast.IntegerLiteral, is_signed bool, is_multi_allowed bool,
	styp string, pos token.Pos, mut useen []u64, umin u64, umax u64, mut iseen []i64, imin i64, imax i64) {
	mut overflows := false
	mut uval := u64(0)
	mut ival := i64(0)

	if is_signed {
		val := expr.val.i64()
		ival = val
		if val < imin || val >= imax {
			c.error('enum value `${expr.val}` overflows the enum type `${styp}`, values of which have to be in [${imin}, ${imax}]',
				pos)
			overflows = true
		}
	} else {
		val := expr.val.u64()
		uval = val
		if val >= umax {
			overflows = true
			if val == umax {
				is_bin := expr.val.starts_with('0b')
				is_oct := expr.val.starts_with('0o')
				is_hex := expr.val.starts_with('0x')

				if is_hex {
					overflows = val.hex() != umax.hex()
				} else if !is_bin && !is_oct && !is_hex {
					overflows = expr.val.str() != umax.str()
				}
			}
			if overflows {
				c.error('enum value `${expr.val}` overflows the enum type `${styp}`, values of which have to be in [${umin}, ${umax}]',
					pos)
			}
		}
	}
	if !overflows && !c.pref.translated && !c.file.is_translated && !is_multi_allowed {
		if (is_signed && ival in iseen) || (!is_signed && uval in useen) {
			c.add_error_detail('use `@[_allow_multiple_values]` attribute to allow multiple enum values. Use only when it is needed')
			c.error('enum value `${expr.val}` already exists', pos)
		}
	}
	if is_signed {
		iseen << ival
	} else {
		useen << uval
	}
}

@[inline]
fn (mut c Checker) check_loop_labels(label string, pos token.Pos) {
	if label == '' {
		return
	}
	if label in c.loop_labels {
		c.error('the loop label was already defined before', pos)
		return
	}
	c.loop_labels << label
}

fn (mut c Checker) stmt(mut node ast.Stmt) {
	$if trace_checker ? {
		ntype := typeof(*node).replace('v.ast.', '')
		eprintln('checking: ${c.file.path:-30} | pos: ${node.pos.line_str():-39} | node: ${ntype} | ${node}')
	}
	c.expected_type = ast.void_type
	match mut node {
		ast.ExprStmt {
			node.typ = c.expr(mut node.expr)
			c.expected_type = ast.void_type
			mut or_typ := ast.void_type
			match mut node.expr {
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
				if mut node.expr is ast.InfixExpr {
					if node.expr.op == .left_shift {
						left_sym := c.table.final_sym(node.expr.left_type)
						if left_sym.kind != .array
							&& c.table.final_sym(c.unwrap_generic(node.expr.left_type)).kind != .array {
							c.error('unused expression', node.pos)
						}
					}
				}
			}
			if !c.inside_return {
				c.check_expr_option_or_result_call(node.expr, or_typ)
			}
			// TODO: This should work, even if it's prolly useless .-.
			// node.typ = c.check_expr_option_or_result_call(node.expr, ast.void_type)
		}
		ast.AssignStmt {
			c.assign_stmt(mut node)
		}
		ast.Block {
			c.block(mut node)
		}
		ast.BranchStmt {
			c.branch_stmt(node)
		}
		ast.ComptimeFor {
			c.comptime_for(mut node)
		}
		ast.ConstDecl {
			c.inside_const = true
			c.const_decl(mut node)
			c.inside_const = false
		}
		ast.DeferStmt {
			c.inside_defer = true
			if node.idx_in_fn < 0 && c.table.cur_fn != unsafe { nil } {
				node.idx_in_fn = c.table.cur_fn.defer_stmts.len
				c.table.cur_fn.defer_stmts << unsafe { &node }
			}
			if c.locked_names.len != 0 || c.rlocked_names.len != 0 {
				c.error('defers are not allowed in lock statements', node.pos)
			}
			for i, ident in node.defer_vars {
				mut id := ident
				if mut id.info is ast.IdentVar {
					if id.comptime && (id.tok_kind == .question
						|| id.name in ast.valid_comptime_not_user_defined) {
						node.defer_vars[i] = ast.Ident{
							scope: unsafe { nil }
							name:  ''
						}
						continue
					}
					typ := c.ident(mut id)
					if typ == ast.error_type_idx {
						continue
					}
					id.info.typ = typ
					node.defer_vars[i] = id
				}
			}
			c.stmts(mut node.stmts)
			c.inside_defer = false
		}
		ast.EnumDecl {
			c.enum_decl(mut node)
		}
		ast.FnDecl {
			c.fn_decl(mut node)
		}
		ast.ForCStmt {
			c.for_c_stmt(mut node)
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
		ast.GotoLabel {
			c.goto_label(node)
		}
		ast.GotoStmt {
			c.goto_stmt(node)
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
			c.is_just_builtin_mod = node.name == 'builtin'
			c.is_builtin_mod = c.is_just_builtin_mod || node.name in ['os', 'strconv']
			c.check_valid_snake_case(node.name, 'module name', node.pos)
		}
		ast.Return {
			// c.returns = true
			c.return_stmt(mut node)
			c.scope_returns = true
		}
		ast.SemicolonStmt {}
		ast.SqlStmt {
			c.sql_stmt(mut node)
		}
		ast.StructDecl {
			c.struct_decl(mut node)
		}
		ast.TypeDecl {
			c.type_decl(mut node)
		}
		ast.EmptyStmt {
			if c.pref.is_verbose {
				eprintln('Checker.stmt() EmptyStmt')
				print_backtrace()
			}
		}
		ast.NodeError {}
		ast.DebuggerStmt {
			c.table.used_features.debugger = true
		}
		ast.AsmStmt {
			c.asm_stmt(mut node)
		}
		ast.AssertStmt {
			c.assert_stmt(mut node)
		}
	}
}

fn (mut c Checker) assert_stmt(mut node ast.AssertStmt) {
	if node.is_used {
		c.table.used_features.asserts = true
	}
	cur_exp_typ := c.expected_type
	c.expected_type = ast.bool_type
	assert_type := c.check_expr_option_or_result_call(node.expr, c.expr(mut node.expr))
	c.markused_assertstmt_auto_str(mut node)
	if assert_type != ast.bool_type_idx {
		atype_name := c.table.sym(assert_type).name
		c.error('assert can be used only with `bool` expressions, but found `${atype_name}` instead',
			node.pos)
	}
	if node.extra !is ast.EmptyExpr {
		extra_type := c.expr(mut node.extra)
		if extra_type != ast.string_type {
			extra_type_name := c.table.sym(extra_type).name
			c.error('assert allows only a single string as its second argument, but found `${extra_type_name}` instead',
				node.extra_pos)
		}
	}
	c.fail_if_unreadable(node.expr, ast.bool_type_idx, 'assertion')
	c.expected_type = cur_exp_typ
}

fn (mut c Checker) block(mut node ast.Block) {
	if node.is_unsafe {
		prev_unsafe := c.inside_unsafe
		c.inside_unsafe = true
		c.stmts(mut node.stmts)
		c.inside_unsafe = prev_unsafe
	} else {
		if node.stmts.len > 0 && node.stmts.last() is ast.ExprStmt {
			last_stmt := node.stmts.last() as ast.ExprStmt
			if last_stmt.expr !in [ast.CallExpr, ast.IfExpr, ast.MatchExpr, ast.InfixExpr] {
				c.warn('expression evaluated but not used', node.stmts.last().pos)
			}
		}
		c.stmts(mut node.stmts)
	}
}

fn (mut c Checker) branch_stmt(node ast.BranchStmt) {
	if c.inside_defer {
		c.error('`${node.kind.str()}` is not allowed in defer statements', node.pos)
	}
	if c.in_for_count == 0 {
		if c.comptime.inside_comptime_for {
			c.error('${node.kind.str()} is not allowed within a compile-time loop', node.pos)
		} else {
			c.error('${node.kind.str()} statement not within a loop', node.pos)
		}
	}
	if node.label.len > 0 {
		if node.label !in c.loop_labels {
			c.error('invalid label name `${node.label}`', node.pos)
		}
	}
}

fn (mut c Checker) global_decl(mut node ast.GlobalDecl) {
	required_args_attr := ['_linker_section']
	for attr_name in required_args_attr {
		if attr := node.attrs.find_first(attr_name) {
			if attr.arg == '' {
				c.error('missing argument for @[${attr_name}] attribute', attr.pos)
			}
		}
	}
	for mut field in node.fields {
		c.check_valid_snake_case(field.name, 'global name', field.pos)

		if field.name in ast.global_reserved_type_names {
			c.error('invalid use of reserved type `${field.name}` as a global name', field.pos)
		}
		if field.name == '_' {
			c.error('cannot use `_` as a global name', field.pos)
			return
		}

		if field.name in c.global_names {
			c.error('duplicate global `${field.name}`', field.pos)
		}
		if '${c.mod}.${field.name}' in c.const_names {
			c.error('duplicate global and const `${field.name}`', field.pos)
		}
		sym := c.table.sym(field.typ)
		if sym.kind == .placeholder {
			c.error('unknown type `${sym.name}`', field.typ_pos)
		}
		if field.has_expr {
			if field.expr is ast.AnonFn && field.name == 'main' {
				c.error('the `main` function is the program entry point, cannot redefine it',
					field.pos)
			}
			field.typ = c.expr(mut field.expr)
			mut v := c.file.global_scope.find_global(field.name) or {
				panic('internal compiler error - could not find global in scope')
			}
			v.typ = ast.mktyp(field.typ)
		} else {
			field_sym := c.table.sym(field.typ)
			if field_sym.info is ast.ArrayFixed && c.array_fixed_has_unresolved_size(field_sym.info) {
				mut size_expr := field_sym.info.size_expr
				field.typ = c.eval_array_fixed_sizes(mut size_expr, 0, field_sym.info.elem_type)
				mut v := c.file.global_scope.find_global(field.name) or {
					panic('internal compiler error - could not find global in scope')
				}
				v.typ = ast.mktyp(field.typ)
			}
		}
		c.global_names << field.name
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
	mut aliases := c.asm_ios(mut stmt.output, mut stmt.scope, true)
	aliases2 := c.asm_ios(mut stmt.input, mut stmt.scope, false)
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
				c.error('unknown assembler directive: `${template.name}`', template.pos)
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
	match arg {
		ast.AsmAlias {}
		ast.AsmAddressing {
			if arg.scale !in [-1, 1, 2, 4, 8] {
				c.error('scale must be one of 1, 2, 4, or 8', arg.pos)
			}
			c.asm_arg(arg.displacement, stmt, aliases)
			c.asm_arg(arg.base, stmt, aliases)
			c.asm_arg(arg.index, stmt, aliases)
		}
		ast.BoolLiteral {} // all of these are guaranteed to be correct.
		ast.FloatLiteral {}
		ast.CharLiteral {}
		ast.IntegerLiteral {}
		ast.AsmRegister {} // if the register is not found, the parser will register it as an alias
		ast.AsmDisp {}
		string {}
	}
}

fn (mut c Checker) asm_ios(mut ios []ast.AsmIO, mut scope ast.Scope, output bool) []string {
	mut aliases := []string{}
	for mut io in ios {
		typ := c.expr(mut io.expr)
		if output {
			c.fail_if_immutable(mut io.expr)
		}
		if io.alias != '' {
			aliases << io.alias
			if io.alias in scope.objects {
				scope.objects[io.alias] = ast.Var{
					name:      io.alias
					expr:      io.expr
					is_arg:    true
					typ:       typ
					orig_type: typ
					pos:       io.pos
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
	if c.pref.backend.is_js() || c.pref.backend == .golang {
		// consider the best way to handle the .go.vv files
		if !c.file.path.ends_with('.js.v') && !c.file.path.ends_with('.go.v')
			&& !c.file.path.ends_with('.go.vv') {
			c.error('hash statements are only allowed in backend specific files such "x.js.v" and "x.go.v"',
				node.pos)
		}
		if c.pref.backend != .golang && c.mod == 'main' {
			c.error('hash statements are not allowed in the main module. Place them in a separate module.',
				node.pos)
		}
		return
	}
	match node.kind {
		'include', 'insert', 'preinclude', 'postinclude' {
			original_flag := node.main
			mut flag := node.main
			if flag.contains('@DIR') {
				vdir := c.dir_path()
				val := flag.replace('@DIR', vdir)
				node.val = '${node.kind} ${val}'
				node.main = val
				flag = val
			}
			if flag.contains('@VROOT') {
				// c.note(checker.vroot_is_deprecated_message, node.pos)
				vroot := util.resolve_vmodroot(flag.replace('@VROOT', '@VMODROOT'), c.file.path) or {
					c.error(err.msg(), node.pos)
					return
				}
				node.val = '${node.kind} ${vroot}'
				node.main = vroot
				flag = vroot
			}
			if flag.contains('@VEXEROOT') {
				vroot := flag.replace('@VEXEROOT', c.pref.vroot)
				node.val = '${node.kind} ${vroot}'
				node.main = vroot
				flag = vroot
			}
			if flag.contains('@VMODROOT') {
				vroot := util.resolve_vmodroot(flag, c.file.path) or {
					c.error(err.msg(), node.pos)
					return
				}
				node.val = '${node.kind} ${vroot}'
				node.main = vroot
				flag = vroot
			}
			if flag.contains('\$env(') {
				env := util.resolve_env_value(flag, true) or {
					c.error(err.msg(), node.pos)
					return
				}
				node.main = env
			}
			if flag.contains('\$d(') {
				d := util.resolve_d_value(c.pref.compile_values, flag) or {
					c.error(err.msg(), node.pos)
					return
				}
				node.main = d
			}
			flag_no_comment := flag.all_before('//').trim_space()
			if node.kind in ['include', 'preinclude', 'postinclude'] {
				if !((flag_no_comment.starts_with('"') && flag_no_comment.ends_with('"'))
					|| (flag_no_comment.starts_with('<') && flag_no_comment.ends_with('>'))) {
					c.error('including C files should use either `"header_file.h"` or `<header_file.h>` quoting',
						node.pos)
				}
			}
			if node.kind == 'insert' {
				if !(flag_no_comment.starts_with('"') && flag_no_comment.ends_with('"')) {
					c.error('inserting .c or .h files, should use `"header_file.h"` quoting',
						node.pos)
				}
				node.main = node.main.trim('"')
				if fcontent := os.read_file(node.main) {
					node.val = fcontent
				} else {
					mut missing_message := 'The file ${original_flag}, needed for insertion by module `${node.mod}`,'
					if os.is_file(node.main) {
						missing_message += ' is not readable.'
					} else {
						missing_message += ' does not exist.'
					}
					if node.msg != '' {
						missing_message += ' ${node.msg}.'
					}
					c.error(missing_message, node.pos)
				}
			}
		}
		'pkgconfig' {
			if c.pref.output_cross_c {
				// do not add any flags, since we do not know what the target platform is for the cross platform builds
				// and it is better to be as conservative as possible
				return
			}
			args := if node.main.contains('--') {
				node.main.split(' ')
			} else {
				'--cflags --libs ${node.main}'.split(' ')
			}
			mut m := pkgconfig.main(args) or {
				c.error(err.msg(), node.pos)
				return
			}
			cflags := m.run() or {
				c.error(err.msg(), node.pos)
				return
			}
			c.table.parse_cflag(cflags, c.mod, c.pref.compile_defines_all) or {
				c.error(err.msg(), node.pos)
				return
			}
		}
		'flag' {
			// #flag linux -lm
			mut flag := node.main
			if flag == 'flag' { // Checks for empty flag
				c.error('no argument(s) provided for #flag', node.pos)
			}
			if flag.contains('@VROOT') {
				// c.note(checker.vroot_is_deprecated_message, node.pos)
				flag = util.resolve_vmodroot(flag.replace('@VROOT', '@VMODROOT'), c.file.path) or {
					c.error(err.msg(), node.pos)
					return
				}
			}
			if flag.contains('@DIR') {
				// expand `@DIR` to its absolute path
				flag = flag.replace('@DIR', c.dir_path())
			}
			if flag.contains('@VEXEROOT') {
				// expand `@VEXEROOT` to its absolute path
				flag = flag.replace('@VEXEROOT', c.pref.vroot)
			}
			if flag.contains('@VMODROOT') {
				flag = util.resolve_vmodroot(flag, c.file.path) or {
					c.error(err.msg(), node.pos)
					return
				}
			}
			if flag.contains('\$env(') {
				flag = util.resolve_env_value(flag, true) or {
					c.error(err.msg(), node.pos)
					return
				}
			}
			if flag.contains('\$d(') {
				flag = util.resolve_d_value(c.pref.compile_values, flag) or {
					c.error(err.msg(), node.pos)
					return
				}
			}
			for deprecated in ['@VMOD', '@VMODULE', '@VPATH', '@VLIB_PATH'] {
				if flag.contains(deprecated) {
					if !flag.contains('@VMODROOT') {
						c.error('${deprecated} had been deprecated, use @VMODROOT instead.',
							node.pos)
					}
				}
			}
			c.table.parse_cflag(flag, c.mod, c.pref.compile_defines_all) or {
				c.error(err.msg(), node.pos)
			}
		}
		else {
			if node.kind == 'define' {
				if !c.is_builtin_mod && !c.file.path.ends_with('.c.v')
					&& !c.file.path.contains('vlib') {
					if !c.pref.is_bare {
						c.error("#define can only be used in vlib (V's standard library) and *.c.v files",
							node.pos)
					}
				}
			} else {
				c.error('expected `#define`, `#flag`, `#include`, `#insert` or `#pkgconfig` not ${node.val}',
					node.pos)
			}
		}
	}
}

fn (mut c Checker) import_stmt(node ast.Import) {
	if node.mod == 'x.vweb' {
		println('`x.vweb` is now `veb`. The module is no longer experimental. Simply `import veb` instead of `import x.vweb`.')
	} else if node.mod == 'vweb' {
		println('`vweb` has been deprecated. Please use the more stable and fast `veb` instead.')
	}
	c.check_valid_snake_case(node.alias, 'module alias', node.pos)
	for sym in node.syms {
		name := '${node.mod}.${sym.name}'
		if sym.name[0].is_capital() {
			if type_sym := c.table.find_sym(name) {
				if type_sym.kind != .placeholder {
					if !type_sym.is_pub {
						c.error('module `${node.mod}` type `${sym.name}` is private',
							sym.pos)
					}
					continue
				}
			}
			c.error('module `${node.mod}` has no type `${sym.name}`', sym.pos)
			continue
		}
		if sym.name in ast.builtin_type_names {
			c.error('cannot import or override builtin type', sym.pos)
		}
		if func := c.table.find_fn(name) {
			if !func.is_pub {
				c.error('module `${node.mod}` function `${sym.name}()` is private', sym.pos)
			}
			continue
		}
		if _ := c.file.global_scope.find_const(name) {
			continue
		}
		c.error('module `${node.mod}` has no constant or function `${sym.name}`', sym.pos)
	}
	if c.table.module_deprecated[node.mod] {
		c.deprecate('module', node.mod, c.table.module_attrs[node.mod], node.pos)
	}
}

// stmts should be used for processing normal statement lists (fn bodies, for loop bodies etc).
fn (mut c Checker) stmts(mut stmts []ast.Stmt) {
	old_stmt_level := c.stmt_level
	c.stmt_level = 0
	c.stmts_ending_with_expression(mut stmts, c.expected_or_type)
	c.stmt_level = old_stmt_level
}

// stmts_ending_with_expression, should be used for processing list of statements, that can end with an expression.
// Examples for such lists are the bodies of `or` blocks, `if` expressions and `match` expressions:
//    `x := opt() or { stmt1 stmt2 ExprStmt }`,
//    `x := if cond { stmt1 stmt2 ExprStmt } else { stmt2 stmt3 ExprStmt }`,
//    `x := match expr { Type1 { stmt1 stmt2 ExprStmt } else { stmt2 stmt3 ExprStmt }`.
fn (mut c Checker) stmts_ending_with_expression(mut stmts []ast.Stmt, expected_or_type ast.Type) {
	if stmts.len == 0 {
		c.scope_returns = false
		return
	}
	if c.stmt_level > stmt_level_cutoff_limit {
		c.scope_returns = false
		c.error('checker: too many stmt levels: ${c.stmt_level} ', stmts[0].pos)
		return
	}
	mut unreachable := token.Pos{
		line_nr: -1
	}
	c.stmt_level++
	for i, mut stmt in stmts {
		c.is_last_stmt = i == stmts.len - 1
		if c.scope_returns && unreachable.line_nr == -1 {
			unreachable = stmt.pos
		}
		prev_expected_or_type := c.expected_or_type
		c.expected_or_type = expected_or_type
		c.stmt(mut stmt)
		c.expected_or_type = prev_expected_or_type
		if !c.inside_anon_fn && c.in_for_count > 0 && stmt is ast.BranchStmt
			&& stmt.kind in [.key_continue, .key_break] {
			c.scope_returns = true
		} else if stmt is ast.GotoLabel {
			unreachable = token.Pos{
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
}

fn (mut c Checker) unwrap_generic(typ ast.Type) ast.Type {
	if typ.has_flag(.generic) {
		if c.inside_generic_struct_init {
			generic_names := c.cur_struct_generic_types.map(c.table.sym(it).name)
			if t_typ := c.table.convert_generic_type(typ, generic_names, c.cur_struct_concrete_types) {
				return t_typ
			}
		}
		if c.table.cur_fn != unsafe { nil } {
			if t_typ := c.table.convert_generic_type(typ, c.table.cur_fn.generic_names,
				c.table.cur_concrete_types)
			{
				return t_typ
			}
			if c.inside_lambda && c.table.cur_lambda.call_ctx != unsafe { nil } {
				if t_typ := c.table.convert_generic_type(typ, c.table.cur_lambda.func.decl.generic_names,
					c.table.cur_lambda.call_ctx.concrete_types)
				{
					return t_typ
				}
			}
		}
	}
	return typ
}

pub fn (mut c Checker) expr(mut node ast.Expr) ast.Type {
	c.expr_level++
	defer {
		c.expr_level--
	}

	if c.expr_level > expr_level_cutoff_limit {
		c.error('checker: too many expr levels: ${c.expr_level} ', node.pos())
		return ast.void_type
	}
	match mut node {
		ast.IfExpr {
			return c.if_expr(mut node)
		}
		ast.ComptimeType {
			c.error('incorrect use of compile-time type', node.pos)
		}
		ast.EmptyExpr {
			if c.pref.is_verbose {
				print_backtrace()
			}
			c.error('checker.expr(): unhandled EmptyExpr', token.Pos{})
			return ast.void_type
		}
		ast.CTempVar {
			return node.typ
		}
		ast.AnonFn {
			return c.anon_fn(mut node)
		}
		ast.ArrayDecompose {
			typ := c.expr(mut node.expr)
			type_sym := c.table.sym(typ)
			if type_sym.kind == .array_fixed {
				c.error('direct decomposition of fixed array is not allowed, convert the fixed array to normal array via ${node.expr}[..]',
					node.expr.pos())
				return ast.void_type
			} else if type_sym.kind != .array {
				c.error('decomposition can only be used on arrays', node.expr.pos())
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
			node.expr_type = c.expr(mut node.expr)
			expr_type_sym := c.table.sym(node.expr_type)
			type_sym := c.table.sym(c.unwrap_generic(node.typ))
			if mut node.expr is ast.Ident {
				if mut node.expr.obj is ast.Var {
					ident_typ := if node.expr.obj.smartcasts.len > 0 {
						node.expr.obj.smartcasts.last()
					} else {
						node.expr.obj.typ
					}
					if !node.typ.has_flag(.option) && ident_typ.has_flag(.option)
						&& node.expr.or_expr.kind == .absent {
						c.error('variable `${node.expr.name}` is an Option, it must be unwrapped first',
							node.expr.pos)
					}
				}
			}
			if expr_type_sym.kind == .sum_type {
				c.ensure_type_exists(node.typ, node.pos)
				if !c.table.sumtype_has_variant(c.unwrap_generic(node.expr_type), c.unwrap_generic(node.typ),
					true) {
					addr := '&'.repeat(node.typ.nr_muls())
					c.error('cannot cast `${expr_type_sym.name}` to `${addr}${type_sym.name}`',
						node.pos)
				}
			} else if expr_type_sym.kind == .interface {
				c.ensure_type_exists(node.typ, node.pos)
				if type_sym.kind != .interface {
					c.type_implements(node.typ, node.expr_type, node.pos)
				}
			} else if node.expr_type.clear_flag(.option) != node.typ.clear_flag(.option) {
				mut s := 'cannot cast non-sum type `${expr_type_sym.name}` using `as`'
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
				mut expr := node.exprs[i]
				c.expr(mut expr)
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
			if ret_type != 0 && c.table.sym(ret_type).kind == .alias {
				unaliased_type := c.table.unaliased_type(ret_type)
				if unaliased_type.has_option_or_result() {
					ret_type = unaliased_type
				}
			}
			if !ret_type.has_option_or_result() {
				c.expr_or_block_err(node.or_block.kind, node.name, node.or_block.pos,
					false)
			}
			if node.or_block.kind != .absent {
				if ret_type.has_flag(.option) {
					ret_type = ret_type.clear_flag(.option)
				}
				if ret_type.has_flag(.result) {
					ret_type = ret_type.clear_flag(.result)
				}
			}
			return ret_type
		}
		ast.ChanInit {
			return c.chan_init(mut node)
		}
		ast.CharLiteral {
			return ast.rune_type
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
			return c.comptime_selector(mut node)
		}
		ast.ConcatExpr {
			return c.concat_expr(mut node)
		}
		ast.DumpExpr {
			c.table.used_features.dump = true
			c.expected_type = ast.string_type
			node.expr_type = c.expr(mut node.expr)
			c.markused_dumpexpr(mut node)
			if c.comptime.inside_comptime_for && mut node.expr is ast.Ident {
				if node.expr.ct_expr {
					node.expr_type = c.type_resolver.get_type(node.expr as ast.Ident)
				} else if (node.expr as ast.Ident).name in c.type_resolver.type_map {
					node.expr_type = c.type_resolver.get_ct_type_or_default((node.expr as ast.Ident).name,
						node.expr_type)
				}
			}
			c.check_expr_option_or_result_call(node.expr, node.expr_type)
			etidx := node.expr_type.idx()
			if etidx == ast.void_type_idx {
				c.error('dump expression can not be void', node.expr.pos())
				return ast.void_type
			} else if etidx == ast.char_type_idx && node.expr_type.nr_muls() == 0 {
				c.error('`char` values cannot be dumped directly, use dump(u8(x)) or dump(int(x)) instead',
					node.expr.pos())
				return ast.void_type
			}

			unwrapped_expr_type := c.unwrap_generic(node.expr_type)
			tsym := c.table.sym(unwrapped_expr_type)
			tsym_final := c.table.final_sym(unwrapped_expr_type)
			if tsym_final.kind == .array_fixed {
				info := tsym_final.info as ast.ArrayFixed
				if !info.is_fn_ret {
					// for dumping fixed array we must register the fixed array struct to return from function
					c.table.find_or_register_array_fixed(info.elem_type, info.size, info.size_expr,
						true)
				}
			}
			type_cname := if node.expr_type.has_flag(.option) {
				'_option_${tsym.cname}'
			} else {
				tsym.cname
			}
			c.table.dumps[int(unwrapped_expr_type.clear_flags(.result, .atomic_f))] = type_cname
			node.cname = type_cname
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
		ast.SpawnExpr {
			return c.spawn_expr(mut node)
		}
		ast.Ident {
			return c.ident(mut node)
		}
		ast.IfGuardExpr {
			old_inside_if_guard := c.inside_if_guard
			c.inside_if_guard = true
			node.expr_type = c.expr(mut node.expr)
			c.inside_if_guard = old_inside_if_guard
			if c.pref.skip_unused && node.expr_type.has_flag(.generic) {
				unwrapped_type := c.unwrap_generic(node.expr_type)
				c.table.used_features.comptime_syms[unwrapped_type] = true
			}
			if !node.expr_type.has_flag(.option) && !node.expr_type.has_flag(.result) {
				mut no_opt_or_res := true
				match mut node.expr {
					ast.IndexExpr {
						no_opt_or_res = false
						node.expr_type = node.expr_type.set_flag(.option)
						node.expr.is_option = true
					}
					ast.PrefixExpr {
						if node.expr.op == .arrow {
							no_opt_or_res = false
							node.expr_type = node.expr_type.set_flag(.option)
							node.expr.is_option = true
						}
					}
					else {}
				}
				if no_opt_or_res {
					c.error('expression should either return an Option or a Result', node.expr.pos())
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
		ast.LambdaExpr {
			c.inside_lambda = true
			c.table.cur_lambda = unsafe { &node }
			defer {
				c.inside_lambda = false
				c.table.cur_lambda = unsafe { nil }
			}
			return c.lambda_expr(mut node, c.expected_type)
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
		ast.Nil {
			if !c.inside_unsafe {
				c.error('`nil` is only allowed in `unsafe` code', node.pos)
			}
			return ast.nil_type
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
			return c.expr(mut node.expr)
		}
		ast.RangeExpr {
			// branch range expression of `match x { a...b {} }`, or: `a#[x..y]`:
			ltyp := c.expr(mut node.low)
			htyp := c.expr(mut node.high)
			if !c.check_types(ltyp, htyp) {
				lstype := c.table.type_to_str(ltyp)
				hstype := c.table.type_to_str(htyp)
				c.add_error_detail('')
				c.add_error_detail(' low part type: ${lstype}')
				c.add_error_detail('high part type: ${hstype}')
				c.error('the low and high parts of a range expression, should have matching types',
					node.pos)
			}
			node.typ = c.promote(ltyp, htyp)
			return ltyp
		}
		ast.SelectExpr {
			return c.select_expr(mut node)
		}
		ast.SelectorExpr {
			mut ret_type := c.selector_expr(mut node)
			if c.table.sym(ret_type).kind == .chan {
				return ret_type
			}
			if !ret_type.has_flag(.option) && !ret_type.has_flag(.result) {
				c.expr_or_block_err(node.or_block.kind, node.field_name, node.or_block.pos,
					true)
			}
			if node.or_block.kind != .absent {
				if ret_type.has_flag(.option) {
					ret_type = ret_type.clear_flag(.option)
				}
				if ret_type.has_flag(.result) {
					ret_type = ret_type.clear_flag(.result)
				}
			}
			return ret_type
		}
		ast.SizeOf {
			if !node.is_type {
				node.typ = c.expr(mut node.expr)
			}
			sym := c.table.final_sym(node.typ)
			if sym.kind == .placeholder && sym.language != .c {
				// Allow `sizeof(C.MYSQL_TIME)` etc
				c.error('unknown type `${sym.name}`', node.pos)
			}
			// c.deprecate_old_isreftype_and_sizeof_of_a_guessed_type(node.guessed_type,
			//	node.typ, node.pos, 'sizeof')
			return ast.u32_type
		}
		ast.IsRefType {
			if !node.is_type {
				node.typ = c.expr(mut node.expr)
			}
			// c.deprecate_old_isreftype_and_sizeof_of_a_guessed_type(node.guessed_type,
			//	node.typ, node.pos, 'isreftype')
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
				return ast.u8_type.set_nr_muls(1)
			}
			if node.language == .js {
				// string literal starts with "js": `JS.console.log(js'hello')`
				if c.js_string.is_void() {
					c.js_string = c.table.find_type('JS.String')
				}
				return c.js_string
			}
			if node.is_raw {
				// raw strings don't need any sort of checking related to unicode
				return ast.string_type
			}
			return c.string_lit(mut node)
		}
		ast.StringInterLiteral {
			return c.string_inter_lit(mut node)
		}
		ast.StructInit {
			if node.unresolved {
				mut expr_ := c.table.resolve_init(node, c.unwrap_generic(node.typ))
				c.markused_used_maps(c.table.used_features.used_maps == 0 && expr_ is ast.MapInit)
				return c.expr(mut expr_)
			}
			mut inited_fields := []string{}
			return c.struct_init(mut node, false, mut inited_fields)
		}
		ast.TypeNode {
			if !c.inside_x_is_type && node.typ.has_flag(.generic) && unsafe { c.table.cur_fn != 0 }
				&& c.table.cur_fn.generic_names.len == 0 {
				c.error('unexpected generic variable in non-generic function `${c.table.cur_fn.name}`',
					node.pos)
			} else if node.stmt != ast.empty_stmt && node.typ == ast.void_type {
				c.stmt(mut node.stmt)
				node.typ = c.table.find_type((node.stmt as ast.StructDecl).name)
			}
			return node.typ
		}
		ast.TypeOf {
			if !node.is_type {
				node.typ = c.expr(mut node.expr)
			}
			return ast.string_type
		}
		ast.UnsafeExpr {
			return c.unsafe_expr(mut node)
		}
		ast.Likely {
			ltype := c.expr(mut node.expr)
			if !c.check_types(ltype, ast.bool_type) {
				ltype_sym := c.table.sym(ltype)
				lname := if node.is_likely { '_likely_' } else { '_unlikely_' }
				c.error('`${lname}()` expects a boolean expression, instead it got `${ltype_sym.name}`',
					node.pos)
			}
			return ast.bool_type
		}
		ast.NodeError {}
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

fn (mut c Checker) cast_expr(mut node ast.CastExpr) ast.Type {
	// Given: `Outside( Inside(xyz) )`,
	//        node.expr_type: `Inside`
	//        node.typ: `Outside`
	mut to_type := c.unwrap_generic(node.typ)

	old_inside_integer_literal_cast := c.inside_integer_literal_cast
	c.inside_integer_literal_cast = to_type.is_int() && node.expr is ast.IntegerLiteral
	node.expr_type = c.expr(mut node.expr) // type to be casted
	c.inside_integer_literal_cast = old_inside_integer_literal_cast

	if mut node.expr is ast.ComptimeSelector {
		node.expr_type = c.type_resolver.get_comptime_selector_type(node.expr, node.expr_type)
	} else if node.expr is ast.Ident && c.comptime.is_comptime_variant_var(node.expr) {
		node.expr_type = c.type_resolver.get_ct_type_or_default('${c.comptime.comptime_for_variant_var}.typ',
			ast.void_type)
	}
	mut from_type := c.unwrap_generic(node.expr_type)
	from_sym := c.table.sym(from_type)
	final_from_sym := c.table.final_sym(from_type)

	mut to_sym := c.table.sym(to_type) // type to be used as cast
	mut final_to_sym := c.table.final_sym(to_type)
	final_to_type := if mut to_sym.info is ast.Alias {
		to_sym.info.parent_type
	} else {
		to_type
	}
	final_to_is_ptr := to_type.is_ptr() || final_to_type.is_ptr()
	c.markused_castexpr(mut node, to_type, mut final_to_sym)
	if to_type.has_flag(.result) {
		c.error('casting to Result type is forbidden', node.pos)
	}
	c.check_any_type(to_type, to_sym, node.pos)

	if c.pref.backend.is_js() {
		if (to_sym.is_number() && from_sym.name == 'JS.Number')
			|| (to_sym.is_number() && from_sym.name == 'JS.BigInt')
			|| (to_sym.is_string() && from_sym.name == 'JS.String')
			|| (to_type.is_bool() && from_sym.name == 'JS.Boolean')
			|| (from_type.is_bool() && to_sym.name == 'JS.Boolean')
			|| (from_sym.is_number() && to_sym.name == 'JS.Number')
			|| (from_sym.is_number() && to_sym.name == 'JS.BigInt')
			|| (from_sym.is_string() && to_sym.name == 'JS.String') {
			return to_type
		}
	}

	if !c.expected_type.has_flag(.generic) && to_sym.name.len == 1
		&& to_sym.name.starts_with_capital() {
		c.error('unknown type `${to_sym.name}`', node.pos)
	}

	if to_sym.language != .c {
		c.ensure_type_exists(to_type, node.pos)

		if to_sym.info is ast.Alias && to_sym.info.parent_type.has_flag(.option)
			&& !to_type.has_flag(.option) {
			c.error('alias to Option type requires to be used as Option type (?${to_sym.name}(...))',
				node.pos)
		}
	}
	if from_sym.kind == .u8 && from_type.is_ptr() && to_sym.kind == .string && !to_type.is_ptr() {
		c.error('to convert a C string buffer pointer to a V string, use x.vstring() instead of string(x)',
			node.pos)
	}
	if from_type == ast.void_type {
		c.error('expression does not return a value so it cannot be cast', node.expr.pos())
	}
	if to_type.has_flag(.option) && from_type == ast.none_type {
		// allow conversion from none to every option type
	} else if to_sym.kind == .sum_type {
		to_sym_info := to_sym.info as ast.SumType
		if c.pref.skip_unused && to_sym_info.concrete_types.len > 0 {
			c.table.used_features.comptime_syms[to_type] = true
		}
		if to_sym_info.generic_types.len > 0 && to_sym_info.concrete_types.len == 0 {
			c.error('generic sumtype `${to_sym.name}` must specify type parameter, e.g. ${to_sym.name}[int]',
				node.pos)
		}
		if from_type in [ast.int_literal_type, ast.float_literal_type] {
			xx := if from_type == ast.int_literal_type { ast.int_type } else { ast.f64_type }
			node.expr_type = c.promote_num(node.expr_type, xx)
			from_type = node.expr_type
		}
		if !c.table.sumtype_has_variant(to_type, from_type, false) {
			ft := c.table.type_to_str(from_type)
			tt := c.table.type_to_str(to_type)
			c.error('cannot cast `${ft}` to `${tt}`', node.pos)
		}
	} else if mut to_sym.info is ast.Alias && !(final_to_sym.kind == .struct && final_to_is_ptr) {
		if (!c.check_types(from_type, to_sym.info.parent_type) && !(final_to_sym.is_int()
			&& final_from_sym.kind in [.enum, .bool, .i8, .u8, .char]))
			|| (final_to_sym.kind == .struct
			&& from_type.idx() in [ast.voidptr_type_idx, ast.nil_type_idx]) {
			ft := c.table.type_to_str(from_type)
			tt := c.table.type_to_str(to_type)
			c.error('cannot cast `${ft}` to `${tt}` (alias to `${final_to_sym.name}`)',
				node.pos)
		}
	} else if to_sym.kind == .struct && mut to_sym.info is ast.Struct
		&& (!to_sym.info.is_typedef || from_type.idx() in [ast.voidptr_type_idx, ast.nil_type_idx])
		&& !final_to_is_ptr {
		// For now we ignore C typedef because of `C.Window(C.None)` in vlib/clipboard (except for `from_type` is voidptr/nil)
		if from_sym.kind == .struct && from_sym.info is ast.Struct && !from_type.is_ptr() {
			if !to_type.has_flag(.option) {
				c.warn('casting to struct is deprecated, use e.g. `Struct{...expr}` instead',
					node.pos)
			}
			if !c.check_struct_signature(from_sym.info, to_sym.info) {
				c.error('cannot convert struct `${from_sym.name}` to struct `${to_sym.name}`',
					node.pos)
			}
		} else {
			ft := c.table.type_to_str(from_type)
			c.error('cannot cast `${ft}` to struct', node.pos)
		}
	} else if to_sym.kind == .struct && final_to_is_ptr {
		if from_sym.info is ast.Alias {
			from_type = from_sym.info.parent_type.derive_add_muls(from_type)
		}
		if mut node.expr is ast.IntegerLiteral {
			if node.expr.val.int() == 0 && !c.pref.translated && !c.file.is_translated {
				c.error('cannot null cast a struct pointer, use &${to_sym.name}(unsafe { nil })',
					node.pos)
			} else if !c.inside_unsafe && !c.pref.translated && !c.file.is_translated {
				c.error('cannot cast int to a struct pointer outside `unsafe`', node.pos)
			}
		} else if mut node.expr is ast.Ident {
			match mut node.expr.obj {
				ast.GlobalField, ast.ConstField, ast.Var {
					if mut node.expr.obj.expr is ast.IntegerLiteral {
						if node.expr.obj.expr.val.int() == 0 && !c.pref.translated
							&& !c.file.is_translated {
							c.error('cannot null cast a struct pointer, use &${to_sym.name}(unsafe { nil })',
								node.pos)
						} else if !c.inside_unsafe && !c.pref.translated && !c.file.is_translated {
							c.error('cannot cast int to a struct pointer outside `unsafe`',
								node.pos)
						}
					}
				}
				else {}
			}
		}
		if from_type == ast.voidptr_type_idx && !c.inside_unsafe && !c.pref.translated
			&& !c.file.is_translated {
			c.error('cannot cast voidptr to a struct outside `unsafe`', node.pos)
		}
		if !from_type.is_int() && final_from_sym.kind != .enum
			&& !from_type.is_any_kind_of_pointer() {
			ft := c.table.type_to_str(from_type)
			tt := c.table.type_to_str(to_type)
			c.error('cannot cast `${ft}` to `${tt}`', node.pos)
		}
	} else if !from_type.has_option_or_result() && mut to_sym.info is ast.Interface {
		if c.type_implements(from_type, to_type, node.pos) {
			if !from_type.is_any_kind_of_pointer() && from_sym.kind != .interface
				&& !c.inside_unsafe && !from_type.is_number() {
				c.mark_as_referenced(mut &node.expr, true)
			}
			if to_sym.info.is_generic {
				inferred_type := c.unwrap_generic_interface(from_type, to_type, node.pos)
				if inferred_type != 0 {
					to_type = inferred_type
					to_sym = c.table.sym(to_type)
					final_to_sym = c.table.final_sym(to_type)
				}
			}
		} else {
			ft := c.table.type_to_str(from_type)
			tt := c.table.type_to_str(to_type)
			c.error('`${ft}` does not implement interface `${tt}`, cannot cast `${ft}` to interface `${tt}`',
				node.pos)
		}
	} else if to_type == ast.bool_type && from_type != ast.bool_type && !c.inside_unsafe
		&& !c.pref.translated && !c.file.is_translated {
		c.error('cannot cast to bool - use e.g. `some_int != 0` instead', node.pos)
	} else if from_type == ast.none_type && !to_type.has_flag(.option) && !to_type.has_flag(.result) {
		type_name := c.table.type_to_str(to_type)
		c.error('cannot cast `none` to `${type_name}`', node.pos)
	} else if !from_type.has_option_or_result() && from_sym.kind == .struct && !from_type.is_ptr() {
		if (final_to_is_ptr || to_sym.kind !in [.sum_type, .interface]) && !c.is_builtin_mod {
			from_type_name := c.table.type_to_str(from_type)
			type_name := c.table.type_to_str(to_type)
			c.error('cannot cast struct `${from_type_name}` to `${type_name}`', node.pos)
		}
	} else if to_sym.kind == .u8 && !final_from_sym.is_number()
		&& !from_type.is_any_kind_of_pointer() && final_from_sym.kind !in [.char, .enum, .bool] {
		ft := c.table.type_to_str(from_type)
		tt := c.table.type_to_str(to_type)
		c.error('cannot cast type `${ft}` to `${tt}`', node.pos)
	} else if (from_type.has_flag(.option) && !to_type.has_flag(.option))
		|| from_type.has_flag(.result) || from_type.has_flag(.variadic) {
		// variadic case can happen when arrays are converted into variadic
		msg := if from_type.has_flag(.option) {
			'an Option'
		} else if from_type.has_flag(.result) {
			'a Result'
		} else {
			'a variadic'
		}
		c.error('cannot type cast ${msg}', node.pos)
	} else if !c.inside_unsafe && to_type.is_ptr() && from_type.is_ptr() && to_type != from_type
		&& to_type.deref() != ast.char_type && from_type.deref() != ast.char_type {
		ft := c.table.type_to_str(from_type)
		tt := c.table.type_to_str(to_type)
		c.warn('casting `${ft}` to `${tt}` is only allowed in `unsafe` code', node.pos)
	} else if from_sym.kind == .array_fixed && !from_type.is_ptr() {
		if !c.pref.translated && !c.file.is_translated && !c.inside_unsafe {
			c.warn('cannot cast a fixed array (use e.g. `&arr[0]` instead)', node.pos)
		}
	} else if final_from_sym.kind == .string && final_to_sym.is_number()
		&& final_to_sym.kind != .rune {
		snexpr := node.expr.str()
		tt := c.table.type_to_str(to_type)
		c.error('cannot cast string to `${tt}`, use `${snexpr}.${final_to_sym.name}()` instead.',
			node.pos)
	} else if final_from_sym.kind == .string && final_to_is_ptr && to_sym.kind != .string {
		snexpr := node.expr.str()
		tt := c.table.type_to_str(to_type)
		c.error('cannot cast string to `${tt}`, use `${snexpr}.str` instead.', node.pos)
	} else if final_from_sym.kind == .string && to_sym.kind == .char {
		snexpr := node.expr.str()
		tt := c.table.type_to_str(to_type)
		c.error('cannot cast string to `${tt}`, use `${snexpr}[index]` instead.', node.pos)
	} else if final_from_sym.kind == .string && to_type.is_voidptr()
		&& !node.expr_type.has_flag(.generic) && !from_type.is_ptr() {
		c.error('cannot cast string to `voidptr`, use voidptr(s.str) instead', node.pos)
	} else if final_from_sym.kind == .string && to_type.is_pointer() && !c.inside_unsafe {
		tt := c.table.type_to_str(to_type)
		c.error('cannot cast string to `${tt}` outside `unsafe`, use ${tt}(s.str) instead',
			node.pos)
	} else if final_from_sym.kind == .array && !from_type.is_ptr() && to_type != ast.string_type
		&& !(to_type.has_flag(.option) && from_type.idx() == to_type.idx()) {
		ft := c.table.type_to_str(from_type)
		tt := c.table.type_to_str(to_type)
		c.error('cannot cast array `${ft}` to `${tt}`', node.pos)
	} else if from_type.has_flag(.option) && to_type.has_flag(.option)
		&& to_sym.kind != final_from_sym.kind {
		ft := c.table.type_to_str(from_type)
		tt := c.table.type_to_str(to_type)
		c.error('cannot cast incompatible option ${final_to_sym.name} `${ft}` to `${tt}`',
			node.pos)
	} else if to_sym.kind == .rune && from_sym.is_string() {
		snexpr := node.expr.str()
		ft := c.table.type_to_str(from_type)
		c.error('cannot cast `${ft}` to rune, use `${snexpr}.runes()` instead.', node.pos)
	} else if !from_type.is_ptr() && from_type != ast.string_type
		&& final_from_sym.info is ast.Struct && !final_from_sym.info.is_empty_struct()
		&& (final_to_sym.is_int() || final_to_sym.is_float()) {
		ft := c.table.type_to_str(from_type)
		tt := c.table.type_to_str(to_type)
		c.error('cannot cast type `${ft}` to `${tt}`', node.pos)
	}

	// if from_type == ast.voidptr_type_idx && !c.inside_unsafe && !c.pref.translated
	// Do not allow `&u8(unsafe { nil })` etc, force nil or voidptr cast
	if from_type.is_number() && to_type.is_ptr() && !c.inside_unsafe && !c.pref.translated
		&& !c.file.is_translated {
		if from_sym.language != .c {
			ne_name := node.expr.str()
			if !ne_name.starts_with('C.') {
				// TODO make an error
				c.warn('cannot cast a number to a type reference, use `nil` or a voidptr cast first: `&Type(voidptr(123))`',
					node.pos)
			}
		}
	}

	// T(0) where T is array or map
	if node.typ.has_flag(.generic) && to_sym.kind in [.array, .map, .array_fixed]
		&& node.expr.is_literal() {
		c.error('cannot cast literal value to ${to_sym.name} type', node.pos)
	}

	if to_sym.kind == .enum && !(c.inside_unsafe || c.file.is_translated) && from_sym.is_int() {
		c.error('casting numbers to enums, should be done inside `unsafe{}` blocks', node.pos)
	}

	if final_to_sym.kind == .function && final_from_sym.kind == .function && !(c.inside_unsafe
		|| c.file.is_translated) && !c.check_matching_function_symbols(final_from_sym, final_to_sym) {
		c.error('casting a function value from one function signature, to another function signature, should be done inside `unsafe{}` blocks',
			node.pos)
	} else if final_to_sym.kind == .function && final_from_sym.kind != .function {
		if to_type.has_flag(.option) && node.expr !is ast.None {
			c.error('casting number to Option function is not allowed, only compatible function or `none`',
				node.pos)
		} else if !(c.inside_unsafe || c.file.is_translated) {
			if node.expr is ast.IntegerLiteral {
				c.warn('casting number to function value should be done inside `unsafe{}` blocks',
					node.pos)
			} else if node.expr is ast.Nil {
				c.warn('casting `nil` to function value should be done inside `unsafe{}` blocks',
					node.pos)
			} else if node.expr is ast.None {
				if from_type.has_flag(.option) {
					c.warn('cannot pass `none` to a non Option function type', node.pos)
				}
			} else if final_from_sym.kind != .voidptr {
				c.error('invalid casting value to function', node.pos)
			}
		}
	}
	if to_type.is_ptr() && to_sym.kind == .alias && from_sym.kind == .map {
		c.error('cannot cast to alias pointer `${c.table.type_to_str(to_type)}` because `${c.table.type_to_str(from_type)}` is a value',
			node.pos)
	}

	if to_type == ast.string_type {
		if from_type in [ast.u8_type, ast.bool_type] {
			snexpr := node.expr.str()
			ft := c.table.type_to_str(from_type)
			c.error('cannot cast type `${ft}` to string, use `${snexpr}.str()` instead.',
				node.pos)
		} else if from_type.is_any_kind_of_pointer() {
			snexpr := node.expr.str()
			ft := c.table.type_to_str(from_type)
			c.error('cannot cast pointer type `${ft}` to string, use `&u8(${snexpr}).vstring()` or `cstring_to_vstring(${snexpr})` instead.',
				node.pos)
		} else if from_type.is_number() {
			snexpr := node.expr.str()
			c.error('cannot cast number to string, use `${snexpr}.str()` instead.', node.pos)
		} else if from_sym.kind == .alias && final_from_sym.name != 'string' {
			ft := c.table.type_to_str(from_type)
			c.error('cannot cast type `${ft}` to string, use `x.str()` instead.', node.pos)
		} else if final_from_sym.kind == .array {
			snexpr := node.expr.str()
			if final_from_sym.name == '[]u8' {
				c.error('cannot cast []u8 to string, use `${snexpr}.bytestr()` or `${snexpr}.str()` instead.',
					node.pos)
			} else {
				first_elem_idx := '[0]'
				c.error('cannot cast array to string, use `${snexpr}${first_elem_idx}.str()` instead.',
					node.pos)
			}
		} else if final_from_sym.kind == .enum {
			snexpr := node.expr.str()
			c.error('cannot cast enum to string, use ${snexpr}.str() instead.', node.pos)
		} else if final_from_sym.kind == .map {
			c.error('cannot cast map to string.', node.pos)
		} else if final_from_sym.kind == .sum_type {
			snexpr := node.expr.str()
			ft := c.table.type_to_str(from_type)
			c.error('cannot cast sumtype `${ft}` to string, use `${snexpr}.str()` instead.',
				node.pos)
		} else if final_from_sym.kind == .function {
			fnexpr := node.expr.str()
			c.error('cannot cast function `${fnexpr}` to string', node.pos)
		} else if to_type != ast.string_type && from_type == ast.string_type
			&& (!(to_sym.kind == .alias && final_to_sym.name == 'string')) {
			mut error_msg := 'cannot cast a string to a type `${final_to_sym.name}`, that is not an alias of string'
			if mut node.expr is ast.StringLiteral {
				if node.expr.val.len == 1 {
					error_msg += ", for denoting characters use `${node.expr.val}` instead of '${node.expr.val}'"
				}
			}
			c.error(error_msg, node.pos)
		}
	} else if to_type.is_int() && mut node.expr is ast.IntegerLiteral {
		tt := c.table.type_to_str(to_type)
		tsize, _ := c.table.type_size(to_type.idx_type())
		bit_size := tsize * 8
		signed := node.expr.val[0] == `-`
		value_string := match node.expr.val[0] {
			`-`, `+` {
				node.expr.val[1..]
			}
			else {
				node.expr.val
			}
		}
		mut is_overflowed := false
		v, e := strconv.common_parse_uint2(value_string, 0, bit_size)
		match e {
			0 {}
			-3 {
				// FIXME: Once integer literal is considered as hard error, remove this warn and migrate to later error
				c.error('value `${node.expr.val}` overflows `${tt}`', node.pos)
				is_overflowed = true
			}
			else {
				c.error('cannot cast value `${node.expr.val}` to `${tt}`', node.pos)
			}
		}

		// checks if integer literal's most significant bit
		// alters sign bit when casting to signed integer
		if !is_overflowed && to_type.is_signed() {
			signed_one := match to_type.idx() {
				ast.char_type_idx, ast.i8_type_idx {
					u64(0xff)
				}
				ast.i16_type_idx {
					u64(0xffff)
				}
				ast.int_type_idx, ast.i32_type_idx {
					u64(0xffffffff)
				}
				ast.i64_type_idx {
					u64(0xffffffffffffffff)
				}
				ast.isize_type_idx {
					$if x64 {
						u64(0xffffffffffffffff)
					} $else {
						u64(0xffffffff)
					}
				}
				else {
					c.error('ICE: Not a valid signed type', node.pos)
					0
				}
			}
			max_signed := (u64(1) << (bit_size - 1)) - 1

			is_overflowed = v == signed_one || (signed && v - 2 == max_signed)
				|| (!signed && v - 1 == max_signed)
			// FIXME: Once integer literal is considered as hard error, remove this warn and migrate to later error
			if is_overflowed {
				c.warn('value `${node.expr.val}` overflows `${tt}`, this will be considered hard error soon',
					node.pos)
			}
		}

		// FIXME: Once integer literal is considered as hard error, uncomment this
		// if is_overflowed {
		// 	c.error('value `${node.expr.val}` overflows `${tt}`', node.pos)
		// }
	} else if to_type.is_float() && mut node.expr is ast.FloatLiteral {
		tt := c.table.type_to_str(to_type)
		strconv.atof64(node.expr.val) or {
			c.error('cannot cast value `${node.expr.val}` to `${tt}`', node.pos)
		}
	}
	if from_sym.language == .v && !from_type.is_ptr()
		&& final_from_sym.kind in [.sum_type, .interface]
		&& final_to_sym.kind !in [.sum_type, .interface] {
		ft := c.table.type_to_str(from_type)
		tt := c.table.type_to_str(to_type)
		kind_name := if from_sym.kind == .sum_type { 'sum type' } else { 'interface' }
		c.error('cannot cast `${ft}` ${kind_name} value to `${tt}`, use `${node.expr} as ${tt}` instead',
			node.pos)
	}
	if from_sym.language == .v && from_type.is_ptr() && !to_type.is_ptr() && !final_to_type.is_ptr()
		&& !node.expr.is_auto_deref_var() && final_to_sym.kind == .struct
		&& final_from_sym.kind == .struct {
		if c.check_struct_signature(final_from_sym.info as ast.Struct, final_to_sym.info as ast.Struct) {
			ft := c.table.type_to_str(from_type)
			tt := c.table.type_to_str(to_type)
			c.error('cannot cast `${ft}` to `${tt}`, you must dereference it first (e.g. ${tt}(*var))',
				node.pos)
		}
	}

	if node.has_arg {
		c.expr(mut node.arg)
	}

	// checks on int literal to enum cast if the value represents a value on the enum
	if to_sym.kind == .enum {
		if mut node.expr is ast.IntegerLiteral {
			enum_typ_name := c.table.get_type_name(to_type)
			node_val := node.expr.val.i64()

			if enum_decl := c.table.enum_decls[to_sym.name] {
				mut in_range := false
				if enum_decl.is_flag {
					if enum_decl.fields.len == 64 {
						// for 64 fields, just use max_u64 and avoid UB:
						in_range = node_val >= 0 && u64(node_val) <= max_u64
					} else {
						// if a flag enum has 4 variants, the maximum possible value would have all 4 flags set (0b1111)
						max_val := (u64(1) << enum_decl.fields.len) - 1
						in_range = node_val >= 0 && u64(node_val) <= max_val
					}
				} else {
					mut enum_val := i64(0)

					for enum_field in enum_decl.fields {
						// check if the field of the enum value is an integer literal
						if enum_field.expr is ast.IntegerLiteral {
							enum_val = enum_field.expr.val.i64()
						} else if comptime_value := c.eval_comptime_const_expr(enum_field.expr,
							0)
						{
							enum_val = comptime_value.i64() or { -1 }
						}
						if node_val == enum_val {
							in_range = true
							break
						}
						enum_val += 1
					}
				}

				if !in_range {
					c.warn('${node_val} does not represent a value of enum ${enum_typ_name}',
						node.pos)
				}
			}
		}
		if node.expr_type == ast.string_type_idx {
			c.add_error_detail('use ${c.table.type_to_str(node.typ)}.from_string(${node.expr}) instead')
			c.error('cannot cast `string` to `enum`', node.pos)
		}
	}

	if c.pref.warn_about_allocs && to_sym.info is ast.Interface {
		c.warn_alloc('cast to interface', node.pos)
	}
	node.typname = c.table.sym(node.typ).name
	return node.typ
}

fn (mut c Checker) at_expr(mut node ast.AtExpr) ast.Type {
	match node.kind {
		.fn_name {
			if c.table.cur_fn == unsafe { nil } {
				return ast.void_type
			}
			if c.table.cur_fn.is_static_type_method {
				node.val = c.table.cur_fn.name.all_after_last('__static__')
			} else {
				node.val = c.table.cur_fn.name.all_after_last('.')
			}
		}
		.method_name {
			if c.table.cur_fn == unsafe { nil } {
				return ast.void_type
			}
			fname := c.table.cur_fn.name.all_after_last('.')
			if c.table.cur_fn.is_method {
				node.val = c.table.type_to_str(c.table.cur_fn.receiver.typ).all_after_last('.') +
					'.' + fname
			} else if c.table.cur_fn.is_static_type_method {
				node.val = fname.all_before('__static__') + '.' + fname.all_after('__static__')
			} else {
				node.val = fname
			}
		}
		.mod_name {
			if c.table.cur_fn == unsafe { nil } {
				return ast.void_type
			}
			node.val = c.table.cur_fn.mod
		}
		.struct_name {
			if c.table.cur_fn.is_method || c.table.cur_fn.is_static_type_method {
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
		.file_dir {
			node.val = os.real_path(os.dir(c.file.path))
		}
		.line_nr {
			node.val = (node.pos.line_nr + 1).str()
		}
		.file_path_line_nr {
			node.val = os.file_name(c.file.path) + ':' + (node.pos.line_nr + 1).str()
		}
		.column_nr {
			node.val = (node.pos.col + 1).str()
		}
		.location {
			mut mname := 'unknown'
			if c.table.cur_fn != unsafe { nil } {
				if c.table.cur_fn.is_method {
					mname = c.table.type_to_str(c.table.cur_fn.receiver.typ) + '{}.' +
						c.table.cur_fn.name.all_after_last('.')
				} else {
					mname = c.table.cur_fn.name
				}
				if c.table.cur_fn.is_static_type_method {
					mname = mname.replace('__static__', '.') + ' (static)'
				}
			}
			node.val = c.file.path + ':' + (node.pos.line_nr + 1).str() + ', ${mname}'
		}
		.vhash {
			node.val = version.vhash()
		}
		.v_current_hash {
			node.val = c.v_current_commit_hash
		}
		.vmod_file {
			// cache the vmod content, do not read it many times
			if c.vmod_file_content.len == 0 {
				mut mcache := vmod.get_cache()
				vmod_file_location := mcache.get_by_file(c.file.path)
				if vmod_file_location.vmod_file.len == 0 {
					c.error('@VMOD_FILE can only be used in projects that have a v.mod file',
						node.pos)
				}
				vmod_content := os.read_file(vmod_file_location.vmod_file) or { '' }
				c.vmod_file_content = vmod_content.replace('\r\n', '\n') // normalise EOLs just in case
			}
			node.val = c.vmod_file_content
		}
		.vroot_path {
			node.val = c.pref.vroot
		}
		.vexeroot_path {
			node.val = c.pref.vroot
		}
		.vmodroot_path {
			mut mcache := vmod.get_cache()
			vmod_file_location := mcache.get_by_file(c.file.path)
			node.val = os.dir(vmod_file_location.vmod_file)
		}
		.vmod_hash {
			mut mcache := vmod.get_cache()
			vmod_file_location := mcache.get_by_file(c.file.path)
			if vmod_file_location.vmod_file.len == 0 {
				c.error('@VMODHASH can only be used in projects that have a v.mod file',
					node.pos)
			}
			hash := version.githash(os.dir(vmod_file_location.vmod_file)) or {
				c.error(err.msg(), node.pos)
				''
			}
			node.val = hash
		}
		.build_date {
			node.val = util.stable_build_time.strftime('%Y-%m-%d')
		}
		.build_time {
			node.val = util.stable_build_time.strftime('%H:%M:%S')
		}
		.build_timestamp {
			node.val = util.stable_build_time.unix().str()
		}
		.unknown {
			c.error('unknown @ identifier: ${node.name}. Available identifiers: ${token.valid_at_tokens}',
				node.pos)
		}
	}
	return ast.string_type
}

fn (mut c Checker) resolve_var_fn(func &ast.Fn, mut node ast.Ident, name string) ast.Type {
	mut fn_type := c.table.find_or_register_fn_type(func, false, true)
	if fn_type < 0 {
		mut f := ast.Fn{
			...func
		}
		f.name = ''
		fn_type = c.table.find_or_register_fn_type(f, false, true)
	}
	if func.generic_names.len > 0 {
		concrete_types := node.concrete_types.map(c.unwrap_generic(it))
		if typ_ := c.table.convert_generic_type(fn_type, func.generic_names, concrete_types) {
			fn_type = typ_
			if concrete_types.all(!it.has_flag(.generic)) {
				c.table.register_fn_concrete_types(func.fkey(), concrete_types)
			}
		}
	}
	node.name = name
	node.kind = .function
	node.info = ast.IdentFn{
		typ: fn_type
	}
	return fn_type
}

fn (mut c Checker) ident(mut node ast.Ident) ast.Type {
	if c.pref.linfo.is_running {
		// Mini LS hack (v -line-info "a.v:16")
		c.ident_autocomplete(node)
	}
	// TODO: move this
	if c.const_deps.len > 0 {
		mut name := node.name
		if !name.contains('.') && node.mod != 'builtin' {
			name = '${node.mod}.${node.name}'
		}
		// detect cycles, while allowing for references to the same constant,
		// used inside its initialisation like: `struct Abc { x &Abc } ... const a = [ Abc{0}, Abc{unsafe{&a[0]}} ]!`
		// see vlib/v/tests/const_fixed_array_containing_references_to_itself_test.v
		if unsafe { c.const_var != 0 } && name == c.const_var.name {
			if mut c.const_var.expr is ast.ArrayInit && c.const_var.expr.is_fixed
				&& c.expected_type.nr_muls() > 0 {
				elem_typ := c.expected_type.deref()
				node.kind = .constant
				node.name = c.const_var.name
				node.info = ast.IdentVar{
					typ: elem_typ
				}
				// c.const_var.typ = elem_typ
				node.obj = c.const_var
				return c.expected_type
			}
			c.error('cycle in constant `${c.const_var.name}`', node.pos)
			return ast.void_type
		}
		c.const_deps << name
	}
	if node.kind == .blank_ident {
		if node.tok_kind !in [.assign, .decl_assign] {
			c.error('undefined ident: `_` (may only be used in assignments)', node.pos)
		}
		return ast.void_type
	} else if node.kind in [.constant, .global, .variable] {
		// second use
		info := node.info as ast.IdentVar
		typ := c.type_resolver.get_type_or_default(node, info.typ)
		// Got a var with type T, return current generic type
		if node.or_expr.kind != .absent {
			if !info.typ.has_flag(.option) {
				if node.or_expr.kind == .propagate_option {
					c.error('cannot use `?` on non-option variable', node.pos)
				} else if node.or_expr.kind == .block {
					c.error('cannot use `or {}` block on non-option variable', node.pos)
				}
			}
			unwrapped_typ := typ.clear_option_and_result()
			c.expected_or_type = unwrapped_typ
			c.stmts_ending_with_expression(mut node.or_expr.stmts, c.expected_or_type)
			c.check_or_expr(node.or_expr, typ, c.expected_or_type, node)
			return unwrapped_typ
		}
		return typ
	} else if node.kind == .function {
		info := node.info as ast.IdentFn
		if func := c.table.find_fn(node.name) {
			if func.generic_names.len > 0 {
				if node.concrete_types.len == 0 {
					c.error('`${node.name}` is a generic fn, you should pass its concrete types, e.g. ${node.name}[int]',
						node.pos)
				}
				concrete_types := node.concrete_types.map(c.unwrap_generic(it))
				if concrete_types.all(!it.has_flag(.generic)) {
					c.table.register_fn_concrete_types(func.fkey(), concrete_types)
				}
			}
		}
		return info.typ
	} else if node.kind == .unresolved {
		// first use
		if node.tok_kind == .assign && node.is_mut {
			c.error('`mut` is not allowed with `=` (use `:=` to declare a variable)',
				node.pos)
		}
		c.markused_external_type(!c.is_builtin_mod && node.language == .v && node.name.contains('.'))
		if mut obj := node.scope.find(node.name) {
			match mut obj {
				ast.GlobalField {
					if node.mod == '' {
						node.kind = .global
						node.info = ast.IdentVar{
							typ: obj.typ
						}
						node.obj = obj
						return obj.typ
					}
				}
				ast.Var {
					// inside vweb tmpl ident positions are meaningless, use the position of the comptime call.
					// if the variable is declared before the comptime call then we can assume all is well.
					// `node.name !in node.scope.objects && node.scope.start_pos < c.comptime_call_pos` (inherited)
					node_pos := if c.pref.is_vweb && node.name !in node.scope.objects
						&& node.scope.start_pos < c.comptime_call_pos {
						c.comptime_call_pos
					} else {
						node.pos.pos
					}
					if node_pos < obj.pos.pos {
						c.error('undefined variable `${node.name}` (used before declaration)',
							node.pos)
					}
					is_sum_type_cast := obj.smartcasts.len != 0
						&& !c.prevent_sum_type_unwrapping_once
					c.prevent_sum_type_unwrapping_once = false
					mut typ := if is_sum_type_cast { obj.smartcasts.last() } else { obj.typ }
					if typ == 0 {
						if mut obj.expr is ast.Ident {
							if obj.expr.kind == .unresolved {
								c.error('unresolved variable: `${node.name}`', node.pos)
								return ast.void_type
							}
						}
						if mut obj.expr is ast.IfGuardExpr {
							// new variable from if guard shouldn't have the option flag for further use
							// a temp variable will be generated which unwraps it
							sym := c.table.sym(obj.expr.expr_type)
							if sym.kind == .multi_return {
								mr_info := sym.info as ast.MultiReturn
								if mr_info.types.len == obj.expr.vars.len {
									for vi, var in obj.expr.vars {
										if var.name == node.name {
											typ = mr_info.types[vi]
										}
									}
								}
							} else {
								typ = obj.expr.expr_type.clear_option_and_result()
							}
						} else if obj.expr is ast.EmptyExpr {
							c.error('invalid variable `${node.name}`', node.pos)
							typ = ast.void_type
						} else {
							typ = c.expr(mut obj.expr)
						}
					}
					if c.inside_interface_deref && c.table.is_interface_var(obj) {
						typ = typ.deref()
					}
					is_option := typ.has_option_or_result() || node.or_expr.kind != .absent
					node.kind = .variable
					node.info = ast.IdentVar{
						typ:       typ
						is_option: is_option
					}
					if !is_sum_type_cast {
						obj.typ = typ
					}
					node.obj = obj
					node.ct_expr = obj.ct_type_var != .no_comptime
					// unwrap option (`println(x)`)
					if is_option {
						if node.or_expr.kind == .absent {
							return typ.clear_flag(.result)
						}
						if !typ.has_flag(.option) {
							if node.or_expr.kind == .propagate_option {
								c.error('cannot use `?` on non-option variable', node.pos)
							} else if node.or_expr.kind == .block {
								c.error('cannot use `or {}` block on non-option variable',
									node.pos)
							}
						}
						unwrapped_typ := typ.clear_option_and_result()
						c.expected_or_type = unwrapped_typ
						c.stmts_ending_with_expression(mut node.or_expr.stmts, c.expected_or_type)
						c.check_or_expr(node.or_expr, typ, c.expected_or_type, node)
						return unwrapped_typ
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
			name = '${node.mod}.${node.name}'
		}
		if mut obj := c.file.global_scope.find(name) {
			match mut obj {
				ast.GlobalField {
					node.kind = .global
					node.info = ast.IdentVar{
						typ: obj.typ
					}
					node.obj = obj
					return obj.typ
				}
				ast.ConstField {
					if !(obj.is_pub || obj.mod == c.mod || c.pref.is_test) {
						c.error('constant `${obj.name}` is private', node.pos)
					}
					mut typ := obj.typ
					if typ == 0 {
						old_c_mod := c.mod
						c.mod = obj.mod
						c.inside_const = true
						typ = c.expr(mut obj.expr)
						c.inside_const = false
						c.mod = old_c_mod

						if mut obj.expr is ast.CallExpr {
							if obj.expr.or_block.kind != .absent {
								typ = typ.clear_option_and_result()
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

					if obj.attrs.contains('deprecated') && obj.mod != c.mod {
						c.deprecate('const', obj.name, obj.attrs, node.pos)
					}

					if node.or_expr.kind != .absent {
						unwrapped_typ := typ.clear_option_and_result()
						c.expected_or_type = unwrapped_typ
						c.stmts_ending_with_expression(mut node.or_expr.stmts, c.expected_or_type)
						c.check_or_expr(node.or_expr, typ, c.expected_or_type, node)
					}
					return typ
				}
				else {}
			}
		}
		// Non-anon-function object (not a call), e.g. `onclick(my_click)`
		if func := c.table.find_fn(name) {
			if func.generic_names.len > 0 {
				if node.concrete_types.len == 0 {
					c.error('`${node.name}` is a generic fn, you should pass its concrete types, e.g. ${node.name}[int]',
						node.pos)
				}
				mut has_generic := false // foo[T] instead of foo[int]
				for concrete_type in node.concrete_types {
					if concrete_type.has_flag(.generic) {
						has_generic = true
					}
				}
				if c.table.cur_fn != unsafe { nil } && c.table.cur_concrete_types.len == 0
					&& has_generic && c.table.sym(c.expected_type).kind != .function {
					c.error('a generic fn with generic types, cannot be used outside of another generic fn',
						node.pos)
				}
			}
			return c.resolve_var_fn(func, mut node, name)
		}
	}
	if node.language == .c {
		if node.name == 'C.NULL' {
			return ast.voidptr_type
		}
		if x := c.table.global_scope.find_const(node.name) {
			return x.typ
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
		if node.obj is ast.ConstField {
			field := node.obj as ast.ConstField
			if field.attrs.contains('deprecated') && field.mod != c.mod {
				c.deprecate('const', field.name, field.attrs, node.pos)
			}
		}
		if builtin_type != ast.void_type {
			return builtin_type
		}
		node.mod = saved_mod
	}
	if node.tok_kind == .assign {
		c.error('undefined ident: `${node.name}` (use `:=` to declare a variable)', node.pos)
	} else if node.name == 'errcode' {
		c.error('undefined ident: `errcode`; did you mean `err.code`?', node.pos)
	} else {
		if c.inside_ct_attr {
			c.note('`[if ${node.name}]` is deprecated. Use `@[if ${node.name}?]` instead',
				node.pos)
		} else {
			cname_mod := node.name.all_before('.')
			if cname_mod.len != node.name.len {
				mut const_names_in_mod := []string{}
				for _, so in c.table.global_scope.objects {
					if so is ast.ConstField {
						if so.mod == cname_mod {
							const_names_in_mod << so.name
						}
					}
				}
				c.error(util.new_suggestion(node.name, const_names_in_mod).say('undefined ident: `${node.name}`'),
					node.pos)
			} else {
				// If a variable is not found in the scope of an anonymous function
				// but is in an external scope, then we can suggest the user add it to the capturing list.
				if c.inside_anon_fn {
					found_var := c.fn_scope.find_var(node.name)

					if found_var != none {
						if c.inside_lambda {
							// Lambdas don't support capturing variables yet, so that's the only hint.
							c.error('undefined variable `${node.name}`', node.pos)
						} else {
							c.add_error_detail('use `fn [${node.name}] () {` instead of `fn () {`')
							c.error('`${node.name}` must be explicitly listed as inherited variable to be used inside a closure',
								node.pos)
						}
						return ast.void_type
					}
				}

				c.error('undefined ident: `${node.name}`', node.pos)
			}
		}
	}
	return ast.void_type
}

fn (mut c Checker) concat_expr(mut node ast.ConcatExpr) ast.Type {
	mut mr_types := []ast.Type{}
	for mut expr in node.vals {
		mut typ := c.expr(mut expr)
		if typ == ast.nil_type {
			// nil and voidptr produces the same struct type name
			typ = ast.voidptr_type
		}
		mr_types << typ
	}
	if node.vals.len == 1 {
		typ := mr_types[0]
		node.return_type = typ
		return typ
	} else {
		for i := 0; i < mr_types.len; i++ {
			if mr_types[i] == ast.void_type {
				c.error('type `void` cannot be used in multi-return', node.vals[i].pos())
				return ast.void_type
			}
		}
		typ := c.table.find_or_register_multi_return(mr_types)
		ast.new_type(typ)
		node.return_type = typ
		return typ
	}
}

// smartcast takes the expression with the current type which should be smartcasted to the target type in the given scope
fn (mut c Checker) smartcast(mut expr ast.Expr, cur_type ast.Type, to_type_ ast.Type, mut scope ast.Scope,
	is_comptime bool, is_option_unwrap bool) {
	sym := c.table.sym(cur_type)
	to_type := if sym.kind == .interface && c.table.sym(to_type_).kind != .interface {
		to_type_.ref()
	} else {
		to_type_
	}
	match mut expr {
		ast.SelectorExpr {
			mut is_mut := false
			mut smartcasts := []ast.Type{}
			expr_sym := c.table.sym(expr.expr_type)
			mut orig_type := ast.no_type
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
			mut expr_str := expr.expr.str()
			if mut expr.expr is ast.ParExpr && expr.expr.expr is ast.AsCast {
				expr_str = expr.expr.expr.expr.str()
			}
			field := scope.find_struct_field(expr_str, expr.expr_type, expr.field_name)
			if field != unsafe { nil } {
				smartcasts << field.smartcasts
			}
			// smartcast either if the value is immutable or if the mut argument is explicitly given
			if !is_mut || expr.is_mut || is_option_unwrap || orig_type.has_flag(.option) {
				smartcasts << to_type
				scope.register_struct_field(expr_str, ast.ScopeStructField{
					struct_type: expr.expr_type
					name:        expr.field_name
					typ:         cur_type
					smartcasts:  smartcasts
					pos:         expr.pos
					orig_type:   orig_type
				})
			} else {
				c.smartcast_mut_pos = expr.pos
			}
		}
		ast.Ident {
			mut is_mut := false
			mut smartcasts := []ast.Type{}
			mut is_already_casted := false
			mut orig_type := 0
			mut is_inherited := false
			mut ct_type_var := ast.ComptimeVarKind.no_comptime
			mut is_ct_type_unwrapped := false
			if mut expr.obj is ast.Var {
				is_ct_type_unwrapped = expr.obj.ct_type_var != ast.ComptimeVarKind.no_comptime
				is_mut = expr.obj.is_mut
				smartcasts << expr.obj.smartcasts
				is_already_casted = expr.obj.pos.pos == expr.pos.pos
				if orig_type == 0 {
					orig_type = expr.obj.typ
				}
				is_inherited = expr.obj.is_inherited
				ct_type_var = if is_comptime {
					.smartcast
				} else if c.table.type_kind(to_type_) == .aggregate {
					.aggregate
				} else {
					.no_comptime
				}
			}
			// smartcast either if the value is immutable or if the mut argument is explicitly given
			if (!is_mut || expr.is_mut || is_option_unwrap) && !is_already_casted {
				smartcasts << to_type
				if var := scope.find_var(expr.name) {
					if is_comptime && var.ct_type_var == .smartcast {
						if cur_type.has_flag(.option) && !to_type.has_flag(.option) {
							if !var.is_unwrapped {
								scope.register(ast.Var{
									name:              expr.name
									typ:               cur_type
									pos:               expr.pos
									is_used:           true
									is_mut:            expr.is_mut
									is_inherited:      is_inherited
									smartcasts:        [to_type]
									orig_type:         orig_type
									ct_type_var:       ct_type_var
									ct_type_unwrapped: is_ct_type_unwrapped
									is_unwrapped:      true
								})
							} else {
								scope.update_smartcasts(expr.name, to_type, true)
							}
						} else {
							scope.update_smartcasts(expr.name, to_type, false)
						}
						return
					}
				}
				scope.register(ast.Var{
					name:              expr.name
					typ:               cur_type
					pos:               expr.pos
					is_used:           true
					is_mut:            expr.is_mut
					is_inherited:      is_inherited
					is_unwrapped:      is_option_unwrap
					smartcasts:        smartcasts
					orig_type:         orig_type
					ct_type_var:       ct_type_var
					ct_type_unwrapped: is_ct_type_unwrapped
				})
			} else if is_mut && !expr.is_mut {
				c.smartcast_mut_pos = expr.pos
			}
		}
		else {
			c.smartcast_cond_pos = expr.pos()
		}
	}
}

fn (mut c Checker) select_expr(mut node ast.SelectExpr) ast.Type {
	node.is_expr = c.expected_type != ast.void_type
	node.expected_type = c.expected_type
	for mut branch in node.branches {
		c.stmt(mut branch.stmt)
		match mut branch.stmt {
			ast.ExprStmt {
				if branch.is_timeout {
					if !branch.stmt.typ.is_int() {
						tsym := c.table.sym(branch.stmt.typ)
						c.error('invalid type `${tsym.name}` for timeout - expected integer number of nanoseconds aka `time.Duration`',
							branch.stmt.pos)
					}
				} else {
					if mut branch.stmt.expr is ast.InfixExpr {
						if branch.stmt.expr.left !in [ast.Ident, ast.SelectorExpr, ast.IndexExpr] {
							c.error('channel in `select` key must be predefined', branch.stmt.expr.left.pos())
						}
					} else {
						c.error('invalid expression for `select` key', branch.stmt.expr.pos())
					}
				}
			}
			ast.AssignStmt {
				expr := branch.stmt.right[0]
				match expr {
					ast.PrefixExpr {
						if expr.right !in [ast.Ident, ast.SelectorExpr, ast.IndexExpr] {
							c.error('channel in `select` key must be predefined', expr.right.pos())
						}
						if expr.or_block.kind != .absent {
							err_prefix := if expr.or_block.kind == .block {
								'or block'
							} else {
								'error propagation'
							}
							c.error('${err_prefix} not allowed in `select` key', expr.or_block.pos)
						}
					}
					else {
						c.error('`<-` receive expression expected', branch.stmt.right[0].pos())
					}
				}
				if mut branch.stmt.left[0] is ast.Ident {
					ident := branch.stmt.left[0] as ast.Ident
					if ident.kind == .blank_ident && branch.stmt.op != .decl_assign {
						c.error('cannot send on `_`, use `_ := <- quit` instead', branch.stmt.left[0].pos())
					}
				}
			}
			else {
				if !branch.is_else {
					c.error('receive or send statement expected as `select` key', branch.stmt.pos)
				}
			}
		}
		c.stmts(mut branch.stmts)
	}
	return ast.bool_type
}

fn (mut c Checker) lock_expr(mut node ast.LockExpr) ast.Type {
	expected_type := c.expected_type
	if c.rlocked_names.len > 0 || c.locked_names.len > 0 {
		c.error('nested `lock`/`rlock` not allowed', node.pos)
	}
	for i in 0 .. node.lockeds.len {
		mut expr_ := node.lockeds[i]
		e_typ := c.expr(mut expr_)
		id_name := node.lockeds[i].str()
		if !e_typ.has_flag(.shared_f) {
			obj_type := if node.lockeds[i] is ast.Ident { 'variable' } else { 'struct element' }
			c.error('`${id_name}` must be declared as `shared` ${obj_type} to be locked',
				node.lockeds[i].pos())
		}
		if id_name in c.locked_names {
			c.error('`${id_name}` is already locked', node.lockeds[i].pos())
		} else if id_name in c.rlocked_names {
			c.error('`${id_name}` is already read-locked', node.lockeds[i].pos())
		}
		if node.is_rlock[i] {
			c.rlocked_names << id_name
		} else {
			c.locked_names << id_name
		}
	}
	c.stmts(mut node.stmts)
	// handle `x := rlock a { a.getval() }`
	mut ret_type := ast.void_type
	if node.stmts.len > 0 {
		mut last_stmt := node.stmts.last()
		if mut last_stmt is ast.ExprStmt {
			c.expected_type = expected_type
			ret_type = c.expr(mut last_stmt.expr)
		}
	}
	c.rlocked_names = []
	c.locked_names = []
	if ret_type != ast.void_type {
		node.is_expr = true
	}
	node.typ = ret_type
	return ret_type
}

fn (mut c Checker) unsafe_expr(mut node ast.UnsafeExpr) ast.Type {
	prev_unsafe := c.inside_unsafe
	c.inside_unsafe = true
	t := c.expr(mut node.expr)
	c.inside_unsafe = prev_unsafe
	return t
}

fn (mut c Checker) find_definition(ident ast.Ident) !ast.Expr {
	match ident.kind {
		.unresolved, .blank_ident { return error('none') }
		.variable, .constant { return c.find_obj_definition(ident.obj) }
		.global { return error('${ident.name} is a global variable') }
		.function { return error('${ident.name} is a function') }
	}
}

fn (mut c Checker) find_obj_definition(obj ast.ScopeObject) !ast.Expr {
	// TODO: remove once we have better type inference
	mut name := ''
	match obj {
		ast.EmptyScopeObject, ast.Var, ast.ConstField, ast.GlobalField, ast.AsmRegister { name = obj.name }
	}
	mut expr := ast.empty_expr
	if obj is ast.Var {
		if obj.is_mut {
			return error('`${name}` is mut and may have changed since its definition')
		}
		expr = obj.expr
	} else if obj is ast.ConstField {
		expr = obj.expr
	} else {
		return error('`${name}` is a global variable and is unknown at compile time')
	}
	if mut expr is ast.Ident {
		return c.find_definition(expr)
	}
	if mut expr is ast.ComptimeCall && expr.is_compile_value {
		if expr.result_type == ast.bool_type {
			return ast.BoolLiteral{
				val: expr.compile_value.bool()
			}
		}
	}
	if !expr.is_pure_literal() {
		return error('definition of `${name}` is unknown at compile time')
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

fn (mut c Checker) mark_as_referenced(mut node ast.Expr, as_interface bool) {
	match mut node {
		ast.Ident {
			if mut node.obj is ast.Var {
				mut obj := unsafe { &node.obj }
				if c.fn_scope != unsafe { nil } {
					obj = c.fn_scope.find_var(node.obj.name) or { obj }
				}
				if obj.typ == 0 {
					return
				}
				type_sym := c.table.sym(obj.typ.set_nr_muls(0))
				if obj.is_stack_obj && !type_sym.is_heap() && !type_sym.is_int()
					&& !c.pref.translated && !c.file.is_translated {
					suggestion := if type_sym.kind == .struct {
						'declaring `${type_sym.name}` as `@[heap]`'
					} else {
						'wrapping the `${type_sym.name}` object in a `struct` declared as `@[heap]`'
					}
					mischief := if as_interface { 'used as interface object' } else { 'referenced' }
					c.error('`${node.name}` cannot be ${mischief} outside `unsafe` blocks as it might be stored on stack. Consider ${suggestion}.',
						node.pos)
				} else if type_sym.kind == .array_fixed {
					c.error('cannot reference fixed array `${node.name}` outside `unsafe` blocks as it is supposed to be stored on stack',
						node.pos)
				} else {
					match type_sym.kind {
						.struct {
							info := type_sym.info as ast.Struct
							if !info.is_heap && !node.obj.is_inherited {
								node.obj.is_auto_heap = true
							}
						}
						.sum_type, .interface {}
						.function {
							if type_sym.info is ast.FnType {
								if type_sym.info.is_anon {
									node.obj.is_auto_heap = true
								}
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

fn (mut c Checker) get_base_name(node &ast.Expr) string {
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

fn (mut c Checker) prefix_expr(mut node ast.PrefixExpr) ast.Type {
	old_inside_ref_lit := c.inside_ref_lit
	c.inside_ref_lit = c.inside_ref_lit || node.op == .amp
	right_type := c.expr(mut node.right)
	c.inside_ref_lit = old_inside_ref_lit
	node.right_type = right_type
	mut expr := node.right
	expr = expr.remove_par()
	if node.op == .amp {
		if expr is ast.Nil {
			c.error('invalid operation: cannot take address of nil', expr.pos())
		}
		if mut node.right is ast.PrefixExpr {
			if node.right.op == .amp {
				c.error('unexpected `&`, expecting expression', node.right.pos)
			}
		} else if mut expr is ast.PrefixExpr {
			if expr.op == .amp {
				c.error('cannot take the address of this expression', expr.pos)
			}
		} else if mut expr is ast.SelectorExpr {
			if expr.expr.is_literal() {
				c.error('cannot take the address of a literal value', node.pos.extend(expr.pos))
			} else if expr.expr is ast.StructInit {
				c.error('should not create object instance on the heap to simply access a member',
					node.pos.extend(expr.pos))
			}
			right_sym := c.table.sym(right_type)
			expr_sym := c.table.sym(expr.expr_type)
			if expr_sym.kind == .struct && (expr_sym.info as ast.Struct).is_minify
				&& (expr.typ == ast.bool_type_idx || (right_sym.kind == .enum
				&& !(right_sym.info as ast.Enum).is_flag
				&& !(right_sym.info as ast.Enum).uses_exprs)) {
				c.error('cannot take the address of field in struct `${c.table.type_to_str(expr.expr_type)}`, which is tagged as `@[minify]`',
					node.pos.extend(expr.pos))
			}

			if expr.typ.has_flag(.option) {
				c.error('cannot take the address of an Option field', node.pos.extend(expr.pos))
			}
		}
	}
	// TODO: testing ref/deref strategy
	right_is_ptr := right_type.is_ptr()
	if node.op == .amp && (!right_is_ptr || (right_is_ptr && expr is ast.CallExpr)) {
		if expr in [ast.BoolLiteral, ast.CallExpr, ast.CharLiteral, ast.FloatLiteral, ast.IntegerLiteral,
			ast.InfixExpr, ast.StringLiteral, ast.StringInterLiteral] {
			c.error('cannot take the address of ${expr}', node.pos)
		}
		if mut expr is ast.Ident {
			if expr.kind == .constant && !c.inside_unsafe && c.pref.experimental {
				c.warn('cannot take the address of const outside `unsafe`', expr.pos)
			}
		}
		if expr is ast.SelectorExpr {
			typ_sym := c.table.sym(right_type)
			if typ_sym.kind == .map && !c.inside_unsafe {
				c.error('cannot take the address of map values outside `unsafe`', node.pos)
			}
		}
		if mut expr is ast.IndexExpr {
			typ_sym := c.table.sym(expr.left_type)
			mut is_mut := false
			if mut expr.left is ast.Ident {
				ident := expr.left
				// TODO: temporary, remove this
				ident_obj := ident.obj
				if ident_obj is ast.Var {
					is_mut = ident_obj.is_mut
				}
			}
			if !c.inside_unsafe {
				if typ_sym.kind == .array && is_mut {
					c.error('cannot take the address of mutable array elements outside unsafe blocks',
						expr.pos)
				}

				if typ_sym.kind == .map {
					c.error('cannot take the address of map values outside `unsafe`',
						expr.pos)
				}
			}
		}
		if !c.inside_fn_arg && !c.inside_unsafe {
			c.mark_as_referenced(mut &expr, false)
		}
		return right_type.ref()
	} else if node.op == .amp && expr !is ast.CastExpr {
		if !c.inside_fn_arg && !c.inside_unsafe {
			c.mark_as_referenced(mut &expr, false)
		}
		if expr.is_auto_deref_var() {
			return right_type
		} else {
			return right_type.ref()
		}
	}
	right_sym := c.table.final_sym(c.unwrap_generic(right_type))
	if node.op == .mul {
		if right_type.has_flag(.option) {
			c.error('type `?${right_sym.name}` is an Option, it must be unwrapped first; use `*var?` to do it',
				expr.pos())
		}
		if right_type.is_ptr() {
			return right_type.deref()
		}
		if !right_type.is_pointer() && !c.pref.translated && !c.file.is_translated {
			s := c.table.type_to_str(right_type)
			c.error('invalid indirect of `${s}`, the type `${right_sym.name}` is not a pointer',
				node.pos)
		}
		if right_type.is_voidptr() {
			c.error('cannot dereference to void', node.pos)
		}
		if mut expr is ast.Ident {
			if var := expr.scope.find_var('${expr.name}') {
				if var.expr is ast.UnsafeExpr {
					if var.expr.expr is ast.Nil {
						c.error('cannot deference a `nil` pointer', expr.pos)
					}
				}
			}
		}
	}
	if node.op == .bit_not && !c.pref.translated && !c.file.is_translated {
		if right_sym.info is ast.Enum && !right_sym.info.is_flag {
			c.error('operator `~` can only be used with `@[flag]` tagged enums', node.pos)
		}
		// Only check for int not enum as it is done above
		if !right_sym.is_int() && right_sym.info !is ast.Enum {
			c.type_error_for_operator('~', 'integer', right_sym.name, node.pos)
		}
	}
	if node.op == .not && right_sym.kind != .bool && !c.pref.translated && !c.file.is_translated {
		c.type_error_for_operator('!', 'bool', right_sym.name, node.pos)
	}
	// FIXME
	// there are currently other issues to investigate if right_type
	// is unwrapped directly as initialization, so do it here
	if node.op == .minus && !right_sym.is_number() {
		c.type_error_for_operator('-', 'numeric', right_sym.name, node.pos)
	}
	if node.op == .arrow {
		raw_right_sym := c.table.final_sym(right_type)
		if raw_right_sym.kind == .chan {
			c.stmts_ending_with_expression(mut node.or_block.stmts, c.expected_or_type)
			return raw_right_sym.chan_info().elem_type
		}
		c.type_error_for_operator('<-', '`chan`', raw_right_sym.name, node.pos)
	}
	return right_type
}

fn (mut c Checker) type_error_for_operator(op_label string, types_label string, found_type_label string,
	pos token.Pos) {
	c.error('operator `${op_label}` can only be used with ${types_label} types, but the value after `${op_label}` is of type `${found_type_label}` instead',
		pos)
}

fn (mut c Checker) check_index(typ_sym &ast.TypeSymbol, index ast.Expr, index_type ast.Type, pos token.Pos,
	range_index bool, is_gated bool) {
	if typ_sym.kind in [.array, .array_fixed, .string] {
		index_type_sym := c.table.sym(index_type)
		if !(index_type.is_int() || index_type_sym.kind == .enum
			|| (index_type_sym.kind == .alias
			&& (index_type_sym.info as ast.Alias).parent_type.is_int())
			|| (c.pref.translated && index_type.is_any_kind_of_pointer())) {
			type_str := if typ_sym.kind == .string {
				'non-integer string index `${c.table.type_to_str(index_type)}`'
			} else {
				'non-integer index `${c.table.type_to_str(index_type)}` (array type `${typ_sym.name}`)'
			}
			c.error('${type_str}', pos)
		}
		if index is ast.IntegerLiteral && !is_gated {
			if index.val[0] == `-` {
				c.error('negative index `${index.val}`', index.pos)
			} else if typ_sym.kind == .array_fixed {
				i := index.val.int()
				info := typ_sym.info as ast.ArrayFixed
				if (!range_index && i >= info.size) || (range_index && i > info.size) {
					c.error('index out of range (index: ${i}, len: ${info.size})', index.pos)
				}
			}
		}
		if index_type.has_option_or_result() {
			type_str := if typ_sym.kind == .string {
				'(type `${typ_sym.name}`)'
			} else {
				'(array type `${typ_sym.name}`)'
			}
			c.error('cannot use Option or Result as index ${type_str}', pos)
		}
	}
}

fn (mut c Checker) index_expr(mut node ast.IndexExpr) ast.Type {
	mut typ := c.expr(mut node.left)
	if typ == 0 {
		c.error('unknown type for expression `${node.left}`', node.pos)
		return typ
	}
	mut typ_sym := c.table.final_sym(typ)
	node.left_type = typ
	match typ_sym.kind {
		.map {
			node.is_map = true
		}
		.array {
			node.is_array = true
			if node.or_expr.kind != .absent && node.index is ast.RangeExpr {
				c.error('custom error handling on range expressions for arrays is not supported yet.',
					node.or_expr.pos)
			}
		}
		.array_fixed {
			node.is_farray = true
		}
		.any {
			unwrapped_typ := c.unwrap_generic(typ)
			unwrapped_sym := c.table.final_sym(unwrapped_typ)
			if unwrapped_sym.kind in [.map, .array, .array_fixed] {
				typ = unwrapped_typ
				typ_sym = unsafe { unwrapped_sym }
			}
		}
		.string {
			if node.is_gated && c.mod != 'strings' {
				c.table.used_features.range_index = true
			}
		}
		else {}
	}
	if !c.is_builtin_mod && c.mod !in ['strings', 'math.bits'] {
		if node.index is ast.RangeExpr {
			c.table.used_features.range_index = true
		}
		c.table.used_features.index = true
	}
	is_aggregate_arr := typ_sym.kind == .aggregate
		&& (typ_sym.info as ast.Aggregate).types.filter(c.table.type_kind(it) !in [.array, .array_fixed, .string, .map]).len == 0
	if typ_sym.kind !in [.array, .array_fixed, .string, .map]
		&& (!typ.is_ptr() || typ_sym.kind in [.sum_type, .interface])
		&& typ !in [ast.byteptr_type, ast.charptr_type] && !typ.has_flag(.variadic)
		&& !is_aggregate_arr {
		c.error('type `${typ_sym.name}` does not support indexing', node.pos)
	}
	if is_aggregate_arr {
		// treating indexexpr of sumtype of array types
		typ = (typ_sym.info as ast.Aggregate).types[0]
	}
	if typ.has_flag(.option) {
		if node.left is ast.Ident && node.left.or_expr.kind == .absent {
			c.error('type `?${typ_sym.name}` is an Option, it must be unwrapped first; use `var?[]` to do it',
				node.left.pos())
		} else if node.left is ast.CallExpr {
			c.error('type `?${typ_sym.name}` is an Option, it must be unwrapped with `func()?`, or use `func() or {default}`',
				node.left.pos())
		} else if node.left is ast.SelectorExpr && node.left.or_block.kind == .absent {
			c.error('type `?${typ_sym.name}` is an Option, it must be unwrapped first; use `${node.left}?` to do it',
				node.left.pos())
		}
	} else if typ.has_flag(.result) {
		c.error('type `!${typ_sym.name}` is a Result, it does not support indexing', node.left.pos())
	}
	if typ_sym.kind == .string && !typ.is_ptr() && node.is_setter {
		c.error('cannot assign to s[i] since V strings are immutable\n' +
			'(note, that variables may be mutable but string values are always immutable, like in Go and Java)',
			node.pos)
	}

	if !c.inside_unsafe && !c.is_builtin_mod && !c.inside_if_guard && !c.is_index_assign
		&& typ_sym.kind == .map && node.or_expr.stmts.len == 0 {
		elem_type := c.table.value_type(typ)
		if elem_type.is_any_kind_of_pointer() {
			c.warn('accessing a pointer map value requires an `or {}` block outside `unsafe`',
				node.pos)
		}
		mut checked_types := []ast.Type{}
		if c.is_contains_any_kind_of_pointer(elem_type, mut checked_types) {
			c.warn('accessing map value that contain pointers requires an `or {}` block outside `unsafe`',
				node.pos)
		}
	}

	if (typ.is_ptr() && !typ.has_flag(.shared_f)
		&& (!node.left.is_auto_deref_var() || (typ_sym.kind == .struct && typ_sym.name != 'array')))
		|| typ.is_pointer() {
		mut is_ok := false
		mut is_mut_struct := false
		if mut node.left is ast.Ident && node.left.obj is ast.Var {
			// `mut param []T` function parameter
			is_ok = node.left.obj.is_mut && node.left.obj.is_arg && !typ.deref().is_ptr()
				&& typ_sym.kind != .struct
			// `mut param Struct`
			is_mut_struct = node.left.obj.is_mut && node.left.obj.is_arg && typ_sym.kind == .struct
		}
		if !is_ok && node.index is ast.RangeExpr {
			s := c.table.type_to_str(typ)
			c.error('type `${s}` does not support slicing', node.pos)
		} else if is_mut_struct {
			c.error('type `mut ${typ_sym.name}` does not support slicing', node.pos)
		} else if !c.inside_unsafe && !is_ok && !c.pref.translated && !c.file.is_translated {
			c.error('pointer indexing is only allowed in `unsafe` blocks', node.pos)
		}
	}
	if mut node.index is ast.RangeExpr { // [1..2]
		if node.index.has_low {
			index_type := c.expr(mut node.index.low)
			c.check_index(typ_sym, node.index.low, index_type, node.pos, true, node.is_gated)
		}
		if node.index.has_high {
			index_type := c.expr(mut node.index.high)
			c.check_index(typ_sym, node.index.high, index_type, node.pos, true, node.is_gated)
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
			index_type := c.expr(mut node.index)
			key_type := c.unwrap_generic(info.key_type)
			if !c.check_types(index_type, key_type) {
				err := c.expected_msg(index_type, key_type)
				c.error('invalid key: ${err}', node.pos)
			}
			value_sym := c.table.sym(info.value_type)
			if !node.is_setter && value_sym.kind == .sum_type && node.or_expr.kind == .absent
				&& !c.inside_unsafe && !c.inside_if_guard {
				c.warn('`or {}` block required when indexing a map with sum type value',
					node.pos)
			}
		} else {
			index_type := c.expr(mut node.index)
			// for [1] case #[1] is not allowed!
			if node.is_gated == true {
				c.error('`#[]` allowed only for ranges', node.pos)
			}
			c.check_index(typ_sym, node.index, index_type, node.pos, false, false)
		}
		value_type := c.table.value_type(typ)
		if value_type != ast.void_type {
			typ = value_type
		}
	}
	if node.or_expr.stmts.len > 0 && node.or_expr.stmts.last() is ast.ExprStmt {
		c.expected_or_type = typ
	}
	c.stmts_ending_with_expression(mut node.or_expr.stmts, c.expected_or_type)
	c.check_expr_option_or_result_call(node, typ)
	return typ
}

// `.green` or `Color.green`
// If a short form is used, `expected_type` needs to be an enum
// with this value.
fn (mut c Checker) enum_val(mut node ast.EnumVal) ast.Type {
	mut typ_idx := if node.enum_name == '' {
		// Get the type of the enum without enum name by looking at the expected type.
		// e.g. `set_color(.green)`, V knows that `Green` is the expected type.
		if c.expected_type == ast.void_type && c.expected_expr_type != ast.void_type {
			c.expected_expr_type.idx()
		} else {
			c.expected_type.idx()
		}
	} else {
		c.table.find_type_idx(node.enum_name)
	}
	if typ_idx == 0 {
		// Handle `builtin` enums like `ChanState`, so that `x := ChanState.closed` works.
		// In the checker the name for such enums was set to `main.ChanState` instead of
		// just `ChanState`.
		if node.enum_name.starts_with('${c.mod}.') {
			typ_idx = c.table.find_type(node.enum_name['${c.mod}.'.len..])
			if typ_idx == 0 {
				c.error('unknown enum `${node.enum_name}` (type_idx=0)', node.pos)
				return ast.void_type
			}
		}
		if typ_idx == 0 {
			// the actual type is still unknown, produce an error, instead of panic:
			c.error('unknown enum `${node.enum_name}` (type_idx=0)', node.pos)
			return ast.void_type
		}
	}
	mut typ := ast.new_type(typ_idx)
	if c.pref.translated || c.file.is_translated {
		// TODO: make more strict
		node.typ = typ
		return typ
	}
	if typ == ast.void_type {
		c.error('not an enum', node.pos)
		return ast.void_type
	}
	mut typ_sym := c.table.sym(typ)
	if typ_sym.kind == .array && node.enum_name.len == 0 {
		array_info := typ_sym.info as ast.Array
		typ = array_info.elem_type
		typ_sym = c.table.sym(typ)
	}
	fsym := c.table.final_sym(typ)
	if fsym.kind != .enum && !c.pref.translated && !c.file.is_translated {
		// TODO: in C int fields can be compared to enums, need to handle that in C2V
		if typ_sym.kind == .placeholder {
			// If it's a placeholder, the type doesn't exist, print
			// an error that makes sense here.
			c.error('unknown type `${typ_sym.name}`', node.pos)
		} else {
			c.error('expected type is not an enum (`${typ_sym.name}`)', node.pos)
		}
		return ast.void_type
	}
	if fsym.info !is ast.Enum {
		c.error('not an enum', node.pos)
		return ast.void_type
	}
	if !(typ_sym.is_pub || typ_sym.mod == c.mod) {
		c.error('enum `${typ_sym.name}` is private', node.pos)
	}
	info := typ_sym.enum_info()
	if node.val !in info.vals {
		suggestion := util.new_suggestion(node.val, info.vals)
		c.error(suggestion.say('enum `${typ_sym.name}` does not have a value `${node.val}`'),
			node.pos)
	}
	node.typ = typ
	return typ
}

fn (mut c Checker) chan_init(mut node ast.ChanInit) ast.Type {
	if node.typ != 0 {
		info := c.table.sym(node.typ).chan_info()
		node.elem_type = info.elem_type
		if node.elem_type != 0 {
			elem_sym := c.table.sym(node.elem_type)
			if elem_sym.kind == .placeholder {
				c.error('unknown type `${elem_sym.name}`', node.elem_type_pos)
			}
		}
		if node.has_cap {
			c.check_array_init_para_type('cap', mut node.cap_expr, node.pos)
		}
		if c.pref.skip_unused && node.typ.has_flag(.generic) {
			c.table.used_features.comptime_syms[c.unwrap_generic(node.typ)] = true
		}
		return node.typ
	} else {
		c.error('`chan` of unknown type', node.pos)
		return node.typ
	}
}

fn (mut c Checker) offset_of(node ast.OffsetOf) ast.Type {
	sym := c.table.final_sym(node.struct_type)
	if sym.kind != .struct {
		c.error('first argument of __offsetof must be struct', node.pos)
		return ast.u32_type
	}
	if !c.table.struct_has_field(sym, node.field) {
		c.error('struct `${sym.name}` has no field called `${node.field}`', node.pos)
	}
	return ast.u32_type
}

fn (mut c Checker) check_dup_keys(node &ast.MapInit, i int) {
	key_i := node.keys[i]
	if key_i is ast.StringLiteral {
		for j in 0 .. i {
			key_j := node.keys[j]
			if key_j is ast.StringLiteral {
				if key_i.val == key_j.val {
					c.error('duplicate key "${key_i.val}" in map literal', key_i.pos)
				}
			}
		}
	} else if key_i is ast.IntegerLiteral {
		for j in 0 .. i {
			key_j := node.keys[j]
			if key_j is ast.IntegerLiteral {
				if key_i.val == key_j.val {
					c.error('duplicate key "${key_i.val}" in map literal', key_i.pos)
				}
			}
		}
	}
}

fn (c &Checker) check_struct_signature_init_fields(from ast.Struct, to ast.Struct, node ast.StructInit) bool {
	if node.init_fields.len == 0 {
		return from.fields.len == to.fields.len
	}

	mut count_not_in_from := 0
	for field in node.init_fields {
		filtered := from.fields.filter(it.name == field.name)
		if filtered.len != 1 {
			count_not_in_from++
		}
	}

	return (from.fields.len + count_not_in_from) == to.fields.len
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
			// field has different type
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

fn (mut c Checker) fetch_field_name(field ast.StructField) string {
	mut name := field.name
	for attr in field.attrs {
		if attr.kind == .string && attr.arg != '' && attr.name == 'sql' {
			name = attr.arg
			break
		}
	}
	sym := c.table.sym(field.typ)
	if sym.kind == .struct && sym.name != 'time.Time' {
		name = '${name}_id'
	}
	return name
}

fn (mut c Checker) ensure_generic_type_specify_type_names(typ ast.Type, pos token.Pos, is_container_typ bool, is_generic_container bool) bool {
	if typ == 0 {
		c.error('unknown type', pos)
		return false
	}

	c.ensure_generic_type_level++
	defer {
		c.ensure_generic_type_level--
	}
	if c.ensure_generic_type_level > expr_level_cutoff_limit {
		c.error('checker: too many levels of Checker.ensure_generic_type_specify_type_names calls: ${c.ensure_generic_type_level} ',
			pos)
		return false
	}

	sym := c.table.final_sym(typ)
	if c.ensure_generic_type_level > 38 {
		eprintln('>> c.ensure_generic_type_level: ${c.ensure_generic_type_level} > 38, in ${@METHOD}, typ: ${typ}, sym.kind: ${sym.kind}, pos: ${pos}')
	}
	match sym.kind {
		.function {
			fn_info := sym.info as ast.FnType
			if !c.ensure_generic_type_specify_type_names(fn_info.func.return_type, fn_info.func.return_type_pos,
				is_container_typ, is_generic_container) {
				return false
			}
			for param in fn_info.func.params {
				if !c.ensure_generic_type_specify_type_names(param.typ, param.type_pos,
					is_container_typ, is_generic_container) {
					return false
				}
			}
			if fn_info.func.generic_names.len > 0 && !typ.has_flag(.generic) {
				c.error('`${sym.name}` type is generic fn type, must specify the generic type names, e.g. ${sym.name}[T], ${sym.name}[int]',
					pos)
				return false
			}
		}
		.array {
			if !c.ensure_generic_type_specify_type_names((sym.info as ast.Array).elem_type,
				pos, true, typ.has_flag(.generic)) {
				return false
			}
		}
		.array_fixed {
			if !c.ensure_generic_type_specify_type_names((sym.info as ast.ArrayFixed).elem_type,
				pos, true, typ.has_flag(.generic)) {
				return false
			}
		}
		.map {
			info := sym.info as ast.Map
			if !c.ensure_generic_type_specify_type_names(info.key_type, pos, true, typ.has_flag(.generic)) {
				return false
			}
			if !c.ensure_generic_type_specify_type_names(info.value_type, pos, true, typ.has_flag(.generic)) {
				return false
			}
		}
		.sum_type {
			info := sym.info as ast.SumType
			if info.generic_types.len > 0 && ((is_container_typ && !is_generic_container)
				|| !typ.has_flag(.generic)) && info.concrete_types.len == 0 {
				c.error('`${sym.name}` type is generic sumtype, must specify the generic type names, e.g. ${sym.name}[T], ${sym.name}[int]',
					pos)
				return false
			}
		}
		.struct {
			info := sym.info as ast.Struct
			if info.generic_types.len > 0 && !typ.has_flag(.generic) && info.concrete_types.len == 0 {
				c.error('`${sym.name}` type is generic struct, must specify the generic type names, e.g. ${sym.name}[T], ${sym.name}[int]',
					pos)
				return false
			}
		}
		.interface {
			info := sym.info as ast.Interface
			if info.generic_types.len > 0 && !typ.has_flag(.generic) && info.concrete_types.len == 0 {
				c.error('`${sym.name}` type is generic interface, must specify the generic type names, e.g. ${sym.name}[T], ${sym.name}[int]',
					pos)
				return false
			}
		}
		.alias {
			info := sym.info as ast.Alias
			if !c.ensure_generic_type_specify_type_names(info.parent_type, pos, is_container_typ,
				is_generic_container) {
				return false
			}
		}
		else {}
	}
	return true
}

fn (mut c Checker) ensure_type_exists(typ ast.Type, pos token.Pos) bool {
	if typ == 0 {
		c.error('unknown type', pos)
		return false
	}
	c.type_level++
	defer {
		c.type_level--
	}
	if c.type_level > type_level_cutoff_limit {
		c.error('checker: too many levels of Checker.ensure_type_exists calls: ${c.type_level}, probably due to a self referencing type',
			pos)
		return false
	}
	sym := c.table.sym(typ)
	if !c.is_builtin_mod && !sym.is_pub && sym.mod != c.mod && sym.mod != 'main' {
		if sym.kind == .function {
			fn_info := sym.info as ast.FnType
			// hack: recover fn mod from func name
			mut fn_mod := sym.mod
			if fn_mod == '' {
				fn_mod = fn_info.func.name.all_before_last('.')
				if fn_mod == fn_info.func.name {
					fn_mod = 'builtin'
				}
			}
			if fn_mod != '' && fn_mod != c.mod && fn_info.func.name != '' && !fn_info.is_anon {
				c.error('function type `${fn_info.func.name}` was declared as private to module `${fn_mod}`, so it can not be used inside module `${c.mod}`',
					pos)
				return false
			}
		} else if sym.mod != '' {
			c.error('${sym.kind} `${sym.name}` was declared as private to module `${sym.mod}`, so it can not be used inside module `${c.mod}`',
				pos)
			return false
		}
	}
	match sym.kind {
		.placeholder {
			// if sym.language == .c && sym.name == 'C.time_t' {
			// TODO: temporary hack until we can define C aliases
			// return true
			//}
			// if sym.language == .v && !sym.name.starts_with('C.') {
			// if sym.language in [.v, .c] {
			if sym.language == .v {
				c.error(util.new_suggestion(sym.name, c.table.known_type_names()).say('unknown type `${sym.name}`'),
					pos)
				return false
			} else if sym.language == .c {
				if !c.pref.translated && !c.file.is_translated {
					c.warn(util.new_suggestion(sym.name, c.table.known_type_names()).say('unknown type `${sym.name}` (all virtual C types must be defined, this will be an error soon)'),
						pos)
				}
				// dump(sym)
				// for _, t in c.table.type_symbols {
				// println(t.name)
				//}
			}
		}
		.int_literal, .float_literal {
			// Separate error condition for `int_literal` and `float_literal` because `util.suggestion` may give different
			// suggestions due to f32 comparison issue.
			if !c.is_builtin_mod {
				msg := if sym.kind == .int_literal {
					'unknown type `${sym.name}`.\nDid you mean `int`?'
				} else {
					'unknown type `${sym.name}`.\nDid you mean `f64`?'
				}
				c.error(msg, pos)
				return false
			}
		}
		.function {
			fn_info := sym.info as ast.FnType
			if !c.ensure_type_exists(fn_info.func.return_type, fn_info.func.return_type_pos) {
				return false
			}
			for param in fn_info.func.params {
				if !c.ensure_type_exists(param.typ, param.type_pos) {
					return false
				}
			}
		}
		.array {
			if !c.ensure_type_exists((sym.info as ast.Array).elem_type, pos) {
				return false
			}
		}
		.array_fixed {
			if !c.ensure_type_exists((sym.info as ast.ArrayFixed).elem_type, pos) {
				return false
			}
		}
		.map {
			info := sym.info as ast.Map
			if !c.ensure_type_exists(info.key_type, pos) {
				return false
			}
			if !c.ensure_type_exists(info.value_type, pos) {
				return false
			}
		}
		.sum_type {
			info := sym.info as ast.SumType
			for concrete_typ in info.concrete_types {
				if !c.ensure_type_exists(concrete_typ, pos) {
					return false
				}
			}
		}
		.chan {
			if sym.info is ast.Chan {
				if !c.ensure_type_exists((sym.info as ast.Chan).elem_type, token.Pos{
					...pos
					col: pos.col + 5
				}) {
					return false
				}
			}
		}
		else {}
	}
	return true
}

// return true if a violation of a shared variable access rule is detected
fn (mut c Checker) fail_if_unreadable(expr ast.Expr, typ ast.Type, what string) bool {
	mut pos := token.Pos{}
	match expr {
		ast.Ident {
			if typ.has_flag(.shared_f) {
				if expr.name !in c.rlocked_names && expr.name !in c.locked_names {
					action := if what == 'argument' { 'passed' } else { 'used' }
					c.error('`${expr.name}` is `shared` and must be `rlock`ed or `lock`ed to be ${action} as non-mut ${what}',
						expr.pos)
					return true
				}
			}
			return false
		}
		ast.SelectorExpr {
			pos = expr.pos
			if typ.has_flag(.shared_f) {
				expr_name := '${expr.expr}.${expr.field_name}'
				if expr_name !in c.rlocked_names && expr_name !in c.locked_names {
					action := if what == 'argument' { 'passed' } else { 'used' }
					c.error('`${expr_name}` is `shared` and must be `rlock`ed or `lock`ed to be ${action} as non-mut ${what}',
						expr.pos)
					return true
				}
				return false
			} else {
				if c.fail_if_unreadable(expr.expr, expr.expr_type, what) {
					return true
				}
			}
		}
		ast.CallExpr {
			pos = expr.pos
			if expr.is_method {
				if c.fail_if_unreadable(expr.left, expr.left_type, what) {
					return true
				}
			}
			return false
		}
		ast.LockExpr {
			// TODO: check expressions inside the lock by appending to c.(r)locked_names
			return false
		}
		ast.IndexExpr {
			pos = expr.left.pos().extend(expr.pos)
			if c.fail_if_unreadable(expr.left, expr.left_type, what) {
				return true
			}
			if typ.has_flag(.shared_f) && expr.left is ast.SelectorExpr {
				return false
			}
		}
		ast.InfixExpr {
			pos = expr.left.pos().extend(expr.pos)
			if c.fail_if_unreadable(expr.left, expr.left_type, what) {
				return true
			}
			if c.fail_if_unreadable(expr.right, expr.right_type, what) {
				return true
			}
		}
		else {
			pos = expr.pos()
		}
	}
	if typ.has_flag(.shared_f) {
		c.error('you have to create a handle and `rlock` it to use a `shared` element as non-mut ${what}',
			pos)
		return true
	}
	return false
}

fn (mut c Checker) fail_if_stack_struct_action_outside_unsafe(mut ident ast.Ident, failed_action string) {
	if mut ident.obj is ast.Var {
		mut obj := unsafe { &ident.obj }
		if c.fn_scope != unsafe { nil } {
			obj = c.fn_scope.find_var(ident.obj.name) or { obj }
		}
		if obj.is_stack_obj && !c.inside_unsafe {
			sym := c.table.sym(obj.typ.set_nr_muls(0))
			is_heap := sym.is_heap()
			if (!is_heap || !obj.typ.is_ptr()) && !c.pref.translated && !c.file.is_translated {
				is_mut_param := c.table.cur_fn != unsafe { nil }
					&& c.table.cur_fn.params.filter(it.name == ident.name && it.is_mut).len > 0
				if is_mut_param {
					return
				}
				suggestion := if !is_heap && sym.kind == .struct {
					'declaring `${sym.name}` as `@[heap]`'
				} else if !is_heap {
					'wrapping the `${sym.name}` object in a `struct` declared as `@[heap]`'
				} else { // e.g. var from `for a in heap_object {`
					'declaring `${ident.name}` mutable'
				}
				c.error('`${ident.name}` cannot be ${failed_action} outside `unsafe` blocks as it might refer to an object stored on stack. Consider ${suggestion}.',
					ident.pos)
			}
		}
	}
}

fn (mut c Checker) goto_label(node ast.GotoLabel) {
	// Register a goto label
	if node.name !in c.goto_labels {
		c.goto_labels[node.name] = node
		c.goto_labels[node.name].is_used = false
	}
}

fn (mut c Checker) goto_stmt(node ast.GotoStmt) {
	if c.inside_defer {
		c.error('goto is not allowed in defer statements', node.pos)
	}
	if !c.inside_unsafe {
		if !c.pref.translated && !c.file.is_translated {
			c.warn('`goto` requires `unsafe` (consider using labelled break/continue)',
				node.pos)
		}
	}
	if c.table.cur_fn != unsafe { nil } && node.name !in c.table.cur_fn.label_names {
		c.error('unknown label `${node.name}`', node.pos)
	}
	c.goto_labels[node.name].is_used = true // Register a label use
	// TODO: check label doesn't bypass variable declarations
}

fn (mut c Checker) check_unused_labels() {
	for name, label in c.goto_labels {
		if !label.is_used {
			// TODO: show label's location
			c.warn('label `${name}` defined and not used', label.pos)
			c.goto_labels[name].is_used = true // so that this warning is not shown again
		}
	}
}

fn (mut c Checker) deprecate_old_isreftype_and_sizeof_of_a_guessed_type(is_guessed_type bool, typ ast.Type,
	pos token.Pos, label string) {
	if is_guessed_type {
		styp := c.table.type_to_str(typ)
		c.note('`${label}(${styp})` is deprecated. Use `v fmt -w .` to convert it to `${label}[${styp}]()` instead.',
			pos)
	}
}

fn (c &Checker) check_import_sym_conflict(ident string) bool {
	for import_sym in c.file.imports {
		// Check if alias exists or not
		if !import_sym.alias.is_blank() {
			if import_sym.alias.len == ident.len && import_sym.alias == ident {
				return true
			}
		} else if import_sym.mod.len == ident.len && import_sym.mod == ident {
			return true
		}
	}
	return false
}

// update_unresolved_fixed_sizes updates the unresolved type symbols for array fixed return type and alias type.
pub fn (mut c Checker) update_unresolved_fixed_sizes() {
	for mut stmt in c.unresolved_fixed_sizes {
		if mut stmt is ast.FnDecl { // return types
			ret_sym := c.table.sym(stmt.return_type)
			if ret_sym.info is ast.ArrayFixed && c.array_fixed_has_unresolved_size(ret_sym.info) {
				mut size_expr := ret_sym.info.size_expr
				stmt.return_type = c.eval_array_fixed_sizes(mut size_expr, 0, ret_sym.info.elem_type)
			}
		} else if mut stmt is ast.TypeDecl { // alias
			mut alias_decl := stmt
			if mut alias_decl is ast.AliasTypeDecl {
				alias_sym := c.table.sym(alias_decl.parent_type)
				if alias_sym.info is ast.ArrayFixed
					&& c.array_fixed_has_unresolved_size(alias_sym.info) {
					mut size_expr := alias_sym.info.size_expr
					alias_decl.parent_type = c.eval_array_fixed_sizes(mut size_expr, 0,
						alias_sym.info.elem_type)

					// overwriting current alias type
					mut typ_sym := c.table.type_symbols[alias_decl.typ.idx()]
					typ_sym.parent_idx = alias_decl.parent_type.idx()
					if mut typ_sym.info is ast.Alias {
						typ_sym.info.parent_type = alias_decl.parent_type
					}
				}
			}
		}
	}
}

fn (mut c Checker) dir_path() string {
	return os.real_path(os.dir(c.file.path))
}
