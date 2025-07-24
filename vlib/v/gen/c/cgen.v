// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import os
import term
import strings
import hash.fnv1a
import v.ast
import v.pref
import v.token
import v.util
import v.util.version
import v.depgraph
import v.type_resolver
import sync.pool

// Note: some of the words in c_reserved, are not reserved in C, but are
// in C++, or have special meaning in V, thus need escaping too. `small`
// should not be needed, but see:
// https://stackoverflow.com/questions/5874215/what-is-rpcndr-h
const c_reserved = ['asm', 'array', 'auto', 'bool', 'break', 'calloc', 'case', 'char', 'class',
	'complex', 'const', 'continue', 'default', 'delete', 'do', 'double', 'else', 'enum', 'error',
	'exit', 'export', 'extern', 'false', 'float', 'for', 'free', 'goto', 'if', 'inline', 'int',
	'link', 'long', 'malloc', 'namespace', 'new', 'nil', 'panic', 'register', 'restrict', 'return',
	'short', 'signed', 'sizeof', 'static', 'string', 'struct', 'switch', 'typedef', 'typename',
	'typeof', 'union', 'unix', 'unsigned', 'void', 'volatile', 'while', 'template', 'true', 'small',
	'stdout', 'stdin', 'stderr', 'far', 'near', 'huge', 'requires']
const c_reserved_chk = token.new_keywords_matcher_from_array_trie(c_reserved)
// same order as in token.Kind
const cmp_str = ['eq', 'ne', 'gt', 'lt', 'ge', 'le']
// when operands are switched
const cmp_rev = ['eq', 'ne', 'lt', 'gt', 'le', 'ge']
const result_name = ast.result_name
const option_name = ast.option_name

pub struct Gen {
	pref                &pref.Preferences = unsafe { nil }
	field_data_type     ast.Type // cache her to avoid map lookups
	enum_data_type      ast.Type // cache her to avoid map lookups
	variant_data_type   ast.Type // cache her to avoid map lookups
	module_built        string
	timers_should_print bool
mut:
	out        strings.Builder
	extern_out strings.Builder // extern declarations for -parallel-cc
	// line_nr                   int
	cheaders                  strings.Builder
	preincludes               strings.Builder // allows includes to go before `definitions`
	postincludes              strings.Builder // allows includes to go after all the rest of the code generation
	includes                  strings.Builder // all C #includes required by V modules
	typedefs                  strings.Builder
	enum_typedefs             strings.Builder // enum types
	definitions               strings.Builder // typedefs, defines etc (everything that goes to the top of the file)
	type_definitions          strings.Builder // typedefs, defines etc (everything that goes to the top of the file)
	sort_fn_definitions       strings.Builder // sort fns
	alias_definitions         strings.Builder // alias fixed array of non-builtin
	hotcode_definitions       strings.Builder // -live declarations & functions
	channel_definitions       strings.Builder // channel related code
	thread_definitions        strings.Builder // thread defines
	comptime_definitions      strings.Builder // custom defines, given by -d/-define flags on the CLI
	type_default_vars         strings.Builder // type_default() var declarations
	cleanup                   strings.Builder
	cleanups                  map[string]strings.Builder // contents of `void _vcleanup(){}`
	gowrappers                strings.Builder            // all go callsite wrappers
	waiter_fn_definitions     strings.Builder            // waiter fns definitions
	auto_str_funcs            strings.Builder            // function bodies of all auto generated _str funcs
	dump_funcs                strings.Builder            // function bodies of all auto generated _str funcs
	pcs_declarations          strings.Builder            // -prof profile counter declarations for each function
	cov_declarations          strings.Builder            // -cov coverage
	embedded_data             strings.Builder            // data to embed in the executable/binary
	shared_types              strings.Builder            // shared/lock types
	shared_functions          strings.Builder            // shared constructors
	out_options_forward       strings.Builder            // forward `option_xxxx` types
	out_options               strings.Builder            // `option_xxxx` types
	out_results_forward       strings.Builder            // forward`result_xxxx` types
	out_results               strings.Builder            // `result_xxxx` types
	json_forward_decls        strings.Builder            // json type forward decls
	sql_buf                   strings.Builder            // for writing exprs to args via `sqlite3_bind_int()` etc
	global_const_defs         map[string]GlobalConstDef
	sorted_global_const_names []string
	file                      &ast.File  = unsafe { nil }
	table                     &ast.Table = unsafe { nil }
	styp_cache                map[ast.Type]string
	no_eq_method_types        map[ast.Type]bool // types that does not need to call its auto eq methods for optimization
	unique_file_path_hash     u64               // a hash of file.path, used for making auxiliary fn generation unique (like `compare_xyz`)
	fn_decl                   &ast.FnDecl = unsafe { nil } // pointer to the FnDecl we are currently inside otherwise 0
	last_fn_c_name            string
	tmp_count                 int  // counter for unique tmp vars (_tmp1, _tmp2 etc); resets at the start of each fn.
	tmp_count_af              int  // a separate tmp var counter for autofree fn calls
	tmp_count_declarations    int  // counter for unique tmp names (_d1, _d2 etc); does NOT reset, used for C declarations
	global_tmp_count          int  // like tmp_count but global and not reset in each function
	discard_or_result         bool // do not safe last ExprStmt of `or` block in tmp variable to defer ongoing expr usage
	is_direct_array_access    bool // inside a `[direct_array_access fn a() {}` function
	is_assign_lhs             bool // inside left part of assign expr (for array_set(), etc)
	is_void_expr_stmt         bool // ExprStmt whose result is discarded
	is_arraymap_set           bool // map or array set value state
	is_amp                    bool // for `&Foo{}` to merge PrefixExpr `&` and StructInit `Foo{}`; also for `&u8(unsafe { nil })` etc
	is_sql                    bool // Inside `sql db{}` statement, generating sql instead of C (e.g. `and` instead of `&&` etc)
	is_shared                 bool // for initialization of hidden mutex in `[rw]shared` literals
	is_vlines_enabled         bool // is it safe to generate #line directives when -g is passed
	is_autofree               bool // false, inside the bodies of fns marked with [manualfree], otherwise === g.pref.autofree
	is_builtin_mod            bool
	is_json_fn                bool // inside json.encode()
	is_js_call                bool // for handling a special type arg #1 `json.decode(User, ...)`
	is_fn_index_call          bool
	is_cc_msvc                bool // g.pref.ccompiler == 'msvc'
	is_option_auto_heap       bool
	vlines_path               string            // set to the proper path for generating #line directives
	options_pos_forward       int               // insertion point to forward
	options_forward           []string          // to forward
	options                   map[string]string // to avoid duplicates
	results_forward           []string          // to forward
	results                   map[string]string // to avoid duplicates
	done_options              shared []string   // to avoid duplicates
	done_results              shared []string   // to avoid duplicates
	chan_pop_options          map[string]string // types for `x := <-ch or {...}`
	chan_push_options         map[string]string // types for `ch <- x or {...}`
	mtxs                      string            // array of mutexes if the `lock` has multiple variables
	tmp_var_ptr               map[string]bool   // indicates if the tmp var passed to or_block() is a ptr
	labeled_loops             map[string]&ast.Stmt
	contains_ptr_cache        map[ast.Type]bool
	inner_loop                &ast.Stmt = unsafe { nil }
	cur_indexexpr             []int          // list of nested indexexpr which generates array_set/map_set
	shareds                   map[int]string // types with hidden mutex for which decl has been emitted
	coverage_files            map[u64]&CoverageInfo
	inside_smartcast          bool
	inside_ternary            int  // ?: comma separated statements on a single line
	inside_map_postfix        bool // inside map++/-- postfix expr
	inside_map_infix          bool // inside map<</+=/-= infix expr
	inside_assign             bool
	inside_map_index          bool
	inside_array_index        bool
	inside_array_fixed_struct bool
	inside_opt_or_res         bool
	inside_opt_data           bool
	inside_if_option          bool
	inside_if_result          bool
	inside_match_option       bool
	inside_match_result       bool
	inside_vweb_tmpl          bool
	inside_return             bool
	inside_return_tmpl        bool
	inside_struct_init        bool
	inside_or_block           bool
	inside_call               bool
	inside_curry_call         bool // inside foo()()!, foo()()?, foo()()
	inside_dump_fn            bool
	inside_c_extern           bool // inside `@[c_extern] fn C.somename(param1 int, param2 voidptr, param3 &char) &char`
	expected_fixed_arr        bool
	inside_for_c_stmt         bool
	inside_cast_in_heap       int // inside cast to interface type in heap (resolve recursive calls)
	inside_cast               bool
	inside_selector           bool
	inside_selector_deref     bool // indicates if the inside selector was already dereferenced
	inside_memset             bool
	inside_const              bool
	inside_array_item         bool
	inside_const_opt_or_res   bool
	inside_lambda             bool
	inside_cinit              bool
	inside_global_decl        bool
	inside_interface_deref    bool
	last_tmp_call_var         []string
	last_if_option_type       ast.Type // stores the expected if type on nested if expr
	loop_depth                int
	ternary_names             map[string]string
	ternary_level_names       map[string][]string
	arraymap_set_pos          int      // map or array set value position
	stmt_path_pos             []int    // positions of each statement start, for inserting C statements before the current statement
	skip_stmt_pos             bool     // for handling if expressions + autofree (since both prepend C statements)
	left_is_opt               bool     // left hand side on assignment is an option
	right_is_opt              bool     // right hand side on assignment is an option
	assign_ct_type            ast.Type // left hand side resolved comptime type
	indent                    int
	empty_line                bool
	assign_op                 token.Kind // *=, =, etc (for array_set)
	defer_stmts               []ast.DeferStmt
	defer_ifdef               string
	defer_profile_code        string
	defer_vars                []string
	closure_structs           []string
	str_types                 []StrType       // types that need automatic str() generation
	generated_str_fns         []StrType       // types that already have a str() function
	str_fn_names              shared []string // remove duplicate function names
	threaded_fns              shared []string // for generating unique wrapper types and fns for `go xxx()`
	waiter_fns                shared []string // functions that wait for `go xxx()` to finish
	needed_equality_fns       []ast.Type
	generated_eq_fns          []ast.Type
	array_sort_fn             shared []string
	array_contains_types      []ast.Type
	array_index_types         []ast.Type
	auto_fn_definitions       []string // auto generated functions definition list
	sumtype_casting_fns       []SumtypeCastingFn
	anon_fn_definitions       []string        // anon generated functions definition list
	anon_fns                  shared []string // remove duplicate anon generated functions
	sumtype_definitions       map[int]bool    // `_TypeA_to_sumtype_TypeB()` fns that have been generated
	trace_fn_definitions      []string
	json_types                []ast.Type           // to avoid json gen duplicates
	pcs                       []ProfileCounterMeta // -prof profile counter fn_names => fn counter name
	hotcode_fn_names          []string
	hotcode_fpaths            []string
	embedded_files            []ast.EmbeddedFile
	sql_i                     int
	sql_stmt_name             string
	sql_bind_name             string
	sql_idents                []string
	sql_idents_types          []ast.Type
	sql_left_type             ast.Type
	sql_table_name            string
	sql_fkey                  string
	sql_parent_id             string
	sql_side                  SqlExprSide // left or right, to distinguish idents in `name == name`
	sql_last_stmt_out_len     int
	strs_to_free0             []string // strings.Builder
	// strs_to_free          []string // strings.Builder
	// tmp_arg_vars_to_free  []string
	// autofree_pregen       map[string]string
	// autofree_pregen_buf   strings.Builder
	// autofree_tmp_vars     []string // to avoid redefining the same tmp vars in a single function
	// nr_vars_to_free       int
	// doing_autofree_tmp    bool
	type_resolver                    type_resolver.TypeResolver
	comptime                         &type_resolver.ResolverInfo = unsafe { nil }
	prevent_sum_type_unwrapping_once bool // needed for assign new values to sum type
	// used in match multi branch
	// TypeOne, TypeTwo {}
	// where an aggregate (at least two types) is generated
	// sum type deref needs to know which index to deref because unions take care of the correct field
	aggregate_type_idx  int
	arg_no_auto_deref   bool     // smartcast must not be dereferenced
	branch_parent_pos   int      // used in BranchStmt (continue/break) for autofree stop position
	returned_var_name   string   // to detect that a var doesn't need to be freed since it's being returned
	infix_left_var_name string   // a && if expr
	curr_var_name       []string // curr var name on assignment
	called_fn_name      string
	timers              &util.Timers = util.get_timers()
	force_main_console  bool              // true when @[console] used on fn main()
	as_cast_type_names  map[string]string // table for type name lookup in runtime (for __as_cast)
	obf_table           map[string]string
	referenced_fns      shared map[string]bool // functions that have been referenced
	nr_closures         int
	expected_cast_type  ast.Type // for match expr of sumtypes
	expected_arg_mut    bool     // generating a mutable fn parameter
	or_expr_return_type ast.Type // or { 0, 1 } return type
	anon_fn             bool
	tests_inited        bool
	has_main            bool
	// main_fn_decl_node  ast.FnDecl
	cur_mod                ast.Module
	cur_concrete_types     []ast.Type // do not use table.cur_concrete_types because table is global, so should not be accessed by different threads
	cur_fn                 &ast.FnDecl = unsafe { nil } // same here
	cur_lock               ast.LockExpr
	cur_struct_init_typ    ast.Type
	autofree_methods       map[ast.Type]string
	generated_free_methods map[ast.Type]bool
	autofree_scope_stmts   []string
	use_segfault_handler   bool = true
	test_function_names    []string
	/////////
	// out_parallel []strings.Builder
	// out_idx      int
	out_fn_start_pos     []int  // for generating multiple .c files, stores locations of all fn positions in `out` string builder
	static_modifier      string // for parallel_cc
	static_non_parallel  string // for non -parallel_cc
	has_reflection       bool   // v.reflection has been imported
	has_debugger         bool   // $dbg has been used in the code
	reflection_strings   &map[string]int
	defer_return_tmp_var string
	vweb_filter_fn_name  string   // vweb__filter or x__vweb__filter, used by $vweb.html() for escaping strings in the templates, depending on which `vweb` import is used
	export_funcs         []string // for .dll export function names
	//
	type_default_impl_level int
}

@[heap]
pub struct GenOutput {
pub:
	header           string          // produced output for out.h (-parallel-cc)
	res_builder      strings.Builder // produced output (complete)
	out_str          string          // produced output from g.out
	out0_str         string          // helpers output (auto fns, dump fns) for out_0.c (-parallel-cc)
	extern_str       string          // extern chunk for (-parallel-cc)
	out_fn_start_pos []int           // fn decl positions
}

pub fn gen(files []&ast.File, mut table ast.Table, pref_ &pref.Preferences) GenOutput {
	mut module_built := ''
	if pref_.build_mode == .build_module {
		for file in files {
			if file.path.contains(pref_.path)
				&& file.mod.short_name == pref_.path.all_after_last(os.path_separator).trim_right(os.path_separator) {
				module_built = file.mod.name
				break
			}
		}
	}
	mut timers_should_print := false
	$if time_cgening ? {
		timers_should_print = true
	}
	mut reflection_strings := map[string]int{}
	mut global_g := Gen{
		file:                 unsafe { nil }
		out:                  strings.new_builder(512000)
		cheaders:             strings.new_builder(15000)
		includes:             strings.new_builder(100)
		preincludes:          strings.new_builder(100)
		postincludes:         strings.new_builder(100)
		typedefs:             strings.new_builder(100)
		enum_typedefs:        strings.new_builder(100)
		type_definitions:     strings.new_builder(100)
		sort_fn_definitions:  strings.new_builder(100)
		alias_definitions:    strings.new_builder(100)
		hotcode_definitions:  strings.new_builder(100)
		channel_definitions:  strings.new_builder(100)
		thread_definitions:   strings.new_builder(100)
		comptime_definitions: strings.new_builder(100)
		definitions:          strings.new_builder(100)
		gowrappers:           strings.new_builder(100)
		auto_str_funcs:       strings.new_builder(100)
		dump_funcs:           strings.new_builder(100)
		pcs_declarations:     strings.new_builder(100)
		cov_declarations:     strings.new_builder(100)
		embedded_data:        strings.new_builder(1000)
		out_options_forward:  strings.new_builder(100)
		out_options:          strings.new_builder(100)
		out_results_forward:  strings.new_builder(100)
		out_results:          strings.new_builder(100)
		shared_types:         strings.new_builder(100)
		shared_functions:     strings.new_builder(100)
		json_forward_decls:   strings.new_builder(100)
		sql_buf:              strings.new_builder(100)
		table:                table
		pref:                 pref_
		fn_decl:              unsafe { nil }
		is_autofree:          pref_.autofree
		indent:               -1
		module_built:         module_built
		timers_should_print:  timers_should_print
		timers:               util.new_timers(
			should_print: timers_should_print
			label:        'global_cgen'
		)
		inner_loop:           unsafe { &ast.empty_stmt }
		field_data_type:      table.find_type('FieldData')
		enum_data_type:       table.find_type('EnumData')
		variant_data_type:    table.find_type('VariantData')
		is_cc_msvc:           pref_.ccompiler == 'msvc'
		use_segfault_handler: pref_.should_use_segfault_handler()
		static_modifier:      if pref_.parallel_cc { 'static ' } else { '' }
		static_non_parallel:  if !pref_.parallel_cc { 'static ' } else { '' }
		has_reflection:       'v.reflection' in table.modules
		has_debugger:         'v.debug' in table.modules
		reflection_strings:   &reflection_strings
	}

	global_g.type_resolver = type_resolver.TypeResolver.new(table, global_g)
	global_g.comptime = &global_g.type_resolver.info
	// anon fn may include assert and thus this needs
	// to be included before any test contents are written
	if pref_.is_test {
		global_g.write_tests_definitions()
	}

	util.timing_start('cgen init')
	for mod in global_g.table.modules {
		global_g.cleanups[mod] = strings.new_builder(100)
	}
	global_g.init()
	util.timing_measure('cgen init')
	global_g.tests_inited = false
	global_g.file = files.last()
	if !pref_.no_parallel {
		util.timing_start('cgen parallel processing')
		mut pp := pool.new_pool_processor(callback: cgen_process_one_file_cb)
		pp.set_shared_context(global_g) // TODO: make global_g shared
		pp.work_on_items(files)
		util.timing_measure('cgen parallel processing')

		util.timing_start('cgen unification')
		for g in pp.get_results_ref[Gen]() {
			global_g.embedded_files << g.embedded_files
			global_g.out << g.out
			global_g.cheaders << g.cheaders
			global_g.preincludes << g.preincludes
			global_g.postincludes << g.postincludes
			global_g.includes << g.includes
			global_g.typedefs << g.typedefs
			global_g.type_definitions << g.type_definitions
			global_g.sort_fn_definitions << g.sort_fn_definitions
			global_g.alias_definitions << g.alias_definitions
			global_g.definitions << g.definitions
			global_g.gowrappers << g.gowrappers
			global_g.waiter_fn_definitions << g.waiter_fn_definitions
			global_g.auto_str_funcs << g.auto_str_funcs
			global_g.dump_funcs << g.auto_str_funcs
			global_g.comptime_definitions << g.comptime_definitions
			global_g.pcs_declarations << g.pcs_declarations
			global_g.cov_declarations << g.cov_declarations
			global_g.hotcode_definitions << g.hotcode_definitions
			global_g.embedded_data << g.embedded_data
			global_g.shared_types << g.shared_types
			global_g.shared_functions << g.channel_definitions
			global_g.export_funcs << g.export_funcs

			global_g.force_main_console = global_g.force_main_console || g.force_main_console

			// merge maps
			for k, v in g.global_const_defs {
				global_g.global_const_defs[k] = v
			}
			for k, v in g.shareds {
				global_g.shareds[k] = v
			}
			for k, v in g.chan_pop_options {
				global_g.chan_pop_options[k] = v
			}
			for k, v in g.chan_push_options {
				global_g.chan_push_options[k] = v
			}
			for k, v in g.options {
				global_g.options[k] = v
			}
			for k, v in g.results {
				global_g.results[k] = v
			}
			for k, v in g.as_cast_type_names {
				global_g.as_cast_type_names[k] = v
			}
			for k, v in g.sumtype_definitions {
				global_g.sumtype_definitions[k] = v
			}
			for k, v in g.coverage_files {
				global_g.coverage_files[k] = v
			}
			global_g.json_forward_decls << g.json_forward_decls
			global_g.enum_typedefs << g.enum_typedefs
			global_g.channel_definitions << g.channel_definitions
			global_g.thread_definitions << g.thread_definitions
			global_g.sql_buf << g.sql_buf
			global_g.cleanups[g.file.mod.name] << g.cleanup

			for str_type in g.str_types {
				global_g.str_types << str_type
			}
			for scf in g.sumtype_casting_fns {
				if scf !in global_g.sumtype_casting_fns {
					global_g.sumtype_casting_fns << scf
				}
			}

			global_g.nr_closures += g.nr_closures
			global_g.has_main = global_g.has_main || g.has_main

			global_g.auto_fn_definitions << g.auto_fn_definitions
			global_g.anon_fn_definitions << g.anon_fn_definitions
			global_g.needed_equality_fns << g.needed_equality_fns // duplicates are resolved later in gen_equality_fns
			global_g.array_contains_types << g.array_contains_types
			global_g.array_index_types << g.array_index_types
			global_g.pcs << g.pcs
			global_g.json_types << g.json_types
			global_g.hotcode_fn_names << g.hotcode_fn_names
			global_g.hotcode_fpaths << g.hotcode_fpaths
			global_g.test_function_names << g.test_function_names
			for k, v in g.autofree_methods {
				global_g.autofree_methods[k] = v
			}
			for k, v in g.no_eq_method_types {
				global_g.no_eq_method_types[k] = v
			}

			unsafe { g.free_builders() } // free at the very end
		}
	} else {
		util.timing_start('cgen serial processing')
		for file in files {
			global_g.file = file
			global_g.gen_file()
			global_g.cleanups[file.mod.name].drain_builder(mut global_g.cleanup, 100)
			global_g.global_tmp_count = 0
			global_g.tmp_count = 0
		}
		util.timing_measure('cgen serial processing')

		util.timing_start('cgen unification')
	}

	global_g.gen_jsons()
	global_g.dump_expr_definitions() // this uses global_g.get_str_fn, so it has to go before the below for loop
	for i := 0; i < global_g.str_types.len; i++ {
		global_g.final_gen_str(global_g.str_types[i])
	}
	for sumtype_casting_fn in global_g.sumtype_casting_fns {
		global_g.write_sumtype_casting_fn(sumtype_casting_fn)
	}
	global_g.write_shareds()
	global_g.write_chan_pop_option_fns()
	global_g.write_chan_push_option_fns()
	global_g.gen_array_contains_methods()
	global_g.gen_array_index_methods()
	global_g.gen_equality_fns()
	global_g.gen_free_methods()
	global_g.register_iface_return_types()
	global_g.write_results()
	global_g.write_options()
	global_g.sort_globals_consts()
	util.timing_measure('cgen unification')

	mut g := global_g
	util.timing_start('cgen common')

	// to make sure type idx's are the same in cached mods
	if g.pref.build_mode == .build_module {
		is_toml := g.pref.path.contains('/toml')
		for idx, sym in g.table.type_symbols {
			if idx in [0, 31] {
				continue
			}
			if is_toml && sym.cname.contains('map[string]') {
				// Temporary hack to make toml work with -usecache TODO remove
				continue
			}
			g.definitions.writeln('int _v_type_idx_${sym.cname}(); // 1build module ${g.pref.path}')
		}
	} else if g.pref.use_cache {
		is_toml := g.pref.path.contains('/toml')
		for idx, sym in g.table.type_symbols {
			if idx in [0, 31] {
				continue
			}
			if is_toml && sym.cname.contains('map[string]') {
				continue
			}
			g.definitions.writeln('int _v_type_idx_${sym.cname}() { return ${idx}; }; //lol ${g.pref.path}')
		}
	}

	// v files are finished, what remains is pure C code
	g.gen_vlines_reset()
	if g.pref.build_mode != .build_module {
		// no init in builtin.o
		g.write_init_function()
	}

	if g.pref.is_coverage {
		mut total_code_points := 0
		for k, cov in g.coverage_files {
			g.cheaders.writeln('#define _v_cov_file_offset_${k} ${total_code_points}')
			total_code_points += cov.points.len
		}
		g.cheaders.writeln('long int _v_cov[${total_code_points}] = {0};')
	}

	// insert for options forward
	if g.out_options_forward.len > 0 || g.out_results_forward.len > 0 {
		tail := g.type_definitions.cut_to(g.options_pos_forward)
		if g.out_options_forward.len > 0 {
			g.type_definitions.writeln('// #start V forward option_xxx definitions:')
			g.type_definitions.writeln(g.out_options_forward.str())
			g.type_definitions.writeln('// #end V forward option_xxx definitions\n')
		}
		if g.out_results_forward.len > 0 {
			g.type_definitions.writeln('// #start V forward result_xxx definitions:')
			g.type_definitions.writeln(g.out_results_forward.str())
			g.type_definitions.writeln('// #end V forward result_xxx definitions\n')
		}
		g.type_definitions.writeln(tail)
	}

	g.finish()

	mut b := strings.new_builder(g.out.len + 600_000)
	b.write_string(g.hashes())
	if g.use_segfault_handler || g.pref.is_prof {
		b.writeln('\n#define V_USE_SIGNAL_H')
	}
	b.write_string2('\n// V comptime_definitions:\n', g.comptime_definitions.str())
	b.write_string2('\n// V typedefs:\n', g.typedefs.str())
	b.write_string2('\n // V preincludes:\n', g.preincludes.str())
	b.write_string2('\n// V cheaders:\n', g.cheaders.str())
	if g.pcs_declarations.len > 0 {
		g.pcs_declarations.writeln('// V profile thread local:')
		g.pcs_declarations.writeln('#if defined(__cplusplus) && __cplusplus >= 201103L')
		g.pcs_declarations.writeln('\t#define PROF_THREAD_LOCAL thread_local')
		g.pcs_declarations.writeln('#elif defined(__GNUC__) && __GNUC__ < 5')
		g.pcs_declarations.writeln('\t#define PROF_THREAD_LOCAL __thread')
		g.pcs_declarations.writeln('#elif defined(_MSC_VER)')
		g.pcs_declarations.writeln('\t#define PROF_THREAD_LOCAL __declspec(thread)')
		g.pcs_declarations.writeln('#elif defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L && !defined(__STDC_NO_THREADS__)')
		g.pcs_declarations.writeln('\t#define PROF_THREAD_LOCAL _Thread_local')
		g.pcs_declarations.writeln('#endif')
		g.pcs_declarations.writeln('#ifndef PROF_THREAD_LOCAL')
		g.pcs_declarations.writeln('\t#if defined(__GNUC__)')
		g.pcs_declarations.writeln('\t\t#define PROF_THREAD_LOCAL __thread')
		g.pcs_declarations.writeln('\t#endif')
		g.pcs_declarations.writeln('#endif')
		g.pcs_declarations.writeln('#ifdef PROF_THREAD_LOCAL')
		g.pcs_declarations.writeln('\tstatic PROF_THREAD_LOCAL double prof_measured_time = 0.0;')
		g.pcs_declarations.writeln('#else')
		g.pcs_declarations.writeln('\tdouble prof_measured_time = 0.0; // multithreaded: wrong values for func times without its children')
		g.pcs_declarations.writeln('#endif')
		b.write_string2('\n// V profile counters:\n', g.pcs_declarations.str())
	}
	b.write_string2('\n// V includes:\n', g.includes.str())
	b.writeln('\n// V global/const #define ... :')
	for var_name in g.sorted_global_const_names {
		if var := g.global_const_defs[var_name] {
			if var.def.starts_with('#define') {
				b.writeln(var.def)
			}
		}
	}
	if g.enum_typedefs.len > 0 {
		b.write_string2('\n// Enum definitions:\n', g.enum_typedefs.str())
	}
	if g.thread_definitions.len > 0 {
		b.write_string2('\n// Thread definitions:\n', g.thread_definitions.str())
	}
	if g.type_definitions.len > 0 {
		b.write_string2('\n// V type definitions:\n', g.type_definitions.str())
	}
	if g.alias_definitions.len > 0 {
		b.write_string2('\n// V alias definitions:\n', g.alias_definitions.str())
	}
	if g.shared_types.len > 0 {
		b.write_string2('\n// V shared types:\n', g.shared_types.str())
	}
	if g.out_options.len > 0 {
		b.write_string2('\n// V Option_xxx definitions:\n', g.out_options.str())
	}
	if g.out_results.len > 0 {
		b.write_string2('\n// V result_xxx definitions:\n', g.out_results.str())
	}
	b.write_string2('\n// V definitions:\n', g.definitions.str())
	if g.sort_fn_definitions.len > 0 {
		b.write_string2('\n// V sort fn definitions:\n', g.sort_fn_definitions.str())
	}
	if !pref_.parallel_cc {
		b.writeln('\n// V global/const non-precomputed definitions:')
		for var_name in g.sorted_global_const_names {
			if var := g.global_const_defs[var_name] {
				if !var.def.starts_with('#define') {
					b.writeln(var.def)
				}
			}
		}
	}
	interface_table := g.interface_table()
	if interface_table.len > 0 {
		b.write_string2('\n// V interface table:\n', interface_table)
	}
	if g.hotcode_definitions.len > 0 {
		b.write_string2('\n// V hotcode definitions:\n', g.hotcode_definitions.str())
	}
	if g.shared_functions.len > 0 {
		b.writeln('\n// V shared type functions:\n')
		b.write_string2(g.shared_functions.str(), c_concurrency_helpers)
	}
	if g.channel_definitions.len > 0 {
		b.write_string2('\n// V channel code:\n', g.channel_definitions.str())
	}
	if g.anon_fn_definitions.len > 0 {
		if g.nr_closures > 0 {
			b.writeln2('\n// V closure helpers', c_closure_helpers(g.pref))
		}
		/*
		b.writeln('\n// V anon functions:')
		for fn_def in g.anon_fn_definitions {
			b.writeln(fn_def)
		}
		*/
	}
	if g.pref.is_coverage {
		b.write_string2('\n// V coverage:\n', g.cov_declarations.str())
	}
	b.writeln('\n// end of V out (header)')
	mut header := b.last_n(b.len)
	header = '#ifndef V_HEADER_FILE\n#define V_HEADER_FILE' + header
	header += '\n#endif\n'

	mut helpers := strings.new_builder(300_000)
	// Code added here (after the header) goes to out_0.c in parallel cc mode
	// Previously it went to the header which resulted in duplicated code and more code
	// to compile for the C compiler
	if g.embedded_data.len > 0 {
		helpers.write_string2('\n// V embedded data:\n', g.embedded_data.str())
	}
	if g.anon_fn_definitions.len > 0 {
		if g.nr_closures > 0 {
			helpers.writeln2('\n// V closure helpers', c_closure_fn_helpers(g.pref))
		}
		/*
		b.writeln('\n// V anon functions:')
		for fn_def in g.anon_fn_definitions {
			b.writeln(fn_def)
		}
		*/
		if g.pref.parallel_cc {
			g.extern_out.writeln('extern void* __closure_create(void* fn, void* data);')
			g.extern_out.writeln('extern void __closure_init();')
		}
	}
	if g.pref.parallel_cc {
		helpers.writeln('\n// V global/const non-precomputed definitions:')
		for var_name in g.sorted_global_const_names {
			if var := g.global_const_defs[var_name] {
				if !var.def.starts_with('#define') {
					helpers.writeln(var.def)
					if var.def.contains(' = ') {
						g.extern_out.writeln('extern ${var.def.all_before(' = ')};')
					} else {
						g.extern_out.writeln('extern ${var.def}')
					}
				}
			}
		}
	}
	if g.waiter_fn_definitions.len > 0 {
		if g.pref.parallel_cc {
			g.extern_out.write_string2('\n// V gowrappers waiter fns:\n', g.waiter_fn_definitions.bytestr())
		}
		helpers.write_string2('\n// V gowrappers waiter fns:\n', g.waiter_fn_definitions.str())
	}
	if g.auto_str_funcs.len > 0 {
		helpers.write_string2('\n// V auto str functions:\n', g.auto_str_funcs.str())
	}
	if g.auto_fn_definitions.len > 0 {
		helpers.writeln('\n// V auto functions:')
		for fn_def in g.auto_fn_definitions {
			helpers.writeln(fn_def)
		}
	}
	if g.json_forward_decls.len > 0 {
		helpers.write_string2('\n// V json forward decls:\n', g.json_forward_decls.bytestr())
		if g.pref.parallel_cc {
			g.extern_out.write_string2('\n// V json forward decls:\n', g.json_forward_decls.str())
		}
	}
	if g.gowrappers.len > 0 {
		helpers.write_string2('\n// V gowrappers:\n', g.gowrappers.str())
	}
	if g.dump_funcs.len > 0 {
		helpers.write_string2('\n// V dump functions:\n', g.dump_funcs.str())
	}
	if g.anon_fn_definitions.len > 0 {
		helpers.writeln('\n// V anon functions:')
		for fn_def in g.anon_fn_definitions {
			helpers.writeln(fn_def)
		}
	}

	if g.pref.is_shared && g.pref.os == .windows && g.export_funcs.len > 0 {
		// generate a .def for export function names, avoid function name mangle
		mut def_name := ''
		mut dll_name := ''
		if g.pref.out_name.ends_with('.dll') {
			def_name = g.pref.out_name[0..g.pref.out_name.len - 4] + '.def'
			dll_name = g.pref.out_name.all_after_last('\\')
		} else {
			def_name = g.pref.out_name + '.def'
			dll_name = g.pref.out_name.all_after_last('\\') + '.dll'
		}
		file_content := 'LIBRARY ${dll_name}\n\nEXPORTS\n' + g.export_funcs.join('\n')
		os.write_file('${def_name}', file_content) or { panic(err) }
	}

	// End of out_0.c

	shelpers := helpers.str()

	if !g.pref.parallel_cc {
		b.write_string(shelpers)
	}

	// The rest of the output
	out_str := g.out.str()
	extern_out_str := g.extern_out.str()
	b.write_string(out_str)
	b.writeln('// THE END.')

	postincludes_str := g.postincludes.str()
	if postincludes_str != '' {
		b.write_string2('\n // V postincludes:\n', postincludes_str)
	}

	util.timing_measure('cgen common')
	$if trace_all_generic_fn_keys ? {
		gkeys := g.table.fn_generic_types.keys()
		for gkey in gkeys {
			eprintln('>> g.table.fn_generic_types key: ${gkey}')
		}
	}
	out_fn_start_pos := g.out_fn_start_pos.clone()
	unsafe { helpers.free() }
	unsafe { g.free_builders() }

	return GenOutput{
		header:           header
		res_builder:      b
		out_str:          out_str
		out0_str:         shelpers
		extern_str:       extern_out_str
		out_fn_start_pos: out_fn_start_pos
	}
}

fn cgen_process_one_file_cb(mut p pool.PoolProcessor, idx int, wid int) &Gen {
	file := p.get_item[&ast.File](idx)
	timing_label_for_thread := 'C GEN thread ${wid}'
	util.timing_start(timing_label_for_thread)
	defer {
		util.timing_measure_cumulative(timing_label_for_thread)
	}
	mut global_g := unsafe { &Gen(p.get_shared_context()) }
	mut g := &Gen{
		file:                  file
		out:                   strings.new_builder(512000)
		cheaders:              strings.new_builder(15000)
		includes:              strings.new_builder(100)
		typedefs:              strings.new_builder(100)
		type_definitions:      strings.new_builder(100)
		sort_fn_definitions:   strings.new_builder(100)
		alias_definitions:     strings.new_builder(100)
		definitions:           strings.new_builder(100)
		gowrappers:            strings.new_builder(100)
		waiter_fn_definitions: strings.new_builder(100)
		auto_str_funcs:        strings.new_builder(100)
		comptime_definitions:  strings.new_builder(100)
		pcs_declarations:      strings.new_builder(100)
		cov_declarations:      strings.new_builder(100)
		hotcode_definitions:   strings.new_builder(100)
		embedded_data:         strings.new_builder(1000)
		out_options_forward:   strings.new_builder(100)
		out_options:           strings.new_builder(100)
		out_results_forward:   strings.new_builder(100)
		out_results:           strings.new_builder(100)
		shared_types:          strings.new_builder(100)
		shared_functions:      strings.new_builder(100)
		channel_definitions:   strings.new_builder(100)
		thread_definitions:    strings.new_builder(100)
		json_forward_decls:    strings.new_builder(100)
		enum_typedefs:         strings.new_builder(100)
		sql_buf:               strings.new_builder(100)
		cleanup:               strings.new_builder(100)
		table:                 global_g.table
		pref:                  global_g.pref
		fn_decl:               unsafe { nil }
		indent:                -1
		module_built:          global_g.module_built
		timers:                util.new_timers(
			should_print: global_g.timers_should_print
			label:        'cgen_process_one_file_cb idx: ${idx}, wid: ${wid}'
		)
		inner_loop:            &ast.empty_stmt
		field_data_type:       global_g.table.find_type('FieldData')
		enum_data_type:        global_g.table.find_type('EnumData')
		variant_data_type:     global_g.table.find_type('VariantData')
		array_sort_fn:         global_g.array_sort_fn
		waiter_fns:            global_g.waiter_fns
		threaded_fns:          global_g.threaded_fns
		str_fn_names:          global_g.str_fn_names
		anon_fns:              global_g.anon_fns
		options_forward:       global_g.options_forward
		results_forward:       global_g.results_forward
		done_options:          global_g.done_options
		done_results:          global_g.done_results
		is_autofree:           global_g.pref.autofree
		obf_table:             global_g.obf_table
		referenced_fns:        global_g.referenced_fns
		is_cc_msvc:            global_g.is_cc_msvc
		use_segfault_handler:  global_g.use_segfault_handler
		has_reflection:        'v.reflection' in global_g.table.modules
		has_debugger:          'v.debug' in global_g.table.modules
		reflection_strings:    global_g.reflection_strings
	}
	g.type_resolver = type_resolver.TypeResolver.new(global_g.table, g)
	g.comptime = &g.type_resolver.info
	g.gen_file()
	return g
}

// free_builders should be called only when a Gen would NOT be used anymore
// it frees the bulk of the memory that is private to the Gen instance
// (the various string builders)
@[unsafe]
pub fn (mut g Gen) free_builders() {
	unsafe {
		g.out.free()
		g.cheaders.free()
		g.includes.free()
		g.typedefs.free()
		g.type_definitions.free()
		g.sort_fn_definitions.free()
		g.alias_definitions.free()
		g.definitions.free()
		g.cleanup.free()
		g.gowrappers.free()
		g.waiter_fn_definitions.free()
		g.auto_str_funcs.free()
		g.dump_funcs.free()
		g.comptime_definitions.free()
		g.pcs_declarations.free()
		g.cov_declarations.free()
		g.hotcode_definitions.free()
		g.embedded_data.free()
		g.shared_types.free()
		g.shared_functions.free()
		g.channel_definitions.free()
		g.thread_definitions.free()
		g.out_options_forward.free()
		g.out_options.free()
		g.out_results_forward.free()
		g.out_results.free()
		g.json_forward_decls.free()
		g.enum_typedefs.free()
		g.sql_buf.free()
		for _, mut v in g.cleanups {
			v.free()
		}
	}
}

pub fn (mut g Gen) gen_file() {
	g.timers.start('cgen_file ${g.file.path}')
	g.unique_file_path_hash = fnv1a.sum64_string(g.file.path)
	if g.pref.is_vlines {
		g.vlines_path = util.vlines_escape_path(g.file.path, g.pref.ccompiler)
		g.is_vlines_enabled = true
		g.inside_ternary = 0
	}
	g.stmts(g.file.stmts)
	// Transfer embedded files
	for path in g.file.embedded_files {
		if path !in g.embedded_files {
			g.embedded_files << path
		}
	}
	g.timers.show('cgen_file ${g.file.path}')
}

pub fn (g &Gen) hashes() string {
	return c_commit_hash_default.replace('@@@', version.vhash())
}

pub fn (mut g Gen) init() {
	if g.pref.custom_prelude != '' {
		g.cheaders.writeln(g.pref.custom_prelude)
	} else if !g.pref.no_preludes {
		g.cheaders.writeln('// Generated by the V compiler')
		if g.pref.relaxed_gcc14 {
			// See https://gcc.gnu.org/gcc-14/porting_to.html#c-code-generators:
			g.cheaders.writeln('
#if defined __GNUC__ && __GNUC__ >= 14
#pragma GCC diagnostic warning "-Wimplicit-function-declaration"
#pragma GCC diagnostic warning "-Wincompatible-pointer-types"
#pragma GCC diagnostic warning "-Wint-conversion"
#pragma GCC diagnostic warning "-Wreturn-mismatch"
#endif
')
		}
		if g.pref.os == .linux {
			// For gettid() declaration (and other GNU-specific bits).
			g.cheaders.writeln('#define _GNU_SOURCE')
		}
		if g.pref.os == .wasm32 {
			g.cheaders.writeln('#define VWASM 1')
			// Include <stdint.h> instead of <inttypes.h> for WASM target
			g.cheaders.writeln('#include <stdint.h>')
			g.cheaders.writeln('#include <stddef.h>')
		} else {
			tcc_undef_has_include := '
#if defined(__TINYC__) && defined(__has_include)
// tcc does not support has_include properly yet, turn it off completely
#undef __has_include
#endif'
			g.preincludes.writeln(tcc_undef_has_include)
			g.cheaders.writeln(tcc_undef_has_include)
			g.includes.writeln(tcc_undef_has_include)
			if g.pref.os == .freebsd {
				g.cheaders.writeln('#include <inttypes.h>')
				g.cheaders.writeln('#include <stddef.h>')
			} else {
				install_compiler_msg := ' Please install the package `build-essential`.'
				g.cheaders.writeln(get_guarded_include_text('<inttypes.h>', 'The C compiler can not find <inttypes.h>.${install_compiler_msg}')) // int64_t etc
				if g.pref.os == .ios {
					g.cheaders.writeln(get_guarded_include_text('<stdbool.h>', 'The C compiler can not find <stdbool.h>.${install_compiler_msg}')) // bool, true, false
				}
				g.cheaders.writeln(get_guarded_include_text('<stddef.h>', 'The C compiler can not find <stddef.h>.${install_compiler_msg}')) // size_t, ptrdiff_t
			}
		}
		if g.pref.nofloat {
			g.cheaders.writeln('#define VNOFLOAT 1')
		}
		g.cheaders.writeln(c_builtin_types)
		if !g.pref.skip_unused || g.table.used_features.used_maps > 0 {
			g.cheaders.writeln(c_mapfn_callback_types)
		}
		if g.pref.is_bare {
			g.cheaders.writeln(c_bare_headers)
		} else {
			g.cheaders.writeln(c_headers)
		}
		if !g.pref.skip_unused || g.table.used_features.used_maps > 0 {
			g.cheaders.writeln(c_wyhash_headers)
		}
	}
	if g.pref.os == .ios {
		g.cheaders.writeln('#define __TARGET_IOS__ 1')
		g.cheaders.writeln('#include <spawn.h>')
	}
	if g.pref.os == .linux {
		g.cheaders.writeln('#if __GLIBC__ == 2 && __GLIBC_MINOR__ < 30')
		g.cheaders.writeln('#include <sys/syscall.h>')
		g.cheaders.writeln('#define gettid() syscall(SYS_gettid)')
		g.cheaders.writeln('#endif')
	}
	g.write_builtin_types()
	g.options_pos_forward = g.type_definitions.len
	g.write_typedef_types()
	g.write_typeof_functions()
	g.write_sorted_types()
	g.write_array_fixed_return_types()
	g.write_multi_return_types()
	g.definitions.writeln('// end of definitions #endif')
	if g.pref.compile_defines_all.len > 0 {
		g.comptime_definitions.writeln('// V compile time defines by -d or -define flags:')
		g.comptime_definitions.writeln('//     All custom defines      : ' +
			g.pref.compile_defines_all.join(','))
		g.comptime_definitions.writeln('//     Turned ON custom defines: ' +
			g.pref.compile_defines.join(','))
		for cdefine in g.pref.compile_defines {
			g.comptime_definitions.writeln('#define CUSTOM_DEFINE_${cdefine}')
		}
		g.comptime_definitions.writeln('')
	}
	if g.table.gostmts > 0 {
		g.comptime_definitions.writeln('#define __VTHREADS__ (1)')
	}
	if g.pref.gc_mode in [.boehm_full, .boehm_incr, .boehm_full_opt, .boehm_incr_opt, .boehm_leak] {
		g.comptime_definitions.writeln('#define _VGCBOEHM (1)')
	}
	if g.pref.is_debug || 'debug' in g.pref.compile_defines {
		g.comptime_definitions.writeln('#define _VDEBUG (1)')
	}
	if g.pref.is_prod || 'prod' in g.pref.compile_defines {
		g.comptime_definitions.writeln('#define _VPROD (1)')
	}
	if g.pref.is_test || 'test' in g.pref.compile_defines {
		g.comptime_definitions.writeln('#define _VTEST (1)')
	}
	if g.pref.is_prof || 'profile' in g.pref.compile_defines {
		g.comptime_definitions.writeln('#define _VPROFILE (1)')
	}
	if g.pref.autofree {
		g.comptime_definitions.writeln('#define _VAUTOFREE (1)')
	}
	if g.pref.prealloc {
		g.comptime_definitions.writeln('#define _VPREALLOC (1)')
	}
	if g.pref.use_cache {
		g.comptime_definitions.writeln('#define _VUSECACHE (1)')
	}
	if g.pref.build_mode == .build_module {
		g.comptime_definitions.writeln('#define _VBUILDMODULE (1)')
	}
	if g.pref.is_livemain || g.pref.is_liveshared {
		g.generate_hotcode_reloading_declarations()
	}
	// we know that this is being called before the multi-threading starts
	// and this is being called in the main thread, so we can mutate the table
	mut muttable := unsafe { &ast.Table(g.table) }
	if g.use_segfault_handler {
		muttable.used_features.used_fns['v_segmentation_fault_handler'] = true
	}
	muttable.used_features.used_fns['eprintln'] = true
	muttable.used_features.used_fns['print_backtrace'] = true
	muttable.used_features.used_fns['exit'] = true
}

pub fn (mut g Gen) finish() {
	if g.pref.is_prof && g.pref.build_mode != .build_module {
		g.gen_vprint_profile_stats()
	}
	if g.pref.is_livemain || g.pref.is_liveshared {
		g.generate_hotcode_reloader_code()
	}
	g.handle_embedded_files_finish()
	if g.pref.is_test {
		g.gen_c_main_for_tests()
	} else if (g.pref.is_shared || g.pref.is_liveshared) && g.pref.os == .windows {
		// create DllMain() for windows .dll
		g.gen_dll_main()
	} else {
		g.gen_c_main()
	}
}

// get_sumtype_variant_type_name returns the variant type name according to its type
@[inline]
pub fn (mut g Gen) get_sumtype_variant_type_name(typ ast.Type, sym ast.TypeSymbol) string {
	return if typ.has_flag(.option) {
		'_option_${sym.cname}'
	} else if sym.is_c_struct() {
		g.cc_type(typ, true)
	} else {
		sym.cname
	}
}

// get_sumtype_variant_name returns the variant name according to its type
@[inline]
pub fn (mut g Gen) get_sumtype_variant_name(typ ast.Type, sym ast.TypeSymbol) string {
	return if typ.has_flag(.option) { '_option_${sym.cname}' } else { sym.cname }
}

pub fn (mut g Gen) write_typeof_functions() {
	g.writeln('')
	g.writeln('// >> typeof() support for sum types / interfaces')
	for ityp, sym in g.table.type_symbols {
		if sym.kind == .sum_type {
			if g.pref.skip_unused && sym.idx !in g.table.used_features.used_syms {
				continue
			}
			static_prefix := if g.pref.build_mode == .build_module { 'static ' } else { '' }
			sum_info := sym.info as ast.SumType
			if sum_info.is_generic {
				continue
			}
			g.writeln('${static_prefix}char * v_typeof_sumtype_${sym.cname}(int sidx) {')
			g.definitions.writeln('${static_prefix}char * v_typeof_sumtype_${sym.cname}(int);')
			if g.pref.build_mode == .build_module {
				g.writeln('\t\tif( sidx == _v_type_idx_${sym.cname}() ) return "${util.strip_main_name(sym.name)}";')
				for v in sum_info.variants {
					subtype := g.table.sym(v)
					g.writeln('\tif( sidx == _v_type_idx_${g.get_sumtype_variant_name(v,
						subtype)}() ) return "${util.strip_main_name(subtype.name)}";')
				}
				g.writeln('\treturn "unknown ${util.strip_main_name(sym.name)}";')
			} else {
				tidx := g.table.find_type_idx(sym.name)
				g.writeln('\tswitch(sidx) {')
				g.writeln('\t\tcase ${tidx}: return "${util.strip_main_name(sym.name)}";')
				mut idxs := []int{}
				for v in sum_info.variants {
					if v in idxs {
						continue
					}
					subtype := g.table.sym(v)
					g.writeln('\t\tcase ${int(v)}: return "${util.strip_main_name(subtype.name)}";')
					idxs << v
				}
				g.writeln('\t\tdefault: return "unknown ${util.strip_main_name(sym.name)}";')
				g.writeln('\t}')
			}
			g.writeln2('}', '')
			g.writeln('${static_prefix}int v_typeof_sumtype_idx_${sym.cname}(int sidx) {')
			if g.pref.build_mode == .build_module {
				g.writeln('\t\tif( sidx == _v_type_idx_${sym.cname}() ) return ${int(ityp)};')
				for v in sum_info.variants {
					subtype := g.table.sym(v)
					g.writeln('\tif( sidx == _v_type_idx_${subtype.cname}() ) return ${int(v)};')
				}
				g.writeln('\treturn ${int(ityp)};')
			} else {
				tidx := g.table.find_type_idx(sym.name)
				g.writeln2('\tswitch(sidx) {', '\t\tcase ${tidx}: return ${int(ityp)};')
				mut idxs := []int{}
				for v in sum_info.variants {
					if v in idxs {
						continue
					}
					g.writeln('\t\tcase ${int(v)}: return ${int(v)};')
					idxs << v
				}
				g.writeln2('\t\tdefault: return ${int(ityp)};', '\t}')
			}
			g.writeln('}')
		} else if sym.kind == .interface {
			if sym.info !is ast.Interface {
				continue
			}
			inter_info := sym.info as ast.Interface
			if inter_info.is_generic {
				continue
			}
			if g.pref.skip_unused && sym.idx !in g.table.used_features.used_syms {
				continue
			}
			g.definitions.writeln('${g.static_non_parallel}char * v_typeof_interface_${sym.cname}(int sidx);')
			if g.pref.parallel_cc {
				g.extern_out.writeln('extern char * v_typeof_interface_${sym.cname}(int sidx);')
			}
			g.writeln('${g.static_non_parallel}char * v_typeof_interface_${sym.cname}(int sidx) {')
			for t in inter_info.types {
				sub_sym := g.table.sym(ast.mktyp(t))
				if sub_sym.info is ast.Struct && sub_sym.info.is_unresolved_generic() {
					continue
				}
				if g.pref.skip_unused && sub_sym.kind == .struct
					&& sub_sym.idx !in g.table.used_features.used_syms {
					continue
				}
				g.writeln('\tif (sidx == _${sym.cname}_${sub_sym.cname}_index) return "${util.strip_main_name(sub_sym.name)}";')
			}
			g.writeln2('\treturn "unknown ${util.strip_main_name(sym.name)}";', '}')
			g.definitions.writeln('int v_typeof_interface_idx_${sym.cname}(int sidx);')
			g.writeln2('', 'int v_typeof_interface_idx_${sym.cname}(int sidx) {')
			if g.pref.parallel_cc {
				g.extern_out.writeln('extern int v_typeof_interface_idx_${sym.cname}(int sidx);')
			}
			for t in inter_info.types {
				sub_sym := g.table.sym(ast.mktyp(t))
				if sub_sym.info is ast.Struct && sub_sym.info.is_unresolved_generic() {
					continue
				}
				if g.pref.skip_unused && sub_sym.kind == .struct
					&& sub_sym.idx !in g.table.used_features.used_syms {
					continue
				}
				g.writeln('\tif (sidx == _${sym.cname}_${sub_sym.cname}_index) return ${int(t.set_nr_muls(0))};')
			}
			g.writeln2('\treturn ${int(ityp)};', '}')
		}
	}
	g.writeln2('// << typeof() support for sum types', '')
}

// V type to C typecc
@[inline]
fn (mut g Gen) styp(t ast.Type) string {
	if !t.has_option_or_result() {
		return g.base_type(t)
	} else if t.has_flag(.option) {
		// Register an optional if it's not registered yet
		return g.register_option(t)
	} else {
		return g.register_result(t)
	}
}

fn (mut g Gen) base_type(_t ast.Type) string {
	t := g.unwrap_generic(_t)
	if styp := g.styp_cache[t] {
		return styp
	}
	if g.pref.nofloat {
		// TODO: compile time if for perf?
		if t == ast.f32_type {
			return 'u32'
		} else if t == ast.f64_type {
			return 'u64'
		}
	}
	/*
	// On 64 bit systems int is an i64
	$if amd64 || arm64 {
		if g.pref.use_64_int && t == ast.int_type {
			return 'i64'
		}
	}
	*/
	share := t.share()
	mut styp := if share == .atomic_t { t.atomic_typename() } else { g.cc_type(t, true) }
	if t.has_flag(.shared_f) {
		styp = g.find_or_register_shared(t, styp)
	}
	nr_muls := t.nr_muls()
	if nr_muls > 0 {
		styp += strings.repeat(`*`, nr_muls)
	}
	g.styp_cache[t] = styp
	return styp
}

fn (mut g Gen) generic_fn_name(types []ast.Type, before string) string {
	if types.len == 0 {
		return before
	}
	// Using _T_ to differentiate between get[string] and get_string
	// `foo[int]()` => `foo_T_int()`
	mut name := before + '_T'
	for typ in types {
		name += '_' + strings.repeat_string('__ptr__', typ.nr_muls()) + g.styp(typ.set_nr_muls(0))
	}
	return name
}

fn (mut g Gen) expr_string(expr ast.Expr) string {
	pos := g.out.len
	// pos2 := 	g.out_parallel[g.out_idx].len
	g.expr(expr)
	// g.out_parallel[g.out_idx].cut_to(pos2)
	return g.out.cut_to(pos).trim_space()
}

fn (mut g Gen) expr_string_opt(typ ast.Type, expr ast.Expr) string {
	expr_str := g.expr_string(expr)
	if expr is ast.None {
		return '(${g.styp(typ)}){.state=2, .err=${expr_str}, .data={E_STRUCT}}'
	}
	return expr_str
}

fn (mut g Gen) expr_string_with_cast(expr ast.Expr, typ ast.Type, exp ast.Type) string {
	pos := g.out.len
	// pos2 := 	g.out_parallel[g.out_idx].len
	g.expr_with_cast(expr, typ, exp)
	// g.out_parallel[g.out_idx].cut_to(pos2)
	return g.out.cut_to(pos).trim_space()
}

// Surround a potentially multi-statement expression safely with `prepend` and `append`.
// (and create a statement)
fn (mut g Gen) expr_string_surround(prepend string, expr ast.Expr, append string) string {
	pos := g.out.len
	// pos2 := 	g.out_parallel[g.out_idx].len
	g.stmt_path_pos << pos
	defer {
		g.stmt_path_pos.delete_last()
	}
	g.write(prepend)
	g.expr(expr)
	g.write(append)
	// g.out_parallel[g.out_idx].cut_to(pos2)
	return g.out.cut_to(pos)
}

// TODO: this really shouldn't be separate from typ
// but I(emily) would rather have this generation
// all unified in one place so that it doesn't break
// if one location changes
fn (mut g Gen) option_type_name(t ast.Type) (string, string) {
	mut base := g.base_type(t)
	mut styp := ''
	sym := g.table.sym(t)
	if sym.info is ast.FnType {
		base = 'anon_fn_${g.table.fn_type_signature(sym.info.func)}'
	}
	if sym.language == .c && sym.kind == .struct {
		styp = '${option_name}_${base.replace(' ', '_')}'
	} else {
		styp = '${option_name}_${base}'
	}
	if t.has_flag(.generic) || t.is_ptr() {
		styp = styp.replace('*', '_ptr')
	}
	return styp, base
}

fn (mut g Gen) result_type_name(t ast.Type) (string, string) {
	mut base := g.base_type(t)
	if t.has_flag(.option) {
		g.register_option(t)
		base = '_option_' + base
	}
	mut styp := ''
	sym := g.table.sym(t)
	if sym.info is ast.FnType {
		base = 'anon_fn_${g.table.fn_type_signature(sym.info.func)}'
	}
	if sym.language == .c && sym.kind == .struct {
		styp = '${result_name}_${base.replace(' ', '_')}'
	} else {
		styp = '${result_name}_${base}'
	}
	if t.has_flag(.generic) || t.is_ptr() {
		styp = styp.replace('*', '_ptr')
	}
	return styp, base
}

fn (g &Gen) option_type_text(styp string, base string) string {
	// replace void with something else
	size := if base == 'void' {
		'u8'
	} else if base == 'int' {
		ast.int_type_name
	} else if base.starts_with('anon_fn') {
		'void*'
	} else if base.starts_with('_option_') {
		base.replace('*', '')
	} else {
		if base.starts_with('struct ') && !base.ends_with('*') { '${base}*' } else { base }
	}
	ret := 'struct ${styp} {
	byte state;
	IError err;
	byte data[sizeof(${size}) > 1 ? sizeof(${size}) : 1];
}'
	return ret
}

fn (g &Gen) result_type_text(styp string, base string) string {
	// replace void with something else
	size := if base == 'void' {
		'u8'
	} else if base == 'int' {
		ast.int_type_name
	} else if base.starts_with('anon_fn') {
		'void*'
	} else if base.starts_with('_option_') {
		base.replace('*', '')
	} else {
		if base.starts_with('struct ') && !base.ends_with('*') { '${base}*' } else { base }
	}
	ret := 'struct ${styp} {
	bool is_error;
	IError err;
	byte data[sizeof(${size}) > 1 ? sizeof(${size}) : 1];
}'
	return ret
}

fn (mut g Gen) register_option(t ast.Type) string {
	styp, base := g.option_type_name(t)
	g.options[base] = styp
	return if !t.has_flag(.option_mut_param_t) { styp } else { '${styp}*' }
}

fn (mut g Gen) register_result(t ast.Type) string {
	styp, base := g.result_type_name(t)
	g.results[base] = styp
	return styp
}

fn (mut g Gen) write_options() {
	mut done := []string{}
	rlock g.done_options {
		done = g.done_options.clone()
	}
	for base, styp in g.options {
		if base in done {
			continue
		}
		done << base
		g.typedefs.writeln('typedef struct ${styp} ${styp};')
		if base in g.options_forward {
			g.out_options_forward.write_string(g.option_type_text(styp, base) + ';\n\n')
		} else {
			g.out_options.write_string(g.option_type_text(styp, base) + ';\n\n')
		}
	}
}

fn (mut g Gen) write_results() {
	mut done := []string{}
	rlock g.done_results {
		done = g.done_results.clone()
	}
	for base, styp in g.results {
		if base in done {
			continue
		}
		done << base
		g.typedefs.writeln('typedef struct ${styp} ${styp};')
		if base in g.results_forward {
			g.out_results_forward.write_string(g.result_type_text(styp, base) + ';\n\n')
		} else {
			g.out_results.write_string(g.result_type_text(styp, base) + ';\n\n')
		}
	}
	for k, _ in g.table.anon_struct_names {
		ck := c_name(k)
		g.typedefs.writeln('typedef struct ${ck} ${ck};')
	}
	for k, _ in g.table.anon_union_names {
		ck := c_name(k)
		g.typedefs.writeln('typedef union ${ck} ${ck};')
	}
}

fn (mut g Gen) find_or_register_shared(t ast.Type, base string) string {
	g.shareds[t.idx()] = base
	return '__shared__${base}'
}

fn (mut g Gen) write_shareds() {
	mut done_types := []int{}
	for typ, base in g.shareds {
		if typ in done_types {
			continue
		}
		done_types << typ
		sh_typ := '__shared__${base}'
		mtx_typ := 'sync__RwMutex'
		g.shared_types.writeln('struct ${sh_typ} {')
		g.shared_types.writeln('\t${mtx_typ} mtx;')
		g.shared_types.writeln('\t${base} val;')
		g.shared_types.writeln('};')
		g.shared_functions.writeln('static inline voidptr __dup${sh_typ}(voidptr src, int sz) {')
		g.shared_functions.writeln('\t${sh_typ}* dest = memdup(src, sz);')
		g.shared_functions.writeln('\tsync__RwMutex_init(&dest->mtx);')
		g.shared_functions.writeln('\treturn dest;')
		g.shared_functions.writeln('}')
		g.typedefs.writeln('typedef struct ${sh_typ} ${sh_typ};')
	}
}

fn (mut g Gen) register_thread_void_wait_call() {
	lock g.waiter_fns {
		if '__v_thread_wait' in g.waiter_fns {
			return
		}
		g.waiter_fns << '__v_thread_wait'
		g.waiter_fn_definitions.writeln('void __v_thread_wait(__v_thread thread);')
	}
	g.gowrappers.writeln('void __v_thread_wait(__v_thread thread) {')
	if g.pref.os == .windows {
		g.gowrappers.writeln('\tu32 stat = WaitForSingleObject(thread, INFINITE);')
	} else {
		g.gowrappers.writeln('\tint stat = pthread_join(thread, (void **)NULL);')
	}
	g.gowrappers.writeln('\tif (stat != 0) { _v_panic(_S("unable to join thread")); }')
	if g.pref.os == .windows {
		g.gowrappers.writeln('\tCloseHandle(thread);')
	}
	g.gowrappers.writeln('}')
}

fn (mut g Gen) register_thread_array_wait_call(eltyp string) string {
	is_void := eltyp == 'void'
	thread_typ := if is_void { '__v_thread' } else { '__v_thread_${eltyp}' }
	ret_typ := if is_void { 'void' } else { 'Array_${eltyp}' }
	thread_arr_typ := 'Array_${thread_typ}'
	fn_name := '${thread_arr_typ}_wait'
	mut should_register := false
	lock g.waiter_fns {
		if fn_name !in g.waiter_fns {
			g.waiter_fns << fn_name
			should_register = true
		}
	}
	if should_register {
		if is_void {
			g.register_thread_void_wait_call()
			g.waiter_fn_definitions.writeln('void ${fn_name}(${thread_arr_typ} a);')
			g.gowrappers.writeln('
void ${fn_name}(${thread_arr_typ} a) {
	for (int i = 0; i < a.len; ++i) {
		${thread_typ} t = ((${thread_typ}*)a.data)[i];
		if (t == 0) continue;
		__v_thread_wait(t);
	}
}')
		} else {
			g.waiter_fn_definitions.writeln('${ret_typ} ${fn_name}(${thread_arr_typ} a);')
			g.gowrappers.writeln('
${ret_typ} ${fn_name}(${thread_arr_typ} a) {
	${ret_typ} res = __new_array_with_default(a.len, a.len, sizeof(${eltyp}), 0);
	for (int i = 0; i < a.len; ++i) {
		${thread_typ} t = ((${thread_typ}*)a.data)[i];')
			if g.pref.os == .windows {
				g.gowrappers.writeln('\t\tif (t.handle == 0) continue;')
			} else {
				g.gowrappers.writeln('\t\tif (t == 0) continue;')
			}
			g.gowrappers.writeln('\t\t((${eltyp}*)res.data)[i] = __v_thread_${eltyp}_wait(t);
	}
	return res;
}')
		}
	}
	return fn_name
}

fn (mut g Gen) register_thread_fixed_array_wait_call(node ast.CallExpr, eltyp string) string {
	is_void := eltyp == 'void'
	thread_typ := if is_void { '__v_thread' } else { '__v_thread_${eltyp}' }
	ret_typ := if is_void { 'void' } else { 'Array_${eltyp}' }
	rec_sym := g.table.sym(node.receiver_type)
	len := (rec_sym.info as ast.ArrayFixed).size
	thread_arr_typ := rec_sym.cname
	fn_name := '${thread_arr_typ}_wait'
	mut should_register := false
	lock g.waiter_fns {
		if fn_name !in g.waiter_fns {
			g.waiter_fns << fn_name
			should_register = true
		}
	}
	if should_register {
		if is_void {
			g.register_thread_void_wait_call()
			g.waiter_fn_definitions.writeln('void ${fn_name}(${thread_arr_typ} a);')
			g.gowrappers.writeln('
void ${fn_name}(${thread_arr_typ} a) {
	for (int i = 0; i < ${len}; ++i) {
		${thread_typ} t = ((${thread_typ}*)a)[i];
		if (t == 0) continue;
		__v_thread_wait(t);
	}
}')
		} else {
			g.waiter_fn_definitions.writeln('${ret_typ} ${fn_name}(${thread_arr_typ} a);')
			g.gowrappers.writeln('
${ret_typ} ${fn_name}(${thread_arr_typ} a) {
	${ret_typ} res = __new_array_with_default(${len}, ${len}, sizeof(${eltyp}), 0);
	for (int i = 0; i < ${len}; ++i) {
		${thread_typ} t = ((${thread_typ}*)a)[i];')
			if g.pref.os == .windows {
				g.gowrappers.writeln('\t\tif (t.handle == 0) continue;')
			} else {
				g.gowrappers.writeln('\t\tif (t == 0) continue;')
			}
			g.gowrappers.writeln('\t\t((${eltyp}*)res.data)[i] = __v_thread_${eltyp}_wait(t);
	}
	return res;
}')
		}
	}
	return fn_name
}

fn (mut g Gen) register_chan_pop_option_call(opt_el_type string, styp string) {
	g.chan_pop_options[opt_el_type] = styp
}

fn (mut g Gen) write_chan_pop_option_fns() {
	mut done := []string{}
	for opt_el_type, styp in g.chan_pop_options {
		if opt_el_type in done {
			continue
		}
		if g.pref.skip_unused {
			if sym := g.table.find_sym(opt_el_type) {
				if sym.idx !in g.table.used_features.used_syms {
					continue
				}
			}
		}
		done << opt_el_type
		g.channel_definitions.writeln('
static inline ${opt_el_type} __Option_${styp}_popval(${styp} ch) {
	${opt_el_type} _tmp = {0};
	if (sync__Channel_try_pop_priv(ch, _tmp.data, false)) {
		return (${opt_el_type}){ .state = 2, .err = _v_error(_S("channel closed")), .data = {E_STRUCT} };
	}
	return _tmp;
}')
	}
}

fn (mut g Gen) register_chan_push_option_fn(el_type string, styp string) {
	g.chan_push_options[styp] = el_type
}

fn (mut g Gen) write_chan_push_option_fns() {
	mut done := []string{}
	for styp, el_type in g.chan_push_options {
		if styp in done {
			continue
		}
		if g.pref.skip_unused {
			if sym := g.table.find_sym(el_type) {
				if sym.idx !in g.table.used_features.used_syms {
					continue
				}
			}
		}
		done << styp
		g.register_option(ast.void_type.set_flag(.option))
		g.channel_definitions.writeln('
static inline ${option_name}_void __Option_${styp}_pushval(${styp} ch, ${el_type} e) {
	if (sync__Channel_try_push_priv(ch, &e, false)) {
		return (${option_name}_void){ .state = 2, .err = _v_error(_S("channel closed")), .data = {E_STRUCT} };
	}
	return (${option_name}_void){0};
}')
	}
}

// cc_type whether to prefix 'struct' or not (C__Foo -> struct Foo)
fn (mut g Gen) cc_type(typ ast.Type, is_prefix_struct bool) string {
	sym := g.table.sym(g.unwrap_generic(typ))
	mut styp := sym.scoped_cname()
	// TODO: this needs to be removed; cgen shouldn't resolve generic types (job of checker)
	match sym.info {
		ast.Struct, ast.Interface, ast.SumType {
			if sym.info.is_generic && sym.generic_types.len == 0 {
				mut sgtyps := '_T'
				for gt in sym.info.generic_types {
					gts := g.table.sym(g.unwrap_generic(gt))
					sgtyps += '_${gts.cname}'
				}
				styp += sgtyps
			}
		}
		else {}
	}
	if is_prefix_struct && sym.language == .c {
		styp = styp[3..]
		if sym.kind == .struct {
			info := sym.info as ast.Struct
			if !info.is_typedef {
				styp = 'struct ${styp}'
			}
		}
	}
	return styp
}

@[inline]
fn (g &Gen) type_sidx(t ast.Type) string {
	if g.pref.build_mode == .build_module {
		sym := g.table.sym(t)
		return '_v_type_idx_${sym.cname}()'
	}
	return int(t).str()
}

pub fn (mut g Gen) write_typedef_types() {
	type_symbols := g.table.type_symbols.filter(!it.is_builtin
		&& it.kind in [.array, .array_fixed, .chan, .map])
	for sym in type_symbols {
		match sym.kind {
			.array {
				info := sym.info as ast.Array
				elem_sym := g.table.sym(info.elem_type)
				if elem_sym.kind != .placeholder && !info.elem_type.has_flag(.generic) {
					g.type_definitions.writeln('typedef array ${sym.cname};')
				}
			}
			.array_fixed {
				info := sym.info as ast.ArrayFixed
				elem_sym := g.table.sym(info.elem_type)
				if elem_sym.is_builtin() {
					styp := sym.cname
					len := info.size
					if len > 0 {
						mut fixed := g.styp(info.elem_type)
						if elem_sym.info is ast.FnType {
							pos := g.out.len
							// pos2:=g.out_parallel[g.out_idx].len
							g.write_fn_ptr_decl(&elem_sym.info, '')
							fixed = g.out.cut_to(pos)
							// g.out_parallel[g.out_idx].cut_to(pos2)
							mut def_str := 'typedef ${fixed};'
							def_str = def_str.replace_once('(*)', '(*${styp}[${len}])')
							g.type_definitions.writeln(def_str)
						} else if !info.is_fn_ret {
							base := g.styp(info.elem_type.clear_option_and_result())
							if info.elem_type.has_flag(.option) && base !in g.options_forward {
								lock g.done_options {
									if base !in g.done_options {
										g.type_definitions.writeln('typedef ${fixed} ${styp} [${len}];')
										g.options_forward << base
										g.done_options << styp
									}
								}
							} else {
								g.type_definitions.writeln('typedef ${fixed} ${styp} [${len}];')
								if info.elem_type.has_flag(.result) && base !in g.results_forward {
									g.results_forward << base
								}
							}
						}
					}
				}
			}
			.chan {
				if sym.name != 'chan' {
					chan_inf := sym.chan_info()
					chan_elem_type := chan_inf.elem_type
					esym := g.table.sym(chan_elem_type)
					if g.pref.skip_unused && esym.idx !in g.table.used_features.used_syms {
						continue
					}
					g.type_definitions.writeln('typedef chan ${sym.cname};')
					is_fixed_arr := esym.kind == .array_fixed
					if !chan_elem_type.has_flag(.generic) {
						el_stype := if is_fixed_arr { '_v_' } else { '' } + g.styp(chan_elem_type)
						val_arg_pop := if is_fixed_arr { '&val.ret_arr' } else { '&val' }
						val_arg_push := if is_fixed_arr { 'val' } else { '&val' }
						push_arg := el_stype + if is_fixed_arr { '*' } else { '' }
						g.channel_definitions.writeln('
static inline ${el_stype} __${sym.cname}_popval(${sym.cname} ch) {
	${el_stype} val;
	sync__Channel_try_pop_priv(ch, ${val_arg_pop}, false);
	return val;
}')
						g.channel_definitions.writeln('
static inline void __${sym.cname}_pushval(${sym.cname} ch, ${push_arg} val) {
	sync__Channel_try_push_priv(ch, ${val_arg_push}, false);
}')
					}
				}
			}
			.map {
				if g.pref.skip_unused && g.table.used_features.used_maps == 0 {
					continue
				}
				g.type_definitions.writeln('typedef map ${sym.cname};')
			}
			else {
				continue
			}
		}
	}
	for sym in g.table.type_symbols {
		if sym.kind == .alias && !sym.is_builtin && sym.name !in ['byte', 'i32'] {
			if g.pref.skip_unused && sym.idx !in g.table.used_features.used_syms {
				continue
			}
			g.write_alias_typesymbol_declaration(sym)
		}
	}
	g.write_sorted_fn_typesymbol_declaration()
	// Generating interfaces after all the common types have been defined
	// to prevent generating interface struct before definition of field types

	interfaces := g.table.type_symbols.filter(it.info is ast.Interface && !it.is_builtin)
	mut interface_non_generic_syms := []&ast.TypeSymbol{cap: interfaces.len}
	for sym in interfaces {
		g.write_interface_typedef(sym)
		if sym.info is ast.Interface && !sym.info.is_generic {
			interface_non_generic_syms << sym
		}
	}
	for sym in interface_non_generic_syms {
		if g.pref.skip_unused && sym.idx !in g.table.used_features.used_syms {
			continue
		}
		g.write_interface_typesymbol_declaration(sym)
	}
}

pub fn (mut g Gen) write_alias_typesymbol_declaration(sym ast.TypeSymbol) {
	mut levels := 0
	parent := g.table.type_symbols[sym.parent_idx]
	is_c_parent := parent.name.len > 2 && parent.name[0] == `C` && parent.name[1] == `.`
	mut is_fixed_array_of_non_builtin := false
	mut parent_styp := parent.cname
	if is_c_parent {
		if sym.info is ast.Alias {
			parent_styp = g.styp(sym.info.parent_type)
		}
	} else {
		if sym.info is ast.Alias {
			parent_styp = g.styp(sym.info.parent_type)
			parent_sym := g.table.sym(sym.info.parent_type)
			if parent_sym.info is ast.ArrayFixed {
				mut elem_sym := g.table.sym(parent_sym.info.elem_type)
				if !elem_sym.is_builtin() {
					is_fixed_array_of_non_builtin = true
				}

				mut parent_elem_info := parent_sym.info as ast.ArrayFixed
				mut parent_elem_styp := g.styp(sym.info.parent_type)
				mut out := strings.new_builder(50)
				for {
					if mut elem_sym.info is ast.ArrayFixed {
						old := out.str()
						out.clear()
						out.writeln('typedef ${elem_sym.cname} ${parent_elem_styp} [${parent_elem_info.size}];')
						out.writeln(old)
						parent_elem_styp = elem_sym.cname
						parent_elem_info = elem_sym.info as ast.ArrayFixed
						elem_sym = g.table.sym(elem_sym.info.elem_type)
						levels++
					} else {
						break
					}
				}
				if out.len != 0 {
					g.type_definitions.writeln(out.str())
				}
			}
		}
	}
	if parent_styp == 'byte' && sym.cname == 'u8' {
		// TODO: remove this check; it is here just to fix V rebuilding in -cstrict mode with clang-12
		return
	}
	if sym.name.starts_with('C.') {
		// `pub type C.HINSTANCE = voidptr` means that `HINSTANCE` should be treated as a voidptr by V.
		// The C type itself however already exists on the C side, so just treat C__HINSTANCE as a macro for it:
		g.type_definitions.writeln('#define ${sym.cname} ${sym.cname#[3..]}')
		return
	}
	if is_fixed_array_of_non_builtin && levels == 0 {
		g.alias_definitions.writeln('typedef ${parent_styp} ${sym.cname};')
	} else {
		g.type_definitions.writeln('typedef ${parent_styp} ${sym.cname};')
	}
}

pub fn (mut g Gen) write_interface_typedef(sym ast.TypeSymbol) {
	struct_name := c_name(sym.cname)
	g.typedefs.writeln('typedef struct ${struct_name} ${struct_name};')
}

pub fn (mut g Gen) write_interface_typesymbol_declaration(sym ast.TypeSymbol) {
	info := sym.info as ast.Interface
	struct_name := c_name(sym.cname)
	g.type_definitions.writeln('struct ${struct_name} {')
	g.type_definitions.writeln('\tunion {')
	g.type_definitions.writeln('\t\tvoid* _object;')
	for variant in info.types {
		mk_typ := ast.mktyp(variant)
		if mk_typ != variant && mk_typ in info.types {
			continue
		}
		vsym := g.table.sym(mk_typ)
		if g.pref.skip_unused && vsym.idx !in g.table.used_features.used_syms {
			continue
		}
		vcname := vsym.cname
		g.type_definitions.writeln('\t\t${vcname}* _${vcname};')
	}
	g.type_definitions.writeln('\t};')
	g.type_definitions.writeln('\tint _typ;')
	for field in info.fields {
		styp := g.styp(field.typ)
		cname := c_name(field.name)
		g.type_definitions.writeln('\t${styp}* ${cname};')
	}
	g.type_definitions.writeln('};')
}

pub fn (mut g Gen) write_fn_typesymbol_declaration(sym ast.TypeSymbol) {
	info := sym.info as ast.FnType
	func := info.func
	is_fn_sig := func.name == ''
	not_anon := !info.is_anon
	mut has_generic_arg := false

	if g.pref.skip_unused && sym.idx !in g.table.used_features.used_syms {
		return
	}

	for param in func.params {
		if param.typ.has_flag(.generic) {
			has_generic_arg = true
			break
		}
	}
	if !info.has_decl && (not_anon || is_fn_sig) && !func.return_type.has_flag(.generic)
		&& !has_generic_arg {
		fn_name := sym.cname
		mut call_conv := ''
		mut msvc_call_conv := ''
		for attr in func.attrs {
			match attr.name {
				'callconv' {
					if g.is_cc_msvc {
						msvc_call_conv = '__${attr.arg} '
					} else {
						call_conv = '${attr.arg}'
					}
				}
				else {}
			}
		}
		call_conv_attribute_suffix := if call_conv != '' {
			'__attribute__((${call_conv}))'
		} else {
			''
		}
		ret_typ :=
			if !func.return_type.has_flag(.option) && g.table.sym(func.return_type).kind == .array_fixed { '_v_' } else { '' } +
			g.styp(func.return_type)
		g.type_definitions.write_string('typedef ${ret_typ} (${msvc_call_conv}*${fn_name})(')
		for i, param in func.params {
			g.type_definitions.write_string(g.styp(param.typ))
			if i < func.params.len - 1 {
				g.type_definitions.write_string(',')
			}
		}
		g.type_definitions.writeln(')${call_conv_attribute_suffix};')
	}
}

pub fn (mut g Gen) write_array_fixed_return_types() {
	fixed_arr_rets := g.table.type_symbols.filter(it.info is ast.ArrayFixed && it.info.is_fn_ret
		&& !it.info.elem_type.has_flag(.generic))
	if fixed_arr_rets.len == 0 {
		return
	}

	g.typedefs.writeln('\n// BEGIN_array_fixed_return_typedefs')
	g.type_definitions.writeln('\n// BEGIN_array_fixed_return_structs')

	for sym in fixed_arr_rets {
		info := sym.info as ast.ArrayFixed
		if info.size <= 0 {
			// unresolved sizes e.g. [unknown_const]int
			continue
		}
		mut fixed_elem_name := g.styp(info.elem_type.set_nr_muls(0))
		if info.elem_type.is_ptr() {
			fixed_elem_name += '*'.repeat(info.elem_type.nr_muls())
		}
		g.typedefs.writeln('typedef struct ${sym.cname} ${sym.cname};')
		g.type_definitions.writeln('struct ${sym.cname} {')
		g.type_definitions.writeln('\t${fixed_elem_name} ret_arr[${info.size}];')
		g.type_definitions.writeln('};')
	}

	g.typedefs.writeln('// END_array_fixed_return_typedefs\n')
	g.type_definitions.writeln('// END_array_fixed_return_structs\n')
}

pub fn (mut g Gen) write_multi_return_types() {
	start_pos := g.type_definitions.len
	g.typedefs.writeln('\n// BEGIN_multi_return_typedefs')
	g.type_definitions.writeln('\n// BEGIN_multi_return_structs')
	multi_rets := g.table.type_symbols.filter(it.kind == .multi_return && !it.cname.contains('['))
	for sym in multi_rets {
		info := sym.mr_info()
		if info.types.any(it.has_flag(.generic)) {
			continue
		}
		if g.pref.skip_unused && sym.idx !in g.table.used_features.used_syms {
			continue
		}
		g.typedefs.writeln('typedef struct ${sym.cname} ${sym.cname};')
		g.type_definitions.writeln('struct ${sym.cname} {')
		for i, mr_typ in info.types {
			type_name := g.styp(mr_typ)
			if mr_typ.has_flag(.option) {
				// option in multi_return
				// Dont use g.styp() here because it will register
				// option and we dont want that
				styp, base := g.option_type_name(mr_typ)
				lock g.done_options {
					if base !in g.done_options {
						g.done_options << base
						last_text := g.type_definitions.after(start_pos).clone()
						g.type_definitions.go_back_to(start_pos)
						g.typedefs.writeln('typedef struct ${styp} ${styp};')
						g.type_definitions.writeln('${g.option_type_text(styp, base)};')
						g.type_definitions.write_string(last_text)
					}
				}
			}
			if mr_typ.has_flag(.result) {
				// result in multi_return
				// Dont use g.styp() here because it will register
				// result and we dont want that
				styp, base := g.result_type_name(mr_typ)
				lock g.done_results {
					if base !in g.done_results {
						g.done_results << base
						last_text := g.type_definitions.after(start_pos).clone()
						g.type_definitions.go_back_to(start_pos)
						g.typedefs.writeln('typedef struct ${styp} ${styp};')
						g.type_definitions.writeln('${g.result_type_text(styp, base)};')
						g.type_definitions.write_string(last_text)
					}
				}
			}
			g.type_definitions.writeln('\t${type_name} arg${i};')
		}
		g.type_definitions.writeln('};\n')
	}
	g.typedefs.writeln('// END_multi_return_typedefs\n')
	g.type_definitions.writeln('// END_multi_return_structs\n')
}

@[inline]
fn prefix_with_counter(prefix string, counter int) string {
	mut sb := strings.new_builder(prefix.len + 5)
	sb.write_string(prefix)
	sb.write_decimal(counter)
	return sb.str()
}

pub fn (mut g Gen) new_tmp_var() string {
	g.tmp_count++
	return prefix_with_counter('_t', g.tmp_count)
}

pub fn (mut g Gen) new_global_tmp_var() string {
	g.global_tmp_count++
	return prefix_with_counter('_t', g.global_tmp_count)
}

pub fn (mut g Gen) current_tmp_var() string {
	return prefix_with_counter('_t', g.tmp_count)
}

pub fn (mut g Gen) reset_tmp_count() int {
	old := g.tmp_count
	g.tmp_count = 0
	return old
}

fn (mut g Gen) decrement_inside_ternary() {
	key := g.inside_ternary.str()
	for name in g.ternary_level_names[key] {
		g.ternary_names.delete(name)
	}
	g.ternary_level_names.delete(key)
	g.inside_ternary--
}

fn (mut g Gen) stmts(stmts []ast.Stmt) {
	g.stmts_with_tmp_var(stmts, '')
}

fn is_noreturn_callexpr(expr ast.Expr) bool {
	if expr is ast.CallExpr {
		return expr.is_noreturn
	}
	return false
}

// stmts_with_tmp_var is used in `if` or `match` branches.
// It returns true, if the last statement was a `return`
fn (mut g Gen) stmts_with_tmp_var(stmts []ast.Stmt, tmp_var string) bool {
	g.indent++
	if g.inside_ternary > 0 {
		g.write('(')
	}
	mut last_stmt_was_return := false
	for i, stmt in stmts {
		if i == stmts.len - 1 {
			if stmt is ast.Return {
				last_stmt_was_return = true
			}
		}
		if i == stmts.len - 1 && tmp_var != '' {
			// Handle if expressions, set the value of the last expression to the temp var.
			if g.inside_if_option || g.inside_match_option {
				g.set_current_pos_as_last_stmt_pos()
				g.skip_stmt_pos = true
				if stmt is ast.ExprStmt {
					if stmt.typ == ast.error_type_idx || stmt.expr is ast.None {
						g.writeln('${tmp_var}.state = 2;')
						g.write('${tmp_var}.err = ')
						g.expr(stmt.expr)
						g.writeln(';')
					} else {
						mut styp := g.base_type(stmt.typ)
						$if tinyc && x32 && windows {
							if stmt.typ == ast.int_literal_type {
								styp = 'int'
							} else if stmt.typ == ast.float_literal_type {
								styp = 'f64'
							}
						}
						if stmt.typ.has_flag(.option)
							|| (g.inside_if_option && stmt.expr is ast.IfExpr) {
							g.writeln('')
							g.write('${tmp_var} = ')
							g.expr(stmt.expr)
							g.writeln(';')
						} else {
							// on assignment or struct field initialization
							inside_assign_context := g.inside_struct_init
								|| g.inside_assign
								|| (!g.inside_return && g.inside_match_option)
							ret_expr_typ := if inside_assign_context {
								stmt.typ
							} else {
								g.fn_decl.return_type
							}
							ret_typ := if inside_assign_context {
								stmt.typ
							} else {
								g.fn_decl.return_type.clear_flag(.option)
							}
							styp = g.base_type(ret_typ)
							if stmt.expr is ast.CallExpr && stmt.expr.is_noreturn {
								g.writeln(';')
							} else if !ret_expr_typ.has_option_or_result()
								&& !stmt.typ.has_option_or_result() && g.last_if_option_type != 0
								&& !g.last_if_option_type.has_option_or_result() {
								g.write('${tmp_var} = ')
								g.expr_with_cast(stmt.expr, stmt.typ, ret_typ)
								g.writeln(';')
							} else {
								g.write('_option_ok(&(${styp}[]) { ')
								g.expr_with_cast(stmt.expr, stmt.typ, ret_typ)
								g.writeln(' }, (${option_name}*)(&${tmp_var}), sizeof(${styp}));')
							}
						}
					}
				} else if stmt is ast.Return {
					g.stmt(stmt)
				}
			} else if g.inside_if_result || g.inside_match_result {
				g.set_current_pos_as_last_stmt_pos()
				g.skip_stmt_pos = true
				if stmt is ast.ExprStmt {
					if stmt.typ == ast.error_type_idx {
						g.writeln('${tmp_var}.is_error = true;')
						g.write('${tmp_var}.err = ')
						g.expr(stmt.expr)
						g.writeln(';')
					} else {
						mut styp := g.base_type(stmt.typ)
						$if tinyc && x32 && windows {
							if stmt.typ == ast.int_literal_type {
								styp = 'int'
							} else if stmt.typ == ast.float_literal_type {
								styp = 'f64'
							}
						}
						if stmt.typ.has_flag(.result) {
							g.writeln('')
							g.write('${tmp_var} = ')
							g.expr(stmt.expr)
							g.writeln(';')
						} else {
							ret_typ := g.fn_decl.return_type.clear_flag(.result)
							styp = g.base_type(ret_typ)
							if stmt.expr is ast.CallExpr && stmt.expr.is_noreturn {
								g.expr(stmt.expr)
								g.writeln(';')
							} else {
								g.write('_result_ok(&(${styp}[]) { ')
								g.expr_with_cast(stmt.expr, stmt.typ, ret_typ)
								g.writeln(' }, (${result_name}*)(&${tmp_var}), sizeof(${styp}));')
							}
						}
					}
				} else if stmt is ast.Return {
					g.stmt(stmt)
				}
			} else {
				mut is_array_fixed_init := false
				mut ret_type := ast.void_type

				g.set_current_pos_as_last_stmt_pos()
				g.skip_stmt_pos = true
				mut is_noreturn := false
				if stmt in [ast.Return, ast.BranchStmt] {
					is_noreturn = true
				} else if stmt is ast.ExprStmt {
					is_noreturn = is_noreturn_callexpr(stmt.expr)
					if stmt.expr is ast.ArrayInit && stmt.expr.is_fixed {
						is_array_fixed_init = true
						ret_type = stmt.expr.typ
					}
				}
				if !is_noreturn {
					if is_array_fixed_init {
						g.write('memcpy(${tmp_var}, (${g.styp(ret_type)})')
					} else {
						g.write('${tmp_var} = ')
					}
				}
				g.stmt(stmt)
				if is_array_fixed_init {
					lines := g.go_before_last_stmt().trim_right('; \n')
					g.writeln('${lines}, sizeof(${tmp_var}));')
				}
				if !g.out.last_n(2).contains(';') {
					g.writeln(';')
				}
			}
		} else {
			if i > 0 && stmt is ast.Return {
				last_stmt := stmts[i - 1]
				if last_stmt is ast.ExprStmt && last_stmt.expr is ast.CallExpr
					&& !g.out.last_n(2).contains(';') {
					g.writeln(';')
				}
			}
			g.stmt(stmt)
			if stmt is ast.ExprStmt && (g.inside_if_option || g.inside_if_result
				|| g.inside_match_option || g.inside_match_result
				|| (stmt.expr is ast.IndexExpr && stmt.expr.or_expr.kind != .absent)) {
				g.writeln(';')
			}
		}
		g.skip_stmt_pos = false
		if g.inside_ternary > 0 && i < stmts.len - 1 {
			g.write(',')
		}
	}
	g.indent--
	if g.inside_ternary > 0 {
		g.write2('', ')')
	}
	if g.is_autofree && !g.inside_vweb_tmpl && stmts.len > 0 {
		// use the first stmt to get the scope
		stmt := stmts[0]
		if stmt !is ast.FnDecl && g.inside_ternary == 0 {
			// g.trace_autofree('// autofree scope')
			// g.trace_autofree('// autofree_scope_vars($stmt.pos.pos) | ${typeof(stmt)}')
			// go back 1 position is important so we dont get the
			// internal scope of for loops and possibly other nodes
			// g.autofree_scope_vars(stmt.pos.pos - 1)
			mut stmt_pos := stmt.pos
			if stmt_pos.pos == 0 {
				// Do not autofree if the position is 0, since the correct scope won't be found.
				// Report a bug, since position shouldn't be 0 for most nodes.
				if stmt is ast.Module {
					return last_stmt_was_return
				}
				if stmt is ast.ExprStmt {
					// For some reason ExprStmt.pos is 0 when ExprStmt.expr is comp if expr
					// Extract the pos. TODO figure out why and fix.
					stmt_pos = stmt.expr.pos()
				}
				if stmt_pos.pos == 0 {
					$if trace_autofree ? {
						println('autofree: first stmt pos = 0. ${stmt.type_name()}')
					}
					return last_stmt_was_return
				}
			}
			g.autofree_scope_vars(stmt_pos.pos - 1, stmt_pos.line_nr, false)
		}
	}
	return last_stmt_was_return
}

// expr_with_tmp_var is used in assign expr to `option` or `result` type.
// applicable to situations where the expr_typ does not have `option` and `result`,
// e.g. field default: "foo ?int = 1", field assign: "foo = 1", field init: "foo: 1"
fn (mut g Gen) expr_with_tmp_var(expr ast.Expr, expr_typ ast.Type, ret_typ ast.Type, tmp_var string) {
	if !ret_typ.has_option_or_result() {
		panic('cgen: parameter `ret_typ` of function `expr_with_tmp_var()` must be an Option or Result')
	}

	stmt_str := g.go_before_last_stmt().trim_space()
	mut styp := g.base_type(ret_typ)
	g.empty_line = true
	final_expr_sym := g.table.final_sym(expr_typ)

	if final_expr_sym.kind == .none {
		g.write('${g.styp(ret_typ)} ${tmp_var} = ')
		g.gen_option_error(ret_typ, expr)
		g.writeln(';')
	} else if expr is ast.Ident && expr_typ == ast.error_type {
		g.writeln('${g.styp(ret_typ)} ${tmp_var} = {.state=2, .err=${expr.name}};')
	} else {
		mut simple_assign := false
		expr_typ_is_option := expr_typ.has_flag(.option)
		ret_typ_is_option := ret_typ.has_flag(.option)
		if ret_typ.has_flag(.generic) {
			if expr is ast.SelectorExpr && g.cur_concrete_types.len == 0 {
				// resolve generic struct on selectorExpr inside non-generic function
				if expr.expr is ast.Ident {
					if expr.expr.obj is ast.Var {
						if expr.expr.obj.expr is ast.StructInit {
							struct_info := g.table.sym(expr.expr.obj.typ).info
							if struct_info is ast.Struct {
								g.cur_concrete_types << struct_info.concrete_types
							}
						}
					}
				}
			}
			unwrapped_ret_typ := g.unwrap_generic(ret_typ)
			styp = g.base_type(unwrapped_ret_typ)
			ret_styp := g.styp(unwrapped_ret_typ).replace('*', '_ptr')
			g.writeln('${ret_styp} ${tmp_var};')
		} else {
			if ret_typ.has_flag(.option_mut_param_t) {
				ret_styp := g.styp(ret_typ).replace('*', '')
				g.writeln('${ret_styp} ${tmp_var};')
			} else {
				g.writeln('${g.styp(ret_typ)} ${tmp_var};')
			}
		}
		mut expr_is_fixed_array_var := false
		mut fn_option_clone := false
		if ret_typ_is_option {
			if expr_typ_is_option && expr in [ast.StructInit, ast.ArrayInit, ast.MapInit] {
				simple_assign = expr is ast.StructInit
				if simple_assign {
					g.write('${tmp_var} = ')
				} else {
					g.write('_option_none(&(${styp}[]) { ')
				}
			} else {
				simple_assign =
					((expr is ast.SelectorExpr || (expr is ast.Ident && !expr.is_auto_heap()))
					&& ret_typ.is_ptr() && expr_typ.is_ptr() && expr_typ_is_option)
					|| (expr_typ == ret_typ && !(expr_typ.has_option_or_result()
					&& (expr_typ.is_ptr() || expr is ast.LambdaExpr)))
				// option ptr assignment simplification
				if simple_assign {
					g.write('${tmp_var} = ')
				} else if expr_typ_is_option && expr is ast.PrefixExpr
					&& expr.right is ast.StructInit
					&& (expr.right as ast.StructInit).init_fields.len == 0 {
					g.write('_option_none(&(${styp}[]) { ')
				} else if expr in [ast.Ident, ast.SelectorExpr]
					&& final_expr_sym.kind == .array_fixed {
					expr_is_fixed_array_var = true
					g.write('_option_ok(&')
				} else {
					g.write('_option_ok(&(${styp}[]) { ')
					if final_expr_sym.info is ast.FnType {
						final_ret_sym := g.table.final_sym(ret_typ)
						if final_ret_sym.info is ast.FnType {
							g.write('(')
							g.write_fn_ptr_decl(&final_ret_sym.info, '')
							g.write(')')
							fn_option_clone = expr is ast.SelectorExpr && expr_typ.has_flag(.option)
						}
					}
				}
				if !expr.is_literal() && expr_typ != ast.nil_type
					&& ret_typ.nr_muls() > expr_typ.nr_muls()
					&& !ret_typ.has_flag(.option_mut_param_t) {
					g.write('&'.repeat(ret_typ.nr_muls() - expr_typ.nr_muls()))
				} else if ret_typ.has_flag(.option_mut_param_t) {
					if expr_typ.is_ptr() {
						if ret_typ.nr_muls() < expr_typ.nr_muls() {
							g.write('*')
						}
					} else {
						if expr_typ.has_flag(.option) {
							fn_option_clone = true
							g.write('(${styp})')
						}
						g.write('&')
					}
				}
			}
		} else {
			g.write('_result_ok(&(${styp}[]) { ')
		}
		g.expr_with_cast(expr, expr_typ, ret_typ)
		if fn_option_clone {
			g.write('.data')
		}
		if ret_typ_is_option {
			if simple_assign {
				g.writeln(';')
			} else if expr_is_fixed_array_var {
				g.writeln(', (${option_name}*)(&${tmp_var}), sizeof(${styp}));')
			} else {
				g.writeln(' }, (${option_name}*)(&${tmp_var}), sizeof(${styp}));')
			}
		} else {
			g.writeln(' }, (${result_name}*)(&${tmp_var}), sizeof(${styp}));')
		}
		g.set_current_pos_as_last_stmt_pos()
	}

	g.write2(stmt_str, ' ')
	g.write(tmp_var)
}

@[inline]
fn (mut g Gen) write_v_source_line_info_pos(pos token.Pos) {
	if g.inside_ternary == 0 && g.pref.is_vlines && g.is_vlines_enabled {
		nline := pos.line_nr + 1
		lineinfo := '\n#line ${nline} "${g.vlines_path}"'
		$if trace_gen_source_line_info ? {
			eprintln('> lineinfo: ${lineinfo.replace('\n', '')}')
		}
		g.writeln(lineinfo)
	}
}

@[inline]
fn (mut g Gen) write_v_source_line_info(node ast.Node) {
	g.write_v_source_line_info_pos(node.pos())
	if g.inside_ternary == 0 && g.pref.is_coverage
		&& node !in [ast.MatchBranch, ast.IfBranch, ast.InfixExpr] {
		g.write_coverage_point(node.pos())
	}
}

@[inline]
fn (mut g Gen) write_v_source_line_info_stmt(stmt ast.Stmt) {
	g.write_v_source_line_info_pos(stmt.pos)
	if g.inside_ternary == 0 && g.pref.is_coverage && !g.inside_for_c_stmt
		&& stmt !in [ast.FnDecl, ast.ForCStmt, ast.ForInStmt, ast.ForStmt] {
		if stmt is ast.AssertStmt {
			if stmt.expr !in [ast.InfixExpr, ast.MatchExpr] {
				g.write_coverage_point(stmt.pos)
			}
		} else {
			g.write_coverage_point(stmt.pos)
		}
	}
}

fn (mut g Gen) stmt(node ast.Stmt) {
	$if trace_cgen_stmt ? {
		ntype := typeof(node).replace('v.ast.', '')
		if g.file == unsafe { nil } {
			eprintln('cgen: <nil> | pos: ${node.pos.line_str():-39} | node: ${ntype} | ${node}')
		} else {
			eprintln('cgen: ${g.file.path:-30} | pos: ${node.pos.line_str():-39} | node: ${ntype} | ${node}')
		}
	}
	old_inside_call := g.inside_call
	g.inside_call = false
	defer {
		g.inside_call = old_inside_call
	}
	if !g.skip_stmt_pos {
		g.set_current_pos_as_last_stmt_pos()
	}
	match node {
		ast.FnDecl {
			g.fn_decl(node)
		}
		ast.Block {
			g.write_v_source_line_info_stmt(node)
			if !node.is_unsafe {
				g.writeln('{')
			} else {
				if g.pref.is_prod {
					g.writeln('{')
				} else {
					g.writeln('{ // Unsafe block')
				}
			}
			g.stmts(node.stmts)
			g.writeln('}')
		}
		ast.AssignStmt {
			g.write_v_source_line_info_stmt(node)
			g.assign_stmt(node)
		}
		ast.AssertStmt {
			g.write_v_source_line_info_stmt(node)
			g.assert_stmt(node)
		}
		ast.ConstDecl {
			g.write_v_source_line_info_stmt(node)
			g.const_decl(node)
		}
		ast.ComptimeFor {
			g.comptime_for(node)
		}
		ast.BranchStmt {
			g.write_v_source_line_info_stmt(node)
			g.branch_stmt(node)
		}
		ast.ExprStmt {
			g.write_v_source_line_info_stmt(node)
			// af := g.autofree && node.expr is ast.CallExpr && !g.is_builtin_mod
			// if af {
			// g.autofree_call_pregen(node.expr as ast.CallExpr)
			// }
			old_is_void_expr_stmt := g.is_void_expr_stmt
			g.is_void_expr_stmt = !node.is_expr
			if node.expr.is_auto_deref_var() {
				g.write('*')
			}
			if node.typ != ast.void_type && g.expected_cast_type != 0
				&& node.expr !in [ast.IfExpr, ast.MatchExpr] {
				g.expr_with_cast(node.expr, node.typ, g.expected_cast_type)
			} else {
				g.expr(node.expr)
			}
			g.is_void_expr_stmt = old_is_void_expr_stmt
			if g.inside_ternary == 0 && !g.inside_if_option && !g.inside_match_option
				&& !g.inside_if_result && !g.inside_match_result && !node.is_expr
				&& node.expr !is ast.IfExpr {
				if node.expr is ast.MatchExpr {
					g.writeln('')
				} else {
					g.writeln(';')
				}
			}
		}
		ast.ForCStmt {
			prev_branch_parent_pos := g.branch_parent_pos
			g.branch_parent_pos = node.pos.pos
			save_inner_loop := g.inner_loop
			g.inner_loop = unsafe { &node }
			if node.label != '' {
				unsafe {
					g.labeled_loops[node.label] = &node
				}
			}
			g.write_v_source_line_info_stmt(node)
			g.for_c_stmt(node)
			g.branch_parent_pos = prev_branch_parent_pos
			g.labeled_loops.delete(node.label)
			g.inner_loop = save_inner_loop
		}
		ast.ForInStmt {
			prev_branch_parent_pos := g.branch_parent_pos
			g.branch_parent_pos = node.pos.pos
			save_inner_loop := g.inner_loop
			g.inner_loop = unsafe { &node }
			if node.label != '' {
				unsafe {
					g.labeled_loops[node.label] = &node
				}
			}
			g.write_v_source_line_info_stmt(node)
			g.for_in_stmt(node)
			g.branch_parent_pos = prev_branch_parent_pos
			g.labeled_loops.delete(node.label)
			g.inner_loop = save_inner_loop
		}
		ast.ForStmt {
			prev_branch_parent_pos := g.branch_parent_pos
			g.branch_parent_pos = node.pos.pos
			save_inner_loop := g.inner_loop
			g.inner_loop = unsafe { &node }
			if node.label != '' {
				unsafe {
					g.labeled_loops[node.label] = &node
				}
			}
			g.write_v_source_line_info_stmt(node)
			g.for_stmt(node)
			g.branch_parent_pos = prev_branch_parent_pos
			g.labeled_loops.delete(node.label)
			g.inner_loop = save_inner_loop
		}
		ast.Return {
			g.return_stmt(node)
		}
		ast.DeferStmt {
			mut defer_stmt := node
			defer_stmt.ifdef = g.defer_ifdef
			g.writeln('${g.defer_flag_var(defer_stmt)} = true;')
			g.defer_stmts << defer_stmt
		}
		ast.EnumDecl {
			g.enum_decl(node)
		}
		ast.GlobalDecl {
			g.global_decl(node)
		}
		ast.Import {}
		ast.InterfaceDecl {
			// definitions are sorted and added in write_types
			ts := g.table.sym(node.typ)
			if !(ts.info as ast.Interface).is_generic {
				for method in node.methods {
					if method.return_type.has_flag(.option) {
						// Register an option if it's not registered yet
						g.register_option(method.return_type)
					} else if method.return_type.has_flag(.result) {
						if g.pref.skip_unused
							&& g.table.sym(method.return_type).idx !in g.table.used_features.used_syms {
							continue
						}
						// Register a result if it's not registered yet
						g.register_result(method.return_type)
					}
				}
			}
		}
		ast.StructDecl {
			name := if node.language == .c {
				util.no_dots(node.name)
			} else if node.name in ['array', 'string'] {
				node.name
			} else if node.scoped_name != '' {
				c_name(node.scoped_name)
			} else {
				c_name(node.name)
			}
			// TODO: For some reason, build fails with autofree with this line
			// as it's only informative, comment it for now
			// g.gen_attrs(node.attrs)
			// g.writeln('typedef struct {')
			// for field in it.fields {
			// field_type_sym := g.table.sym(field.typ)
			// g.writeln('\t$field_type_sym.name $field.name;')
			// }
			// g.writeln('} $name;')
			if node.language == .c {
				return
			}
			if g.pref.skip_unused && node.idx !in g.table.used_features.used_syms {
				return
			}
			if node.is_union {
				g.typedefs.writeln('typedef union ${name} ${name};')
			} else {
				g.typedefs.writeln('typedef struct ${name} ${name};')
			}
		}
		ast.GotoLabel {
			g.writeln('${c_name(node.name)}: {}')
		}
		ast.GotoStmt {
			g.write_v_source_line_info_stmt(node)
			g.writeln('goto ${c_name(node.name)};')
		}
		ast.AsmStmt {
			if g.is_cc_msvc {
				g.error('msvc does not support inline assembly', node.pos)
			}
			g.write_v_source_line_info_stmt(node)
			g.asm_stmt(node)
		}
		ast.HashStmt {
			g.hash_stmt(node)
		}
		ast.TypeDecl {
			if !g.pref.skip_unused {
				g.writeln('// TypeDecl')
			}
		}
		ast.SemicolonStmt {
			g.writeln(';')
		}
		ast.SqlStmt {
			g.sql_stmt(node)
		}
		ast.Module {
			g.is_builtin_mod = util.module_is_builtin(node.name)
			g.cur_mod = node
		}
		ast.EmptyStmt {}
		ast.DebuggerStmt {
			g.debugger_stmt(node)
		}
		ast.NodeError {}
	}
	if !g.skip_stmt_pos { // && g.stmt_path_pos.len > 0 {
		g.stmt_path_pos.delete_last()
	}
	// TODO: If we have temporary string exprs to free after this statement, do it. e.g.:
	// `foo('a' + 'b')` => `tmp := 'a' + 'b'; foo(tmp); string_free(&tmp);`
}

fn (mut g Gen) write_defer_stmts() {
	for i := g.defer_stmts.len - 1; i >= 0; i-- {
		defer_stmt := g.defer_stmts[i]
		if !g.pref.is_prod {
			g.writeln('// Defer begin')
		}
		g.writeln('if (${g.defer_flag_var(defer_stmt)}) {')

		//		g.indent++
		if defer_stmt.ifdef.len > 0 {
			g.writeln(defer_stmt.ifdef)
			g.stmts(defer_stmt.stmts)
			g.writeln2('', '#endif')
		} else {
			g.stmts(defer_stmt.stmts)
		}
		//		g.indent--
		g.writeln('}')
		if !g.pref.is_prod {
			g.writeln('// Defer end')
		}
	}
}

struct SumtypeCastingFn {
	fn_name string
	got     ast.Type
	exp     ast.Type
}

fn (mut g Gen) get_sumtype_casting_fn(got_ ast.Type, exp_ ast.Type) string {
	mut got, exp := got_.idx_type(), exp_.idx_type()
	i := int(got) | int(u32(exp) << 18) | int(u32(exp_.has_flag(.option)) << 17) | int(u32(got_.has_flag(.option)) << 16)
	exp_sym := g.table.sym(exp)
	mut got_sym := g.table.sym(got)
	cname := if exp == ast.int_type_idx {
		ast.int_type_name
	} else {
		g.get_sumtype_variant_name(exp_, exp_sym)
	}
	// fn_name := '${got_sym.cname}_to_sumtype_${exp_sym.cname}'
	sumtype_variant_name := g.get_sumtype_variant_name(got_, got_sym)
	fn_name := '${sumtype_variant_name}_to_sumtype_${cname}'
	if g.pref.experimental && fn_name.contains('v__ast__Struct_to_sumtype_v__ast__TypeInfo') {
		print_backtrace()
		eprintln('======\n\n')
		// exit(0)
	}
	if got == exp || g.sumtype_definitions[i] {
		return fn_name
	}
	for got_sym.parent_idx != 0 && got_sym.idx !in (exp_sym.info as ast.SumType).variants {
		got_sym = g.table.sym(ast.idx_to_type(got_sym.parent_idx))
	}
	g.sumtype_definitions[i] = true
	g.sumtype_casting_fns << SumtypeCastingFn{
		fn_name: fn_name
		got:     if got_.has_flag(.option) {
			new_got := ast.idx_to_type(got_sym.idx).set_flag(.option)
			new_got
		} else {
			got_sym.idx
		}
		exp:     if exp_.has_flag(.option) {
			new_exp := exp.set_flag(.option)
			new_exp
		} else {
			exp
		}
	}
	return fn_name
}

fn (mut g Gen) write_sumtype_casting_fn(fun SumtypeCastingFn) {
	got, exp := fun.got, fun.exp
	got_sym, exp_sym := g.table.sym(got), g.table.sym(exp)
	mut got_cname, exp_cname := g.get_sumtype_variant_type_name(got, got_sym), exp_sym.cname
	mut type_idx := g.type_sidx(got)
	mut sb := strings.new_builder(128)
	mut variant_name := g.get_sumtype_variant_name(got, got_sym)
	if got_sym.info is ast.FnType {
		got_name := 'fn ${g.table.fn_type_source_signature(got_sym.info.func)}'
		got_cname = 'anon_fn_${g.table.fn_type_signature(got_sym.info.func)}'
		type_idx = g.table.type_idxs[got_name].str()
		exp_info := exp_sym.info as ast.SumType
		for variant in exp_info.variants {
			variant_sym := g.table.sym(variant)
			if variant_sym.info is ast.FnType {
				if g.table.fn_type_source_signature(variant_sym.info.func) == g.table.fn_type_source_signature(got_sym.info.func) {
					variant_name = g.get_sumtype_variant_name(variant, variant_sym)
					got_cname = g.get_sumtype_variant_type_name(variant, variant_sym)
					type_idx = int(variant).str()
					break
				}
			}
		}
		sb.writeln('static inline ${exp_cname} ${fun.fn_name}(${got_cname} x) {')
		sb.writeln('\t${got_cname} ptr = x;')
	} else {
		// g.definitions.writeln('${g.static_modifier} inline ${exp_cname} ${fun.fn_name}(${got_cname}* x);')
		// sb.writeln('${g.static_modifier} inline ${exp_cname} ${fun.fn_name}(${got_cname}* x) {')
		g.definitions.writeln('${exp_cname} ${fun.fn_name}(${got_cname}* x);')
		sb.writeln('${exp_cname} ${fun.fn_name}(${got_cname}* x) {')
		sb.writeln('\t${got_cname}* ptr = memdup(x, sizeof(${got_cname}));')
	}
	for embed_hierarchy in g.table.get_embeds(got_sym) {
		// last embed in the hierarchy
		mut embed_cname := ''
		mut embed_name := ''
		mut accessor := '&x->'
		for j, embed in embed_hierarchy {
			embed_sym := g.table.sym(embed)
			embed_cname = embed_sym.cname
			embed_name = embed_sym.embed_name()
			if j > 0 {
				accessor += '.'
			}
			accessor += embed_name
		}
		// if the variable is not used, the C compiler will optimize it away
		sb.writeln('\t${embed_cname}* ${embed_name}_ptr = memdup(${accessor}, sizeof(${embed_cname}));')
	}
	sb.write_string('\treturn (${exp_cname}){ ._${variant_name} = ptr, ._typ = ${type_idx}')
	for field in (exp_sym.info as ast.SumType).fields {
		mut ptr := 'ptr'
		mut type_cname := got_cname
		_, embed_types := g.table.find_field_from_embeds(got_sym, field.name) or {
			ast.StructField{}, []ast.Type{}
		}
		if embed_types.len > 0 {
			embed_sym := g.table.sym(embed_types.last())
			ptr = '${embed_sym.embed_name()}_ptr'
			type_cname = embed_sym.cname
		}
		field_styp := g.styp(field.typ)
		if got_sym.kind in [.sum_type, .interface] {
			// the field is already a wrapped pointer; we shouldn't wrap it once again
			sb.write_string(', .${c_name(field.name)} = ptr->${field.name}')
		} else {
			sb.write_string(', .${c_name(field.name)} = (${field_styp}*)((char*)${ptr} + __offsetof_ptr(${ptr}, ${type_cname}, ${c_name(field.name)}))')
		}
	}
	sb.writeln('};\n}')
	g.auto_fn_definitions << sb.str()
}

fn (mut g Gen) call_cfn_for_casting_expr(fname string, expr ast.Expr, exp ast.Type, got ast.Type, exp_styp string,
	got_is_ptr bool, got_is_fn bool, got_styp string) {
	mut rparen_n := 1
	mut mutable_idx := 0

	is_not_ptr_and_fn := !got_is_ptr && !got_is_fn
	is_sumtype_cast := !got_is_fn && fname.contains('_to_sumtype_')
	is_comptime_variant := is_not_ptr_and_fn && expr is ast.Ident
		&& g.comptime.is_comptime_variant_var(expr)
	if exp.is_ptr() {
		if $d('mutable_sumtype', false) && is_sumtype_cast && g.expected_arg_mut
			&& expr is ast.Ident {
			g.write('&(${exp_styp.trim_right('*')}){._${got_styp.trim_right('*')}=')
			rparen_n = 0
			mutable_idx = got.idx()
		} else if (expr is ast.UnsafeExpr && expr.expr is ast.Nil) || got == ast.nil_type {
			g.write('(void*)0')
			return
		} else {
			g.write('HEAP(${exp_styp}, ${fname}(')
			rparen_n++
		}
	} else {
		g.write('${fname}(')
	}
	if is_not_ptr_and_fn {
		is_cast_fixed_array_init := expr is ast.CastExpr
			&& (expr.expr is ast.ArrayInit && expr.expr.is_fixed)

		if !is_cast_fixed_array_init && (is_comptime_variant || !expr.is_lvalue()
			|| (expr is ast.Ident && (expr.obj.is_simple_define_const()
			|| (expr.obj is ast.Var && expr.obj.is_index_var)))) {
			// Note: the `_to_sumtype_` family of functions do call memdup internally, making
			// another duplicate with the HEAP macro is redundant, so use ADDR instead:
			if expr.is_as_cast() {
				if !got_is_ptr && expr is ast.SelectorExpr {
					// (var as Type).field_non_ptr
					g.write('&')
				}
				old_inside_smartcast := g.inside_smartcast
				g.inside_smartcast = true
				defer {
					g.inside_smartcast = old_inside_smartcast
				}
			} else {
				promotion_macro_name := if is_sumtype_cast { 'ADDR' } else { 'HEAP' }
				g.write('${promotion_macro_name}(${got_styp}, (')
				rparen_n += 2
			}
		} else {
			g.write('&')
		}
	}
	if got_styp == 'none' && !g.cur_fn.return_type.has_flag(.option) {
		g.write('(none){E_STRUCT}')
	} else if is_comptime_variant {
		ctyp := g.type_resolver.get_ct_type_or_default('${g.comptime.comptime_for_variant_var}.typ',
			ast.void_type)
		g.write(g.type_default(ctyp))
	} else {
		old_left_is_opt := g.left_is_opt
		g.left_is_opt = !exp.has_flag(.option)
		g.expr(expr)
		g.left_is_opt = old_left_is_opt
	}
	if mutable_idx != 0 {
		g.write(', ._typ=${mutable_idx}}')
	}
	g.write(')'.repeat(rparen_n))
}

// use instead of expr() when you need a var to use as reference
fn (mut g Gen) expr_with_var(expr ast.Expr, expected_type ast.Type, do_cast bool) string {
	stmt_str := g.go_before_last_stmt().trim_space()
	g.empty_line = true
	tmp_var := g.new_tmp_var()
	styp := g.styp(expected_type)

	g.writeln('${styp} ${tmp_var};')
	g.write('memcpy(&${tmp_var}, ')
	if do_cast {
		g.write('(${styp})')
	}
	g.expr(expr)
	g.writeln(', sizeof(${styp}));')
	g.write(stmt_str)
	return tmp_var
}

// expr_with_fixed_array generates code for fixed array initialization with expr which requires tmp var
fn (mut g Gen) expr_with_fixed_array(expr ast.Expr, got_type_raw ast.Type, expected_type ast.Type) string {
	stmt_str := g.go_before_last_stmt().trim_space()
	g.empty_line = true
	tmp_var := g.new_tmp_var()
	styp := g.styp(expected_type)
	if expr is ast.ArrayInit {
		g.writeln('${styp} ${tmp_var};')
		// [ foo(), foo() ]!
		val_typ := g.table.value_type(got_type_raw)
		val_styp := g.styp(val_typ)
		val_sym := g.table.final_sym(val_typ)
		prefix := if val_sym.kind !in [.array, .array_fixed] { '&' } else { '' }
		for i, item_expr in expr.exprs {
			g.write('memcpy(${prefix}${tmp_var}[${i}], ')
			needs_addr := (item_expr is ast.CallExpr && !item_expr.return_type.is_ptr()
				&& g.table.final_sym(item_expr.return_type).kind !in [.array, .array_fixed])
				|| (item_expr is ast.InfixExpr && !item_expr.promoted_type.is_ptr())
				|| item_expr is ast.StructInit
			if needs_addr {
				g.write('ADDR(${val_styp}, ')
			}
			g.expr(item_expr)
			if needs_addr {
				g.write(')')
			}
			g.writeln(', sizeof(${val_styp}));')
		}
	} else if expr is ast.CallExpr {
		// return var.call() where returns is option/result fixed array
		g.writeln('${styp} ${tmp_var} = {0};')
		g.write('memcpy(&${tmp_var}.data, ')
		g.expr(expr)
		g.writeln(', sizeof(${g.base_type(expected_type)}));')
	} else {
		g.expr(expr)
	}
	g.write(stmt_str)
	return tmp_var
}

// use instead of expr() when you need to cast to a different type
fn (mut g Gen) expr_with_cast(expr ast.Expr, got_type_raw ast.Type, expected_type ast.Type) {
	got_type := ast.mktyp(got_type_raw)
	exp_sym := g.table.sym(expected_type)
	got_sym := g.table.sym(got_type)
	expected_is_ptr := expected_type.is_ptr()
	got_is_ptr := got_type.is_ptr()
	// allow using the new Error struct as a string, to avoid a breaking change
	// TODO: temporary to allow people to migrate their code; remove soon
	if got_type == ast.error_type_idx && expected_type == ast.string_type_idx {
		g.write('(*(')
		g.expr(expr)
		g.write('.msg))')
		return
	}
	if got_sym.kind == .none && exp_sym.idx == ast.error_type_idx {
		g.expr(expr)
		return
	}
	if got_sym.info !is ast.Interface && exp_sym.info is ast.Interface
		&& got_type.idx() != expected_type.idx() && !expected_type.has_flag(.result) {
		if expr is ast.StructInit && !got_type.is_ptr() {
			g.inside_cast_in_heap++
			got_styp := g.cc_type(got_type.ref(), true)
			// TODO: why does cc_type even add this in the first place?
			exp_styp := exp_sym.cname
			mut fname := 'I_${got_styp}_to_Interface_${exp_styp}'
			if exp_sym.info.is_generic {
				fname = g.generic_fn_name(exp_sym.info.concrete_types, fname)
			}
			g.call_cfn_for_casting_expr(fname, expr, expected_type, got_type, exp_styp,
				true, false, got_styp)
			g.inside_cast_in_heap--
		} else {
			got_styp := g.cc_type(got_type, true)
			got_is_shared := got_type.has_flag(.shared_f)
			exp_styp := if got_is_shared { '__shared__${exp_sym.cname}' } else { exp_sym.cname }
			// If it's shared, we need to use the other caster:
			mut fname := if got_is_shared {
				'I___shared__${got_styp}_to_shared_Interface_${exp_styp}'
			} else {
				'I_${got_styp}_to_Interface_${exp_styp}'
			}
			lock g.referenced_fns {
				g.referenced_fns[fname] = true
			}
			if exp_sym.info.is_generic {
				fname = g.generic_fn_name(exp_sym.info.concrete_types, fname)
			}
			if g.pref.experimental {
				// Do not allocate for `Interface(unsafe{nil})` casts
				is_nil_cast := expr is ast.UnsafeExpr && expr.expr is ast.Nil
				if is_nil_cast {
					g.write('((void*)0)')
					return
				}
			}
			g.call_cfn_for_casting_expr(fname, expr, expected_type, got_type, exp_styp,
				got_is_ptr, false, got_styp)
		}
		return
	}
	// cast to sum type
	exp_styp := g.styp(expected_type)
	mut got_styp := g.styp(got_type)
	mut got_is_fn := false
	if got_sym.info is ast.FnType {
		if got_sym.info.is_anon {
			got_styp = 'anon_fn_${g.table.fn_type_signature(got_sym.info.func)}'
		}
		got_is_fn = true
	}
	if expected_type != ast.void_type {
		unwrapped_expected_type := g.unwrap_generic(expected_type)
		unwrapped_exp_sym := g.table.sym(unwrapped_expected_type)
		mut unwrapped_got_type := g.unwrap_generic(got_type)
		mut unwrapped_got_sym := g.table.sym(unwrapped_got_type)

		expected_deref_type := if expected_is_ptr {
			unwrapped_expected_type.deref()
		} else {
			unwrapped_expected_type
		}
		got_deref_type := if got_is_ptr { unwrapped_got_type.deref() } else { unwrapped_got_type }
		if g.table.sumtype_has_variant(expected_deref_type, got_deref_type, false) {
			mut is_already_sum_type := false
			scope := g.file.scope.innermost(expr.pos().pos)
			if expr is ast.Ident {
				if v := scope.find_var(expr.name) {
					if v.smartcasts.len > 0 && unwrapped_expected_type == v.orig_type {
						is_already_sum_type = true
					}
				}
			} else if expr is ast.SelectorExpr {
				v := scope.find_struct_field(expr.expr.str(), expr.expr_type, expr.field_name)
				if v != unsafe { nil } {
					if v.smartcasts.len > 0 && unwrapped_expected_type == v.orig_type {
						is_already_sum_type = true
					}
				}
			}
			if is_already_sum_type && !g.inside_return {
				// Don't create a new sum type wrapper if there is already one
				g.prevent_sum_type_unwrapping_once = true
				g.expr(expr)
			} else {
				if mut unwrapped_got_sym.info is ast.Aggregate {
					unwrapped_got_type = unwrapped_got_sym.info.types[g.aggregate_type_idx]
					unwrapped_got_sym = g.table.sym(unwrapped_got_type)
				}

				fname := g.get_sumtype_casting_fn(unwrapped_got_type, unwrapped_expected_type)
				if expr is ast.ArrayInit && got_sym.kind == .array_fixed {
					stmt_str := g.go_before_last_stmt().trim_space()
					g.empty_line = true
					tmp_var := g.new_tmp_var()
					g.write('${got_styp} ${tmp_var} = ')
					g.expr(expr)
					g.writeln(';')
					g.write2(stmt_str, ' ')
					g.write('${fname}(&${tmp_var})')
					return
				} else {
					g.call_cfn_for_casting_expr(fname, expr, expected_type, got_type,
						unwrapped_exp_sym.cname, got_is_ptr, got_is_fn, got_styp)
				}
			}
			return
		}
	}
	// Generic dereferencing logic
	neither_void := ast.voidptr_type !in [got_type, expected_type]
		&& ast.nil_type !in [got_type, expected_type]
	if expected_type.has_flag(.shared_f) && !got_type_raw.has_flag(.shared_f)
		&& !expected_type.has_option_or_result() {
		shared_styp := exp_styp[0..exp_styp.len - 1] // `shared` implies ptr, so eat one `*`
		if got_type_raw.is_ptr() {
			g.error('cannot convert reference to `shared`', expr.pos())
		}
		if exp_sym.kind == .array {
			g.writeln('(${shared_styp}*)__dup_shared_array(&(${shared_styp}){.mtx = {0}, .val =')
		} else if exp_sym.kind == .map {
			g.writeln('(${shared_styp}*)__dup_shared_map(&(${shared_styp}){.mtx = {0}, .val =')
		} else {
			g.writeln('(${shared_styp}*)__dup${shared_styp}(&(${shared_styp}){.mtx = {0}, .val =')
		}
		old_is_shared := g.is_shared
		g.is_shared = false
		g.expr(expr)
		g.is_shared = old_is_shared
		g.writeln('}, sizeof(${shared_styp}))')
		return
	} else if got_type_raw.has_flag(.shared_f) && !expected_type.has_flag(.shared_f) {
		if expected_is_ptr {
			g.write('&')
		}
		g.expr(expr)
		if expr !is ast.IndexExpr {
			g.write('->val')
		}
		return
	}
	if got_is_ptr && !expected_is_ptr && neither_void && exp_sym.kind != .placeholder
		&& expr !is ast.InfixExpr {
		got_deref_type := got_type.deref()
		deref_sym := g.table.sym(got_deref_type)
		deref_will_match := expected_type in [got_type, got_deref_type, deref_sym.parent_idx]
		got_is_opt_or_res := got_type.has_option_or_result()
		if deref_will_match || got_is_opt_or_res || expr.is_auto_deref_var() {
			g.write('*')
		}
	}
	if expr is ast.IntegerLiteral {
		if expected_type in [ast.u64_type, ast.u32_type, ast.u16_type] && expr.val[0] != `-` {
			g.expr(expr)
			g.write('U')
			return
		}
	}
	if (exp_sym.kind == .function && !expected_type.has_option_or_result())
		|| (g.inside_struct_init && expected_type == ast.voidptr_type
		&& expected_type != got_type_raw && expr !is ast.StructInit) {
		g.write('(voidptr)')
	}
	// no cast
	g.expr(expr)
}

fn write_octal_escape(mut b strings.Builder, c u8) {
	b << 92 // \
	b << 48 + (c >> 6) // oct digit 2
	b << 48 + (c >> 3) & 7 // oct digit 1
	b << 48 + c & 7 // oct digit 0
}

fn cescape_nonascii(original string) string {
	mut b := strings.new_builder(original.len)
	for c in original {
		if c < 32 || c > 126 {
			// Encode with a 3 digit octal escape code, which has the
			// advantage to be limited/non dependent on what character
			// will follow next, unlike hex escapes:
			write_octal_escape(mut b, c)
			continue
		}
		b << c
	}
	res := b.str()
	return res
}

// cestring returns a V string, properly escaped for embedding in a C string literal.
fn cestring(s string) string {
	return s.replace('\\', '\\\\').replace('"', "'")
}

// ctoslit returns a '_S("$s")' call, where s is properly escaped.
fn ctoslit(s string) string {
	return '_S("' + cescape_nonascii(cestring(s)) + '")'
}

fn (mut g Gen) gen_attrs(attrs []ast.Attr) {
	if g.pref.skip_unused {
		return
	}
	if g.inside_c_extern {
		return
	}
	for attr in attrs {
		g.writeln('// Attr: [${attr.name}]')
	}
}

fn (mut g Gen) asm_stmt(stmt ast.AsmStmt) {
	g.write('__asm__')
	if stmt.is_volatile {
		g.write(' volatile')
	}
	if stmt.is_goto {
		g.write(' goto')
	}
	g.writeln(' (')
	g.indent++
	for template_tmp in stmt.templates {
		mut template := template_tmp
		g.write('"')
		if template.is_directive {
			g.write('.')
		}
		g.write(template.name)
		if template.is_label {
			g.write(':')
		} else {
			g.write(' ')
		}
		// swap destination and operands for att syntax, not for arm64
		if template.args.len != 0 && !template.is_directive && stmt.arch != .arm64
			&& stmt.arch != .s390x && stmt.arch != .ppc64le && stmt.arch != .loongarch64 {
			template.args.prepend(template.args.last())
			template.args.delete(template.args.len - 1)
		}

		for i, arg in template.args {
			if stmt.arch == .amd64 && (template.name == 'call' || template.name[0] == `j`)
				&& arg is ast.AsmRegister {
				g.write('*') // indirect branching
			}

			g.asm_arg(arg, stmt)
			if i + 1 < template.args.len {
				g.write(', ')
			}
		}

		if !template.is_label {
			g.write(';')
		}
		g.writeln('"')
	}

	if stmt.output.len != 0 || stmt.input.len != 0 || stmt.clobbered.len != 0 || stmt.is_goto {
		g.write(': ')
	}
	g.gen_asm_ios(stmt.output)
	if stmt.input.len != 0 || stmt.clobbered.len != 0 || stmt.is_goto {
		g.write(': ')
	}
	g.gen_asm_ios(stmt.input)
	if stmt.clobbered.len != 0 || stmt.is_goto {
		g.write(': ')
	}
	for i, clob in stmt.clobbered {
		g.write2('"', clob.reg.name)
		g.write('"')
		if i + 1 < stmt.clobbered.len {
			g.writeln(',')
		} else {
			g.writeln('')
		}
	}
	if stmt.is_goto {
		g.write(': ')
	}
	for i, label in stmt.global_labels {
		g.write(label)
		if i + 1 < stmt.clobbered.len {
			g.writeln(',')
		} else {
			g.writeln('')
		}
	}
	g.indent--
	g.writeln(');')
}

fn (mut g Gen) asm_arg(arg ast.AsmArg, stmt ast.AsmStmt) {
	match arg {
		ast.AsmAlias {
			name := arg.name
			if name in stmt.local_labels || name in stmt.global_labels
				|| name in g.file.global_labels || stmt.is_basic
				|| (name !in stmt.input.map(it.alias) && name !in stmt.output.map(it.alias)) {
				asm_formatted_name := if name in stmt.global_labels { '%l[${name}]' } else { name }
				g.write(asm_formatted_name)
			} else {
				g.write('%[${name}]')
			}
		}
		ast.CharLiteral {
			g.write("'${arg.val}'")
		}
		ast.IntegerLiteral {
			if stmt.arch == .arm64 {
				g.write('#${arg.val}')
			} else if stmt.arch == .s390x || stmt.arch == .ppc64le || stmt.arch == .loongarch64 {
				g.write('${arg.val}')
			} else {
				g.write('\$${arg.val}')
			}
		}
		ast.FloatLiteral {
			if g.pref.nofloat {
				g.write('\$${arg.val.int()}')
			} else {
				g.write('\$${arg.val}')
			}
		}
		ast.BoolLiteral {
			g.write('\$${arg.val.str()}')
		}
		ast.AsmRegister {
			if stmt.arch == .loongarch64 {
				g.write('$${arg.name}')
			} else {
				if !stmt.is_basic {
					g.write('%') // escape percent with percent in extended assembly
				}
				g.write('%${arg.name}')
			}
		}
		ast.AsmAddressing {
			if arg.segment != '' {
				g.write('%%${arg.segment}:')
			}
			base := arg.base
			index := arg.index
			displacement := arg.displacement
			scale := arg.scale
			match arg.mode {
				.base {
					g.write('(')
					g.asm_arg(base, stmt)
					g.write(')')
				}
				.displacement {
					g.asm_arg(displacement, stmt)
				}
				.base_plus_displacement {
					g.asm_arg(displacement, stmt)
					g.write('(')
					g.asm_arg(base, stmt)
					g.write(')')
				}
				.index_times_scale_plus_displacement {
					if displacement is ast.AsmDisp {
						g.asm_arg(displacement, stmt)
						g.write('(, ')
					} else if displacement is ast.AsmRegister {
						g.write('(')
						g.asm_arg(displacement, stmt)
						g.write(',')
					} else {
						panic('unexpected ${displacement.type_name()}')
					}
					g.asm_arg(index, stmt)
					g.write(',${scale})')
				}
				.base_plus_index_plus_displacement {
					g.asm_arg(displacement, stmt)
					g.write('(')
					g.asm_arg(base, stmt)
					g.write(',')
					g.asm_arg(index, stmt)
					g.write(',1)')
				}
				.base_plus_index_times_scale_plus_displacement {
					g.asm_arg(displacement, stmt)
					g.write('(')
					g.asm_arg(base, stmt)
					g.write(',')
					g.asm_arg(index, stmt)
					g.write(',${scale})')
				}
				.rip_plus_displacement {
					g.asm_arg(displacement, stmt)
					g.write('(')
					g.asm_arg(base, stmt)
					g.write(')')
				}
				.invalid {
					g.error('invalid addressing mode', arg.pos)
				}
			}
		}
		ast.AsmDisp {
			g.write(arg.val)
		}
		string {
			g.write(arg)
		}
	}
}

fn (mut g Gen) gen_asm_ios(ios []ast.AsmIO) {
	for i, io in ios {
		if io.alias != '' {
			g.write('[${io.alias}] ')
		}
		g.write('"${io.constraint}" (')
		g.expr(io.expr)
		g.write(')')
		if i + 1 < ios.len {
			g.writeln(',')
		} else {
			g.writeln('')
		}
	}
}

fn cnewlines(s string) string {
	return s.replace('\n', r'\n')
}

fn (mut g Gen) write_fn_ptr_decl(func &ast.FnType, ptr_name string) {
	ret_styp := g.styp(func.func.return_type)
	g.write('${ret_styp} (*${ptr_name}) (')
	arg_len := func.func.params.len
	for i, arg in func.func.params {
		arg_styp := g.styp(arg.typ)
		g.write(arg_styp)
		g.write(' ${arg.name}')
		if i < arg_len - 1 {
			g.write(', ')
		}
	}
	g.write(')')
}

fn (mut g Gen) register_ternary_name(name string) {
	level_key := g.inside_ternary.str()
	if level_key !in g.ternary_level_names {
		g.ternary_level_names[level_key] = []string{}
	}
	new_name := g.new_tmp_var()
	g.ternary_names[name] = new_name
	g.ternary_level_names[level_key] << name
}

fn (mut g Gen) get_ternary_name(name string) string {
	if g.inside_ternary == 0 {
		return name
	}
	if name !in g.ternary_names {
		return name
	}
	return g.ternary_names[name]
}

fn (mut g Gen) gen_clone_assignment(var_type ast.Type, val ast.Expr, typ ast.Type, add_eq bool) bool {
	if val !in [ast.Ident, ast.SelectorExpr] {
		return false
	}
	right_sym := g.table.sym(typ)
	if g.is_autofree {
		if add_eq {
			g.write('=')
		}
		if right_sym.kind == .array {
			// `arr1 = arr2` => `arr1 = arr2.clone()`
			shared_styp := g.styp(typ.set_nr_muls(0))
			if typ.share() == .shared_t {
				g.write('(${shared_styp}*)__dup_shared_array(&(${shared_styp}){.mtx = {0}, .val =')
			}
			is_sumtype := g.table.type_kind(var_type) == .sum_type
			if is_sumtype {
				variant_typ := g.styp(typ).replace('*', '')
				fn_name := g.get_sumtype_casting_fn(typ, var_type)
				g.write('${fn_name}(ADDR(${variant_typ}, array_clone_static_to_depth(')
				if typ.is_ptr() {
					g.write('*')
				}
			} else {
				g.write(' array_clone_static_to_depth(')
			}
			g.expr(val)
			if typ.share() == .shared_t {
				g.write('->val')
			}
			elem_type := (right_sym.info as ast.Array).elem_type
			array_depth := g.get_array_depth(elem_type)
			g.write(', ${array_depth})')
			if typ.share() == .shared_t {
				g.write('}, sizeof(${shared_styp}))')
			}
			if is_sumtype {
				g.write('))')
			}
		} else if right_sym.kind == .string {
			// `str1 = str2` => `str1 = str2.clone()`
			if var_type.has_flag(.option) {
				g.write(' string_option_clone_static(')
			} else {
				g.write(' string_clone_static(')
			}
			g.expr(val)
			g.write(')')
		}
	}
	return true
}

fn (mut g Gen) map_fn_ptrs(key_sym ast.TypeSymbol) (string, string, string, string) {
	mut hash_fn := ''
	mut key_eq_fn := ''
	mut clone_fn := ''
	mut free_fn := '&map_free_nop'
	match key_sym.kind {
		.alias {
			alias_key_type := (key_sym.info as ast.Alias).parent_type
			return g.map_fn_ptrs(g.table.sym(alias_key_type))
		}
		.u8, .i8, .char {
			hash_fn = '&map_hash_int_1'
			key_eq_fn = '&map_eq_int_1'
			clone_fn = '&map_clone_int_1'
		}
		.i16, .u16 {
			hash_fn = '&map_hash_int_2'
			key_eq_fn = '&map_eq_int_2'
			clone_fn = '&map_clone_int_2'
		}
		.enum {
			einfo := (key_sym.info) as ast.Enum
			if g.pref.ccompiler_type == .tinyc
				&& einfo.typ in [ast.u8_type, ast.u16_type, ast.i8_type, ast.i16_type] {
				// workaround for tcc, since we can not generate a packed Enum with size < 4 bytes
				return g.map_fn_ptrs(g.table.sym(ast.int_type))
			}
			return g.map_fn_ptrs(g.table.sym(einfo.typ))
		}
		.int, .i32, .u32, .rune, .f32 {
			hash_fn = '&map_hash_int_4'
			key_eq_fn = '&map_eq_int_4'
			clone_fn = '&map_clone_int_4'
		}
		.voidptr {
			ts := if g.pref.m64 {
				unsafe { g.table.sym_by_idx(ast.u64_type_idx) }
			} else {
				unsafe { g.table.sym_by_idx(ast.u32_type_idx) }
			}
			return g.map_fn_ptrs(ts)
		}
		.u64, .i64, .f64 {
			hash_fn = '&map_hash_int_8'
			key_eq_fn = '&map_eq_int_8'
			clone_fn = '&map_clone_int_8'
		}
		.string {
			hash_fn = '&map_hash_string'
			key_eq_fn = '&map_eq_string'
			clone_fn = '&map_clone_string'
			free_fn = '&map_free_string'
		}
		else {
			verror('map key type `${key_sym.name}` not supported')
		}
	}
	return hash_fn, key_eq_fn, clone_fn, free_fn
}

fn (mut g Gen) expr(node_ ast.Expr) {
	old_discard_or_result := g.discard_or_result
	old_is_void_expr_stmt := g.is_void_expr_stmt
	if g.is_void_expr_stmt {
		g.discard_or_result = true
		g.is_void_expr_stmt = false
	} else {
		g.discard_or_result = false
	}
	// Note: please keep the type names in the match here in alphabetical order:
	mut node := unsafe { node_ }
	match mut node {
		ast.ComptimeType {
			g.error('g.expr(): Unhandled ComptimeType', node.pos)
		}
		ast.EmptyExpr {
			g.error('g.expr(): unhandled EmptyExpr', token.Pos{})
		}
		ast.AnonFn {
			g.gen_anon_fn(mut node)
		}
		ast.ArrayDecompose {
			g.expr(node.expr)
		}
		ast.ArrayInit {
			g.array_init(node, '')
		}
		ast.AsCast {
			g.as_cast(node)
		}
		ast.Assoc {
			g.assoc(node)
		}
		ast.AtExpr {
			g.comptime_at(node)
		}
		ast.BoolLiteral {
			g.write(node.val.str())
		}
		ast.CallExpr {
			ret_type := if node.or_block.kind == .absent {
				node.return_type
			} else {
				node.return_type.clear_option_and_result()
			}
			mut shared_styp := ''
			ret_typ_is_shared := ret_type.has_flag(.shared_f)
			if g.is_shared && !ret_typ_is_shared && !g.inside_or_block {
				ret_sym := g.table.sym(ret_type)
				shared_typ := if ret_type.is_ptr() {
					ret_type.deref().set_flag(.shared_f)
				} else {
					ret_type.set_flag(.shared_f)
				}
				shared_styp = g.styp(shared_typ)
				if ret_sym.kind == .array {
					g.writeln('(${shared_styp}*)__dup_shared_array(&(${shared_styp}){.mtx = {0}, .val =')
				} else if ret_sym.kind == .map {
					g.writeln('(${shared_styp}*)__dup_shared_map(&(${shared_styp}){.mtx = {0}, .val =')
				} else {
					g.writeln('(${shared_styp}*)__dup${shared_styp}(&(${shared_styp}){.mtx = {0}, .val =')
				}
			}
			stmt_before_call_expr_pos := if g.stmt_path_pos.len > 0 {
				g.stmt_path_pos.last()
			} else {
				0
			}

			if g.is_shared && !ret_typ_is_shared && !g.inside_or_block && ret_type.is_ptr() {
				g.write('*'.repeat(ret_type.nr_muls()))
			}
			g.call_expr(node)
			if g.is_autofree && !g.is_builtin_mod && !g.is_js_call && g.strs_to_free0.len == 0
				&& !g.inside_lambda {
				// if len != 0, that means we are handling call expr inside call expr (arg)
				// and it'll get messed up here, since it's handled recursively in autofree_call_pregen()
				// so just skip it
				g.autofree_call_pregen(node)
				if g.strs_to_free0.len > 0 {
					g.insert_at(stmt_before_call_expr_pos, g.strs_to_free0.join('\n') +
						'/* inserted before */')
				}
				g.strs_to_free0 = []
			}
			if g.is_shared && !ret_typ_is_shared && !g.inside_or_block {
				g.writeln('}, sizeof(${shared_styp}))')
			}
			/*
			if g.autofree {
				s := g.autofree_pregen[node.pos.pos.str()]
				if s != '' {
					// g.insert_before_stmt('/*START2*/' + g.strs_to_free0.join('\n') + '/*END*/')
					// g.insert_before_stmt('/*START3*/' + node.autofree_pregen + '/*END*/')
					g.insert_before_stmt('/*START3*/' + s + '/*END*/')
					// for s in g.strs_to_free0 {
				}
				// //g.writeln(s)
				// }
				g.strs_to_free0 = []
			}
			*/
		}
		ast.CastExpr {
			g.cast_expr(node)
		}
		ast.ChanInit {
			elem_typ_str := g.styp(node.elem_type)
			noscan := g.check_noscan(node.elem_type)
			g.write('sync__new_channel_st${noscan}(')
			if node.has_cap {
				g.expr(node.cap_expr)
			} else {
				g.write('0')
			}
			g.write2(', sizeof(', elem_typ_str)
			g.write2(')>0 ? sizeof(', elem_typ_str)
			g.write(') : 1)')
		}
		ast.CharLiteral {
			g.char_literal(node)
		}
		ast.Comment {}
		ast.ComptimeCall {
			g.comptime_call(mut node)
		}
		ast.ComptimeSelector {
			g.comptime_selector(node)
		}
		ast.ConcatExpr {
			g.concat_expr(node)
		}
		ast.CTempVar {
			g.write(node.name)
			if node.is_fixed_ret {
				g.write('.ret_arr')
			}
		}
		ast.DumpExpr {
			g.dump_expr(node)
		}
		ast.EnumVal {
			g.enum_val(node)
		}
		ast.FloatLiteral {
			if g.pref.nofloat {
				g.write(node.val.int().str())
			} else {
				g.write(node.val)
			}
		}
		ast.SpawnExpr {
			old_is_arraymap_set := g.is_arraymap_set
			g.is_arraymap_set = false
			g.spawn_and_go_expr(node, .spawn_)
			g.is_arraymap_set = old_is_arraymap_set
		}
		ast.GoExpr {
			// XTODO this results in a cgen bug, order of fields is broken
			// g.spawn_and_go_expr(ast.SpawnExpr{node.pos, node.call_expr, node.is_expr},
			old_is_arraymap_set := g.is_arraymap_set
			g.is_arraymap_set = false
			g.spawn_and_go_expr(ast.SpawnExpr{
				pos:       node.pos
				call_expr: node.call_expr
				is_expr:   node.is_expr
			}, .go_)
			g.is_arraymap_set = old_is_arraymap_set
		}
		ast.Ident {
			g.ident(node)
		}
		ast.IfExpr {
			if !node.is_comptime {
				g.if_expr(node)
			} else {
				g.comptime_if(node)
			}
		}
		ast.IfGuardExpr {
			g.write('/* guard */')
		}
		ast.IndexExpr {
			g.index_expr(node)
		}
		ast.InfixExpr {
			if node.op !in [.left_shift, .plus_assign, .minus_assign] {
				g.infix_expr(node)
			} else {
				g.inside_map_infix = true
				g.infix_expr(node)
				g.inside_map_infix = false
			}
		}
		ast.IntegerLiteral {
			if node.val.starts_with('0o') {
				g.write2('0', node.val[2..])
			} else if node.val.starts_with('-0o') {
				g.write2('-0', node.val[3..])
			} else {
				g.write(node.val)
			}
		}
		ast.IsRefType {
			typ := g.type_resolver.typeof_type(node.expr, g.get_type(node.typ))
			node_typ := g.unwrap_generic(typ)
			sym := g.table.sym(node_typ)
			if sym.language == .v && sym.kind in [.placeholder, .any] {
				g.error('unknown type `${sym.name}`', node.pos)
			}
			is_ref_type := g.contains_ptr(node_typ)
			g.write('/*IsRefType*/ ${is_ref_type}')
		}
		ast.LambdaExpr {
			if node.call_ctx != unsafe { nil } {
				save_cur_concrete_types := g.cur_concrete_types
				g.cur_concrete_types = node.call_ctx.concrete_types
				g.gen_anon_fn(mut node.func)
				g.cur_concrete_types = save_cur_concrete_types
			} else {
				g.gen_anon_fn(mut node.func)
			}
		}
		ast.Likely {
			if node.is_likely {
				g.write('_likely_')
			} else {
				g.write('_unlikely_')
			}
			g.write('(')
			g.expr(node.expr)
			g.write(')')
		}
		ast.LockExpr {
			g.lock_expr(node)
		}
		ast.MapInit {
			g.map_init(node)
		}
		ast.MatchExpr {
			g.match_expr(node)
		}
		ast.NodeError {}
		ast.Nil {
			g.write('((void*)0)')
		}
		ast.None {
			g.write('_const_none__')
		}
		ast.OffsetOf {
			styp := g.styp(node.struct_type)
			g.write('/*OffsetOf*/ (u32)(__offsetof(${util.no_dots(styp)}, ${node.field}))')
		}
		ast.OrExpr {
			// this should never appear here
		}
		ast.ParExpr {
			g.write('(')
			g.expr(node.expr)
			g.write(')')
		}
		ast.PostfixExpr {
			if node.auto_locked != '' {
				g.writeln('sync__RwMutex_lock(&${node.auto_locked}->mtx);')
			}
			g.inside_map_postfix = true
			if node.is_c2v_prefix {
				g.write(node.op.str())
			}
			if node.expr.is_auto_deref_var() {
				g.write('(*')
				g.expr(node.expr)
				g.write(')')
			} else if node.op == .question {
				cur_line := g.go_before_last_stmt().trim_space()
				mut expr_str := ''
				mut is_unwrapped := true
				mut dot_or_ptr := '.'
				if mut node.expr is ast.ComptimeSelector && node.expr.left is ast.Ident {
					// val.$(field.name)?
					expr_str = g.gen_comptime_selector(node.expr)
				} else if mut node.expr is ast.Ident && node.expr.ct_expr {
					// val?
					expr_str = node.expr.name
					is_unwrapped = !g.inside_assign
					dot_or_ptr = if !(node.expr.obj is ast.Var && node.expr.obj.is_auto_deref) {
						'.'
					} else {
						'->'
					}
				}
				g.writeln('if (${expr_str}${dot_or_ptr}state != 0) {')
				g.writeln2('\tpanic_option_not_set(_S("none"));', '}')
				g.write(cur_line)
				if is_unwrapped {
					typ := g.type_resolver.typeof_type(node.expr, node.typ)
					g.write('*(${g.base_type(typ)}*)&')
					g.expr(node.expr)
					g.write('.data')
				} else {
					g.expr(node.expr)
				}
			} else {
				g.expr(node.expr)
			}
			g.inside_map_postfix = false
			if !node.is_c2v_prefix && node.op != .question {
				g.write(node.op.str())
			}
			if node.auto_locked != '' {
				g.writeln(';')
				g.write('sync__RwMutex_unlock(&${node.auto_locked}->mtx)')
			}
		}
		ast.PrefixExpr {
			gen_or := node.op == .arrow && (node.or_block.kind != .absent || node.is_option)
			if node.op == .amp {
				g.is_amp = true
			}
			if node.op == .arrow {
				styp := g.styp(node.right_type)
				right_sym := g.table.sym(node.right_type)
				mut right_inf := right_sym.info as ast.Chan
				elem_type := right_inf.elem_type
				is_gen_or_and_assign_rhs := gen_or && !g.discard_or_result
				cur_line := if is_gen_or_and_assign_rhs {
					line := g.go_before_last_stmt()
					g.out.write_string(util.tabs(g.indent))
					line
				} else {
					''
				}
				tmp_opt := if gen_or { g.new_tmp_var() } else { '' }
				if gen_or {
					opt_elem_type := g.styp(elem_type.set_flag(.option))
					g.register_chan_pop_option_call(opt_elem_type, styp)
					g.write('${opt_elem_type} ${tmp_opt} = __Option_${styp}_popval(')
				} else {
					g.write('__${styp}_popval(')
				}
				g.expr(node.right)
				g.write(')')
				if gen_or {
					if !node.is_option {
						g.or_block(tmp_opt, node.or_block, elem_type)
					}
					if is_gen_or_and_assign_rhs {
						elem_styp := g.styp(elem_type)
						g.write(';\n${cur_line}*(${elem_styp}*)${tmp_opt}.data')
					}
				}
			} else {
				if g.is_option_auto_heap {
					g.write('(${g.base_type(node.right_type)}*)')
				}
				mut has_slice_call := false
				if !g.is_option_auto_heap && !(g.is_amp && node.right.is_auto_deref_var()) {
					has_slice_call = node.op == .amp && node.right is ast.IndexExpr
						&& node.right.index is ast.RangeExpr
					if has_slice_call {
						g.write('ADDR(${g.styp(node.right_type)}, ')
					} else {
						g.write(node.op.str())
					}
				}
				g.expr(node.right)
				if has_slice_call {
					g.write(')')
				}
				if g.is_option_auto_heap {
					g.write('.data')
				}
			}
			g.is_amp = false
		}
		ast.RangeExpr {
			// Only used in IndexExpr and InfixExpr
		}
		ast.SelectExpr {
			g.select_expr(node)
		}
		ast.SelectorExpr {
			g.selector_expr(node)
		}
		ast.SizeOf {
			g.size_of(node)
		}
		ast.SqlExpr {
			if node.is_insert {
				g.sql_insert_expr(node)
			} else {
				g.sql_select_expr(node)
			}
		}
		ast.StringLiteral {
			g.string_literal(node)
		}
		ast.StringInterLiteral {
			g.string_inter_literal(node)
		}
		ast.StructInit {
			if node.unresolved {
				g.expr(g.table.resolve_init(node, g.unwrap_generic(node.typ)))
			} else {
				// `user := User{name: 'Bob'}`
				g.inside_struct_init = true
				g.cur_struct_init_typ = node.typ
				g.struct_init(node)
				g.cur_struct_init_typ = 0
				g.inside_struct_init = false
			}
		}
		ast.TypeNode {
			// match sum Type
			typ := g.unwrap_generic(node.typ)
			sym := g.table.sym(typ)
			sidx := g.type_sidx(typ)
			g.write('${sidx}')
			if !g.pref.is_prod {
				g.write(' /* ${sym.name} */')
			}
		}
		ast.TypeOf {
			g.typeof_expr(node)
		}
		ast.UnsafeExpr {
			g.expr(node.expr)
		}
	}
	g.discard_or_result = old_discard_or_result
	g.is_void_expr_stmt = old_is_void_expr_stmt
}

fn (mut g Gen) char_literal(node ast.CharLiteral) {
	if node.val == r'\`' {
		g.write("'`'")
		return
	}
	// TODO: optimize use L-char instead of u32 when possible
	if !node.val.is_pure_ascii() {
		g.write('((rune)0x${node.val.utf32_code().hex()} /* `${node.val}` */)')
		return
	}
	if node.val.len == 1 {
		clit := node.val[0]
		if clit < 32 || clit == 92 || clit > 126 {
			g.write("'")
			write_octal_escape(mut g.out, clit)
			g.write("'")
			return
		}
	}
	g.write("'${node.val}'")
}

// T.name, typeof(expr).name
fn (mut g Gen) type_name(raw_type ast.Type) {
	typ := g.get_type(raw_type)
	sym := g.table.sym(typ)
	mut s := ''
	if sym.kind == .function {
		if typ.is_ptr() {
			s = '&' + g.fn_decl_str(sym.info as ast.FnType)
		} else {
			s = g.fn_decl_str(sym.info as ast.FnType)
		}
	} else {
		s = g.table.type_to_str(g.unwrap_generic(typ))
	}
	g.write('_S("${util.strip_main_name(s)}")')
}

fn (mut g Gen) typeof_expr(node ast.TypeOf) {
	typ := g.type_resolver.typeof_type(node.expr, g.get_type(node.typ))
	sym := g.table.sym(typ)
	if sym.kind == .sum_type {
		// When encountering a .sum_type, typeof() should be done at runtime,
		// because the subtype of the expression may change:
		g.write('charptr_vstring_literal(v_typeof_sumtype_${sym.cname}( (')
		if typ.nr_muls() > 0 {
			g.write('*'.repeat(typ.nr_muls()))
		}
		g.expr(node.expr)
		g.write(')._typ ))')
	} else if sym.kind == .array_fixed {
		fixed_info := sym.info as ast.ArrayFixed
		typ_name := g.table.get_type_name(fixed_info.elem_type)
		g.write('_S("[${fixed_info.size}]${util.strip_main_name(typ_name)}")')
	} else if sym.kind == .function {
		info := sym.info as ast.FnType
		g.write('_S("${g.fn_decl_str(info)}")')
	} else if typ.has_flag(.variadic) {
		varg_elem_type_sym := g.table.sym(g.table.value_type(typ))
		g.write('_S("...${util.strip_main_name(varg_elem_type_sym.name)}")')
	} else {
		g.type_name(typ)
	}
}

fn (mut g Gen) selector_expr(node ast.SelectorExpr) {
	prevent_sum_type_unwrapping_once := g.prevent_sum_type_unwrapping_once
	g.prevent_sum_type_unwrapping_once = false
	if node.name_type > 0 {
		match node.gkind_field {
			.name {
				g.type_name(node.name_type)
				return
			}
			.typ {
				g.write(int(g.unwrap_generic(node.name_type)).str())
				return
			}
			.unaliased_typ {
				g.write(int(g.table.unaliased_type(g.unwrap_generic(node.name_type))).str())
				return
			}
			.indirections {
				g.write(int(g.unwrap_generic(node.name_type).nr_muls()).str())
				return
			}
			.unknown {
				// ast.TypeOf of `typeof(string).idx` etc
				if node.field_name == 'name' {
					// typeof(expr).name
					mut name_type := node.name_type
					if node.expr is ast.TypeOf {
						name_type = g.type_resolver.typeof_type(node.expr.expr, name_type)
					}
					g.type_name(name_type)
					return
				} else if node.field_name in ['idx', 'unaliased_typ'] {
					// `T.idx`, `T.unaliased_typ`, `typeof(expr).idx`, `typeof(expr).unalised_typ`
					mut name_type := node.name_type
					if node.expr is ast.TypeOf {
						name_type = g.type_resolver.typeof_field_type(g.type_resolver.typeof_type(node.expr.expr,
							name_type), node.field_name)
						g.write(int(name_type).str())
					} else {
						g.write(int(g.unwrap_generic(name_type)).str())
					}
					return
				} else if node.field_name in ['key_type', 'value_type', 'element_type'] {
					// `T.<field_name>`, `typeof(expr).<field_name>`
					mut name_type := node.name_type
					name_type = g.type_resolver.typeof_field_type(g.type_resolver.typeof_type(node.expr,
						name_type), node.field_name)
					g.write(int(name_type).str())
					return
				} else if node.field_name == 'indirections' {
					mut name_type := node.name_type
					if node.expr is ast.TypeOf {
						name_type = g.type_resolver.typeof_type(node.expr.expr, name_type)
					}
					// `typeof(expr).indirections`
					g.write(int(g.unwrap_generic(name_type).nr_muls()).str())
					return
				}
				g.error('unknown generic field', node.pos)
			}
		}
	}
	if node.expr_type == 0 {
		g.checker_bug('unexpected SelectorExpr.expr_type = 0', node.pos)
	}

	unwrapped_expr_type := g.unwrap_generic(node.expr_type)
	sym := g.table.sym(unwrapped_expr_type)
	field_name := if sym.language == .v { c_name(node.field_name) } else { node.field_name }
	is_as_cast := node.expr is ast.AsCast
	if is_as_cast {
		g.write('(')
	}
	if node.or_block.kind != .absent && g.table.sym(node.typ).kind != .chan {
		is_ptr := sym.kind in [.interface, .sum_type]
		stmt_str := g.go_before_last_stmt().trim_space()
		styp := g.styp(g.unwrap_generic(node.typ))
		g.empty_line = true
		is_option_unwrap := node.typ.has_flag(.option)
		tmp_var := g.new_tmp_var()
		g.write('${styp} ')
		if is_option_unwrap {
			g.write('*')
		}
		g.write('${tmp_var} = ')
		mut needs_addr := false
		needs_deref := is_ptr && !is_option_unwrap
		if needs_deref {
			g.write('*(')
		} else if is_option_unwrap && !is_ptr {
			needs_addr = node.expr !in [ast.Ident, ast.PrefixExpr]
			if !needs_addr {
				g.write('&')
			} else {
				g.write('ADDR(${styp}, ')
			}
		}
		g.expr(node.expr)
		for i, embed in node.from_embed_types {
			embed_sym := g.table.sym(embed)
			embed_name := embed_sym.embed_name()
			is_left_ptr := if i == 0 {
				node.expr_type.is_ptr()
			} else {
				node.from_embed_types[i - 1].is_ptr()
			}
			if is_left_ptr {
				g.write('->')
			} else {
				g.write('.')
			}
			g.write(embed_name)
		}
		if node.expr_type.is_ptr() && node.from_embed_types.len == 0 {
			g.write('->')
		} else {
			g.write('.')
		}
		g.write(field_name)
		if needs_deref {
			g.write(')')
		}
		if needs_addr {
			g.write(')')
		}
		if is_option_unwrap {
			g.tmp_var_ptr[tmp_var] = true
		}
		g.or_block(tmp_var, node.or_block, node.typ)
		if is_option_unwrap {
			g.tmp_var_ptr.delete(tmp_var)
		}
		g.write2(stmt_str, ' ')
		unwrapped_typ := node.typ.clear_option_and_result()
		unwrapped_styp := g.styp(unwrapped_typ)
		g.write('(*(${unwrapped_styp}*)${tmp_var}->data)')
		return
	}

	// if node expr is a root ident and an optional
	mut is_opt_or_res := node.expr is ast.Ident && node.expr_type.has_option_or_result()
	if is_opt_or_res {
		opt_base_typ := g.base_type(node.expr_type)
		g.write('(*(${opt_base_typ}*)')
	}
	final_sym := g.table.final_sym(unwrapped_expr_type)
	if final_sym.kind == .array_fixed {
		if node.field_name != 'len' {
			g.error('field_name should be `len`', node.pos)
		}
		info := final_sym.info as ast.ArrayFixed
		g.write('${info.size}')
		return
	} else if sym.kind == .chan && (node.field_name == 'len' || node.field_name == 'closed') {
		g.write('sync__Channel_${node.field_name}(')
		g.expr(node.expr)
		g.write(')')
		return
	} else if g.enum_data_type == node.typ {
		g.expr(node.expr)
		g.write2('.', node.field_name)
		return
	}
	mut sum_type_deref_field := ''
	mut sum_type_dot := '.'
	mut field_typ := ast.void_type
	mut is_option_unwrap := false
	is_iface_or_sumtype := sym.kind in [.interface, .sum_type]
	mut deref_count := 0
	if f := g.table.find_field_with_embeds(sym, node.field_name) {
		field_sym := g.table.sym(f.typ)
		field_typ = f.typ
		if is_iface_or_sumtype {
			g.write('(*(')
			deref_count++
		}
		is_option := field_typ.has_flag(.option)
		if field_sym.kind in [.sum_type, .interface] || is_option {
			if !prevent_sum_type_unwrapping_once {
				// check first if field is sum type because scope searching is expensive
				scope := g.file.scope.innermost(node.pos.pos)
				field := scope.find_struct_field(node.expr.str(), node.expr_type, node.field_name)
				if field != unsafe { nil } {
					nested_unwrap := is_option && field.smartcasts.len > 1
					is_option_unwrap = is_option && field.smartcasts.len > 0
						&& field.typ.clear_flag(.option) == field.smartcasts.last()
					if field.orig_type.is_ptr() {
						sum_type_dot = '->'
					}
					if nested_unwrap && field_sym.kind == .sum_type {
						g.write('*(')
						deref_count++
					}
					for i, typ in field.smartcasts {
						if i == 0 && (is_option_unwrap || nested_unwrap) {
							deref := if g.inside_selector {
								if is_iface_or_sumtype || (field.orig_type.is_ptr() && g.left_is_opt
									&& is_option_unwrap) {
									'*'.repeat(typ.nr_muls())
								} else {
									'*'.repeat(typ.nr_muls() + 1)
								}
							} else if sym.kind == .interface && !typ.is_ptr()
								&& field.orig_type.has_flag(.option) {
								''
							} else {
								'*'
							}
							deref_count += deref.len
							g.inside_selector_deref = deref_count > typ.nr_muls()
							g.write('(${deref}(${g.styp(typ)}*)')
						}
						if i == 0 || !nested_unwrap {
							g.write('(')
						}
						if field_sym.kind == .sum_type && !is_option {
							g.write('*')
						}
						cast_sym := g.table.sym(g.unwrap_generic(typ))
						if field_sym.kind == .interface && cast_sym.kind == .interface
							&& !is_option_unwrap {
							ptr := '*'.repeat(field.typ.nr_muls())
							dot := if node.expr_type.is_ptr() { '->' } else { '.' }
							g.write('I_${field_sym.cname}_as_I_${cast_sym.cname}(${ptr}${node.expr}${dot}${node.field_name}))')
							return
						} else if !is_option_unwrap {
							if i != 0 {
								dot := if field.typ.is_ptr() { '->' } else { '.' }
								sum_type_deref_field += ')${dot}'
							}
							if cast_sym.info is ast.Aggregate {
								agg_sym := g.table.sym(cast_sym.info.types[g.aggregate_type_idx])
								sum_type_deref_field += '_${agg_sym.cname}'
							} else {
								if i == 0 && nested_unwrap {
									sum_type_deref_field += 'data)'
								} else {
									sum_type_deref_field += '_${cast_sym.cname}'
								}
							}
						}
					}
				}
			}
		}
	} else if m := sym.find_method_with_generic_parent(node.field_name) {
		mut has_embeds := false
		if sym.info in [ast.Struct, ast.Aggregate] {
			if node.from_embed_types.len > 0 {
				has_embeds = true
			}
		}
		if !has_embeds {
			if !node.has_hidden_receiver {
				if node.expr is ast.Ident && sym.info is ast.Interface {
					left_cc_type := g.cc_type(g.table.unaliased_type(node.expr_type),
						false)
					left_type_name := util.no_dots(left_cc_type)
					g.write('${c_name(left_type_name)}_name_table[${node.expr.name}${g.dot_or_ptr(node.expr_type)}_typ]._method_${m.name}')
				} else {
					g.write('${g.styp(node.expr_type.idx_type())}_${m.name}')
				}
				return
			}
			receiver := m.params[0]
			expr_styp := g.styp(g.unwrap_generic(node.expr_type).idx_type())
			name := '_V_closure_${expr_styp}_${m.name}_${node.pos.pos}'
			lock g.anon_fns {
				if name !in g.anon_fns {
					g.anon_fns << name
					g.gen_closure_fn(expr_styp, m, name)
				}
			}
			g.write('__closure_create(${name}, ')
			if !receiver.typ.is_ptr() {
				g.write('memdup_uncollectable(')
			}
			mut has_addr := false
			if !node.expr_type.is_ptr() {
				if node.expr is ast.IndexExpr {
					has_addr = true
					g.write('ADDR(${g.styp(node.expr_type)}, ')
				} else {
					g.write('&')
				}
			}
			if !node.expr.is_lvalue() {
				current_stmt := g.go_before_last_stmt()
				g.empty_line = true
				var := g.new_ctemp_var_then_gen(node.expr, node.expr_type)
				g.write(current_stmt.trim_left('\t '))
				g.expr(ast.Expr(var))
			} else {
				g.expr(node.expr)
			}
			if has_addr {
				g.write(')')
			}
			if !receiver.typ.is_ptr() {
				g.write(', sizeof(${expr_styp}))')
			}
			g.write(')')
			return
		}
	} else {
		if is_iface_or_sumtype {
			g.write('(*(')
		}
	}
	// var?.field_opt
	field_is_opt := node.expr is ast.Ident && node.expr.is_auto_heap()
		&& node.expr.or_expr.kind != .absent && field_typ.has_flag(.option)
	if field_is_opt {
		g.write('((${g.base_type(field_typ)})')
	}
	old_inside_selector := g.inside_selector
	old_inside_selector_deref := g.inside_selector_deref
	g.inside_selector = node.expr is ast.SelectorExpr && node.expr.expr is ast.Ident
	n_ptr := node.expr_type.nr_muls() - 1
	if n_ptr > 0 {
		g.write2('(', '*'.repeat(n_ptr))
		g.expr(node.expr)
		g.write(')')
	} else {
		g.expr(node.expr)
	}
	mut opt_ptr_already_deref := g.inside_selector_deref
	g.inside_selector = old_inside_selector
	g.inside_selector_deref = old_inside_selector_deref
	if field_is_opt {
		g.write(')')
	}
	if is_opt_or_res {
		g.write('.data)')
	}
	if is_as_cast {
		g.write(')')
	}
	// struct embedding
	mut has_embed := false
	if sym.info in [ast.Struct, ast.Aggregate] {
		if node.generic_from_embed_types.len > 0 && sym.info is ast.Struct {
			if sym.info.embeds.len > 0 {
				mut is_find := false
				for arr_val in node.generic_from_embed_types {
					if arr_val.len > 0 {
						if arr_val[0] == sym.info.embeds[0] {
							g.write_selector_expr_embed_name(node, arr_val)
							is_find = true
							has_embed = true
							break
						}
					}
				}
				if !is_find {
					has_embed = node.from_embed_types.len > 0
					g.write_selector_expr_embed_name(node, node.from_embed_types)
				}
			} else {
				has_embed = node.from_embed_types.len > 0
				g.write_selector_expr_embed_name(node, node.from_embed_types)
			}
		} else if sym.info is ast.Aggregate {
			agg_sym := g.table.sym(sym.info.types[g.aggregate_type_idx])
			if !g.table.struct_has_field(agg_sym, field_name) {
				has_embed = node.from_embed_types.len > 0
				g.write_selector_expr_embed_name(node, node.from_embed_types)
			}
		} else {
			has_embed = node.from_embed_types.len > 0
			g.write_selector_expr_embed_name(node, node.from_embed_types)
		}
	}
	alias_to_ptr := sym.info is ast.Alias && sym.info.parent_type.is_ptr()
	is_dereferenced := node.expr is ast.SelectorExpr && node.expr.expr_type.is_ptr()
		&& !node.expr.typ.is_ptr() && final_sym.kind in [.interface, .sum_type]
	left_is_ptr := field_is_opt
		|| (((!is_dereferenced && unwrapped_expr_type.is_ptr()) || sym.kind == .chan
		|| alias_to_ptr) && node.from_embed_types.len == 0)
		|| (node.expr.is_as_cast() && g.inside_smartcast)
		|| (!opt_ptr_already_deref && unwrapped_expr_type.is_ptr())
	if !has_embed && left_is_ptr {
		g.write('->')
	} else {
		g.write('.')
	}
	if !has_embed && node.expr_type.has_flag(.shared_f) {
		g.write('val.')
	}
	if node.expr_type == 0 {
		verror('cgen: SelectorExpr | expr_type: 0 | it.expr: `${node.expr}` | field: `${node.field_name}` | file: ${g.file.path} | line: ${node.pos.line_nr}')
	}
	g.write(field_name)
	if is_option_unwrap {
		if g.table.final_sym(node.expr_type).kind in [.sum_type, .interface] {
			g.write('->')
		} else {
			g.write('.')
		}
		g.write('data))')
	}
	if sum_type_deref_field != '' {
		g.write2(sum_type_dot, sum_type_deref_field)
		g.write(')')
	}
	if sym.kind in [.interface, .sum_type] {
		g.write('))')
	}
}

fn (mut g Gen) gen_closure_fn(expr_styp string, m ast.Fn, name string) {
	receiver := m.params[0]
	data_styp := g.styp(receiver.typ.idx_type())

	mut sb := strings.new_builder(256)
	sb.write_string('${g.styp(m.return_type)} ${name}(')
	for i in 1 .. m.params.len {
		param := m.params[i]
		if i != 1 {
			sb.write_string(', ')
		}
		sb.write_string('${g.styp(param.typ)} a${i}')
	}
	sb.write_string(')')
	if g.pref.parallel_cc {
		g.extern_out.write_string(sb.bytestr())
		g.extern_out.writeln(';')
	}
	sb.writeln(' {')
	sb.writeln('\t${data_styp}* a0 = __CLOSURE_GET_DATA();')
	if m.return_type != ast.void_type {
		sb.write_string('\treturn ')
	} else {
		sb.write_string('\t')
	}
	mut method_name := m.name
	rec_sym := g.table.sym(receiver.typ)
	if rec_sym.info is ast.Struct {
		if rec_sym.info.concrete_types.len > 0 {
			method_name = g.generic_fn_name(rec_sym.info.concrete_types, m.name)
		}
	}
	if rec_sym.info is ast.Interface && rec_sym.info.get_methods().contains(method_name) {
		left_cc_type := g.cc_type(g.table.unaliased_type(receiver.typ), false)
		left_type_name := util.no_dots(left_cc_type)
		sb.write_string('${c_name(left_type_name)}_name_table[a0->_typ]._method_${method_name}(')
	} else {
		sb.write_string('${expr_styp}_${method_name}(')
		if !receiver.typ.is_ptr() {
			sb.write_string('*')
		}
	}
	for i in 0 .. m.params.len {
		if i != 0 {
			sb.write_string(', ')
		}
		sb.write_string('a${i}')
	}
	sb.writeln(');')
	sb.writeln('}')

	g.anon_fn_definitions << sb.str()
	g.nr_closures++
}

fn (mut g Gen) write_selector_expr_embed_name(node ast.SelectorExpr, embed_types []ast.Type) {
	is_shared := node.expr_type.has_flag(.shared_f)
	for i, embed in embed_types {
		embed_sym := g.table.sym(embed)
		embed_name := embed_sym.embed_name()
		is_left_ptr := if i == 0 {
			node.expr_type.is_ptr() && !is_shared
		} else {
			embed_types[i - 1].is_ptr()
		}
		if i == 0 && is_shared {
			g.write('->val')
		}
		if is_left_ptr {
			g.write('->')
		} else {
			g.write('.')
		}
		g.write(embed_name)
	}
}

// check_var_scope checks if the variable has its value known from the node position
@[inline]
fn (mut g Gen) check_var_scope(obj ast.Var, node_pos int) bool {
	if obj.pos.pos >= node_pos {
		return false
	}
	match obj.expr {
		ast.MatchExpr {
			for branch in obj.expr.branches {
				if branch.scope.contains(node_pos) {
					return false
				}
			}
		}
		ast.IfExpr {
			for branch in obj.expr.branches {
				if branch.scope.contains(node_pos) {
					return false
				}
			}
		}
		else {}
	}
	return true
}

// debugger_stmt writes the call to V debugger REPL
fn (mut g Gen) debugger_stmt(node ast.DebuggerStmt) {
	paline, pafile, pamod, pafn := g.panic_debug_info(node.pos)
	is_anon := g.cur_fn != unsafe { nil } && g.cur_fn.is_anon
	is_generic := g.cur_fn != unsafe { nil } && g.cur_fn.generic_names.len > 0
	is_method := g.cur_fn != unsafe { nil } && g.cur_fn.is_method
	receiver_type := if g.cur_fn != unsafe { nil } && g.cur_fn.is_method {
		g.table.type_to_str(g.cur_fn.receiver.typ)
	} else {
		''
	}
	scope_vars := g.file.scope.innermost(node.pos.pos).get_all_vars()

	// prepares the map containing the scope variable infos
	mut vars := []string{}
	mut keys := strings.new_builder(100)
	mut values := strings.new_builder(100)
	mut count := 1
	outer: for _, obj in scope_vars {
		if obj.name !in vars {
			if obj.name in g.curr_var_name {
				continue
			}
			if obj is ast.Var && g.check_var_scope(obj, node.pos.pos) {
				keys.write_string('_S("${obj.name}")')
				var_typ := if obj.ct_type_var != .no_comptime {
					g.type_resolver.get_type(ast.Ident{ obj: obj })
				} else if obj.smartcasts.len > 0 {
					obj.smartcasts.last()
				} else {
					obj.typ
				}
				values.write_string('{.typ=_S("${g.table.type_to_str(g.unwrap_generic(var_typ))}"),.value=')
				obj_sym := g.table.sym(obj.typ)
				cast_sym := g.table.sym(var_typ)

				mut param_var := strings.new_builder(50)
				is_option := obj.orig_type.has_flag(.option)
				var_typ_is_option := var_typ.has_flag(.option)
				if obj.smartcasts.len > 0 {
					is_option_unwrap := is_option && !obj.typ.has_flag(.option)
						&& obj.orig_type.has_flag(.option)
					mut opt_cast := false
					mut func := if cast_sym.info is ast.Aggregate {
						''
					} else {
						g.get_str_fn(var_typ)
					}
					if obj.smartcasts.len > 1 && obj_sym.kind == .sum_type {
						param_var.write_string('*(')
					}
					param_var.write_string('(')
					if obj_sym.kind == .sum_type && !obj.is_auto_heap {
						if is_option {
							if !is_option_unwrap {
								param_var.write_string('*(')
							}
							styp := g.base_type(obj.typ)
							param_var.write_string('*(${styp}*)')
							opt_cast = true
						} else {
							param_var.write_string('*')
						}
					} else if g.table.is_interface_var(obj) || obj.ct_type_var == .smartcast {
						param_var.write_string('*')
					} else if is_option {
						opt_cast = true
						param_var.write_string('*(${g.base_type(obj.typ)}*)')
					}

					dot := if obj.orig_type.is_ptr() || obj.is_auto_heap { '->' } else { '.' }
					if obj.ct_type_var == .smartcast {
						cur_variant_sym := g.table.sym(g.unwrap_generic(g.type_resolver.get_ct_type_or_default('${g.comptime.comptime_for_variant_var}.typ',
							ast.void_type)))
						param_var.write_string('${obj.name}${dot}_${cur_variant_sym.cname}')
					} else if cast_sym.info is ast.Aggregate {
						sym := g.table.sym(cast_sym.info.types[g.aggregate_type_idx])
						func = g.get_str_fn(cast_sym.info.types[g.aggregate_type_idx])
						param_var.write_string('${obj.name}${dot}_${sym.cname}')
					} else if obj_sym.kind == .interface && cast_sym.kind == .interface {
						ptr := '*'.repeat(obj.typ.nr_muls())
						param_var.write_string('I_${obj_sym.cname}_as_I_${cast_sym.cname}(${ptr}${obj.name})')
					} else if obj_sym.kind in [.sum_type, .interface] {
						param_var.write_string('${obj.name}')
						if opt_cast {
							param_var.write_string('.data)')
						}
						param_var.write_string('${dot}_${cast_sym.cname}')
					} else if is_option && !var_typ_is_option {
						param_var.write_string('${obj.name}.data')
					} else {
						param_var.write_string('${obj.name}')
					}
					param_var.write_string(')')

					values.write_string('${func}(${param_var.str()})}')
				} else {
					func := g.get_str_fn(var_typ)
					if is_option && !var_typ_is_option {
						// option unwrap
						base_typ := g.base_type(obj.typ)
						values.write_string('${func}(*(${base_typ}*)${obj.name}.data)}')
					} else {
						_, str_method_expects_ptr, _ := cast_sym.str_method_info()

						// eprintln(">> ${obj.name} | str expects ptr? ${str_method_expects_ptr} | ptr? ${var_typ.is_ptr()} || auto heap? ${obj.is_auto_heap} | auto deref? ${obj.is_auto_deref}")
						deref := if var_typ_is_option {
							''
						} else if str_method_expects_ptr && !obj.typ.is_ptr() {
							'&'
						} else if !str_method_expects_ptr && obj.typ.is_ptr() {
							'*'.repeat(obj.typ.nr_muls())
						} else if !str_method_expects_ptr && obj_sym.is_heap() {
							'*'
						} else if obj.is_auto_heap && var_typ.is_ptr() && str_method_expects_ptr {
							'*'
						} else if !obj.is_auto_heap && var_typ.is_ptr() && str_method_expects_ptr {
							''
						} else if obj.is_auto_heap && var_typ.is_ptr() {
							'*'
						} else if obj.typ.is_ptr() && !obj.is_auto_deref {
							'&'
						} else if obj.typ.is_ptr() && obj.is_auto_deref {
							'*'
						} else {
							''
						}
						values.write_string('${func}(${deref}${obj.name})}')
					}
				}
				vars << obj.name
				if count != scope_vars.len {
					keys.write_string(',')
					values.write_string(',')
				}
			}
		}
		count += 1
	}
	g.writeln('{')
	g.writeln('\tMap_string_string _scope = new_map_init(&map_hash_string, &map_eq_string, &map_clone_string, &map_free_string, ${vars.len}, sizeof(string), sizeof(v__debug__DebugContextVar),')
	g.write2('\t\t_MOV((string[${vars.len}]){', keys.str())
	g.writeln('}),')
	g.write2('\t\t_MOV((v__debug__DebugContextVar[${vars.len}]){', values.str())
	g.writeln('}));')
	g.writeln('\tv__debug__Debugger_interact(&g_debugger, (v__debug__DebugContextInfo){.is_anon=${is_anon},.is_generic=${is_generic},.is_method=${is_method},.receiver_typ_name=_S("${receiver_type}"),.line=${paline},.file=_S("${pafile}"),.mod=_S("${pamod}"),.fn_name=_S("${pafn}"),.scope=_scope});')
	g.write('}')
}

fn (mut g Gen) enum_decl(node ast.EnumDecl) {
	enum_name := util.no_dots(node.name)
	is_flag := node.is_flag
	if g.is_cc_msvc {
		mut last_value := '0'
		enum_typ_name := g.table.get_type_name(node.typ)
		if g.pref.skip_unused && node.typ.idx() !in g.table.used_features.used_syms {
			return
		}
		g.enum_typedefs.writeln('')
		g.enum_typedefs.writeln('typedef ${enum_typ_name} ${enum_name};')
		for i, field in node.fields {
			g.enum_typedefs.write_string2('\t#define ${enum_name}__${field.name} ', '(')
			if is_flag {
				g.enum_typedefs.write_string2((u64(1) << i).str(), 'ULL')
			} else if field.has_expr {
				expr_str := g.expr_string(field.expr)
				g.enum_typedefs.write_string(expr_str)
				last_value = expr_str
			} else {
				if i != 0 {
					last_value += '+1'
				}
				g.enum_typedefs.write_string(last_value)
			}
			g.enum_typedefs.writeln(')')
		}
		return
	}
	if g.pref.skip_unused && node.typ.idx() !in g.table.used_features.used_syms {
		return
	}
	g.enum_typedefs.writeln('')
	if node.typ != ast.int_type {
		g.enum_typedefs.writeln('#pragma pack(push, 1)')
	}
	g.enum_typedefs.writeln('typedef enum {')
	mut cur_enum_expr := ''
	mut cur_enum_offset := 0
	for i, field in node.fields {
		g.enum_typedefs.write_string('\t${enum_name}__${field.name}')
		if field.has_expr {
			g.enum_typedefs.write_string(' = ')
			expr_str := g.expr_string(field.expr)
			if field.expr is ast.Ident && field.expr.kind == .constant {
				const_def := g.global_const_defs[util.no_dots(field.expr.name)]
				if const_def.def.starts_with('#define') {
					g.enum_typedefs.write_string(const_def.def.all_after_last(' '))
				} else if const_def.def.contains('const ') {
					g.enum_typedefs.write_string(const_def.def.all_after_last('=').all_before_last(';'))
				} else {
					g.enum_typedefs.write_string(expr_str)
				}
			} else {
				g.enum_typedefs.write_string(expr_str)
			}
			cur_enum_expr = expr_str
			cur_enum_offset = 0
		} else if is_flag {
			g.enum_typedefs.write_string(' = ')
			cur_enum_expr = 'u64(1) << ${i}'
			g.enum_typedefs.write_string2((u64(1) << i).str(), 'U')
			cur_enum_offset = 0
		}
		cur_value := if cur_enum_offset > 0 {
			'${cur_enum_expr}+${cur_enum_offset}'
		} else {
			cur_enum_expr
		}
		if g.pref.is_prod {
			g.enum_typedefs.writeln(',')
		} else {
			g.enum_typedefs.writeln(', // ${cur_value}')
		}
		cur_enum_offset++
	}
	packed_attribute := if !g.is_cc_msvc && node.typ != ast.int_type {
		'__attribute__((packed))'
	} else {
		''
	}
	g.enum_typedefs.writeln('} ${packed_attribute} ${enum_name};')
	if node.typ != ast.int_type {
		g.enum_typedefs.writeln('#pragma pack(pop)\n')
	}
}

fn (mut g Gen) enum_expr(node ast.Expr) {
	match node {
		ast.EnumVal {
			g.write(node.val)
		}
		else {
			g.expr(node)
		}
	}
}

fn (mut g Gen) lock_expr(node ast.LockExpr) {
	g.cur_lock = unsafe { node } // is ok because it is discarded at end of fn
	defer {
		g.cur_lock = ast.LockExpr{
			scope: unsafe { nil }
		}
	}
	tmp_result := if node.is_expr { g.new_tmp_var() } else { '' }
	mut cur_line := ''
	if node.is_expr {
		styp := g.styp(node.typ)
		cur_line = g.go_before_last_stmt()
		g.writeln('${styp} ${tmp_result};')
	}
	mut mtxs := ''
	if node.lockeds.len == 0 {
		// this should not happen
	} else if node.lockeds.len == 1 {
		lock_prefix := if node.is_rlock[0] { 'r' } else { '' }
		g.write('sync__RwMutex_${lock_prefix}lock(&')
		g.expr(node.lockeds[0])
		g.writeln('->mtx);')
	} else {
		mtxs = g.new_tmp_var()
		g.writeln2('uintptr_t _arr_${mtxs}[${node.lockeds.len}];', 'bool _isrlck_${mtxs}[${node.lockeds.len}];')
		mut j := 0
		for i, is_rlock in node.is_rlock {
			if !is_rlock {
				g.write('_arr_${mtxs}[${j}] = (uintptr_t)&')
				g.expr(node.lockeds[i])
				g.writeln2('->mtx;', '_isrlck_${mtxs}[${j}] = false;')
				j++
			}
		}
		for i, is_rlock in node.is_rlock {
			if is_rlock {
				g.write('_arr_${mtxs}[${j}] = (uintptr_t)&')
				g.expr(node.lockeds[i])
				g.writeln2('->mtx;', '_isrlck_${mtxs}[${j}] = true;')
				j++
			}
		}
		if node.lockeds.len == 2 {
			g.writeln('if (_arr_${mtxs}[0] > _arr_${mtxs}[1]) {')
			g.writeln('\tuintptr_t _ptr_${mtxs} = _arr_${mtxs}[0];')
			g.writeln('\t_arr_${mtxs}[0] = _arr_${mtxs}[1];')
			g.writeln('\t_arr_${mtxs}[1] = _ptr_${mtxs};')
			g.writeln('\tbool _bool_${mtxs} = _isrlck_${mtxs}[0];')
			g.writeln('\t_isrlck_${mtxs}[0] = _isrlck_${mtxs}[1];')
			g.writeln('\t_isrlck_${mtxs}[1] = _bool_${mtxs};')
			g.writeln('}')
		} else {
			g.writeln('__sort_ptr(_arr_${mtxs}, _isrlck_${mtxs}, ${node.lockeds.len});')
		}
		g.writeln2('for (int ${mtxs}=0; ${mtxs}<${node.lockeds.len}; ${mtxs}++) {', '\tif (${mtxs} && _arr_${mtxs}[${mtxs}] == _arr_${mtxs}[${mtxs}-1]) continue;')
		g.writeln2('\tif (_isrlck_${mtxs}[${mtxs}])', '\t\tsync__RwMutex_rlock((sync__RwMutex*)_arr_${mtxs}[${mtxs}]);')
		g.writeln2('\telse', '\t\tsync__RwMutex_lock((sync__RwMutex*)_arr_${mtxs}[${mtxs}]);')
		g.writeln('}')
	}
	g.mtxs = mtxs
	defer {
		g.mtxs = ''
	}
	g.writeln('/*lock*/ {')
	g.stmts_with_tmp_var(node.stmts, tmp_result)
	if node.is_expr {
		g.writeln(';')
	}
	g.writeln('}')
	g.unlock_locks()
	if node.is_expr {
		g.writeln('')
		g.write2(cur_line, '${tmp_result}')
	}
}

fn (mut g Gen) unlock_locks() {
	if g.cur_lock.lockeds.len == 0 {
	} else if g.cur_lock.lockeds.len == 1 {
		lock_prefix := if g.cur_lock.is_rlock[0] { 'r' } else { '' }
		g.write('sync__RwMutex_${lock_prefix}unlock(&')
		g.expr(g.cur_lock.lockeds[0])
		g.write('->mtx);')
	} else {
		g.writeln('for (int ${g.mtxs}=${g.cur_lock.lockeds.len - 1}; ${g.mtxs}>=0; ${g.mtxs}--) {')
		g.writeln('\tif (${g.mtxs} && _arr_${g.mtxs}[${g.mtxs}] == _arr_${g.mtxs}[${g.mtxs}-1]) continue;')
		g.writeln('\tif (_isrlck_${g.mtxs}[${g.mtxs}])')
		g.writeln('\t\tsync__RwMutex_runlock((sync__RwMutex*)_arr_${g.mtxs}[${g.mtxs}]);')
		g.writeln('\telse')
		g.writeln('\t\tsync__RwMutex_unlock((sync__RwMutex*)_arr_${g.mtxs}[${g.mtxs}]);')
		g.write('}')
	}
}

fn (mut g Gen) map_init(node ast.MapInit) {
	unwrap_key_typ := g.unwrap_generic(node.key_type)
	unwrap_val_typ := g.unwrap_generic(node.value_type).clear_flag(.result)
	key_typ_str := g.styp(unwrap_key_typ)
	value_typ_str := g.styp(unwrap_val_typ)
	value_sym := g.table.sym(unwrap_val_typ)
	key_sym := g.table.final_sym(unwrap_key_typ)
	hash_fn, key_eq_fn, clone_fn, free_fn := g.map_fn_ptrs(key_sym)
	size := node.vals.len
	mut shared_styp := '' // only needed for shared &[]{...}
	mut styp := ''
	is_amp := g.is_amp
	g.is_amp = false
	if is_amp {
		g.go_back(1) // delete the `&` already generated in `prefix_expr()
	}
	if g.is_shared {
		mut shared_typ := node.typ.set_flag(.shared_f)
		shared_styp = g.styp(shared_typ)
		g.writeln('(${shared_styp}*)__dup_shared_map(&(${shared_styp}){.mtx = {0}, .val =')
	} else if is_amp {
		styp = g.styp(node.typ)
		g.write('(${styp}*)memdup(ADDR(${styp}, ')
	}
	noscan_key := g.check_noscan(node.key_type)
	noscan_value := g.check_noscan(node.value_type)
	mut noscan := if noscan_key.len != 0 || noscan_value.len != 0 { '_noscan' } else { '' }
	if noscan.len != 0 {
		if noscan_key.len != 0 {
			noscan += '_key'
		}
		if noscan_value.len != 0 {
			noscan += '_value'
		}
	}
	if size > 0 {
		effective_typ_str := if value_sym.kind == .function { 'voidptr' } else { value_typ_str }
		if node.has_update_expr {
			g.writeln('new_map_update_init(')
			g.write('\t&(')
			g.expr(node.update_expr)
			g.writeln('), ${size}, sizeof(${key_typ_str}), sizeof(${effective_typ_str}),')
		} else {
			g.writeln('new_map_init${noscan}(${hash_fn}, ${key_eq_fn}, ${clone_fn}, ${free_fn}, ${size}, sizeof(${key_typ_str}), sizeof(${effective_typ_str}),')
		}
		g.writeln('\t_MOV((${key_typ_str}[${size}]){')
		for expr in node.keys {
			g.write('\t\t')
			g.expr(expr)
			g.writeln(',')
		}
		g.writeln2('\t}),', '\t_MOV((${effective_typ_str}[${size}]){')
		for i, expr in node.vals {
			g.write('\t\t')
			if expr.is_auto_deref_var() {
				g.write('*')
			}
			if value_sym.kind == .sum_type {
				g.expr_with_cast(expr, node.val_types[i], unwrap_val_typ)
			} else if node.val_types[i].has_flag(.option) || node.val_types[i] == ast.none_type {
				g.expr_with_opt(expr, node.val_types[i], unwrap_val_typ)
			} else {
				g.expr(expr)
			}
			g.writeln(', ')
		}
		g.writeln2('\t})', ')')
	} else if node.has_update_expr {
		g.write('map_clone(&(')
		g.expr(node.update_expr)
		g.writeln('))')
	} else {
		g.writeln('new_map${noscan}(sizeof(${key_typ_str}), sizeof(${value_typ_str}), ${hash_fn}, ${key_eq_fn}, ${clone_fn}, ${free_fn})')
	}
	if g.is_shared {
		g.write('}, sizeof(${shared_styp}))')
	} else if is_amp {
		g.write('), sizeof(${styp}))')
	}
}

fn (mut g Gen) select_expr(node ast.SelectExpr) {
	is_expr := node.is_expr || g.inside_ternary > 0
	cur_line := if is_expr {
		g.empty_line = true
		g.go_before_last_stmt()
	} else {
		''
	}
	n_channels := if node.has_exception { node.branches.len - 1 } else { node.branches.len }
	mut channels := []ast.Expr{cap: n_channels}
	mut objs := []ast.Expr{cap: n_channels}
	mut tmp_objs := []string{cap: n_channels}
	mut elem_types := []string{cap: n_channels}
	mut is_push := []bool{cap: n_channels}
	mut has_else := false
	mut has_timeout := false
	mut timeout_expr := ast.empty_expr
	mut exception_branch := -1
	for j, branch in node.branches {
		if branch.is_else {
			has_else = true
			exception_branch = j
		} else if branch.is_timeout {
			has_timeout = true
			exception_branch = j
			timeout_expr = (branch.stmt as ast.ExprStmt).expr
		} else {
			match branch.stmt {
				ast.ExprStmt {
					// send expression
					expr := branch.stmt.expr as ast.InfixExpr
					channels << expr.left
					if expr.right in [ast.Ident, ast.IndexExpr, ast.SelectorExpr, ast.StructInit] {
						// addressable objects in the `C` output
						objs << expr.right
						tmp_objs << ''
						elem_types << ''
					} else {
						// must be evaluated to tmp var before real `select` is performed
						objs << ast.empty_expr
						tmp_obj := g.new_tmp_var()
						tmp_objs << tmp_obj
						el_stype := g.styp(ast.mktyp(expr.right_type))
						g.writeln('${el_stype} ${tmp_obj};')
					}
					is_push << true
				}
				ast.AssignStmt {
					rec_expr := branch.stmt.right[0] as ast.PrefixExpr
					channels << rec_expr.right
					is_push << false
					// create tmp unless the object with *exactly* the type we need exists already
					if branch.stmt.op == .decl_assign
						|| branch.stmt.right_types[0] != branch.stmt.left_types[0] {
						tmp_obj := g.new_tmp_var()
						tmp_objs << tmp_obj
						el_stype := g.styp(branch.stmt.right_types[0])
						elem_types << if branch.stmt.op == .decl_assign {
							el_stype + ' '
						} else {
							''
						}
						g.writeln('${el_stype} ${tmp_obj};')
					} else {
						tmp_objs << ''
						elem_types << ''
					}
					objs << branch.stmt.left[0]
				}
				else {}
			}
		}
	}
	chan_array := g.new_tmp_var()
	if n_channels == 0 {
		g.writeln('Array_sync__Channel_ptr ${chan_array} = __new_array_with_default(0, 0, sizeof(sync__Channel*), 0);')
	} else {
		g.write('Array_sync__Channel_ptr ${chan_array} = new_array_from_c_array(${n_channels}, ${n_channels}, sizeof(sync__Channel*), _MOV((sync__Channel*[${n_channels}]){')
		for i in 0 .. n_channels {
			if i > 0 {
				g.write(', ')
			}
			g.write('(sync__Channel*)(')
			g.expr(channels[i])
			g.write(')')
		}
		g.writeln('}));\n')
	}
	directions_array := g.new_tmp_var()
	if n_channels == 0 {
		g.writeln('Array_sync__Direction ${directions_array} = __new_array_with_default(0, 0, sizeof(sync__Direction), 0);')
	} else {
		g.write('Array_sync__Direction ${directions_array} = new_array_from_c_array(${n_channels}, ${n_channels}, sizeof(sync__Direction), _MOV((sync__Direction[${n_channels}]){')
		for i in 0 .. n_channels {
			if i > 0 {
				g.write(', ')
			}
			if is_push[i] {
				g.write('sync__Direction__push')
			} else {
				g.write('sync__Direction__pop')
			}
		}
		g.writeln('}));\n')
	}
	objs_array := g.new_tmp_var()
	if n_channels == 0 {
		g.writeln('Array_voidptr ${objs_array} = __new_array_with_default(0, 0, sizeof(voidptr), 0);')
	} else {
		g.write('Array_voidptr ${objs_array} = new_array_from_c_array(${n_channels}, ${n_channels}, sizeof(voidptr), _MOV((voidptr[${n_channels}]){')
		for i in 0 .. n_channels {
			if i > 0 {
				g.write(', &')
			} else {
				g.write('&')
			}
			if tmp_objs[i] == '' {
				g.expr(objs[i])
			} else {
				g.write(tmp_objs[i])
			}
		}
		g.writeln('}));\n')
	}
	select_result := g.new_tmp_var()
	g.write('int ${select_result} = sync__channel_select(&${chan_array}, ${directions_array}, &${objs_array}, ')
	if has_timeout {
		g.expr(timeout_expr)
	} else if has_else {
		g.write('0')
	} else {
		g.write('_const_time__infinite')
	}
	g.writeln(');')
	// free the temps that were created
	g.writeln2('array_free(&${objs_array});', 'array_free(&${directions_array});')
	g.writeln('array_free(&${chan_array});')
	mut i := 0
	for j in 0 .. node.branches.len {
		if j > 0 {
			g.write('} else ')
		}
		g.write('if (${select_result} == ')
		if j == exception_branch {
			g.writeln('-1) {')
		} else {
			g.writeln('${i}) {')
			if !is_push[i] && tmp_objs[i] != '' {
				g.write('\t${elem_types[i]}')
				g.expr(objs[i])
				g.writeln(' = ${tmp_objs[i]};')
			}
			i++
		}
		g.stmts(node.branches[j].stmts)
	}
	g.writeln('}')
	if is_expr {
		g.empty_line = false
		g.write2(cur_line, '(${select_result} != -2)')
	}
}

fn (mut g Gen) get_const_name(node ast.Ident) string {
	if g.pref.translated && !g.is_builtin_mod
		&& !util.module_is_builtin(node.name.all_before_last('.')) {
		mut x := util.no_dots(node.name)
		if x.starts_with('main__') {
			x = x['main__'.len..]
		}
		return x
	} else {
		return '_const_' + g.get_ternary_name(c_name(node.name))
	}
}

fn (mut g Gen) ident(node ast.Ident) {
	prevent_sum_type_unwrapping_once := g.prevent_sum_type_unwrapping_once
	g.prevent_sum_type_unwrapping_once = false
	if node.name == 'lld' {
		return
	}
	if node.name.starts_with('C.') {
		g.write(util.no_dots(node.name[2..]))
		return
	}
	mut name := if node.kind == .function { c_fn_name(node.name) } else { c_name(node.name) }
	if node.kind == .constant {
		if g.pref.translated && !g.is_builtin_mod
			&& !util.module_is_builtin(node.name.all_before_last('.')) {
			// Don't prepend "_const" to translated C consts,
			// but only in user code, continue prepending "_const" to builtin consts.
			mut x := util.no_dots(node.name)
			if x.starts_with('main__') {
				x = x['main__'.len..]
			}
			g.write(x)
			return
		} else {
			if g.inside_opt_or_res && node.or_expr.kind != .absent && node.obj.typ.has_flag(.option) {
				styp := g.base_type(node.obj.typ)
				g.write('(*(${styp}*)')

				defer {
					g.write('.data)')
				}
			}
			// TODO: globals hack
			g.write('_const_')
		}
	}
	mut is_auto_heap := node.is_auto_heap()
	mut is_option := false
	if node.info is ast.IdentVar {
		if node.obj is ast.Var {
			if !g.is_assign_lhs
				&& node.obj.ct_type_var !in [.smartcast, .generic_param, .no_comptime, .aggregate] {
				comptime_type := g.type_resolver.get_type(node)
				if comptime_type.has_flag(.option) {
					if (g.inside_opt_or_res || g.left_is_opt) && node.or_expr.kind == .absent {
						if !g.is_assign_lhs && is_auto_heap {
							g.write('(*${name})')
						} else {
							g.write(name)
						}
					} else {
						styp := g.base_type(comptime_type)
						if is_auto_heap {
							g.write('(*(${styp}*)${name}->data)')
						} else {
							g.write('(*(${styp}*)${name}.data)')
						}
					}
				} else {
					if is_auto_heap {
						g.write2('*', name)
					} else {
						g.write(name)
					}
				}
				if node.or_expr.kind != .absent && !(g.inside_opt_or_res && g.inside_assign
					&& !g.is_assign_lhs) {
					stmt_str := g.go_before_last_stmt().trim_space()
					g.empty_line = true
					var_name := if !g.is_assign_lhs && is_auto_heap { '(*${name})' } else { name }
					g.or_block(var_name, node.or_expr, comptime_type)
					g.writeln(stmt_str)
				}
				return
			}
		}
		// x ?int
		// `x = 10` => `x.data = 10` (g.right_is_opt == false)
		// `x = new_opt()` => `x = new_opt()` (g.right_is_opt == true)
		// `println(x)` => `println(*(int*)x.data)`
		if node.info.is_option && !(g.is_assign_lhs && g.right_is_opt) {
			has_smartcast := node.obj is ast.Var && node.obj.smartcasts.len > 0
				&& !(node.obj.ct_type_var == .no_comptime && node.obj.is_unwrapped)
			if has_smartcast {
				g.write('*(')
			}
			if (g.inside_opt_or_res || g.left_is_opt) && node.or_expr.kind == .absent {
				if !g.is_assign_lhs && is_auto_heap {
					g.write('(*${name})')
				} else {
					if node.obj is ast.Var {
						// mutable option var
						if (g.is_assign_lhs || g.inside_struct_init) && node.obj.is_auto_deref {
							g.write('*')
						}
						if node.obj.is_inherited {
							g.write(closure_ctx + '->')
						}
					}
					g.write(name)
				}
			} else {
				g.unwrap_option_type(node.info.typ, name, is_auto_heap)
			}
			if node.or_expr.kind != .absent && !(g.inside_opt_or_res && g.inside_assign
				&& !g.is_assign_lhs) {
				stmt_str := g.go_before_last_stmt().trim_space()
				g.empty_line = true
				var_name := if !g.is_assign_lhs && is_auto_heap {
					'(*${name})'
				} else {
					name
				}
				g.or_block(var_name, node.or_expr, node.info.typ)
				g.write(stmt_str)
			}
			if node.obj is ast.Var {
				if has_smartcast {
					obj_sym := g.table.sym(g.unwrap_generic(node.obj.typ))
					if node.obj.ct_type_var == .smartcast {
						ctyp := g.unwrap_generic(g.type_resolver.get_type(node))
						cur_variant_sym := g.table.sym(ctyp)
						variant_name := g.get_sumtype_variant_name(ctyp, cur_variant_sym)
						g.write('._${variant_name}')
						if node.obj.is_unwrapped {
							g.write('.data')
						}
					} else if obj_sym.kind == .sum_type {
						variant_typ := g.unwrap_generic(node.obj.smartcasts.last())
						cast_sym := g.table.sym(g.unwrap_generic(node.obj.smartcasts.last()))
						variant_name := g.get_sumtype_variant_name(variant_typ, cast_sym)
						g.write('._${variant_name}')
					}
					g.write(')')
				}
			}
			return
		}
		if !g.is_assign_lhs && node.info.share == .shared_t {
			g.write('${name}.val')
			return
		}
		is_option = node.info.is_option || (node.obj is ast.Var && node.obj.typ.has_flag(.option))
		if node.obj is ast.Var {
			is_auto_heap = node.obj.is_auto_heap
				&& (!g.is_assign_lhs || g.assign_op != .decl_assign)
			if is_auto_heap {
				g.write('(*(')
			}
			is_option = is_option || node.obj.orig_type.has_flag(.option)
			if node.obj.smartcasts.len > 0 {
				obj_sym := g.table.sym(g.unwrap_generic(node.obj.typ))
				if !prevent_sum_type_unwrapping_once {
					nested_unwrap := node.obj.smartcasts.len > 1
					unwrap_sumtype := is_option && nested_unwrap && obj_sym.kind == .sum_type
					if unwrap_sumtype {
						g.write('(*(')
					}
					for i, typ in node.obj.smartcasts {
						is_option_unwrap := i == 0 && is_option
							&& typ == node.obj.orig_type.clear_flag(.option)
						g.write('(')
						if i == 0 && node.obj.is_unwrapped && node.obj.ct_type_var == .smartcast {
							ctyp := g.unwrap_generic(g.type_resolver.get_type(node))
							g.write('*(${g.base_type(ctyp)}*)(')
						}
						if obj_sym.kind == .sum_type && !is_auto_heap {
							if is_option {
								if i == 0 {
									if !is_option_unwrap {
										g.write('*(')
									}
									styp := g.base_type(node.obj.typ)
									g.write('*(${styp}*)')
								}
							} else if !g.arg_no_auto_deref {
								g.write('*')
							}
						} else if (g.inside_interface_deref && g.table.is_interface_var(node.obj))
							|| node.obj.ct_type_var == .smartcast
							|| (obj_sym.kind == .interface
							&& g.table.type_kind(node.obj.typ) == .any) {
							g.write('*')
						} else if is_option {
							g.write('*(${g.base_type(node.obj.typ)}*)')
						}
					}
					for i, typ in node.obj.smartcasts {
						is_option_unwrap := is_option && typ == node.obj.typ.clear_flag(.option)
						cast_sym := g.table.sym(g.unwrap_generic(typ))
						if obj_sym.kind == .interface && cast_sym.kind == .interface {
							if cast_sym.cname != obj_sym.cname {
								ptr := '*'.repeat(node.obj.typ.nr_muls())
								g.write('I_${obj_sym.cname}_as_I_${cast_sym.cname}(${ptr}${node.name})')
							} else {
								ptr := if is_option {
									''
								} else {
									'*'.repeat(node.obj.typ.nr_muls())
								}
								g.write('${ptr}${node.name}')
							}
						} else {
							mut is_ptr := false
							if i == 0 {
								if node.obj.is_inherited {
									g.write(closure_ctx + '->')
								}
								if node.obj.typ.nr_muls() > 1 {
									g.write2('(', '*'.repeat(node.obj.typ.nr_muls() - 1))
									g.write2(name, ')')
								} else {
									g.write(name)
								}
								if node.obj.orig_type.is_ptr() {
									is_ptr = true
								}
							}
							dot := if is_ptr || is_auto_heap { '->' } else { '.' }
							if cast_sym.info is ast.Aggregate {
								sym := g.table.sym(cast_sym.info.types[g.aggregate_type_idx])
								g.write('${dot}_${sym.cname}')
							} else {
								if is_option && !node.obj.is_unwrapped {
									if i == 0 {
										g.write('.data')
									}
									if !is_option_unwrap {
										g.write(')')
									}
								}
								if node.obj.ct_type_var == .smartcast {
									mut ctyp := g.unwrap_generic(g.type_resolver.get_type(node))
									cur_variant_sym := g.table.sym(ctyp)
									if node.obj.is_unwrapped {
										ctyp = ctyp.set_flag(.option)
										g.write('${dot}_${g.get_sumtype_variant_name(ctyp,
											cur_variant_sym)}')
										g.write(').data')
									} else {
										g.write('${dot}_${g.get_sumtype_variant_name(ctyp,
											cur_variant_sym)}')
									}
								} else if !is_option_unwrap
									&& obj_sym.kind in [.sum_type, .interface] {
									g.write('${dot}_${cast_sym.cname}')
								}
								if i != 0 && unwrap_sumtype {
									g.write(')')
								}
							}
						}
						if i == 0 && node.obj.ct_type_var != .smartcast && node.obj.is_unwrapped {
							dot := if (!node.obj.ct_type_unwrapped && !node.obj.orig_type.is_ptr()
								&& obj_sym.is_heap())
								|| node.obj.orig_type.has_flag(.option_mut_param_t) {
								'->'
							} else {
								'.'
							}
							g.write('${dot}data')
						}
						g.write(')')
					}
					if is_auto_heap {
						g.write('))')
					}
					return
				}
			}
			if node.obj.is_inherited {
				g.write(closure_ctx + '->')
			}
		}
	} else if node.info is ast.IdentFn {
		// TODO: PERF fn lookup for each fn call in translated mode
		if func := g.table.find_fn(node.name) {
			if g.pref.translated || g.file.is_translated || func.is_file_translated {
				// `p_mobjthinker` => `P_MobjThinker`
				if cattr := func.attrs.find_first('c') {
					name = cattr.arg
				}
			} else if node.concrete_types.len > 0 {
				name = g.generic_fn_name(node.concrete_types, name)
			}
		}
	}
	g.write(g.get_ternary_name(name))
	if is_auto_heap {
		g.write('))')
		if is_option && node.or_expr.kind != .absent {
			g.write('.data')
		}
	}
	if node.or_expr.kind != .absent && !(g.inside_opt_or_res && g.inside_assign && !g.is_assign_lhs) {
		stmt_str := g.go_before_last_stmt().trim_space()
		g.empty_line = true
		var_opt := if is_auto_heap { '(*${name})' } else { name }
		g.or_block(var_opt, node.or_expr, node.obj.typ)
		g.write(stmt_str)
	}
}

fn (mut g Gen) cast_expr(node ast.CastExpr) {
	tmp_inside_cast := g.inside_cast
	g.inside_cast = true
	defer {
		g.inside_cast = tmp_inside_cast
	}
	node_typ := g.unwrap_generic(node.typ)
	mut expr_type := node.expr_type
	sym := g.table.sym(node_typ)
	if g.comptime.is_comptime(node.expr) {
		expr_type = g.unwrap_generic(g.type_resolver.get_type(node.expr))
	}
	node_typ_is_option := node.typ.has_flag(.option)
	if sym.kind in [.sum_type, .interface] {
		if node_typ_is_option && node.expr is ast.None {
			g.gen_option_error(node.typ, node.expr)
		} else if node.expr is ast.Ident && g.comptime.is_comptime_variant_var(node.expr) {
			g.expr_with_cast(node.expr, g.type_resolver.get_ct_type_or_default('${g.comptime.comptime_for_variant_var}.typ',
				ast.void_type), node_typ)
		} else if node_typ_is_option {
			g.expr_with_opt(node.expr, expr_type, node.typ)
		} else {
			g.expr_with_cast(node.expr, expr_type, node_typ)
		}
	} else if !node_typ_is_option && !node.typ.is_ptr() && sym.info is ast.Struct
		&& !sym.info.is_typedef {
		// deprecated, replaced by Struct{...exr}
		styp := g.styp(node.typ)
		g.write('*((${styp} *)(&')
		g.expr(node.expr)
		g.write('))')
	} else if sym.kind == .alias && g.table.final_sym(node.typ).kind == .array_fixed {
		if node_typ_is_option {
			g.expr_with_opt(node.expr, expr_type, node.typ)
		} else {
			if node.expr is ast.ArrayInit && g.assign_op != .decl_assign && !g.inside_const {
				g.write('(${g.styp(node.expr.typ)})')
			}
			g.expr(node.expr)
		}
	} else if expr_type == ast.bool_type && node.typ.is_int() {
		styp := g.styp(node_typ)
		g.write('(${styp}[]){(')
		g.expr(node.expr)
		g.write(')?1:0}[0]')
	} else {
		styp := g.styp(node.typ)
		if (g.pref.translated || g.file.is_translated) && sym.kind == .function {
			// TODO: handle the type in fn casts, not just exprs
			/*
			info := sym.info as ast.FnType
			if cattr := info.func.attrs.find_first('c') {
				name = cattr.arg
			}
			*/
		}
		mut cast_label := ''
		// `ast.string_type` is done for MSVC's bug
		if sym.kind != .alias
			|| (sym.info is ast.Alias && !sym.info.parent_type.has_flag(.option)
			&& sym.info.parent_type !in [expr_type, ast.string_type]) {
			if sym.kind == .string && !node.typ.is_ptr() {
				cast_label = '*(string*)&'
			} else if !(g.is_cc_msvc && g.styp(node.typ) == g.styp(expr_type)) {
				cast_label = '(${styp})'
			}
		}
		if node_typ_is_option && node.expr is ast.None {
			g.gen_option_error(node.typ, node.expr)
		} else if node_typ_is_option {
			if sym.info is ast.Alias {
				if sym.info.parent_type.has_flag(.option) {
					cur_stmt := g.go_before_last_stmt()
					g.empty_line = true
					parent_type := sym.info.parent_type
					tmp_var := g.new_tmp_var()
					tmp_var2 := g.new_tmp_var()
					g.writeln2('${styp} ${tmp_var};', '${g.styp(parent_type)} ${tmp_var2};')
					g.write('_option_ok(&(${g.base_type(parent_type)}[]) { ')
					g.expr(node.expr)
					g.writeln(' }, (${option_name}*)(&${tmp_var2}), sizeof(${g.base_type(parent_type)}));')
					g.writeln('_option_ok(&(${g.styp(parent_type)}[]) { ${tmp_var2} }, (${option_name}*)&${tmp_var}, sizeof(${g.styp(parent_type)}));')
					g.write2(cur_stmt, tmp_var)
				} else if node.expr_type.has_flag(.option) {
					g.expr_opt_with_alias(node.expr, expr_type, node.typ)
				} else {
					g.expr_with_opt(node.expr, expr_type, node.typ)
				}
			} else {
				g.expr_with_opt(node.expr, expr_type, node.typ)
			}
		} else if sym.info is ast.Alias && sym.info.parent_type.has_flag(.option) {
			g.expr_with_opt(node.expr, expr_type, sym.info.parent_type)
		} else {
			g.write('(${cast_label}(')
			if node.expr is ast.Ident {
				if !node.typ.is_ptr() && node.expr_type.is_ptr() && node.expr.obj is ast.Var
					&& node.expr.obj.smartcasts.len > 0 {
					g.write('*'.repeat(node.expr_type.nr_muls()))
				}
			}
			if sym.kind == .alias && g.table.final_sym(node.typ).kind == .string {
				ptr_cnt := node.typ.nr_muls() - expr_type.nr_muls()
				if ptr_cnt > 0 {
					g.write('&'.repeat(ptr_cnt))
				}
			}
			g.expr(node.expr)
			if node.expr is ast.IntegerLiteral {
				if node_typ in [ast.u64_type, ast.u32_type, ast.u16_type] {
					if !node.expr.val.starts_with('-') {
						g.write('U')
					}
				}
			}
			g.write('))')
		}
	}
}

fn (mut g Gen) concat_expr(node ast.ConcatExpr) {
	mut typ := node.return_type.clear_option_and_result()
	if g.inside_return {
		typ = g.fn_decl.return_type.clear_option_and_result()
	} else if g.inside_or_block {
		typ = g.or_expr_return_type.clear_option_and_result()
	}
	styp := g.styp(typ)
	sym := g.table.sym(node.return_type)
	is_multi := sym.kind == .multi_return
	if !is_multi {
		g.expr(node.vals[0])
	} else {
		types := (g.table.sym(typ).info as ast.MultiReturn).types
		g.write('(${styp}){')
		for i, expr in node.vals {
			g.write('.arg${i}=')
			expr_typ := g.get_expr_type(expr)
			if expr_typ != ast.void_type && types[i].has_flag(.option) {
				g.expr_with_opt(expr, expr_typ, types[i])
			} else {
				old_left_is_opt := g.left_is_opt
				g.left_is_opt = true
				g.expr(expr)
				g.left_is_opt = old_left_is_opt
			}
			if i < node.vals.len - 1 {
				g.write(',')
			}
		}
		g.write('}')
	}
}

@[inline]
fn (g &Gen) expr_is_multi_return_call(expr ast.Expr) bool {
	if expr is ast.CallExpr {
		return g.table.sym(expr.return_type).kind == .multi_return
	}
	return false
}

fn (mut g Gen) gen_result_error(target_type ast.Type, expr ast.Expr) {
	styp := g.styp(g.unwrap_generic(target_type))
	g.write('(${styp}){ .is_error=true, .err=')
	g.expr(expr)
	g.write(', .data={E_STRUCT} }')
}

// NB: remove this when option has no errors anymore
fn (mut g Gen) gen_option_error(target_type ast.Type, expr ast.Expr) {
	styp := g.styp(g.unwrap_generic(target_type))
	g.write('(${styp}){ .state=2, .err=')
	if target_type.has_flag(.option) && expr is ast.Ident && expr.or_expr.kind == .propagate_option {
		g.expr(ast.None{}) // option type unwrapping error
	} else {
		g.expr(expr)
	}
	g.write(', .data={E_STRUCT} }')
}

fn (mut g Gen) hash_stmt_guarded_include(node ast.HashStmt) string {
	mut missing_message := 'Header file ${node.main}, needed for module `${node.mod}` was not found.'
	if node.msg != '' {
		missing_message += ' ${node.msg}.'
	} else {
		missing_message += ' Please install the corresponding development headers.'
	}
	mut guarded_include := get_guarded_include_text(node.main, missing_message)
	if node.main == '<errno.h>' {
		// fails with musl-gcc and msvc; but an unguarded include works:
		guarded_include = '#include ${node.main}'
	}
	return guarded_include
}

fn (mut g Gen) hash_stmt(node ast.HashStmt) {
	line_nr := node.pos.line_nr + 1
	mut ct_condition := ''
	if node.ct_conds.len > 0 {
		ct_condition_start := g.out.len
		for idx, ct_expr in node.ct_conds {
			g.comptime_if_cond(ct_expr, false)
			if idx < node.ct_conds.len - 1 {
				g.write(' && ')
			}
		}
		ct_condition = g.out.cut_to(ct_condition_start).trim_space()
	}
	// #include etc
	if node.kind == 'include' {
		guarded_include := g.hash_stmt_guarded_include(node)
		if node.main.contains('.m') {
			g.definitions.writeln('')
			if ct_condition != '' {
				g.definitions.writeln('#if ${ct_condition}')
			}
			// Objective C code import, include it after V types, so that e.g. `string` is
			// available there
			g.definitions.writeln('// added by module `${node.mod}`, file: ${os.file_name(node.source_file)}:${line_nr}:')
			g.definitions.writeln(guarded_include)
			if ct_condition != '' {
				g.definitions.writeln('#endif // \$if ${ct_condition}')
			}
		} else {
			g.includes.writeln('')
			if ct_condition != '' {
				g.includes.writeln('#if ${ct_condition}')
			}
			g.includes.writeln('// added by module `${node.mod}`, file: ${os.file_name(node.source_file)}:${line_nr}:')
			g.includes.writeln(guarded_include)
			if ct_condition != '' {
				g.includes.writeln('#endif // \$if ${ct_condition}')
			}
		}
	} else if node.kind == 'preinclude' {
		guarded_include := g.hash_stmt_guarded_include(node)
		if node.main.contains('.m') {
			// Might need to support '#preinclude' for .m files as well but for the moment
			// this does the same as '#include' for them
			g.definitions.writeln('')
			if ct_condition != '' {
				g.definitions.writeln('#if ${ct_condition}')
			}
			// Objective C code import, include it after V types, so that e.g. `string` is
			// available there
			g.definitions.writeln('// added by module `${node.mod}`, file: ${os.file_name(node.source_file)}:${line_nr}:')
			g.definitions.writeln(guarded_include)
			if ct_condition != '' {
				g.definitions.writeln('#endif // \$if ${ct_condition}')
			}
		} else {
			g.preincludes.writeln('')
			if ct_condition != '' {
				g.preincludes.writeln('#if ${ct_condition}')
			}
			g.preincludes.writeln('// added by module `${node.mod}`, file: ${os.file_name(node.source_file)}:${line_nr}:')
			g.preincludes.writeln(guarded_include)
			if ct_condition != '' {
				g.preincludes.writeln('#endif // \$if ${ct_condition}')
			}
		}
	} else if node.kind == 'postinclude' {
		guarded_include := g.hash_stmt_guarded_include(node)
		g.postincludes.writeln('')
		if ct_condition != '' {
			g.postincludes.writeln('#if ${ct_condition}')
		}
		g.postincludes.writeln('// added by module `${node.mod}`, file: ${os.file_name(node.source_file)}:${line_nr}:')
		g.postincludes.writeln(guarded_include)
		if ct_condition != '' {
			g.postincludes.writeln('#endif // \$if ${ct_condition}')
		}
	} else if node.kind == 'insert' {
		if ct_condition != '' {
			g.includes.writeln('#if ${ct_condition}')
		}
		g.includes.writeln('// inserted by module `${node.mod}`, file: ${os.file_name(node.source_file)}:${line_nr}:')
		g.includes.writeln(node.val)
		if ct_condition != '' {
			g.includes.writeln('#endif // \$if ${ct_condition}')
		}
	} else if node.kind == 'define' {
		if ct_condition != '' {
			g.includes.writeln('#if ${ct_condition}')
		}
		g.includes.writeln('// defined by module `${node.mod}`')
		g.includes.writeln('#define ${node.main}')
		if ct_condition != '' {
			g.includes.writeln('#endif // \$if ${ct_condition}')
		}
	}
}

fn (mut g Gen) branch_stmt(node ast.BranchStmt) {
	if node.label != '' {
		x := g.labeled_loops[node.label] or {
			panic('${node.label} doesn\'t exist ${g.file.path}, ${node.pos}')
		}
		match x {
			ast.ForCStmt {
				if x.scope.contains(g.cur_lock.pos.pos) {
					g.unlock_locks()
				}
			}
			ast.ForInStmt {
				if x.scope.contains(g.cur_lock.pos.pos) {
					g.unlock_locks()
				}
			}
			ast.ForStmt {
				if x.scope.contains(g.cur_lock.pos.pos) {
					g.unlock_locks()
				}
			}
			else {}
		}

		if node.kind == .key_break {
			g.writeln('goto ${node.label}__break;')
		} else {
			g.writeln('goto ${node.label}__continue;')
		}
	} else {
		inner_loop := g.inner_loop
		match inner_loop {
			ast.ForCStmt {
				if inner_loop.scope.contains(g.cur_lock.pos.pos) {
					g.unlock_locks()
				}
			}
			ast.ForInStmt {
				if inner_loop.scope.contains(g.cur_lock.pos.pos) {
					g.unlock_locks()
				}
			}
			ast.ForStmt {
				if inner_loop.scope.contains(g.cur_lock.pos.pos) {
					g.unlock_locks()
				}
			}
			else {}
		}
		// continue or break
		if g.is_autofree && !g.is_builtin_mod {
			g.trace_autofree('// free before continue/break')
			g.autofree_scope_vars_stop(node.pos.pos - 1, node.pos.line_nr, true, g.branch_parent_pos)
		}
		g.writeln('${node.kind};')
	}
}

fn (mut g Gen) return_stmt(node ast.Return) {
	g.set_current_pos_as_last_stmt_pos()
	g.write_v_source_line_info_stmt(node)

	old_inside_return := g.inside_return
	g.inside_return = true
	defer {
		g.inside_return = old_inside_return
	}

	exprs_len := node.exprs.len
	expr0 := if exprs_len > 0 { node.exprs[0] } else { ast.empty_expr }
	type0 := if exprs_len > 0 { g.unwrap_generic(node.types[0]) } else { ast.void_type }

	if exprs_len > 0 {
		// skip `return $vweb.html()`
		if expr0 is ast.ComptimeCall && expr0.is_vweb {
			g.inside_return_tmpl = true
			g.expr(expr0)
			g.inside_return_tmpl = false
			g.writeln(';')
			return
		}
	}
	ret_type := g.unwrap_generic(g.fn_decl.return_type)

	// got to do a correct check for multireturn
	sym := g.table.sym(ret_type)
	mut fn_ret_type := ret_type
	if sym.kind == .alias {
		unaliased_type := g.table.unaliased_type(fn_ret_type)
		if unaliased_type.has_option_or_result() {
			fn_ret_type = g.unwrap_generic(unaliased_type)
		}
	}
	fn_return_is_multi := sym.kind == .multi_return
	fn_return_is_option := fn_ret_type.has_flag(.option)
	fn_return_is_result := fn_ret_type.has_flag(.result)
	fn_return_is_fixed_array := sym.is_array_fixed()
	fn_return_is_fixed_array_non_result := fn_return_is_fixed_array
		&& !fn_ret_type.has_option_or_result()
	mut has_semicolon := false
	if exprs_len == 0 {
		g.write_defer_stmts_when_needed()
		if fn_return_is_option || fn_return_is_result {
			styp := g.styp(fn_ret_type)
			if g.is_autofree {
				g.trace_autofree('// free before return (no values returned)')
				g.autofree_scope_vars(node.pos.pos, node.pos.line_nr, false)
			}
			g.writeln('return (${styp}){0};')
		} else {
			if g.is_autofree {
				g.trace_autofree('// free before return (no values returned)')
				g.autofree_scope_vars(node.pos.pos - 1, node.pos.line_nr, true)
			}
			g.writeln('return;')
		}
		return
	}
	tmpvar := g.new_tmp_var()
	g.defer_return_tmp_var = tmpvar
	ret_typ := g.ret_styp(fn_ret_type)

	// `return fn_call_opt()`
	if exprs_len == 1 && (fn_return_is_option || fn_return_is_result) && expr0 is ast.CallExpr
		&& expr0.return_type == ret_type && expr0.or_block.kind == .absent {
		if g.defer_stmts.len > 0 {
			g.write('${ret_typ} ${tmpvar} = ')
			g.expr(expr0)
			g.writeln(';')
			g.write_defer_stmts_when_needed()
			g.writeln('return ${tmpvar};')
		} else {
			g.write_defer_stmts_when_needed()
			g.write('return ')
			g.expr(expr0)
			g.writeln(';')
		}
		return
	}
	mut use_tmp_var := g.defer_stmts.len > 0 || g.defer_profile_code.len > 0
		|| g.cur_lock.lockeds.len > 0
		|| (fn_return_is_multi && exprs_len >= 1 && fn_return_is_option)
		|| fn_return_is_fixed_array_non_result
		|| (fn_return_is_multi && node.types.any(g.table.final_sym(it).kind == .array_fixed))
	// handle promoting none/error/function returning _option'
	if fn_return_is_option {
		option_none := expr0 is ast.None
		ftyp := g.styp(type0)
		mut is_regular_option := ftyp == '_option'
		if option_none || is_regular_option || type0 == ast.error_type_idx {
			if g.fn_decl != unsafe { nil } && g.fn_decl.is_test {
				test_error_var := g.new_tmp_var()
				g.write('${ret_typ} ${test_error_var} = ')
				g.gen_option_error(fn_ret_type, expr0)
				g.writeln(';')
				g.write_defer_stmts_when_needed()
				g.gen_failing_return_error_for_test_fn(node, test_error_var)
				return
			}
			if use_tmp_var {
				g.write('${ret_typ} ${tmpvar} = ')
			} else {
				g.write('return ')
			}
			g.gen_option_error(fn_ret_type, expr0)
			g.writeln(';')
			if use_tmp_var {
				// handle options when returning `none` for `?(int, ?int)`
				if fn_return_is_multi && exprs_len >= 1 {
					mr_info := sym.info as ast.MultiReturn
					for i in 0 .. mr_info.types.len {
						if mr_info.types[i].has_flag(.option) {
							g.write('(*(${g.base_type(fn_ret_type)}*)${tmpvar}.data).arg${i} = ')
							g.gen_option_error(mr_info.types[i], ast.None{})
							g.writeln(';')
						}
					}
				}
				g.write_defer_stmts_when_needed()
				g.writeln('return ${tmpvar};')
			}
			return
		}
	}
	// handle promoting error/function returning result
	if fn_return_is_result {
		ftyp := g.styp(type0)
		mut is_regular_result := ftyp == result_name
		if is_regular_result || type0 == ast.error_type_idx {
			if g.fn_decl != unsafe { nil } && g.fn_decl.is_test {
				test_error_var := g.new_tmp_var()
				g.write('${ret_typ} ${test_error_var} = ')
				g.gen_result_error(fn_ret_type, expr0)
				g.writeln(';')
				g.write_defer_stmts_when_needed()
				g.gen_failing_return_error_for_test_fn(node, test_error_var)
				return
			}
			if use_tmp_var {
				g.write('${ret_typ} ${tmpvar} = ')
			} else {
				g.write('return ')
			}
			g.gen_result_error(fn_ret_type, expr0)
			g.writeln(';')
			if use_tmp_var {
				g.write_defer_stmts_when_needed()
				g.writeln('return ${tmpvar};')
			}
			return
		}
	}
	// regular cases
	if fn_return_is_multi && exprs_len > 0 && !g.expr_is_multi_return_call(expr0) {
		if exprs_len == 1 && (expr0 is ast.IfExpr || expr0 is ast.MatchExpr) {
			// use a temporary for `return if cond { x,y } else { a,b }` or `return match expr { abc { x, y } else { z, w } }`
			g.write('${ret_typ} ${tmpvar} = ')
			g.expr(expr0)
			g.writeln(';')
			g.write_defer_stmts_when_needed()
			g.writeln('return ${tmpvar};')
			return
		}
		mr_info := sym.info as ast.MultiReturn
		mut styp := ''
		if fn_return_is_option {
			g.writeln('${ret_typ} ${tmpvar};')
			styp = g.base_type(ret_type)
			g.write('_option_ok(&(${styp}[]) { ')
		} else if fn_return_is_result {
			g.writeln('${ret_typ} ${tmpvar};')
			styp = g.base_type(ret_type)
			g.write('_result_ok(&(${styp}[]) { ')
		} else {
			if use_tmp_var {
				g.write('${ret_typ} ${tmpvar} = ')
			} else {
				g.write('return ')
			}
			styp = g.styp(ret_type)
		}
		// Use this to keep the tmp assignments in order
		mut multi_unpack := ''
		mut final_assignments := ''
		g.write('(${styp}){')
		mut arg_idx := 0
		for i, expr in node.exprs {
			// Check if we are dealing with a multi return and handle it separately
			if g.expr_is_multi_return_call(expr) {
				call_expr := expr as ast.CallExpr
				expr_sym := g.table.sym(call_expr.return_type)
				mut tmp := g.new_tmp_var()
				if !call_expr.return_type.has_option_or_result() {
					line := g.go_before_last_stmt()
					expr_styp := g.styp(call_expr.return_type)
					g.write('${expr_styp} ${tmp}=')
					g.expr(expr)
					g.writeln(';')
					multi_unpack += g.go_before_last_stmt()
					g.write(line)
				} else {
					line := g.go_before_last_stmt()
					g.tmp_count--
					g.expr(expr)
					multi_unpack += g.go_before_last_stmt()
					g.write(line)
					expr_styp := g.base_type(call_expr.return_type)
					tmp = '(*(${expr_styp}*)${tmp}.data)'
				}
				expr_types := expr_sym.mr_info().types
				for j, _ in expr_types {
					g.write('.arg${arg_idx}=${tmp}.arg${j}')
					if j < expr_types.len || i < exprs_len - 1 {
						g.write(',')
					}
					arg_idx++
				}
				continue
			}
			g.write('.arg${arg_idx}=')
			if expr !is ast.ArrayInit && g.table.final_sym(node.types[i]).kind == .array_fixed {
				line := g.go_before_last_stmt().trim_space()
				expr_styp := g.styp(node.types[i])
				g.write('memcpy(&${tmpvar}.arg${arg_idx}, ')
				if expr is ast.StructInit {
					g.write('(${expr_styp})')
				}
				g.expr(expr)
				g.writeln(', sizeof(${expr_styp}));')
				final_assignments += g.go_before_last_stmt() + '\t'
				g.write2(line, '{0}')
			} else {
				if expr.is_auto_deref_var() {
					g.write('*')
				}
				if mr_info.types[i].has_flag(.option) {
					g.expr_with_opt(expr, node.types[i], mr_info.types[i])
				} else if g.table.sym(mr_info.types[i]).kind in [.sum_type, .interface] {
					g.expr_with_cast(expr, node.types[i], mr_info.types[i])
				} else {
					g.expr(expr)
				}
			}
			if i < exprs_len - 1 {
				g.write(', ')
			}
			arg_idx++
		}
		g.write('}')
		if fn_return_is_option {
			g.writeln(' }, (${option_name}*)(&${tmpvar}), sizeof(${styp}));')
			g.write_defer_stmts_when_needed()
			g.write('return ${tmpvar}')
		} else if fn_return_is_result {
			g.writeln(' }, (${result_name}*)(&${tmpvar}), sizeof(${styp}));')
			g.write_defer_stmts_when_needed()
			g.write('return ${tmpvar}')
		}
		// Make sure to add our unpacks
		if multi_unpack != '' {
			g.insert_before_stmt(multi_unpack)
		}
		if final_assignments != '' {
			g.writeln(';')
			g.write(final_assignments)
		}
		if use_tmp_var && !fn_return_is_option && !fn_return_is_result {
			if !has_semicolon {
				g.writeln(';')
			}
			g.write_defer_stmts_when_needed()
			g.writeln('return ${tmpvar};')
			has_semicolon = true
		}
	} else if exprs_len >= 1 {
		if node.types.len == 0 {
			g.checker_bug('node.exprs.len == ${node.exprs.len} && node.types.len == 0',
				node.pos)
		}
		// normal return
		return_sym := g.table.final_sym(type0)
		// `return opt_ok(expr)` for functions that expect an option
		expr_type_is_opt := match expr0 {
			ast.CallExpr {
				expr0.return_type.has_flag(.option) && expr0.or_block.kind == .absent
			}
			else {
				type0.has_flag(.option)
			}
		}
		if fn_return_is_option && !expr_type_is_opt && return_sym.name != option_name {
			if fn_return_is_fixed_array && (expr0 in [ast.StructInit, ast.CallExpr, ast.CastExpr]
				|| (expr0 is ast.ArrayInit && expr0.has_callexpr))
				&& g.table.final_sym(type0).kind == .array_fixed {
				styp := g.styp(fn_ret_type.clear_option_and_result())
				if expr0 is ast.CallExpr {
					tmp_var := g.expr_with_fixed_array(expr0, type0, fn_ret_type)
					g.writeln('${ret_typ} ${tmpvar} = ${tmp_var};')
				} else {
					g.writeln('${ret_typ} ${tmpvar} = (${ret_typ}){ .state=0, .err=_const_none__, .data={E_STRUCT} };')
					if expr0 is ast.StructInit {
						g.write('memcpy(${tmpvar}.data, ')
						tmp_var := g.expr_with_opt(expr0, type0, fn_ret_type)
						g.writeln('.data, sizeof(${styp}));')
						if tmp_var != '' {
							g.writeln('${tmpvar}.state = ${tmp_var}.state;')
						}
					} else {
						g.write('memcpy(${tmpvar}.data, ')
						g.expr(expr0)
						g.writeln(', sizeof(${styp}));')
					}
				}
			} else {
				g.writeln('${ret_typ} ${tmpvar};')
				styp := g.base_type(fn_ret_type)
				g.write('_option_ok(&(${styp}[]) { ')
				if !fn_ret_type.is_ptr() && type0.is_ptr() {
					if !(expr0 is ast.Ident && !g.is_amp) {
						g.write('*')
					}
				}
				if return_sym.kind == .array_fixed && expr0 !is ast.ArrayInit {
					info := return_sym.info as ast.ArrayFixed
					g.fixed_array_var_init(g.expr_string(expr0), expr0.is_auto_deref_var(),
						info.elem_type, info.size)
				} else {
					g.expr_with_cast(expr0, type0, fn_ret_type.clear_option_and_result())
				}
				g.writeln(' }, (${option_name}*)(&${tmpvar}), sizeof(${styp}));')
			}
			g.write_defer_stmts_when_needed()
			g.autofree_scope_vars(node.pos.pos - 1, node.pos.line_nr, true)
			g.writeln('return ${tmpvar};')
			return
		}
		expr_type_is_result := match expr0 {
			ast.CallExpr {
				expr0.return_type.has_flag(.result) && expr0.or_block.kind == .absent
			}
			else {
				type0.has_flag(.result)
			}
		}
		if fn_return_is_result && !expr_type_is_result && return_sym.name != result_name {
			g.writeln('${ret_typ} ${tmpvar} = {0};')
			if fn_return_is_fixed_array && expr0 !is ast.ArrayInit
				&& g.table.final_sym(type0).kind == .array_fixed {
				styp := g.styp(fn_ret_type.clear_option_and_result())
				g.write('memcpy(${tmpvar}.data, ')
				if expr0 in [ast.CallExpr, ast.StructInit] {
					g.expr_with_opt(expr0, type0, fn_ret_type)
					g.write('.data')
				} else {
					g.expr(expr0)
				}
				g.writeln(', sizeof(${styp}));')
			} else {
				styp := g.base_type(fn_ret_type)
				g.write('_result_ok(&(${styp}[]) { ')
				if !fn_ret_type.is_ptr() && type0.is_ptr() {
					if !((expr0 is ast.Ident && !g.is_amp) || sym.kind == .interface) {
						g.write('*')
					}
				}
				if fn_ret_type.has_flag(.option) {
					g.expr_with_opt(expr0, type0, fn_ret_type.clear_flag(.result))
				} else if return_sym.kind == .array_fixed && expr0 !is ast.ArrayInit {
					info := return_sym.info as ast.ArrayFixed
					g.fixed_array_var_init(g.expr_string(expr0), expr0.is_auto_deref_var(),
						info.elem_type, info.size)
				} else {
					g.expr_with_cast(expr0, type0, fn_ret_type.clear_flag(.result))
				}
				g.writeln(' }, (${result_name}*)(&${tmpvar}), sizeof(${styp}));')
			}
			g.write_defer_stmts_when_needed()
			g.autofree_scope_vars(node.pos.pos - 1, node.pos.line_nr, true)
			g.writeln('return ${tmpvar};')
			return
		}
		// autofree before `return`
		// set free_parent_scopes to true, since all variables defined in parent
		// scopes need to be freed before the return
		if g.is_autofree {
			if expr0 is ast.Ident {
				g.returned_var_name = expr0.name
			}
			if !use_tmp_var && !g.is_builtin_mod {
				use_tmp_var = expr0 is ast.CallExpr
			}
		}
		// Create a temporary variable for the return expression
		if use_tmp_var || !g.is_builtin_mod {
			// `return foo(a, b, c)`
			// `tmp := foo(a, b, c); free(a); free(b); free(c); return tmp;`
			// Save return value in a temp var so that all args (a,b,c) can be freed
			// Don't use a tmp var if a variable is simply returned: `return x`
			// Just in case of defer statements exists, that the return values cannot
			// be modified.
			if use_tmp_var {
				use_tmp_var = true
				g.write('${ret_typ} ${tmpvar} = ')
			} else {
				if !g.is_builtin_mod {
					g.autofree_scope_vars(node.pos.pos - 1, node.pos.line_nr, true)
				}
				g.write('return ')
			}
		} else {
			g.autofree_scope_vars(node.pos.pos - 1, node.pos.line_nr, true)
			g.write('return ')
		}
		if expr0.is_auto_deref_var() && !fn_return_is_fixed_array {
			if ret_type.is_ptr() {
				var_str := g.expr_string(expr0)
				g.write(var_str.trim('&'))
			} else if ret_type.has_flag(.option) {
				g.expr_with_opt(expr0, type0, ret_type)
			} else if g.table.sym(ret_type).kind in [.sum_type, .interface] {
				g.expr_with_cast(expr0, type0, ret_type)
			} else {
				g.write('*')
				g.expr(expr0)
			}
		} else {
			if ret_type.has_flag(.option) {
				expr0_is_alias_fn_ret := expr0 is ast.CallExpr && type0.has_flag(.option)
					&& g.table.type_kind(type0) in [.placeholder, .alias]
				// return foo() where foo() returns different option alias than current fn
				if expr0_is_alias_fn_ret {
					g.expr_opt_with_cast(expr0, type0, ret_type)
				} else {
					g.expr_with_opt(expr0, type0, ret_type)
				}
			} else {
				if fn_return_is_fixed_array && !type0.has_option_or_result() {
					if node.exprs[0] is ast.Ident {
						g.writeln('{0};')
						typ := if node.exprs[0].is_auto_deref_var() {
							type0.deref()
						} else {
							type0
						}
						typ_sym := g.table.final_sym(typ)
						if typ_sym.kind == .array_fixed
							&& (typ_sym.info as ast.ArrayFixed).is_fn_ret {
							g.write('memcpy(${tmpvar}.ret_arr, ${g.expr_string(expr0)}.ret_arr, sizeof(${g.styp(typ)}))')
						} else {
							g.write('memcpy(${tmpvar}.ret_arr, ${g.expr_string(expr0)}, sizeof(${g.styp(typ)}))')
						}
					} else if expr0 in [ast.ArrayInit, ast.StructInit] {
						if expr0 is ast.ArrayInit && expr0.is_fixed && expr0.has_init {
							if (expr0 as ast.ArrayInit).init_expr.is_literal() {
								g.write('{.ret_arr=')
								g.expr_with_cast(expr0, type0, ret_type)
								g.writeln('};')
							} else {
								g.writeln('{0};')
								g.write('memcpy(${tmpvar}.ret_arr, ')
								g.expr_with_cast(expr0, type0, ret_type)
								g.write(', sizeof(${g.styp(type0)}))')
							}
						} else {
							g.writeln('{0};')
							tmpvar2 := g.new_tmp_var()
							g.write('${g.styp(type0)} ${tmpvar2} = ')
							g.expr_with_cast(expr0, type0, ret_type)
							g.writeln(';')
							g.write('memcpy(${tmpvar}.ret_arr, ${tmpvar2}, sizeof(${g.styp(type0)}))')
						}
					} else {
						g.writeln('{0};')
						g.write('memcpy(${tmpvar}.ret_arr, ')
						g.expr_with_cast(expr0, type0, ret_type)
						g.write(', sizeof(${g.styp(type0)}))')
					}
				} else {
					g.expr_with_cast(expr0, type0, ret_type)
				}
			}
		}
		if use_tmp_var {
			g.writeln(';')
			has_semicolon = true
			g.write_defer_stmts_when_needed()
			if !g.is_builtin_mod {
				g.autofree_scope_vars(node.pos.pos - 1, node.pos.line_nr, true)
			}
			g.write('return ${tmpvar}')
			has_semicolon = false
		}
	} else {
		println('this should never happen')
		g.write('/*F*/return')
	}
	if !has_semicolon {
		g.writeln(';')
	}
}

// check_expr_is_const checks if the expr is eligible to be used as const initializer on C global scope
fn (mut g Gen) check_expr_is_const(expr ast.Expr) bool {
	match expr {
		ast.StringLiteral, ast.IntegerLiteral, ast.BoolLiteral, ast.FloatLiteral, ast.CharLiteral {
			return true
		}
		ast.ArrayInit {
			return expr.exprs.all(g.check_expr_is_const(it))
		}
		ast.ParExpr {
			return g.check_expr_is_const(expr.expr)
		}
		ast.InfixExpr {
			return g.check_expr_is_const(expr.left) && g.check_expr_is_const(expr.right)
		}
		ast.Ident {
			return expr.kind == .function || g.table.final_sym(expr.obj.typ).kind != .array_fixed
		}
		ast.StructInit, ast.EnumVal {
			return true
		}
		ast.CastExpr {
			return g.check_expr_is_const(expr.expr)
		}
		ast.PrefixExpr {
			return expr.right is ast.Ident || g.check_expr_is_const(expr.right)
		}
		ast.UnsafeExpr {
			return g.check_expr_is_const(expr.expr)
		}
		else {
			return false
		}
	}
}

fn (mut g Gen) assoc(node ast.Assoc) {
	g.writeln('// assoc')
	if node.typ == 0 {
		return
	}
	styp := g.styp(node.typ)
	g.writeln('(${styp}){')
	mut inited_fields := map[string]int{}
	for i, field in node.fields {
		inited_fields[field] = i
	}
	// Merge inited_fields in the rest of the fields.
	sym := g.table.sym(node.typ)
	info := sym.info as ast.Struct
	for field in info.fields {
		field_name := c_name(field.name)
		if field.name in inited_fields {
			g.write('\t.${field_name} = ')
			g.expr(node.exprs[inited_fields[field.name]])
			g.writeln(', ')
		} else {
			g.writeln('\t.${field_name} = ${node.var_name}.${field_name},')
		}
	}
	g.write('}')
	if g.is_amp {
		g.write(', sizeof(${styp}))')
	}
}

@[noreturn]
fn verror(s string) {
	util.verror('cgen error', s)
}

@[noreturn]
fn (g &Gen) error(s string, pos token.Pos) {
	util.show_compiler_message('cgen error:', pos: pos, file_path: g.file.path, message: s)
	exit(1)
}

fn (g &Gen) checker_bug(s string, pos token.Pos) {
	g.error('checker bug; ${s}', pos)
}

// write_debug_calls_typeof_functions inserts calls to all typeof functions for
// interfaces and sum-types in debug mode so that the compiler does not optimize them.
// These functions are needed to be able to get the name of a specific structure/type in the debugger.
fn (mut g Gen) write_debug_calls_typeof_functions() {
	if !g.pref.is_debug {
		return
	}

	g.writeln2('\t// we call these functions in debug mode so that the C compiler', '\t// does not optimize them and we can access them in the debugger.')
	for _, sym in g.table.type_symbols {
		if sym.kind == .sum_type {
			sum_info := sym.info as ast.SumType
			if sum_info.is_generic {
				continue
			}
			if g.pref.skip_unused && sym.idx !in g.table.used_features.used_syms {
				continue
			}
			g.writeln('\tv_typeof_sumtype_${sym.cname}(0);')
		}
		if sym.kind == .interface {
			if sym.info !is ast.Interface {
				continue
			}
			inter_info := sym.info as ast.Interface
			if inter_info.is_generic
				|| (g.pref.skip_unused && sym.idx !in g.table.used_features.used_syms) {
				continue
			}
			g.writeln('\tv_typeof_interface_${sym.cname}(0);')
		}
	}
}

fn (mut g Gen) write_init_function() {
	if g.pref.no_builtin || (g.pref.translated && g.pref.is_o) {
		return
	}
	util.timing_start(@METHOD)
	defer {
		util.timing_measure(@METHOD)
	}

	// Force generate _vinit_caller, _vcleanup_caller , these are needed under Windows,
	// because dl.open() / dl.close() will call them when loading/unloading shared dll.
	if g.pref.is_liveshared && g.pref.os != .windows {
		return
	}

	fn_vinit_start_pos := g.out.len

	// ___argv is declared as voidptr here, because that unifies the windows/unix logic
	g.writeln('void _vinit(int ___argc, voidptr ___argv) {')

	g.write_debug_calls_typeof_functions()

	if g.pref.trace_calls && g.pref.should_trace_fn_name('_vinit') {
		g.writeln('\tv__trace_calls__on_call(_S("_vinit"));')
	}

	if g.use_segfault_handler && !g.pref.is_shared {
		if _ := g.table.fns['v_segmentation_fault_handler'] {
			// 11 is SIGSEGV. It is hardcoded here, to avoid FreeBSD compilation errors for trivial examples.
			// shared object does not need this
			g.writeln('#if __STDC_HOSTED__ == 1\n\tsignal(11, v_segmentation_fault_handler);\n#endif')
		}
	}

	if g.pref.is_bare {
		if _ := g.table.fns['init_global_allocator'] {
			g.writeln('init_global_allocator();')
		}
	}

	if g.pref.prealloc {
		if _ := g.table.fns['prealloc_vinit'] {
			g.writeln('prealloc_vinit();')
		}
	}

	// Note: the as_cast table should be *before* the other constant initialize calls,
	// because it may be needed during const initialization of builtin and during
	// calling module init functions too, just in case they do fail...
	if g.as_cast_type_names.len > 0 {
		g.write('\tas_cast_type_indexes = ')
		g.writeln(g.as_cast_name_table())
	}
	if !g.pref.is_shared && (!g.pref.skip_unused || g.table.used_features.external_types) {
		// shared object does not need this
		if _ := g.table.find_fn('builtin_init') {
			g.writeln('\tbuiltin_init();')
		}
	}

	if g.nr_closures > 0 {
		g.writeln('\t_closure_mtx_init();')
	}

	// reflection bootstrapping
	if g.has_reflection {
		if var := g.global_const_defs['g_reflection'] {
			g.writeln(var.init)
			g.gen_reflection_data()
		}
	}
	mut cleaning_up_array := []string{cap: g.table.modules.len}

	for mod_name in g.table.modules {
		if g.has_reflection && mod_name == 'v.reflection' {
			// ignore v.reflection and v.debug already initialized above
			continue
		}
		mut const_section_header_shown := false
		// write globals and consts init later
		for var_name in g.sorted_global_const_names {
			if var := g.global_const_defs[var_name] {
				if var.mod == mod_name && var.init.len > 0 {
					if !const_section_header_shown {
						g.writeln('\t// Initializations of consts for module ${mod_name}')
						const_section_header_shown = true
					}
					g.writeln(var.init)
				}
			}
		}
		init_fn_name := '${mod_name}.init'
		if initfn := g.table.find_fn(init_fn_name) {
			if initfn.return_type == ast.void_type && initfn.params.len == 0 {
				mut should_be_skipped := false
				if initfn.source_fn != unsafe { nil } {
					fndecl := unsafe { &ast.FnDecl(initfn.source_fn) }
					if fndecl.should_be_skipped {
						should_be_skipped = fndecl.should_be_skipped
					}
				}
				if should_be_skipped {
					g.writeln('\t// Skipping fn init() for module ${mod_name}')
				} else {
					g.writeln('\t// Calling fn init() for module ${mod_name}')
					mod_c_name := util.no_dots(mod_name)
					init_fn_c_name := '${mod_c_name}__init'
					g.writeln('\t${init_fn_c_name}();')
				}
			}
		}
		cleanup_fn_name := '${mod_name}.cleanup'
		if cleanupfn := g.table.find_fn(cleanup_fn_name) {
			if cleanupfn.return_type == ast.void_type && cleanupfn.params.len == 0 {
				mod_c_name := util.no_dots(mod_name)
				cleanup_fn_c_name := '${mod_c_name}__cleanup'
				cleaning_up_array << '\t${cleanup_fn_c_name}();'
				cleaning_up_array << '\t// Cleaning up for module ${mod_name}'
			}
		}
	}

	g.writeln('}')
	if g.pref.printfn_list.len > 0 && '_vinit' in g.pref.printfn_list {
		println(g.out.after(fn_vinit_start_pos))
	}

	fn_vcleanup_start_pos := g.out.len
	g.writeln('void _vcleanup(void) {')
	if g.pref.trace_calls && g.pref.should_trace_fn_name('_vcleanup') {
		g.writeln('\tv__trace_calls__on_call(_S("_vcleanup"));')
	}
	if g.is_autofree {
		// g.writeln('puts("cleaning up...");')
		reversed_table_modules := g.table.modules.reverse()
		for mod_name in reversed_table_modules {
			g.writeln2('\t// Cleanups for module ${mod_name} :', g.cleanups[mod_name].str())
		}
		if g.as_cast_type_names.len > 0 {
			g.writeln('\tarray_free(&as_cast_type_indexes);')
		}
	}
	for x in cleaning_up_array.reverse() {
		g.writeln(x)
	}
	if g.pref.use_coroutines {
		g.writeln('\tdelete_photon_work_pool();')
	}
	if g.pref.is_coverage {
		g.write_coverage_stats()
		g.writeln('\tvprint_coverage_stats();')
	}
	g.writeln('}')
	if g.pref.printfn_list.len > 0 && '_vcleanup' in g.pref.printfn_list {
		println(g.out.after(fn_vcleanup_start_pos))
	}

	if g.pref.is_shared {
		// shared libraries need a way to call _vinit/2. For that purpose,
		// provide a constructor/destructor pair, ensuring that all constants
		// are initialized just once, and that they will be freed too.
		// Note: os.args in this case will be [].
		if g.pref.os != .windows {
			g.writeln('__attribute__ ((constructor))')
		}
		g.export_funcs << '_vinit_caller'
		g.writeln('void _vinit_caller() {')
		g.writeln('\tstatic bool once = false; if (once) {return;} once = true;')
		if g.nr_closures > 0 {
			g.writeln('\t__closure_init(); // vinit_caller()')
		}
		g.writeln('\t_vinit(0,0);')
		g.writeln('}')

		if g.pref.os != .windows {
			g.writeln('__attribute__ ((destructor))')
		}
		g.export_funcs << '_vcleanup_caller'
		g.writeln('void _vcleanup_caller() {')
		g.writeln('\tstatic bool once = false; if (once) {return;} once = true;')
		g.writeln('\t_vcleanup();')
		g.writeln('}')
	}
}

fn (mut g Gen) write_builtin_types() {
	if g.pref.no_builtin {
		return
	}
	mut builtin_types := []&ast.TypeSymbol{} // builtin types
	// builtin types need to be on top
	// everything except builtin will get sorted
	for builtin_name in ast.builtins {
		sym := g.table.sym_by_idx(g.table.type_idxs[builtin_name])
		if g.pref.skip_unused && sym.idx !in g.table.used_features.used_syms {
			continue
		}
		if sym.info is ast.Interface {
			g.write_interface_typedef(sym)
			if !sym.info.is_generic {
				g.write_interface_typesymbol_declaration(sym)
			}
		} else {
			builtin_types << sym
		}
	}
	g.write_types(builtin_types)
}

// C struct definitions, ordered
// Sort the types, make sure types that are referenced by other types
// are added before them.
fn (mut g Gen) write_sorted_types() {
	g.type_definitions.writeln('// #start sorted_symbols')
	defer {
		g.type_definitions.writeln('// #end sorted_symbols')
	}
	unsafe {
		mut symbols := []&ast.TypeSymbol{cap: g.table.type_symbols.len} // structs that need to be sorted
		non_builtin_syms := g.table.type_symbols.filter(!it.is_builtin)
		for sym in non_builtin_syms {
			symbols << sym
		}
		sorted_symbols := g.sort_structs(symbols)
		g.write_types(sorted_symbols)
	}
}

fn (mut g Gen) write_types(symbols []&ast.TypeSymbol) {
	mut struct_names := map[string]bool{}
	for sym in symbols {
		if sym.name.starts_with('C.') {
			continue
		}
		if sym.kind == .none && (!g.pref.skip_unused || g.table.used_features.used_none > 0) {
			g.type_definitions.writeln('struct none {')
			g.type_definitions.writeln('\tEMPTY_STRUCT_DECLARATION;')
			g.type_definitions.writeln('};')
			g.typedefs.writeln('typedef struct none none;')
		}
		mut name := sym.scoped_cname()
		if g.pref.skip_unused && g.table.used_features.used_maps == 0 {
			if name in ['map', 'mapnode', 'SortedMap', 'MapMode', 'DenseArray'] {
				continue
			}
		}
		match sym.info {
			ast.Struct {
				if !struct_names[name] {
					// generate field option types for fixed array of option struct before struct declaration
					opt_fields := sym.info.fields.filter(g.table.final_sym(it.typ).kind == .array_fixed)
					for opt_field in opt_fields {
						field_sym := g.table.final_sym(opt_field.typ)
						arr := field_sym.info as ast.ArrayFixed
						if !arr.elem_type.has_flag(.option) {
							continue
						}
						styp := field_sym.cname
						mut fixed_elem_name := g.styp(arr.elem_type.set_nr_muls(0))
						if arr.elem_type.is_ptr() {
							fixed_elem_name += '*'.repeat(arr.elem_type.nr_muls())
						}
						len := arr.size
						lock g.done_options {
							if styp !in g.done_options {
								g.type_definitions.writeln('typedef ${fixed_elem_name} ${styp} [${len}];')
								g.done_options << styp
							}
						}
					}
					if g.pref.skip_unused && sym.idx !in g.table.used_features.used_syms {
						continue
					}
					g.struct_decl(sym.info, name, false, false)
					struct_names[name] = true
				}
			}
			ast.Thread {
				if !g.pref.is_bare && !g.pref.no_builtin {
					if g.pref.os == .windows {
						if name == '__v_thread' {
							g.thread_definitions.writeln('typedef HANDLE ${name};')
						} else {
							// Windows can only return `u32` (no void*) from a thread, so the
							// V gohandle must maintain a pointer to the return value
							g.thread_definitions.writeln('typedef struct {')
							g.thread_definitions.writeln('\tvoid* ret_ptr;')
							g.thread_definitions.writeln('\tHANDLE handle;')
							g.thread_definitions.writeln('} ${name};')
						}
					} else {
						g.thread_definitions.writeln('typedef pthread_t ${name};')
					}
				}
			}
			ast.SumType {
				if sym.info.is_generic || struct_names[name]
					|| (g.pref.skip_unused && sym.idx !in g.table.used_features.used_syms) {
					continue
				}
				struct_names[name] = true
				g.typedefs.writeln('typedef struct ${name} ${name};')
				mut idxs := []int{}
				if !g.pref.is_prod {
					// Do not print union sum type coment in prod mode
					g.type_definitions.writeln('')
					g.type_definitions.writeln('// Union sum type ${name} = ')
					for variant in sym.info.variants {
						if variant in idxs {
							continue
						}
						g.type_definitions.writeln('//          | ${variant:4d} = ${g.styp(variant.idx_type())}')
						idxs << variant
					}
					idxs.clear()
				}
				g.type_definitions.writeln('struct ${name} {')
				g.type_definitions.writeln('\tunion {')
				for variant in sym.info.variants {
					if variant in idxs {
						continue
					}
					idxs << variant
					variant_sym := g.table.sym(variant)
					mut var := if variant.has_flag(.option) { variant } else { variant.ref() }
					variant_name := g.get_sumtype_variant_name(variant, variant_sym)
					if variant_sym.info is ast.FnType {
						if variant_sym.info.is_anon {
							var = variant
						}
					}
					var_type := if variant.has_flag(.option) {
						'${g.styp(var)}*'
					} else {
						g.styp(var)
					}
					g.type_definitions.writeln('\t\t${var_type} _${variant_name};')
				}
				g.type_definitions.writeln('\t};')
				g.type_definitions.writeln('\tint _typ;')
				if sym.info.fields.len > 0 {
					g.writeln('\t// pointers to common sumtype fields')
					for field in sym.info.fields {
						g.type_definitions.writeln('\t${g.styp(field.typ)}* ${c_name(field.name)};')
					}
				}
				g.type_definitions.writeln('};')
			}
			ast.ArrayFixed {
				elem_sym := g.table.sym(sym.info.elem_type)
				if !elem_sym.is_builtin() && !sym.info.elem_type.has_flag(.generic)
					&& !sym.info.is_fn_ret {
					// .array_fixed {
					styp := sym.cname
					// array_fixed_char_300 => char x[300]
					// [16]&&&EventListener{} => Array_fixed_main__EventListener_16_ptr3
					// => typedef main__EventListener*** Array_fixed_main__EventListener_16_ptr3 [16]
					mut fixed_elem_name := g.styp(sym.info.elem_type.set_nr_muls(0))
					if sym.info.elem_type.is_ptr() {
						fixed_elem_name += '*'.repeat(sym.info.elem_type.nr_muls())
					}
					len := sym.info.size
					if len > 0 {
						if elem_sym.info is ast.FnType {
							pos := g.out.len
							g.write_fn_ptr_decl(&elem_sym.info, '')
							fixed_elem_name = g.out.cut_to(pos)
							mut def_str := 'typedef ${fixed_elem_name};'
							def_str = def_str.replace_once('(*)', '(*${styp}[${len}])')
							g.type_definitions.writeln(def_str)
						} else if elem_sym.info !is ast.ArrayFixed
							|| (elem_sym.info as ast.ArrayFixed).size > 0 {
							// fixed array of option struct must be defined backwards
							if sym.info.elem_type.has_flag(.option) && elem_sym.info is ast.Struct {
								styp_elem, base := g.option_type_name(sym.info.elem_type)
								lock g.done_options {
									if base !in g.done_options {
										g.done_options << base
										g.typedefs.writeln('typedef struct ${styp_elem} ${styp_elem};')
										g.type_definitions.writeln('${g.option_type_text(styp_elem,
											base)};')
									}
								}
								g.type_definitions.writeln('typedef ${fixed_elem_name} ${styp} [${len}];')
							} else if !(elem_sym.info is ast.ArrayFixed && elem_sym.info.is_fn_ret) {
								if g.pref.skip_unused
									&& elem_sym.idx !in g.table.used_features.used_syms {
									continue
								}
								g.type_definitions.writeln('typedef ${fixed_elem_name} ${styp} [${len}];')
							}
						}
					}
				}
			}
			else {}
		}
	}
}

// sort functions by dependent arguments and return value
// As functions may depend on one another, make sure they are
// defined in the correct order: add non dependent ones first.
fn (mut g Gen) write_sorted_fn_typesymbol_declaration() {
	util.timing_start(@METHOD)
	defer {
		util.timing_measure(@METHOD)
	}
	mut syms := []&ast.TypeSymbol{} // functions to be defined
	for sym in g.table.type_symbols {
		if sym.kind == .function && !sym.is_builtin {
			syms << sym
		}
	}
	mut pending := []&ast.TypeSymbol{} // functions with a dependency
	for {
		// Add non dependent functions or functions which
		// dependency has been added.
		next: for sym in syms {
			info := sym.info as ast.FnType
			func := info.func
			return_sym := g.table.sym(func.return_type)
			if return_sym in syms {
				pending << sym
				continue
			}
			for param in func.params {
				param_sym := g.table.sym(param.typ)
				if param_sym in syms {
					pending << sym
					continue next
				}
			}
			g.write_fn_typesymbol_declaration(sym)
		}
		if pending.len == 0 {
			// All functions were added.
			break
		}
		if syms.len == pending.len {
			// Could not add any function: there is a circular
			// dependency.
			mut deps := []string{}
			for sym in pending {
				deps << sym.name
			}
			verror(
				'cgen.write_sorted_fn_typesymbol_declaration(): the following functions form a dependency cycle:\n' +
				deps.join(','))
		}
		unsafe {
			// Swap the to-be-processed and the dependent functions.
			syms, pending = pending, syms[..0]
		}
	}
}

// sort structs by dependent fields
fn (mut g Gen) sort_structs(typesa []&ast.TypeSymbol) []&ast.TypeSymbol {
	util.timing_start(@METHOD)
	defer {
		util.timing_measure(@METHOD)
	}
	mut dep_graph := depgraph.new_dep_graph()
	// types name list
	mut type_names := []string{}
	for sym in typesa {
		type_names << sym.scoped_name()
	}
	// loop over types
	for sym in typesa {
		if sym.kind == .interface {
			dep_graph.add(sym.name, [])
			continue
		}
		// create list of deps
		mut field_deps := []string{}
		match sym.info {
			ast.ArrayFixed {
				mut skip := false
				// allow: `struct Node{ children [4]&Node }`
				// skip adding the struct as a dependency to the fixed array: [4]&Node -> Node
				// the struct must depend on the fixed array for definition order: Node -> [4]&Node
				// NOTE: Is there is a simpler way to do this?
				elem_sym := g.table.final_sym(sym.info.elem_type)
				if elem_sym.info is ast.Struct && sym.info.elem_type.is_ptr() {
					for field in elem_sym.info.fields {
						if sym.idx == field.typ.idx() {
							skip = true
							break
						}
					}
				}
				if !skip {
					dep := g.table.final_sym(sym.info.elem_type).name
					if dep in type_names {
						field_deps << dep
					}
				}
			}
			ast.Struct {
				for embed in sym.info.embeds {
					dep := g.table.sym(embed).name
					// skip if not in types list or already in deps
					if dep !in type_names || dep in field_deps {
						continue
					}
					field_deps << dep
				}
				for field in sym.info.fields {
					if field.typ.is_ptr() {
						continue
					}
					fsym := g.table.sym(field.typ)
					dep := fsym.name
					// skip if not in types list or already in deps
					if dep !in type_names || dep in field_deps {
						continue
					}
					field_deps << dep
					if fsym.info is ast.Alias {
						xdep := g.table.sym(fsym.info.parent_type).name
						if xdep !in type_names || xdep in field_deps {
							continue
						}
						field_deps << xdep
					}
				}
			}
			ast.SumType {
				for variant in sym.info.variants {
					vsym := g.table.sym(variant)
					if vsym.info !is ast.Struct {
						continue
					}
					fields := g.table.struct_fields(vsym)
					for field in fields {
						if field.typ.is_ptr() {
							continue
						}
						fsym := g.table.sym(field.typ)
						if fsym.info is ast.Alias {
							xsym := g.table.sym(fsym.info.parent_type)
							if xsym.info !is ast.ArrayFixed {
								continue
							}
							xdep := xsym.name
							// skip if not in types list or already in deps
							if xdep !in type_names || xdep in field_deps {
								continue
							}
							field_deps << xdep
							continue
						}
						if fsym.info !is ast.ArrayFixed {
							continue
						}
						dep := fsym.name
						// skip if not in types list or already in deps
						if dep !in type_names || dep in field_deps {
							continue
						}
						field_deps << dep
					}
				}
			}
			else {}
		}
		// add type and dependent types to graph
		dep_graph.add(sym.scoped_name(), field_deps)
	}
	// sort graph
	dep_graph_sorted := dep_graph.resolve()
	if !dep_graph_sorted.acyclic {
		// this should no longer be called since it's in the parser
		// TODO: should it be removed?
		verror('cgen.sort_structs(): the following structs form a dependency cycle:\n' +
			dep_graph_sorted.display_cycles() +
			'\nyou can solve this by making one or both of the dependent struct fields references, eg: field &MyStruct' +
			'\nif you feel this is an error, please create a new issue here: https://github.com/vlang/v/issues and tag @joe-conigliaro')
	}
	// sort types
	unsafe {
		mut sorted_symbols := []&ast.TypeSymbol{cap: dep_graph_sorted.nodes.len}
		for node in dep_graph_sorted.nodes {
			sorted_symbols << g.table.sym_by_idx(g.table.type_idxs[node.name])
		}
		return sorted_symbols
	}
}

fn (mut g Gen) gen_or_block_stmts(cvar_name string, cast_typ string, stmts []ast.Stmt, return_type ast.Type,
	is_option bool) {
	g.indent++
	for i, stmt in stmts {
		if i == stmts.len - 1 {
			expr_stmt := stmt as ast.ExprStmt
			g.set_current_pos_as_last_stmt_pos()
			if g.inside_return && (expr_stmt.typ.idx() == ast.error_type_idx
				|| expr_stmt.typ in [ast.none_type, ast.error_type]) {
				// `return foo() or { error('failed') }`
				if g.cur_fn != unsafe { nil } {
					if g.cur_fn.return_type.has_flag(.result) {
						g.write('return ')
						g.gen_result_error(g.cur_fn.return_type, expr_stmt.expr)
						g.writeln(';')
					} else if g.cur_fn.return_type.has_flag(.option) {
						g.write('return ')
						g.gen_option_error(g.cur_fn.return_type, expr_stmt.expr)
						g.writeln(';')
					}
				}
			} else {
				if expr_stmt.typ == ast.none_type_idx {
					g.write('${cvar_name} = ')
					g.gen_option_error(return_type, expr_stmt.expr)
					g.writeln(';')
				} else if return_type == ast.rvoid_type {
					// fn returns !, do not fill var.data
					old_inside_opt_data := g.inside_opt_data
					g.inside_opt_data = true
					g.expr(expr_stmt.expr)
					g.inside_opt_data = old_inside_opt_data
					g.writeln(';')
					g.stmt_path_pos.delete_last()
				} else {
					mut is_array_fixed := false
					mut return_wrapped := false
					mut return_is_option := is_option && return_type.has_option_or_result()
					tmp_op := if cvar_name in g.tmp_var_ptr { '->' } else { '.' }
					if is_option {
						is_array_fixed = g.table.final_sym(return_type).kind == .array_fixed
						if !is_array_fixed {
							if g.inside_return && !g.inside_struct_init
								&& expr_stmt.expr is ast.CallExpr&& (expr_stmt.expr as ast.CallExpr).return_type.has_option_or_result()
								&& g.cur_fn.return_type.has_option_or_result() && return_is_option
								&& expr_stmt.expr.or_block.kind == .absent {
								g.write('${cvar_name} = ')
								return_wrapped = true
							} else if expr_stmt.expr is ast.CallExpr {
								if expr_stmt.expr.is_return_used {
									g.write('*(${cast_typ}*) ${cvar_name}${tmp_op}data = ')
								}
							} else if g.inside_opt_or_res && return_is_option && g.inside_assign {
								g.write('_option_ok(&(${cast_typ}[]) { ')
								g.expr_with_cast(expr_stmt.expr, expr_stmt.typ, return_type.clear_option_and_result())
								g.writeln(' }, (${option_name}*)&${cvar_name}, sizeof(${cast_typ}));')
								g.indent--
								return
							} else {
								g.write('*(${cast_typ}*) ${cvar_name}${tmp_op}data = ')
							}
						}
					} else {
						g.write('${cvar_name} = ')
					}
					if is_array_fixed {
						g.write('memcpy(${cvar_name}${tmp_op}data, (${cast_typ})')
					}
					// return expr or { fn_returns_option() }
					if is_option && g.inside_return && expr_stmt.expr is ast.CallExpr
						&& return_is_option {
						g.expr_with_cast(expr_stmt.expr, expr_stmt.typ, return_type)
					} else {
						old_inside_opt_data := g.inside_opt_data
						g.inside_opt_data = true
						g.expr_with_cast(expr_stmt.expr, expr_stmt.typ, return_type.clear_option_and_result())
						g.inside_opt_data = old_inside_opt_data
					}
					if is_array_fixed {
						g.write(', sizeof(${cast_typ}))')
					}
					g.writeln(';')
					g.stmt_path_pos.delete_last()
					if return_wrapped {
						g.writeln('return ${cvar_name};')
					}
				}
			}
		} else {
			g.stmt(stmt)
		}
	}
	g.indent--
}

// If user is accessing the return value eg. in assignment, pass the variable name.
// If the user is not using the option return value. We need to pass a temp var
// to access its fields (`.ok`, `.error` etc)
// `os.cp(...)` => `Option bool tmp = os__cp(...); if (tmp.state != 0) { ... }`
// Returns the type of the last stmt
fn (mut g Gen) or_block(var_name string, or_block ast.OrExpr, return_type ast.Type) {
	cvar_name := c_name(var_name)
	tmp_op := if var_name in g.tmp_var_ptr || return_type.has_flag(.option_mut_param_t) {
		'->'
	} else {
		'.'
	}
	if or_block.kind == .block && or_block.stmts.len == 0 {
		// generate nothing, block is empty
		g.write(';\n${util.tabs(g.indent)}(void)${cvar_name};')
		return
	}
	mut mr_styp := g.base_type(return_type)
	is_none_ok := return_type == ast.ovoid_type
	g.writeln(';')
	if is_none_ok {
		g.writeln('if (${cvar_name}${tmp_op}state != 0) {')
	} else {
		if return_type != 0 && g.table.sym(return_type).kind == .function {
			mr_styp = 'voidptr'
		}
		if return_type.has_flag(.result) {
			g.writeln('if (${cvar_name}${tmp_op}is_error) {')
		} else {
			g.writeln('if (${cvar_name}${tmp_op}state != 0) {')
		}
	}
	if or_block.kind == .block {
		g.or_expr_return_type = return_type.clear_option_and_result()
		g.writeln('\tIError err = ${cvar_name}${tmp_op}err;')

		g.inside_or_block = true
		defer {
			g.inside_or_block = false
		}
		stmts := or_block.stmts
		if stmts.len > 0 && stmts.last() is ast.ExprStmt && stmts.last().typ != ast.void_type {
			g.gen_or_block_stmts(cvar_name, mr_styp, stmts, return_type, true)
		} else {
			g.stmts(stmts)
			if stmts.len > 0 && stmts.last() is ast.ExprStmt {
				g.writeln(';')
			}
		}
		g.or_expr_return_type = ast.void_type
	} else if or_block.kind == .propagate_result
		|| (or_block.kind == .propagate_option && return_type.has_flag(.result)) {
		if g.file.mod.name == 'main' && (g.fn_decl == unsafe { nil } || g.fn_decl.is_main) {
			// In main(), an `opt()!` call is sugar for `opt() or { panic(err) }`
			err_msg := 'IError_name_table[${cvar_name}${tmp_op}err._typ]._method_msg(${cvar_name}${tmp_op}err._object)'
			if g.pref.is_debug {
				paline, pafile, pamod, pafn := g.panic_debug_info(or_block.pos)
				g.writeln('panic_debug(${paline}, tos3("${pafile}"), tos3("${pamod}"), tos3("${pafn}"), ${err_msg});')
			} else {
				g.writeln('\tpanic_result_not_set(${err_msg});')
			}
		} else if g.fn_decl != unsafe { nil } && g.fn_decl.is_test {
			g.gen_failing_error_propagation_for_test_fn(or_block, cvar_name)
		} else {
			// In ordinary functions, `opt()!` call is sugar for:
			// `opt() or { return err }`
			// Since we *do* return, first we have to ensure that
			// the deferred statements are generated.
			g.write_defer_stmts()
			// Now that option types are distinct we need a cast here
			if g.fn_decl == unsafe { nil } || g.fn_decl.return_type == ast.void_type {
				g.writeln('\treturn;')
			} else {
				styp := g.styp(g.fn_decl.return_type)
				err_obj := g.new_tmp_var()
				g.writeln('\t${styp} ${err_obj} = {0};')
				if g.fn_decl.return_type.has_flag(.result) {
					g.writeln('\t${err_obj}.is_error = true;')
				} else if g.fn_decl.return_type.has_flag(.option) {
					g.writeln('\t${err_obj}.state = 2;')
				}
				g.writeln('\t${err_obj}.err = ${cvar_name}${tmp_op}err;')
				g.writeln('\treturn ${err_obj};')
			}
		}
	} else if or_block.kind == .propagate_option {
		if g.file.mod.name == 'main' && (g.fn_decl == unsafe { nil } || g.fn_decl.is_main) {
			// In main(), an `opt()?` call is sugar for `opt() or { panic(err) }`
			err_msg := 'IError_name_table[${cvar_name}${tmp_op}err._typ]._method_msg(${cvar_name}${tmp_op}err._object)'
			if g.pref.is_debug {
				paline, pafile, pamod, pafn := g.panic_debug_info(or_block.pos)
				g.writeln('panic_debug(${paline}, tos3("${pafile}"), tos3("${pamod}"), tos3("${pafn}"), ${err_msg}.len == 0 ? _S("option not set ()") : ${err_msg});')
			} else {
				g.writeln('\tpanic_option_not_set( ${err_msg} );')
			}
		} else if g.fn_decl != unsafe { nil } && g.fn_decl.is_test {
			g.gen_failing_error_propagation_for_test_fn(or_block, cvar_name)
		} else {
			// In ordinary functions, `opt()?` call is sugar for:
			// `opt() or { return err }`
			// Since we *do* return, first we have to ensure that
			// the deferred statements are generated.
			g.write_defer_stmts()
			// Now that option types are distinct we need a cast here
			if g.fn_decl == unsafe { nil } || g.fn_decl.return_type == ast.void_type {
				g.writeln('\treturn;')
			} else if g.fn_decl.return_type.clear_option_and_result() == return_type.clear_option_and_result() {
				styp := g.styp(g.fn_decl.return_type).replace('*', '_ptr')
				err_obj := g.new_tmp_var()
				g.writeln2('\t${styp} ${err_obj};', '\tmemcpy(&${err_obj}, &${cvar_name}, sizeof(_option));')
				g.writeln('\treturn ${err_obj};')
			} else {
				g.write('\treturn ')
				g.gen_option_error(g.fn_decl.return_type, ast.None{})
				g.writeln(';')
			}
		}
	}
	g.writeln('}')
	g.set_current_pos_as_last_stmt_pos()
}

@[inline]
fn c_name(name_ string) string {
	name := util.no_dots(name_)
	if c_reserved_chk.matches(name) {
		return '__v_${name}'
	}
	return name
}

@[inline]
fn c_fn_name(name_ string) string {
	name := util.no_dots(name_)
	if c_reserved_chk.matches(name) {
		return '_v_${name}'
	}
	return name
}

fn (mut g Gen) type_default_sumtype(typ_ ast.Type, sym ast.TypeSymbol) string {
	if typ_.has_flag(.option) {
		return '(${g.styp(typ_)}){.state=2, .err=_const_none__, .data={E_STRUCT}}'
	}
	first_typ := g.unwrap_generic((sym.info as ast.SumType).variants[0])
	first_sym := g.table.sym(first_typ)
	first_styp := g.styp(first_typ)
	first_field := g.get_sumtype_variant_name(first_typ, first_sym)
	default_str := if first_typ.has_flag(.option) {
		'(${first_styp}){.state=2, .err=_const_none__, .data={E_STRUCT}}'
	} else if first_sym.info is ast.Struct && first_sym.info.is_empty_struct() {
		'{E_STRUCT}'
	} else {
		g.type_default_no_sumtype(first_typ)
	}
	if default_str[0] == `{` {
		return '(${g.styp(typ_)}){._${first_field}=HEAP(${first_styp}, ((${first_styp})${default_str})),._typ=${int(first_typ)}}'
	} else {
		return '(${g.styp(typ_)}){._${first_field}=HEAP(${first_styp}, (${default_str})),._typ=${int(first_typ)}}'
	}
}

@[inline]
fn (mut g Gen) type_default_no_sumtype(typ_ ast.Type) string {
	return g.type_default_impl(typ_, false)
}

@[inline]
fn (mut g Gen) type_default(typ_ ast.Type) string {
	return g.type_default_impl(typ_, true)
}

fn (mut g Gen) type_default_impl(typ_ ast.Type, decode_sumtype bool) string {
	g.type_default_impl_level++
	defer {
		g.type_default_impl_level--
	}
	if g.type_default_impl_level > 37 {
		eprintln('>>> Gen.type_default_impl g.type_default_impl_level: ${g.type_default_impl_level} | typ_: ${typ_} | decode_sumtype: ${decode_sumtype}')
	}
	if g.type_default_impl_level > 40 {
		verror('reached maximum levels of nesting for ${@LOCATION}')
	}
	typ := g.unwrap_generic(typ_)
	if typ.has_flag(.option) {
		return '(${g.styp(typ)}){.state=2, .err=_const_none__, .data={E_STRUCT}}'
	}
	if typ.has_flag(.result) {
		return '{0}'
	}
	// Always set pointers to 0
	if typ.is_ptr() && !typ.has_flag(.shared_f) {
		return '0'
	}
	if typ.idx() < ast.string_type_idx {
		// Default values for other types are not needed because of mandatory initialization
		return '0'
	}
	sym := g.table.sym(typ)
	match sym.kind {
		.string {
			return '(string){.str=(byteptr)"", .is_lit=1}'
		}
		.array_fixed {
			if sym.is_empty_struct_array() {
				return '{E_STRUCT}'
			}
			return '{0}'
		}
		.sum_type {
			return if decode_sumtype { g.type_default_sumtype(typ, sym) } else { '{0}' }
		}
		.interface, .multi_return, .thread {
			return '{0}'
		}
		.alias {
			return g.type_default((sym.info as ast.Alias).parent_type)
		}
		.chan {
			elem_type := sym.chan_info().elem_type
			elemtypstr := g.styp(elem_type)
			noscan := g.check_noscan(elem_type)
			return 'sync__new_channel_st${noscan}(0, sizeof(${elemtypstr}))'
		}
		.array {
			elem_typ := sym.array_info().elem_type
			elem_sym := g.styp(elem_typ)
			mut elem_type_str := util.no_dots(elem_sym)
			if elem_type_str.starts_with('C__') {
				elem_type_str = elem_type_str[3..]
			}
			noscan := g.check_noscan(elem_typ)
			init_str := '__new_array${noscan}(0, 0, sizeof(${elem_type_str}))'
			if typ.has_flag(.shared_f) {
				atyp := '__shared__${sym.cname}'
				return '(${atyp}*)__dup_shared_array(&(${atyp}){.mtx = {0}, .val =${init_str}}, sizeof(${atyp}))'
			} else {
				return init_str
			}
		}
		.map {
			info := sym.map_info()
			key_typ := g.table.sym(info.key_type)
			hash_fn, key_eq_fn, clone_fn, free_fn := g.map_fn_ptrs(key_typ)
			noscan_key := g.check_noscan(info.key_type)
			noscan_value := g.check_noscan(info.value_type)
			mut noscan := if noscan_key.len != 0 || noscan_value.len != 0 { '_noscan' } else { '' }
			if noscan.len != 0 {
				if noscan_key.len != 0 {
					noscan += '_key'
				}
				if noscan_value.len != 0 {
					noscan += '_value'
				}
			}
			init_str := 'new_map${noscan}(sizeof(${g.styp(info.key_type)}), sizeof(${g.styp(info.value_type)}), ${hash_fn}, ${key_eq_fn}, ${clone_fn}, ${free_fn})'
			if typ.has_flag(.shared_f) {
				mtyp := '__shared__Map_${key_typ.cname}_${g.styp(info.value_type).replace('*',
					'_ptr')}'
				return '(${mtyp}*)__dup_shared_map(&(${mtyp}){.mtx = {0}, .val =${init_str}}, sizeof(${mtyp}))'
			} else {
				return init_str
			}
		}
		.struct {
			mut has_none_zero := false
			info := sym.info as ast.Struct
			mut init_str := if info.is_anon && !g.inside_global_decl {
				'(${g.styp(typ)}){'
			} else {
				'{'
			}
			$if windows {
				if !typ.has_flag(.shared_f) && g.inside_global_decl {
					init_str = '(${g.styp(typ)}){'
				}
			}
			if sym.language in [.c, .v] {
				for field in info.fields {
					field_sym := g.table.sym(field.typ)
					is_option := field.typ.has_flag(.option)
					if is_option || field.has_default_expr
						|| field_sym.kind in [.enum, .array_fixed, .array, .map, .string, .bool, .alias, .i8, .i16, .int, .i64, .u8, .u16, .u32, .u64, .f32, .f64, .char, .voidptr, .byteptr, .charptr, .struct, .chan, .sum_type] {
						if sym.language == .c && !field.has_default_expr && !is_option {
							continue
						}
						field_name := c_name(field.name)
						if field.has_default_expr {
							mut expr_str := ''
							if field_sym.kind in [.sum_type, .interface] {
								expr_str = g.expr_string_with_cast(field.default_expr,
									field.default_expr_typ, field.typ)
							} else if field_sym.is_array_fixed() && g.inside_global_decl {
								array_info := field_sym.array_fixed_info()
								match field.default_expr {
									ast.CallExpr {
										ret_typ := g.styp(field.default_expr.return_type)
										tmp_var := g.new_tmp_var()
										g.type_default_vars.writeln('${ret_typ} ${tmp_var} = {0};')
										g.type_default_vars.writeln('memcpy(${tmp_var}, ${g.expr_string(field.default_expr)}, sizeof(${ret_typ}));')
										expr_str += '{'
										for i in 0 .. array_info.size {
											expr_str += '${tmp_var}[${i}]'
											if i != array_info.size - 1 {
												expr_str += ', '
											}
										}
										expr_str += '}'
									}
									ast.ArrayInit {
										ret_typ := g.styp(field.default_expr.typ)
										tmp_var := g.new_tmp_var()
										g.type_default_vars.writeln('${ret_typ} ${tmp_var} = ${g.expr_string(field.default_expr)};')
										expr_str += '{'
										for i in 0 .. array_info.size {
											expr_str += '${tmp_var}[${i}]'
											if i != array_info.size - 1 {
												expr_str += ', '
											}
										}
										expr_str += '}'
									}
									else {
										expr_str = g.expr_string(field.default_expr)
									}
								}
							} else {
								default_str := g.expr_string_opt(field.typ, field.default_expr)
								if default_str.count(';\n') > 1 {
									g.type_default_vars.writeln(default_str.all_before_last('\n'))
									expr_str = default_str.all_after_last('\n')
								} else {
									expr_str = default_str
								}
							}
							init_str += '.${field_name} = ${expr_str},'
						} else {
							zero_str := if field_sym.language == .v && field_sym.info is ast.Struct
								&& field_sym.info.is_empty_struct() {
								'{E_STRUCT}'
							} else if field_sym.kind == .sum_type {
								if decode_sumtype {
									g.type_default_sumtype(field.typ, field_sym)
								} else {
									'{0}'
								}
							} else {
								g.type_default(field.typ)
							}
							init_str += '.${field_name} = ${zero_str},'
						}
						has_none_zero = true
					}
				}
			}
			typ_is_shared_f := typ.has_flag(.shared_f)
			if has_none_zero {
				init_str += '}'
				if !typ_is_shared_f {
					type_name := if info.is_anon || g.inside_global_decl {
						// No name needed for anon structs, C figures it out on its own.
						''
					} else {
						'(${g.styp(typ)})'
					}
					init_str = type_name + init_str
				}
			} else {
				init_str += '0}'
			}
			if typ_is_shared_f {
				styp := '__shared__${g.table.sym(typ).cname}'
				return '(${styp}*)__dup${styp}(&(${styp}){.mtx = {0}, .val = ${init_str}}, sizeof(${styp}))'
			} else {
				return init_str
			}
		}
		.enum {
			// returns the enum's first value
			if enum_decl := g.table.enum_decls[sym.name] {
				return if enum_decl.fields[0].expr is ast.EmptyExpr {
					'0'
				} else {
					g.expr_string(enum_decl.fields[0].expr)
				}
			} else {
				return '0'
			}
		}
		else {
			return '0'
		}
	}
}

fn (g &Gen) get_all_test_function_names() []string {
	mut tfuncs := []string{}
	mut tsuite_begin := ''
	mut tsuite_end := ''
	for name in g.test_function_names {
		if name.ends_with('.testsuite_begin') {
			tsuite_begin = name
			continue
		}
		if name.contains('.test_') {
			tfuncs << name
			continue
		}
		if name.ends_with('.testsuite_end') {
			tsuite_end = name
			continue
		}
	}
	mut all_tfuncs := []string{}
	if tsuite_begin != '' {
		all_tfuncs << tsuite_begin
	}
	all_tfuncs << tfuncs
	if tsuite_end != '' {
		all_tfuncs << tsuite_end
	}
	return all_tfuncs
}

@[inline]
fn (mut g Gen) get_type(typ ast.Type) ast.Type {
	return if typ == g.field_data_type { g.comptime.comptime_for_field_value.typ } else { typ }
}

fn (mut g Gen) size_of(node ast.SizeOf) {
	typ := g.type_resolver.typeof_type(node.expr, g.get_type(node.typ))
	node_typ := g.unwrap_generic(typ)
	sym := g.table.sym(node_typ)
	if sym.language == .v && sym.kind in [.placeholder, .any] {
		g.error('unknown type `${sym.name}`', node.pos)
	}
	if node.expr is ast.StringLiteral {
		if node.expr.language == .c {
			g.write('sizeof("${node.expr.val}")')
			return
		}
	}
	styp := g.styp(node_typ)
	g.write('sizeof(${util.no_dots(styp)})')
}

@[inline]
fn (mut g Gen) gen_enum_prefix(typ ast.Type) string {
	if g.pref.translated && typ.is_number() {
		// Mostly in translated code, when C enums are used as ints in switches
		return '_const_main__'
	} else {
		styp := g.styp(g.table.unaliased_type(typ))
		return '${styp}__'
	}
}

fn (mut g Gen) enum_val(node ast.EnumVal) {
	g.write('${g.gen_enum_prefix(node.typ)}${node.val}')
}

fn (mut g Gen) as_cast(node ast.AsCast) {
	// Make sure the sum type can be cast to this type (the types
	// are the same), otherwise panic.
	unwrapped_node_typ := g.unwrap_generic(node.typ)
	styp := g.styp(unwrapped_node_typ)
	sym := g.table.sym(unwrapped_node_typ)
	mut expr_type_sym := g.table.sym(g.unwrap_generic(node.expr_type))
	if mut expr_type_sym.info is ast.SumType {
		dot := if node.expr_type.is_ptr() { '->' } else { '.' }
		if node.expr.has_fn_call() && !g.is_cc_msvc {
			tmp_var := g.new_tmp_var()
			expr_styp := g.styp(node.expr_type)
			g.write('({ ${expr_styp} ${tmp_var} = ')
			g.expr(node.expr)
			g.write('; ')
			if sym.info is ast.FnType {
				g.write('(${styp})__as_cast(')
			} else if g.inside_smartcast {
				g.write('(${styp}*)__as_cast(')
			} else {
				g.write('*(${styp}*)__as_cast(')
			}
			g.write2(tmp_var, dot)
			g.write2('_${sym.cname},', tmp_var)
			g.write(dot)
			sidx := g.type_sidx(unwrapped_node_typ)
			g.write('_typ, ${sidx}); })')
		} else {
			if sym.info is ast.FnType {
				g.write('(${styp})__as_cast(')
			} else if g.inside_smartcast {
				g.write('(${styp}*)__as_cast(')
			} else {
				g.write('*(${styp}*)__as_cast(')
			}
			g.write('(')
			g.expr(node.expr)
			g.write2(')', dot)
			g.write2('_${sym.cname},', '(')
			g.expr(node.expr)
			g.write2(')', dot)
			sidx := g.type_sidx(unwrapped_node_typ)
			g.write('_typ, ${sidx})')
		}

		// fill as cast name table
		for variant in expr_type_sym.info.variants {
			idx := u32(variant).str()
			if idx in g.as_cast_type_names {
				continue
			}
			variant_sym := g.table.sym(variant)
			g.as_cast_type_names[idx] = variant_sym.name
		}
	} else if expr_type_sym.kind == .interface && sym.kind == .interface {
		g.write('I_${expr_type_sym.cname}_as_I_${sym.cname}(')
		if node.expr_type.is_ptr() {
			g.write('*')
		}
		g.expr(node.expr)
		g.write(')')

		mut info := expr_type_sym.info as ast.Interface
		lock info.conversions {
			if node.typ !in info.conversions {
				left_variants := g.table.iface_types[expr_type_sym.name]
				right_variants := g.table.iface_types[sym.name]
				info.conversions[node.typ] = left_variants.filter(it in right_variants)
			}
		}
		expr_type_sym.info = info
	} else if mut expr_type_sym.info is ast.Interface && node.expr_type != node.typ {
		dot := if node.expr_type.is_ptr() { '->' } else { '.' }
		if node.expr.has_fn_call() && !g.is_cc_msvc {
			tmp_var := g.new_tmp_var()
			expr_styp := g.styp(node.expr_type)
			g.write('({ ${expr_styp} ${tmp_var} = ')
			g.expr(node.expr)
			g.write('; ')
			if sym.info is ast.FnType {
				g.write('(${styp})__as_cast(')
			} else if g.inside_smartcast {
				g.write('(${styp}*)__as_cast(')
			} else {
				g.write('*(${styp}*)__as_cast(')
			}
			g.write2(tmp_var, dot)
			g.write('_${sym.cname},v_typeof_interface_idx_${expr_type_sym.cname}(')
			g.write2(tmp_var, dot)
			sidx := g.type_sidx(unwrapped_node_typ)
			g.write('_typ), ${sidx}); })')
		} else {
			if sym.info is ast.FnType {
				g.write('(${styp})__as_cast(')
			} else if g.inside_smartcast {
				g.write('(${styp}*)__as_cast(')
			} else {
				g.write('*(${styp}*)__as_cast(')
			}
			g.write('(')
			g.expr(node.expr)
			g.write2(')', dot)
			g.write2('_${sym.cname},v_typeof_interface_idx_${expr_type_sym.cname}(', '(')
			g.expr(node.expr)
			g.write2(')', dot)
			sidx := g.type_sidx(unwrapped_node_typ)
			g.write('_typ), ${sidx})')
		}

		// fill as cast name table
		for typ in expr_type_sym.info.types {
			idx := u32(typ).str()
			if idx in g.as_cast_type_names {
				continue
			}
			variant_sym := g.table.sym(typ)
			g.as_cast_type_names[idx] = variant_sym.name
		}
	} else {
		mut is_optional_ident_var := false
		if g.inside_smartcast {
			g.write('&')
		}
		if node.expr is ast.Ident {
			if node.expr.info is ast.IdentVar && node.expr.info.is_option
				&& !unwrapped_node_typ.has_flag(.option) {
				g.unwrap_option_type(unwrapped_node_typ, node.expr.name, node.expr.is_auto_heap())
				is_optional_ident_var = true
			}
		} else if node.expr is ast.SelectorExpr {
			if node.expr.expr is ast.Ident && node.expr.typ.has_flag(.option)
				&& !unwrapped_node_typ.has_flag(.option) {
				g.unwrap_option_type(node.expr.typ, '${node.expr.expr.name}.${node.expr.field_name}',
					node.expr.expr.is_auto_heap())
				is_optional_ident_var = true
			} else if node.expr.expr is ast.SelectorExpr && node.expr.typ.has_flag(.option) {
				if node.expr.expr.expr is ast.Ident {
					g.unwrap_option_type(node.expr.typ, '${node.expr.expr.expr.name}.${node.expr.expr.field_name}.${node.expr.field_name}',
						node.expr.expr.expr.is_auto_heap())
					is_optional_ident_var = true
				}
			}
		}
		if !is_optional_ident_var {
			g.expr(node.expr)
		}
	}
}

fn (g &Gen) as_cast_name_table() string {
	if g.as_cast_type_names.len == 0 {
		return 'new_array_from_c_array(1, 1, sizeof(VCastTypeIndexName), _MOV((VCastTypeIndexName[1]){(VCastTypeIndexName){.tindex = 0,.tname = _S("unknown")}}));\n'
	}
	mut name_ast := strings.new_builder(1024)
	casts_len := g.as_cast_type_names.len + 1
	name_ast.writeln('new_array_from_c_array(${casts_len}, ${casts_len}, sizeof(VCastTypeIndexName), _MOV((VCastTypeIndexName[${casts_len}]){')
	name_ast.writeln('\t\t  (VCastTypeIndexName){.tindex = 0, .tname = _S("unknown")}')
	for key, value in g.as_cast_type_names {
		name_ast.writeln('\t\t, (VCastTypeIndexName){.tindex = ${key}, .tname = _S("${value}")}')
	}
	name_ast.writeln('\t}));\n')
	return name_ast.str()
}

fn (g &Gen) has_been_referenced(fn_name string) bool {
	mut referenced := false
	lock g.referenced_fns {
		referenced = g.referenced_fns[fn_name]
	}
	return referenced
}

fn (mut g Gen) register_iface_return_types() {
	interfaces := g.table.type_symbols.filter(it.kind == .interface && it.info is ast.Interface)
	for isym in interfaces {
		inter_info := isym.info as ast.Interface
		if inter_info.is_generic {
			continue
		}
		for _, method_name in inter_info.get_methods() {
			method := isym.find_method_with_generic_parent(method_name) or { continue }
			if method.return_type.has_flag(.result) {
				if g.pref.skip_unused
					&& g.table.sym(method.return_type).idx !in g.table.used_features.used_syms {
					continue
				}
				g.register_result(method.return_type)
			}
		}
	}
}

// Generates interface table and interface indexes
fn (mut g Gen) interface_table() string {
	util.timing_start(@METHOD)
	defer {
		util.timing_measure(@METHOD)
	}
	mut sb := strings.new_builder(100)
	mut conversion_functions := strings.new_builder(100)
	interfaces := g.table.type_symbols.filter(it.kind == .interface && it.info is ast.Interface)
	for isym in interfaces {
		inter_info := isym.info as ast.Interface
		if inter_info.is_generic {
			continue
		}
		if g.pref.skip_unused && isym.idx !in g.table.used_features.used_syms {
			continue
		}
		// interface_name is for example Speaker
		interface_name := isym.cname
		// generate a struct that references interface methods
		methods_struct_name := 'struct _${interface_name}_interface_methods'
		mut methods_struct_def := strings.new_builder(100)
		methods_struct_def.writeln('${methods_struct_name} {')
		mut inter_methods := inter_info.get_methods()
		inter_methods.sort(a < b)
		mut methodidx := map[string]int{}
		for k, method_name in inter_methods {
			method := isym.find_method_with_generic_parent(method_name) or { continue }
			methodidx[method.name] = k
			ret_styp := if g.pref.skip_unused
				&& g.table.sym(method.return_type).idx !in g.table.used_features.used_syms {
				'void'
			} else {
				g.ret_styp(method.return_type)
			}
			methods_struct_def.write_string('\t${ret_styp} (*_method_${c_fn_name(method.name)})(void* _')
			// the first param is the receiver, it's handled by `void*` above
			for i in 1 .. method.params.len {
				arg := method.params[i]
				methods_struct_def.write_string(', ${g.styp(arg.typ)} ${arg.name}')
			}
			// TODO: g.fn_args(method.args[1..])
			methods_struct_def.writeln(');')
		}
		methods_struct_def.writeln('};')
		// generate an array of the interface methods for the structs using the interface
		// as well as case functions from the struct to the interface
		mut methods_struct := strings.new_builder(100)
		//
		iname_table_length := inter_info.types.len
		if iname_table_length == 0 {
			// msvc can not process `static struct x[0] = {};`
			methods_struct.writeln('${g.static_modifier}${methods_struct_name} ${interface_name}_name_table[1];')
		} else {
			if g.pref.build_mode != .build_module {
				methods_struct.writeln('${g.static_modifier}${methods_struct_name} ${interface_name}_name_table[${iname_table_length}] = {')
			} else {
				methods_struct.writeln('${g.static_modifier}${methods_struct_name} ${interface_name}_name_table[${iname_table_length}];')
			}
		}
		mut cast_functions := strings.new_builder(100)
		mut methods_wrapper := strings.new_builder(100)
		if !g.pref.is_prod {
			methods_wrapper.writeln('// Methods wrapper for interface "${interface_name}"')
		}
		mut already_generated_mwrappers := map[string]int{}
		iinidx_minimum_base := 1000 // Note: NOT 0, to avoid map entries set to 0 later, so `if already_generated_mwrappers[name] > 0 {` works.
		mut current_iinidx := iinidx_minimum_base
		for st in inter_info.types {
			st_sym_info := g.table.sym(st)
			if st_sym_info.info is ast.Struct && st_sym_info.info.is_unresolved_generic() {
				continue
			}
			st_sym := g.table.sym(ast.mktyp(st))
			// cctype is the Cleaned Concrete Type name, *without ptr*,
			// i.e. cctype is always just Cat, not Cat_ptr:
			cctype := g.cc_type(ast.mktyp(st), true)
			cctype2 := if g.pref.skip_unused && st_sym_info.idx !in g.table.used_features.used_syms {
				'voidptr'
			} else {
				cctype
			}
			cctype_param := if cctype == cctype2 { cctype } else { 'void' }
			$if debug_interface_table ? {
				eprintln('>> interface name: ${isym.name} | concrete type: ${st.debug()} | st symname: ${st_sym.name}')
			}
			// Speaker_Cat_index = 0
			interface_index_name := '_${interface_name}_${cctype}_index'
			if already_generated_mwrappers[interface_index_name] > 0 {
				continue
			}
			already_generated_mwrappers[interface_index_name] = current_iinidx
			current_iinidx++
			sb.writeln('static ${interface_name} I_${cctype}_to_Interface_${interface_name}(${cctype_param}* x);')
			mut cast_struct := strings.new_builder(100)
			cast_struct.writeln('(${interface_name}) {')
			cast_struct.writeln('\t\t._${cctype2} = x,')
			cast_struct.writeln('\t\t._typ = ${interface_index_name},')
			if cctype == cctype2 {
				for field in inter_info.fields {
					cname := c_name(field.name)
					field_styp := g.styp(field.typ)
					if _ := st_sym.find_field(field.name) {
						cast_struct.writeln('\t\t.${cname} = (${field_styp}*)((char*)x + __offsetof_ptr(x, ${cctype2}, ${cname})),')
					} else if st_sym.kind == .array
						&& field.name in ['element_size', 'data', 'offset', 'len', 'cap', 'flags'] {
						// Manually checking, we already knows array contains above fields
						cast_struct.writeln('\t\t.${cname} = (${field_styp}*)((char*)x + __offsetof_ptr(x, ${cctype2}, ${cname})),')
					} else {
						// the field is embedded in another struct
						cast_struct.write_string('\t\t.${cname} = (${field_styp}*)((char*)x')
						if st != ast.voidptr_type && st != ast.nil_type {
							if st_sym.kind == .struct {
								if _, embeds := g.table.find_field_from_embeds(st_sym,
									field.name)
								{
									mut typ_name := ''
									for i, embed in embeds {
										esym := g.table.sym(embed)
										if i == 0 {
											cast_struct.write_string(' + __offsetof_ptr(x, ${cctype}, ${esym.embed_name()})')
										} else {
											cast_struct.write_string(' + __offsetof_ptr(x, ${typ_name}, ${esym.embed_name()})')
										}
										typ_name = esym.cname
									}
									if embeds.len > 0 {
										cast_struct.write_string(' + __offsetof_ptr(x, ${typ_name}, ${cname})')
									}
								}
							}
						}
						cast_struct.writeln('),')
					}
				}
			}
			cast_struct.write_string('\t}')
			cast_struct_str := cast_struct.str()

			if !g.pref.is_prod {
				cast_functions.writeln('
// Casting functions for converting "${cctype}" to interface "${interface_name}"')
			}

			cast_functions.writeln('
static inline ${interface_name} I_${cctype}_to_Interface_${interface_name}(${cctype_param}* x) {
return ${cast_struct_str};
}')

			shared_fn_name := 'I___shared__${cctype}_to_shared_Interface___shared__${interface_name}'
			// Avoid undefined types errors by only generating the converters that are referenced:
			if g.has_been_referenced(shared_fn_name) {
				mut cast_shared_struct := strings.new_builder(100)
				cast_shared_struct.writeln('(__shared__${interface_name}) {')
				cast_shared_struct.writeln('\t\t.mtx = {0},')
				cast_shared_struct.writeln('\t\t.val = {')
				cast_shared_struct.writeln('\t\t\t._${cctype} = &x->val,')
				cast_shared_struct.writeln('\t\t\t._typ = ${interface_index_name},')
				cast_shared_struct.writeln('\t\t}')
				cast_shared_struct.write_string('\t}')
				cast_shared_struct_str := cast_shared_struct.str()
				cast_functions.writeln('
// Casting functions for converting "__shared__${cctype}" to interface "__shared__${interface_name}"
static inline __shared__${interface_name} ${shared_fn_name}(__shared__${cctype}* x) {
return ${cast_shared_struct_str};
}')
			}

			if g.pref.build_mode != .build_module {
				methods_struct.writeln('\t{')
			}
			if st == ast.voidptr_type || st == ast.nil_type {
				mut mnames := methodidx.keys()
				mnames.sort(a < b)
				for mname in mnames {
					if g.pref.build_mode != .build_module {
						methods_struct.writeln('\t\t._method_${c_fn_name(mname)} = (void*) 0,')
					}
				}
			}
			mut methods := st_sym.methods.clone()
			mut aliased_method_names := []string{}
			method_names := methods.map(it.name)
			match st_sym.info {
				ast.Struct, ast.Interface, ast.SumType {
					if st_sym.info.parent_type.has_flag(.generic) {
						parent_sym := g.table.sym(st_sym.info.parent_type)
						for method in parent_sym.methods {
							if method.name in methodidx {
								methods << st_sym.find_method_with_generic_parent(method.name) or {
									continue
								}
							}
						}
					}
				}
				ast.Alias {
					parent_sym := g.table.sym(st_sym.info.parent_type)
					match parent_sym.info {
						ast.Struct, ast.Interface, ast.SumType {
							mut t_method_names := methods.map(it.name)
							for method in parent_sym.methods {
								if method.name in methodidx {
									parent_method := parent_sym.find_method_with_generic_parent(method.name) or {
										continue
									}
									if parent_method.name !in methodidx {
										continue
									}
									if parent_method.name !in t_method_names {
										methods << parent_method
										aliased_method_names << parent_method.name
										t_method_names << parent_method.name
									}
								}
							}
						}
						else {}
					}
				}
				else {}
			}
			t_methods := g.table.get_embed_methods(st_sym)
			mut t_method_names := methods.map(it.name)
			for t_method in t_methods {
				if t_method.name !in t_method_names {
					methods << t_method
					t_method_names << t_method.name
				}
			}

			mut ordered_methods := methods.clone()
			ordered_methods.sort(a.name < b.name)
			for method in ordered_methods {
				mut name := method.name
				if method.generic_names.len > 0 && inter_info.parent_type.has_flag(.generic) {
					parent_sym := g.table.sym(inter_info.parent_type)
					match parent_sym.info {
						ast.Struct, ast.Interface, ast.SumType {
							name = g.generic_fn_name(parent_sym.info.concrete_types, method.name)
						}
						else {}
					}
				}

				if method.name !in methodidx {
					// a method that is not part of the interface should be just skipped
					continue
				}
				// .speak = Cat_speak
				if st_sym.info is ast.Struct {
					if method.generic_names.len > 0 && st_sym.info.parent_type.has_flag(.generic) {
						name = g.generic_fn_name(st_sym.info.concrete_types, method.name)
					}
				}
				styp := g.cc_type(method.params[0].typ, true)
				mut method_call := '${styp}_${name}'
				if cctype == cctype2 && !method.params[0].typ.is_ptr() {
					if method.name !in aliased_method_names {
						method_call = '${cctype}_${name}'
					} else {
						method_call = '${styp}_${name}'
					}
					// inline void Cat_speak_Interface_Animal_method_wrapper(Cat c) { return Cat_speak(*c); }
					iwpostfix := '_Interface_${interface_name}_method_wrapper'
					methods_wrapper.write_string('static inline ${g.ret_styp(method.return_type)} ${cctype}_${name}${iwpostfix}(')
					params_start_pos := g.out.len
					mut params := method.params.clone()
					// hack to mutate typ
					params[0] = ast.Param{
						...params[0]
						typ: st.set_nr_muls(1)
					}
					fargs, _, _ := g.fn_decl_params(params, unsafe { nil }, false, false)
					mut parameter_name := g.out.cut_last(g.out.len - params_start_pos)

					if st.is_ptr() {
						parameter_name = parameter_name.trim_string_left('__shared__')
					}

					methods_wrapper.write_string(parameter_name)
					methods_wrapper.writeln(') {')
					methods_wrapper.write_string('\t')
					if method.return_type != ast.void_type {
						methods_wrapper.write_string('return ')
					}
					_, embed_types := g.table.find_method_from_embeds(st_sym, method.name) or {
						ast.Fn{}, []ast.Type{}
					}
					if embed_types.len > 0 && method.name !in method_names {
						embed_sym := g.table.sym(embed_types.last())
						method_name := '${embed_sym.cname}_${method.name}'
						methods_wrapper.write_string('${method_name}(${fargs[0]}')
						for idx_embed, embed in embed_types {
							esym := g.table.sym(embed)
							if idx_embed == 0 || embed_types[idx_embed - 1].is_any_kind_of_pointer() {
								methods_wrapper.write_string('->${esym.embed_name()}')
							} else {
								methods_wrapper.write_string('.${esym.embed_name()}')
							}
						}
						if fargs.len > 1 {
							methods_wrapper.write_string(', ')
						}
						args := fargs[1..].join(', ')
						methods_wrapper.writeln('${args});')
					} else {
						if parameter_name.starts_with('__shared__') {
							methods_wrapper.writeln('${method_call}(${fargs.join(', ')}->val);')
						} else {
							methods_wrapper.writeln('${method_call}(*${fargs.join(', ')});')
						}
					}
					methods_wrapper.writeln('}')
					// .speak = Cat_speak_Interface_Animal_method_wrapper
					method_call += iwpostfix
				}
				if g.pref.build_mode != .build_module && st != ast.voidptr_type
					&& st != ast.nil_type {
					methods_struct.writeln('\t\t._method_${c_fn_name(method.name)} = (void*) ${method_call},')
				}
			}

			if g.pref.build_mode != .build_module {
				methods_struct.writeln('\t},')
			}
			iin_idx := already_generated_mwrappers[interface_index_name] - iinidx_minimum_base
			if g.pref.build_mode != .build_module {
				sb.writeln('${g.static_modifier}const int ${interface_index_name} = ${iin_idx};')
			} else {
				sb.writeln('extern const int ${interface_index_name};')
			}
		}
		for vtyp, variants in inter_info.conversions {
			vsym := g.table.sym(ast.idx_to_type(vtyp))

			if variants.len > 0 {
				conversion_functions.write_string('static inline bool I_${interface_name}_is_I_${vsym.cname}(${interface_name} x) {\n\treturn ')
				for i, variant in variants {
					variant_sym := g.table.sym(variant)
					if g.pref.skip_unused && variant_sym.kind == .struct
						&& variant_sym.idx !in g.table.used_features.used_syms {
						continue
					}
					if i > 0 {
						conversion_functions.write_string(' || ')
					}
					conversion_functions.write_string('(x._typ == _${interface_name}_${variant_sym.cname}_index)')
				}
				conversion_functions.writeln(';\n}')
			}

			conversion_functions.writeln('static inline ${vsym.cname} I_${interface_name}_as_I_${vsym.cname}(${interface_name} x) {')
			for variant in variants {
				variant_sym := g.table.sym(variant)
				conversion_functions.writeln('\tif (x._typ == _${interface_name}_${variant_sym.cname}_index) return I_${variant_sym.cname}_to_Interface_${vsym.cname}(x._${variant_sym.cname});')
			}
			pmessage := 'string__plus(string__plus(tos3("`as_cast`: cannot convert "), tos3(v_typeof_interface_${interface_name}(x._typ))), tos3(" to ${util.strip_main_name(vsym.name)}"))'
			if g.pref.is_debug {
				// TODO: actually return a valid position here
				conversion_functions.write_string2('\tpanic_debug(1, tos3("builtin.v"), tos3("builtin"), tos3("__as_cast"), ',
					pmessage)
				conversion_functions.writeln(');')
			} else {
				conversion_functions.write_string2('\t_v_panic(', pmessage)
				conversion_functions.writeln(');')
			}
			conversion_functions.writeln2('\treturn (${vsym.cname}){0};', '}')
		}
		if !g.pref.is_prod {
			sb.writeln('// ^^^ number of types for interface ${interface_name}: ${current_iinidx - iinidx_minimum_base}')
		}
		if iname_table_length == 0 {
			methods_struct.writeln('')
		} else {
			if g.pref.build_mode != .build_module {
				methods_struct.writeln('};')
			}
		}
		// add line return after interface index declarations
		sb.writeln('')
		if inter_methods.len > 0 {
			sb.writeln2(methods_wrapper.str(), methods_struct_def.str())
			sb.writeln(methods_struct.str())
		}
		if cast_functions.len > 0 {
			sb.writeln(cast_functions.str())
		}
	}
	if conversion_functions.len > 0 {
		sb.writeln(conversion_functions.str())
	}
	return sb.str()
}

fn (mut g Gen) panic_debug_info(pos token.Pos) (int, string, string, string) {
	paline := pos.line_nr + 1
	if g.fn_decl == unsafe { nil } {
		return paline, '', 'main', 'C._vinit'
	}
	pafile := g.fn_decl.file.replace('\\', '/')
	pafn := g.fn_decl.name.after('.')
	pamod := g.fn_decl.modname()
	return paline, pafile, pamod, pafn
}

pub fn get_guarded_include_text(iname string, imessage string) string {
	res := '
	|#if defined(__has_include)
	|
	|#if __has_include(${iname})
	|#include ${iname}
	|#else
	|#error VERROR_MESSAGE ${imessage}
	|#endif
	|
	|#else
	|#include ${iname}
	|#endif
	'.strip_margin()
	return res
}

fn (mut g Gen) trace[T](fbase string, x &T) {
	if g.file.path_base == fbase {
		println('> g.trace | ${fbase:-10s} | ${voidptr(x):16} | ${x}')
	}
}

@[params]
pub struct TraceLastLinesParams {
pub:
	nlines int = 2
	msg    string
}

fn (mut g Gen) trace_last_lines(fbase string, params TraceLastLinesParams) {
	if fbase != '' && g.file.path_base != fbase {
		return
	}
	if params.nlines < 1 || params.nlines > 1000 {
		return
	}
	if g.out.len == 0 {
		return
	}
	mut lines := 1
	mut i := g.out.len - 1
	for ; i >= 0; i-- {
		if g.out[i] == `\n` {
			if lines == params.nlines {
				break
			}
			lines++
		}
	}
	println('> g.trace_last_lines g.out last ${params.nlines} lines, pos: ${i + 1} ... g.out.len: ${g.out.len} ${params.msg}')
	println(term.colorize(term.green, g.out.after(i + 1)))
	println('`'.repeat(80))
}

// ret_type generates proper type name for return type context
pub fn (mut g Gen) ret_styp(typ ast.Type) string {
	mut ret_styp := g.styp(typ)
	if !typ.has_option_or_result() {
		ret_sym := g.table.sym(typ)
		if ret_sym.info is ast.ArrayFixed && !ret_sym.info.is_fn_ret {
			ret_styp = '_v_${ret_styp}'
		} else if ret_sym.info is ast.Alias {
			unalias_sym := g.table.sym(ret_sym.info.parent_type)
			if unalias_sym.info is ast.ArrayFixed && !unalias_sym.info.is_fn_ret {
				ret_styp = '_v_${g.styp(ret_sym.info.parent_type)}'
			}
		}
	}
	return ret_styp
}

pub fn (mut g Gen) get_array_depth(el_typ ast.Type) int {
	typ := g.unwrap_generic(el_typ)
	sym := g.table.final_sym(typ)
	if sym.kind == .array {
		info := sym.info as ast.Array
		return 1 + g.get_array_depth(info.elem_type)
	} else {
		return 0
	}
}

// returns true if `t` includes any pointer(s) - during garbage collection heap regions
// that contain no pointers do not have to be scanned
@[direct_array_access]
pub fn (mut g Gen) contains_ptr(el_typ ast.Type) bool {
	if t_typ := g.contains_ptr_cache[el_typ] {
		return t_typ
	}
	if el_typ.is_any_kind_of_pointer() {
		g.contains_ptr_cache[el_typ] = true
		return true
	}
	typ := g.unwrap_generic(el_typ)
	if typ.is_ptr() {
		return true
	}
	sym := g.table.final_sym(typ)
	if sym.language != .v {
		g.contains_ptr_cache[typ] = true
		return true
	}
	match sym.kind {
		.i8, .i16, .int, .i64, .u8, .u16, .u32, .u64, .f32, .f64, .char, .rune, .bool, .enum {
			g.contains_ptr_cache[typ] = false
			return false
		}
		.array_fixed {
			info := sym.info as ast.ArrayFixed
			return g.contains_ptr(info.elem_type)
		}
		.struct {
			info := sym.info as ast.Struct
			for embed in info.embeds {
				if g.contains_ptr(embed) {
					return true
				}
			}
			for field in info.fields {
				if g.contains_ptr(field.typ) {
					return true
				}
			}
			return false
		}
		.aggregate {
			info := sym.info as ast.Aggregate
			for atyp in info.types {
				if g.contains_ptr(atyp) {
					return true
				}
			}
			return false
		}
		.multi_return {
			info := sym.info as ast.MultiReturn
			for mrtyp in info.types {
				if g.contains_ptr(mrtyp) {
					return true
				}
			}
			return false
		}
		else {
			g.contains_ptr_cache[typ] = true
			return true
		}
	}
}

fn (mut g Gen) check_noscan(elem_typ ast.Type) string {
	if g.pref.gc_mode in [.boehm_full_opt, .boehm_incr_opt] {
		if !g.contains_ptr(elem_typ) {
			return '_noscan'
		}
	}
	return ''
}
