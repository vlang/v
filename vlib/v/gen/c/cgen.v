// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import os
import strings
import hash.fnv1a
import v.ast
import v.pref
import v.token
import v.util
import v.util.version
import v.depgraph
import v.comptime
import sync.pool

// Note: some of the words in c_reserved, are not reserved in C, but are
// in C++, or have special meaning in V, thus need escaping too. `small`
// should not be needed, but see:
// https://stackoverflow.com/questions/5874215/what-is-rpcndr-h
const c_reserved = ['array', 'auto', 'bool', 'break', 'calloc', 'case', 'char', 'class', 'complex',
	'const', 'continue', 'default', 'delete', 'do', 'double', 'else', 'enum', 'error', 'exit',
	'export', 'extern', 'false', 'float', 'for', 'free', 'goto', 'if', 'inline', 'int', 'link',
	'long', 'malloc', 'namespace', 'new', 'nil', 'panic', 'register', 'restrict', 'return', 'short',
	'signed', 'sizeof', 'static', 'string', 'struct', 'switch', 'typedef', 'typename', 'union',
	'unix', 'unsigned', 'void', 'volatile', 'while', 'template', 'true', 'small', 'stdout', 'stdin',
	'stderr', 'far', 'near', 'huge', 'requires']
const c_reserved_chk = token.new_keywords_matcher_from_array_trie(c_reserved)
// same order as in token.Kind
const cmp_str = ['eq', 'ne', 'gt', 'lt', 'ge', 'le']
// when operands are switched
const cmp_rev = ['eq', 'ne', 'lt', 'gt', 'le', 'ge']
const result_name = '_result'
const option_name = '_option'

fn string_array_to_map(a []string) map[string]bool {
	mut res := map[string]bool{}
	for x in a {
		res[x] = true
	}
	return res
}

pub struct Gen {
	pref                &pref.Preferences = unsafe { nil }
	field_data_type     ast.Type // cache her to avoid map lookups
	enum_data_type      ast.Type // cache her to avoid map lookups
	variant_data_type   ast.Type // cache her to avoid map lookups
	module_built        string
	timers_should_print bool
mut:
	out                       strings.Builder
	cheaders                  strings.Builder
	preincludes               strings.Builder // allows includes to go before `definitions`
	includes                  strings.Builder // all C #includes required by V modules
	typedefs                  strings.Builder
	enum_typedefs             strings.Builder // enum types
	definitions               strings.Builder // typedefs, defines etc (everything that goes to the top of the file)
	type_definitions          strings.Builder // typedefs, defines etc (everything that goes to the top of the file)
	alias_definitions         strings.Builder // alias fixed array of non-builtin
	hotcode_definitions       strings.Builder // -live declarations & functions
	channel_definitions       strings.Builder // channel related code
	thread_definitions        strings.Builder // thread defines
	comptime_definitions      strings.Builder // custom defines, given by -d/-define flags on the CLI
	cleanup                   strings.Builder
	cleanups                  map[string]strings.Builder // contents of `void _vcleanup(){}`
	gowrappers                strings.Builder // all go callsite wrappers
	auto_str_funcs            strings.Builder // function bodies of all auto generated _str funcs
	dump_funcs                strings.Builder // function bodies of all auto generated _str funcs
	pcs_declarations          strings.Builder // -prof profile counter declarations for each function
	embedded_data             strings.Builder // data to embed in the executable/binary
	shared_types              strings.Builder // shared/lock types
	shared_functions          strings.Builder // shared constructors
	out_options_forward       strings.Builder // forward `option_xxxx` types
	out_options               strings.Builder // `option_xxxx` types
	out_results_forward       strings.Builder // forward`result_xxxx` types
	out_results               strings.Builder // `result_xxxx` types
	json_forward_decls        strings.Builder // json type forward decls
	sql_buf                   strings.Builder // for writing exprs to args via `sqlite3_bind_int()` etc
	global_const_defs         map[string]GlobalConstDef
	sorted_global_const_names []string
	file                      &ast.File  = unsafe { nil }
	table                     &ast.Table = unsafe { nil }
	unique_file_path_hash     u64 // a hash of file.path, used for making auxiliary fn generation unique (like `compare_xyz`)
	fn_decl                   &ast.FnDecl = unsafe { nil } // pointer to the FnDecl we are currently inside otherwise 0
	last_fn_c_name            string
	tmp_count                 int         // counter for unique tmp vars (_tmp1, _tmp2 etc); resets at the start of each fn.
	tmp_count_af              int  // a separate tmp var counter for autofree fn calls
	tmp_count_declarations    int  // counter for unique tmp names (_d1, _d2 etc); does NOT reset, used for C declarations
	global_tmp_count          int  // like tmp_count but global and not reset in each function
	discard_or_result         bool // do not safe last ExprStmt of `or` block in tmp variable to defer ongoing expr usage
	is_direct_array_access    bool // inside a `[direct_array_access fn a() {}` function
	is_assign_lhs             bool // inside left part of assign expr (for array_set(), etc)
	is_void_expr_stmt         bool // ExprStmt whose result is discarded
	is_arraymap_set           bool // map or array set value state
	is_amp                    bool // for `&Foo{}` to merge PrefixExpr `&` and StructInit `Foo{}`; also for `&u8(0)` etc
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
	vlines_path               string   // set to the proper path for generating #line directives
	options_pos_forward       int      // insertion point to forward
	options_forward           []string // to forward
	options                   map[string]string // to avoid duplicates
	results_forward           []string // to forward
	results                   map[string]string // to avoid duplicates
	done_options              shared []string   // to avoid duplicates
	done_results              shared []string   // to avoid duplicates
	chan_pop_options          map[string]string // types for `x := <-ch or {...}`
	chan_push_options         map[string]string // types for `ch <- x or {...}`
	mtxs                      string // array of mutexes if the `lock` has multiple variables
	labeled_loops             map[string]&ast.Stmt
	inner_loop                &ast.Stmt = unsafe { nil }
	shareds                   map[int]string // types with hidden mutex for which decl has been emitted
	inside_ternary            int  // ?: comma separated statements on a single line
	inside_map_postfix        bool // inside map++/-- postfix expr
	inside_map_infix          bool // inside map<</+=/-= infix expr
	inside_assign             bool
	inside_map_index          bool
	inside_array_index        bool
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
	expected_fixed_arr        bool
	inside_for_c_stmt         bool
	// inside_comptime_for_field bool
	inside_cast_in_heap     int // inside cast to interface type in heap (resolve recursive calls)
	inside_cast             bool
	inside_const            bool
	inside_array_item       bool
	inside_const_opt_or_res bool
	inside_lambda           bool
	inside_cinit            bool
	inside_interface_deref  bool
	last_tmp_call_var       []string
	loop_depth              int
	ternary_names           map[string]string
	ternary_level_names     map[string][]string
	arraymap_set_pos        int      // map or array set value position
	stmt_path_pos           []int    // positions of each statement start, for inserting C statements before the current statement
	skip_stmt_pos           bool     // for handling if expressions + autofree (since both prepend C statements)
	left_is_opt             bool     // left hand side on assignment is an option
	right_is_opt            bool     // right hand side on assignment is an option
	assign_ct_type          ast.Type // left hand side resolved comptime type
	indent                  int
	empty_line              bool
	assign_op               token.Kind // *=, =, etc (for array_set)
	defer_stmts             []ast.DeferStmt
	defer_ifdef             string
	defer_profile_code      string
	defer_vars              []string
	str_types               []StrType       // types that need automatic str() generation
	generated_str_fns       []StrType       // types that already have a str() function
	str_fn_names            []string        // remove duplicate function names
	threaded_fns            shared []string // for generating unique wrapper types and fns for `go xxx()`
	waiter_fns              shared []string // functions that wait for `go xxx()` to finish
	needed_equality_fns     []ast.Type
	generated_eq_fns        []ast.Type
	array_sort_fn           shared []string
	array_contains_types    []ast.Type
	array_index_types       []ast.Type
	auto_fn_definitions     []string // auto generated functions definition list
	sumtype_casting_fns     []SumtypeCastingFn
	anon_fn_definitions     []string     // anon generated functions definition list
	sumtype_definitions     map[int]bool // `_TypeA_to_sumtype_TypeB()` fns that have been generated
	json_types              []ast.Type   // to avoid json gen duplicates
	pcs                     []ProfileCounterMeta // -prof profile counter fn_names => fn counter name
	hotcode_fn_names        []string
	hotcode_fpaths          []string
	embedded_files          []ast.EmbeddedFile
	sql_i                   int
	sql_stmt_name           string
	sql_bind_name           string
	sql_idents              []string
	sql_idents_types        []ast.Type
	sql_left_type           ast.Type
	sql_table_name          string
	sql_fkey                string
	sql_parent_id           string
	sql_side                SqlExprSide // left or right, to distinguish idents in `name == name`
	strs_to_free0           []string    // strings.Builder
	// strs_to_free          []string // strings.Builder
	// tmp_arg_vars_to_free  []string
	// autofree_pregen       map[string]string
	// autofree_pregen_buf   strings.Builder
	// autofree_tmp_vars     []string // to avoid redefining the same tmp vars in a single function
	// nr_vars_to_free       int
	// doing_autofree_tmp    bool
	comptime_info_stack              []comptime.ComptimeInfo // stores the values from the above on each $for loop, to make nesting them easier
	comptime                         comptime.ComptimeInfo
	prevent_sum_type_unwrapping_once bool // needed for assign new values to sum type
	// used in match multi branch
	// TypeOne, TypeTwo {}
	// where an aggregate (at least two types) is generated
	// sum type deref needs to know which index to deref because unions take care of the correct field
	aggregate_type_idx  int
	branch_parent_pos   int    // used in BranchStmt (continue/break) for autofree stop position
	returned_var_name   string // to detect that a var doesn't need to be freed since it's being returned
	infix_left_var_name string // a && if expr
	called_fn_name      string
	timers              &util.Timers = util.get_timers()
	force_main_console  bool // true when [console] used on fn main()
	as_cast_type_names  map[string]string // table for type name lookup in runtime (for __as_cast)
	obf_table           map[string]string
	referenced_fns      shared map[string]bool // functions that have been referenced
	nr_closures         int
	expected_cast_type  ast.Type // for match expr of sumtypes
	or_expr_return_type ast.Type // or { 0, 1 } return type
	anon_fn             bool
	tests_inited        bool
	has_main            bool
	// main_fn_decl_node  ast.FnDecl
	cur_mod                ast.Module
	cur_concrete_types     []ast.Type  // do not use table.cur_concrete_types because table is global, so should not be accessed by different threads
	cur_fn                 &ast.FnDecl = unsafe { nil } // same here
	cur_lock               ast.LockExpr
	cur_struct_init_typ    ast.Type
	autofree_methods       map[int]bool
	generated_free_methods map[int]bool
	autofree_scope_stmts   []string
	use_segfault_handler   bool = true
	test_function_names    []string
	/////////
	// out_parallel []strings.Builder
	// out_idx      int
	out_fn_start_pos []int  // for generating multiple .c files, stores locations of all fn positions in `out` string builder
	static_modifier  string // for parallel_cc

	has_reflection       bool
	reflection_strings   &map[string]int
	defer_return_tmp_var string
	vweb_filter_fn_name  string // vweb__filter or x__vweb__filter, used by $vweb.html() for escaping strings in the templates, depending on which `vweb` import is used
}

// global or const variable definition string
struct GlobalConstDef {
	mod            string   // module name
	def            string   // definition
	init           string   // init later (in _vinit)
	dep_names      []string // the names of all the consts, that this const depends on
	order          int      // -1 for simple defines, string literals, anonymous function names, extern declarations etc
	is_precomputed bool     // can be declared as a const in C: primitive, and a simple definition
}

pub fn gen(files []&ast.File, table &ast.Table, pref_ &pref.Preferences) (string, string, string, []int) {
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
		file: unsafe { nil }
		out: strings.new_builder(512000)
		cheaders: strings.new_builder(15000)
		includes: strings.new_builder(100)
		preincludes: strings.new_builder(100)
		typedefs: strings.new_builder(100)
		enum_typedefs: strings.new_builder(100)
		type_definitions: strings.new_builder(100)
		alias_definitions: strings.new_builder(100)
		hotcode_definitions: strings.new_builder(100)
		channel_definitions: strings.new_builder(100)
		thread_definitions: strings.new_builder(100)
		comptime_definitions: strings.new_builder(100)
		definitions: strings.new_builder(100)
		gowrappers: strings.new_builder(100)
		auto_str_funcs: strings.new_builder(100)
		dump_funcs: strings.new_builder(100)
		pcs_declarations: strings.new_builder(100)
		embedded_data: strings.new_builder(1000)
		out_options_forward: strings.new_builder(100)
		out_options: strings.new_builder(100)
		out_results_forward: strings.new_builder(100)
		out_results: strings.new_builder(100)
		shared_types: strings.new_builder(100)
		shared_functions: strings.new_builder(100)
		json_forward_decls: strings.new_builder(100)
		sql_buf: strings.new_builder(100)
		table: table
		pref: pref_
		fn_decl: unsafe { nil }
		is_autofree: pref_.autofree
		indent: -1
		module_built: module_built
		timers_should_print: timers_should_print
		timers: util.new_timers(should_print: timers_should_print, label: 'global_cgen')
		inner_loop: &ast.empty_stmt
		field_data_type: ast.Type(table.find_type_idx('FieldData'))
		enum_data_type: ast.Type(table.find_type_idx('EnumData'))
		is_cc_msvc: pref_.ccompiler == 'msvc'
		use_segfault_handler: !('no_segfault_handler' in pref_.compile_defines
			|| pref_.os in [.wasm32, .wasm32_emscripten])
		static_modifier: if pref_.parallel_cc { 'static' } else { '' }
		has_reflection: 'v.reflection' in table.modules
		reflection_strings: &reflection_strings
	}

	global_g.comptime = &comptime.ComptimeInfo{
		resolver: &global_g
		table: table
	}

	/*
	global_g.out_parallel = []strings.Builder{len: nr_cpus}
	for i in 0 .. nr_cpus {
		global_g.out_parallel[i] = strings.new_builder(100000)
		global_g.out_parallel[i].writeln('#include "out.h"\n')
	}
	println('LEN=')
	println(global_g.out_parallel.len)
	*/
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
			global_g.out.write(g.out) or { panic(err) }
			global_g.cheaders.write(g.cheaders) or { panic(err) }
			global_g.preincludes.write(g.preincludes) or { panic(err) }
			global_g.includes.write(g.includes) or { panic(err) }
			global_g.typedefs.write(g.typedefs) or { panic(err) }
			global_g.type_definitions.write(g.type_definitions) or { panic(err) }
			global_g.alias_definitions.write(g.alias_definitions) or { panic(err) }
			global_g.definitions.write(g.definitions) or { panic(err) }
			global_g.gowrappers.write(g.gowrappers) or { panic(err) }
			global_g.auto_str_funcs.write(g.auto_str_funcs) or { panic(err) }
			global_g.dump_funcs.write(g.auto_str_funcs) or { panic(err) }
			global_g.comptime_definitions.write(g.comptime_definitions) or { panic(err) }
			global_g.pcs_declarations.write(g.pcs_declarations) or { panic(err) }
			global_g.hotcode_definitions.write(g.hotcode_definitions) or { panic(err) }
			global_g.embedded_data.write(g.embedded_data) or { panic(err) }
			global_g.shared_types.write(g.shared_types) or { panic(err) }
			global_g.shared_functions.write(g.channel_definitions) or { panic(err) }

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
			global_g.json_forward_decls.write(g.json_forward_decls) or { panic(err) }
			global_g.enum_typedefs.write(g.enum_typedefs) or { panic(err) }
			global_g.channel_definitions.write(g.channel_definitions) or { panic(err) }
			global_g.thread_definitions.write(g.thread_definitions) or { panic(err) }
			global_g.sql_buf.write(g.sql_buf) or { panic(err) }

			global_g.cleanups[g.file.mod.name].write(g.cleanup) or { panic(err) } // strings.Builder.write never fails; it is like that in the source

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
			unsafe { g.free_builders() }
			for k, v in g.autofree_methods {
				global_g.autofree_methods[k] = v
			}
		}
	} else {
		util.timing_start('cgen serial processing')
		for file in files {
			global_g.file = file
			global_g.gen_file()
			global_g.cleanups[file.mod.name].drain_builder(mut global_g.cleanup, 100)
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
	global_g.write_results()
	global_g.write_options()
	global_g.sort_globals_consts()
	util.timing_measure('cgen unification')

	mut g := global_g
	util.timing_start('cgen common')

	// to make sure type idx's are the same in cached mods
	if g.pref.build_mode == .build_module {
		for idx, sym in g.table.type_symbols {
			if idx in [0, 31] {
				continue
			}
			g.definitions.writeln('int _v_type_idx_${sym.cname}();')
		}
	} else if g.pref.use_cache {
		for idx, sym in g.table.type_symbols {
			if idx in [0, 31] {
				continue
			}
			g.definitions.writeln('int _v_type_idx_${sym.cname}() { return ${idx}; };')
		}
	}
	//
	// v files are finished, what remains is pure C code
	g.gen_vlines_reset()
	if g.pref.build_mode != .build_module {
		// no init in builtin.o
		g.write_init_function()
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

	mut b := strings.new_builder(640000)
	b.write_string(g.hashes())
	if g.use_segfault_handler {
		b.writeln('\n#define V_USE_SIGNAL_H')
	}
	b.writeln('\n// V comptime_definitions:')
	b.write_string(g.comptime_definitions.str())
	b.writeln('\n// V typedefs:')
	b.write_string(g.typedefs.str())
	b.writeln('\n // V preincludes:')
	b.write_string(g.preincludes.str())
	b.writeln('\n// V cheaders:')
	b.write_string(g.cheaders.str())
	if g.pcs_declarations.len > 0 {
		b.writeln('\n// V profile counters:')
		b.write_string(g.pcs_declarations.str())
	}
	b.writeln('\n// V includes:')
	b.write_string(g.includes.str())
	b.writeln('\n// Enum definitions:')
	b.write_string(g.enum_typedefs.str())
	b.writeln('\n// Thread definitions:')
	b.write_string(g.thread_definitions.str())
	b.writeln('\n// V type definitions:')
	b.write_string(g.type_definitions.str())
	b.writeln('\n// V alias definitions:')
	b.write_string(g.alias_definitions.str())
	b.writeln('\n// V shared types:')
	b.write_string(g.shared_types.str())
	b.writeln('\n// V Option_xxx definitions:')
	b.write_string(g.out_options.str())
	b.writeln('\n// V result_xxx definitions:')
	b.write_string(g.out_results.str())
	b.writeln('\n// V json forward decls:')
	b.write_string(g.json_forward_decls.str())
	b.writeln('\n// V definitions:')
	b.write_string(g.definitions.str())
	b.writeln('\n// V global/const definitions:')
	for var_name in g.sorted_global_const_names {
		if var := g.global_const_defs[var_name] {
			b.writeln(var.def)
		}
	}
	interface_table := g.interface_table()
	if interface_table.len > 0 {
		b.writeln('\n// V interface table:')
		b.write_string(interface_table)
	}
	if g.gowrappers.len > 0 {
		b.writeln('\n// V gowrappers:')
		b.write_string(g.gowrappers.str())
	}
	if g.hotcode_definitions.len > 0 {
		b.writeln('\n// V hotcode definitions:')
		b.write_string(g.hotcode_definitions.str())
	}
	if g.embedded_data.len > 0 {
		b.writeln('\n// V embedded data:')
		b.write_string(g.embedded_data.str())
	}
	if g.shared_functions.len > 0 {
		b.writeln('\n// V shared type functions:')
		b.write_string(g.shared_functions.str())
		b.write_string(c_concurrency_helpers)
	}
	if g.channel_definitions.len > 0 {
		b.writeln('\n// V channel code:')
		b.write_string(g.channel_definitions.str())
	}
	if g.auto_str_funcs.len > 0 {
		b.writeln('\n// V auto str functions:')
		b.write_string(g.auto_str_funcs.str())
	}
	if g.dump_funcs.len > 0 {
		b.writeln('\n// V dump functions:')
		b.write_string(g.dump_funcs.str())
	}
	if g.auto_fn_definitions.len > 0 {
		b.writeln('\n// V auto functions:')
		for fn_def in g.auto_fn_definitions {
			b.writeln(fn_def)
		}
	}
	if g.anon_fn_definitions.len > 0 {
		if g.nr_closures > 0 {
			b.writeln('\n// V closure helpers')
			b.writeln(c_closure_helpers(g.pref))
		}
		b.writeln('\n// V anon functions:')
		for fn_def in g.anon_fn_definitions {
			b.writeln(fn_def)
		}
	}
	b.writeln('\n// end of V out')
	mut header := b.last_n(b.len)
	header = '#ifndef V_HEADER_FILE\n#define V_HEADER_FILE' + header
	header += '\n#endif\n'
	out_str := g.out.str()
	b.write_string(out_str)
	b.writeln('\n// THE END.')
	util.timing_measure('cgen common')
	res := b.str()
	$if trace_all_generic_fn_keys ? {
		gkeys := g.table.fn_generic_types.keys()
		for gkey in gkeys {
			eprintln('>> g.table.fn_generic_types key: ${gkey}')
		}
	}
	out_fn_start_pos := g.out_fn_start_pos.clone()
	unsafe { b.free() }
	unsafe { g.free_builders() }

	return header, res, out_str, out_fn_start_pos
}

fn cgen_process_one_file_cb(mut p pool.PoolProcessor, idx int, wid int) &Gen {
	file := p.get_item[&ast.File](idx)
	mut global_g := unsafe { &Gen(p.get_shared_context()) }
	mut g := &Gen{
		file: file
		out: strings.new_builder(512000)
		cheaders: strings.new_builder(15000)
		includes: strings.new_builder(100)
		typedefs: strings.new_builder(100)
		type_definitions: strings.new_builder(100)
		alias_definitions: strings.new_builder(100)
		definitions: strings.new_builder(100)
		gowrappers: strings.new_builder(100)
		auto_str_funcs: strings.new_builder(100)
		comptime_definitions: strings.new_builder(100)
		pcs_declarations: strings.new_builder(100)
		hotcode_definitions: strings.new_builder(100)
		embedded_data: strings.new_builder(1000)
		out_options_forward: strings.new_builder(100)
		out_options: strings.new_builder(100)
		out_results_forward: strings.new_builder(100)
		out_results: strings.new_builder(100)
		shared_types: strings.new_builder(100)
		shared_functions: strings.new_builder(100)
		channel_definitions: strings.new_builder(100)
		thread_definitions: strings.new_builder(100)
		json_forward_decls: strings.new_builder(100)
		enum_typedefs: strings.new_builder(100)
		sql_buf: strings.new_builder(100)
		cleanup: strings.new_builder(100)
		table: global_g.table
		pref: global_g.pref
		fn_decl: unsafe { nil }
		indent: -1
		module_built: global_g.module_built
		timers: util.new_timers(
			should_print: global_g.timers_should_print
			label: 'cgen_process_one_file_cb idx: ${idx}, wid: ${wid}'
		)
		inner_loop: &ast.empty_stmt
		field_data_type: ast.Type(global_g.table.find_type_idx('FieldData'))
		enum_data_type: ast.Type(global_g.table.find_type_idx('EnumData'))
		array_sort_fn: global_g.array_sort_fn
		waiter_fns: global_g.waiter_fns
		threaded_fns: global_g.threaded_fns
		options_forward: global_g.options_forward
		results_forward: global_g.results_forward
		done_options: global_g.done_options
		done_results: global_g.done_results
		is_autofree: global_g.pref.autofree
		obf_table: global_g.obf_table
		referenced_fns: global_g.referenced_fns
		is_cc_msvc: global_g.is_cc_msvc
		use_segfault_handler: global_g.use_segfault_handler
		has_reflection: 'v.reflection' in global_g.table.modules
		reflection_strings: global_g.reflection_strings
	}
	g.comptime = &comptime.ComptimeInfo{
		resolver: g
		table: global_g.table
	}
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
		g.alias_definitions.free()
		g.definitions.free()
		g.cleanup.free()
		g.gowrappers.free()
		g.auto_str_funcs.free()
		g.dump_funcs.free()
		g.comptime_definitions.free()
		g.pcs_declarations.free()
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
				g.cheaders.writeln(get_guarded_include_text('<inttypes.h>', 'The C compiler can not find <inttypes.h>. Please install build-essentials')) // int64_t etc
				if g.pref.os == .ios {
					g.cheaders.writeln(get_guarded_include_text('<stdbool.h>', 'The C compiler can not find <stdbool.h>. Please install build-essentials')) // bool, true, false
				}
				g.cheaders.writeln(get_guarded_include_text('<stddef.h>', 'The C compiler can not find <stddef.h>. Please install build-essentials')) // size_t, ptrdiff_t
			}
		}
		if g.pref.nofloat {
			g.cheaders.writeln('#define VNOFLOAT 1')
		}
		g.cheaders.writeln(c_builtin_types)
		if g.pref.is_bare {
			g.cheaders.writeln(c_bare_headers)
		} else {
			g.cheaders.writeln(c_headers)
		}
		if !g.pref.skip_unused || g.table.used_maps > 0 {
			g.cheaders.writeln(c_wyhash_headers)
		}
	}
	if g.pref.os == .ios {
		g.cheaders.writeln('#define __TARGET_IOS__ 1')
		g.cheaders.writeln('#include <spawn.h>')
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
	} else {
		g.comptime_definitions.writeln('#define _VAUTOFREE (0)')
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
	// Obfuscate only functions in the main module for now.
	// Generate the obf_ast.
	if g.pref.obfuscate {
		mut i := 0
		// fns
		for key, f in g.table.fns {
			if f.mod != 'main' && key != 'main' {
				continue
			}
			g.obf_table[key] = '_f${i}'
			i++
		}
		// methods
		for type_sym in g.table.type_symbols {
			if type_sym.mod != 'main' {
				continue
			}
			for method in type_sym.methods {
				g.obf_table[type_sym.name + '.' + method.name] = '_f${i}'
				i++
			}
		}
	}
	// we know that this is being called before the multi-threading starts
	// and this is being called in the main thread, so we can mutate the table
	mut muttable := unsafe { &ast.Table(g.table) }
	if g.use_segfault_handler {
		muttable.used_fns['v_segmentation_fault_handler'] = true
	}
	muttable.used_fns['eprintln'] = true
	muttable.used_fns['print_backtrace'] = true
	muttable.used_fns['exit'] = true
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
	} else {
		g.gen_c_main()
	}
}

pub fn (mut g Gen) write_typeof_functions() {
	g.writeln('')
	g.writeln('// >> typeof() support for sum types / interfaces')
	for ityp, sym in g.table.type_symbols {
		if sym.kind == .sum_type {
			static_prefix := if g.pref.build_mode == .build_module { 'static ' } else { '' }
			sum_info := sym.info as ast.SumType
			if sum_info.is_generic {
				continue
			}
			g.writeln('${static_prefix}char * v_typeof_sumtype_${sym.cname}(int sidx) { /* ${sym.name} */ ')
			if g.pref.build_mode == .build_module {
				g.writeln('\t\tif( sidx == _v_type_idx_${sym.cname}() ) return "${util.strip_main_name(sym.name)}";')
				for v in sum_info.variants {
					subtype := g.table.sym(v)
					g.writeln('\tif( sidx == _v_type_idx_${subtype.cname}() ) return "${util.strip_main_name(subtype.name)}";')
				}
				g.writeln('\treturn "unknown ${util.strip_main_name(sym.name)}";')
			} else {
				tidx := g.table.find_type_idx(sym.name)
				g.writeln('\tswitch(sidx) {')
				g.writeln('\t\tcase ${tidx}: return "${util.strip_main_name(sym.name)}";')
				for v in sum_info.variants {
					subtype := g.table.sym(v)
					g.writeln('\t\tcase ${v.idx()}: return "${util.strip_main_name(subtype.name)}";')
				}
				g.writeln('\t\tdefault: return "unknown ${util.strip_main_name(sym.name)}";')
				g.writeln('\t}')
			}
			g.writeln('}')
			g.writeln('')
			g.writeln('${static_prefix}int v_typeof_sumtype_idx_${sym.cname}(int sidx) { /* ${sym.name} */ ')
			if g.pref.build_mode == .build_module {
				g.writeln('\t\tif( sidx == _v_type_idx_${sym.cname}() ) return ${int(ityp)};')
				for v in sum_info.variants {
					subtype := g.table.sym(v)
					g.writeln('\tif( sidx == _v_type_idx_${subtype.cname}() ) return ${int(v)};')
				}
				g.writeln('\treturn ${int(ityp)};')
			} else {
				tidx := g.table.find_type_idx(sym.name)
				g.writeln('\tswitch(sidx) {')
				g.writeln('\t\tcase ${tidx}: return ${int(ityp)};')
				for v in sum_info.variants {
					g.writeln('\t\tcase ${v.idx()}: return ${int(v)};')
				}
				g.writeln('\t\tdefault: return ${int(ityp)};')
				g.writeln('\t}')
			}
			g.writeln('}')
		} else if sym.kind == .interface_ {
			if sym.info !is ast.Interface {
				continue
			}
			inter_info := sym.info as ast.Interface
			if inter_info.is_generic {
				continue
			}
			g.definitions.writeln('static char * v_typeof_interface_${sym.cname}(int sidx);')
			g.writeln('static char * v_typeof_interface_${sym.cname}(int sidx) { /* ${sym.name} */ ')
			for t in inter_info.types {
				sub_sym := g.table.sym(ast.mktyp(t))
				g.writeln('\tif (sidx == _${sym.cname}_${sub_sym.cname}_index) return "${util.strip_main_name(sub_sym.name)}";')
			}
			g.writeln('\treturn "unknown ${util.strip_main_name(sym.name)}";')
			g.writeln('}')
			g.writeln('')
			g.writeln('static int v_typeof_interface_idx_${sym.cname}(int sidx) { /* ${sym.name} */ ')
			for t in inter_info.types {
				sub_sym := g.table.sym(ast.mktyp(t))
				g.writeln('\tif (sidx == _${sym.cname}_${sub_sym.cname}_index) return ${int(t.set_nr_muls(0))};')
			}
			g.writeln('\treturn ${int(ityp)};')
			g.writeln('}')
		}
	}
	g.writeln('// << typeof() support for sum types')
	g.writeln('')
}

// V type to C typecc
fn (mut g Gen) typ(t ast.Type) string {
	if t.has_flag(.option) {
		// Register an optional if it's not registered yet
		return g.register_option(t)
	} else if t.has_flag(.result) {
		return g.register_result(t)
	} else {
		return g.base_type(t)
	}
}

fn (mut g Gen) base_type(_t ast.Type) string {
	t := g.unwrap_generic(_t)
	if g.pref.nofloat {
		// todo compile time if for perf?
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
	nr_muls := g.unwrap_generic(t).nr_muls()
	if nr_muls > 0 {
		styp += strings.repeat(`*`, nr_muls)
	}
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
		name += '_' + strings.repeat_string('__ptr__', typ.nr_muls()) + g.typ(typ.set_nr_muls(0))
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

// TODO this really shouldn't be separate from typ
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
	if sym.language == .c && sym.kind == .struct_ {
		styp = '${c.option_name}_${base.replace(' ', '_')}'
	} else {
		styp = '${c.option_name}_${base}'
	}
	if t.is_ptr() {
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
	if sym.language == .c && sym.kind == .struct_ {
		styp = '${c.result_name}_${base.replace(' ', '_')}'
	} else {
		styp = '${c.result_name}_${base}'
	}
	if t.is_ptr() {
		styp = styp.replace('*', '_ptr')
	}
	return styp, base
}

fn (g Gen) option_type_text(styp string, base string) string {
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

fn (g Gen) result_type_text(styp string, base string) string {
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
	return styp
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
	}
	g.gowrappers.writeln('void __v_thread_wait(__v_thread thread) {')
	if g.pref.os == .windows {
		g.gowrappers.writeln('\tu32 stat = WaitForSingleObject(thread, INFINITE);')
	} else {
		g.gowrappers.writeln('\tint stat = pthread_join(thread, (void **)NULL);')
	}
	g.gowrappers.writeln('\tif (stat != 0) { _v_panic(_SLIT("unable to join thread")); }')
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
			g.gowrappers.writeln('
void ${fn_name}(${thread_arr_typ} a) {
	for (int i = 0; i < a.len; ++i) {
		${thread_typ} t = ((${thread_typ}*)a.data)[i];
		if (t == 0) continue;
		__v_thread_wait(t);
	}
}')
		} else {
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
			g.gowrappers.writeln('
void ${fn_name}(${thread_arr_typ} a) {
	for (int i = 0; i < ${len}; ++i) {
		${thread_typ} t = ((${thread_typ}*)a)[i];
		if (t == 0) continue;
		__v_thread_wait(t);
	}
}')
		} else {
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
		done << opt_el_type
		g.channel_definitions.writeln('
static inline ${opt_el_type} __Option_${styp}_popval(${styp} ch) {
	${opt_el_type} _tmp = {0};
	if (sync__Channel_try_pop_priv(ch, _tmp.data, false)) {
		return (${opt_el_type}){ .state = 2, .err = _v_error(_SLIT("channel closed")), .data = {EMPTY_STRUCT_INITIALIZATION} };
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
		done << styp
		g.register_option(ast.void_type.set_flag(.option))
		g.channel_definitions.writeln('
static inline ${c.option_name}_void __Option_${styp}_pushval(${styp} ch, ${el_type} e) {
	if (sync__Channel_try_push_priv(ch, &e, false)) {
		return (${c.option_name}_void){ .state = 2, .err = _v_error(_SLIT("channel closed")), .data = {EMPTY_STRUCT_INITIALIZATION} };
	}
	return (${c.option_name}_void){0};
}')
	}
}

// cc_type whether to prefix 'struct' or not (C__Foo -> struct Foo)
fn (mut g Gen) cc_type(typ ast.Type, is_prefix_struct bool) string {
	sym := g.table.sym(g.unwrap_generic(typ))
	mut styp := sym.cname
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
		if sym.kind == .struct_ {
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
	return t.idx().str()
}

pub fn (mut g Gen) write_typedef_types() {
	for sym in g.table.type_symbols {
		if sym.name in c.builtins {
			continue
		}
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
						mut fixed := g.typ(info.elem_type)
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
							g.type_definitions.writeln('typedef ${fixed} ${styp} [${len}];')
							base := g.typ(info.elem_type.clear_option_and_result())
							if info.elem_type.has_flag(.option) && base !in g.options_forward {
								g.options_forward << base
							} else if info.elem_type.has_flag(.result) && base !in g.results_forward {
								g.results_forward << base
							}
						}
					}
				}
			}
			.chan {
				if sym.name != 'chan' {
					g.type_definitions.writeln('typedef chan ${sym.cname};')
					chan_inf := sym.chan_info()
					chan_elem_type := chan_inf.elem_type
					is_fixed_arr := g.table.sym(chan_elem_type).kind == .array_fixed
					if !chan_elem_type.has_flag(.generic) {
						el_stype := if is_fixed_arr { '_v_' } else { '' } + g.typ(chan_elem_type)
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
				g.type_definitions.writeln('typedef map ${sym.cname};')
			}
			else {
				continue
			}
		}
	}
	for sym in g.table.type_symbols {
		if sym.kind == .alias && sym.name !in c.builtins && sym.name !in ['byte', 'i32'] {
			g.write_alias_typesymbol_declaration(sym)
		}
	}
	for sym in g.table.type_symbols {
		if sym.kind == .function && sym.name !in c.builtins {
			g.write_fn_typesymbol_declaration(sym)
		}
	}
	// Generating interfaces after all the common types have been defined
	// to prevent generating interface struct before definition of field types
	for sym in g.table.type_symbols {
		if sym.kind == .interface_ && sym.name !in c.builtins {
			g.write_interface_typedef(sym)
		}
	}
	for sym in g.table.type_symbols {
		if sym.kind == .interface_ && sym.name !in c.builtins {
			g.write_interface_typesymbol_declaration(sym)
		}
	}
}

pub fn (mut g Gen) write_alias_typesymbol_declaration(sym ast.TypeSymbol) {
	parent := g.table.type_symbols[sym.parent_idx]
	is_c_parent := parent.name.len > 2 && parent.name[0] == `C` && parent.name[1] == `.`
	mut is_fixed_array_of_non_builtin := false
	mut parent_styp := parent.cname
	if is_c_parent {
		if sym.info is ast.Alias {
			parent_styp = g.typ(sym.info.parent_type)
		}
	} else {
		if sym.info is ast.Alias {
			parent_styp = g.typ(sym.info.parent_type)
			parent_sym := g.table.sym(sym.info.parent_type)
			if parent_sym.info is ast.ArrayFixed {
				elem_sym := g.table.sym(parent_sym.info.elem_type)
				if !elem_sym.is_builtin() {
					is_fixed_array_of_non_builtin = true
				}
			}
		}
	}
	if parent_styp == 'byte' && sym.cname == 'u8' {
		// TODO: remove this check; it is here just to fix V rebuilding in -cstrict mode with clang-12
		return
	}
	if is_fixed_array_of_non_builtin {
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
	if sym.info !is ast.Interface {
		return
	}
	info := sym.info as ast.Interface
	if info.is_generic {
		return
	}
	struct_name := c_name(sym.cname)
	g.type_definitions.writeln('struct ${struct_name} {')
	g.type_definitions.writeln('\tunion {')
	g.type_definitions.writeln('\t\tvoid* _object;')
	for variant in info.types {
		mk_typ := ast.mktyp(variant)
		if mk_typ != variant && mk_typ in info.types {
			continue
		}
		vcname := g.table.sym(mk_typ).cname
		g.type_definitions.writeln('\t\t${vcname}* _${vcname};')
	}
	g.type_definitions.writeln('\t};')
	g.type_definitions.writeln('\tint _typ;')
	for field in info.fields {
		styp := g.typ(field.typ)
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
		call_conv_attribute_suffix := if call_conv.len != 0 {
			'__attribute__((${call_conv}))'
		} else {
			''
		}
		ret_typ :=
			if !func.return_type.has_flag(.option) && g.table.sym(func.return_type).kind == .array_fixed { '_v_' } else { '' } +
			g.typ(func.return_type)
		g.type_definitions.write_string('typedef ${ret_typ} (${msvc_call_conv}*${fn_name})(')
		for i, param in func.params {
			g.type_definitions.write_string(g.typ(param.typ))
			if i < func.params.len - 1 {
				g.type_definitions.write_string(',')
			}
		}
		g.type_definitions.writeln(')${call_conv_attribute_suffix};')
	}
}

pub fn (mut g Gen) write_array_fixed_return_types() {
	g.typedefs.writeln('\n// BEGIN_array_fixed_return_typedefs')
	g.type_definitions.writeln('\n// BEGIN_array_fixed_return_structs')

	for sym in g.table.type_symbols {
		if sym.kind != .array_fixed || (sym.info as ast.ArrayFixed).elem_type.has_flag(.generic)
			|| !(sym.info as ast.ArrayFixed).is_fn_ret {
			continue
		}
		info := sym.info as ast.ArrayFixed
		mut fixed_elem_name := g.typ(info.elem_type.set_nr_muls(0))
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
	for sym in g.table.type_symbols {
		if sym.kind != .multi_return {
			continue
		}
		if sym.cname.contains('[') {
			continue
		}
		info := sym.mr_info()
		if info.types.any(it.has_flag(.generic)) {
			continue
		}
		g.typedefs.writeln('typedef struct ${sym.cname} ${sym.cname};')
		g.type_definitions.writeln('struct ${sym.cname} {')
		for i, mr_typ in info.types {
			type_name := g.typ(mr_typ)
			if mr_typ.has_flag(.option) {
				// option in multi_return
				// Dont use g.typ() here because it will register
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
				// Dont use g.typ() here because it will register
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

pub fn (mut g Gen) new_tmp_var() string {
	g.tmp_count++
	return '_t${g.tmp_count}'
}

pub fn (mut g Gen) new_global_tmp_var() string {
	g.global_tmp_count++
	return '_t${g.global_tmp_count}'
}

pub fn (mut g Gen) new_tmp_declaration_name() string {
	g.tmp_count_declarations++
	return '_d${g.tmp_count_declarations}'
}

pub fn (mut g Gen) current_tmp_var() string {
	return '_t${g.tmp_count}'
}

/*
pub fn (mut g Gen) new_tmp_var2() string {
	g.tmp_count_af++
	return '_tt$g.tmp_count_af'
}
*/
pub fn (mut g Gen) reset_tmp_count() {
	g.tmp_count = 0
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
						if stmt.typ.has_flag(.option) {
							g.writeln('')
							g.write('${tmp_var} = ')
							g.expr(stmt.expr)
							g.writeln(';')
						} else {
							// on assignemnt or struct field initialization
							ret_typ := if g.inside_struct_init || g.inside_assign {
								stmt.typ
							} else {
								g.fn_decl.return_type.clear_flag(.option)
							}
							styp = g.base_type(ret_typ)
							g.write('_option_ok(&(${styp}[]) { ')
							g.expr_with_cast(stmt.expr, stmt.typ, ret_typ)
							g.writeln(' }, (${c.option_name}*)(&${tmp_var}), sizeof(${styp}));')
						}
					}
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
							g.write('_result_ok(&(${styp}[]) { ')
							g.expr_with_cast(stmt.expr, stmt.typ, ret_typ)
							g.writeln(' }, (${c.result_name}*)(&${tmp_var}), sizeof(${styp}));')
						}
					}
				}
			} else {
				g.set_current_pos_as_last_stmt_pos()
				g.skip_stmt_pos = true
				mut is_noreturn := false
				if stmt in [ast.Return, ast.BranchStmt] {
					is_noreturn = true
				} else if stmt is ast.ExprStmt {
					is_noreturn = is_noreturn_callexpr(stmt.expr)
				}
				if !is_noreturn {
					g.write('${tmp_var} = ')
				}
				g.stmt(stmt)
				if !g.out.last_n(2).contains(';') {
					g.writeln(';')
				}
			}
		} else {
			g.stmt(stmt)
			if (g.inside_if_option || g.inside_if_result || g.inside_match_option
				|| g.inside_match_result) && stmt is ast.ExprStmt {
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
		g.write('')
		g.write(')')
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
	if !ret_typ.has_flag(.option) && !ret_typ.has_flag(.result) {
		panic('cgen: parameter `ret_typ` of function `expr_with_tmp_var()` must be an Option or Result')
	}

	stmt_str := g.go_before_last_stmt().trim_space()
	mut styp := g.base_type(ret_typ)
	g.empty_line = true

	if g.table.sym(expr_typ).kind == .none_ {
		g.write('${g.typ(ret_typ)} ${tmp_var} = ')
		g.gen_option_error(ret_typ, expr)
		g.writeln(';')
	} else {
		mut is_ptr_to_ptr_assign := false
		if ret_typ.has_flag(.generic) {
			if expr is ast.SelectorExpr && g.cur_concrete_types.len == 0 {
				// resolve generic struct on selectorExpr inside non-generic function
				if expr.expr is ast.Ident && expr.expr.obj is ast.Var {
					if (expr.expr.obj as ast.Var).expr is ast.StructInit {
						g.cur_concrete_types << (g.table.sym(expr.expr.obj.typ).info as ast.Struct).concrete_types
					}
				}
			}
			styp = g.base_type(g.unwrap_generic(ret_typ))
			ret_styp := g.typ(g.unwrap_generic(ret_typ)).replace('*', '_ptr')
			g.writeln('${ret_styp} ${tmp_var};')
		} else {
			g.writeln('${g.typ(ret_typ)} ${tmp_var};')
		}
		if ret_typ.has_flag(.option) {
			if expr_typ.has_flag(.option) && expr in [ast.StructInit, ast.ArrayInit, ast.MapInit] {
				if expr is ast.StructInit && expr.init_fields.len > 0 {
					g.write('_option_ok(&(${styp}[]) { ')
				} else {
					g.write('_option_none(&(${styp}[]) { ')
				}
			} else {
				is_ptr_to_ptr_assign =
					(expr is ast.SelectorExpr || (expr is ast.Ident && !expr.is_auto_heap()))
					&& ret_typ.is_ptr() && expr_typ.is_ptr() && expr_typ.has_flag(.option)
				// option ptr assignment simplification
				if is_ptr_to_ptr_assign {
					g.write('${tmp_var} = ')
				} else if expr_typ.has_flag(.option) && expr is ast.PrefixExpr
					&& expr.right is ast.StructInit
					&& (expr.right as ast.StructInit).init_fields.len == 0 {
					g.write('_option_none(&(${styp}[]) { ')
				} else {
					g.write('_option_ok(&(${styp}[]) { ')
				}
				if !expr_typ.is_ptr() && ret_typ.is_ptr() {
					g.write('&/*ref*/')
				}
			}
		} else {
			g.write('_result_ok(&(${styp}[]) { ')
		}
		g.expr_with_cast(expr, expr_typ, ret_typ)
		if ret_typ.has_flag(.option) {
			if is_ptr_to_ptr_assign {
				g.writeln(';')
			} else {
				g.writeln(' }, (${c.option_name}*)(&${tmp_var}), sizeof(${styp}));')
			}
		} else {
			g.writeln(' }, (${c.result_name}*)(&${tmp_var}), sizeof(${styp}));')
		}
		g.set_current_pos_as_last_stmt_pos()
	}

	g.write(stmt_str)
	g.write(' ')
	g.write(tmp_var)
}

@[inline]
fn (mut g Gen) write_v_source_line_info(pos token.Pos) {
	if g.inside_ternary == 0 && g.pref.is_vlines && g.is_vlines_enabled {
		nline := pos.line_nr + 1
		lineinfo := '\n#line ${nline} "${g.vlines_path}"'
		$if trace_gen_source_line_info ? {
			eprintln('> lineinfo: ${lineinfo.replace('\n', '')}')
		}
		g.writeln(lineinfo)
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
	g.inside_call = false
	if !g.skip_stmt_pos {
		g.set_current_pos_as_last_stmt_pos()
	}
	match node {
		ast.AsmStmt {
			g.write_v_source_line_info(node.pos)
			g.asm_stmt(node)
		}
		ast.AssertStmt {
			g.write_v_source_line_info(node.pos)
			g.assert_stmt(node)
		}
		ast.AssignStmt {
			g.write_v_source_line_info(node.pos)
			g.assign_stmt(node)
		}
		ast.Block {
			g.write_v_source_line_info(node.pos)
			if node.is_unsafe {
				g.writeln('{ // Unsafe block')
			} else {
				g.writeln('{')
			}
			g.stmts(node.stmts)
			g.writeln('}')
		}
		ast.BranchStmt {
			g.write_v_source_line_info(node.pos)
			g.branch_stmt(node)
		}
		ast.ConstDecl {
			g.write_v_source_line_info(node.pos)
			g.const_decl(node)
		}
		ast.ComptimeFor {
			g.comptime_for(node)
		}
		ast.DeferStmt {
			mut defer_stmt := node
			defer_stmt.ifdef = g.defer_ifdef
			g.writeln('${g.defer_flag_var(defer_stmt)} = true;')
			g.defer_stmts << defer_stmt
		}
		ast.EmptyStmt {}
		ast.EnumDecl {
			g.enum_decl(node)
		}
		ast.ExprStmt {
			g.write_v_source_line_info(node.pos)
			// af := g.autofree && node.expr is ast.CallExpr && !g.is_builtin_mod
			// if af {
			// g.autofree_call_pregen(node.expr as ast.CallExpr)
			// }
			old_is_void_expr_stmt := g.is_void_expr_stmt
			g.is_void_expr_stmt = !node.is_expr
			if node.typ != ast.void_type && g.expected_cast_type != 0
				&& node.expr !in [ast.IfExpr, ast.MatchExpr] {
				g.expr_with_cast(node.expr, node.typ, g.expected_cast_type)
			} else {
				g.expr(node.expr)
			}
			g.is_void_expr_stmt = old_is_void_expr_stmt
			// if af {
			// g.autofree_call_postgen()
			// }
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
		ast.FnDecl {
			g.fn_decl(node)
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
			g.write_v_source_line_info(node.pos)
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
			g.write_v_source_line_info(node.pos)
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
			g.write_v_source_line_info(node.pos)
			g.for_stmt(node)
			g.branch_parent_pos = prev_branch_parent_pos
			g.labeled_loops.delete(node.label)
			g.inner_loop = save_inner_loop
		}
		ast.GlobalDecl {
			g.global_decl(node)
		}
		ast.GotoLabel {
			g.writeln('${c_name(node.name)}: {}')
		}
		ast.GotoStmt {
			g.write_v_source_line_info(node.pos)
			g.writeln('goto ${c_name(node.name)};')
		}
		ast.HashStmt {
			g.hash_stmt(node)
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
					}
					if method.return_type.has_flag(.result) {
						// Register a result if it's not registered yet
						g.register_result(method.return_type)
					}
				}
			}
		}
		ast.Module {
			g.is_builtin_mod = util.module_is_builtin(node.name)
			g.cur_mod = node
		}
		ast.NodeError {}
		ast.Return {
			g.return_stmt(node)
		}
		ast.SemicolonStmt {
			g.writeln(';')
		}
		ast.SqlStmt {
			g.sql_stmt(node)
		}
		ast.StructDecl {
			name := if node.language == .c {
				util.no_dots(node.name)
			} else if node.name in ['array', 'string'] {
				node.name
			} else {
				c_name(node.name)
			}
			// TODO For some reason, build fails with autofree with this line
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
			if node.is_union {
				g.typedefs.writeln('typedef union ${name} ${name};')
			} else {
				g.typedefs.writeln('typedef struct ${name} ${name};')
			}
		}
		ast.TypeDecl {
			if !g.pref.skip_unused {
				g.writeln('// TypeDecl')
			}
		}
	}
	if !g.skip_stmt_pos { // && g.stmt_path_pos.len > 0 {
		g.stmt_path_pos.delete_last()
	}
	// If we have temporary string exprs to free after this statement, do it. e.g.:
	// `foo('a' + 'b')` => `tmp := 'a' + 'b'; foo(tmp); string_free(&tmp);`
	if g.is_autofree {
		// if node is ast.ExprStmt {&& node.expr is ast.CallExpr {
		if node !is ast.FnDecl {
			// p := node.pos()
			// g.autofree_call_postgen(p.pos)
		}
	}
}

fn (mut g Gen) write_defer_stmts() {
	g.indent++
	for i := g.defer_stmts.len - 1; i >= 0; i-- {
		defer_stmt := g.defer_stmts[i]
		g.writeln('// Defer begin')
		g.writeln('if (${g.defer_flag_var(defer_stmt)}) {')
		g.indent++
		if defer_stmt.ifdef.len > 0 {
			g.writeln(defer_stmt.ifdef)
			g.stmts(defer_stmt.stmts)
			g.writeln('')
			g.writeln('#endif')
		} else {
			g.indent--
			g.stmts(defer_stmt.stmts)
			g.indent++
		}
		g.indent--
		g.writeln('}')
		g.writeln('// Defer end')
	}
	g.indent--
}

struct SumtypeCastingFn {
	fn_name string
	got     ast.Type
	exp     ast.Type
}

fn (mut g Gen) get_sumtype_casting_fn(got_ ast.Type, exp_ ast.Type) string {
	got, exp := got_.idx(), exp_.idx()
	i := got | int(u32(exp) << 16)
	exp_sym := g.table.sym(exp)
	mut got_sym := g.table.sym(got)
	cname := if exp == ast.int_type_idx { ast.int_type_name } else { exp_sym.cname }
	// fn_name := '${got_sym.cname}_to_sumtype_${exp_sym.cname}'
	fn_name := '${got_sym.cname}_to_sumtype_${cname}/*KEK*/'
	if got == exp || g.sumtype_definitions[i] {
		return fn_name
	}
	for got_sym.parent_idx != 0 && got_sym.idx !in (exp_sym.info as ast.SumType).variants {
		got_sym = g.table.sym(got_sym.parent_idx)
	}
	g.sumtype_definitions[i] = true
	g.sumtype_casting_fns << SumtypeCastingFn{
		fn_name: fn_name
		got: got_sym.idx
		exp: exp
	}
	return fn_name
}

fn (mut g Gen) write_sumtype_casting_fn(fun SumtypeCastingFn) {
	got, exp := fun.got, fun.exp
	got_sym, exp_sym := g.table.sym(got), g.table.sym(exp)
	mut got_cname, exp_cname := got_sym.cname, exp_sym.cname
	mut type_idx := g.type_sidx(got)
	mut sb := strings.new_builder(128)
	mut is_anon_fn := false
	if got_sym.info is ast.FnType {
		got_name := 'fn ${g.table.fn_type_source_signature(got_sym.info.func)}'
		got_cname = 'anon_fn_${g.table.fn_type_signature(got_sym.info.func)}'
		type_idx = g.table.type_idxs[got_name].str()
		exp_info := exp_sym.info as ast.SumType
		for variant in exp_info.variants {
			variant_sym := g.table.sym(variant)
			if variant_sym.info is ast.FnType {
				if g.table.fn_type_source_signature(variant_sym.info.func) == g.table.fn_type_source_signature(got_sym.info.func) {
					got_cname = variant_sym.cname
					type_idx = variant.idx().str()
					break
				}
			}
		}
		sb.writeln('static inline ${exp_cname} ${fun.fn_name}(${got_cname} x) {')
		sb.writeln('\t${got_cname} ptr = x;')
		is_anon_fn = true
	}
	if !is_anon_fn {
		sb.writeln('static inline ${exp_cname} ${fun.fn_name}(${got_cname}* x) {')
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
	sb.write_string('\treturn (${exp_cname}){ ._${got_cname} = ptr, ._typ = ${type_idx}')
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
		field_styp := g.typ(field.typ)
		if got_sym.kind in [.sum_type, .interface_] {
			// the field is already a wrapped pointer; we shouldn't wrap it once again
			sb.write_string(', .${field.name} = ptr->${field.name}')
		} else {
			sb.write_string(', .${field.name} = (${field_styp}*)((char*)${ptr} + __offsetof_ptr(${ptr}, ${type_cname}, ${field.name}))')
		}
	}
	sb.writeln('};\n}')
	g.auto_fn_definitions << sb.str()
}

fn (mut g Gen) call_cfn_for_casting_expr(fname string, expr ast.Expr, exp_is_ptr bool, exp_styp string, got_is_ptr bool, got_is_fn bool, got_styp string) {
	mut rparen_n := 1
	if exp_is_ptr {
		g.write('HEAP(${exp_styp}, ')
		rparen_n++
	}
	g.write('${fname}(')
	if !got_is_ptr && !got_is_fn {
		if !expr.is_lvalue() || (expr is ast.Ident && expr.obj.is_simple_define_const()) {
			// Note: the `_to_sumtype_` family of functions do call memdup internally, making
			// another duplicate with the HEAP macro is redundant, so use ADDR instead:
			promotion_macro_name := if fname.contains('_to_sumtype_') { 'ADDR' } else { 'HEAP' }
			g.write('${promotion_macro_name}(${got_styp}, (')
			rparen_n += 2
		} else {
			g.write('&')
		}
	}
	if got_styp == 'none' && !g.cur_fn.return_type.has_flag(.option) {
		g.write('(none){EMPTY_STRUCT_INITIALIZATION}')
	} else {
		g.expr(expr)
	}
	g.write(')'.repeat(rparen_n))
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
	if got_sym.kind == .none_ && exp_sym.idx == ast.error_type_idx {
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
			g.call_cfn_for_casting_expr(fname, expr, expected_is_ptr, exp_styp, true,
				false, got_styp)
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
			fname = '/*${exp_sym}*/${fname}'
			if exp_sym.info.is_generic {
				fname = g.generic_fn_name(exp_sym.info.concrete_types, fname)
			}
			g.call_cfn_for_casting_expr(fname, expr, expected_is_ptr, exp_styp, got_is_ptr,
				false, got_styp)
		}
		return
	}
	// cast to sum type
	exp_styp := g.typ(expected_type)
	mut got_styp := g.typ(got_type)
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
				if v := scope.find_struct_field(expr.expr.str(), expr.expr_type, expr.field_name) {
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
					g.write(stmt_str)
					g.write(' ')
					g.write('${fname}(&${tmp_var})')
					return
				} else {
					g.call_cfn_for_casting_expr(fname, expr, expected_is_ptr, unwrapped_exp_sym.cname,
						got_is_ptr, got_is_fn, got_styp)
				}
			}
			return
		}
	}
	// Generic dereferencing logic
	neither_void := ast.voidptr_type !in [got_type, expected_type]
		&& ast.nil_type !in [got_type, expected_type]
	if expected_type.has_flag(.shared_f) && !got_type_raw.has_flag(.shared_f)
		&& !expected_type.has_flag(.option) && !expected_type.has_flag(.result) {
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
		g.write('->val')
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
	if exp_sym.kind == .function && !expected_type.has_flag(.option)
		&& !expected_type.has_flag(.result) {
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
		b.write_u8(c)
	}
	res := b.str()
	return res
}

// cestring returns a V string, properly escaped for embedding in a C string literal.
fn cestring(s string) string {
	return s.replace('\\', '\\\\').replace('"', "'")
}

// ctoslit returns a '_SLIT("$s")' call, where s is properly escaped.
fn ctoslit(s string) string {
	return '_SLIT("' + cescape_nonascii(cestring(s)) + '")'
}

fn (mut g Gen) gen_attrs(attrs []ast.Attr) {
	if g.pref.skip_unused {
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
		// swap destination and operands for att syntax
		if template.args.len != 0 && !template.is_directive {
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
		g.write('"')
		g.write(clob.reg.name)
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
			g.write('\$${arg.val}')
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
			if !stmt.is_basic {
				g.write('%') // escape percent with percent in extended assembly
			}
			g.write('%${arg.name}')
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
	ret_styp := g.typ(func.func.return_type)
	g.write('${ret_styp} (*${ptr_name}) (')
	arg_len := func.func.params.len
	for i, arg in func.func.params {
		arg_styp := g.typ(arg.typ)
		g.write('${arg_styp} ${arg.name}')
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

fn (mut g Gen) gen_clone_assignment(val ast.Expr, typ ast.Type, add_eq bool) bool {
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
			shared_styp := g.typ(typ.set_nr_muls(0))
			if typ.share() == .shared_t {
				g.write('(${shared_styp}*)__dup_shared_array(&(${shared_styp}){.mtx = {0}, .val =')
			}
			g.write(' array_clone_static_to_depth(')
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
		} else if right_sym.kind == .string {
			// `str1 = str2` => `str1 = str2.clone()`
			g.write(' string_clone_static(')
			g.expr(val)
			g.write(')')
		}
	}
	return true
}

fn (mut g Gen) autofree_scope_vars(pos int, line_nr int, free_parent_scopes bool) {
	g.autofree_scope_vars_stop(pos, line_nr, free_parent_scopes, -1)
}

fn (mut g Gen) autofree_scope_vars_stop(pos int, line_nr int, free_parent_scopes bool, stop_pos int) {
	if g.is_builtin_mod {
		// In `builtin` everything is freed manually.
		return
	}
	if pos == -1 {
		// TODO why can pos be -1?
		return
	}
	// eprintln('> free_scope_vars($pos)')
	scope := g.file.scope.innermost(pos)
	if scope.start_pos == 0 {
		// TODO why can scope.pos be 0? (only outside fns?)
		return
	}
	g.trace_autofree('// autofree_scope_vars(pos=${pos} line_nr=${line_nr} scope.pos=${scope.start_pos} scope.end_pos=${scope.end_pos})')
	g.autofree_scope_vars2(scope, scope.start_pos, scope.end_pos, line_nr, free_parent_scopes,
		stop_pos)
}

@[if trace_autofree ?]
fn (mut g Gen) trace_autofree(line string) {
	g.writeln(line)
}

fn (mut g Gen) autofree_scope_vars2(scope &ast.Scope, start_pos int, end_pos int, line_nr int, free_parent_scopes bool, stop_pos int) {
	if scope == unsafe { nil } {
		return
	}
	for _, obj in scope.objects {
		match obj {
			ast.Var {
				g.trace_autofree('// var "${obj.name}" var.pos=${obj.pos.pos} var.line_nr=${obj.pos.line_nr}')
				if obj.name == g.returned_var_name {
					g.trace_autofree('// skipping returned var')
					continue
				}
				if obj.is_or {
					// Skip vars inited with the `or {}`, since they are generated
					// after the or block in C.
					g.trace_autofree('// skipping `or {}` var "${obj.name}"')
					continue
				}
				if obj.is_tmp {
					// Skip for loop vars
					g.trace_autofree('// skipping tmp var "${obj.name}"')
					continue
				}
				if obj.is_inherited {
					g.trace_autofree('// skipping inherited var "${obj.name}"')
					continue
				}
				// if var.typ == 0 {
				// // TODO why 0?
				// continue
				// }
				// if v.pos.pos > end_pos {
				if obj.pos.pos > end_pos || (obj.pos.pos < start_pos && obj.pos.line_nr == line_nr) {
					// Do not free vars that were declared after this scope
					continue
				}
				is_option := obj.typ.has_flag(.option)
				if is_option {
					// TODO: free options
					continue
				}
				g.autofree_variable(obj)
			}
			else {}
		}
	}
	for g.autofree_scope_stmts.len > 0 {
		g.write(g.autofree_scope_stmts.pop())
	}
	// Free all vars in parent scopes as well:
	// ```
	// s := ...
	// if ... {
	// s.free()
	// return
	// }
	// ```
	// if scope.parent != unsafe { nil } && line_nr > 0 {
	if free_parent_scopes && scope.parent != unsafe { nil } && !scope.detached_from_parent
		&& (stop_pos == -1 || scope.parent.start_pos >= stop_pos) {
		g.trace_autofree('// af parent scope:')
		g.autofree_scope_vars2(scope.parent, start_pos, end_pos, line_nr, true, stop_pos)
	}
}

fn (mut g Gen) autofree_variable(v ast.Var) {
	// filter out invalid variables
	if v.typ == 0 {
		return
	}
	sym := g.table.sym(v.typ)
	// if v.name.contains('output2') {
	if g.is_autofree {
		// eprintln('   > var name: ${v.name:-20s} | is_arg: ${v.is_arg.str():6} | var type: ${int(v.typ):8} | type_name: ${sym.name:-33s}')
	}
	// }
	free_fn := g.typ(v.typ.set_nr_muls(0)) + '_free'
	if sym.kind == .array {
		if sym.has_method('free') {
			g.autofree_var_call(free_fn, v)
			return
		}
		g.autofree_var_call('array_free', v)
		return
	}
	if sym.kind == .string {
		// Don't free simple string literals.
		match v.expr {
			ast.StringLiteral {
				g.trace_autofree('// str literal')
			}
			else {
				// NOTE/TODO: assign_stmt multi returns variables have no expr
				// since the type comes from the called fns return type
				/*
				f := v.name[0]
				if
					//!(f >= `a` && f <= `d`) {
					//f != `c` {
					v.name!='cvar_name' {
					t := typeof(v.expr)
				return '// other ' + t + '\n'
				}
				*/
			}
		}
		g.autofree_var_call('string_free', v)
		return
	}
	if g.pref.experimental && v.typ.is_ptr() && sym.name.after('.')[0].is_capital() {
		// Free user reference types
		g.autofree_var_call('free', v)
	}
	if sym.has_method('free') {
		g.autofree_var_call(free_fn, v)
	}
}

fn (mut g Gen) autofree_var_call(free_fn_name string, v ast.Var) {
	if v.is_arg {
		// fn args should not be autofreed
		return
	}
	if v.is_used && v.is_autofree_tmp {
		// tmp expr vars do not need to be freed again here
		return
	}
	if g.is_builtin_mod {
		return
	}
	if !g.is_autofree {
		return
	}
	// if v.is_autofree_tmp && !g.doing_autofree_tmp {
	// return
	// }
	if v.name.contains('expr_write_string_1_') {
		// TODO remove this temporary hack
		return
	}
	mut af := strings.new_builder(128)
	if v.typ.is_ptr() {
		af.write_string('\t')
		if v.typ.share() == .shared_t {
			af.write_string(free_fn_name.replace_each(['__shared__', '']))
		} else {
			af.write_string(free_fn_name)
		}
		af.write_string('(')
		if v.typ.share() == .shared_t {
			af.write_string('&')
		}
		af.write_string(strings.repeat(`*`, v.typ.nr_muls() - 1)) // dereference if it is a pointer to a pointer
		af.write_string(c_name(v.name))
		if v.typ.share() == .shared_t {
			af.write_string('->val')
		}

		af.writeln('); // autofreed ptr var')
	} else {
		if v.typ == ast.error_type && !v.is_autofree_tmp {
			return
		}
		if v.is_auto_heap {
			af.writeln('\t${free_fn_name}(${c_name(v.name)}); // autofreed heap var ${g.cur_mod.name} ${g.is_builtin_mod}')
		} else {
			af.writeln('\t${free_fn_name}(&${c_name(v.name)}); // autofreed var ${g.cur_mod.name} ${g.is_builtin_mod}')
		}
	}
	g.autofree_scope_stmts << af.str()
}

fn (mut g Gen) map_fn_ptrs(key_typ ast.TypeSymbol) (string, string, string, string) {
	mut hash_fn := ''
	mut key_eq_fn := ''
	mut clone_fn := ''
	mut free_fn := '&map_free_nop'
	match key_typ.kind {
		.alias {
			alias_key_type := (key_typ.info as ast.Alias).parent_type
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
		.int, .i32, .u32, .rune, .f32, .enum_ {
			// XTODO i64
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
			verror('map key type `${key_typ.name}` not supported')
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
			if g.is_shared && !ret_type.has_flag(.shared_f) && !g.inside_or_block {
				ret_sym := g.table.sym(ret_type)
				shared_typ := if ret_type.is_ptr() {
					ret_type.deref().set_flag(.shared_f)
				} else {
					ret_type.set_flag(.shared_f)
				}
				shared_styp = g.typ(shared_typ)
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

			if g.is_shared && !ret_type.has_flag(.shared_f) && !g.inside_or_block
				&& ret_type.is_ptr() {
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
			if g.is_shared && !ret_type.has_flag(.shared_f) && !g.inside_or_block {
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
			elem_typ_str := g.typ(node.elem_type)
			noscan := g.check_noscan(node.elem_type)
			g.write('sync__new_channel_st${noscan}(')
			if node.has_cap {
				g.expr(node.cap_expr)
			} else {
				g.write('0')
			}
			g.write(', sizeof(')
			g.write(elem_typ_str)
			g.write(')>0 ? sizeof(')
			g.write(elem_typ_str)
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
				pos: node.pos
				call_expr: node.call_expr
				is_expr: node.is_expr
			}, .go_)
			g.is_arraymap_set = old_is_arraymap_set
		}
		ast.Ident {
			g.ident(node)
		}
		ast.IfExpr {
			g.if_expr(node)
		}
		ast.IfGuardExpr {
			g.write('/* guard */')
		}
		ast.IndexExpr {
			g.index_expr(node)
		}
		ast.InfixExpr {
			if node.op in [.left_shift, .plus_assign, .minus_assign] {
				g.inside_map_infix = true
				g.infix_expr(node)
				g.inside_map_infix = false
			} else {
				g.infix_expr(node)
			}
		}
		ast.IntegerLiteral {
			if node.val.starts_with('0o') {
				g.write('0')
				g.write(node.val[2..])
			} else if node.val.starts_with('-0o') {
				g.write('-0')
				g.write(node.val[3..])
			} else {
				g.write(node.val)
			}
		}
		ast.IsRefType {
			typ := g.resolve_comptime_type(node.expr, g.get_type(node.typ))
			node_typ := g.unwrap_generic(typ)
			sym := g.table.sym(node_typ)
			if sym.language == .v && sym.kind in [.placeholder, .any] {
				g.error('unknown type `${sym.name}`', node.pos)
			}
			is_ref_type := g.contains_ptr(node_typ)
			g.write('/*IsRefType*/ ${is_ref_type}')
		}
		ast.LambdaExpr {
			g.gen_anon_fn(mut node.func)
			// g.write('/* lambda expr: ${node_.str()} */')
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
			styp := g.typ(node.struct_type)
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
				if mut node.expr is ast.ComptimeSelector && node.expr.left is ast.Ident {
					// val.$(field.name)?
					expr_str = '${node.expr.left.str()}.${g.comptime.comptime_for_field_value.name}'
				} else if mut node.expr is ast.Ident && g.comptime.is_comptime_var(node.expr) {
					// val?
					expr_str = node.expr.name
				}
				g.writeln('if (${expr_str}.state != 0) {')
				g.writeln('\tpanic_option_not_set(_SLIT("none"));')
				g.writeln('}')
				g.write(cur_line)
				typ := g.resolve_comptime_type(node.expr, node.typ)
				g.write('*(${g.base_type(typ)}*)&')
				g.expr(node.expr)
				g.write('.data')
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
				styp := g.typ(node.right_type)
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
					opt_elem_type := g.typ(elem_type.set_flag(.option))
					g.register_chan_pop_option_call(opt_elem_type, styp)
					g.write('${opt_elem_type} ${tmp_opt} = __Option_${styp}_popval(')
				} else {
					g.write('__${styp}_popval(')
				}
				g.expr(node.right)
				g.write(')')
				if gen_or {
					if !node.is_option {
						g.write('/*JJJ*/')
						g.or_block(tmp_opt, node.or_block, elem_type)
					}
					if is_gen_or_and_assign_rhs {
						elem_styp := g.typ(elem_type)
						g.write(';\n${cur_line}*(${elem_styp}*)${tmp_opt}.data')
					}
				}
			} else {
				if g.is_option_auto_heap {
					g.write('(${g.base_type(node.right_type)}*)')
				}
				if !g.is_option_auto_heap && !(g.is_amp && node.right.is_auto_deref_var()) {
					g.write(node.op.str())
				}
				g.expr(node.right)
				if g.is_option_auto_heap {
					g.write('.data')
				}
			}
			g.is_amp = false
		}
		ast.RangeExpr {
			// Only used in IndexExpr
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
			g.sql_select_expr(node)
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
			g.write('${sidx} /* ${sym.name} */')
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
	if node.val.len_utf8() < node.val.len {
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
	g.write('_SLIT("${util.strip_main_name(s)}")')
}

fn (mut g Gen) typeof_expr(node ast.TypeOf) {
	typ := g.resolve_comptime_type(node.expr, g.get_type(node.typ))
	sym := g.table.sym(typ)
	if sym.kind == .sum_type {
		// When encountering a .sum_type, typeof() should be done at runtime,
		// because the subtype of the expression may change:
		g.write('charptr_vstring_literal( /* ${sym.name} */ v_typeof_sumtype_${sym.cname}( (')
		g.expr(node.expr)
		g.write(')._typ ))')
	} else if sym.kind == .array_fixed {
		fixed_info := sym.info as ast.ArrayFixed
		typ_name := g.table.get_type_name(fixed_info.elem_type)
		g.write('_SLIT("[${fixed_info.size}]${util.strip_main_name(typ_name)}")')
	} else if sym.kind == .function {
		info := sym.info as ast.FnType
		g.write('_SLIT("${g.fn_decl_str(info)}")')
	} else if typ.has_flag(.variadic) {
		varg_elem_type_sym := g.table.sym(g.table.value_type(typ))
		g.write('_SLIT("...${util.strip_main_name(varg_elem_type_sym.name)}")')
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
			.unknown {
				// ast.TypeOf of `typeof(string).idx` etc
				if node.field_name == 'name' {
					// typeof(expr).name
					mut name_type := node.name_type
					if node.expr is ast.TypeOf {
						name_type = g.resolve_comptime_type(node.expr.expr, name_type)
					}
					g.type_name(name_type)
					return
				} else if node.field_name == 'idx' {
					mut name_type := node.name_type
					if node.expr is ast.TypeOf {
						name_type = g.resolve_comptime_type(node.expr.expr, name_type)
					}
					// `typeof(expr).idx`
					g.write(int(g.unwrap_generic(name_type)).str())
					return
				}
				g.error('unknown generic field', node.pos)
			}
		}
	}
	if node.expr_type == 0 {
		g.checker_bug('unexpected SelectorExpr.expr_type = 0', node.pos)
	}
	sym := g.table.sym(g.unwrap_generic(node.expr_type))
	field_name := if sym.language == .v { c_name(node.field_name) } else { node.field_name }

	if node.or_block.kind != .absent && !g.is_assign_lhs && g.table.sym(node.typ).kind != .chan {
		is_ptr := sym.kind in [.interface_, .sum_type]
		stmt_str := g.go_before_last_stmt().trim_space()
		styp := g.typ(g.unwrap_generic(node.typ))
		g.empty_line = true
		tmp_var := g.new_tmp_var()
		g.write('${styp} ${tmp_var} = ')
		if is_ptr {
			g.write('*(')
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
		if is_ptr {
			g.write(')')
		}
		g.or_block(tmp_var, node.or_block, node.typ)
		g.write(stmt_str)
		g.write(' ')
		unwrapped_typ := node.typ.clear_option_and_result()
		unwrapped_styp := g.typ(unwrapped_typ)
		g.write('(*(${unwrapped_styp}*)${tmp_var}.data)')
		return
	}

	// if node expr is a root ident and an optional
	mut is_opt_or_res := node.expr is ast.Ident && node.expr_type.has_option_or_result()
	if is_opt_or_res {
		opt_base_typ := g.base_type(node.expr_type)
		g.write('(*(${opt_base_typ}*)')
	}
	if sym.kind == .array_fixed {
		if node.field_name != 'len' {
			g.error('field_name should be `len`', node.pos)
		}
		info := sym.info as ast.ArrayFixed
		g.write('${info.size}')
		return
	} else if sym.kind == .chan && (node.field_name == 'len' || node.field_name == 'closed') {
		g.write('sync__Channel_${node.field_name}(')
		g.expr(node.expr)
		g.write(')')
		return
	} else if g.enum_data_type == node.typ {
		g.expr(node.expr)
		g.write('.')
		g.write(node.field_name)
		return
	}
	mut sum_type_deref_field := ''
	mut sum_type_dot := '.'
	mut field_typ := ast.void_type
	if f := g.table.find_field_with_embeds(sym, node.field_name) {
		field_sym := g.table.sym(f.typ)
		field_typ = f.typ
		if sym.kind in [.interface_, .sum_type] {
			g.write('(*(')
		}
		if field_sym.kind in [.sum_type, .interface_] {
			if !prevent_sum_type_unwrapping_once {
				// check first if field is sum type because scope searching is expensive
				scope := g.file.scope.innermost(node.pos.pos)
				if field := scope.find_struct_field(node.expr.str(), node.expr_type, node.field_name) {
					if field.orig_type.is_ptr() {
						sum_type_dot = '->'
					}
					for i, typ in field.smartcasts {
						g.write('(')
						if field_sym.kind == .sum_type {
							g.write('*')
						}
						cast_sym := g.table.sym(g.unwrap_generic(typ))
						if field_sym.kind == .interface_ && cast_sym.kind == .interface_ {
							ptr := '*'.repeat(field.typ.nr_muls())
							dot := if node.expr_type.is_ptr() { '->' } else { '.' }
							g.write('I_${field_sym.cname}_as_I_${cast_sym.cname}(${ptr}${node.expr}${dot}${node.field_name}))')
							return
						} else {
							if i != 0 {
								dot := if field.typ.is_ptr() { '->' } else { '.' }
								sum_type_deref_field += ')${dot}'
							}
							if cast_sym.info is ast.Aggregate {
								agg_sym := g.table.sym(cast_sym.info.types[g.aggregate_type_idx])
								sum_type_deref_field += '_${agg_sym.cname}'
							} else {
								sum_type_deref_field += '_${cast_sym.cname}'
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
				g.write('${g.typ(node.expr_type.idx())}_${m.name}')
				return
			}
			receiver := m.params[0]
			expr_styp := g.typ(node.expr_type.idx())
			data_styp := g.typ(receiver.typ.idx())
			mut sb := strings.new_builder(256)
			name := '_V_closure_${expr_styp}_${m.name}_${node.pos.pos}'
			sb.write_string('${g.typ(m.return_type)} ${name}(')
			for i in 1 .. m.params.len {
				param := m.params[i]
				if i != 1 {
					sb.write_string(', ')
				}
				sb.write_string('${g.typ(param.typ)} a${i}')
			}
			sb.writeln(') {')
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
			if rec_sym.info is ast.Interface {
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

			g.write('__closure_create(${name}, ')
			if !receiver.typ.is_ptr() {
				g.write('memdup_uncollectable(')
			}
			if !node.expr_type.is_ptr() {
				g.write('&')
			}
			g.expr(node.expr)
			if !receiver.typ.is_ptr() {
				g.write(', sizeof(${expr_styp}))')
			}
			g.write(')')
			return
		}
	} else {
		if sym.kind in [.interface_, .sum_type] {
			g.write('(*(')
		}
	}
	// var?.field_opt
	field_is_opt := node.expr is ast.Ident && node.expr.is_auto_heap()
		&& node.expr.or_expr.kind != .absent && field_typ.has_flag(.option)
	if field_is_opt {
		g.write('((${g.base_type(field_typ)})')
	}
	n_ptr := node.expr_type.nr_muls() - 1
	if n_ptr > 0 {
		g.write('(')
		g.write('*'.repeat(n_ptr))
		g.expr(node.expr)
		g.write(')')
	} else {
		g.expr(node.expr)
	}
	if field_is_opt {
		g.write(')')
	}
	if is_opt_or_res {
		g.write('.data)')
	}
	// struct embedding
	if sym.info in [ast.Struct, ast.Aggregate] {
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
	}
	alias_to_ptr := sym.info is ast.Alias && sym.info.parent_type.is_ptr()
	if field_is_opt
		|| ((node.expr_type.is_ptr() || sym.kind == .chan || alias_to_ptr)
		&& node.from_embed_types.len == 0) {
		g.write('->')
	} else {
		g.write('.')
	}
	if node.expr_type.has_flag(.shared_f) {
		g.write('val.')
	}
	if node.expr_type == 0 {
		verror('cgen: SelectorExpr | expr_type: 0 | it.expr: `${node.expr}` | field: `${node.field_name}` | file: ${g.file.path} | line: ${node.pos.line_nr}')
	}
	g.write(field_name)
	if sum_type_deref_field != '' {
		g.write('${sum_type_dot}${sum_type_deref_field})')
	}
	if sym.kind in [.interface_, .sum_type] {
		g.write('))')
	}
}

fn (mut g Gen) enum_decl(node ast.EnumDecl) {
	enum_name := util.no_dots(node.name)
	is_flag := node.is_flag
	if g.pref.ccompiler == 'msvc' {
		mut last_value := '0'
		enum_typ_name := g.table.get_type_name(node.typ)
		g.enum_typedefs.writeln('')
		g.enum_typedefs.writeln('typedef ${enum_typ_name} ${enum_name};')
		for i, field in node.fields {
			g.enum_typedefs.write_string('\t#define ${enum_name}__${field.name} ')
			g.enum_typedefs.write_string('(')
			if is_flag {
				g.enum_typedefs.write_string((u64(1) << i).str())
				g.enum_typedefs.write_string('ULL')
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
			g.enum_typedefs.write_string((u64(1) << i).str())
			g.enum_typedefs.write_string('U')
			cur_enum_offset = 0
		}
		cur_value := if cur_enum_offset > 0 {
			'${cur_enum_expr}+${cur_enum_offset}'
		} else {
			cur_enum_expr
		}
		g.enum_typedefs.writeln(', // ${cur_value}')
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
		styp := g.typ(node.typ)
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
		g.writeln('uintptr_t _arr_${mtxs}[${node.lockeds.len}];')
		g.writeln('bool _isrlck_${mtxs}[${node.lockeds.len}];')
		mut j := 0
		for i, is_rlock in node.is_rlock {
			if !is_rlock {
				g.write('_arr_${mtxs}[${j}] = (uintptr_t)&')
				g.expr(node.lockeds[i])
				g.writeln('->mtx;')
				g.writeln('_isrlck_${mtxs}[${j}] = false;')
				j++
			}
		}
		for i, is_rlock in node.is_rlock {
			if is_rlock {
				g.write('_arr_${mtxs}[${j}] = (uintptr_t)&')
				g.expr(node.lockeds[i])
				g.writeln('->mtx;')
				g.writeln('_isrlck_${mtxs}[${j}] = true;')
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
		g.writeln('for (int ${mtxs}=0; ${mtxs}<${node.lockeds.len}; ${mtxs}++) {')
		g.writeln('\tif (${mtxs} && _arr_${mtxs}[${mtxs}] == _arr_${mtxs}[${mtxs}-1]) continue;')
		g.writeln('\tif (_isrlck_${mtxs}[${mtxs}])')
		g.writeln('\t\tsync__RwMutex_rlock((sync__RwMutex*)_arr_${mtxs}[${mtxs}]);')
		g.writeln('\telse')
		g.writeln('\t\tsync__RwMutex_lock((sync__RwMutex*)_arr_${mtxs}[${mtxs}]);')
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
		g.write(cur_line)
		g.write('${tmp_result}')
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
	key_typ_str := g.typ(unwrap_key_typ)
	value_typ_str := g.typ(unwrap_val_typ)
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
		shared_styp = g.typ(shared_typ)
		g.writeln('(${shared_styp}*)__dup_shared_map(&(${shared_styp}){.mtx = {0}, .val =')
	} else if is_amp {
		styp = g.typ(node.typ)
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
		if value_sym.kind == .function {
			g.writeln('new_map_init${noscan}(${hash_fn}, ${key_eq_fn}, ${clone_fn}, ${free_fn}, ${size}, sizeof(${key_typ_str}), sizeof(voidptr),')
		} else {
			g.writeln('new_map_init${noscan}(${hash_fn}, ${key_eq_fn}, ${clone_fn}, ${free_fn}, ${size}, sizeof(${key_typ_str}), sizeof(${value_typ_str}),')
		}
		g.writeln('\t\t_MOV((${key_typ_str}[${size}]){')
		for expr in node.keys {
			g.write('\t\t\t')
			g.expr(expr)
			g.writeln(', ')
		}
		g.writeln('\t\t}),')
		if value_sym.kind == .function {
			g.writeln('\t\t_MOV((voidptr[${size}]){')
		} else {
			g.writeln('\t\t_MOV((${value_typ_str}[${size}]){')
		}
		for i, expr in node.vals {
			g.write('\t\t\t')
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
		g.writeln('\t\t})')
		g.writeln('\t)')
	} else {
		g.write('new_map${noscan}(sizeof(${key_typ_str}), sizeof(${value_typ_str}), ${hash_fn}, ${key_eq_fn}, ${clone_fn}, ${free_fn})')
	}
	g.writeln('')
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
						el_stype := g.typ(ast.mktyp(expr.right_type))
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
						el_stype := g.typ(branch.stmt.right_types[0])
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
	g.write('int ${select_result} = sync__channel_select(&/*arr*/${chan_array}, ${directions_array}, &/*arr*/${objs_array}, ')
	if has_timeout {
		g.expr(timeout_expr)
	} else if has_else {
		g.write('0')
	} else {
		g.write('_const_time__infinite')
	}
	g.writeln(');')
	// free the temps that were created
	g.writeln('array_free(&${objs_array});')
	g.writeln('array_free(&${directions_array});')
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
		g.write(cur_line)
		g.write('(${select_result} != -2)')
	}
}

@[inline]
pub fn (mut g Gen) is_generic_param_var(node ast.Expr) bool {
	return node is ast.Ident && node.info is ast.IdentVar && node.obj is ast.Var
		&& (node.obj as ast.Var).ct_type_var == .generic_param
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
	mut name := c_name(node.name)
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
			// TODO globals hack
			g.write('_const_')
		}
	}
	mut is_auto_heap := node.is_auto_heap()
	mut is_option := false
	if node.info is ast.IdentVar {
		if node.obj is ast.Var {
			if !g.is_assign_lhs
				&& node.obj.ct_type_var !in [.smartcast, .generic_param, .no_comptime] {
				comptime_type := g.comptime.get_comptime_var_type(node)
				if comptime_type.has_flag(.option) {
					if (g.inside_opt_or_res || g.left_is_opt) && node.or_expr.kind == .absent {
						if !g.is_assign_lhs && is_auto_heap {
							g.write('(*${name})')
						} else {
							g.write(name)
						}
					} else {
						g.write('/*opt*/')
						styp := g.base_type(comptime_type)
						if is_auto_heap {
							g.write('(*(${styp}*)${name}->data)')
						} else {
							g.write('(*(${styp}*)${name}.data)')
						}
					}
				} else {
					g.write(name)
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
			if (g.inside_opt_or_res || g.left_is_opt) && node.or_expr.kind == .absent {
				if !g.is_assign_lhs && is_auto_heap {
					g.write('(*${name})')
				} else {
					g.write(name)
				}
			} else {
				g.write('/*opt*/')
				styp := g.base_type(node.info.typ)
				if is_auto_heap {
					g.write('(*(${styp}*)${name}->data)')
				} else {
					type_sym := g.table.sym(node.info.typ)
					if type_sym.kind == .alias {
						// Alias to Option type
						parent_typ := (type_sym.info as ast.Alias).parent_type
						if parent_typ.has_flag(.option) {
							g.write('*((${g.base_type(parent_typ)}*)')
						}
						g.write('(*(${styp}*)${name}.data)')
						if parent_typ.has_flag(.option) {
							g.write('.data)')
						}
					} else {
						g.write('(*(${styp}*)${name}.data)')
					}
				}
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
			if node.obj.smartcasts.len > 0 {
				obj_sym := g.table.sym(node.obj.typ)
				if !prevent_sum_type_unwrapping_once {
					for _, typ in node.obj.smartcasts {
						is_option_unwrap := is_option && typ == node.obj.typ.clear_flag(.option)
						g.write('(')
						if obj_sym.kind == .sum_type && !is_auto_heap {
							if is_option {
								if !is_option_unwrap {
									g.write('*(')
								}
								styp := g.base_type(node.obj.typ)
								g.write('*(${styp}*)')
							} else {
								g.write('*')
							}
						} else if (g.inside_interface_deref && g.table.is_interface_var(node.obj))
							|| node.obj.ct_type_var == .smartcast {
							g.write('*')
						} else if is_option {
							g.write('*(${g.base_type(node.obj.typ)}*)')
						}
					}
					for i, typ in node.obj.smartcasts {
						is_option_unwrap := is_option && typ == node.obj.typ.clear_flag(.option)
						cast_sym := g.table.sym(g.unwrap_generic(typ))
						if obj_sym.kind == .interface_ && cast_sym.kind == .interface_ {
							ptr := '*'.repeat(node.obj.typ.nr_muls())
							g.write('I_${obj_sym.cname}_as_I_${cast_sym.cname}(${ptr}${node.name})')
						} else {
							mut is_ptr := false
							if i == 0 {
								if node.obj.is_inherited {
									g.write(closure_ctx + '->')
								}
								g.write(name)
								if node.obj.orig_type.is_ptr() {
									is_ptr = true
								}
							}
							dot := if is_ptr || is_auto_heap { '->' } else { '.' }
							if cast_sym.info is ast.Aggregate {
								sym := g.table.sym(cast_sym.info.types[g.aggregate_type_idx])
								g.write('${dot}_${sym.cname}')
							} else {
								if is_option {
									g.write('.data')
									if !is_option_unwrap {
										g.write(')')
									}
								}
								if node.obj.ct_type_var == .smartcast {
									cur_variant_sym := g.table.sym(g.unwrap_generic(g.comptime.get_comptime_var_type(node)))
									g.write('${dot}_${cur_variant_sym.cname}')
								} else if !is_option_unwrap
									&& obj_sym.kind in [.sum_type, .interface_] {
									g.write('${dot}_${cast_sym.cname}')
								}
							}
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
		// TODO PERF fn lookup for each fn call in translated mode
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

		if g.pref.obfuscate && g.cur_mod.name == 'main' && name.starts_with('main__') {
			key := node.name
			g.write('/* obf identfn: ${key} */')
			name = g.obf_table[key] or {
				panic('cgen: obf name "${key}" not found, this should never happen')
			}
		}
	}
	g.write(g.get_ternary_name(name))
	if is_auto_heap {
		g.write('))')
		if is_option {
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
	if (node.expr is ast.Ident && g.comptime.is_comptime_var(node.expr))
		|| node.expr is ast.ComptimeSelector {
		expr_type = g.unwrap_generic(g.comptime.get_comptime_var_type(node.expr))
	}
	if sym.kind in [.sum_type, .interface_] {
		if node.typ.has_flag(.option) && node.expr is ast.None {
			g.gen_option_error(node.typ, node.expr)
		} else if node.typ.has_flag(.option) {
			g.expr_with_opt(node.expr, expr_type, node.typ)
		} else {
			g.expr_with_cast(node.expr, expr_type, node_typ)
		}
	} else if !node.typ.has_flag(.option) && sym.kind == .struct_ && !node.typ.is_ptr()
		&& !(sym.info as ast.Struct).is_typedef {
		// deprecated, replaced by Struct{...exr}
		styp := g.typ(node.typ)
		g.write('*((${styp} *)(&')
		g.expr(node.expr)
		g.write('))')
	} else if sym.kind == .alias && g.table.final_sym(node.typ).kind == .array_fixed {
		if node.expr is ast.ArrayInit && g.assign_op != .decl_assign {
			g.write('(${g.typ(node.expr.typ)})')
		}
		g.expr(node.expr)
	} else if expr_type == ast.bool_type && node.typ.is_int() {
		styp := g.typ(node_typ)
		g.write('(${styp}[]){(')
		g.expr(node.expr)
		g.write(')?1:0}[0]')
	} else {
		styp := g.typ(node.typ)
		if (g.pref.translated || g.file.is_translated) && sym.kind == .function {
			// TODO handle the type in fn casts, not just exprs
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
			|| (!(sym.info as ast.Alias).parent_type.has_flag(.option)
			&& (sym.info as ast.Alias).parent_type !in [expr_type, ast.string_type]) {
			if sym.kind == .string && !node.typ.is_ptr() {
				cast_label = '*(string*)&'
			} else if !(g.is_cc_msvc && g.typ(node.typ) == g.typ(expr_type)) {
				cast_label = '(${styp})'
			}
		}
		if node.typ.has_flag(.option) && node.expr is ast.None {
			g.gen_option_error(node.typ, node.expr)
		} else if node.typ.has_flag(.option) {
			if sym.kind == .alias {
				if (sym.info as ast.Alias).parent_type.has_flag(.option) {
					cur_stmt := g.go_before_last_stmt()
					g.empty_line = true
					parent_type := (sym.info as ast.Alias).parent_type
					tmp_var := g.new_tmp_var()
					tmp_var2 := g.new_tmp_var()
					g.writeln('${styp} ${tmp_var};')
					g.writeln('${g.typ(parent_type)} ${tmp_var2};')
					g.write('_option_ok(&(${g.base_type(parent_type)}[]) { ')
					g.expr(node.expr)
					g.writeln(' }, (${c.option_name}*)(&${tmp_var2}), sizeof(${g.base_type(parent_type)}));')
					g.writeln('_option_ok(&(${g.typ(parent_type)}[]) { ${tmp_var2} }, (${c.option_name}*)&${tmp_var}, sizeof(${g.typ(parent_type)}));')
					g.write(cur_stmt)
					g.write(tmp_var)
				} else if node.expr_type.has_flag(.option) {
					g.expr_opt_with_cast(node.expr, expr_type, node.typ)
				} else {
					g.expr_with_opt(node.expr, expr_type, node.typ)
				}
			} else {
				g.expr_with_opt(node.expr, expr_type, node.typ)
			}
		} else if sym.kind == .alias && (sym.info as ast.Alias).parent_type.has_flag(.option) {
			g.expr_with_opt(node.expr, expr_type, (sym.info as ast.Alias).parent_type)
		} else {
			g.write('(${cast_label}(')
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
	mut styp := g.typ(node.return_type.clear_option_and_result())
	if g.inside_return {
		styp = g.typ(g.fn_decl.return_type.clear_option_and_result())
	} else if g.inside_or_block {
		styp = g.typ(g.or_expr_return_type.clear_option_and_result())
	}
	sym := g.table.sym(node.return_type)
	is_multi := sym.kind == .multi_return
	if !is_multi {
		g.expr(node.vals[0])
	} else {
		g.write('(${styp}){')
		for i, expr in node.vals {
			g.write('.arg${i}=')
			old_left_is_opt := g.left_is_opt
			g.left_is_opt = true
			g.expr(expr)
			g.left_is_opt = old_left_is_opt
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
	styp := g.typ(g.unwrap_generic(target_type))
	g.write('(${styp}){ .is_error=true, .err=')
	g.expr(expr)
	g.write(', .data={EMPTY_STRUCT_INITIALIZATION} }')
}

// NB: remove this when option has no errors anymore
fn (mut g Gen) gen_option_error(target_type ast.Type, expr ast.Expr) {
	styp := g.typ(g.unwrap_generic(target_type))
	g.write('(${styp}){ .state=2, .err=')
	if target_type.has_flag(.option) && expr is ast.Ident && expr.or_expr.kind == .propagate_option {
		g.expr(ast.None{}) // option type unwrapping error
	} else {
		g.expr(expr)
	}
	g.write(', .data={EMPTY_STRUCT_INITIALIZATION} }')
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
		if node.main.contains('.m') {
			g.definitions.writeln('')
			if ct_condition.len > 0 {
				g.definitions.writeln('#if ${ct_condition}')
			}
			// Objective C code import, include it after V types, so that e.g. `string` is
			// available there
			g.definitions.writeln('// added by module `${node.mod}`, file: ${os.file_name(node.source_file)}:${line_nr}:')
			g.definitions.writeln(guarded_include)
			if ct_condition.len > 0 {
				g.definitions.writeln('#endif // \$if ${ct_condition}')
			}
		} else {
			g.includes.writeln('')
			if ct_condition.len > 0 {
				g.includes.writeln('#if ${ct_condition}')
			}
			g.includes.writeln('// added by module `${node.mod}`, file: ${os.file_name(node.source_file)}:${line_nr}:')
			g.includes.writeln(guarded_include)
			if ct_condition.len > 0 {
				g.includes.writeln('#endif // \$if ${ct_condition}')
			}
		}
	} else if node.kind == 'preinclude' {
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
		if node.main.contains('.m') {
			// Might need to support '#preinclude' for .m files as well but for the moment
			// this does the same as '#include' for them
			g.definitions.writeln('')
			if ct_condition.len > 0 {
				g.definitions.writeln('#if ${ct_condition}')
			}
			// Objective C code import, include it after V types, so that e.g. `string` is
			// available there
			g.definitions.writeln('// added by module `${node.mod}`, file: ${os.file_name(node.source_file)}:${line_nr}:')
			g.definitions.writeln(guarded_include)
			if ct_condition.len > 0 {
				g.definitions.writeln('#endif // \$if ${ct_condition}')
			}
		} else {
			g.preincludes.writeln('')
			if ct_condition.len > 0 {
				g.preincludes.writeln('#if ${ct_condition}')
			}
			g.preincludes.writeln('// added by module `${node.mod}`, file: ${os.file_name(node.source_file)}:${line_nr}:')
			g.preincludes.writeln(guarded_include)
			if ct_condition.len > 0 {
				g.preincludes.writeln('#endif // \$if ${ct_condition}')
			}
		}
	} else if node.kind == 'insert' {
		if ct_condition.len > 0 {
			g.includes.writeln('#if ${ct_condition}')
		}
		g.includes.writeln('// inserted by module `${node.mod}`, file: ${os.file_name(node.source_file)}:${line_nr}:')
		g.includes.writeln(node.val)
		if ct_condition.len > 0 {
			g.includes.writeln('#endif // \$if ${ct_condition}')
		}
	} else if node.kind == 'define' {
		if ct_condition.len > 0 {
			g.includes.writeln('#if ${ct_condition}')
		}
		g.includes.writeln('// defined by module `${node.mod}`')
		g.includes.writeln('#define ${node.main}')
		if ct_condition.len > 0 {
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
	g.write_v_source_line_info(node.pos)

	g.inside_return = true
	defer {
		g.inside_return = false
	}

	if node.exprs.len > 0 {
		// skip `return $vweb.html()`
		if node.exprs[0] is ast.ComptimeCall && node.exprs[0].is_vweb {
			g.inside_return_tmpl = true
			g.expr(node.exprs[0])
			g.inside_return_tmpl = false
			g.writeln(';')
			return
		}
	}

	// got to do a correct check for multireturn
	sym := g.table.sym(g.unwrap_generic(g.fn_decl.return_type))
	mut fn_ret_type := g.fn_decl.return_type
	if sym.kind == .alias {
		unaliased_type := g.table.unaliased_type(fn_ret_type)
		if unaliased_type.has_option_or_result() {
			fn_ret_type = unaliased_type
		}
	}
	fn_return_is_multi := sym.kind == .multi_return
	fn_return_is_option := fn_ret_type.has_flag(.option)
	fn_return_is_result := fn_ret_type.has_flag(.result)
	fn_return_is_fixed_array := sym.is_array_fixed() && !fn_ret_type.has_option_or_result()

	mut has_semicolon := false
	if node.exprs.len == 0 {
		g.write_defer_stmts_when_needed()
		if fn_return_is_option || fn_return_is_result {
			styp := g.typ(fn_ret_type)
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
	mut ret_typ := g.typ(g.unwrap_generic(fn_ret_type))
	if fn_ret_type.has_flag(.generic) && fn_return_is_fixed_array {
		ret_typ = '_v_${ret_typ}'
	} else if sym.kind == .alias && fn_return_is_fixed_array {
		ret_typ = '_v_' + g.typ((sym.info as ast.Alias).parent_type)
	}
	if node.exprs.len == 1 {
		// `return fn_call_opt()`
		if (fn_return_is_option || fn_return_is_result) && node.exprs[0] is ast.CallExpr
			&& node.exprs[0].return_type == g.fn_decl.return_type
			&& node.exprs[0].or_block.kind == .absent {
			g.write('${ret_typ} ${tmpvar} = ')
			g.expr(node.exprs[0])
			g.writeln(';')
			g.write_defer_stmts_when_needed()
			g.writeln('return ${tmpvar};')
			return
		}
	}
	mut use_tmp_var := g.defer_stmts.len > 0 || g.defer_profile_code.len > 0
		|| g.cur_lock.lockeds.len > 0
		|| (fn_return_is_multi && node.exprs.len >= 1 && fn_return_is_option)
		|| fn_return_is_fixed_array
	// handle promoting none/error/function returning _option'
	if fn_return_is_option {
		option_none := node.exprs[0] is ast.None
		ftyp := g.typ(node.types[0])
		mut is_regular_option := ftyp == '_option'
		if option_none || is_regular_option || node.types[0] == ast.error_type_idx {
			if g.fn_decl != unsafe { nil } && g.fn_decl.is_test {
				test_error_var := g.new_tmp_var()
				g.write('${ret_typ} ${test_error_var} = ')
				g.gen_option_error(fn_ret_type, node.exprs[0])
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
			g.gen_option_error(fn_ret_type, node.exprs[0])
			g.writeln(';')
			if use_tmp_var {
				// handle options when returning `none` for `?(int, ?int)`
				if fn_return_is_multi && node.exprs.len >= 1 {
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
		ftyp := g.typ(node.types[0])
		mut is_regular_result := ftyp == c.result_name
		if is_regular_result || node.types[0] == ast.error_type_idx {
			if g.fn_decl != unsafe { nil } && g.fn_decl.is_test {
				test_error_var := g.new_tmp_var()
				g.write('${ret_typ} ${test_error_var} = ')
				g.gen_result_error(fn_ret_type, node.exprs[0])
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
			g.gen_result_error(fn_ret_type, node.exprs[0])
			g.writeln(';')
			if use_tmp_var {
				g.write_defer_stmts_when_needed()
				g.writeln('return ${tmpvar};')
			}
			return
		}
	}
	// regular cases
	if fn_return_is_multi && node.exprs.len > 0 && !g.expr_is_multi_return_call(node.exprs[0]) {
		if node.exprs.len == 1 && (node.exprs[0] is ast.IfExpr || node.exprs[0] is ast.MatchExpr) {
			// use a temporary for `return if cond { x,y } else { a,b }` or `return match expr { abc { x, y } else { z, w } }`
			g.write('${ret_typ} ${tmpvar} = ')
			g.expr(node.exprs[0])
			g.writeln(';')
			g.write_defer_stmts_when_needed()
			g.writeln('return ${tmpvar};')
			return
		}
		mr_info := sym.info as ast.MultiReturn
		mut styp := ''
		if fn_return_is_option {
			g.writeln('${ret_typ} ${tmpvar};')
			styp = g.base_type(g.fn_decl.return_type)
			g.write('_option_ok(&(${styp}[]) { ')
		} else if fn_return_is_result {
			g.writeln('${ret_typ} ${tmpvar};')
			styp = g.base_type(g.fn_decl.return_type)
			g.write('_result_ok(&(${styp}[]) { ')
		} else {
			if use_tmp_var {
				g.write('${ret_typ} ${tmpvar} = ')
			} else {
				g.write('return ')
			}
			styp = g.typ(g.fn_decl.return_type)
		}
		// Use this to keep the tmp assignments in order
		mut multi_unpack := ''
		g.write('(${styp}){')
		mut arg_idx := 0
		for i, expr in node.exprs {
			// Check if we are dealing with a multi return and handle it separately
			if g.expr_is_multi_return_call(expr) {
				call_expr := expr as ast.CallExpr
				expr_sym := g.table.sym(call_expr.return_type)
				mut tmp := g.new_tmp_var()
				if !call_expr.return_type.has_flag(.option)
					&& !call_expr.return_type.has_flag(.result) {
					line := g.go_before_last_stmt()
					expr_styp := g.typ(call_expr.return_type)
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
					tmp = ('(*(${expr_styp}*)${tmp}.data)')
				}
				expr_types := expr_sym.mr_info().types
				for j, _ in expr_types {
					g.write('.arg${arg_idx}=${tmp}.arg${j}')
					if j < expr_types.len || i < node.exprs.len - 1 {
						g.write(',')
					}
					arg_idx++
				}
				continue
			}
			g.write('.arg${arg_idx}=')
			if expr.is_auto_deref_var() {
				g.write('*')
			}
			if g.table.sym(mr_info.types[i]).kind in [.sum_type, .interface_] {
				g.expr_with_cast(expr, node.types[i], mr_info.types[i])
			} else if mr_info.types[i].has_flag(.option) {
				g.expr_with_opt(expr, node.types[i], mr_info.types[i])
			} else {
				g.expr(expr)
			}

			if i < node.exprs.len - 1 {
				g.write(', ')
			}
			arg_idx++
		}
		g.write('}')
		if fn_return_is_option {
			g.writeln(' }, (${c.option_name}*)(&${tmpvar}), sizeof(${styp}));')
			g.write_defer_stmts_when_needed()
			g.write('return ${tmpvar}')
		} else if fn_return_is_result {
			g.writeln(' }, (${c.result_name}*)(&${tmpvar}), sizeof(${styp}));')
			g.write_defer_stmts_when_needed()
			g.write('return ${tmpvar}')
		}
		// Make sure to add our unpacks
		if multi_unpack.len > 0 {
			g.insert_before_stmt(multi_unpack)
		}
		if use_tmp_var && !fn_return_is_option && !fn_return_is_result {
			if !has_semicolon {
				g.writeln(';')
			}
			g.write_defer_stmts_when_needed()
			g.writeln('return ${tmpvar};')
			has_semicolon = true
		}
	} else if node.exprs.len >= 1 {
		if node.types.len == 0 {
			g.checker_bug('node.exprs.len == ${node.exprs.len} && node.types.len == 0',
				node.pos)
		}
		// normal return
		return_sym := g.table.sym(node.types[0])
		expr0 := node.exprs[0]
		// `return opt_ok(expr)` for functions that expect an option
		expr_type_is_opt := match expr0 {
			ast.CallExpr {
				expr0.return_type.has_flag(.option) && expr0.or_block.kind == .absent
			}
			else {
				node.types[0].has_flag(.option)
			}
		}
		if fn_return_is_option && !expr_type_is_opt && return_sym.name != c.option_name {
			styp := g.base_type(fn_ret_type)
			g.writeln('${ret_typ} ${tmpvar};')
			g.write('_option_ok(&(${styp}[]) { ')
			if !g.unwrap_generic(fn_ret_type).is_ptr() && node.types[0].is_ptr() {
				if !(node.exprs[0] is ast.Ident && !g.is_amp) {
					g.write('*')
				}
			}
			for i, expr in node.exprs {
				if return_sym.kind == .array_fixed && expr !is ast.ArrayInit {
					g.fixed_array_var_init(expr, (return_sym.info as ast.ArrayFixed).size)
				} else {
					g.expr_with_cast(expr, node.types[i], fn_ret_type.clear_option_and_result())
				}
				if i < node.exprs.len - 1 {
					g.write(', ')
				}
			}
			g.writeln(' }, (${c.option_name}*)(&${tmpvar}), sizeof(${styp}));')
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
				node.types[0].has_flag(.result)
			}
		}
		if fn_return_is_result && !expr_type_is_result && return_sym.name != c.result_name {
			styp := g.base_type(fn_ret_type)
			g.writeln('${ret_typ} ${tmpvar};')
			g.write('_result_ok(&(${styp}[]) { ')
			if !fn_ret_type.is_ptr() && node.types[0].is_ptr() {
				if !(node.exprs[0] is ast.Ident && !g.is_amp) {
					g.write('*')
				}
			}
			for i, expr in node.exprs {
				if fn_ret_type.has_flag(.option) {
					g.expr_with_opt(expr, node.types[i], fn_ret_type.clear_flag(.result))
				} else if return_sym.kind == .array_fixed && expr !is ast.ArrayInit {
					g.fixed_array_var_init(expr, (return_sym.info as ast.ArrayFixed).size)
				} else {
					g.expr_with_cast(expr, node.types[i], fn_ret_type.clear_flag(.result))
				}
				if i < node.exprs.len - 1 {
					g.write(', ')
				}
			}
			g.writeln(' }, (${c.result_name}*)(&${tmpvar}), sizeof(${styp}));')
			g.write_defer_stmts_when_needed()
			g.autofree_scope_vars(node.pos.pos - 1, node.pos.line_nr, true)
			g.writeln('return ${tmpvar};')
			return
		}
		// autofree before `return`
		// set free_parent_scopes to true, since all variables defined in parent
		// scopes need to be freed before the return
		if g.is_autofree {
			expr := node.exprs[0]
			if expr is ast.Ident {
				g.returned_var_name = expr.name
			}
		}
		// free := g.is_autofree && !g.is_builtin_mod // node.exprs[0] is ast.CallExpr
		// Create a temporary variable for the return expression
		if use_tmp_var || !g.is_builtin_mod {
			// `return foo(a, b, c)`
			// `tmp := foo(a, b, c); free(a); free(b); free(c); return tmp;`
			// Save return value in a temp var so that all args (a,b,c) can be freed
			// Don't use a tmp var if a variable is simply returned: `return x`
			// Just in case of defer statements exists, that the return values cannot
			// be modified.
			if node.exprs[0] !is ast.Ident || use_tmp_var {
				use_tmp_var = true
				g.write('${ret_typ} ${tmpvar} = ')
			} else {
				use_tmp_var = false
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
			if g.fn_decl.return_type.is_ptr() {
				var_str := g.expr_string(expr0)
				g.write(var_str.trim('&'))
			} else if g.fn_decl.return_type.has_flag(.option) {
				g.expr_with_opt(expr0, node.types[0], g.fn_decl.return_type)
			} else if g.table.sym(g.fn_decl.return_type).kind in [.sum_type, .interface_] {
				g.expr_with_cast(expr0, node.types[0], g.fn_decl.return_type)
			} else {
				g.write('*')
				g.expr(expr0)
			}
		} else {
			if g.fn_decl.return_type.has_flag(.option) {
				g.expr_with_opt(node.exprs[0], node.types[0], g.fn_decl.return_type)
			} else {
				if fn_return_is_fixed_array && !node.types[0].has_option_or_result() {
					g.writeln('{0};')
					if node.exprs[0] is ast.Ident {
						typ := if expr0.is_auto_deref_var() {
							node.types[0].deref()
						} else {
							node.types[0]
						}
						g.write('memcpy(${tmpvar}.ret_arr, ${g.expr_string(node.exprs[0])}, sizeof(${g.typ(typ)})) /*ret*/')
					} else if node.exprs[0] in [ast.ArrayInit, ast.StructInit] {
						if node.exprs[0] is ast.ArrayInit && node.exprs[0].is_fixed
							&& node.exprs[0].has_init {
							g.write('memcpy(${tmpvar}.ret_arr, ')
							g.expr_with_cast(node.exprs[0], node.types[0], g.fn_decl.return_type)
							g.write(', sizeof(${g.typ(node.types[0])})) /*ret*/')
						} else {
							tmpvar2 := g.new_tmp_var()
							g.write('${g.typ(node.types[0])} ${tmpvar2} = ')
							g.expr_with_cast(node.exprs[0], node.types[0], g.fn_decl.return_type)
							g.writeln(';')
							g.write('memcpy(${tmpvar}.ret_arr, ${tmpvar2}, sizeof(${g.typ(node.types[0])})) /*ret*/')
						}
					} else {
						g.write('memcpy(${tmpvar}.ret_arr, ')
						g.expr_with_cast(node.exprs[0], node.types[0], g.fn_decl.return_type)
						g.write(', sizeof(${g.typ(node.types[0])})) /*ret*/')
					}
				} else {
					g.expr_with_cast(node.exprs[0], node.types[0], g.fn_decl.return_type)
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

fn (mut g Gen) const_decl(node ast.ConstDecl) {
	g.inside_const = true
	defer {
		g.inside_const = false
	}
	for field in node.fields {
		if g.pref.skip_unused {
			if field.name !in g.table.used_consts {
				$if trace_skip_unused_consts ? {
					eprintln('>> skipping unused const name: ${field.name}')
				}
				continue
			}
		}
		name := c_name(field.name)
		mut const_name := '_const_' + name
		if !g.is_builtin_mod {
			if cattr := node.attrs.find_first('export') {
				const_name = cattr.arg
			}
		}
		field_expr := field.expr
		match field.expr {
			ast.ArrayInit {
				if field.expr.is_fixed && g.pref.build_mode != .build_module
					&& (!g.is_cc_msvc || field.expr.elem_type != ast.string_type) {
					styp := g.typ(field.expr.typ)
					val := g.expr_string(field.expr)
					g.global_const_defs[util.no_dots(field.name)] = GlobalConstDef{
						mod: field.mod
						def: '${styp} ${const_name} = ${val}; // fixed array const'
						dep_names: g.table.dependent_names_in_expr(field_expr)
					}
				} else if field.expr.is_fixed && g.is_cc_msvc
					&& field.expr.elem_type == ast.string_type {
					g.const_decl_init_later_msvc_string_fixed_array(field.mod, name, field.expr,
						field.typ)
				} else {
					g.const_decl_init_later(field.mod, name, field.expr, field.typ, false,
						false)
				}
			}
			ast.StringLiteral {
				val := g.expr_string(field.expr)
				g.global_const_defs[util.no_dots(field.name)] = GlobalConstDef{
					mod: field.mod
					def: 'string ${const_name}; // a string literal, inited later'
					init: '\t${const_name} = ${val};'
					order: -1
				}
			}
			ast.CallExpr {
				if field.expr.return_type.has_flag(.option)
					|| field.expr.return_type.has_flag(.result) {
					g.inside_const_opt_or_res = true
					unwrap_opt_res := field.expr.or_block.kind != .absent
					g.const_decl_init_later(field.mod, name, field.expr, field.typ, unwrap_opt_res,
						unwrap_opt_res)
				} else {
					g.const_decl_init_later(field.mod, name, field.expr, field.typ, false,
						false)
				}
				g.inside_const_opt_or_res = false
			}
			else {
				// Note: -usecache uses prebuilt modules, each compiled with:
				// `v build-module vlib/module`
				// combined with a top level program, that is compiled with:
				// `v -usecache toplevel`
				// For it to work, the consts optimisations should be identical, because
				// only the top level program will have the const initialisation code for
				// all the modules.
				// TODO: encapsulate const initialisation for each module in a separate function,
				// that is just called by the top level program in _vinit, instead of generating
				// all the code inside _vinit for each module.
				use_cache_mode := g.pref.build_mode == .build_module || g.pref.use_cache
				if !use_cache_mode {
					if ct_value := field.comptime_expr_value() {
						if g.const_decl_precomputed(field.mod, name, field.name, ct_value,
							field.typ)
						{
							continue
						}
					}
				}
				if field.is_simple_define_const() {
					// "Simple" expressions are not going to need multiple statements,
					// only the ones which are inited later, so it's safe to use expr_string
					g.const_decl_simple_define(field.mod, field.name, g.expr_string(field_expr))
				} else if field.expr is ast.CastExpr {
					if field.expr.expr is ast.ArrayInit {
						if field.expr.expr.is_fixed && g.pref.build_mode != .build_module {
							styp := g.typ(field.expr.typ)
							val := g.expr_string(field.expr.expr)
							g.global_const_defs[util.no_dots(field.name)] = GlobalConstDef{
								mod: field.mod
								def: '${styp} ${const_name} = ${val}; // fixed array const'
								dep_names: g.table.dependent_names_in_expr(field_expr)
							}
							continue
						}
					}
					g.const_decl_init_later(field.mod, name, field.expr, field.typ, false,
						false)
				} else if field.expr is ast.InfixExpr {
					mut has_unwrap_opt_res := false
					if field.expr.left is ast.CallExpr {
						has_unwrap_opt_res = field.expr.left.or_block.kind != .absent
					} else if field.expr.right is ast.CallExpr {
						has_unwrap_opt_res = field.expr.right.or_block.kind != .absent
					}
					g.const_decl_init_later(field.mod, name, field.expr, field.typ, false,
						has_unwrap_opt_res)
				} else {
					g.const_decl_init_later(field.mod, name, field.expr, field.typ, false,
						false)
				}
			}
		}
	}
}

fn (mut g Gen) const_decl_precomputed(mod string, name string, field_name string, ct_value ast.ComptTimeConstValue, typ ast.Type) bool {
	mut styp := g.typ(typ)
	cname := if g.pref.translated && !g.is_builtin_mod { name } else { '_const_${name}' }
	$if trace_const_precomputed ? {
		eprintln('> styp: ${styp} | cname: ${cname} | ct_value: ${ct_value} | ${ct_value.type_name()}')
	}
	match ct_value {
		i8 {
			g.const_decl_write_precomputed(mod, styp, cname, field_name, ct_value.str())
		}
		i16 {
			g.const_decl_write_precomputed(mod, styp, cname, field_name, ct_value.str())
		}
		i32 {
			g.const_decl_write_precomputed(mod, styp, cname, field_name, ct_value.str())
		}
		int {
			// XTODO int64
			g.const_decl_write_precomputed(mod, styp, cname, field_name, ct_value.str())
		}
		i64 {
			if typ == ast.i64_type {
				return false
			}
			if typ == ast.int_type {
				// TODO: use g.const_decl_write_precomputed here too.
				// For now, use #define macros, so existing code compiles
				// with -cstrict. Add checker errors for overflows instead,
				// so V can catch them earlier, instead of relying on the
				// C compiler for that.
				g.const_decl_simple_define(mod, name, ct_value.str())
				return true
			}
			if typ == ast.u64_type {
				g.const_decl_write_precomputed(mod, styp, cname, field_name, ct_value.str() + 'U')
			} else {
				g.const_decl_write_precomputed(mod, styp, cname, field_name, ct_value.str())
			}
		}
		u8 {
			g.const_decl_write_precomputed(mod, styp, cname, field_name, ct_value.str())
		}
		u16 {
			g.const_decl_write_precomputed(mod, styp, cname, field_name, ct_value.str())
		}
		u32 {
			g.const_decl_write_precomputed(mod, styp, cname, field_name, ct_value.str())
		}
		u64 {
			g.const_decl_write_precomputed(mod, styp, cname, field_name, ct_value.str() + 'U')
		}
		f32 {
			g.const_decl_write_precomputed(mod, styp, cname, field_name, ct_value.str())
		}
		f64 {
			g.const_decl_write_precomputed(mod, styp, cname, field_name, ct_value.str())
		}
		rune {
			rune_code := u32(ct_value)
			if rune_code <= 127 {
				if rune_code in [`"`, `\\`, `'`] {
					return false
				}
				escval := util.smart_quote(u8(rune_code).ascii_str(), false)

				g.global_const_defs[util.no_dots(field_name)] = GlobalConstDef{
					mod: mod
					def: "#define ${cname} '${escval}'"
					order: -1
				}
			} else {
				g.const_decl_write_precomputed(mod, styp, cname, field_name, u32(ct_value).str())
			}
		}
		string {
			escaped_val := util.smart_quote(ct_value, false)
			// g.const_decl_write_precomputed(line_nr, styp, cname, '_SLIT("$escaped_val")')
			// TODO: ^ the above for strings, cause:
			// `error C2099: initializer is not a constant` errors in MSVC,
			// so fall back to the delayed initialisation scheme:
			g.global_const_defs[util.no_dots(field_name)] = GlobalConstDef{
				mod: mod
				def: '${styp} ${cname}; // str inited later'
				init: '\t${cname} = _SLIT("${escaped_val}");'
				order: -1
			}
			if g.is_autofree {
				g.cleanups[mod].writeln('\tstring_free(&${cname});')
			}
		}
		voidptr {
			g.const_decl_write_precomputed(mod, styp, cname, field_name, '(voidptr)(0x${ct_value})')
		}
		ast.EmptyExpr {
			return false
		}
	}
	return true
}

fn (mut g Gen) const_decl_write_precomputed(mod string, styp string, cname string, field_name string, ct_value string) {
	if g.pref.is_livemain || g.pref.is_liveshared {
		// Note: tcc has problems reloading .so files with consts in them, when the consts are then used inside the reloaded
		// live functions. As a workaround, just use simple #define macros in this case.
		//
		// If you change it, please also test with `v -live run examples/hot_reload/graph.v` which uses `math.pi` .
		g.global_const_defs[util.no_dots(field_name)] = GlobalConstDef{
			mod: mod
			def: '#define ${cname} ${ct_value} // precomputed3, -live mode'
			order: -1
		}
		return
	}
	g.global_const_defs[util.no_dots(field_name)] = GlobalConstDef{
		mod: mod
		def: '${g.static_modifier} const ${styp} ${cname} = ${ct_value}; // precomputed2'
		// is_precomputed: true
	}
}

fn (mut g Gen) const_decl_simple_define(mod string, name string, val string) {
	// Simple expressions should use a #define
	// so that we don't pollute the binary with unnecessary global vars
	// Do not do this when building a module, otherwise the consts
	// will not be accessible.
	mut x := util.no_dots(name)
	if g.pref.translated && !g.is_builtin_mod && !util.module_is_builtin(name.all_before_last('.')) {
		// Don't prepend "_const" to translated C consts,
		// but only in user code, continue prepending "_const" to builtin consts.
		if x.starts_with('main__') {
			x = x['main__'.len..]
		}
	} else {
		x = '_const_${x}'
	}
	if g.pref.translated {
		g.global_const_defs[util.no_dots(name)] = GlobalConstDef{
			mod: mod
			def: 'const int ${x} = ${val};'
			order: -1
		}
	} else {
		g.global_const_defs[util.no_dots(name)] = GlobalConstDef{
			mod: mod
			def: '#define ${x} ${val}'
			order: -1
		}
	}
}

fn (mut g Gen) c_const_name(name string) string {
	return if g.pref.translated && !g.is_builtin_mod { name } else { '_const_${name}' }
}

fn (mut g Gen) const_decl_init_later(mod string, name string, expr ast.Expr, typ ast.Type, unwrap_option bool, surround_cbr bool) {
	// Initialize more complex consts in `void _vinit/2{}`
	// (C doesn't allow init expressions that can't be resolved at compile time).
	mut styp := g.typ(typ)
	cname := g.c_const_name(name)
	mut init := strings.new_builder(100)
	if cname == '_const_os__args' {
		if g.pref.os == .windows {
			init.writeln('\t_const_os__args = os__init_os_args_wide(___argc, (byteptr*)___argv);')
		} else {
			init.writeln('\t_const_os__args = os__init_os_args(___argc, (byte**)___argv);')
		}
	} else {
		if surround_cbr {
			init.writeln('{')
		}
		if unwrap_option {
			init.writeln(g.expr_string_surround('\t${cname} = *(${styp}*)', expr, '.data;'))
		} else {
			init.writeln(g.expr_string_surround('\t${cname} = ', expr, ';'))
		}
		if surround_cbr {
			init.writeln('}')
		}
	}
	mut def := '${styp} ${cname}'
	expr_sym := g.table.sym(typ)
	if expr_sym.kind == .function {
		// allow for: `const xyz = abc`, where `abc` is `fn abc() {}`
		func := (expr_sym.info as ast.FnType).func
		def = g.fn_var_signature(func.return_type, func.params.map(it.typ), cname)
	}
	g.global_const_defs[util.no_dots(name)] = GlobalConstDef{
		mod: mod
		def: '${def}; // inited later'
		init: init.str().trim_right('\n')
		dep_names: g.table.dependent_names_in_expr(expr)
	}
	if g.is_autofree {
		sym := g.table.sym(typ)
		if styp.starts_with('Array_') {
			if sym.has_method_with_generic_parent('free') {
				g.cleanup.writeln('\t${styp}_free(&${cname});')
			} else {
				g.cleanup.writeln('\tarray_free(&${cname});')
			}
		} else if styp == 'string' {
			g.cleanup.writeln('\tstring_free(&${cname});')
		} else if sym.kind == .map {
			g.cleanup.writeln('\tmap_free(&${cname});')
		} else if styp == 'IError' {
			g.cleanup.writeln('\tIError_free(&${cname});')
		}
	}
}

fn (mut g Gen) const_decl_init_later_msvc_string_fixed_array(mod string, name string, expr ast.ArrayInit, typ ast.Type) {
	mut styp := g.typ(typ)
	cname := g.c_const_name(name)
	mut init := strings.new_builder(100)
	for i, elem_expr in expr.exprs {
		init.writeln(g.expr_string_surround('\t${cname}[${i}] = ', elem_expr, ';'))
	}
	mut def := '${styp} ${cname}'
	g.global_const_defs[util.no_dots(name)] = GlobalConstDef{
		mod: mod
		def: '${def}; // inited later'
		init: init.str().trim_right('\n')
		dep_names: g.table.dependent_names_in_expr(expr)
	}
	if g.is_autofree {
		sym := g.table.sym(typ)
		if sym.has_method_with_generic_parent('free') {
			g.cleanup.writeln('\t${styp}_free(&${cname});')
		} else {
			g.cleanup.writeln('\tarray_free(&${cname});')
		}
	}
}

fn (mut g Gen) global_decl(node ast.GlobalDecl) {
	// was static used here to to make code optimizable? it was removed when
	// 'extern' was used to fix the duplicate symbols with usecache && clang
	// visibility_kw := if g.pref.build_mode == .build_module && g.is_builtin_mod { 'static ' }
	visibility_kw := if
		(g.pref.use_cache || (g.pref.build_mode == .build_module && g.module_built != node.mod))
		&& !util.should_bundle_module(node.mod) {
		'extern '
	} else {
		'${g.static_modifier} ' // TODO used to be '' before parallel_cc, may cause issues
	}
	// should the global be initialized now, not later in `vinit()`
	cinit := node.attrs.contains('cinit')
	g.inside_cinit = cinit
	defer {
		g.inside_cinit = false
	}
	cextern := node.attrs.contains('c_extern')
	should_init := (!g.pref.use_cache && g.pref.build_mode != .build_module)
		|| (g.pref.build_mode == .build_module && g.module_built == node.mod)
	mut attributes := ''
	if node.attrs.contains('weak') {
		attributes += 'VWEAK '
	}
	for field in node.fields {
		if g.pref.skip_unused {
			if field.name !in g.table.used_globals {
				$if trace_skip_unused_globals ? {
					eprintln('>> skipping unused global name: ${field.name}')
				}
				continue
			}
		}
		styp := g.typ(field.typ)
		mut anon_fn_expr := unsafe { field.expr }
		if field.has_expr && mut anon_fn_expr is ast.AnonFn {
			g.gen_anon_fn_decl(mut anon_fn_expr)
			fn_type_name := g.get_anon_fn_type_name(mut anon_fn_expr, field.name)
			g.global_const_defs[util.no_dots(fn_type_name)] = GlobalConstDef{
				mod: node.mod
				def: '${fn_type_name} = ${g.table.sym(field.typ).name}; // global2'
				order: -1
			}
			continue
		}
		mut def_builder := strings.new_builder(100)
		mut init := ''
		extern := if cextern { 'extern ' } else { '' }
		modifier := if field.is_volatile { ' volatile ' } else { '' }
		def_builder.write_string('${extern}${visibility_kw}${modifier}${styp} ${attributes} ${field.name}')
		if cextern {
			def_builder.writeln('; // global5')
			g.global_const_defs[util.no_dots(field.name)] = GlobalConstDef{
				mod: node.mod
				def: def_builder.str()
				order: -1
			}
			continue
		}
		if field.has_expr || cinit {
			// `__global x = unsafe { nil }` should still use the simple direct initialisation, `g_main_argv` needs it.
			mut is_simple_unsafe_expr := false
			if field.expr is ast.UnsafeExpr {
				if field.expr.expr is ast.Nil {
					is_simple_unsafe_expr = true
				}
				if field.expr.expr.is_literal() {
					is_simple_unsafe_expr = true
				}
			}
			if g.pref.translated {
				def_builder.write_string(' = ${g.expr_string(field.expr)}')
			} else if (field.expr.is_literal() && should_init) || cinit
				|| (field.expr is ast.ArrayInit && field.expr.is_fixed)
				|| (is_simple_unsafe_expr && should_init) {
				// Simple literals can be initialized right away in global scope in C.
				// e.g. `int myglobal = 10;`
				def_builder.write_string(' = ${g.expr_string(field.expr)}')
			} else {
				// More complex expressions need to be moved to `_vinit()`
				// e.g. `__global ( mygblobal = 'hello ' + world' )`
				init = '\t${field.name} = ${g.expr_string(field.expr)}; // 3global'
			}
		} else if !g.pref.translated { // don't zero globals from C code
			default_initializer := g.type_default(field.typ)
			if default_initializer == '{0}' && should_init {
				def_builder.write_string(' = {0}')
			} else {
				if field.name !in ['as_cast_type_indexes', 'g_memory_block', 'global_allocator'] {
					init = '\t${field.name} = *(${styp}*)&((${styp}[]){${g.type_default(field.typ)}}[0]); // global'
				}
			}
		}
		def_builder.writeln('; // global4')
		g.global_const_defs[util.no_dots(field.name)] = GlobalConstDef{
			mod: node.mod
			def: def_builder.str()
			init: init
			dep_names: g.table.dependent_names_in_expr(field.expr)
		}
	}
}

fn (mut g Gen) assoc(node ast.Assoc) {
	g.writeln('// assoc')
	if node.typ == 0 {
		return
	}
	styp := g.typ(node.typ)
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

	g.writeln('\t// we call these functions in debug mode so that the C compiler')
	g.writeln('\t// does not optimize them and we can access them in the debugger.')
	for _, sym in g.table.type_symbols {
		if sym.kind == .sum_type {
			sum_info := sym.info as ast.SumType
			if sum_info.is_generic {
				continue
			}
			g.writeln('\tv_typeof_sumtype_${sym.cname}(0);')
		}
		if sym.kind == .interface_ {
			if sym.info !is ast.Interface {
				continue
			}
			inter_info := sym.info as ast.Interface
			if inter_info.is_generic {
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
	if g.pref.is_liveshared {
		return
	}
	fn_vinit_start_pos := g.out.len

	// ___argv is declared as voidptr here, because that unifies the windows/unix logic
	g.writeln('void _vinit(int ___argc, voidptr ___argv) {')

	g.write_debug_calls_typeof_functions()

	if g.pref.trace_calls {
		g.writeln('\tv__trace_calls__on_call(_SLIT("_vinit"));')
	}

	if g.use_segfault_handler && !g.pref.is_shared {
		// 11 is SIGSEGV. It is hardcoded here, to avoid FreeBSD compilation errors for trivial examples.
		// shared object does not need this
		g.writeln('#if __STDC_HOSTED__ == 1\n\tsignal(11, v_segmentation_fault_handler);\n#endif')
	}
	if g.pref.is_bare {
		g.writeln('init_global_allocator();')
	}

	if g.pref.prealloc {
		g.writeln('prealloc_vinit();')
	}
	// Note: the as_cast table should be *before* the other constant initialize calls,
	// because it may be needed during const initialization of builtin and during
	// calling module init functions too, just in case they do fail...
	g.write('\tas_cast_type_indexes = ')
	g.writeln(g.as_cast_name_table())
	if !g.pref.is_shared {
		// shared object does not need this
		g.writeln('\tbuiltin_init();')
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
			// ignore v.reflection already initialized above
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
	if g.pref.trace_calls {
		g.writeln('\tv__trace_calls__on_call(_SLIT("_vcleanup"));')
	}
	if g.is_autofree {
		// g.writeln('puts("cleaning up...");')
		reversed_table_modules := g.table.modules.reverse()
		for mod_name in reversed_table_modules {
			g.writeln('\t// Cleanups for module ${mod_name} :')
			g.writeln(g.cleanups[mod_name].str())
		}
		g.writeln('\tarray_free(&as_cast_type_indexes);')
	}
	for x in cleaning_up_array.reverse() {
		g.writeln(x)
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
		if g.pref.os == .windows {
			g.writeln('// workaround for windows, export _vinit_caller, let dl.open() call it')
			g.writeln('// NOTE: This is hardcoded in vlib/dl/dl_windows.c.v!')
			g.writeln('VV_EXPORTED_SYMBOL void _vinit_caller();')
		} else {
			g.writeln('__attribute__ ((constructor))')
		}
		g.writeln('void _vinit_caller() {')
		g.writeln('\tstatic bool once = false; if (once) {return;} once = true;')
		g.writeln('\t_vinit(0,0);')
		g.writeln('}')

		if g.pref.os == .windows {
			g.writeln('// workaround for windows, export _vcleanup_caller, let dl.close() call it')
			g.writeln('// NOTE: This is hardcoded in vlib/dl/dl_windows.c.v!')
			g.writeln('VV_EXPORTED_SYMBOL void _vcleanup_caller();')
		} else {
			g.writeln('__attribute__ ((destructor))')
		}
		g.writeln('void _vcleanup_caller() {')
		g.writeln('\tstatic bool once = false; if (once) {return;} once = true;')
		g.writeln('\t_vcleanup();')
		g.writeln('}')
	}
}

const builtins = ['string', 'array', 'DenseArray', 'map', 'Error', 'IError', option_name, result_name]

fn (mut g Gen) write_builtin_types() {
	if g.pref.no_builtin {
		return
	}
	mut builtin_types := []&ast.TypeSymbol{} // builtin types
	// builtin types need to be on top
	// everything except builtin will get sorted
	for builtin_name in c.builtins {
		sym := g.table.sym_by_idx(g.table.type_idxs[builtin_name])
		if sym.kind == .interface_ {
			g.write_interface_typedef(sym)
			g.write_interface_typesymbol_declaration(sym)
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
		for sym in g.table.type_symbols {
			if sym.name !in c.builtins {
				symbols << sym
			}
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
		if sym.kind == .none_ {
			g.type_definitions.writeln('struct none {')
			g.type_definitions.writeln('\tEMPTY_STRUCT_DECLARATION;')
			g.type_definitions.writeln('};')
			g.typedefs.writeln('typedef struct none none;')
		}
		mut name := sym.cname
		match sym.info {
			ast.Struct {
				if !struct_names[name] {
					g.struct_decl(sym.info, name, false)
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
				if sym.info.is_generic || struct_names[name] {
					continue
				}
				struct_names[name] = true
				g.typedefs.writeln('typedef struct ${name} ${name};')
				g.type_definitions.writeln('')
				g.type_definitions.writeln('// Union sum type ${name} = ')
				for variant in sym.info.variants {
					g.type_definitions.writeln('//          | ${variant:4d} = ${g.typ(variant.idx()):-20s}')
				}
				g.type_definitions.writeln('struct ${name} {')
				g.type_definitions.writeln('\tunion {')
				for variant in sym.info.variants {
					variant_sym := g.table.sym(variant)
					mut var := variant.ref()
					if variant_sym.info is ast.FnType {
						if variant_sym.info.is_anon {
							var = variant
						}
					}
					g.type_definitions.writeln('\t\t${g.typ(var)} _${variant_sym.cname};')
				}
				g.type_definitions.writeln('\t};')
				g.type_definitions.writeln('\tint _typ;')
				if sym.info.fields.len > 0 {
					g.writeln('\t// pointers to common sumtype fields')
					for field in sym.info.fields {
						g.type_definitions.writeln('\t${g.typ(field.typ.ref())} ${field.name};')
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
					mut fixed_elem_name := g.typ(sym.info.elem_type.set_nr_muls(0))
					if sym.info.elem_type.is_ptr() {
						fixed_elem_name += '*'.repeat(sym.info.elem_type.nr_muls())
					}
					len := sym.info.size
					if len > 0 {
						if fixed_elem_name.starts_with('C__') {
							fixed_elem_name = fixed_elem_name[3..]
						}
						if elem_sym.info is ast.FnType {
							pos := g.out.len
							g.write_fn_ptr_decl(&elem_sym.info, '')
							fixed_elem_name = g.out.cut_to(pos)
							mut def_str := 'typedef ${fixed_elem_name};'
							def_str = def_str.replace_once('(*)', '(*${styp}[${len}])')
							g.type_definitions.writeln(def_str)
						} else if elem_sym.info !is ast.ArrayFixed
							|| (elem_sym.info as ast.ArrayFixed).size > 0 {
							g.type_definitions.writeln('typedef ${fixed_elem_name} ${styp} [${len}];')
						}
					}
				}
			}
			else {}
		}
	}
}

fn (mut g Gen) sort_globals_consts() {
	util.timing_start(@METHOD)
	defer {
		util.timing_measure(@METHOD)
	}
	g.sorted_global_const_names.clear()
	mut dep_graph := depgraph.new_dep_graph()
	for var_name, var_info in g.global_const_defs {
		dep_graph.add_with_value(var_name, var_info.dep_names, var_info.order)
	}
	dep_graph_sorted := dep_graph.resolve()
	for order in [-1, 0] {
		for node in dep_graph_sorted.nodes {
			if node.value == order {
				g.sorted_global_const_names << node.name
			}
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
		type_names << sym.name
	}
	// loop over types
	for sym in typesa {
		if sym.kind == .interface_ {
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
		dep_graph.add(sym.name, field_deps)
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

fn (mut g Gen) gen_or_block_stmts(cvar_name string, cast_typ string, stmts []ast.Stmt, return_type ast.Type, is_option bool) {
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
					if is_option {
						is_array_fixed = expr_stmt.expr is ast.ArrayInit
							&& g.table.final_sym(return_type).kind == .array_fixed
						if !is_array_fixed {
							g.write('*(${cast_typ}*) ${cvar_name}.data = ')
						}
					} else {
						g.write('${cvar_name} = ')
					}

					if is_array_fixed {
						g.write('memcpy(${cvar_name}.data, (${cast_typ})')
					}
					old_inside_opt_data := g.inside_opt_data
					g.inside_opt_data = true
					g.expr_with_cast(expr_stmt.expr, expr_stmt.typ, return_type.clear_option_and_result())
					g.inside_opt_data = old_inside_opt_data
					if is_array_fixed {
						g.write(', sizeof(${cast_typ}))')
					}
					g.writeln(';')
					g.stmt_path_pos.delete_last()
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
	mut mr_styp := g.base_type(return_type)
	is_none_ok := return_type == ast.ovoid_type
	g.writeln(';')
	if is_none_ok {
		g.writeln('if (${cvar_name}.state != 0) {')
	} else {
		if return_type != 0 && g.table.sym(return_type).kind == .function {
			mr_styp = 'voidptr'
		}
		if return_type.has_flag(.result) {
			g.writeln('if (${cvar_name}.is_error) {') // /*or block*/ ')
		} else {
			g.writeln('if (${cvar_name}.state != 0) {') // /*or block*/ ')
		}
	}
	if or_block.kind == .block {
		g.or_expr_return_type = return_type.clear_option_and_result()
		g.writeln('\tIError err = ${cvar_name}.err;')

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
			err_msg := 'IError_name_table[${cvar_name}.err._typ]._method_msg(${cvar_name}.err._object)'
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
			if g.fn_decl.return_type == ast.void_type {
				g.writeln('\treturn;')
			} else {
				styp := g.typ(g.fn_decl.return_type)
				err_obj := g.new_tmp_var()
				g.writeln('\t${styp} ${err_obj};')
				g.writeln('\tmemcpy(&${err_obj}, &${cvar_name}, sizeof(${c.result_name}));')
				g.writeln('\treturn ${err_obj};')
			}
		}
	} else if or_block.kind == .propagate_option {
		if g.file.mod.name == 'main' && (g.fn_decl == unsafe { nil } || g.fn_decl.is_main) {
			// In main(), an `opt()?` call is sugar for `opt() or { panic(err) }`
			err_msg := 'IError_name_table[${cvar_name}.err._typ]._method_msg(${cvar_name}.err._object)'
			if g.pref.is_debug {
				paline, pafile, pamod, pafn := g.panic_debug_info(or_block.pos)
				g.writeln('panic_debug(${paline}, tos3("${pafile}"), tos3("${pamod}"), tos3("${pafn}"), ${err_msg}.len == 0 ? _SLIT("option not set ()") : ${err_msg});')
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
			if g.fn_decl.return_type == ast.void_type {
				g.writeln('\treturn;')
			} else {
				styp := g.typ(g.fn_decl.return_type)
				err_obj := g.new_tmp_var()
				g.writeln('\t${styp} ${err_obj};')
				g.writeln('\tmemcpy(&${err_obj}, &${cvar_name}, sizeof(_option));')
				g.writeln('\treturn ${err_obj};')
			}
		}
	}
	g.writeln('}')
	g.set_current_pos_as_last_stmt_pos()
}

@[inline]
fn c_name(name_ string) string {
	name := util.no_dots(name_)
	if c.c_reserved_chk.matches(name) {
		return '__v_${name}'
	}
	return name
}

@[inline]
fn c_fn_name(name_ string) string {
	name := util.no_dots(name_)
	if c.c_reserved_chk.matches(name) {
		return '_v_${name}'
	}
	return name
}

fn (mut g Gen) type_default(typ_ ast.Type) string {
	typ := g.unwrap_generic(typ_)
	if typ.has_option_or_result() {
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
		.interface_, .sum_type, .array_fixed, .multi_return, .thread {
			return '{0}'
		}
		.alias {
			return g.type_default((sym.info as ast.Alias).parent_type)
		}
		.chan {
			elem_type := sym.chan_info().elem_type
			elemtypstr := g.typ(elem_type)
			noscan := g.check_noscan(elem_type)
			return 'sync__new_channel_st${noscan}(0, sizeof(${elemtypstr}))'
		}
		.array {
			elem_typ := sym.array_info().elem_type
			elem_sym := g.typ(elem_typ)
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
			init_str := 'new_map${noscan}(sizeof(${g.typ(info.key_type)}), sizeof(${g.typ(info.value_type)}), ${hash_fn}, ${key_eq_fn}, ${clone_fn}, ${free_fn})'
			if typ.has_flag(.shared_f) {
				mtyp := '__shared__Map_${key_typ.cname}_${g.typ(info.value_type).replace('*',
					'_ptr')}'
				return '(${mtyp}*)__dup_shared_map(&(${mtyp}){.mtx = {0}, .val =${init_str}}, sizeof(${mtyp}))'
			} else {
				return init_str
			}
		}
		.struct_ {
			mut has_none_zero := false
			mut init_str := '{'
			info := sym.info as ast.Struct
			if sym.language == .v {
				for field in info.fields {
					field_sym := g.table.sym(field.typ)
					if field.has_default_expr
						|| field_sym.kind in [.array, .map, .string, .bool, .alias, .i8, .i16, .int, .i64, .u8, .u16, .u32, .u64, .f32, .f64, .char, .voidptr, .byteptr, .charptr, .struct_, .chan] {
						field_name := c_name(field.name)
						if field.has_default_expr {
							mut expr_str := ''
							if g.table.sym(field.typ).kind in [.sum_type, .interface_] {
								expr_str = g.expr_string_with_cast(field.default_expr,
									field.default_expr_typ, field.typ)
							} else {
								expr_str = g.expr_string(field.default_expr)
							}
							init_str += '.${field_name} = ${expr_str},'
						} else {
							mut zero_str := g.type_default(field.typ)
							if zero_str == '{0}' {
								if field_sym.info is ast.Struct && field_sym.language == .v {
									if field_sym.info.fields.len == 0
										&& field_sym.info.embeds.len == 0 {
										zero_str = '{EMPTY_STRUCT_INITIALIZATION}'
									}
								}
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
					type_name := if info.is_anon {
						// No name needed for anon structs, C figures it out on its own.
						''
					} else {
						'(${g.typ(typ)})'
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
		.enum_ {
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

fn (g Gen) get_all_test_function_names() []string {
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
	if tsuite_begin.len > 0 {
		all_tfuncs << tsuite_begin
	}
	all_tfuncs << tfuncs
	if tsuite_end.len > 0 {
		all_tfuncs << tsuite_end
	}
	return all_tfuncs
}

@[inline]
fn (mut g Gen) get_type(typ ast.Type) ast.Type {
	return if typ == g.field_data_type { g.comptime.comptime_for_field_value.typ } else { typ }
}

fn (mut g Gen) size_of(node ast.SizeOf) {
	typ := g.resolve_comptime_type(node.expr, g.get_type(node.typ))
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
	styp := g.typ(node_typ)
	g.write('sizeof(${util.no_dots(styp)})')
}

@[inline]
fn (mut g Gen) gen_enum_prefix(typ ast.Type) string {
	if g.pref.translated && typ.is_number() {
		// Mostly in translated code, when C enums are used as ints in switches
		return '_const_main__'
	} else {
		styp := g.typ(g.table.unaliased_type(typ))
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
	styp := g.typ(unwrapped_node_typ)
	sym := g.table.sym(unwrapped_node_typ)
	mut expr_type_sym := g.table.sym(g.unwrap_generic(node.expr_type))
	if mut expr_type_sym.info is ast.SumType {
		dot := if node.expr_type.is_ptr() { '->' } else { '.' }
		if node.expr.has_fn_call() && !g.is_cc_msvc {
			tmp_var := g.new_tmp_var()
			expr_styp := g.typ(node.expr_type)
			g.write('({ ${expr_styp} ${tmp_var} = ')
			g.expr(node.expr)
			g.write('; ')
			if sym.info is ast.FnType {
				g.write('/* as */ (${styp})__as_cast(')
			} else {
				g.write('/* as */ *(${styp}*)__as_cast(')
			}
			g.write(tmp_var)
			g.write(dot)
			g.write('_${sym.cname},')
			g.write(tmp_var)
			g.write(dot)
			sidx := g.type_sidx(unwrapped_node_typ)
			g.write('_typ, ${sidx}); })')
		} else {
			if sym.info is ast.FnType {
				g.write('/* as */ (${styp})__as_cast(')
			} else {
				g.write('/* as */ *(${styp}*)__as_cast(')
			}
			g.write('(')
			g.expr(node.expr)
			g.write(')')
			g.write(dot)
			g.write('_${sym.cname},')
			g.write('(')
			g.expr(node.expr)
			g.write(')')
			g.write(dot)
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
	} else if expr_type_sym.kind == .interface_ && sym.kind == .interface_ {
		g.write('I_${expr_type_sym.cname}_as_I_${sym.cname}(')
		if node.expr_type.is_ptr() {
			g.write('*')
		}
		g.expr(node.expr)
		g.write(')')

		mut info := expr_type_sym.info as ast.Interface
		if node.typ !in info.conversions {
			left_variants := g.table.iface_types[expr_type_sym.name]
			right_variants := g.table.iface_types[sym.name]
			info.conversions[node.typ] = left_variants.filter(it in right_variants)
		}
		expr_type_sym.info = info
	} else {
		g.expr(node.expr)
	}
}

fn (g Gen) as_cast_name_table() string {
	if g.as_cast_type_names.len == 0 {
		return 'new_array_from_c_array(1, 1, sizeof(VCastTypeIndexName), _MOV((VCastTypeIndexName[1]){(VCastTypeIndexName){.tindex = 0,.tname = _SLIT("unknown")}}));\n'
	}
	mut name_ast := strings.new_builder(1024)
	casts_len := g.as_cast_type_names.len + 1
	name_ast.writeln('new_array_from_c_array(${casts_len}, ${casts_len}, sizeof(VCastTypeIndexName), _MOV((VCastTypeIndexName[${casts_len}]){')
	name_ast.writeln('\t\t  (VCastTypeIndexName){.tindex = 0, .tname = _SLIT("unknown")}')
	for key, value in g.as_cast_type_names {
		name_ast.writeln('\t\t, (VCastTypeIndexName){.tindex = ${key}, .tname = _SLIT("${value}")}')
	}
	name_ast.writeln('\t}));\n')
	return name_ast.str()
}

fn (g Gen) has_been_referenced(fn_name string) bool {
	mut referenced := false
	lock g.referenced_fns {
		referenced = g.referenced_fns[fn_name]
	}
	return referenced
}

// Generates interface table and interface indexes
fn (mut g Gen) interface_table() string {
	util.timing_start(@METHOD)
	defer {
		util.timing_measure(@METHOD)
	}
	mut sb := strings.new_builder(100)
	mut conversion_functions := strings.new_builder(100)
	for isym in g.table.type_symbols {
		if isym.kind != .interface_ {
			continue
		}
		if isym.info !is ast.Interface {
			// Do not remove this check, `isym.info` could be `&IError`.
			continue
		}
		inter_info := isym.info as ast.Interface
		if inter_info.is_generic {
			continue
		}
		// interface_name is for example Speaker
		interface_name := isym.cname
		// generate a struct that references interface methods
		methods_struct_name := 'struct _${interface_name}_interface_methods'
		mut methods_struct_def := strings.new_builder(100)
		methods_struct_def.writeln('${methods_struct_name} {')
		inter_methods := inter_info.get_methods()
		mut methodidx := map[string]int{}
		for k, method_name in inter_methods {
			method := isym.find_method_with_generic_parent(method_name) or { continue }
			methodidx[method.name] = k
			ret_styp := g.typ(method.return_type)
			methods_struct_def.write_string('\t${ret_styp} (*_method_${c_fn_name(method.name)})(void* _')
			// the first param is the receiver, it's handled by `void*` above
			for i in 1 .. method.params.len {
				arg := method.params[i]
				methods_struct_def.write_string(', ${g.typ(arg.typ)} ${arg.name}')
			}
			// TODO g.fn_args(method.args[1..])
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
			methods_struct.writeln('${g.static_modifier} ${methods_struct_name} ${interface_name}_name_table[1];')
		} else {
			if g.pref.build_mode != .build_module {
				methods_struct.writeln('${g.static_modifier} ${methods_struct_name} ${interface_name}_name_table[${iname_table_length}] = {')
			} else {
				methods_struct.writeln('${g.static_modifier} ${methods_struct_name} ${interface_name}_name_table[${iname_table_length}];')
			}
		}
		mut cast_functions := strings.new_builder(100)
		mut methods_wrapper := strings.new_builder(100)
		methods_wrapper.writeln('// Methods wrapper for interface "${interface_name}"')
		mut already_generated_mwrappers := map[string]int{}
		iinidx_minimum_base := 1000 // Note: NOT 0, to avoid map entries set to 0 later, so `if already_generated_mwrappers[name] > 0 {` works.
		mut current_iinidx := iinidx_minimum_base
		for st in inter_info.types {
			st_sym := g.table.sym(ast.mktyp(st))
			// cctype is the Cleaned Concrete Type name, *without ptr*,
			// i.e. cctype is always just Cat, not Cat_ptr:
			cctype := g.cc_type(ast.mktyp(st), true)
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
			if isym.name != 'vweb.DbInterface' { // TODO remove this
				sb.writeln('static ${interface_name} I_${cctype}_to_Interface_${interface_name}(${cctype}* x);')
				mut cast_struct := strings.new_builder(100)
				cast_struct.writeln('(${interface_name}) {')
				cast_struct.writeln('\t\t._${cctype} = x,')
				cast_struct.writeln('\t\t._typ = ${interface_index_name},')
				for field in inter_info.fields {
					cname := c_name(field.name)
					field_styp := g.typ(field.typ)
					if _ := st_sym.find_field(field.name) {
						cast_struct.writeln('\t\t.${cname} = (${field_styp}*)((char*)x + __offsetof_ptr(x, ${cctype}, ${cname})),')
					} else if st_sym.kind == .array
						&& field.name in ['element_size', 'data', 'offset', 'len', 'cap', 'flags'] {
						// Manually checking, we already knows array contains above fields
						cast_struct.writeln('\t\t.${cname} = (${field_styp}*)((char*)x + __offsetof_ptr(x, ${cctype}, ${cname})),')
					} else {
						// the field is embedded in another struct
						cast_struct.write_string('\t\t.${cname} = (${field_styp}*)((char*)x')
						if st == ast.voidptr_type || st == ast.nil_type {
							cast_struct.write_string('/*.... ast.voidptr_type */')
						} else {
							if st_sym.kind == .struct_ {
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
				cast_struct.write_string('\t}')
				cast_struct_str := cast_struct.str()

				cast_functions.writeln('
// Casting functions for converting "${cctype}" to interface "${interface_name}"
static inline ${interface_name} I_${cctype}_to_Interface_${interface_name}(${cctype}* x) {
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
			}

			if g.pref.build_mode != .build_module {
				methods_struct.writeln('\t{')
			}
			if st == ast.voidptr_type || st == ast.nil_type {
				for mname, _ in methodidx {
					if g.pref.build_mode != .build_module {
						methods_struct.writeln('\t\t._method_${c_fn_name(mname)} = (void*) 0,')
					}
				}
			}
			mut methods := st_sym.methods.clone()
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
				else {}
			}
			t_methods := g.table.get_embed_methods(st_sym)
			for t_method in t_methods {
				if t_method.name !in methods.map(it.name) {
					methods << t_method
				}
			}

			for method in methods {
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
				if !method.params[0].typ.is_ptr() {
					method_call = '${cctype}_${name}'
					// inline void Cat_speak_Interface_Animal_method_wrapper(Cat c) { return Cat_speak(*c); }
					iwpostfix := '_Interface_${interface_name}_method_wrapper'
					methods_wrapper.write_string('static inline ${g.typ(method.return_type)} ${cctype}_${name}${iwpostfix}(')
					params_start_pos := g.out.len
					mut params := method.params.clone()
					// hack to mutate typ
					params[0] = ast.Param{
						...params[0]
						typ: st.set_nr_muls(1)
					}
					fargs, _, _ := g.fn_decl_params(params, unsafe { nil }, false)
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

			// >> Hack to allow old style custom error implementations
			// TODO: remove once deprecation period for `IError` methods has ended
			// fix MSVC not handling empty struct inits
			if methods.len == 0 && isym.idx == ast.error_type_idx {
				methods_struct.writeln('\t\t._method_msg = NULL,')
				methods_struct.writeln('\t\t._method_code = NULL,')
			}
			// <<

			if g.pref.build_mode != .build_module {
				methods_struct.writeln('\t},')
			}
			iin_idx := already_generated_mwrappers[interface_index_name] - iinidx_minimum_base
			if g.pref.build_mode != .build_module {
				sb.writeln('${g.static_modifier} const int ${interface_index_name} = ${iin_idx};')
			} else {
				sb.writeln('extern const int ${interface_index_name};')
			}
		}
		for vtyp, variants in inter_info.conversions {
			vsym := g.table.sym(vtyp)

			if variants.len > 0 {
				conversion_functions.write_string('static inline bool I_${interface_name}_is_I_${vsym.cname}(${interface_name} x) {\n\treturn ')
				for i, variant in variants {
					variant_sym := g.table.sym(variant)
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
				conversion_functions.write_string('\tpanic_debug(1, tos3("builtin.v"), tos3("builtin"), tos3("__as_cast"), ')
				conversion_functions.write_string(pmessage)
				conversion_functions.writeln(');')
			} else {
				conversion_functions.write_string('\t_v_panic(')
				conversion_functions.write_string(pmessage)
				conversion_functions.writeln(');')
			}
			conversion_functions.writeln('\treturn (${vsym.cname}){0};')
			conversion_functions.writeln('}')
		}
		sb.writeln('// ^^^ number of types for interface ${interface_name}: ${current_iinidx - iinidx_minimum_base}')
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
			sb.writeln(methods_wrapper.str())
			sb.writeln(methods_struct_def.str())
			sb.writeln(methods_struct.str())
		}
		sb.writeln(cast_functions.str())
	}
	sb.writeln(conversion_functions.str())
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
pub fn (mut g Gen) contains_ptr(el_typ ast.Type) bool {
	if el_typ.is_any_kind_of_pointer() {
		return true
	}
	typ := g.unwrap_generic(el_typ)
	if typ.is_ptr() {
		return true
	}
	sym := g.table.final_sym(typ)
	if sym.language != .v {
		return true
	}
	match sym.kind {
		.i8, .i16, .int, .i64, .u8, .u16, .u32, .u64, .f32, .f64, .char, .rune, .bool, .enum_ {
			return false
		}
		.array_fixed {
			info := sym.info as ast.ArrayFixed
			return g.contains_ptr(info.elem_type)
		}
		.struct_ {
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
