// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import os
import strings
import v.ast
import v.table
import v.pref
import v.token
import v.util
import v.depgraph

const (
	// NB: some of the words in c_reserved, are not reserved in C,
	// but are in C++, or have special meaning in V, thus need escaping too.
	c_reserved = ['auto', 'break', 'calloc', 'case', 'char', 'class', 'const', 'continue', 'default',
		'delete', 'do', 'double', 'else', 'enum', 'error', 'exit', 'export', 'extern', 'float',
		'for', 'free', 'goto', 'if', 'inline', 'int', 'link', 'long', 'malloc', 'namespace', 'new',
		'panic', 'register', 'restrict', 'return', 'short', 'signed', 'sizeof', 'static', 'struct',
		'switch', 'typedef', 'typename', 'union', 'unix', 'unsigned', 'void', 'volatile', 'while']
	// same order as in token.Kind
	cmp_str    = ['eq', 'ne', 'gt', 'lt', 'ge', 'le']
	// when operands are switched
	cmp_rev    = ['eq', 'ne', 'lt', 'gt', 'le', 'ge']
)

struct Gen {
	pref         &pref.Preferences
	module_built string
mut:
	table                 &table.Table
	out                   strings.Builder
	cheaders              strings.Builder
	includes              strings.Builder // all C #includes required by V modules
	typedefs              strings.Builder
	typedefs2             strings.Builder
	type_definitions      strings.Builder // typedefs, defines etc (everything that goes to the top of the file)
	definitions           strings.Builder // typedefs, defines etc (everything that goes to the top of the file)
	inits                 map[string]strings.Builder // contents of `void _vinit/2{}`
	cleanups              map[string]strings.Builder // contents of `void _vcleanup(){}`
	gowrappers            strings.Builder // all go callsite wrappers
	stringliterals        strings.Builder // all string literals (they depend on tos3() beeing defined
	auto_str_funcs        strings.Builder // function bodies of all auto generated _str funcs
	comptime_defines      strings.Builder // custom defines, given by -d/-define flags on the CLI
	pcs_declarations      strings.Builder // -prof profile counter declarations for each function
	hotcode_definitions   strings.Builder // -live declarations & functions
	embedded_data         strings.Builder // data to embed in the executable/binary
	shared_types          strings.Builder // shared/lock types
	shared_functions      strings.Builder // shared constructors
	channel_definitions   strings.Builder // channel related code
	options_typedefs      strings.Builder // Option typedefs
	options               strings.Builder // `Option_xxxx` types
	json_forward_decls    strings.Builder // json type forward decls
	enum_typedefs         strings.Builder // enum types
	sql_buf               strings.Builder // for writing exprs to args via `sqlite3_bind_int()` etc
	file                  ast.File
	fn_decl               &ast.FnDecl // pointer to the FnDecl we are currently inside otherwise 0
	last_fn_c_name        string
	tmp_count             int      // counter for unique tmp vars (_tmp1, tmp2 etc)
	tmp_count2            int      // a separate tmp var counter for autofree fn calls
	is_c_call             bool     // e.g. `C.printf("v")`
	is_assign_lhs         bool     // inside left part of assign expr (for array_set(), etc)
	discard_or_result     bool     // do not safe last ExprStmt of `or` block in tmp variable to defer ongoing expr usage
	is_void_expr_stmt     bool     // ExprStmt whos result is discarded
	is_arraymap_set       bool     // map or array set value state
	is_amp                bool     // for `&Foo{}` to merge PrefixExpr `&` and StructInit `Foo{}`; also for `&byte(0)` etc
	is_sql                bool     // Inside `sql db{}` statement, generating sql instead of C (e.g. `and` instead of `&&` etc)
	is_shared             bool     // for initialization of hidden mutex in `[rw]shared` literals
	is_vlines_enabled     bool     // is it safe to generate #line directives when -g is passed
	arraymap_set_pos      int      // map or array set value position
	vlines_path           string   // set to the proper path for generating #line directives
	optionals             []string // to avoid duplicates TODO perf, use map
	chan_pop_optionals    []string // types for `x := <-ch or {...}`
	chan_push_optionals   []string // types for `ch <- x or {...}`
	shareds               []int    // types with hidden mutex for which decl has been emitted
	inside_ternary        int      // ?: comma separated statements on a single line
	inside_map_postfix    bool     // inside map++/-- postfix expr
	inside_map_infix      bool     // inside map<</+=/-= infix expr
	inside_map_index      bool
	inside_opt_data       bool
	inside_if_optional    bool
	ternary_names         map[string]string
	ternary_level_names   map[string][]string
	stmt_path_pos         []int // positions of each statement start, for inserting C statements before the current statement
	skip_stmt_pos         bool  // for handling if expressions + autofree (since both prepend C statements)
	right_is_opt          bool
	is_autofree           bool // false, inside the bodies of fns marked with [manualfree], otherwise === g.pref.autofree
	indent                int
	empty_line            bool
	is_test               bool
	assign_op             token.Kind // *=, =, etc (for array_set)
	defer_stmts           []ast.DeferStmt
	defer_ifdef           string
	defer_profile_code    string
	str_types             []string     // types that need automatic str() generation
	threaded_fns          []string     // for generating unique wrapper types and fns for `go xxx()`
	waiter_fns            []string     // functions that wait for `go xxx()` to finish
	array_fn_definitions  []string     // array equality functions that have been defined
	map_fn_definitions    []string     // map equality functions that have been defined
	struct_fn_definitions []string     // struct equality functions that have been defined
	alias_fn_definitions  []string     // alias equality functions that have been defined
	auto_fn_definitions   []string     // auto generated functions defination list
	anon_fn_definitions   []string     // anon generated functions defination list
	sumtype_definitions   map[int]bool // `_TypeA_to_sumtype_TypeB()` fns that have been generated
	is_json_fn            bool     // inside json.encode()
	json_types            []string // to avoid json gen duplicates
	pcs                   []ProfileCounterMeta // -prof profile counter fn_names => fn counter name
	is_builtin_mod        bool
	hotcode_fn_names      []string
	embedded_files        []ast.EmbeddedFile
	cur_fn                ast.FnDecl
	cur_generic_types     []table.Type // `int`, `string`, etc in `foo<T>()`
	sql_i                 int
	sql_stmt_name         string
	sql_side              SqlExprSide // left or right, to distinguish idents in `name == name`
	inside_vweb_tmpl      bool
	inside_return         bool
	inside_or_block       bool
	strs_to_free0         []string // strings.Builder
	// strs_to_free          []string // strings.Builder
	inside_call           bool
	has_main              bool
	inside_const          bool
	comp_for_method       string      // $for method in T.methods {}
	comp_for_field_var    string      // $for field in T.fields {}; the variable name
	comp_for_field_value  table.Field // value of the field variable
	comp_for_field_type   table.Type  // type of the field variable inferred from `$if field.typ is T {}`
	comptime_var_type_map map[string]table.Type
	// tmp_arg_vars_to_free  []string
	// autofree_pregen       map[string]string
	// autofree_pregen_buf   strings.Builder
	// autofree_tmp_vars     []string // to avoid redefining the same tmp vars in a single function
	called_fn_name   string
	cur_mod          ast.Module
	is_js_call       bool // for handling a special type arg #1 `json.decode(User, ...)`
	is_fn_index_call bool
	// nr_vars_to_free       int
	// doing_autofree_tmp    bool
	inside_lambda                    bool
	prevent_sum_type_unwrapping_once bool // needed for assign new values to sum type
	// used in match multi branch
	// TypeOne, TypeTwo {}
	// where an aggregate (at least two types) is generated
	// sum type deref needs to know which index to deref because unions take care of the correct field
	aggregate_type_idx int
	returned_var_name  string // to detect that a var doesn't need to be freed since it's being returned
	branch_parent_pos  int    // used in BranchStmt (continue/break) for autofree stop position
	timers             &util.Timers = util.new_timers(false)
	force_main_console bool // true when [console] used on fn main()
	as_cast_type_names map[string]string // table for type name lookup in runtime (for __as_cast)
	obf_table          map[string]string
	// main_fn_decl_node  ast.FnDecl
}

pub fn gen(files []ast.File, table &table.Table, pref &pref.Preferences) string {
	// println('start cgen2')
	mut module_built := ''
	if pref.build_mode == .build_module {
		for file in files {
			if pref.path in file.path
				&& file.mod.short_name == pref.path.all_after_last(os.path_separator).trim_right(os.path_separator) {
				module_built = file.mod.name
				break
			}
		}
	}
	mut timers_should_print := false
	$if time_cgening ? {
		timers_should_print = true
	}
	mut g := Gen{
		out: strings.new_builder(512000)
		cheaders: strings.new_builder(15000)
		includes: strings.new_builder(100)
		typedefs: strings.new_builder(100)
		typedefs2: strings.new_builder(100)
		type_definitions: strings.new_builder(100)
		definitions: strings.new_builder(100)
		gowrappers: strings.new_builder(100)
		stringliterals: strings.new_builder(100)
		auto_str_funcs: strings.new_builder(100)
		comptime_defines: strings.new_builder(100)
		pcs_declarations: strings.new_builder(100)
		hotcode_definitions: strings.new_builder(100)
		embedded_data: strings.new_builder(1000)
		options_typedefs: strings.new_builder(100)
		options: strings.new_builder(100)
		shared_types: strings.new_builder(100)
		shared_functions: strings.new_builder(100)
		channel_definitions: strings.new_builder(100)
		json_forward_decls: strings.new_builder(100)
		enum_typedefs: strings.new_builder(100)
		sql_buf: strings.new_builder(100)
		table: table
		pref: pref
		fn_decl: 0
		is_autofree: true
		indent: -1
		module_built: module_built
		timers: util.new_timers(timers_should_print)
	}
	g.timers.start('cgen init')
	for mod in g.table.modules {
		g.inits[mod] = strings.new_builder(100)
		g.cleanups[mod] = strings.new_builder(100)
	}
	g.init()
	g.timers.show('cgen init')
	mut tests_inited := false
	mut autofree_used := false
	for file in files {
		g.timers.start('cgen_file $file.path')
		g.file = file
		if g.pref.is_vlines {
			g.vlines_path = util.vlines_escape_path(file.path, g.pref.ccompiler)
		}
		// println('\ncgen "$g.file.path" nr_stmts=$file.stmts.len')
		// building_v := true && (g.file.path.contains('/vlib/') || g.file.path.contains('cmd/v'))
		g.is_test = g.pref.is_test
		if g.file.path == '' || !g.pref.autofree {
			// cgen test or building V
			// println('autofree=false')
			g.is_autofree = false
		} else {
			g.is_autofree = true
			autofree_used = true
		}
		// anon fn may include assert and thus this needs
		// to be included before any test contents are written
		if g.is_test && !tests_inited {
			g.write_tests_definitions()
			tests_inited = true
		}
		g.stmts(file.stmts)
		// Transfer embedded files
		if file.embedded_files.len > 0 {
			for path in file.embedded_files {
				if path !in g.embedded_files {
					g.embedded_files << path
				}
			}
		}
		g.timers.show('cgen_file $file.path')
	}
	g.timers.start('cgen common')
	if autofree_used {
		g.is_autofree = true // so that void _vcleanup is generated
	}
	// to make sure type idx's are the same in cached mods
	if g.pref.build_mode == .build_module {
		for idx, typ in g.table.types {
			if idx == 0 {
				continue
			}
			g.definitions.writeln('int _v_type_idx_${typ.cname}();')
		}
	} else if g.pref.use_cache {
		for idx, typ in g.table.types {
			if idx == 0 {
				continue
			}
			g.definitions.writeln('int _v_type_idx_${typ.cname}() { return $idx; };')
		}
	}
	//
	g.dump_expr_definitions()
	// v files are finished, what remains is pure C code
	g.gen_vlines_reset()
	if g.pref.build_mode != .build_module {
		// no init in builtin.o
		g.write_init_function()
	}

	g.finish()

	mut b := strings.new_builder(640000)
	b.write_string(g.hashes())
	b.writeln('\n// V comptime_defines:')
	b.write_string(g.comptime_defines.str())
	b.writeln('\n// V typedefs:')
	b.write_string(g.typedefs.str())
	b.writeln('\n// V typedefs2:')
	b.write_string(g.typedefs2.str())
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
	b.writeln('\n// V type definitions:')
	b.write_string(g.type_definitions.str())
	b.writeln('\n// V shared types:')
	b.write_string(g.shared_types.str())
	b.writeln('\n// V Option_xxx definitions:')
	b.write_string(g.options.str())
	b.writeln('\n// V json forward decls:')
	b.write_string(g.json_forward_decls.str())
	b.writeln('\n// V definitions:')
	b.write_string(g.definitions.str())
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
	if g.options_typedefs.len > 0 {
		b.writeln('\n// V option typedefs:')
		b.write_string(g.options_typedefs.str())
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
	if g.stringliterals.len > 0 {
		b.writeln('\n// V stringliterals:')
		b.write_string(g.stringliterals.str())
	}
	if g.auto_str_funcs.len > 0 {
		// if g.pref.build_mode != .build_module {
		b.writeln('\n// V auto str functions:')
		b.write_string(g.auto_str_funcs.str())
		// }
	}
	if g.auto_fn_definitions.len > 0 {
		for fn_def in g.auto_fn_definitions {
			b.writeln(fn_def)
		}
	}
	if g.anon_fn_definitions.len > 0 {
		for fn_def in g.anon_fn_definitions {
			b.writeln(fn_def)
		}
	}
	b.writeln('\n// V out')
	b.write_string(g.out.str())
	b.writeln('\n// THE END.')
	g.timers.show('cgen common')
	return b.str()
}

pub fn (g &Gen) hashes() string {
	mut res := c_commit_hash_default.replace('@@@', util.vhash())
	res += c_current_commit_hash_default.replace('@@@', util.githash(g.pref.building_v))
	return res
}

pub fn (mut g Gen) init() {
	if g.pref.custom_prelude != '' {
		g.cheaders.writeln(g.pref.custom_prelude)
	} else if !g.pref.no_preludes {
		g.cheaders.writeln('// Generated by the V compiler')
		tcc_undef_has_include := '
#if defined(__TINYC__) && defined(__has_include)
// tcc does not support has_include properly yet, turn it off completely
#undef __has_include
#endif'
		g.cheaders.writeln(tcc_undef_has_include)
		g.includes.writeln(tcc_undef_has_include)
		g.cheaders.writeln(get_guarded_include_text('<inttypes.h>', 'The C compiler can not find <inttypes.h> . Please install build-essentials')) // int64_t etc
		g.cheaders.writeln(c_builtin_types)
		if g.pref.is_bare {
			g.cheaders.writeln(bare_c_headers)
		} else {
			g.cheaders.writeln(c_headers)
		}
		g.definitions.writeln('void _STR_PRINT_ARG(const char*, char**, int*, int*, int, ...);')
		g.definitions.writeln('string _STR(const char*, int, ...);')
		g.definitions.writeln('string _STR_TMP(const char*, ...);')
	}
	if g.pref.os == .ios {
		g.cheaders.writeln('#define __TARGET_IOS__ 1')
		g.cheaders.writeln('#include <spawn.h>')
	}
	g.write_builtin_types()
	g.write_typedef_types()
	g.write_typeof_functions()
	if g.pref.build_mode != .build_module {
		// _STR functions should not be defined in builtin.o
		g.write_str_fn_definitions()
	}
	g.write_sorted_types()
	g.write_multi_return_types()
	g.definitions.writeln('// end of definitions #endif')
	//
	g.stringliterals.writeln('')
	g.stringliterals.writeln('// >> string literal consts')
	if g.pref.build_mode != .build_module {
		g.stringliterals.writeln('void vinit_string_literals(){')
	}
	if g.pref.compile_defines_all.len > 0 {
		g.comptime_defines.writeln('// V compile time defines by -d or -define flags:')
		g.comptime_defines.writeln('//     All custom defines      : ' +
			g.pref.compile_defines_all.join(','))
		g.comptime_defines.writeln('//     Turned ON custom defines: ' +
			g.pref.compile_defines.join(','))
		for cdefine in g.pref.compile_defines {
			g.comptime_defines.writeln('#define CUSTOM_DEFINE_$cdefine')
		}
		g.comptime_defines.writeln('')
	}
	if g.pref.is_debug || 'debug' in g.pref.compile_defines {
		g.comptime_defines.writeln('#define _VDEBUG (1)')
	}
	if g.pref.is_prod || 'prod' in g.pref.compile_defines {
		g.comptime_defines.writeln('#define _VPROD (1)')
	}
	if g.pref.is_test || 'test' in g.pref.compile_defines {
		g.comptime_defines.writeln('#define _VTEST (1)')
	}
	if g.pref.autofree {
		g.comptime_defines.writeln('#define _VAUTOFREE (1)')
		// g.comptime_defines.writeln('unsigned char* g_cur_str;')
	}
	if g.pref.prealloc {
		g.comptime_defines.writeln('#define _VPREALLOC (1)')
	}
	if g.pref.is_livemain || g.pref.is_liveshared {
		g.generate_hotcode_reloading_declarations()
	}
	// Obfuscate only functions in the main module for now.
	// Generate the obf_table.
	if g.pref.obfuscate {
		mut i := 0
		// fns
		for key, f in g.table.fns {
			if f.mod != 'main' && key != 'main' { // !key.starts_with('main.') {
				continue
			}
			g.obf_table[key] = '_f$i'
			i++
		}
		// methods
		for type_sym in g.table.types {
			if type_sym.mod != 'main' {
				continue
			}
			for method in type_sym.methods {
				g.obf_table[type_sym.name + '.' + method.name] = '_f$i'
				i++
			}
		}
	}
}

pub fn (mut g Gen) finish() {
	if g.pref.build_mode != .build_module {
		g.stringliterals.writeln('}')
	}
	g.stringliterals.writeln('// << string literal consts')
	g.stringliterals.writeln('')
	if g.pref.is_prof && g.pref.build_mode != .build_module {
		g.gen_vprint_profile_stats()
	}
	if g.pref.is_livemain || g.pref.is_liveshared {
		g.generate_hotcode_reloader_code()
	}
	if g.embed_file_is_prod_mode() && g.embedded_files.len > 0 {
		g.gen_embedded_data()
	}
	if g.pref.is_test {
		g.gen_c_main_for_tests()
	} else {
		g.gen_c_main()
	}
}

pub fn (mut g Gen) write_typeof_functions() {
	g.writeln('')
	g.writeln('// >> typeof() support for sum types / interfaces')
	for typ in g.table.types {
		if typ.kind == .sum_type {
			sum_info := typ.info as table.SumType
			g.writeln('static char * v_typeof_sumtype_${typ.cname}(int sidx) { /* $typ.name */ ')
			if g.pref.build_mode == .build_module {
				g.writeln('\t\tif( sidx == _v_type_idx_${typ.cname}() ) return "${util.strip_main_name(typ.name)}";')
				for v in sum_info.variants {
					subtype := g.table.get_type_symbol(v)
					g.writeln('\tif( sidx == _v_type_idx_${subtype.cname}() ) return "${util.strip_main_name(subtype.name)}";')
				}
				g.writeln('\treturn "unknown ${util.strip_main_name(typ.name)}";')
			} else {
				tidx := g.table.find_type_idx(typ.name)
				g.writeln('\tswitch(sidx) {')
				g.writeln('\t\tcase $tidx: return "${util.strip_main_name(typ.name)}";')
				for v in sum_info.variants {
					subtype := g.table.get_type_symbol(v)
					g.writeln('\t\tcase $v: return "${util.strip_main_name(subtype.name)}";')
				}
				g.writeln('\t\tdefault: return "unknown ${util.strip_main_name(typ.name)}";')
				g.writeln('\t}')
			}
			g.writeln('}')
		} else if typ.kind == .interface_ {
			inter_info := typ.info as table.Interface
			g.writeln('static char * v_typeof_interface_${typ.cname}(int sidx) { /* $typ.name */ ')
			for t in inter_info.types {
				subtype := g.table.get_type_symbol(t)
				g.writeln('\tif (sidx == _${typ.cname}_${subtype.cname}_index) return "${util.strip_main_name(subtype.name)}";')
			}
			g.writeln('\treturn "unknown ${util.strip_main_name(typ.name)}";')
			g.writeln('}')
		}
	}
	g.writeln('// << typeof() support for sum types')
	g.writeln('')
}

// V type to C typecc
fn (mut g Gen) typ(t table.Type) string {
	styp := g.base_type(t)
	if t.has_flag(.optional) {
		// Register an optional if it's not registered yet
		return g.register_optional(t)
	}
	/*
	if styp.starts_with('C__') {
		return styp[3..]
	}
	*/
	return styp
}

fn (mut g Gen) base_type(t table.Type) string {
	share := t.share()
	mut styp := if share == .atomic_t { t.atomic_typename() } else { g.cc_type(t, true) }
	if t.has_flag(.shared_f) {
		styp = g.find_or_register_shared(t, styp)
	}
	nr_muls := t.nr_muls()
	if nr_muls > 0 {
		styp += strings.repeat(`*`, nr_muls)
	}
	return styp
}

fn (mut g Gen) expr_string(expr ast.Expr) string {
	pos := g.out.len
	g.expr(expr)
	expr_str := g.out.after(pos)
	g.out.go_back(expr_str.len)
	return expr_str.trim_space()
}

// TODO this really shouldnt be seperate from typ
// but I(emily) would rather have this generation
// all unified in one place so that it doesnt break
// if one location changes
fn (mut g Gen) optional_type_name(t table.Type) (string, string) {
	base := g.base_type(t)
	mut styp := 'Option_$base'
	if t.is_ptr() {
		styp = styp.replace('*', '_ptr')
	}
	return styp, base
}

fn (g &Gen) optional_type_text(styp string, base string) string {
	// replace void with something else
	size := if base == 'void' { 'byte' } else { base }
	ret := 'struct $styp {
	byte state;
	IError err;
	byte data[sizeof($size)];
}'
	return ret
}

fn (mut g Gen) register_optional(t table.Type) string {
	styp, base := g.optional_type_name(t)
	if styp !in g.optionals {
		g.typedefs2.writeln('typedef struct $styp $styp;')
		g.options.write_string(g.optional_type_text(styp, base))
		g.options.writeln(';\n')
		g.optionals << styp.clone()
	}
	return styp
}

fn (mut g Gen) find_or_register_shared(t table.Type, base string) string {
	sh_typ := '__shared__$base'
	t_idx := t.idx()
	if t_idx in g.shareds {
		return sh_typ
	}
	mtx_typ := 'sync__RwMutex'
	g.shared_types.writeln('struct $sh_typ { $base val; $mtx_typ mtx; };')
	g.shared_functions.writeln('static inline voidptr __dup${sh_typ}(voidptr src, int sz) {')
	g.shared_functions.writeln('\t$sh_typ* dest = memdup(src, sz);')
	g.shared_functions.writeln('\tsync__RwMutex_init(&dest->mtx);')
	g.shared_functions.writeln('\treturn dest;')
	g.shared_functions.writeln('}')
	g.typedefs2.writeln('typedef struct $sh_typ $sh_typ;')
	// println('registered shared type $sh_typ')
	g.shareds << t_idx
	return sh_typ
}

fn (mut g Gen) register_thread_array_wait_call(eltyp string) string {
	is_void := eltyp == 'void'
	thread_typ := if is_void { '__v_thread' } else { '__v_thread_$eltyp' }
	ret_typ := if is_void { 'void' } else { 'Array_$eltyp' }
	thread_arr_typ := 'Array_$thread_typ'
	fn_name := '${thread_arr_typ}_wait'
	if fn_name !in g.waiter_fns {
		g.waiter_fns << fn_name
		if is_void {
			g.gowrappers.writeln('
void ${fn_name}($thread_arr_typ a) {
	for (int i = 0; i < a.len; ++i) {
		$thread_typ t = (($thread_typ*)a.data)[i];
		__v_thread_wait(t);
	}
}')
		} else {
			g.gowrappers.writeln('
$ret_typ ${fn_name}($thread_arr_typ a) {
	$ret_typ res = __new_array_with_default(a.len, a.len, sizeof($eltyp), 0);
	for (int i = 0; i < a.len; ++i) {
		$thread_typ t = (($thread_typ*)a.data)[i];
		(($eltyp*)res.data)[i] = __v_thread_${eltyp}_wait(t);
	}
	return res;
}')
		}
	}
	return fn_name
}

fn (mut g Gen) register_chan_pop_optional_call(opt_el_type string, styp string) {
	if opt_el_type !in g.chan_pop_optionals {
		g.chan_pop_optionals << opt_el_type
		g.channel_definitions.writeln('
static inline $opt_el_type __Option_${styp}_popval($styp ch) {
	$opt_el_type _tmp = {0};
	if (sync__Channel_try_pop_priv(ch, _tmp.data, false)) {
		return ($opt_el_type){ .state = 2, .err = v_error(_SLIT("channel closed")) };
	}
	return _tmp;
}')
	}
}

fn (mut g Gen) register_chan_push_optional_call(el_type string, styp string) {
	if styp !in g.chan_push_optionals {
		g.chan_push_optionals << styp
		g.channel_definitions.writeln('
static inline Option_void __Option_${styp}_pushval($styp ch, $el_type e) {
	if (sync__Channel_try_push_priv(ch, &e, false)) {
		return (Option_void){ .state = 2, .err = v_error(_SLIT("channel closed")) };
	}
	return (Option_void){0};
}')
	}
}

// cc_type whether to prefix 'struct' or not (C__Foo -> struct Foo)
fn (g &Gen) cc_type(typ table.Type, is_prefix_struct bool) string {
	sym := g.table.get_type_symbol(g.unwrap_generic(typ))
	mut styp := sym.cname
	// TODO: this needs to be removed; cgen shouldn't resolve generic types (job of checker)
	if mut sym.info is table.Struct {
		if sym.info.generic_types.len > 0 {
			mut sgtyps := '_T'
			for gt in sym.info.generic_types {
				gts := g.table.get_type_symbol(if gt.has_flag(.generic) {
					g.unwrap_generic(gt)
				} else {
					gt
				})
				sgtyps += '_$gts.cname'
			}
			styp += sgtyps
		}
	} else if mut sym.info is table.MultiReturn {
		// TODO: this doesn't belong here, but makes it working for now
		mut cname := 'multi_return'
		for mr_typ in sym.info.types {
			mr_type_sym := g.table.get_type_symbol(g.unwrap_generic(mr_typ))
			cname += '_$mr_type_sym.cname'
		}
		return cname
	}
	if is_prefix_struct && styp.starts_with('C__') {
		styp = styp[3..]
		if sym.kind == .struct_ {
			info := sym.info as table.Struct
			if !info.is_typedef {
				styp = 'struct $styp'
			}
		}
	}
	return styp
}

[inline]
fn (g &Gen) type_sidx(t table.Type) string {
	if g.pref.build_mode == .build_module {
		sym := g.table.get_type_symbol(t)
		return '_v_type_idx_${sym.cname}()'
	}
	return '$t.idx()'
}

//
pub fn (mut g Gen) write_typedef_types() {
	for typ in g.table.types {
		if typ.name in c.builtins {
			continue
		}
		match typ.kind {
			.alias {
				parent := unsafe { &g.table.types[typ.parent_idx] }
				is_c_parent := parent.name.len > 2 && parent.name[0] == `C` && parent.name[1] == `.`
				mut is_typedef := false
				if parent.info is table.Struct {
					is_typedef = parent.info.is_typedef
				}
				mut parent_styp := parent.cname
				if is_c_parent {
					if !is_typedef {
						parent_styp = 'struct ' + parent.cname[3..]
					} else {
						parent_styp = parent.cname[3..]
					}
				}
				g.type_definitions.writeln('typedef $parent_styp $typ.cname;')
			}
			.array {
				g.type_definitions.writeln('typedef array $typ.cname;')
			}
			.interface_ {
				g.write_interface_typesymbol_declaration(typ)
			}
			.chan {
				if typ.name != 'chan' {
					g.type_definitions.writeln('typedef chan $typ.cname;')
					chan_inf := typ.chan_info()
					chan_elem_type := chan_inf.elem_type
					if !chan_elem_type.has_flag(.generic) {
						el_stype := g.typ(chan_elem_type)
						g.channel_definitions.writeln('
static inline $el_stype __${typ.cname}_popval($typ.cname ch) {
	$el_stype val;
	sync__Channel_try_pop_priv(ch, &val, false);
	return val;
}')
						g.channel_definitions.writeln('
static inline void __${typ.cname}_pushval($typ.cname ch, $el_stype val) {
	sync__Channel_try_push_priv(ch, &val, false);
}')
					}
				}
			}
			.map {
				g.type_definitions.writeln('typedef map $typ.cname;')
			}
			.function {
				g.write_fn_typesymbol_declaration(typ)
			}
			else {
				continue
			}
		}
	}
}

pub fn (mut g Gen) write_interface_typesymbol_declaration(sym table.TypeSymbol) {
	info := sym.info as table.Interface
	g.type_definitions.writeln('typedef struct {')
	g.type_definitions.writeln('\tvoid* _object;')
	g.type_definitions.writeln('\tint _interface_idx;')
	for field in info.fields {
		styp := g.typ(field.typ)
		cname := c_name(field.name)
		g.type_definitions.writeln('\t$styp* $cname;')
	}
	g.type_definitions.writeln('} ${c_name(sym.name)};\n')
}

pub fn (mut g Gen) write_fn_typesymbol_declaration(sym table.TypeSymbol) {
	info := sym.info as table.FnType
	func := info.func
	is_fn_sig := func.name == ''
	not_anon := !info.is_anon
	if !info.has_decl && (not_anon || is_fn_sig) && !func.return_type.has_flag(.generic) {
		fn_name := sym.cname
		g.type_definitions.write_string('typedef ${g.typ(func.return_type)} (*$fn_name)(')
		for i, param in func.params {
			g.type_definitions.write_string(g.typ(param.typ))
			if i < func.params.len - 1 {
				g.type_definitions.write_string(',')
			}
		}
		g.type_definitions.writeln(');')
	}
}

pub fn (mut g Gen) write_multi_return_types() {
	g.typedefs.writeln('\n// BEGIN_multi_return_typedefs')
	g.type_definitions.writeln('\n// BEGIN_multi_return_structs')
	for sym in g.table.types {
		if sym.kind != .multi_return {
			continue
		}
		info := sym.mr_info()
		if info.types.filter(it.has_flag(.generic)).len > 0 {
			continue
		}
		g.typedefs.writeln('typedef struct $sym.cname $sym.cname;')
		g.type_definitions.writeln('struct $sym.cname {')
		for i, mr_typ in info.types {
			type_name := g.typ(mr_typ)
			g.type_definitions.writeln('\t$type_name arg$i;')
		}
		g.type_definitions.writeln('};\n')
	}
	g.typedefs.writeln('// END_multi_return_typedefs\n')
	g.type_definitions.writeln('// END_multi_return_structs\n')
}

pub fn (mut g Gen) write(s string) {
	$if trace_gen ? {
		eprintln('gen file: ${g.file.path:-30} | last_fn_c_name: ${g.last_fn_c_name:-45} | write: $s')
	}
	if g.indent > 0 && g.empty_line {
		g.out.write_string(util.tabs(g.indent))
	}
	g.out.write_string(s)
	g.empty_line = false
}

pub fn (mut g Gen) writeln(s string) {
	$if trace_gen ? {
		eprintln('gen file: ${g.file.path:-30} | last_fn_c_name: ${g.last_fn_c_name:-45} | writeln: $s')
	}
	if g.indent > 0 && g.empty_line {
		g.out.write_string(util.tabs(g.indent))
	}
	g.out.writeln(s)
	g.empty_line = true
}

pub fn (mut g Gen) new_tmp_var() string {
	g.tmp_count++
	return '_t$g.tmp_count'
}

pub fn (mut g Gen) current_tmp_var() string {
	return '_t$g.tmp_count'
}

/*
pub fn (mut g Gen) new_tmp_var2() string {
	g.tmp_count2++
	return '_tt$g.tmp_count2'
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

// tmp_var is used in `if` expressions only
fn (mut g Gen) stmts_with_tmp_var(stmts []ast.Stmt, tmp_var string) {
	g.indent++
	if g.inside_ternary > 0 {
		g.write('(')
	}
	for i, stmt in stmts {
		if i == stmts.len - 1 && tmp_var != '' {
			// Handle if expressions, set the value of the last expression to the temp var.
			if g.inside_if_optional {
				g.stmt_path_pos << g.out.len
				g.skip_stmt_pos = true
				if stmt is ast.ExprStmt {
					sym := g.table.get_type_symbol(stmt.typ)
					if sym.name in ['Option2', 'Option'] || stmt.expr is ast.None {
						tmp := g.new_tmp_var()
						g.write('Option $tmp = (Option){.state = 0,.err = ')
						g.expr(stmt.expr)
						g.writeln('};')
						g.writeln('memcpy(&$tmp_var, &$tmp, sizeof(Option));')
					} else {
						mut styp := g.base_type(stmt.typ)
						$if tinyc && x32 && windows {
							if stmt.typ == table.int_literal_type {
								styp = 'int'
							} else if stmt.typ == table.float_literal_type {
								styp = 'f64'
							}
						}
						g.write('opt_ok(&($styp[]) { ')
						g.stmt(stmt)
						g.writeln(' }, (Option*)(&$tmp_var), sizeof($styp));')
					}
				}
			} else {
				g.stmt_path_pos << g.out.len
				g.skip_stmt_pos = true
				g.write('$tmp_var = ')
				g.stmt(stmt)
			}
		} else {
			g.stmt(stmt)
			if g.inside_if_optional && stmt is ast.ExprStmt {
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
		// stmt := stmts[stmts.len-1]
		if stmt !is ast.FnDecl && g.inside_ternary == 0 {
			// g.writeln('// autofree scope')
			// g.writeln('// autofree_scope_vars($stmt.pos.pos) | ${typeof(stmt)}')
			// go back 1 position is important so we dont get the
			// internal scope of for loops and possibly other nodes
			// g.autofree_scope_vars(stmt.pos.pos - 1)
			mut stmt_pos := stmt.pos
			if stmt_pos.pos == 0 {
				// Do not autofree if the position is 0, since the correct scope won't be found.
				// Report a bug, since position shouldn't be 0 for most nodes.
				if stmt is ast.Module {
					return
				}
				if stmt is ast.ExprStmt {
					// For some reason ExprStmt.pos is 0 when ExprStmt.expr is comp if expr
					// Extract the pos. TODO figure out why and fix.
					stmt_pos = stmt.expr.position()
				}
				if stmt_pos.pos == 0 {
					print('autofree: first stmt pos = 0. ')
					println(stmt.type_name())
					return
				}
			}
			g.autofree_scope_vars(stmt_pos.pos - 1, stmt_pos.line_nr, false)
		}
	}
}

[inline]
fn (mut g Gen) write_v_source_line_info(pos token.Position) {
	if g.inside_ternary == 0 && g.pref.is_vlines && g.is_vlines_enabled {
		nline := pos.line_nr + 1
		lineinfo := '\n#line $nline "$g.vlines_path"'
		g.writeln(lineinfo)
	}
}

fn (mut g Gen) stmt(node ast.Stmt) {
	if !g.skip_stmt_pos {
		g.stmt_path_pos << g.out.len
	}
	defer {
	}
	// println('g.stmt()')
	// g.writeln('//// stmt start')
	match node {
		ast.AssertStmt {
			g.write_v_source_line_info(node.pos)
			g.gen_assert_stmt(node)
		}
		ast.AssignStmt {
			g.write_v_source_line_info(node.pos)
			g.gen_assign_stmt(node)
		}
		ast.Block {
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
			if node.label != '' {
				if node.kind == .key_break {
					g.writeln('goto ${node.label}__break;')
				} else {
					// assert node.kind == .key_continue
					g.writeln('goto ${node.label}__continue;')
				}
			} else {
				// continue or break
				if g.is_autofree && !g.is_builtin_mod {
					g.writeln('// free before continue/break')
					g.autofree_scope_vars_stop(node.pos.pos - 1, node.pos.line_nr, true,
						g.branch_parent_pos)
				}
				g.writeln('$node.kind;')
			}
		}
		ast.ConstDecl {
			g.write_v_source_line_info(node.pos)
			// if g.pref.build_mode != .build_module {
			g.const_decl(node)
			// }
		}
		ast.CompFor {
			g.comp_for(node)
		}
		ast.DeferStmt {
			mut defer_stmt := node
			defer_stmt.ifdef = g.defer_ifdef
			g.writeln('${g.defer_flag_var(defer_stmt)} = true;')
			g.defer_stmts << defer_stmt
		}
		ast.EnumDecl {
			enum_name := util.no_dots(node.name)
			is_flag := node.is_flag
			g.enum_typedefs.writeln('typedef enum {')
			mut cur_enum_expr := ''
			mut cur_enum_offset := 0
			for i, field in node.fields {
				g.enum_typedefs.write_string('\t${enum_name}_$field.name')
				if field.has_expr {
					g.enum_typedefs.write_string(' = ')
					expr_str := g.expr_string(field.expr)
					g.enum_typedefs.write_string(expr_str)
					cur_enum_expr = expr_str
					cur_enum_offset = 0
				} else if is_flag {
					g.enum_typedefs.write_string(' = ')
					cur_enum_expr = '1 << $i'
					g.enum_typedefs.write_string((1 << i).str())
					cur_enum_offset = 0
				}
				cur_value := if cur_enum_offset > 0 {
					'$cur_enum_expr+$cur_enum_offset'
				} else {
					cur_enum_expr
				}
				g.enum_typedefs.writeln(', // $cur_value')
				cur_enum_offset++
			}
			g.enum_typedefs.writeln('} $enum_name;\n')
		}
		ast.ExprStmt {
			g.write_v_source_line_info(node.pos)
			// af := g.autofree && node.expr is ast.CallExpr && !g.is_builtin_mod
			// if af {
			// g.autofree_call_pregen(node.expr as ast.CallExpr)
			// }
			old_is_void_expr_stmt := g.is_void_expr_stmt
			g.is_void_expr_stmt = !node.is_expr
			g.expr(node.expr)
			g.is_void_expr_stmt = old_is_void_expr_stmt
			// if af {
			// g.autofree_call_postgen()
			// }
			if g.inside_ternary == 0 && !g.inside_if_optional && !node.is_expr
				&& node.expr !is ast.IfExpr {
				g.writeln(';')
			}
		}
		ast.FnDecl {
			g.process_fn_decl(node)
		}
		ast.ForCStmt {
			prev_branch_parent_pos := g.branch_parent_pos
			g.branch_parent_pos = node.pos.pos
			g.write_v_source_line_info(node.pos)
			g.for_c_stmt(node)
			g.branch_parent_pos = prev_branch_parent_pos
		}
		ast.ForInStmt {
			prev_branch_parent_pos := g.branch_parent_pos
			g.branch_parent_pos = node.pos.pos
			g.write_v_source_line_info(node.pos)
			g.for_in_stmt(node)
			g.branch_parent_pos = prev_branch_parent_pos
		}
		ast.ForStmt {
			prev_branch_parent_pos := g.branch_parent_pos
			g.branch_parent_pos = node.pos.pos
			g.write_v_source_line_info(node.pos)
			g.for_stmt(node)
			g.branch_parent_pos = prev_branch_parent_pos
		}
		ast.GlobalDecl {
			g.global_decl(node)
		}
		ast.GoStmt {
			g.go_stmt(node, false)
		}
		ast.GotoLabel {
			g.writeln('$node.name: {}')
		}
		ast.GotoStmt {
			g.writeln('goto $node.name;')
		}
		ast.HashStmt {
			// #include etc
			if node.kind == 'include' {
				mut missing_message := 'Header file $node.main, needed for module `$node.mod` was not found.'
				if node.msg != '' {
					missing_message += ' ${node.msg}.'
				} else {
					missing_message += ' Please install the corresponding development headers.'
				}
				mut guarded_include := get_guarded_include_text(node.main, missing_message)
				if node.main == '<errno.h>' {
					// fails with musl-gcc and msvc; but an unguarded include works:
					guarded_include = '#include $node.main'
				}
				if node.main.contains('.m') {
					// Objective C code import, include it after V types, so that e.g. `string` is
					// available there
					g.definitions.writeln('// added by module `$node.mod`:')
					g.definitions.writeln(guarded_include)
				} else {
					g.includes.writeln('// added by module `$node.mod`:')
					g.includes.writeln(guarded_include)
				}
			} else if node.kind == 'define' {
				g.includes.writeln('// defined by module `$node.mod` in file `$node.source_file`:')
				g.includes.writeln('#define $node.main')
			}
		}
		ast.Import {}
		ast.InterfaceDecl {
			// definitions are sorted and added in write_types
		}
		ast.Module {
			// g.is_builtin_mod = node.name == 'builtin'
			g.is_builtin_mod = node.name in ['builtin', 'os', 'strconv', 'strings', 'gg']
			// g.cur_mod = node.name
			g.cur_mod = node
		}
		ast.Return {
			g.write_defer_stmts_when_needed()
			// af := g.autofree && node.exprs.len > 0 && node.exprs[0] is ast.CallExpr && !g.is_builtin_mod
			/*
			af := g.autofree && !g.is_builtin_mod
			if false && af {
				g.writeln('// ast.Return free')
				g.autofree_scope_vars(node.pos.pos - 1, node.pos.line_nr, true)
				g.writeln('// ast.Return free_end2')
			}
			*/
			g.return_statement(node)
		}
		ast.SqlStmt {
			g.sql_stmt(node)
		}
		ast.StructDecl {
			name := if node.language == .c { util.no_dots(node.name) } else { c_name(node.name) }
			// TODO For some reason, build fails with autofree with this line
			// as it's only informative, comment it for now
			// g.gen_attrs(node.attrs)
			// g.writeln('typedef struct {')
			// for field in it.fields {
			// field_type_sym := g.table.get_type_symbol(field.typ)
			// g.writeln('\t$field_type_sym.name $field.name;')
			// }
			// g.writeln('} $name;')
			if node.language == .c {
				return
			}
			if node.is_union {
				g.typedefs.writeln('typedef union $name $name;')
			} else {
				g.typedefs.writeln('typedef struct $name $name;')
			}
		}
		ast.TypeDecl {
			g.writeln('// TypeDecl')
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
			// p := node.position()
			// g.autofree_call_postgen(p.pos)
		}
	}
}

fn (mut g Gen) write_defer_stmts() {
	for defer_stmt in g.defer_stmts {
		g.writeln('// Defer begin')
		g.writeln('if (${g.defer_flag_var(defer_stmt)} == true) {')
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
}

fn (mut g Gen) for_c_stmt(node ast.ForCStmt) {
	if node.is_multi {
		g.is_vlines_enabled = false
		if node.label.len > 0 {
			g.writeln('$node.label:')
		}
		g.writeln('{')
		g.indent++
		if node.has_init {
			g.stmt(node.init)
		}
		g.writeln('bool _is_first = true;')
		g.writeln('while (true) {')
		g.writeln('\tif (_is_first) {')
		g.writeln('\t\t_is_first = false;')
		g.writeln('\t} else {')
		if node.has_inc {
			g.indent++
			g.stmt(node.inc)
			g.writeln(';')
			g.indent--
		}
		g.writeln('}')
		if node.has_cond {
			g.write('if (!(')
			g.expr(node.cond)
			g.writeln(')) break;')
		}
		g.is_vlines_enabled = true
		g.stmts(node.stmts)
		if node.label.len > 0 {
			g.writeln('${node.label}__continue: {}')
		}
		g.writeln('}')
		g.indent--
		g.writeln('}')
		if node.label.len > 0 {
			g.writeln('${node.label}__break: {}')
		}
	} else {
		g.is_vlines_enabled = false
		if node.label.len > 0 {
			g.writeln('$node.label:')
		}
		g.write('for (')
		if !node.has_init {
			g.write('; ')
		} else {
			g.stmt(node.init)
			// Remove excess return and add space
			if g.out.last_n(1) == '\n' {
				g.out.go_back(1)
				g.empty_line = false
				g.write(' ')
			}
		}
		if node.has_cond {
			g.expr(node.cond)
		}
		g.write('; ')
		if node.has_inc {
			g.stmt(node.inc)
		}
		g.writeln(') {')
		g.is_vlines_enabled = true
		g.stmts(node.stmts)
		if node.label.len > 0 {
			g.writeln('${node.label}__continue: {}')
		}
		g.writeln('}')
		if node.label.len > 0 {
			g.writeln('${node.label}__break: {}')
		}
	}
}

fn (mut g Gen) for_stmt(node ast.ForStmt) {
	g.is_vlines_enabled = false
	if node.label.len > 0 {
		g.writeln('$node.label:')
	}
	g.writeln('for (;;) {')
	if !node.is_inf {
		g.indent++
		g.stmt_path_pos << g.out.len
		g.write('if (!(')
		g.expr(node.cond)
		g.writeln(')) break;')
		g.indent--
	}
	g.is_vlines_enabled = true
	g.stmts(node.stmts)
	if node.label.len > 0 {
		g.writeln('\t${node.label}__continue: {}')
	}
	g.writeln('}')
	if node.label.len > 0 {
		g.writeln('${node.label}__break: {}')
	}
}

fn (mut g Gen) for_in_stmt(node ast.ForInStmt) {
	if node.label.len > 0 {
		g.writeln('\t$node.label: {}')
	}
	if node.is_range {
		// `for x in 1..10 {`
		i := if node.val_var == '_' { g.new_tmp_var() } else { c_name(node.val_var) }
		g.write('for (int $i = ')
		g.expr(node.cond)
		g.write('; $i < ')
		g.expr(node.high)
		g.writeln('; ++$i) {')
	} else if node.kind == .array {
		// `for num in nums {`
		g.writeln('// FOR IN array')
		styp := g.typ(node.val_type)
		val_sym := g.table.get_type_symbol(node.val_type)
		mut cond_var := ''
		if node.cond is ast.Ident || node.cond is ast.SelectorExpr {
			cond_var = g.expr_string(node.cond)
		} else {
			cond_var = g.new_tmp_var()
			g.write(g.typ(node.cond_type))
			g.write(' $cond_var = ')
			g.expr(node.cond)
			g.writeln(';')
		}
		i := if node.key_var in ['', '_'] { g.new_tmp_var() } else { node.key_var }
		field_accessor := if node.cond_type.is_ptr() { '->' } else { '.' }
		share_accessor := if node.cond_type.share() == .shared_t { 'val.' } else { '' }
		op_field := field_accessor + share_accessor
		g.empty_line = true
		g.writeln('for (int $i = 0; $i < $cond_var${op_field}len; ++$i) {')
		if node.val_var != '_' {
			if val_sym.kind == .function {
				g.write('\t')
				g.write_fn_ptr_decl(val_sym.info as table.FnType, c_name(node.val_var))
				g.writeln(' = ((voidptr*)$cond_var${op_field}data)[$i];')
			} else if val_sym.kind == .array_fixed && !node.val_is_mut {
				right := '(($styp*)$cond_var${op_field}data)[$i]'
				g.writeln('\t$styp ${c_name(node.val_var)};')
				g.writeln('\tmemcpy(*($styp*)${c_name(node.val_var)}, (byte*)$right, sizeof($styp));')
			} else {
				// If val is mutable (pointer behind the scenes), we need to generate
				// `int* val = ((int*)arr.data) + i;`
				// instead of
				// `int* val = ((int**)arr.data)[i];`
				// right := if node.val_is_mut { styp } else { styp + '*' }
				right := if node.val_is_mut {
					'(($styp)$cond_var${op_field}data) + $i'
				} else {
					'(($styp*)$cond_var${op_field}data)[$i]'
				}
				g.writeln('\t$styp ${c_name(node.val_var)} = $right;')
			}
		}
	} else if node.kind == .array_fixed {
		mut cond_var := ''
		cond_type_is_ptr := node.cond_type.is_ptr()
		cond_is_literal := node.cond is ast.ArrayInit
		if cond_is_literal {
			cond_var = g.new_tmp_var()
			g.write(g.typ(node.cond_type))
			g.write(' $cond_var = ')
			g.expr(node.cond)
			g.writeln(';')
		} else if cond_type_is_ptr {
			cond_var = g.new_tmp_var()
			cond_var_type := g.typ(node.cond_type).trim('*')
			if !node.cond.is_lvalue() {
				g.write('$cond_var_type *$cond_var = (($cond_var_type)')
			} else {
				g.write('$cond_var_type *$cond_var = (')
			}
			g.expr(node.cond)
			g.writeln(');')
		} else {
			cond_var = g.expr_string(node.cond)
		}
		idx := if node.key_var in ['', '_'] { g.new_tmp_var() } else { node.key_var }
		cond_sym := g.table.get_type_symbol(node.cond_type)
		info := cond_sym.info as table.ArrayFixed
		g.writeln('for (int $idx = 0; $idx != $info.size; ++$idx) {')
		if node.val_var != '_' {
			val_sym := g.table.get_type_symbol(node.val_type)
			is_fixed_array := val_sym.kind == .array_fixed && !node.val_is_mut
			if val_sym.kind == .function {
				g.write('\t')
				g.write_fn_ptr_decl(val_sym.info as table.FnType, c_name(node.val_var))
			} else if is_fixed_array {
				styp := g.typ(node.val_type)
				g.writeln('\t$styp ${c_name(node.val_var)};')
				g.writeln('\tmemcpy(*($styp*)${c_name(node.val_var)}, (byte*)$cond_var[$idx], sizeof($styp));')
			} else {
				styp := g.typ(node.val_type)
				g.write('\t$styp ${c_name(node.val_var)}')
			}
			if !is_fixed_array {
				addr := if node.val_is_mut { '&' } else { '' }
				if cond_type_is_ptr {
					g.writeln(' = ${addr}(*$cond_var)[$idx];')
				} else if cond_is_literal {
					g.writeln(' = $addr$cond_var[$idx];')
				} else {
					g.write(' = $addr')
					g.expr(node.cond)
					g.writeln('[$idx];')
				}
			}
		}
	} else if node.kind == .map {
		// `for key, val in map {
		g.writeln('// FOR IN map')
		mut cond_var := ''
		if node.cond is ast.Ident {
			cond_var = g.expr_string(node.cond)
		} else {
			cond_var = g.new_tmp_var()
			g.write(g.typ(node.cond_type))
			g.write(' $cond_var = ')
			g.expr(node.cond)
			g.writeln(';')
		}
		arw_or_pt := if node.cond_type.is_ptr() { '->' } else { '.' }
		idx := g.new_tmp_var()
		g.empty_line = true
		g.writeln('for (int $idx = 0; $idx < $cond_var${arw_or_pt}key_values.len; ++$idx) {')
		// TODO: don't have this check when the map has no deleted elements
		g.writeln('\tif (!DenseArray_has_index(&$cond_var${arw_or_pt}key_values, $idx)) {continue;}')
		if node.key_var != '_' {
			key_styp := g.typ(node.key_type)
			key := c_name(node.key_var)
			g.writeln('\t$key_styp $key = /*key*/ *($key_styp*)DenseArray_key(&$cond_var${arw_or_pt}key_values, $idx);')
			// TODO: analyze whether node.key_type has a .clone() method and call .clone() for all types:
			if node.key_type == table.string_type {
				g.writeln('\t$key = string_clone($key);')
			}
		}
		if node.val_var != '_' {
			val_sym := g.table.get_type_symbol(node.val_type)
			if val_sym.kind == .function {
				g.write('\t')
				g.write_fn_ptr_decl(val_sym.info as table.FnType, c_name(node.val_var))
				g.write(' = (*(voidptr*)')
				g.writeln('DenseArray_value(&$cond_var${arw_or_pt}key_values, $idx));')
			} else if val_sym.kind == .array_fixed && !node.val_is_mut {
				val_styp := g.typ(node.val_type)
				g.writeln('\t$val_styp ${c_name(node.val_var)};')
				g.writeln('\tmemcpy(*($val_styp*)${c_name(node.val_var)}, (byte*)DenseArray_value(&$cond_var${arw_or_pt}key_values, $idx), sizeof($val_styp));')
			} else {
				val_styp := g.typ(node.val_type)
				if node.val_type.is_ptr() {
					g.write('\t$val_styp ${c_name(node.val_var)} = &(*($val_styp)')
				} else {
					g.write('\t$val_styp ${c_name(node.val_var)} = (*($val_styp*)')
				}
				g.writeln('DenseArray_value(&$cond_var${arw_or_pt}key_values, $idx));')
			}
		}
	} else if node.kind == .string {
		cond := if node.cond is ast.StringLiteral || node.cond is ast.StringInterLiteral {
			ast.Expr(g.new_ctemp_var_then_gen(node.cond, table.string_type))
		} else {
			node.cond
		}
		i := if node.key_var in ['', '_'] { g.new_tmp_var() } else { node.key_var }
		g.write('for (int $i = 0; $i < ')
		g.expr(cond)
		g.writeln('.len; ++$i) {')
		if node.val_var != '_' {
			g.write('\tbyte ${c_name(node.val_var)} = ')
			g.expr(cond)
			g.writeln('.str[$i];')
		}
	} else if node.kind == .struct_ {
		cond_type_sym := g.table.get_type_symbol(node.cond_type)
		next_fn := cond_type_sym.find_method('next') or {
			verror('`next` method not found')
			return
		}
		ret_typ := next_fn.return_type
		t_expr := g.new_tmp_var()
		g.write('${g.typ(node.cond_type)} $t_expr = ')
		g.expr(node.cond)
		g.writeln(';')
		g.writeln('while (1) {')
		t := g.new_tmp_var()
		receiver_typ := next_fn.params[0].typ
		receiver_styp := g.typ(receiver_typ)
		fn_name := receiver_styp.replace_each(['*', '', '.', '__']) + '_next'
		g.write('\t${g.typ(ret_typ)} $t = ${fn_name}(')
		if !node.cond_type.is_ptr() && receiver_typ.is_ptr() {
			g.write('&')
		}
		g.writeln('$t_expr);')
		g.writeln('\tif (${t}.state != 0) break;')
		val := if node.val_var in ['', '_'] { g.new_tmp_var() } else { node.val_var }
		val_styp := g.typ(node.val_type)
		g.writeln('\t$val_styp $val = *($val_styp*)${t}.data;')
	} else {
		s := g.table.type_to_str(node.cond_type)
		g.error('for in: unhandled symbol `$node.cond` of type `$s`', node.pos)
	}
	g.stmts(node.stmts)
	if node.label.len > 0 {
		g.writeln('\t${node.label}__continue: {}')
	}
	g.writeln('}')
	if node.label.len > 0 {
		g.writeln('\t${node.label}__break: {}')
	}
}

fn (mut g Gen) write_sumtype_casting_fn(got_ table.Type, exp_ table.Type) {
	got, exp := got_.idx(), exp_.idx()
	i := got | (exp << 16)
	if got == exp || g.sumtype_definitions[i] {
		return
	}
	g.sumtype_definitions[i] = true
	got_sym := g.table.get_type_symbol(got)
	exp_sym := g.table.get_type_symbol(exp)
	mut sb := strings.new_builder(128)
	got_cname, exp_cname := got_sym.cname, exp_sym.cname
	sb.writeln('static inline $exp_cname ${got_cname}_to_sumtype_${exp_cname}($got_cname* x) {')
	sb.writeln('\t$got_cname* ptr = memdup(x, sizeof($got_cname));')
	sb.write_string('\treturn ($exp_cname){ ._$got_cname = ptr, ._typ = ${g.type_sidx(got)}')
	for field in (exp_sym.info as table.SumType).fields {
		field_styp := g.typ(field.typ)
		if got_sym.kind in [.sum_type, .interface_] {
			// the field is already a wrapped pointer; we shouldn't wrap it once again
			sb.write_string(', .$field.name = ptr->$field.name')
		} else {
			sb.write_string(', .$field.name = ($field_styp*)((char*)ptr + __offsetof_ptr(ptr, $got_cname, $field.name))')
		}
	}
	sb.writeln('};\n}')
	g.auto_fn_definitions << sb.str()
}

// use instead of expr() when you need to cast to a different type
fn (mut g Gen) expr_with_cast(expr ast.Expr, got_type_raw table.Type, expected_type table.Type) {
	got_type := g.table.mktyp(got_type_raw)
	exp_sym := g.table.get_type_symbol(expected_type)
	expected_is_ptr := expected_type.is_ptr()
	got_is_ptr := got_type.is_ptr()
	got_sym := g.table.get_type_symbol(got_type)
	// allow using the new Error struct as a string, to avoid a breaking change
	// TODO: temporary to allow people to migrate their code; remove soon
	if got_type == table.error_type_idx && expected_type == table.string_type_idx {
		g.write('(*(')
		g.expr(expr)
		g.write('.msg))')
		return
	}
	if exp_sym.kind == .interface_ && got_type_raw.idx() != expected_type.idx()
		&& !expected_type.has_flag(.optional) {
		got_styp := g.cc_type(got_type, true)
		exp_styp := g.cc_type(expected_type, true)
		if expected_is_ptr {
			g.write('HEAP($exp_styp, ')
		}
		g.write('I_${got_styp}_to_Interface_${exp_styp}(')
		if !got_is_ptr {
			g.write('&')
		}
		g.expr(expr)
		g.write(')')
		if expected_is_ptr {
			g.write(')')
		}
		return
	}
	// cast to sum type
	exp_styp := g.typ(expected_type)
	got_styp := g.typ(got_type)
	if expected_type != table.void_type {
		expected_deref_type := if expected_is_ptr { expected_type.deref() } else { expected_type }
		got_deref_type := if got_is_ptr { got_type.deref() } else { got_type }
		if g.table.sumtype_has_variant(expected_deref_type, got_deref_type) {
			mut is_already_sum_type := false
			scope := g.file.scope.innermost(expr.position().pos)
			if expr is ast.Ident {
				if v := scope.find_var(expr.name) {
					if v.sum_type_casts.len > 0 {
						is_already_sum_type = true
					}
				}
			} else if expr is ast.SelectorExpr {
				if _ := scope.find_struct_field(expr.expr_type, expr.field_name) {
					is_already_sum_type = true
				}
			}
			if is_already_sum_type {
				// Don't create a new sum type wrapper if there is already one
				g.prevent_sum_type_unwrapping_once = true
				g.expr(expr)
			} else {
				g.write_sumtype_casting_fn(got_type, expected_type)
				if expected_is_ptr {
					g.write('HEAP($exp_sym.cname, ')
				}
				g.write('${got_sym.cname}_to_sumtype_${exp_sym.cname}(')
				if !got_is_ptr {
					g.write('ADDR($got_styp, (')
					g.expr(expr)
					g.write(')))')
				} else {
					g.expr(expr)
					g.write(')')
				}
				if expected_is_ptr {
					g.write(')')
				}
			}
			return
		}
	}
	// Generic dereferencing logic
	neither_void := table.voidptr_type !in [got_type, expected_type]
	to_shared := expected_type.has_flag(.shared_f) && !got_type_raw.has_flag(.shared_f)
		&& !expected_type.has_flag(.optional)
	// from_shared := got_type_raw.has_flag(.shared_f) && !expected_type.has_flag(.shared_f)
	if to_shared {
		shared_styp := exp_styp[0..exp_styp.len - 1] // `shared` implies ptr, so eat one `*`
		if got_type_raw.is_ptr() {
			g.error('cannot convert reference to `shared`', expr.position())
		}
		if exp_sym.kind == .array {
			g.writeln('($shared_styp*)__dup_shared_array(&($shared_styp){.val = ')
		} else if exp_sym.kind == .map {
			g.writeln('($shared_styp*)__dup_shared_map(&($shared_styp){.val = ')
		} else {
			g.writeln('($shared_styp*)__dup${shared_styp}(&($shared_styp){.val = ')
		}
		g.expr(expr)
		g.writeln('}, sizeof($shared_styp))')
		return
	}
	if got_is_ptr && !expected_is_ptr && neither_void
		&& exp_sym.kind !in [.interface_, .placeholder] && expr !is ast.InfixExpr {
		got_deref_type := got_type.deref()
		deref_sym := g.table.get_type_symbol(got_deref_type)
		deref_will_match := expected_type in [got_type, got_deref_type, deref_sym.parent_idx]
		got_is_opt := got_type.has_flag(.optional)
		if deref_will_match || got_is_opt {
			g.write('*')
		}
	}
	// no cast
	g.expr(expr)
}

// cestring returns a V string, properly escaped for embeddeding in a C string literal.
fn cestring(s string) string {
	return s.replace('\\', '\\\\').replace('"', "'")
}

// ctoslit returns a '_SLIT("$s")' call, where s is properly escaped.
fn ctoslit(s string) string {
	return '_SLIT("' + cestring(s) + '")'
}

fn (mut g Gen) gen_attrs(attrs []table.Attr) {
	for attr in attrs {
		g.writeln('// Attr: [$attr.name]')
	}
}

fn cnewlines(s string) string {
	return s.replace('\n', r'\n')
}

fn (mut g Gen) write_fn_ptr_decl(func &table.FnType, ptr_name string) {
	ret_styp := g.typ(func.func.return_type)
	g.write('$ret_styp (*$ptr_name) (')
	arg_len := func.func.params.len
	for i, arg in func.func.params {
		arg_styp := g.typ(arg.typ)
		g.write('$arg_styp $arg.name')
		if i < arg_len - 1 {
			g.write(', ')
		}
	}
	g.write(')')
}

// TODO this function is scary. Simplify/split up.
fn (mut g Gen) gen_assign_stmt(assign_stmt ast.AssignStmt) {
	if assign_stmt.is_static {
		g.write('static ')
	}
	mut return_type := table.void_type
	is_decl := assign_stmt.op == .decl_assign
	op := if is_decl { token.Kind.assign } else { assign_stmt.op }
	right_expr := assign_stmt.right[0]
	match right_expr {
		ast.CallExpr { return_type = right_expr.return_type }
		ast.LockExpr { return_type = right_expr.typ }
		ast.MatchExpr { return_type = right_expr.return_type }
		ast.IfExpr { return_type = right_expr.typ }
		else {}
	}
	// Free the old value assigned to this string var (only if it's `str = [new value]`
	// or `x.str = [new value]` )
	mut af := g.is_autofree && !g.is_builtin_mod && assign_stmt.op == .assign
		&& assign_stmt.left_types.len == 1
		&& (assign_stmt.left[0] is ast.Ident || assign_stmt.left[0] is ast.SelectorExpr)
	// assign_stmt.left_types[0] in [table.string_type, table.array_type] &&
	mut sref_name := ''
	mut type_to_free := ''
	if af {
		first_left_type := assign_stmt.left_types[0]
		first_left_sym := g.table.get_type_symbol(assign_stmt.left_types[0])
		if first_left_type == table.string_type || first_left_sym.kind == .array {
			type_to_free = if first_left_type == table.string_type { 'string' } else { 'array' }
			mut ok := true
			left0 := assign_stmt.left[0]
			if left0 is ast.Ident {
				if left0.name == '_' {
					ok = false
				}
			}
			if ok {
				sref_name = '_sref$assign_stmt.pos.pos'
				g.write('$type_to_free $sref_name = (') // TODO we are copying the entire string here, optimize
				// we can't just do `.str` since we need the extra data from the string struct
				// doing `&string` is also not an option since the stack memory with the data will be overwritten
				g.expr(left0) // assign_stmt.left[0])
				g.writeln('); // free $type_to_free on re-assignment2')
				defer {
					if af {
						g.writeln('${type_to_free}_free(&$sref_name);')
					}
				}
			} else {
				af = false
			}
		} else {
			af = false
		}
	}
	// Autofree tmp arg vars
	// first_right := assign_stmt.right[0]
	// af := g.autofree && first_right is ast.CallExpr && !g.is_builtin_mod
	// if af {
	// g.autofree_call_pregen(first_right as ast.CallExpr)
	// }
	//
	//
	// Handle optionals. We need to declare a temp variable for them, that's why they are handled
	// here, not in call_expr().
	// `pos := s.index('x') or { return }`
	// ==========>
	// Option_int _t190 = string_index(s, _STR("x"));
	// if (_t190.state != 2) {
	// Error err = _t190.err;
	// return;
	// }
	// int pos = *(int*)_t190.data;
	// mut tmp_opt := ''
	/*
	is_optional := false && g.is_autofree && (assign_stmt.op in [.decl_assign, .assign])
		&& assign_stmt.left_types.len == 1 && assign_stmt.right[0] is ast.CallExpr
	if is_optional {
		// g.write('/* optional assignment */')
		call_expr := assign_stmt.right[0] as ast.CallExpr
		if call_expr.or_block.kind != .absent {
			styp := g.typ(call_expr.return_type.set_flag(.optional))
			tmp_opt = g.new_tmp_var()
			g.write('/*AF opt*/$styp $tmp_opt = ')
			g.expr(assign_stmt.right[0])
			g.or_block(tmp_opt, call_expr.or_block, call_expr.return_type)
			g.writeln('/*=============ret*/')
			// if af && is_optional {
			// g.autofree_call_postgen()
			// }
			// return
		}
	}
	*/
	// json_test failed w/o this check
	if return_type != table.void_type && return_type != 0 {
		sym := g.table.get_type_symbol(return_type)
		if sym.kind == .multi_return {
			// multi return
			// TODO Handle in if_expr
			is_opt := return_type.has_flag(.optional)
			mr_var_name := 'mr_$assign_stmt.pos.pos'
			mr_styp := g.typ(return_type)
			g.write('$mr_styp $mr_var_name = ')
			g.expr(assign_stmt.right[0])
			g.writeln(';')
			for i, lx in assign_stmt.left {
				if lx is ast.Ident {
					if lx.kind == .blank_ident {
						continue
					}
				}
				styp := g.typ(assign_stmt.left_types[i])
				if assign_stmt.op == .decl_assign {
					g.write('$styp ')
				}
				g.expr(lx)
				if is_opt {
					mr_base_styp := g.base_type(return_type)
					g.writeln(' = (*($mr_base_styp*)${mr_var_name}.data).arg$i;')
				} else {
					g.writeln(' = ${mr_var_name}.arg$i;')
				}
			}
			return
		}
	}
	// TODO: non idents on left (exprs)
	if assign_stmt.has_cross_var {
		for i, left in assign_stmt.left {
			match left {
				ast.Ident {
					left_typ := assign_stmt.left_types[i]
					left_sym := g.table.get_type_symbol(left_typ)
					if left_sym.kind == .function {
						g.write_fn_ptr_decl(left_sym.info as table.FnType, '_var_$left.pos.pos')
						g.writeln(' = $left.name;')
					} else {
						styp := g.typ(left_typ)
						g.writeln('$styp _var_$left.pos.pos = $left.name;')
					}
				}
				ast.IndexExpr {
					sym := g.table.get_type_symbol(left.left_type)
					if sym.kind == .array {
						info := sym.info as table.Array
						elem_typ := g.table.get_type_symbol(info.elem_type)
						if elem_typ.kind == .function {
							left_typ := assign_stmt.left_types[i]
							left_sym := g.table.get_type_symbol(left_typ)
							g.write_fn_ptr_decl(left_sym.info as table.FnType, '_var_$left.pos.pos')
							g.write(' = *(voidptr*)array_get(')
						} else {
							styp := g.typ(info.elem_type)
							g.write('$styp _var_$left.pos.pos = *($styp*)array_get(')
						}
						if left.left_type.is_ptr() {
							g.write('*')
						}
						needs_clone := info.elem_type == table.string_type && g.is_autofree
						if needs_clone {
							g.write('/*1*/string_clone(')
						}
						g.expr(left.left)
						if needs_clone {
							g.write(')')
						}
						g.write(', ')
						g.expr(left.index)
						g.writeln(');')
					} else if sym.kind == .map {
						info := sym.info as table.Map
						skeytyp := g.typ(info.key_type)
						styp := g.typ(info.value_type)
						zero := g.type_default(info.value_type)
						val_typ := g.table.get_type_symbol(info.value_type)
						if val_typ.kind == .function {
							left_type := assign_stmt.left_types[i]
							left_sym := g.table.get_type_symbol(left_type)
							g.write_fn_ptr_decl(left_sym.info as table.FnType, '_var_$left.pos.pos')
							g.write(' = *(voidptr*)map_get_1(')
						} else {
							g.write('$styp _var_$left.pos.pos = *($styp*)map_get_1(')
						}
						if !left.left_type.is_ptr() {
							g.write('ADDR(map, ')
							g.expr(left.left)
							g.write(')')
						} else {
							g.expr(left.left)
						}
						g.write(', &($skeytyp[]){')
						g.expr(left.index)
						g.write('}')
						if val_typ.kind == .function {
							g.writeln(', &(voidptr[]){ $zero });')
						} else {
							g.writeln(', &($styp[]){ $zero });')
						}
					}
				}
				ast.SelectorExpr {
					styp := g.typ(left.typ)
					g.write('$styp _var_$left.pos.pos = ')
					g.expr(left.expr)
					if left.expr_type.is_ptr() {
						g.write('/* left.expr_type */')
						g.writeln('->$left.field_name;')
					} else {
						g.writeln('.$left.field_name;')
					}
				}
				else {}
			}
		}
	}
	// `a := 1` | `a,b := 1,2`
	for i, left in assign_stmt.left {
		mut var_type := assign_stmt.left_types[i]
		mut val_type := assign_stmt.right_types[i]
		val := assign_stmt.right[i]
		mut is_call := false
		mut blank_assign := false
		mut ident := ast.Ident{
			scope: 0
		}
		left_sym := g.table.get_type_symbol(var_type)
		if left is ast.Ident {
			ident = left
			// id_info := ident.var_info()
			// var_type = id_info.typ
			blank_assign = left.kind == .blank_ident
			// TODO: temporary, remove this
			left_info := left.info
			if left_info is ast.IdentVar {
				share := left_info.share
				if share == .shared_t {
					var_type = var_type.set_flag(.shared_f)
				}
				if share == .atomic_t {
					var_type = var_type.set_flag(.atomic_f)
				}
			}
		}
		styp := g.typ(var_type)
		mut is_fixed_array_init := false
		mut has_val := false
		match val {
			ast.ArrayInit {
				is_fixed_array_init = val.is_fixed
				has_val = val.has_val
			}
			ast.CallExpr {
				is_call = true
				return_type = val.return_type
			}
			// TODO: no buffer fiddling
			ast.AnonFn {
				if blank_assign {
					g.write('{')
				}
				// if it's a decl assign (`:=`) or a blank assignment `_ =`/`_ :=` then generate `void (*ident) (args) =`
				if (is_decl || blank_assign) && left is ast.Ident {
					ret_styp := g.typ(val.decl.return_type)
					g.write('$ret_styp (*$ident.name) (')
					def_pos := g.definitions.len
					g.fn_args(val.decl.params, val.decl.is_variadic)
					g.definitions.go_back(g.definitions.len - def_pos)
					g.write(') = ')
				} else {
					g.is_assign_lhs = true
					g.assign_op = assign_stmt.op
					g.expr(left)
					g.is_assign_lhs = false
					g.is_arraymap_set = false
					if left is ast.IndexExpr {
						sym := g.table.get_type_symbol(left.left_type)
						if sym.kind in [.map, .array] {
							g.expr(val)
							g.writeln('});')
							continue
						}
					}
					g.write(' = ')
				}
				g.expr(val)
				g.writeln(';')
				if blank_assign {
					g.write('}')
				}
				continue
			}
			else {}
		}
		right_sym := g.table.get_type_symbol(val_type)
		is_fixed_array_copy := right_sym.kind == .array_fixed && val is ast.Ident
		g.is_assign_lhs = true
		g.assign_op = assign_stmt.op
		if val_type.has_flag(.optional) {
			g.right_is_opt = true
		}
		if blank_assign {
			if is_call {
				old_is_void_expr_stmt := g.is_void_expr_stmt
				g.is_void_expr_stmt = true
				g.expr(val)
				g.is_void_expr_stmt = old_is_void_expr_stmt
			} else {
				g.write('{$styp _ = ')
				g.expr(val)
				g.writeln(';}')
			}
		} else if is_fixed_array_init && assign_stmt.op == .assign {
			right := val as ast.ArrayInit
			v_var := g.new_tmp_var()
			arr_typ := styp.trim('*')
			g.write('$arr_typ $v_var = ')
			g.expr(right)
			g.writeln(';')
			pos := g.out.len
			g.expr(left)

			if g.is_arraymap_set && g.arraymap_set_pos > 0 {
				g.out.go_back_to(g.arraymap_set_pos)
				g.write(', &$v_var)')
				g.is_arraymap_set = false
				g.arraymap_set_pos = 0
			} else {
				g.out.go_back_to(pos)
				is_var_mut := !is_decl && left.is_auto_deref_var()
				addr := if is_var_mut { '' } else { '&' }
				g.writeln('')
				g.write('memcpy($addr')
				g.expr(left)
				g.writeln(', &$v_var, sizeof($arr_typ));')
			}
			g.is_assign_lhs = false
		} else {
			is_inside_ternary := g.inside_ternary != 0
			cur_line := if is_inside_ternary && is_decl {
				g.register_ternary_name(ident.name)
				g.empty_line = false
				g.go_before_ternary()
			} else {
				''
			}
			mut str_add := false
			mut op_overloaded := false
			if var_type == table.string_type_idx && assign_stmt.op == .plus_assign {
				if left is ast.IndexExpr {
					// a[0] += str => `array_set(&a, 0, &(string[]) {string_add(...))})`
					g.expr(left)
					g.write('string_add(')
				} else {
					// str += str2 => `str = string_add(str, str2)`
					g.expr(left)
					g.write(' = /*f*/string_add(')
				}
				g.is_assign_lhs = false
				str_add = true
			}
			// Assignment Operator Overloading
			if ((left_sym.kind == .struct_ && right_sym.kind == .struct_)
				|| (left_sym.kind == .alias && right_sym.kind == .alias))
				&& assign_stmt.op in [.plus_assign, .minus_assign, .div_assign, .mult_assign, .mod_assign] {
				extracted_op := match assign_stmt.op {
					.plus_assign { '+' }
					.minus_assign { '-' }
					.div_assign { '/' }
					.mod_assign { '%' }
					.mult_assign { '*' }
					else { 'unknown op' }
				}
				g.expr(left)
				g.write(' = ${styp}_${util.replace_op(extracted_op)}(')
				op_overloaded = true
			}
			if right_sym.kind == .function && is_decl {
				if is_inside_ternary && is_decl {
					g.out.write_string(util.tabs(g.indent - g.inside_ternary))
				}
				func := right_sym.info as table.FnType
				ret_styp := g.typ(func.func.return_type)
				g.write('$ret_styp (*${g.get_ternary_name(ident.name)}) (')
				def_pos := g.definitions.len
				g.fn_args(func.func.params, func.func.is_variadic)
				g.definitions.go_back(g.definitions.len - def_pos)
				g.write(')')
			} else {
				if is_decl {
					if is_inside_ternary {
						g.out.write_string(util.tabs(g.indent - g.inside_ternary))
					}
					g.write('$styp ')
				}
				if left is ast.Ident || left is ast.SelectorExpr {
					g.prevent_sum_type_unwrapping_once = true
				}
				if !is_fixed_array_copy || is_decl {
					if !is_decl && left.is_auto_deref_var() {
						g.write('*')
					}
					g.expr(left)
				}
			}
			if is_inside_ternary && is_decl {
				g.write(';\n$cur_line')
				g.out.write_string(util.tabs(g.indent))
				g.expr(left)
			}
			g.is_assign_lhs = false
			if is_fixed_array_copy {
				if is_decl {
					g.writeln(';')
				}
			} else if !g.is_arraymap_set && !str_add && !op_overloaded {
				g.write(' $op ')
			} else if str_add || op_overloaded {
				g.write(', ')
			}
			mut cloned := false
			if g.is_autofree && right_sym.kind in [.array, .string] {
				if g.gen_clone_assignment(val, right_sym, false) {
					cloned = true
				}
			}
			unwrap_optional := !var_type.has_flag(.optional) && val_type.has_flag(.optional)
			if unwrap_optional {
				// Unwrap the optional now that the testing code has been prepended.
				// `pos := s.index(...
				// `int pos = *(int)_t10.data;`
				// if g.is_autofree {
				/*
				if is_optional {
					g.write('*($styp*)')
					g.write(tmp_opt + '.data/*FFz*/')
					g.right_is_opt = false
					if g.inside_ternary == 0 && !assign_stmt.is_simple {
						g.writeln(';')
					}
					return
				}
				*/
			}
			g.is_shared = var_type.has_flag(.shared_f)
			if !cloned {
				if is_fixed_array_copy {
					i_var := g.new_tmp_var()
					fixed_array := right_sym.info as table.ArrayFixed
					g.write('for (int $i_var=0; $i_var<$fixed_array.size; $i_var++) {')
					g.expr(left)
					g.write('[$i_var] = ')
					if val.is_auto_deref_var() {
						g.write('*')
					}
					g.expr(val)
					g.write('[$i_var];')
					g.writeln('}')
				} else if is_decl {
					if is_fixed_array_init && !has_val {
						if val is ast.ArrayInit {
							if val.has_default {
								g.write('{')
								g.expr(val.default_expr)
								info := right_sym.info as table.ArrayFixed
								for _ in 1 .. info.size {
									g.write(', ')
									g.expr(val.default_expr)
								}
								g.write('}')
							} else {
								g.write('{0}')
							}
						} else {
							g.write('{0}')
						}
					} else {
						if val.is_auto_deref_var() {
							g.write('*')
						}
						g.expr(val)
					}
				} else {
					if assign_stmt.has_cross_var {
						g.gen_cross_tmp_variable(assign_stmt.left, val)
					} else {
						g.expr_with_cast(val, val_type, var_type)
					}
				}
			}
			if str_add || op_overloaded {
				g.write(')')
			}
			if g.is_arraymap_set {
				g.write(' })')
				g.is_arraymap_set = false
			}
			g.is_shared = false
		}
		g.right_is_opt = false
		if g.inside_ternary == 0 && (assign_stmt.left.len > 1 || !assign_stmt.is_simple) {
			g.writeln(';')
		}
	}
}

fn (mut g Gen) gen_cross_tmp_variable(left []ast.Expr, val ast.Expr) {
	val_ := val
	match val {
		ast.Ident {
			mut has_var := false
			for lx in left {
				if lx is ast.Ident {
					if val.name == lx.name {
						g.write('_var_')
						g.write(lx.pos.pos.str())
						has_var = true
						break
					}
				}
			}
			if !has_var {
				g.expr(val_)
			}
		}
		ast.IndexExpr {
			mut has_var := false
			for lx in left {
				if val_.str() == lx.str() {
					g.write('_var_')
					g.write(lx.position().pos.str())
					has_var = true
					break
				}
			}
			if !has_var {
				g.expr(val_)
			}
		}
		ast.InfixExpr {
			g.gen_cross_tmp_variable(left, val.left)
			g.write(val.op.str())
			g.gen_cross_tmp_variable(left, val.right)
		}
		ast.PrefixExpr {
			g.write(val.op.str())
			g.gen_cross_tmp_variable(left, val.right)
		}
		ast.PostfixExpr {
			g.gen_cross_tmp_variable(left, val.expr)
			g.write(val.op.str())
		}
		ast.SelectorExpr {
			mut has_var := false
			for lx in left {
				if val_.str() == lx.str() {
					g.write('_var_')
					g.write(lx.position().pos.str())
					has_var = true
					break
				}
			}
			if !has_var {
				g.expr(val_)
			}
		}
		else {
			g.expr(val_)
		}
	}
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

fn (mut g Gen) gen_clone_assignment(val ast.Expr, right_sym table.TypeSymbol, add_eq bool) bool {
	if val !is ast.Ident && val !is ast.SelectorExpr {
		return false
	}
	if g.is_autofree && right_sym.kind == .array {
		// `arr1 = arr2` => `arr1 = arr2.clone()`
		if add_eq {
			g.write('=')
		}
		g.write(' array_clone_static(')
		g.expr(val)
		g.write(')')
	} else if g.is_autofree && right_sym.kind == .string {
		if add_eq {
			g.write('=')
		}
		// `str1 = str2` => `str1 = str2.clone()`
		g.write(' string_clone_static(')
		g.expr(val)
		g.write(')')
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
	g.writeln('// autofree_scope_vars(pos=$pos line_nr=$line_nr scope.pos=$scope.start_pos scope.end_pos=$scope.end_pos)')
	g.autofree_scope_vars2(scope, scope.start_pos, scope.end_pos, line_nr, free_parent_scopes,
		stop_pos)
}

// fn (mut g Gen) autofree_scope_vars2(scope &ast.Scope, end_pos int) {
fn (mut g Gen) autofree_scope_vars2(scope &ast.Scope, start_pos int, end_pos int, line_nr int, free_parent_scopes bool, stop_pos int) {
	if isnil(scope) {
		return
	}
	for _, obj in scope.objects {
		match obj {
			ast.Var {
				g.writeln('// var "$obj.name" var.pos=$obj.pos.pos var.line_nr=$obj.pos.line_nr')
				if obj.name == g.returned_var_name {
					g.writeln('// skipping returned var')
					continue
				}
				if obj.is_or {
					// Skip vars inited with the `or {}`, since they are generated
					// after the or block in C.
					g.writeln('// skipping `or{}` var "$obj.name"')
					continue
				}
				if obj.is_tmp {
					// Skip for loop vars
					g.writeln('// skipping tmp var "$obj.name"')
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
				is_optional := obj.typ.has_flag(.optional)
				if is_optional {
					// TODO: free optionals
					continue
				}
				g.autofree_variable(obj)
			}
			else {}
		}
	}
	// Free all vars in parent scopes as well:
	// ```
	// s := ...
	// if ... {
	// s.free()
	// return
	// }
	// ```
	// if !isnil(scope.parent) && line_nr > 0 {
	if free_parent_scopes && !isnil(scope.parent)
		&& (stop_pos == -1 || scope.parent.start_pos >= stop_pos) {
		g.writeln('// af parent scope:')
		g.autofree_scope_vars2(scope.parent, start_pos, end_pos, line_nr, true, stop_pos)
	}
}

fn (mut g Gen) autofree_variable(v ast.Var) {
	sym := g.table.get_type_symbol(v.typ)
	// if v.name.contains('output2') {
	// eprintln('   > var name: ${v.name:-20s} | is_arg: ${v.is_arg.str():6} | var type: ${int(v.typ):8} | type_name: ${sym.name:-33s}')
	// }
	if sym.kind == .array {
		g.autofree_var_call('array_free', v)
		return
	}
	if sym.kind == .string {
		// Don't free simple string literals.
		match v.expr {
			ast.StringLiteral {
				g.writeln('// str literal')
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
	if sym.has_method('free') {
		g.autofree_var_call(c_name(sym.name) + '_free', v)
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
	if v.typ.is_ptr() {
		g.writeln('\t${free_fn_name}(${c_name(v.name)}); // autofreed ptr var')
	} else {
		g.writeln('\t${free_fn_name}(&${c_name(v.name)}); // autofreed var $g.cur_mod.name $g.is_builtin_mod')
	}
}

fn (mut g Gen) gen_anon_fn_decl(mut node ast.AnonFn) {
	if !node.has_gen {
		pos := g.out.len
		g.stmt(node.decl)
		fn_body := g.out.after(pos)
		g.out.go_back(fn_body.len)
		g.anon_fn_definitions << fn_body
		node.has_gen = true
	}
}

fn (mut g Gen) map_fn_ptrs(key_typ table.TypeSymbol) (string, string, string, string) {
	mut hash_fn := ''
	mut key_eq_fn := ''
	mut clone_fn := ''
	mut free_fn := '&map_free_nop'
	match key_typ.kind {
		.byte, .i8, .char {
			hash_fn = '&map_hash_int_1'
			key_eq_fn = '&map_eq_int_1'
			clone_fn = '&map_clone_int_1'
		}
		.i16, .u16 {
			hash_fn = '&map_hash_int_2'
			key_eq_fn = '&map_eq_int_2'
			clone_fn = '&map_clone_int_2'
		}
		.int, .u32, .rune, .f32, .enum_ {
			hash_fn = '&map_hash_int_4'
			key_eq_fn = '&map_eq_int_4'
			clone_fn = '&map_clone_int_4'
		}
		.voidptr {
			ts := if g.pref.m64 {
				&g.table.types[table.u64_type_idx]
			} else {
				&g.table.types[table.u32_type_idx]
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
			verror('map key type not supported')
		}
	}
	return hash_fn, key_eq_fn, clone_fn, free_fn
}

fn (mut g Gen) expr(node ast.Expr) {
	// println('cgen expr() line_nr=$node.pos.line_nr')
	old_discard_or_result := g.discard_or_result
	old_is_void_expr_stmt := g.is_void_expr_stmt
	if g.is_void_expr_stmt {
		g.discard_or_result = true
		g.is_void_expr_stmt = false
	} else {
		g.discard_or_result = false
	}
	// NB: please keep the type names in the match here in alphabetical order:
	match mut node {
		ast.AnonFn {
			// TODO: dont fiddle with buffers
			g.gen_anon_fn_decl(mut node)
			fsym := g.table.get_type_symbol(node.typ)
			g.write(fsym.name)
		}
		ast.ArrayDecompose {
			g.expr(node.expr)
		}
		ast.ArrayInit {
			g.array_init(node)
		}
		ast.AsCast {
			g.as_cast(node)
		}
		ast.Assoc {
			g.assoc(node)
		}
		ast.BoolLiteral {
			g.write(node.val.str())
		}
		ast.CallExpr {
			// if g.fileis('1.strings') {
			// println('\ncall_expr()()')
			// }
			ret_type := if node.or_block.kind == .absent {
				node.return_type
			} else {
				node.return_type.clear_flag(.optional)
			}
			mut shared_styp := ''
			if g.is_shared && !ret_type.has_flag(.shared_f) {
				ret_sym := g.table.get_type_symbol(ret_type)
				shared_typ := ret_type.set_flag(.shared_f)
				shared_styp = g.typ(shared_typ)
				if ret_sym.kind == .array {
					g.writeln('($shared_styp*)__dup_shared_array(&($shared_styp){.val = ')
				} else if ret_sym.kind == .map {
					g.writeln('($shared_styp*)__dup_shared_map(&($shared_styp){.val = ')
				} else {
					g.writeln('($shared_styp*)__dup${shared_styp}(&($shared_styp){.val = ')
				}
			}
			g.call_expr(node)
			// if g.fileis('1.strings') {
			// println('before:' + node.autofree_pregen)
			// }
			if g.is_autofree && !g.is_builtin_mod && !g.is_js_call && g.strs_to_free0.len == 0
				&& !g.inside_lambda { // && g.inside_ternary ==
				// if len != 0, that means we are handling call expr inside call expr (arg)
				// and it'll get messed up here, since it's handled recursively in autofree_call_pregen()
				// so just skip it
				g.autofree_call_pregen(node)
				if g.strs_to_free0.len > 0 {
					g.insert_before_stmt(g.strs_to_free0.join('\n') + '/* inserted before */')
				}
				g.strs_to_free0 = []
				// println('pos=$node.pos.pos')
			}
			if g.is_shared && !ret_type.has_flag(.shared_f) {
				g.writeln('}, sizeof($shared_styp))')
			}
			// if g.autofree && node.autofree_pregen != '' { // g.strs_to_free0.len != 0 {
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
			// g.write('/*cast*/')
			if g.is_amp {
				// &Foo(0) => ((Foo*)0)
				g.out.go_back(1)
			}
			g.is_amp = false
			sym := g.table.get_type_symbol(node.typ)
			if sym.kind == .string && !node.typ.is_ptr() {
				// `string(x)` needs `tos()`, but not `&string(x)
				// `tos(str, len)`, `tos2(str)`
				if node.has_arg {
					g.write('tos((byteptr)')
				} else {
					g.write('tos2((byteptr)')
				}
				g.expr(node.expr)
				expr_sym := g.table.get_type_symbol(node.expr_type)
				if expr_sym.kind == .array {
					// if we are casting an array, we need to add `.data`
					g.write('.data')
				}
				if node.has_arg {
					// len argument
					g.write(', ')
					g.expr(node.arg)
				}
				g.write(')')
			} else if sym.kind in [.sum_type, .interface_] {
				g.expr_with_cast(node.expr, node.expr_type, node.typ)
			} else if sym.kind == .struct_ && !node.typ.is_ptr()
				&& !(sym.info as table.Struct).is_typedef {
				// deprecated, replaced by Struct{...exr}
				styp := g.typ(node.typ)
				g.write('*(($styp *)(&')
				g.expr(node.expr)
				g.write('))')
			} else {
				styp := g.typ(node.typ)
				mut cast_label := ''
				// `table.string_type` is done for MSVC's bug
				if sym.kind != .alias
					|| (sym.info as table.Alias).parent_type !in [node.expr_type, table.string_type] {
					cast_label = '($styp)'
				}
				g.write('(${cast_label}(')
				g.expr(node.expr)
				if node.expr is ast.IntegerLiteral {
					if node.typ in [table.u64_type, table.u32_type, table.u16_type] {
						if !node.expr.val.starts_with('-') {
							g.write('U')
						}
					}
				}
				g.write('))')
			}
		}
		ast.ChanInit {
			elem_typ_str := g.typ(node.elem_type)
			g.write('sync__new_channel_st(')
			if node.has_cap {
				g.expr(node.cap_expr)
			} else {
				g.write('0')
			}
			g.write(', sizeof(')
			g.write(elem_typ_str)
			g.write('))')
		}
		ast.CharLiteral {
			if node.val == r'\`' {
				g.write("'`'")
			} else {
				g.write("L'$node.val'")
			}
		}
		ast.DumpExpr {
			g.dump_expr(node)
		}
		ast.AtExpr {
			g.comp_at(node)
		}
		ast.ComptimeCall {
			g.comptime_call(node)
		}
		ast.ComptimeSelector {
			g.comptime_selector(node)
		}
		ast.Comment {}
		ast.ConcatExpr {
			g.concat_expr(node)
		}
		ast.CTempVar {
			// g.write('/*ctmp .orig: $node.orig.str() , ._typ: $node.typ, .is_ptr: $node.is_ptr */ ')
			g.write(node.name)
		}
		ast.EnumVal {
			// g.write('${it.mod}${it.enum_name}_$it.val')
			styp := g.typ(node.typ)
			g.write('${styp}_$node.val')
		}
		ast.FloatLiteral {
			g.write(node.val)
		}
		ast.GoExpr {
			g.go_expr(node)
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
				g.write(node.val) // .int().str())
			}
		}
		ast.LockExpr {
			g.lock_expr(node)
		}
		ast.MatchExpr {
			g.match_expr(node)
		}
		ast.MapInit {
			g.map_init(node)
		}
		ast.None {
			g.write('_const_none__')
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
				g.writeln('sync__RwMutex_lock(&$node.auto_locked->mtx);')
			}
			g.inside_map_postfix = true
			if node.expr.is_auto_deref_var() {
				g.write('(*')
				g.expr(node.expr)
				g.write(')')
			} else {
				g.expr(node.expr)
			}
			g.inside_map_postfix = false
			g.write(node.op.str())
			if node.auto_locked != '' {
				g.writeln(';')
				g.write('sync__RwMutex_unlock(&$node.auto_locked->mtx)')
			}
		}
		ast.PrefixExpr {
			gen_or := node.op == .arrow && (node.or_block.kind != .absent || node.is_option)
			if node.op == .amp {
				g.is_amp = true
			}
			if node.op == .arrow {
				styp := g.typ(node.right_type)
				right_sym := g.table.get_type_symbol(node.right_type)
				mut right_inf := right_sym.info as table.Chan
				elem_type := right_inf.elem_type
				is_gen_or_and_assign_rhs := gen_or && !g.discard_or_result
				cur_line := if is_gen_or_and_assign_rhs {
					line := g.go_before_stmt(0)
					g.out.write_string(util.tabs(g.indent))
					line
				} else {
					''
				}
				tmp_opt := if gen_or { g.new_tmp_var() } else { '' }
				if gen_or {
					opt_elem_type := g.typ(elem_type.set_flag(.optional))
					g.register_chan_pop_optional_call(opt_elem_type, styp)
					g.write('$opt_elem_type $tmp_opt = __Option_${styp}_popval(')
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
						elem_styp := g.typ(elem_type)
						g.write(';\n$cur_line*($elem_styp*)${tmp_opt}.data')
					}
				}
			} else {
				// g.write('/*pref*/')
				g.write(node.op.str())
				// g.write('(')
				g.expr(node.right)
			}
			g.is_amp = false
		}
		ast.RangeExpr {
			// Only used in IndexExpr
		}
		ast.SelectExpr {
			g.select_expr(node)
		}
		ast.SizeOf {
			node_typ := g.unwrap_generic(node.typ)
			sym := g.table.get_type_symbol(node_typ)
			if sym.language == .v && sym.kind in [.placeholder, .any] {
				g.error('unknown type `$sym.name`', node.pos)
			}
			styp := g.typ(node_typ)
			g.write('/*SizeOf*/ sizeof(${util.no_dots(styp)})')
		}
		ast.OffsetOf {
			styp := g.typ(node.struct_type)
			g.write('/*OffsetOf*/ (u32)(__offsetof(${util.no_dots(styp)}, $node.field))')
		}
		ast.SqlExpr {
			g.sql_select_expr(node, false, '')
		}
		ast.StringLiteral {
			g.string_literal(node)
		}
		ast.StringInterLiteral {
			g.string_inter_literal(node)
		}
		ast.StructInit {
			if node.unresolved {
				g.expr(ast.resolve_init(node, g.unwrap_generic(node.typ), g.table))
			} else {
				// `user := User{name: 'Bob'}`
				g.struct_init(node)
			}
		}
		ast.SelectorExpr {
			g.selector_expr(node)
		}
		ast.Type {
			// match sum Type
			// g.write('/* Type */')
			// type_idx := node.typ.idx()
			sym := g.table.get_type_symbol(node.typ)
			sidx := g.type_sidx(node.typ)
			// g.write('$type_idx /* $sym.name */')
			g.write('$sidx /* $sym.name */')
		}
		ast.TypeOf {
			g.typeof_expr(node)
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
		ast.UnsafeExpr {
			g.expr(node.expr)
		}
	}
	g.discard_or_result = old_discard_or_result
	g.is_void_expr_stmt = old_is_void_expr_stmt
}

// T.name, typeof(expr).name
fn (mut g Gen) type_name(type_ table.Type) {
	mut typ := type_
	if typ.has_flag(.generic) {
		typ = g.cur_generic_types[0]
	}
	s := g.table.type_to_str(typ)
	g.write('_SLIT("${util.strip_main_name(s)}")')
}

fn (mut g Gen) typeof_expr(node ast.TypeOf) {
	sym := g.table.get_type_symbol(node.expr_type)
	if sym.kind == .sum_type {
		// When encountering a .sum_type, typeof() should be done at runtime,
		// because the subtype of the expression may change:
		g.write('tos3( /* $sym.name */ v_typeof_sumtype_${sym.cname}( (')
		g.expr(node.expr)
		g.write(')._typ ))')
	} else if sym.kind == .array_fixed {
		fixed_info := sym.info as table.ArrayFixed
		typ_name := g.table.get_type_name(fixed_info.elem_type)
		g.write('_SLIT("[$fixed_info.size]${util.strip_main_name(typ_name)}")')
	} else if sym.kind == .function {
		info := sym.info as table.FnType
		g.write('_SLIT("${g.fn_decl_str(info)}")')
	} else if node.expr_type.has_flag(.variadic) {
		varg_elem_type_sym := g.table.get_type_symbol(g.table.value_type(node.expr_type))
		g.write('_SLIT("...${util.strip_main_name(varg_elem_type_sym.name)}")')
	} else {
		g.write('_SLIT("${util.strip_main_name(sym.name)}")')
	}
}

fn (mut g Gen) selector_expr(node ast.SelectorExpr) {
	prevent_sum_type_unwrapping_once := g.prevent_sum_type_unwrapping_once
	g.prevent_sum_type_unwrapping_once = false
	if node.name_type > 0 {
		g.type_name(node.name_type)
		return
	}
	if node.expr_type == 0 {
		g.checker_bug('unexpected SelectorExpr.expr_type = 0', node.pos)
	}
	sym := g.table.get_type_symbol(g.unwrap_generic(node.expr_type))
	// if node expr is a root ident and an optional
	mut is_optional := node.expr is ast.Ident && node.expr_type.has_flag(.optional)
	if is_optional {
		opt_base_typ := g.base_type(node.expr_type)
		g.writeln('(*($opt_base_typ*)')
	}
	if sym.kind in [.interface_, .sum_type] {
		g.write('(*(')
	}
	if sym.kind == .array_fixed {
		if node.field_name != 'len' {
			g.error('field_name should be `len`', node.pos)
		}
		info := sym.info as table.ArrayFixed
		g.write('$info.size')
		return
	}
	if sym.kind == .chan && (node.field_name == 'len' || node.field_name == 'closed') {
		g.write('sync__Channel_${node.field_name}(')
		g.expr(node.expr)
		g.write(')')
		return
	}
	mut sum_type_deref_field := ''
	mut sum_type_dot := '.'
	if f := g.table.find_field(sym, node.field_name) {
		field_sym := g.table.get_type_symbol(f.typ)
		if field_sym.kind == .sum_type {
			if !prevent_sum_type_unwrapping_once {
				// check first if field is sum type because scope searching is expensive
				scope := g.file.scope.innermost(node.pos.pos)
				if field := scope.find_struct_field(node.expr_type, node.field_name) {
					if field.orig_type.is_ptr() {
						sum_type_dot = '->'
					}
					// union sum type deref
					for i, typ in field.sum_type_casts {
						g.write('(*')
						cast_sym := g.table.get_type_symbol(typ)
						if i != 0 {
							dot := if field.typ.is_ptr() { '->' } else { '.' }
							sum_type_deref_field += ')$dot'
						}
						if mut cast_sym.info is table.Aggregate {
							agg_sym := g.table.get_type_symbol(cast_sym.info.types[g.aggregate_type_idx])
							sum_type_deref_field += '_$agg_sym.cname'
						} else {
							sum_type_deref_field += '_$cast_sym.cname'
						}
					}
				}
			}
		}
	}
	g.expr(node.expr)
	if is_optional {
		g.write('.data)')
	}
	// struct embedding
	if sym.info is table.Struct {
		if node.from_embed_type != 0 {
			embed_sym := g.table.get_type_symbol(node.from_embed_type)
			embed_name := embed_sym.embed_name()
			if node.expr_type.is_ptr() {
				g.write('->')
			} else {
				g.write('.')
			}
			g.write(embed_name)
		}
	}
	if (node.expr_type.is_ptr() || sym.kind == .chan) && node.from_embed_type == 0 {
		g.write('->')
	} else {
		// g.write('. /*typ=  $it.expr_type */') // ${g.typ(it.expr_type)} /')
		g.write('.')
	}
	if node.expr_type.has_flag(.shared_f) {
		g.write('val.')
	}
	if node.expr_type == 0 {
		verror('cgen: SelectorExpr | expr_type: 0 | it.expr: `$node.expr` | field: `$node.field_name` | file: $g.file.path | line: $node.pos.line_nr')
	}
	g.write(c_name(node.field_name))
	if sum_type_deref_field != '' {
		g.write('$sum_type_dot$sum_type_deref_field)')
	}
	if sym.kind in [.interface_, .sum_type] {
		g.write('))')
	}
}

fn (mut g Gen) enum_expr(node ast.Expr) {
	match node {
		ast.EnumVal { g.write(node.val) }
		else { g.expr(node) }
	}
}

fn (mut g Gen) infix_expr(node ast.InfixExpr) {
	if node.auto_locked != '' {
		g.writeln('sync__RwMutex_lock(&$node.auto_locked->mtx);')
	}
	if node.op in [.key_is, .not_is] {
		g.is_expr(node)
		return
	}
	left_type := g.unwrap_generic(node.left_type)
	left_sym := g.table.get_type_symbol(left_type)
	left_final_sym := g.table.get_final_type_symbol(left_type)
	unaliased_left := if left_sym.kind == .alias {
		(left_sym.info as table.Alias).parent_type
	} else {
		left_type
	}
	op_is_key_in_or_not_in := node.op in [.key_in, .not_in]
	op_is_eq_or_ne := node.op in [.eq, .ne]
	right_sym := g.table.get_type_symbol(node.right_type)
	right_final_sym := g.table.get_final_type_symbol(node.right_type)
	has_eq_overloaded := !left_sym.has_method('==')
	unaliased_right := if right_sym.info is table.Alias {
		right_sym.info.parent_type
	} else {
		node.right_type
	}
	if unaliased_left == table.ustring_type_idx && !op_is_key_in_or_not_in {
		fn_name := match node.op {
			.plus {
				'ustring_add('
			}
			.eq {
				'ustring_eq('
			}
			.ne {
				'ustring_ne('
			}
			.lt {
				'ustring_lt('
			}
			.le {
				'ustring_le('
			}
			.gt {
				'ustring_gt('
			}
			.ge {
				'ustring_ge('
			}
			else {
				verror('op error for type `$left_sym.name`')
				'/*node error*/'
			}
		}
		g.write(fn_name)
		g.expr(node.left)
		g.write(', ')
		g.expr(node.right)
		g.write(')')
	} else if unaliased_left == table.string_type_idx && !op_is_key_in_or_not_in {
		// `str == ''` -> `str.len == 0` optimization
		if node.op in [.eq, .ne] && node.right is ast.StringLiteral
			&& (node.right as ast.StringLiteral).val == '' {
			arrow := if left_type.is_ptr() { '->' } else { '.' }
			g.write('(')
			g.expr(node.left)
			g.write(')')
			g.write('${arrow}len $node.op 0')
		} else {
			fn_name := match node.op {
				.plus {
					'string_add('
				}
				.eq {
					'string_eq('
				}
				.ne {
					'string_ne('
				}
				.lt {
					'string_lt('
				}
				.le {
					'string_le('
				}
				.gt {
					'string_gt('
				}
				.ge {
					'string_ge('
				}
				else {
					verror('op error for type `$left_sym.name`')
					'/*node error*/'
				}
			}
			g.write(fn_name)
			g.expr(node.left)
			g.write(', ')
			g.expr(node.right)
			g.write(')')
		}
	} else if op_is_eq_or_ne && left_sym.kind == .array && right_sym.kind == .array {
		ptr_typ := g.gen_array_equality_fn(left_type.clear_flag(.shared_f))
		if node.op == .ne {
			g.write('!')
		}
		g.write('${ptr_typ}_arr_eq(')
		if node.left_type.is_ptr() && !node.left_type.has_flag(.shared_f) {
			g.write('*')
		}
		g.expr(node.left)
		if node.left_type.has_flag(.shared_f) {
			if node.left_type.is_ptr() {
				g.write('->val')
			} else {
				g.write('.val')
			}
		}
		g.write(', ')
		if node.right_type.is_ptr() && !node.right_type.has_flag(.shared_f) {
			g.write('*')
		}
		g.expr(node.right)
		if node.right_type.has_flag(.shared_f) {
			if node.right_type.is_ptr() {
				g.write('->val')
			} else {
				g.write('.val')
			}
		}
		g.write(')')
	} else if op_is_eq_or_ne && left_sym.kind == .array_fixed && right_sym.kind == .array_fixed {
		ptr_typ := g.gen_fixed_array_equality_fn(left_type)
		if node.op == .ne {
			g.write('!')
		}
		g.write('${ptr_typ}_arr_eq(')
		if node.left_type.is_ptr() {
			g.write('*')
		}
		if node.left is ast.ArrayInit {
			s := g.typ(left_type)
			g.write('($s)')
		}
		g.expr(node.left)
		g.write(', ')
		if node.right is ast.ArrayInit {
			s := g.typ(left_type)
			g.write('($s)')
		}
		g.expr(node.right)
		g.write(')')
	} else if op_is_eq_or_ne && left_sym.kind == .alias && right_sym.kind == .alias {
		ptr_typ := g.gen_alias_equality_fn(left_type)
		if node.op == .eq {
			g.write('${ptr_typ}_alias_eq(')
		} else if node.op == .ne {
			g.write('!${ptr_typ}_alias_eq(')
		}
		if node.left_type.is_ptr() {
			g.write('*')
		}
		g.expr(node.left)
		g.write(', ')
		if node.right_type.is_ptr() {
			g.write('*')
		}
		g.expr(node.right)
		g.write(')')
	} else if op_is_eq_or_ne && left_sym.kind == .map && right_sym.kind == .map {
		ptr_typ := g.gen_map_equality_fn(left_type)
		if node.op == .eq {
			g.write('${ptr_typ}_map_eq(')
		} else if node.op == .ne {
			g.write('!${ptr_typ}_map_eq(')
		}
		if node.left_type.is_ptr() {
			g.write('*')
		}
		g.expr(node.left)
		g.write(', ')
		if node.right_type.is_ptr() {
			g.write('*')
		}
		g.expr(node.right)
		g.write(')')
	} else if op_is_eq_or_ne && left_sym.kind == .struct_ && right_sym.kind == .struct_ {
		if !has_eq_overloaded {
			// Define `==` as negation of Autogenerated `!=`
			styp := g.typ(left_type)
			if node.op == .ne {
				g.write('!${styp}__eq(')
			} else if node.op == .eq {
				g.write('${styp}__eq(')
			}
		}
		if has_eq_overloaded {
			// Auto generate both `==` and `!=`
			ptr_typ := g.gen_struct_equality_fn(left_type)
			if node.op == .eq {
				g.write('${ptr_typ}_struct_eq(')
			} else if node.op == .ne {
				g.write('!${ptr_typ}_struct_eq(')
			}
		}
		if node.left_type.is_ptr() {
			g.write('*')
		}
		g.expr(node.left)
		g.write(', ')
		if node.right_type.is_ptr() {
			g.write('*')
		}
		g.expr(node.right)
		g.write(')')
	} else if op_is_key_in_or_not_in {
		if node.op == .not_in {
			g.write('!')
		}
		if right_sym.kind == .array {
			if mut node.right is ast.ArrayInit {
				if node.right.exprs.len > 0 {
					// `a in [1,2,3]` optimization => `a == 1 || a == 2 || a == 3`
					// avoids an allocation
					// g.write('/*in opt*/')
					g.write('(')
					g.in_optimization(node.left, node.right)
					g.write(')')
					return
				}
			}
			fn_name := g.gen_array_contains_method(node.right_type)
			g.write('(${fn_name}(')
			if node.right_type.is_ptr() {
				g.write('*')
			}
			g.expr(node.right)
			g.write(', ')
			g.expr(node.left)
			g.write('))')
			return
		} else if right_sym.kind == .map {
			g.write('_IN_MAP(')
			if !node.left_type.is_ptr() {
				left_type_str := g.table.type_to_str(node.left_type)
				g.write('ADDR($left_type_str, ')
				g.expr(node.left)
				g.write(')')
			} else {
				g.expr(node.left)
			}
			g.write(', ')
			if !node.right_type.is_ptr() {
				g.write('ADDR(map, ')
				g.expr(node.right)
				g.write(')')
			} else {
				g.expr(node.right)
			}
			g.write(')')
		} else if right_sym.kind == .string {
			g.write('string_contains(')
			g.expr(node.right)
			g.write(', ')
			g.expr(node.left)
			g.write(')')
		}
	} else if node.op == .left_shift && left_final_sym.kind == .array {
		// arr << val
		tmp := g.new_tmp_var()
		info := left_final_sym.info as table.Array
		if right_final_sym.kind == .array && info.elem_type != node.right_type {
			// push an array => PUSH_MANY, but not if pushing an array to 2d array (`[][]int << []int`)
			g.write('_PUSH_MANY(')
			mut expected_push_many_atype := left_type
			if !expected_push_many_atype.is_ptr() {
				// fn f(mut a []int) { a << [1,2,3] } -> type of `a` is `array_int*` -> no need for &
				g.write('&')
			} else {
				expected_push_many_atype = expected_push_many_atype.deref()
			}
			g.expr(node.left)
			g.write(', (')
			g.expr_with_cast(node.right, node.right_type, left_type)
			styp := g.typ(expected_push_many_atype)
			g.write('), $tmp, $styp)')
		} else {
			// push a single element
			elem_type_str := g.typ(info.elem_type)
			elem_sym := g.table.get_type_symbol(info.elem_type)
			g.write('array_push(')
			if !left_type.is_ptr() {
				g.write('&')
			}
			g.expr(node.left)
			if elem_sym.kind == .function {
				g.write(', _MOV((voidptr[]){ ')
			} else {
				g.write(', _MOV(($elem_type_str[]){ ')
			}
			// if g.autofree
			needs_clone := info.elem_type == table.string_type && !g.is_builtin_mod
			if needs_clone {
				g.write('string_clone(')
			}
			g.expr_with_cast(node.right, node.right_type, info.elem_type)
			if needs_clone {
				g.write(')')
			}
			g.write(' }))')
		}
	} else if node.op == .arrow {
		// chan <- val
		gen_or := node.or_block.kind != .absent
		styp := left_sym.cname
		mut left_inf := left_sym.info as table.Chan
		elem_type := left_inf.elem_type
		tmp_opt := if gen_or { g.new_tmp_var() } else { '' }
		if gen_or {
			elem_styp := g.typ(elem_type)
			g.register_chan_push_optional_call(elem_styp, styp)
			g.write('Option_void $tmp_opt = __Option_${styp}_pushval(')
		} else {
			g.write('__${styp}_pushval(')
		}
		g.expr(node.left)
		g.write(', ')
		g.expr(node.right)
		g.write(')')
		if gen_or {
			g.or_block(tmp_opt, node.or_block, table.void_type)
		}
	} else if unaliased_left.idx() in [table.u32_type_idx, table.u64_type_idx]
		&& unaliased_right.is_signed() && node.op in [.eq, .ne, .gt, .lt, .ge, .le] {
		bitsize := if unaliased_left.idx() == table.u32_type_idx
			&& unaliased_right.idx() != table.i64_type_idx {
			32
		} else {
			64
		}
		g.write('_us${bitsize}_${c.cmp_str[int(node.op) - int(token.Kind.eq)]}(')
		g.expr(node.left)
		g.write(',')
		g.expr(node.right)
		g.write(')')
	} else if unaliased_right.idx() in [table.u32_type_idx, table.u64_type_idx]
		&& unaliased_left.is_signed() && node.op in [.eq, .ne, .gt, .lt, .ge, .le] {
		bitsize := if unaliased_right.idx() == table.u32_type_idx
			&& unaliased_left.idx() != table.i64_type_idx {
			32
		} else {
			64
		}
		g.write('_us${bitsize}_${c.cmp_rev[int(node.op) - int(token.Kind.eq)]}(')
		g.expr(node.right)
		g.write(',')
		g.expr(node.left)
		g.write(')')
	} else {
		a := (left_sym.name[0].is_capital() || left_sym.name.contains('.'))
			&& left_sym.kind !in [.enum_, .function, .interface_, .sum_type]
			&& left_sym.language != .c
		b := left_sym.kind != .alias
		c := left_sym.kind == .alias && (left_sym.info as table.Alias).language == .c
		// Check if aliased type is a struct
		d := !b
			&& g.typ((left_sym.info as table.Alias).parent_type).split('__').last()[0].is_capital()
		// Do not generate operator overloading with these `right_sym.kind`.
		e := right_sym.kind !in [.voidptr, .int_literal, .int]
		if node.op in [.plus, .minus, .mul, .div, .mod, .lt, .eq] && ((a && b && e) || c || d) {
			// Overloaded operators
			the_left_type := if !d { left_type } else { (left_sym.info as table.Alias).parent_type }
			g.write(g.typ(the_left_type))
			g.write('_')
			g.write(util.replace_op(node.op.str()))
			g.write('(')
			g.expr(node.left)
			g.write(', ')
			g.expr(node.right)
			g.write(')')
		} else if node.op in [.ne, .gt, .ge, .le] && ((a && b && e) || c || d) {
			the_left_type := if !d { left_type } else { (left_sym.info as table.Alias).parent_type }
			typ := g.typ(the_left_type)
			if node.op == .gt {
				g.write('$typ')
			} else {
				g.write('!$typ')
			}
			g.write('_')
			if node.op == .ne {
				g.write('_eq')
			} else if node.op in [.ge, .le, .gt] {
				g.write('_lt')
			}
			if node.op in [.le, .gt] {
				g.write('(')
				g.expr(node.right)
				g.write(', ')
				g.expr(node.left)
				g.write(')')
			} else {
				g.write('(')
				g.expr(node.left)
				g.write(', ')
				g.expr(node.right)
				g.write(')')
			}
		} else {
			need_par := node.op in [.amp, .pipe, .xor] // `x & y == 0` => `(x & y) == 0` in C
			if need_par {
				g.write('(')
			}
			if node.left_type.is_ptr() && node.left.is_auto_deref_var() {
				g.write('*')
			}
			g.expr(node.left)
			g.write(' $node.op.str() ')
			if node.right_type.is_ptr() && node.right.is_auto_deref_var() {
				g.write('*')
			}
			g.expr(node.right)
			if need_par {
				g.write(')')
			}
		}
	}
	if node.auto_locked != '' {
		g.writeln(';')
		g.write('sync__RwMutex_unlock(&$node.auto_locked->mtx)')
	}
}

fn (mut g Gen) lock_expr(node ast.LockExpr) {
	tmp_result := if node.is_expr { g.new_tmp_var() } else { '' }
	mut cur_line := ''
	if node.is_expr {
		styp := g.typ(node.typ)
		cur_line = g.go_before_stmt(0)
		g.writeln('$styp $tmp_result;')
	}
	mut mtxs := ''
	if node.lockeds.len == 0 {
		// this should not happen
	} else if node.lockeds.len == 1 {
		id := node.lockeds[0]
		name := id.name
		deref := if id.is_mut { '->' } else { '.' }
		lock_prefix := if node.is_rlock[0] { 'r' } else { '' }
		g.writeln('sync__RwMutex_${lock_prefix}lock(&$name${deref}mtx);')
	} else {
		mtxs = g.new_tmp_var()
		g.writeln('uintptr_t _arr_$mtxs[$node.lockeds.len];')
		g.writeln('bool _isrlck_$mtxs[$node.lockeds.len];')
		mut j := 0
		for i, id in node.lockeds {
			if !node.is_rlock[i] {
				name := id.name
				deref := if id.is_mut { '->' } else { '.' }
				g.writeln('_arr_$mtxs[$j] = &$name${deref}mtx;')
				g.writeln('_isrlck_$mtxs[$j] = false;')
				j++
			}
		}
		for i, id in node.lockeds {
			if node.is_rlock[i] {
				name := id.name
				deref := if id.is_mut { '->' } else { '.' }
				g.writeln('_arr_$mtxs[$j] = &$name${deref}mtx;')
				g.writeln('_isrlck_$mtxs[$j] = true;')
				j++
			}
		}
		g.writeln('__sort_ptr(_arr_$mtxs, _isrlck_$mtxs, $node.lockeds.len);')
		g.writeln('for (int $mtxs=0; $mtxs<$node.lockeds.len; $mtxs++) {')
		g.writeln('\tif ($mtxs && _arr_$mtxs[$mtxs] == _arr_$mtxs[$mtxs-1]) continue;')
		g.writeln('\tif (_isrlck_$mtxs[$mtxs])')
		g.writeln('\t\tsync__RwMutex_rlock((sync__RwMutex*)_arr_$mtxs[$mtxs]);')
		g.writeln('\telse')
		g.writeln('\t\tsync__RwMutex_lock((sync__RwMutex*)_arr_$mtxs[$mtxs]);')
		g.writeln('}')
	}
	g.writeln('/*lock*/ {')
	g.stmts_with_tmp_var(node.stmts, tmp_result)
	g.writeln('}')
	if node.lockeds.len == 0 {
		// this should not happen
	} else if node.lockeds.len == 1 {
		id := node.lockeds[0]
		name := id.name
		deref := if id.is_mut { '->' } else { '.' }
		lock_prefix := if node.is_rlock[0] { 'r' } else { '' }
		g.writeln('sync__RwMutex_${lock_prefix}unlock(&$name${deref}mtx);')
	} else {
		// unlock in reverse order
		g.writeln('for (int $mtxs=${node.lockeds.len - 1}; $mtxs>=0; $mtxs--) {')
		g.writeln('\tif ($mtxs && _arr_$mtxs[$mtxs] == _arr_$mtxs[$mtxs-1]) continue;')
		g.writeln('\tif (_isrlck_$mtxs[$mtxs])')
		g.writeln('\t\tsync__RwMutex_runlock((sync__RwMutex*)_arr_$mtxs[$mtxs]);')
		g.writeln('\telse')
		g.writeln('\t\tsync__RwMutex_unlock((sync__RwMutex*)_arr_$mtxs[$mtxs]);')
		g.write('}')
	}
	if node.is_expr {
		g.writeln('')
		g.write(cur_line)
		g.write('$tmp_result')
	}
}

fn (mut g Gen) need_tmp_var_in_match(node ast.MatchExpr) bool {
	if node.is_expr && node.return_type != table.void_type && node.return_type != 0 {
		sym := g.table.get_type_symbol(node.return_type)
		if sym.kind == .multi_return {
			return false
		}
		for branch in node.branches {
			if branch.stmts.len > 1 {
				return true
			}
			if branch.stmts.len == 1 {
				if branch.stmts[0] is ast.ExprStmt {
					stmt := branch.stmts[0] as ast.ExprStmt
					if stmt.expr is ast.CallExpr || stmt.expr is ast.IfExpr
						|| stmt.expr is ast.MatchExpr {
						return true
					}
				}
			}
		}
	}
	return false
}

fn (mut g Gen) match_expr(node ast.MatchExpr) {
	// println('match expr typ=$it.expr_type')
	// TODO
	if node.cond_type == 0 {
		g.writeln('// match 0')
		return
	}
	need_tmp_var := g.need_tmp_var_in_match(node)
	is_expr := (node.is_expr && node.return_type != table.void_type) || g.inside_ternary > 0
	mut cond_var := ''
	mut tmp_var := ''
	mut cur_line := ''
	if is_expr && !need_tmp_var {
		g.inside_ternary++
	}
	if node.cond is ast.Ident || node.cond is ast.SelectorExpr || node.cond is ast.IntegerLiteral
		|| node.cond is ast.StringLiteral || node.cond is ast.FloatLiteral {
		cond_var = g.expr_string(node.cond)
	} else {
		line := if is_expr {
			g.empty_line = true
			g.go_before_stmt(0)
		} else {
			''
		}
		cond_var = g.new_tmp_var()
		g.write('${g.typ(node.cond_type)} $cond_var = ')
		g.expr(node.cond)
		g.writeln(';')
		g.stmt_path_pos << g.out.len
		g.write(line)
	}
	if need_tmp_var {
		g.empty_line = true
		cur_line = g.go_before_stmt(0)
		tmp_var = g.new_tmp_var()
		g.writeln('${g.typ(node.return_type)} $tmp_var;')
	}

	if is_expr && !need_tmp_var {
		// brackets needed otherwise '?' will apply to everything on the left
		g.write('(')
	}
	if node.is_sum_type {
		g.match_expr_sumtype(node, is_expr, cond_var, tmp_var)
	} else {
		g.match_expr_classic(node, is_expr, cond_var, tmp_var)
	}
	g.write(cur_line)
	if need_tmp_var {
		g.write('$tmp_var')
	}
	if is_expr && !need_tmp_var {
		g.write(')')
		g.decrement_inside_ternary()
	}
}

fn (mut g Gen) match_expr_sumtype(node ast.MatchExpr, is_expr bool, cond_var string, tmp_var string) {
	for j, branch in node.branches {
		mut sumtype_index := 0
		// iterates through all types in sumtype branches
		for {
			g.aggregate_type_idx = sumtype_index
			is_last := j == node.branches.len - 1
			sym := g.table.get_type_symbol(node.cond_type)
			if branch.is_else || (node.is_expr && is_last && tmp_var.len == 0) {
				if is_expr && tmp_var.len == 0 {
					// TODO too many branches. maybe separate ?: matches
					g.write(' : ')
				} else {
					g.writeln('')
					g.write_v_source_line_info(branch.pos)
					g.writeln('else {')
				}
			} else {
				if j > 0 || sumtype_index > 0 {
					if is_expr && tmp_var.len == 0 {
						g.write(' : ')
					} else {
						g.writeln('')
						g.write_v_source_line_info(branch.pos)
						g.write('else ')
					}
				}
				if is_expr && tmp_var.len == 0 {
					g.write('(')
				} else {
					if j == 0 && sumtype_index == 0 {
						g.empty_line = true
					}
					g.write_v_source_line_info(branch.pos)
					g.write('if (')
				}
				g.write(cond_var)
				dot_or_ptr := if node.cond_type.is_ptr() { '->' } else { '.' }
				if sym.kind == .sum_type {
					g.write('${dot_or_ptr}_typ == ')
					g.expr(branch.exprs[sumtype_index])
				} else if sym.kind == .interface_ {
					if branch.exprs[sumtype_index] is ast.Type {
						typ := branch.exprs[sumtype_index] as ast.Type
						branch_sym := g.table.get_type_symbol(typ.typ)
						g.write('${dot_or_ptr}_interface_idx == _${sym.cname}_${branch_sym.cname}_index')
					} else if branch.exprs[sumtype_index] is ast.None && sym.name == 'IError' {
						g.write('${dot_or_ptr}_interface_idx == _IError_None___index')
					}
				}
				if is_expr && tmp_var.len == 0 {
					g.write(') ? ')
				} else {
					g.writeln(') {')
				}
			}
			g.stmts_with_tmp_var(branch.stmts, tmp_var)
			if g.inside_ternary == 0 {
				g.write('}')
			}
			sumtype_index++
			if branch.exprs.len == 0 || sumtype_index == branch.exprs.len {
				break
			}
		}
		// reset global field for next use
		g.aggregate_type_idx = 0
	}
}

fn (mut g Gen) match_expr_classic(node ast.MatchExpr, is_expr bool, cond_var string, tmp_var string) {
	type_sym := g.table.get_type_symbol(node.cond_type)
	for j, branch in node.branches {
		is_last := j == node.branches.len - 1
		if branch.is_else || (node.is_expr && is_last && tmp_var.len == 0) {
			if node.branches.len > 1 {
				if is_expr && tmp_var.len == 0 {
					// TODO too many branches. maybe separate ?: matches
					g.write(' : ')
				} else {
					g.writeln('')
					g.write_v_source_line_info(branch.pos)
					g.writeln('else {')
				}
			}
		} else {
			if j > 0 {
				if is_expr && tmp_var.len == 0 {
					g.write(' : ')
				} else {
					g.writeln('')
					g.write_v_source_line_info(branch.pos)
					g.write('else ')
				}
			}
			if is_expr && tmp_var.len == 0 {
				g.write('(')
			} else {
				if j == 0 {
					g.writeln('')
				}
				g.write_v_source_line_info(branch.pos)
				g.write('if (')
			}
			for i, expr in branch.exprs {
				if i > 0 {
					g.write(' || ')
				}
				if type_sym.kind == .string {
					if expr is ast.StringLiteral && (expr as ast.StringLiteral).val == '' {
						g.write('${cond_var}.len == 0')
					} else {
						g.write('string_eq(')
						g.write(cond_var)
						g.write(', ')
						g.expr(expr)
						g.write(')')
					}
				} else if expr is ast.RangeExpr {
					// if type is unsigned and low is 0, check is unneeded
					mut skip_low := false
					if expr.low is ast.IntegerLiteral {
						if node.cond_type in [table.u16_type, table.u32_type, table.u64_type]
							&& expr.low.val == '0' {
							skip_low = true
						}
					}
					g.write('(')
					if !skip_low {
						g.write(cond_var)
						g.write(' >= ')
						g.expr(expr.low)
						g.write(' && ')
					}
					g.write(cond_var)
					g.write(' <= ')
					g.expr(expr.high)
					g.write(')')
				} else {
					g.write(cond_var)
					g.write(' == ')
					g.write('(')
					g.expr(expr)
					g.write(')')
				}
			}
			if is_expr && tmp_var.len == 0 {
				g.write(') ? ')
			} else {
				g.writeln(') {')
			}
		}
		g.stmts_with_tmp_var(branch.stmts, tmp_var)
		if g.inside_ternary == 0 && node.branches.len >= 1 {
			g.write('}')
		}
	}
}

fn (mut g Gen) map_init(node ast.MapInit) {
	key_typ_str := g.typ(node.key_type)
	value_typ_str := g.typ(node.value_type)
	value_typ := g.table.get_type_symbol(node.value_type)
	key_typ := g.table.get_final_type_symbol(node.key_type)
	hash_fn, key_eq_fn, clone_fn, free_fn := g.map_fn_ptrs(key_typ)
	size := node.vals.len
	mut shared_styp := '' // only needed for shared &[]{...}
	mut styp := ''
	is_amp := g.is_amp
	g.is_amp = false
	if is_amp {
		g.out.go_back(1) // delete the `&` already generated in `prefix_expr()
	}
	if g.is_shared {
		mut shared_typ := node.typ.set_flag(.shared_f)
		shared_styp = g.typ(shared_typ)
		g.writeln('($shared_styp*)__dup_shared_map(&($shared_styp){.val = ')
	} else if is_amp {
		styp = g.typ(node.typ)
		g.write('($styp*)memdup(ADDR($styp, ')
	}
	if size > 0 {
		if value_typ.kind == .function {
			g.write('new_map_init_2($hash_fn, $key_eq_fn, $clone_fn, $free_fn, $size, sizeof($key_typ_str), sizeof(voidptr), _MOV(($key_typ_str[$size]){')
		} else {
			g.write('new_map_init_2($hash_fn, $key_eq_fn, $clone_fn, $free_fn, $size, sizeof($key_typ_str), sizeof($value_typ_str), _MOV(($key_typ_str[$size]){')
		}
		for expr in node.keys {
			g.expr(expr)
			g.write(', ')
		}
		if value_typ.kind == .function {
			g.write('}), _MOV((voidptr[$size]){')
		} else {
			g.write('}), _MOV(($value_typ_str[$size]){')
		}
		for expr in node.vals {
			if expr.is_auto_deref_var() {
				g.write('*')
			}
			g.expr(expr)
			g.write(', ')
		}
		g.write('}))')
	} else {
		g.write('new_map_2(sizeof($key_typ_str), sizeof($value_typ_str), $hash_fn, $key_eq_fn, $clone_fn, $free_fn)')
	}
	if g.is_shared {
		g.write('}, sizeof($shared_styp))')
	} else if is_amp {
		g.write('), sizeof($styp))')
	}
}

fn (mut g Gen) select_expr(node ast.SelectExpr) {
	is_expr := node.is_expr || g.inside_ternary > 0
	cur_line := if is_expr {
		g.empty_line = true
		g.go_before_stmt(0)
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
	mut timeout_expr := ast.Expr{}
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
					if expr.right is ast.Ident || expr.right is ast.IndexExpr
						|| expr.right is ast.SelectorExpr || expr.right is ast.StructInit {
						// addressable objects in the `C` output
						objs << expr.right
						tmp_objs << ''
						elem_types << ''
					} else {
						// must be evaluated to tmp var before real `select` is performed
						objs << ast.Expr{}
						tmp_obj := g.new_tmp_var()
						tmp_objs << tmp_obj
						el_stype := g.typ(g.table.mktyp(expr.right_type))
						g.writeln('$el_stype $tmp_obj;')
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
						g.writeln('$el_stype $tmp_obj;')
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
	g.write('Array_sync__Channel_ptr $chan_array = new_array_from_c_array($n_channels, $n_channels, sizeof(sync__Channel*), _MOV((sync__Channel*[$n_channels]){')
	for i in 0 .. n_channels {
		if i > 0 {
			g.write(', ')
		}
		g.write('(sync__Channel*)(')
		g.expr(channels[i])
		g.write(')')
	}
	g.writeln('}));')
	directions_array := g.new_tmp_var()
	g.write('Array_sync__Direction $directions_array = new_array_from_c_array($n_channels, $n_channels, sizeof(sync__Direction), _MOV((sync__Direction[$n_channels]){')
	for i in 0 .. n_channels {
		if i > 0 {
			g.write(', ')
		}
		if is_push[i] {
			g.write('sync__Direction_push')
		} else {
			g.write('sync__Direction_pop')
		}
	}
	g.writeln('}));')
	objs_array := g.new_tmp_var()
	g.write('Array_voidptr $objs_array = new_array_from_c_array($n_channels, $n_channels, sizeof(voidptr), _MOV((voidptr[$n_channels]){')
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
	g.writeln('}));')
	select_result := g.new_tmp_var()
	g.write('int $select_result = sync__channel_select(&/*arr*/$chan_array, $directions_array, &/*arr*/$objs_array, ')
	if has_timeout {
		g.expr(timeout_expr)
	} else if has_else {
		g.write('0')
	} else {
		g.write('-1')
	}
	g.writeln(');')
	// free the temps that were created
	g.writeln('array_free(&$objs_array);')
	g.writeln('array_free(&$directions_array);')
	g.writeln('array_free(&$chan_array);')
	mut i := 0
	for j in 0 .. node.branches.len {
		if j > 0 {
			g.write('} else ')
		}
		g.write('if ($select_result == ')
		if j == exception_branch {
			g.writeln('-1) {')
		} else {
			g.writeln('$i) {')
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
		g.write('($select_result != -2)')
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
	if node.kind == .constant { // && !node.name.starts_with('g_') {
		// TODO globals hack
		g.write('_const_')
	}
	mut name := c_name(node.name)
	// TODO: temporary, remove this
	node_info := node.info
	if node_info is ast.IdentVar {
		// x ?int
		// `x = 10` => `x.data = 10` (g.right_is_opt == false)
		// `x = new_opt()` => `x = new_opt()` (g.right_is_opt == true)
		// `println(x)` => `println(*(int*)x.data)`
		if node_info.is_optional && !(g.is_assign_lhs && g.right_is_opt) {
			g.write('/*opt*/')
			styp := g.base_type(node_info.typ)
			g.write('(*($styp*)${name}.data)')
			return
		}
		if !g.is_assign_lhs && node_info.share == .shared_t {
			g.write('${name}.val')
			return
		}
		scope := g.file.scope.innermost(node.pos.pos)
		if v := scope.find_var(node.name) {
			if v.sum_type_casts.len > 0 {
				if !prevent_sum_type_unwrapping_once {
					for _ in v.sum_type_casts {
						g.write('(*')
					}
					for i, typ in v.sum_type_casts {
						cast_sym := g.table.get_type_symbol(typ)
						mut is_ptr := false
						if i == 0 {
							g.write(name)
							if v.orig_type.is_ptr() {
								is_ptr = true
							}
						}
						dot := if is_ptr { '->' } else { '.' }
						if mut cast_sym.info is table.Aggregate {
							sym := g.table.get_type_symbol(cast_sym.info.types[g.aggregate_type_idx])
							g.write('${dot}_$sym.cname')
						} else {
							g.write('${dot}_$cast_sym.cname')
						}
						g.write(')')
					}
					return
				}
			}
		}
	} else if node_info is ast.IdentFn {
		if g.pref.obfuscate && g.cur_mod.name == 'main' && name.starts_with('main__') {
			key := node.name
			g.write('/* obf identfn: $key */')
			name = g.obf_table[key] or {
				panic('cgen: obf name "$key" not found, this should never happen')
			}
		}
	}
	g.write(g.get_ternary_name(name))
}

fn (mut g Gen) concat_expr(node ast.ConcatExpr) {
	styp := g.typ(node.return_type)
	sym := g.table.get_type_symbol(node.return_type)
	is_multi := sym.kind == .multi_return
	if !is_multi {
		g.expr(node.vals[0])
	} else {
		g.write('($styp){')
		for i, expr in node.vals {
			g.write('.arg$i=')
			g.expr(expr)
			if i < node.vals.len - 1 {
				g.write(',')
			}
		}
		g.write('}')
	}
}

fn (mut g Gen) need_tmp_var_in_if(node ast.IfExpr) bool {
	if node.is_expr && g.inside_ternary == 0 {
		if g.is_autofree || node.typ.has_flag(.optional) {
			return true
		}
		for branch in node.branches {
			if branch.stmts.len == 1 {
				if branch.stmts[0] is ast.ExprStmt {
					stmt := branch.stmts[0] as ast.ExprStmt
					if stmt.expr is ast.CallExpr {
						if stmt.expr.is_method {
							left_sym := g.table.get_type_symbol(stmt.expr.receiver_type)
							if left_sym.kind in [.array, .array_fixed, .map] {
								return true
							}
						}
					}
				}
			}
		}
	}
	return false
}

fn (mut g Gen) if_expr(node ast.IfExpr) {
	if node.is_comptime {
		g.comp_if(node)
		return
	}
	// For simpe if expressions we can use C's `?:`
	// `if x > 0 { 1 } else { 2 }` => `(x > 0) ? (1) : (2)`
	// For if expressions with multiple statements or another if expression inside, it's much
	// easier to use a temp var, than do C tricks with commas, introduce special vars etc
	// (as it used to be done).
	// Always use this in -autofree, since ?: can have tmp expressions that have to be freed.
	needs_tmp_var := g.need_tmp_var_in_if(node)
	tmp := if needs_tmp_var { g.new_tmp_var() } else { '' }
	mut cur_line := ''
	if needs_tmp_var {
		if node.typ.has_flag(.optional) {
			g.inside_if_optional = true
		}
		styp := g.typ(node.typ)
		cur_line = g.go_before_stmt(0)
		g.empty_line = true
		g.writeln('$styp $tmp; /* if prepend */')
	} else if node.is_expr || g.inside_ternary != 0 {
		g.inside_ternary++
		g.write('(')
		for i, branch in node.branches {
			if i > 0 {
				g.write(' : ')
			}
			if i < node.branches.len - 1 || !node.has_else {
				g.expr(branch.cond)
				g.write(' ? ')
			}
			g.stmts(branch.stmts)
		}
		if node.branches.len == 1 {
			g.write(': 0')
		}
		g.write(')')
		g.decrement_inside_ternary()
		return
	}
	mut is_guard := false
	mut guard_idx := 0
	mut guard_vars := []string{}
	for i, branch in node.branches {
		cond := branch.cond
		if cond is ast.IfGuardExpr {
			if !is_guard {
				is_guard = true
				guard_idx = i
				guard_vars = []string{len: node.branches.len}
			}
			if cond.expr !is ast.IndexExpr && cond.expr !is ast.PrefixExpr {
				var_name := g.new_tmp_var()
				guard_vars[i] = var_name
				g.writeln('${g.typ(cond.expr_type)} $var_name;')
			} else {
				guard_vars[i] = ''
			}
		}
	}
	for i, branch in node.branches {
		if i > 0 {
			g.write('} else ')
		}
		// if last branch is `else {`
		if i == node.branches.len - 1 && node.has_else {
			g.writeln('{')
			// define `err` only for simple `if val := opt {...} else {`
			if is_guard && guard_idx == i - 1 {
				cvar_name := guard_vars[guard_idx]
				g.writeln('\tIError err = ${cvar_name}.err;')
			}
		} else {
			match branch.cond {
				ast.IfGuardExpr {
					mut var_name := guard_vars[i]
					mut short_opt := false
					if var_name == '' {
						short_opt = true // we don't need a further tmp, so use the one we'll get later
						var_name = g.new_tmp_var()
						guard_vars[i] = var_name // for `else`
						g.tmp_count--
						g.writeln('if (${var_name}.state == 0) {')
					} else {
						g.write('if ($var_name = ')
						g.expr(branch.cond.expr)
						g.writeln(', ${var_name}.state == 0) {')
					}
					if branch.cond.var_name != '_' {
						base_type := g.base_type(branch.cond.expr_type)
						if short_opt {
							g.write('\t$base_type $branch.cond.var_name = ')
							g.expr(branch.cond.expr)
							g.writeln(';')
						} else {
							g.writeln('\t$base_type $branch.cond.var_name = *($base_type*)${var_name}.data;')
						}
					}
				}
				else {
					g.write('if (')
					g.expr(branch.cond)
					g.writeln(') {')
				}
			}
		}
		if branch.smartcast && branch.stmts.len > 0 {
			infix := branch.cond as ast.InfixExpr
			if mut infix.left is ast.Ident {
				right_type := infix.right as ast.Type
				left_type := infix.left_type
				it_type := g.typ(right_type.typ)
				g.write('\t$it_type* _sc_tmp_$branch.pos.pos = ($it_type*)')
				g.expr(infix.left)
				if left_type.is_ptr() {
					g.write('->')
				} else {
					g.write('.')
				}
				g.writeln('_object;')
				g.writeln('\t$it_type* $infix.left.name = _sc_tmp_$branch.pos.pos;')
			}
		}
		if needs_tmp_var {
			g.stmts_with_tmp_var(branch.stmts, tmp)
		} else {
			g.stmts(branch.stmts)
		}
	}
	g.writeln('}')
	if needs_tmp_var {
		g.empty_line = false
		g.write('$cur_line $tmp')
	}
	if node.typ.has_flag(.optional) {
		g.inside_if_optional = false
	}
}

[inline]
fn (g &Gen) expr_is_multi_return_call(expr ast.Expr) bool {
	match expr {
		ast.CallExpr { return g.table.get_type_symbol(expr.return_type).kind == .multi_return }
		else { return false }
	}
}

fn (mut g Gen) return_statement(node ast.Return) {
	g.write_v_source_line_info(node.pos)
	if node.exprs.len > 0 {
		// skip `retun $vweb.html()`
		if node.exprs[0] is ast.ComptimeCall {
			g.expr(node.exprs[0])
			g.writeln(';')
			return
		}
	}
	g.inside_return = true
	defer {
		g.inside_return = false
	}
	// got to do a correct check for multireturn
	sym := g.table.get_type_symbol(g.fn_decl.return_type)
	fn_return_is_multi := sym.kind == .multi_return
	fn_return_is_optional := g.fn_decl.return_type.has_flag(.optional)
	if node.exprs.len == 0 {
		if fn_return_is_optional {
			styp := g.typ(g.fn_decl.return_type)
			g.writeln('return ($styp){0};')
		} else {
			if g.is_autofree && !g.is_builtin_mod {
				g.writeln('// free before return (no values returned)')
				g.autofree_scope_vars(node.pos.pos - 1, node.pos.line_nr, true)
			}
			g.writeln('return;')
		}
		return
	}
	// handle promoting none/error/function returning 'Option'
	if fn_return_is_optional {
		optional_none := node.exprs[0] is ast.None
		ftyp := g.typ(node.types[0])
		mut is_regular_option := ftyp in ['Option2', 'Option']
		if optional_none || is_regular_option {
			styp := g.typ(g.fn_decl.return_type)
			g.write('return ($styp){ .state=2, .err=')
			g.expr_with_cast(node.exprs[0], node.types[0], g.fn_decl.return_type)
			g.writeln(' };')
			return
		} else if node.types[0] == table.error_type_idx {
			// foo() or { return err }
			styp := g.typ(g.fn_decl.return_type)
			g.write('return ($styp){.state=2, .err=')
			g.expr_with_cast(node.exprs[0], node.types[0], g.fn_decl.return_type)
			g.writeln(' };')
			return
		}
	}
	// regular cases
	if fn_return_is_multi && node.exprs.len > 0 && !g.expr_is_multi_return_call(node.exprs[0]) { // not_optional_none { //&& !fn_return_is_optional {
		// typ_sym := g.table.get_type_symbol(g.fn_decl.return_type)
		// mr_info := typ_sym.info as table.MultiReturn
		mut styp := ''
		mut opt_tmp := ''
		mut opt_type := ''
		if fn_return_is_optional {
			opt_type = g.typ(g.fn_decl.return_type)
			// Create a tmp for this option
			opt_tmp = g.new_tmp_var()
			g.writeln('$opt_type $opt_tmp;')
			styp = g.base_type(g.fn_decl.return_type)
			g.write('opt_ok(&($styp/*X*/[]) { ')
		} else {
			g.write('return ')
			styp = g.typ(g.fn_decl.return_type)
		}
		// Use this to keep the tmp assignments in order
		mut multi_unpack := ''
		g.write('($styp){')
		mut arg_idx := 0
		for i, expr in node.exprs {
			// Check if we are dealing with a multi return and handle it seperately
			if g.expr_is_multi_return_call(expr) {
				c := expr as ast.CallExpr
				expr_sym := g.table.get_type_symbol(c.return_type)
				// Create a tmp for this call
				mut tmp := g.new_tmp_var()
				if !c.return_type.has_flag(.optional) {
					s := g.go_before_stmt(0)
					expr_styp := g.typ(c.return_type)
					g.write('$expr_styp $tmp=')
					g.expr(expr)
					g.writeln(';')
					multi_unpack += g.go_before_stmt(0)
					g.write(s)
				} else {
					s := g.go_before_stmt(0)
					// TODO
					// I (emily) am sorry for doing this
					// I cant find another way to do this so right now
					// this will have to do.
					g.tmp_count--
					g.expr(expr)
					multi_unpack += g.go_before_stmt(0)
					g.write(s)
					// modify tmp so that it is the opt deref
					// TODO copy-paste from cgen.v:2397
					expr_styp := g.base_type(c.return_type)
					tmp = ('/*opt*/(*($expr_styp*)${tmp}.data)')
				}
				expr_types := expr_sym.mr_info().types
				for j, _ in expr_types {
					g.write('.arg$arg_idx=${tmp}.arg$j')
					if j < expr_types.len || i < node.exprs.len - 1 {
						g.write(',')
					}
					arg_idx++
				}
				continue
			}
			g.write('.arg$arg_idx=')
			g.expr(expr)
			arg_idx++
			if i < node.exprs.len - 1 {
				g.write(', ')
			}
		}
		g.write('}')
		if fn_return_is_optional {
			g.writeln(' }, (Option*)(&$opt_tmp), sizeof($styp));')
			g.write('return $opt_tmp')
		}
		// Make sure to add our unpacks
		if multi_unpack.len > 0 {
			g.insert_before_stmt(multi_unpack)
		}
	} else if node.exprs.len >= 1 {
		// normal return
		return_sym := g.table.get_type_symbol(node.types[0])
		expr0 := node.exprs[0]
		// `return opt_ok(expr)` for functions that expect an optional
		expr_type_is_opt := match expr0 {
			ast.CallExpr {
				expr0.return_type.has_flag(.optional) && expr0.or_block.kind == .absent
			}
			else {
				node.types[0].has_flag(.optional)
			}
		}
		if fn_return_is_optional && !expr_type_is_opt && return_sym.name !in ['Option2', 'Option'] {
			styp := g.base_type(g.fn_decl.return_type)
			opt_type := g.typ(g.fn_decl.return_type)
			// Create a tmp for this option
			opt_tmp := g.new_tmp_var()
			g.writeln('$opt_type $opt_tmp;')
			g.write('opt_ok(&($styp[]) { ')
			if !g.fn_decl.return_type.is_ptr() && node.types[0].is_ptr() {
				if !(node.exprs[0] is ast.Ident && !g.is_amp) {
					g.write('*')
				}
			}
			for i, expr in node.exprs {
				g.expr_with_cast(expr, node.types[i], g.fn_decl.return_type.clear_flag(.optional))
				if i < node.exprs.len - 1 {
					g.write(', ')
				}
			}
			g.writeln(' }, (Option*)(&$opt_tmp), sizeof($styp));')
			g.writeln('return $opt_tmp;')
			return
		}
		// free := g.is_autofree && !g.is_builtin_mod // node.exprs[0] is ast.CallExpr
		free := !g.is_builtin_mod // node.exprs[0] is ast.CallExpr
		mut tmp := ''
		if free {
			// `return foo(a, b, c)`
			// `tmp := foo(a, b, c); free(a); free(b); free(c); return tmp;`
			// Save return value in a temp var so that it all args (a,b,c) can be freed
			// Don't use a tmp var if a variable is simply returned: `return x`
			if node.exprs[0] !is ast.Ident {
				tmp = g.new_tmp_var()
				g.write(g.typ(g.fn_decl.return_type))
				g.write(' ')
				g.write(tmp)
				g.write(' = ')
			} else {
				g.write('return ')
			}
		} else {
			g.write('return ')
		}
		g.expr_with_cast(node.exprs[0], node.types[0], g.fn_decl.return_type)
		if free {
			expr := node.exprs[0]
			if expr is ast.Ident {
				g.returned_var_name = expr.name
			}
			g.writeln(';')
			// autofree before `return`
			// set free_parent_scopes to true, since all variables defined in parent
			// scopes need to be freed before the return
			if g.pref.autofree {
				g.autofree_scope_vars(node.pos.pos - 1, node.pos.line_nr, true)
			}
			if tmp != '' {
				g.write('return $tmp')
			}
		}
	} else { // if node.exprs.len == 0 {
		println('this should never happen')
		g.write('/*F*/return')
	}
	g.writeln(';')
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
					eprintln('>> skipping unused const name: $field.name')
				}
				continue
			}
		}

		name := c_name(field.name)
		// TODO hack. Cut the generated value and paste it into definitions.
		pos := g.out.len
		g.expr(field.expr)
		val := g.out.after(pos)
		g.out.go_back(val.len)
		/*
		if field.typ == table.byte_type {
			g.const_decl_simple_define(name, val)
			return
		}
		*/
		/*
		if table.is_number(field.typ) {
			g.const_decl_simple_define(name, val)
		} else if field.typ == table.string_type {
			g.definitions.writeln('string _const_$name; // a string literal, inited later')
			if g.pref.build_mode != .build_module {
				g.stringliterals.writeln('\t_const_$name = $val;')
			}
		} else {
		*/
		match field.expr {
			ast.CharLiteral, ast.FloatLiteral, ast.IntegerLiteral {
				g.const_decl_simple_define(name, val)
			}
			ast.ArrayInit {
				if field.expr.is_fixed {
					styp := g.typ(field.expr.typ)
					if g.pref.build_mode != .build_module {
						g.definitions.writeln('$styp _const_$name = $val; // fixed array const')
					} else {
						g.definitions.writeln('$styp _const_$name; // fixed array const')
					}
				} else {
					g.const_decl_init_later(field.mod, name, val, field.typ, false)
				}
			}
			ast.StringLiteral {
				g.definitions.writeln('string _const_$name; // a string literal, inited later')
				if g.pref.build_mode != .build_module {
					g.stringliterals.writeln('\t_const_$name = $val;')
				}
			}
			ast.CallExpr {
				if val.starts_with('Option_') {
					g.inits[field.mod].writeln(val)
					unwrap_option := field.expr.or_block.kind != .absent
					g.const_decl_init_later(field.mod, name, g.current_tmp_var(), field.typ,
						unwrap_option)
				} else {
					g.const_decl_init_later(field.mod, name, val, field.typ, false)
				}
			}
			else {
				g.const_decl_init_later(field.mod, name, val, field.typ, false)
			}
		}
	}
}

fn (mut g Gen) const_decl_simple_define(name string, val string) {
	// Simple expressions should use a #define
	// so that we don't pollute the binary with unnecessary global vars
	// Do not do this when building a module, otherwise the consts
	// will not be accessible.
	g.definitions.write_string('#define _const_$name ')
	g.definitions.writeln(val)
}

fn (mut g Gen) const_decl_init_later(mod string, name string, val string, typ table.Type, unwrap_option bool) {
	// Initialize more complex consts in `void _vinit/2{}`
	// (C doesn't allow init expressions that can't be resolved at compile time).
	mut styp := g.typ(typ)
	if styp == 'Option2' {
		styp = 'IError'
	}
	cname := '_const_$name'
	g.definitions.writeln('$styp $cname; // inited later')
	if cname == '_const_os__args' {
		if g.pref.os == .windows {
			g.inits[mod].writeln('\t_const_os__args = os__init_os_args_wide(___argc, (byteptr*)___argv);')
		} else {
			g.inits[mod].writeln('\t_const_os__args = os__init_os_args(___argc, (byte**)___argv);')
		}
	} else {
		if unwrap_option {
			g.inits[mod].writeln('\t$cname = *($styp*)${val}.data;')
		} else {
			g.inits[mod].writeln('\t$cname = $val;')
		}
	}
	if g.is_autofree {
		if styp.starts_with('Array_') {
			g.cleanups[mod].writeln('\tarray_free(&$cname);')
		}
		if styp == 'string' {
			g.cleanups[mod].writeln('\tstring_free(&$cname);')
		}
	}
}

fn (mut g Gen) global_decl(node ast.GlobalDecl) {
	mod := if g.pref.build_mode == .build_module && g.is_builtin_mod { 'static ' } else { '' }
	for field in node.fields {
		styp := g.typ(field.typ)
		if field.has_expr {
			g.definitions.writeln('$mod$styp $field.name = $field.expr; // global')
		} else {
			g.definitions.writeln('$mod$styp $field.name; // global')
		}
	}
}

fn (mut g Gen) go_back_out(n int) {
	g.out.go_back(n)
}

const (
	skip_struct_init = ['struct stat', 'struct addrinfo']
)

fn (mut g Gen) struct_init(struct_init ast.StructInit) {
	styp := g.typ(struct_init.typ)
	mut shared_styp := '' // only needed for shared x := St{...
	if styp in c.skip_struct_init {
		// needed for c++ compilers
		g.go_back_out(3)
		return
	}
	sym := g.table.get_final_type_symbol(struct_init.typ)
	is_amp := g.is_amp
	is_multiline := struct_init.fields.len > 5
	g.is_amp = false // reset the flag immediately so that other struct inits in this expr are handled correctly
	if is_amp {
		g.out.go_back(1) // delete the `&` already generated in `prefix_expr()
	}
	if g.is_shared && !g.inside_opt_data && !g.is_arraymap_set {
		mut shared_typ := struct_init.typ.set_flag(.shared_f)
		shared_styp = g.typ(shared_typ)
		g.writeln('($shared_styp*)__dup${shared_styp}(&($shared_styp){.val = ($styp){')
	} else if is_amp {
		g.write('($styp*)memdup(&($styp){')
	} else if struct_init.typ.is_ptr() {
		basetyp := g.typ(struct_init.typ.set_nr_muls(0))
		if is_multiline {
			g.writeln('&($basetyp){')
		} else {
			g.write('&($basetyp){')
		}
	} else {
		if is_multiline {
			g.writeln('($styp){')
		} else {
			g.write('($styp){')
		}
	}
	// mut fields := []string{}
	mut inited_fields := map[string]int{} // TODO this is done in checker, move to ast node
	/*
	if struct_init.fields.len == 0 && struct_init.exprs.len > 0 {
		// Get fields for {a,b} short syntax. Fields array wasn't set in the parser.
		for f in info.fields {
			fields << f.name
		}
	} else {
		fields = struct_init.fields
	}
	*/
	if is_multiline {
		g.indent++
	}
	// User set fields
	mut initialized := false
	for i, field in struct_init.fields {
		inited_fields[field.name] = i
		if sym.kind != .struct_ {
			field_name := c_name(field.name)
			g.write('.$field_name = ')
			if field.typ == 0 {
				g.checker_bug('struct init, field.typ is 0', field.pos)
			}
			field_type_sym := g.table.get_type_symbol(field.typ)
			mut cloned := false
			if g.is_autofree && !field.typ.is_ptr() && field_type_sym.kind in [.array, .string] {
				g.write('/*clone1*/')
				if g.gen_clone_assignment(field.expr, field_type_sym, false) {
					cloned = true
				}
			}
			if !cloned {
				if (field.expected_type.is_ptr() && !field.expected_type.has_flag(.shared_f))
					&& !(field.typ.is_ptr() || field.typ.is_pointer()) && !field.typ.is_number() {
					g.write('/* autoref */&')
				}
				g.expr_with_cast(field.expr, field.typ, field.expected_type)
			}
			if i != struct_init.fields.len - 1 {
				if is_multiline {
					g.writeln(',')
				} else {
					g.write(', ')
				}
			}
			initialized = true
		}
	}
	// The rest of the fields are zeroed.
	// `inited_fields` is a list of fields that have been init'ed, they are skipped
	// mut nr_fields := 0
	if sym.kind == .struct_ {
		info := sym.info as table.Struct
		if info.is_union && struct_init.fields.len > 1 {
			verror('union must not have more than 1 initializer')
		}
		for embed in info.embeds {
			embed_sym := g.table.get_type_symbol(embed)
			embed_name := embed_sym.embed_name()
			if embed_name !in inited_fields {
				default_init := ast.StructInit{
					typ: embed
				}
				g.write('.$embed_name = ')
				g.struct_init(default_init)
				if is_multiline {
					g.writeln(',')
				} else {
					g.write(',')
				}
				initialized = true
			}
		}
		// g.zero_struct_fields(info, inited_fields)
		// nr_fields = info.fields.len
		for field in info.fields {
			if mut sym.info is table.Struct {
				equal_fields := sym.info.fields.filter(it.name == field.name)
				if equal_fields.len == 0 {
					continue
				}
			}
			if field.name in inited_fields {
				sfield := struct_init.fields[inited_fields[field.name]]
				field_name := c_name(sfield.name)
				if sfield.typ == 0 {
					continue
				}
				g.write('.$field_name = ')
				field_type_sym := g.table.get_type_symbol(sfield.typ)
				mut cloned := false
				if g.is_autofree && !sfield.typ.is_ptr() && field_type_sym.kind in [.array, .string] {
					g.write('/*clone1*/')
					if g.gen_clone_assignment(sfield.expr, field_type_sym, false) {
						cloned = true
					}
				}
				if !cloned {
					if (sfield.expected_type.is_ptr() && !sfield.expected_type.has_flag(.shared_f))
						&& !(sfield.typ.is_ptr() || sfield.typ.is_pointer())
						&& !sfield.typ.is_number() {
						g.write('/* autoref */&')
					}
					g.expr_with_cast(sfield.expr, sfield.typ, sfield.expected_type)
				}
				if is_multiline {
					g.writeln(',')
				} else {
					g.write(',')
				}
				initialized = true
				continue
			}
			if info.is_union {
				// unions thould have exactly one explicit initializer
				continue
			}
			if field.typ.has_flag(.optional) {
				// TODO handle/require optionals in inits
				continue
			}
			if field.typ in info.embeds {
				continue
			}
			if struct_init.has_update_expr {
				g.expr(struct_init.update_expr)
				if struct_init.update_expr_type.is_ptr() {
					g.write('->')
				} else {
					g.write('.')
				}
				g.write(field.name)
			} else {
				g.zero_struct_field(field)
			}
			if is_multiline {
				g.writeln(',')
			} else {
				g.write(',')
			}
			initialized = true
		}
	}
	if is_multiline {
		g.indent--
	}

	if !initialized {
		g.write('0')
	}

	g.write('}')
	if g.is_shared && !g.inside_opt_data && !g.is_arraymap_set {
		g.write('}, sizeof($shared_styp))')
	} else if is_amp {
		g.write(', sizeof($styp))')
	}
}

fn (mut g Gen) zero_struct_field(field table.Field) {
	field_name := c_name(field.name)
	g.write('.$field_name = ')
	sym := g.table.get_type_symbol(field.typ)
	defex := ast.fe2ex(field.default_expr)
	if field.has_default_expr {
		if sym.kind in [.sum_type, .interface_] {
			g.expr_with_cast(defex, field.default_expr_typ, field.typ)
			return
		}
		g.expr(defex)
	} else {
		g.write(g.type_default(field.typ))
	}
}

// fn (mut g Gen) zero_struct_fields(info table.Struct, inited_fields map[string]int) {
// }
// { user | name: 'new name' }
fn (mut g Gen) assoc(node ast.Assoc) {
	g.writeln('// assoc')
	if node.typ == 0 {
		return
	}
	styp := g.typ(node.typ)
	g.writeln('($styp){')
	mut inited_fields := map[string]int{}
	for i, field in node.fields {
		inited_fields[field] = i
	}
	// Merge inited_fields in the rest of the fields.
	sym := g.table.get_type_symbol(node.typ)
	info := sym.info as table.Struct
	for field in info.fields {
		field_name := c_name(field.name)
		if field.name in inited_fields {
			g.write('\t.$field_name = ')
			g.expr(node.exprs[inited_fields[field.name]])
			g.writeln(', ')
		} else {
			g.writeln('\t.$field_name = ${node.var_name}.$field_name,')
		}
	}
	g.write('}')
	if g.is_amp {
		g.write(', sizeof($styp))')
	}
}

fn verror(s string) {
	util.verror('cgen error', s)
}

fn (g &Gen) error(s string, pos token.Position) {
	ferror := util.formatted_error('cgen error:', s, g.file.path, pos)
	eprintln(ferror)
	exit(1)
}

fn (g &Gen) checker_bug(s string, pos token.Position) {
	g.error('checker bug; $s', pos)
}

fn (mut g Gen) write_init_function() {
	if g.pref.is_liveshared {
		return
	}
	fn_vinit_start_pos := g.out.len
	// ___argv is declared as voidptr here, because that unifies the windows/unix logic
	g.writeln('void _vinit(int ___argc, voidptr ___argv) {')
	if g.is_autofree {
		// Pre-allocate the string buffer
		// s_str_buf_size := os.getenv('V_STRBUF_MB')
		// mb_size := if s_str_buf_size == '' { 1 } else { s_str_buf_size.int() }
		// g.writeln('g_str_buf = malloc( ${mb_size} * 1024 * 1000 );')
	}
	if g.pref.prealloc {
		g.writeln('g_m2_buf = malloc(50 * 1000 * 1000);')
		g.writeln('g_m2_ptr = g_m2_buf;')
	}
	// NB: the as_cast table should be *before* the other constant initialize calls,
	// because it may be needed during const initialization of builtin and during
	// calling module init functions too, just in case they do fail...
	g.write('\tas_cast_type_indexes = ')
	g.writeln(g.as_cast_name_table())
	//
	g.writeln('\tbuiltin_init();')
	g.writeln('\tvinit_string_literals();')
	//
	for mod_name in g.table.modules {
		g.writeln('\t// Initializations for module $mod_name :')
		g.write(g.inits[mod_name].str())
		init_fn_name := '${mod_name}.init'
		if initfn := g.table.find_fn(init_fn_name) {
			if initfn.return_type == table.void_type && initfn.params.len == 0 {
				mod_c_name := util.no_dots(mod_name)
				init_fn_c_name := '${mod_c_name}__init'
				g.writeln('\t${init_fn_c_name}();')
			}
		}
	}
	g.writeln('}')
	if g.pref.printfn_list.len > 0 && '_vinit' in g.pref.printfn_list {
		println(g.out.after(fn_vinit_start_pos))
	}
	//
	fn_vcleanup_start_pos := g.out.len
	g.writeln('void _vcleanup() {')
	if g.is_autofree {
		// g.writeln('puts("cleaning up...");')
		reversed_table_modules := g.table.modules.reverse()
		for mod_name in reversed_table_modules {
			g.writeln('\t// Cleanups for module $mod_name :')
			g.writeln(g.cleanups[mod_name].str())
		}
		// g.writeln('\tfree(g_str_buf);')
		g.writeln('\tarray_free(&as_cast_type_indexes);')
	}
	g.writeln('}')
	if g.pref.printfn_list.len > 0 && '_vcleanup' in g.pref.printfn_list {
		println(g.out.after(fn_vcleanup_start_pos))
	}
	//
	needs_constructor := g.pref.is_shared && g.pref.os != .windows
	if needs_constructor {
		// shared libraries need a way to call _vinit/2. For that purpose,
		// provide a constructor/destructor pair, ensuring that all constants
		// are initialized just once, and that they will be freed too.
		// NB: os.args in this case will be [].
		g.writeln('__attribute__ ((constructor))')
		g.writeln('void _vinit_caller() {')
		g.writeln('\tstatic bool once = false; if (once) {return;} once = true;')
		g.writeln('\t_vinit(0,0);')
		g.writeln('}')

		g.writeln('__attribute__ ((destructor))')
		g.writeln('void _vcleanup_caller() {')
		g.writeln('\tstatic bool once = false; if (once) {return;} once = true;')
		g.writeln('\t_vcleanup();')
		g.writeln('}')
	}
}

const (
	builtins = ['string', 'array', 'DenseArray', 'map', 'Error', 'IError', 'Option2', 'Option']
)

fn (mut g Gen) write_builtin_types() {
	mut builtin_types := []table.TypeSymbol{} // builtin types
	// builtin types need to be on top
	// everything except builtin will get sorted
	for builtin_name in c.builtins {
		sym := g.table.types[g.table.type_idxs[builtin_name]]
		if sym.kind == .interface_ {
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
	mut types := []table.TypeSymbol{} // structs that need to be sorted
	for typ in g.table.types {
		if typ.name !in c.builtins {
			types << typ
		}
	}
	// sort structs
	types_sorted := g.sort_structs(types)
	// Generate C code
	g.type_definitions.writeln('// builtin types:')
	g.type_definitions.writeln('//------------------ #endbuiltin')
	g.write_types(types_sorted)
}

fn (mut g Gen) write_types(types []table.TypeSymbol) {
	for typ in types {
		if typ.name.starts_with('C.') {
			continue
		}
		// sym := g.table.get_type_symbol(typ)
		mut name := typ.cname
		match mut typ.info {
			table.Struct {
				if typ.info.generic_types.len > 0 {
					continue
				}
				if name.contains('_T_') {
					g.typedefs.writeln('typedef struct $name $name;')
				}
				// TODO avoid buffer manip
				start_pos := g.type_definitions.len
				if typ.info.is_union {
					g.type_definitions.writeln('union $name {')
				} else {
					g.type_definitions.writeln('struct $name {')
				}
				if typ.info.fields.len > 0 || typ.info.embeds.len > 0 {
					for field in typ.info.fields {
						// Some of these structs may want to contain
						// optionals that may not be defined at this point
						// if this is the case then we are going to
						// buffer manip out in front of the struct
						// write the optional in and then continue
						if field.typ.has_flag(.optional) {
							// Dont use g.typ() here becuase it will register
							// optional and we dont want that
							styp, base := g.optional_type_name(field.typ)
							if styp !in g.optionals {
								last_text := g.type_definitions.after(start_pos).clone()
								g.type_definitions.go_back_to(start_pos)
								g.optionals << styp
								g.typedefs2.writeln('typedef struct $styp $styp;')
								g.type_definitions.writeln('${g.optional_type_text(styp,
									base)};')
								g.type_definitions.write_string(last_text)
							}
						}
						type_name := g.typ(field.typ)
						field_name := c_name(field.name)
						g.type_definitions.writeln('\t$type_name $field_name;')
					}
				} else {
					g.type_definitions.writeln('EMPTY_STRUCT_DECLARATION;')
				}
				// g.type_definitions.writeln('} $name;\n')
				//
				g.type_definitions.writeln('};\n')
			}
			table.Alias {
				// table.Alias { TODO
			}
			table.Thread {
				if g.pref.os == .windows {
					if name == '__v_thread' {
						g.type_definitions.writeln('typedef HANDLE $name;')
					} else {
						// Windows can only return `u32` (no void*) from a thread, so the
						// V gohandle must maintain a pointer to the return value
						g.type_definitions.writeln('typedef struct {')
						g.type_definitions.writeln('\tvoid* ret_ptr;')
						g.type_definitions.writeln('\tHANDLE handle;')
						g.type_definitions.writeln('} $name;')
					}
				} else {
					g.type_definitions.writeln('typedef pthread_t $name;')
				}
			}
			table.SumType {
				g.typedefs.writeln('typedef struct $name $name;')
				g.type_definitions.writeln('')
				g.type_definitions.writeln('// Union sum type $name = ')
				for variant in typ.info.variants {
					g.type_definitions.writeln('//          | ${variant:4d} = ${g.typ(variant.idx()):-20s}')
				}
				g.type_definitions.writeln('struct $name {')
				g.type_definitions.writeln('\tunion {')
				for variant in typ.info.variants {
					variant_sym := g.table.get_type_symbol(variant)
					g.type_definitions.writeln('\t\t${g.typ(variant.to_ptr())} _$variant_sym.cname;')
				}
				g.type_definitions.writeln('\t};')
				g.type_definitions.writeln('\tint _typ;')
				if typ.info.fields.len > 0 {
					g.writeln('\t// pointers to common sumtype fields')
					for field in typ.info.fields {
						g.type_definitions.writeln('\t${g.typ(field.typ.to_ptr())} $field.name;')
					}
				}
				g.type_definitions.writeln('};')
				g.type_definitions.writeln('')
			}
			table.ArrayFixed {
				// .array_fixed {
				styp := typ.cname
				// array_fixed_char_300 => char x[300]
				mut fixed := styp[12..]
				len := styp.after('_')
				fixed = fixed[..fixed.len - len.len - 1]
				if fixed.starts_with('C__') {
					fixed = fixed[3..]
				}
				elem_type := typ.info.elem_type
				elem_sym := g.table.get_type_symbol(elem_type)
				if elem_sym.info is table.FnType {
					pos := g.out.len
					g.write_fn_ptr_decl(&elem_sym.info, '')
					fixed = g.out.after(pos)
					g.out.go_back(fixed.len)
					mut def_str := 'typedef $fixed;'
					def_str = def_str.replace_once('(*)', '(*$styp[$len])')
					g.type_definitions.writeln(def_str)
				} else {
					g.type_definitions.writeln('typedef $fixed $styp [$len];')
				}
			}
			else {}
		}
	}
}

// sort structs by dependant fields
fn (g &Gen) sort_structs(typesa []table.TypeSymbol) []table.TypeSymbol {
	mut dep_graph := depgraph.new_dep_graph()
	// types name list
	mut type_names := []string{}
	for typ in typesa {
		type_names << typ.name
	}
	// loop over types
	for t in typesa {
		if t.kind == .interface_ {
			dep_graph.add(t.name, [])
			continue
		}
		// create list of deps
		mut field_deps := []string{}
		match mut t.info {
			table.ArrayFixed {
				dep := g.table.get_type_symbol(t.info.elem_type).name
				if dep in type_names {
					field_deps << dep
				}
			}
			table.Struct {
				for embed in t.info.embeds {
					dep := g.table.get_type_symbol(embed).name
					// skip if not in types list or already in deps
					if dep !in type_names || dep in field_deps {
						continue
					}
					field_deps << dep
				}
				for field in t.info.fields {
					dep := g.table.get_type_symbol(field.typ).name
					// skip if not in types list or already in deps
					if dep !in type_names || dep in field_deps || field.typ.is_ptr() {
						continue
					}
					field_deps << dep
				}
			}
			// table.Interface {}
			else {}
		}
		// add type and dependant types to graph
		dep_graph.add(t.name, field_deps)
	}
	// sort graph
	dep_graph_sorted := dep_graph.resolve()
	if !dep_graph_sorted.acyclic {
		// this should no longer be called since it's catched in the parser
		// TODO: should it be removed?
		verror('cgen.sort_structs(): the following structs form a dependency cycle:\n' +
			dep_graph_sorted.display_cycles() +
			'\nyou can solve this by making one or both of the dependant struct fields references, eg: field &MyStruct' +
			'\nif you feel this is an error, please create a new issue here: https://github.com/vlang/v/issues and tag @joe-conigliaro')
	}
	// sort types
	mut types_sorted := []table.TypeSymbol{}
	for node in dep_graph_sorted.nodes {
		types_sorted << g.table.types[g.table.type_idxs[node.name]]
	}
	return types_sorted
}

[inline]
fn (g &Gen) nth_stmt_pos(n int) int {
	return g.stmt_path_pos[g.stmt_path_pos.len - (1 + n)]
}

fn (mut g Gen) go_before_stmt(n int) string {
	stmt_pos := g.nth_stmt_pos(n)
	cur_line := g.out.after(stmt_pos)
	g.out.go_back(cur_line.len)
	return cur_line
}

[inline]
fn (mut g Gen) go_before_ternary() string {
	return g.go_before_stmt(g.inside_ternary)
}

fn (mut g Gen) insert_before_stmt(s string) {
	cur_line := g.go_before_stmt(0)
	g.writeln(s)
	g.write(cur_line)
}

fn (mut g Gen) write_expr_to_string(expr ast.Expr) string {
	pos := g.out.buf.len
	g.expr(expr)
	return g.out.cut_last(g.out.buf.len - pos)
}

// fn (mut g Gen) start_tmp() {
// }
// If user is accessing the return value eg. in assigment, pass the variable name.
// If the user is not using the optional return value. We need to pass a temp var
// to access its fields (`.ok`, `.error` etc)
// `os.cp(...)` => `Option bool tmp = os__cp(...); if (tmp.state != 0) { ... }`
// Returns the type of the last stmt
fn (mut g Gen) or_block(var_name string, or_block ast.OrExpr, return_type table.Type) {
	cvar_name := c_name(var_name)
	mr_styp := g.base_type(return_type)
	is_none_ok := mr_styp == 'void'
	g.writeln(';')
	if is_none_ok {
		g.writeln('if (${cvar_name}.state != 0 && ${cvar_name}.err._interface_idx != _IError_None___index) {')
	} else {
		g.writeln('if (${cvar_name}.state != 0) { /*or block*/ ')
	}
	if or_block.kind == .block {
		if g.inside_or_block {
			g.writeln('\terr = ${cvar_name}.err;')
		} else {
			g.writeln('\tIError err = ${cvar_name}.err;')
		}
		g.inside_or_block = true
		defer {
			g.inside_or_block = false
		}
		stmts := or_block.stmts
		if stmts.len > 0 && stmts[or_block.stmts.len - 1] is ast.ExprStmt
			&& (stmts[stmts.len - 1] as ast.ExprStmt).typ != table.void_type {
			g.indent++
			for i, stmt in stmts {
				if i == stmts.len - 1 {
					expr_stmt := stmt as ast.ExprStmt
					g.stmt_path_pos << g.out.len
					g.write('*($mr_styp*) ${cvar_name}.data = ')
					old_inside_opt_data := g.inside_opt_data
					g.inside_opt_data = true
					g.expr_with_cast(expr_stmt.expr, expr_stmt.typ, return_type.clear_flag(.optional))
					g.inside_opt_data = old_inside_opt_data
					if g.inside_ternary == 0 && !(expr_stmt.expr is ast.IfExpr) {
						g.writeln(';')
					}
					g.stmt_path_pos.delete_last()
				} else {
					g.stmt(stmt)
				}
			}
			g.indent--
		} else {
			g.stmts(stmts)
		}
	} else if or_block.kind == .propagate {
		if g.file.mod.name == 'main' && (isnil(g.fn_decl) || g.fn_decl.is_main) {
			// In main(), an `opt()?` call is sugar for `opt() or { panic(err) }`
			if g.pref.is_debug {
				paline, pafile, pamod, pafn := g.panic_debug_info(or_block.pos)
				g.writeln('panic_debug($paline, tos3("$pafile"), tos3("$pamod"), tos3("$pafn"), *${cvar_name}.err.msg );')
			} else {
				g.writeln('\tv_panic(_STR("optional not set (%.*s\\000)", 2, ${cvar_name}.err.msg));')
			}
		} else if !isnil(g.fn_decl) && g.fn_decl.is_test {
			g.gen_failing_error_propagation_for_test_fn(or_block, cvar_name)
		} else {
			// In ordinary functions, `opt()?` call is sugar for:
			// `opt() or { return err }`
			// Since we *do* return, first we have to ensure that
			// the defered statements are generated.
			g.write_defer_stmts()
			// Now that option types are distinct we need a cast here
			if g.fn_decl.return_type == table.void_type {
				g.writeln('\treturn;')
			} else {
				styp := g.typ(g.fn_decl.return_type)
				err_obj := g.new_tmp_var()
				g.writeln('\t$styp $err_obj;')
				g.writeln('\tmemcpy(&$err_obj, &$cvar_name, sizeof(Option));')
				g.writeln('\treturn $err_obj;')
			}
		}
	}
	g.write('}')
}

// `a in [1,2,3]` => `a == 1 || a == 2 || a == 3`
fn (mut g Gen) in_optimization(left ast.Expr, right ast.ArrayInit) {
	is_str := right.elem_type == table.string_type
	elem_sym := g.table.get_type_symbol(right.elem_type)
	is_array := elem_sym.kind == .array
	for i, array_expr in right.exprs {
		if is_str {
			g.write('string_eq(')
		} else if is_array {
			ptr_typ := g.gen_array_equality_fn(right.elem_type)
			g.write('${ptr_typ}_arr_eq(')
		}
		g.expr(left)
		if is_str || is_array {
			g.write(', ')
		} else {
			g.write(' == ')
		}
		g.expr(array_expr)
		if is_str || is_array {
			g.write(')')
		}
		if i < right.exprs.len - 1 {
			g.write(' || ')
		}
	}
}

fn op_to_fn_name(name string) string {
	return match name {
		'+' { '_op_plus' }
		'-' { '_op_minus' }
		'*' { '_op_mul' }
		'/' { '_op_div' }
		'%' { '_op_mod' }
		'<' { '_op_lt' }
		'>' { '_op_gt' }
		else { 'bad op $name' }
	}
}

[inline]
fn c_name(name_ string) string {
	name := util.no_dots(name_)
	if name in c.c_reserved {
		return 'v_$name'
	}
	return name
}

fn (mut g Gen) type_default(typ_ table.Type) string {
	typ := g.unwrap_generic(typ_)
	if typ.has_flag(.optional) {
		return '{0}'
	}
	// Always set pointers to 0
	if typ.is_ptr() {
		return '0'
	}
	sym := g.table.get_type_symbol(typ)
	if sym.kind == .array {
		elem_sym := g.typ(sym.array_info().elem_type)
		mut elem_type_str := util.no_dots(elem_sym)
		if elem_type_str.starts_with('C__') {
			elem_type_str = elem_type_str[3..]
		}
		return '__new_array(0, 1, sizeof($elem_type_str))'
	}
	if sym.kind == .map {
		info := sym.map_info()
		key_typ := g.table.get_type_symbol(info.key_type)
		hash_fn, key_eq_fn, clone_fn, free_fn := g.map_fn_ptrs(key_typ)
		return 'new_map_2(sizeof(${g.typ(info.key_type)}), sizeof(${g.typ(info.value_type)}), $hash_fn, $key_eq_fn, $clone_fn, $free_fn)'
	}
	// User struct defined in another module.
	// if typ.contains('__') {
	if sym.kind == .struct_ {
		mut has_none_zero := false
		mut init_str := '{'
		info := sym.info as table.Struct
		for field in info.fields {
			field_sym := g.table.get_type_symbol(field.typ)
			if field_sym.kind in [.array, .map] || field.has_default_expr {
				if field.has_default_expr {
					expr_str := g.expr_string(ast.fe2ex(field.default_expr))
					init_str += '.$field.name = $expr_str,'
				} else {
					init_str += '.$field.name = ${g.type_default(field.typ)},'
				}
				has_none_zero = true
			}
		}
		if has_none_zero {
			init_str += '}'
			type_name := g.typ(typ)
			init_str = '($type_name)' + init_str
		} else {
			init_str += '0}'
		}
		return init_str
	}
	// if typ.ends_with('Fn') { // TODO
	// return '0'
	// }
	// Default values for other types are not needed because of mandatory initialization
	idx := int(typ)
	if idx >= 1 && idx <= 17 {
		return '0'
	}
	/*
	match idx {
		table.bool_type_idx {
			return '0'
		}
		else {}
	}
	*/
	match sym.name {
		'string' { return '(string){.str=(byteptr)""}' }
		'rune' { return '0' }
		else {}
	}
	if sym.kind == .chan {
		elemtypstr := g.typ(sym.chan_info().elem_type)
		return 'sync__new_channel_st(0, sizeof($elemtypstr))'
	}
	return match sym.kind {
		.interface_, .sum_type, .array_fixed, .multi_return { '{0}' }
		.alias { g.type_default((sym.info as table.Alias).parent_type) }
		else { '0' }
	}
	// TODO this results in
	// error: expected a field designator, such as '.field = 4'
	// - Empty ee= (Empty) { . =  {0}  } ;
	/*
	return match typ {
	'bool', 'i8', 'i16', 'i64', 'u16', 'u32', 'u64', 'byte', 'int', 'rune', 'byteptr', 'voidptr' {'0'}
	'string'{ '_SLIT("")'}
	'f32'{ '0.0'}
	'f64'{ '0.0'}
	else { '{0} '}
}
	*/
}

fn (g &Gen) get_all_test_function_names() []string {
	mut tfuncs := []string{}
	mut tsuite_begin := ''
	mut tsuite_end := ''
	for _, f in g.table.fns {
		if f.name.ends_with('.testsuite_begin') {
			tsuite_begin = f.name
			continue
		}
		if f.name.contains('.test_') {
			tfuncs << f.name
			continue
		}
		if f.name.ends_with('.testsuite_end') {
			tsuite_end = f.name
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

fn (g &Gen) is_importing_os() bool {
	return 'os' in g.table.imports
}

fn (mut g Gen) go_expr(node ast.GoExpr) {
	line := g.go_before_stmt(0)
	handle := g.go_stmt(node.go_stmt, true)
	g.empty_line = false
	g.write(line)
	g.write(handle)
}

fn (mut g Gen) go_stmt(node ast.GoStmt, joinable bool) string {
	mut handle := ''
	tmp := g.new_tmp_var()
	mut expr := node.call_expr
	mut name := expr.name // util.no_dots(expr.name)
	// TODO: fn call is duplicated. merge with fn_call().
	for i, generic_type in expr.generic_types {
		if generic_type != table.void_type && generic_type != 0 {
			// Using _T_ to differentiate between get<string> and get_string
			// `foo<int>()` => `foo_T_int()`
			if i == 0 {
				name += '_T'
			}
			name += '_' + g.typ(generic_type)
		}
	}
	if expr.is_method {
		receiver_sym := g.table.get_type_symbol(expr.receiver_type)
		name = receiver_sym.name + '_' + name
	} else if mut expr.left is ast.AnonFn {
		g.gen_anon_fn_decl(mut expr.left)
		fsym := g.table.get_type_symbol(expr.left.typ)
		name = fsym.name
	}
	name = util.no_dots(name)
	if g.pref.obfuscate && g.cur_mod.name == 'main' && name.starts_with('main__') {
		mut key := expr.name
		if expr.is_method {
			sym := g.table.get_type_symbol(expr.receiver_type)
			key = sym.name + '.' + expr.name
		}
		g.write('/* obf go: $key */')
		name = g.obf_table[key] or {
			panic('cgen: obf name "$key" not found, this should never happen')
		}
	}
	g.writeln('// go')
	wrapper_struct_name := 'thread_arg_' + name
	wrapper_fn_name := name + '_thread_wrapper'
	arg_tmp_var := 'arg_' + tmp
	g.writeln('$wrapper_struct_name *$arg_tmp_var = malloc(sizeof(thread_arg_$name));')
	if expr.is_method {
		g.write('$arg_tmp_var->arg0 = ')
		// TODO is this needed?
		/*
		if false && !expr.return_type.is_ptr() {
			g.write('&')
		}
		*/
		g.expr(expr.left)
		g.writeln(';')
	}
	for i, arg in expr.args {
		g.write('$arg_tmp_var->arg${i + 1} = ')
		g.expr(arg.expr)
		g.writeln(';')
	}
	s_ret_typ := g.typ(node.call_expr.return_type)
	if g.pref.os == .windows && node.call_expr.return_type != table.void_type {
		g.writeln('$arg_tmp_var->ret_ptr = malloc(sizeof($s_ret_typ));')
	}
	is_opt := node.call_expr.return_type.has_flag(.optional)
	mut gohandle_name := ''
	if node.call_expr.return_type == table.void_type {
		gohandle_name = if is_opt { '__v_thread_Option_void' } else { '__v_thread' }
	} else {
		opt := if is_opt { 'Option_' } else { '' }
		gohandle_name = '__v_thread_$opt${g.table.get_type_symbol(g.unwrap_generic(node.call_expr.return_type)).cname}'
	}
	if g.pref.os == .windows {
		simple_handle := if joinable && node.call_expr.return_type != table.void_type {
			'thread_handle_$tmp'
		} else {
			'thread_$tmp'
		}
		g.writeln('HANDLE $simple_handle = CreateThread(0,0, (LPTHREAD_START_ROUTINE)$wrapper_fn_name, $arg_tmp_var, 0,0);')
		if joinable && node.call_expr.return_type != table.void_type {
			g.writeln('$gohandle_name thread_$tmp = {')
			g.writeln('\t.ret_ptr = $arg_tmp_var->ret_ptr,')
			g.writeln('\t.handle = thread_handle_$tmp')
			g.writeln('};')
		}
		if !joinable {
			g.writeln('CloseHandle(thread_$tmp);')
		}
	} else {
		g.writeln('pthread_t thread_$tmp;')
		g.writeln('pthread_create(&thread_$tmp, NULL, (void*)$wrapper_fn_name, $arg_tmp_var);')
		if !joinable {
			g.writeln('pthread_detach(thread_$tmp);')
		}
	}
	g.writeln('// endgo\n')
	if joinable {
		handle = 'thread_$tmp'
		// create wait handler for this return type if none exists
		waiter_fn_name := gohandle_name + '_wait'
		if waiter_fn_name !in g.waiter_fns {
			g.gowrappers.writeln('\n$s_ret_typ ${waiter_fn_name}($gohandle_name thread) {')
			mut c_ret_ptr_ptr := 'NULL'
			if node.call_expr.return_type != table.void_type {
				g.gowrappers.writeln('\t$s_ret_typ* ret_ptr;')
				c_ret_ptr_ptr = '&ret_ptr'
			}
			if g.pref.os == .windows {
				if node.call_expr.return_type == table.void_type {
					g.gowrappers.writeln('\tu32 stat = WaitForSingleObject(thread, INFINITE);')
				} else {
					g.gowrappers.writeln('\tu32 stat = WaitForSingleObject(thread.handle, INFINITE);')
					g.gowrappers.writeln('\tret_ptr = thread.ret_ptr;')
				}
			} else {
				g.gowrappers.writeln('\tint stat = pthread_join(thread, (void **)$c_ret_ptr_ptr);')
			}
			g.gowrappers.writeln('\tif (stat != 0) { v_panic(_SLIT("unable to join thread")); }')
			if g.pref.os == .windows {
				if node.call_expr.return_type == table.void_type {
					g.gowrappers.writeln('\tCloseHandle(thread);')
				} else {
					g.gowrappers.writeln('\tCloseHandle(thread.handle);')
				}
			}
			if node.call_expr.return_type != table.void_type {
				g.gowrappers.writeln('\t$s_ret_typ ret = *ret_ptr;')
				g.gowrappers.writeln('\tfree(ret_ptr);')
				g.gowrappers.writeln('\treturn ret;')
			} else {
				g.gowrappers.writeln('\treturn;')
			}
			g.gowrappers.writeln('}')
			g.waiter_fns << waiter_fn_name
		}
	}
	// Register the wrapper type and function
	if name in g.threaded_fns {
		return handle
	}
	g.type_definitions.writeln('\ntypedef struct $wrapper_struct_name {')
	if expr.is_method {
		styp := g.typ(expr.receiver_type)
		g.type_definitions.writeln('\t$styp arg0;')
	}
	need_return_ptr := g.pref.os == .windows && node.call_expr.return_type != table.void_type
	if expr.args.len == 0 && !need_return_ptr {
		g.type_definitions.writeln('EMPTY_STRUCT_DECLARATION;')
	} else {
		for i, arg in expr.args {
			styp := g.typ(arg.typ)
			g.type_definitions.writeln('\t$styp arg${i + 1};')
		}
	}
	if need_return_ptr {
		g.type_definitions.writeln('\tvoid* ret_ptr;')
	}
	g.type_definitions.writeln('} $wrapper_struct_name;')
	thread_ret_type := if g.pref.os == .windows { 'u32' } else { 'void*' }
	g.type_definitions.writeln('$thread_ret_type ${wrapper_fn_name}($wrapper_struct_name *arg);')
	g.gowrappers.writeln('$thread_ret_type ${wrapper_fn_name}($wrapper_struct_name *arg) {')
	if node.call_expr.return_type != table.void_type {
		if g.pref.os == .windows {
			g.gowrappers.write_string('\t*(($s_ret_typ*)(arg->ret_ptr)) = ')
		} else {
			g.gowrappers.writeln('\t$s_ret_typ* ret_ptr = malloc(sizeof($s_ret_typ));')
			g.gowrappers.write_string('\t*ret_ptr = ')
		}
	} else {
		g.gowrappers.write_string('\t')
	}
	g.gowrappers.write_string('${name}(')
	if expr.is_method {
		g.gowrappers.write_string('arg->arg0')
		if expr.args.len > 0 {
			g.gowrappers.write_string(', ')
		}
	}
	for i in 0 .. expr.args.len {
		g.gowrappers.write_string('arg->arg${i + 1}')
		if i < expr.args.len - 1 {
			g.gowrappers.write_string(', ')
		}
	}
	g.gowrappers.writeln(');')
	g.gowrappers.writeln('\tfree(arg);')
	if g.pref.os != .windows && node.call_expr.return_type != table.void_type {
		g.gowrappers.writeln('\treturn ret_ptr;')
	} else {
		g.gowrappers.writeln('\treturn 0;')
	}
	g.gowrappers.writeln('}')
	g.threaded_fns << name
	return handle
}

fn (mut g Gen) as_cast(node ast.AsCast) {
	// Make sure the sum type can be cast to this type (the types
	// are the same), otherwise panic.
	// g.insert_before('
	styp := g.typ(node.typ)
	sym := g.table.get_type_symbol(node.typ)
	expr_type_sym := g.table.get_type_symbol(node.expr_type)
	if expr_type_sym.info is table.SumType {
		dot := if node.expr_type.is_ptr() { '->' } else { '.' }
		g.write('/* as */ *($styp*)__as_cast(')
		g.write('(')
		g.expr(node.expr)
		g.write(')')
		g.write(dot)
		g.write('_$sym.cname,')
		g.write('(')
		g.expr(node.expr)
		g.write(')')
		g.write(dot)
		// g.write('typ, /*expected:*/$node.typ)')
		sidx := g.type_sidx(node.typ)
		expected_sym := g.table.get_type_symbol(node.typ)
		g.write('_typ, $sidx) /*expected idx: $sidx, name: $expected_sym.name */ ')

		// fill as cast name table
		for variant in expr_type_sym.info.variants {
			idx := variant.str()
			if idx in g.as_cast_type_names {
				continue
			}
			variant_sym := g.table.get_type_symbol(variant)
			g.as_cast_type_names[idx] = variant_sym.name
		}
	}
}

fn (g Gen) as_cast_name_table() string {
	if g.as_cast_type_names.len == 0 {
		return 'new_array_from_c_array(1, 1, sizeof(VCastTypeIndexName), _MOV((VCastTypeIndexName[1]){(VCastTypeIndexName){.tindex = 0,.tname = _SLIT("unknown")}}));'
	}
	mut name_table := strings.new_builder(1024)
	casts_len := g.as_cast_type_names.len + 1
	name_table.writeln('new_array_from_c_array($casts_len, $casts_len, sizeof(VCastTypeIndexName), _MOV((VCastTypeIndexName[$casts_len]){')
	name_table.writeln('\t\t  (VCastTypeIndexName){.tindex = 0, .tname = _SLIT("unknown")}')
	for key, value in g.as_cast_type_names {
		name_table.writeln('\t\t, (VCastTypeIndexName){.tindex = $key, .tname = _SLIT("$value")}')
	}
	name_table.writeln('\t}));')
	return name_table.str()
}

fn (mut g Gen) is_expr(node ast.InfixExpr) {
	eq := if node.op == .key_is { '==' } else { '!=' }
	g.write('(')
	g.expr(node.left)
	g.write(')')
	if node.left_type.is_ptr() {
		g.write('->')
	} else {
		g.write('.')
	}
	sym := g.table.get_type_symbol(node.left_type)
	if sym.kind == .interface_ {
		g.write('_interface_idx $eq ')
		// `_Animal_Dog_index`
		sub_type := match mut node.right {
			ast.Type { node.right.typ }
			ast.None { g.table.type_idxs['None__'] }
			else { table.Type(0) }
		}
		sub_sym := g.table.get_type_symbol(sub_type)
		g.write('_${c_name(sym.name)}_${c_name(sub_sym.name)}_index')
		return
	} else if sym.kind == .sum_type {
		g.write('_typ $eq ')
	}
	g.expr(node.right)
}

// Generates interface table and interface indexes
fn (mut g Gen) interface_table() string {
	mut sb := strings.new_builder(100)
	for ityp in g.table.types {
		if ityp.kind != .interface_ {
			continue
		}
		inter_info := ityp.info as table.Interface
		// interface_name is for example Speaker
		interface_name := ityp.cname
		// generate a struct that references interface methods
		methods_struct_name := 'struct _${interface_name}_interface_methods'
		mut methods_typ_def := strings.new_builder(100)
		mut methods_struct_def := strings.new_builder(100)
		methods_struct_def.writeln('$methods_struct_name {')
		mut imethods := map[string]string{} // a map from speak -> _Speaker_speak_fn
		mut methodidx := map[string]int{}
		for k, method in inter_info.methods {
			methodidx[method.name] = k
			typ_name := '_${interface_name}_${method.name}_fn'
			ret_styp := g.typ(method.return_type)
			methods_typ_def.write_string('typedef $ret_styp (*$typ_name)(void* _')
			// the first param is the receiver, it's handled by `void*` above
			for i in 1 .. method.params.len {
				arg := method.params[i]
				methods_typ_def.write_string(', ${g.typ(arg.typ)} $arg.name')
			}
			// TODO g.fn_args(method.args[1..], method.is_variadic)
			methods_typ_def.writeln(');')
			methods_struct_def.writeln('\t$typ_name _method_${c_name(method.name)};')
			imethods[method.name] = typ_name
		}
		methods_struct_def.writeln('};')
		// generate an array of the interface methods for the structs using the interface
		// as well as case functions from the struct to the interface
		mut methods_struct := strings.new_builder(100)
		//
		mut staticprefix := 'static'
		iname_table_length := inter_info.types.len
		if iname_table_length == 0 {
			// msvc can not process `static struct x[0] = {};`
			methods_struct.writeln('$methods_struct_name ${interface_name}_name_table[1];')
		} else {
			if g.pref.build_mode != .build_module {
				methods_struct.writeln('$methods_struct_name ${interface_name}_name_table[$iname_table_length] = {')
			} else {
				methods_struct.writeln('$methods_struct_name ${interface_name}_name_table[$iname_table_length];')
			}
		}
		mut cast_functions := strings.new_builder(100)
		mut methods_wrapper := strings.new_builder(100)
		methods_wrapper.writeln('// Methods wrapper for interface "$interface_name"')
		mut already_generated_mwrappers := map[string]int{}
		iinidx_minimum_base := 1000 // NB: NOT 0, to avoid map entries set to 0 later, so `if already_generated_mwrappers[name] > 0 {` works.
		mut current_iinidx := iinidx_minimum_base
		for st in inter_info.types {
			st_sym := g.table.get_type_symbol(st)
			// cctype is the Cleaned Concrete Type name, *without ptr*,
			// i.e. cctype is always just Cat, not Cat_ptr:
			cctype := g.cc_type(st, true)
			$if debug_interface_table ? {
				eprintln(
					'>> interface name: $ityp.name | concrete type: $st.debug() | st symname: ' +
					st_sym.name)
			}
			// Speaker_Cat_index = 0
			interface_index_name := '_${interface_name}_${cctype}_index'
			if already_generated_mwrappers[interface_index_name] > 0 {
				continue
			}
			already_generated_mwrappers[interface_index_name] = current_iinidx
			current_iinidx++
			// eprintln('>>> current_iinidx: ${current_iinidx-iinidx_minimum_base} | interface_index_name: $interface_index_name')
			sb.writeln('$staticprefix $interface_name I_${cctype}_to_Interface_${interface_name}($cctype* x);')
			mut cast_struct := strings.new_builder(100)
			cast_struct.writeln('($interface_name) {')
			cast_struct.writeln('\t\t._object = (void*) (x),')
			cast_struct.writeln('\t\t._interface_idx = $interface_index_name,')
			for field in inter_info.fields {
				cname := c_name(field.name)
				field_styp := g.typ(field.typ)
				if _ := st_sym.find_field(field.name) {
					cast_struct.writeln('\t\t.$cname = ($field_styp*)((char*)x + __offsetof_ptr(x, $cctype, $cname)),')
				} else {
					// the field is embedded in another struct
					cast_struct.write_string('\t\t.$cname = ($field_styp*)((char*)x')
					for embed_type in st_sym.struct_info().embeds {
						embed_sym := g.table.get_type_symbol(embed_type)
						if _ := embed_sym.find_field(field.name) {
							cast_struct.write_string(' + __offsetof_ptr(x, $cctype, $embed_sym.embed_name()) + __offsetof_ptr(x, $embed_sym.cname, $cname)')
							break
						}
					}
					cast_struct.writeln('),')
				}
			}
			cast_struct.write_string('\t}')
			cast_struct_str := cast_struct.str()

			cast_functions.writeln('
// Casting functions for converting "$cctype" to interface "$interface_name"
$staticprefix inline $interface_name I_${cctype}_to_Interface_${interface_name}($cctype* x) {
	return $cast_struct_str;
}')

			if g.pref.build_mode != .build_module {
				methods_struct.writeln('\t{')
			}
			for _, method in st_sym.methods {
				if method.name !in imethods {
					// a method that is not part of the interface should be just skipped
					continue
				}
				// .speak = Cat_speak
				mut method_call := '${cctype}_$method.name'
				if !method.params[0].typ.is_ptr() {
					// inline void Cat_speak_method_wrapper(Cat c) { return Cat_speak(*c); }
					methods_wrapper.write_string('static inline ${g.typ(method.return_type)}')
					methods_wrapper.write_string(' ${method_call}_method_wrapper(')
					//
					params_start_pos := g.out.len
					mut params := method.params.clone()
					// hack to mutate typ
					params[0] = {
						...params[0]
						typ: params[0].typ.set_nr_muls(1)
					}
					fargs, _ := g.fn_args(params, false) // second argument is ignored anyway
					methods_wrapper.write_string(g.out.cut_last(g.out.len - params_start_pos))
					methods_wrapper.writeln(') {')
					methods_wrapper.write_string('\t')
					if method.return_type != table.void_type {
						methods_wrapper.write_string('return ')
					}
					methods_wrapper.writeln('${method_call}(*${fargs.join(', ')});')
					methods_wrapper.writeln('}')
					// .speak = Cat_speak_method_wrapper
					method_call += '_method_wrapper'
				}
				if g.pref.build_mode != .build_module {
					methods_struct.writeln('\t\t._method_${c_name(method.name)} = (void*) $method_call,')
				}
			}
			if g.pref.build_mode != .build_module {
				methods_struct.writeln('\t},')
			}
			iin_idx := already_generated_mwrappers[interface_index_name] - iinidx_minimum_base
			if g.pref.build_mode != .build_module {
				sb.writeln('int $interface_index_name = $iin_idx;')
			} else {
				sb.writeln('int $interface_index_name;')
			}
		}
		sb.writeln('// ^^^ number of types for interface $interface_name: ${current_iinidx - iinidx_minimum_base}')
		if iname_table_length == 0 {
			methods_struct.writeln('')
		} else {
			if g.pref.build_mode != .build_module {
				methods_struct.writeln('};')
			}
		}
		// add line return after interface index declarations
		sb.writeln('')
		if inter_info.methods.len > 0 {
			sb.writeln(methods_wrapper.str())
			sb.writeln(methods_typ_def.str())
			sb.writeln(methods_struct_def.str())
			sb.writeln(methods_struct.str())
		}
		sb.writeln(cast_functions.str())
	}
	return sb.str()
}

fn (mut g Gen) panic_debug_info(pos token.Position) (int, string, string, string) {
	paline := pos.line_nr + 1
	if isnil(g.fn_decl) {
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
	|#if __has_include($iname)
	|#include $iname
	|#else
	|#error VERROR_MESSAGE $imessage
	|#endif
	|
	|#else
	|#include $iname
	|#endif
	'.strip_margin()
	return res
}

fn (mut g Gen) trace(fbase string, message string) {
	if g.file.path_base == fbase {
		println('> g.trace | ${fbase:-10s} | $message')
	}
}
