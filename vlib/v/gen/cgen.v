// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module gen

import strings
import strconv
import v.ast
import v.table
import v.pref
import v.token
import v.util
import v.depgraph
import term

// NB: keywords after 'new' are reserved in C++
const (
	c_reserved = ['delete', 'exit', 'unix', 'error', 'calloc', 'malloc', 'free', 'panic', 'auto',
		'char',
		'default',
		'do',
		'double',
		'extern',
		'float',
		'inline',
		'int',
		'long',
		'register',
		'restrict',
		'short',
		'signed',
		'sizeof',
		'static',
		'switch',
		'typedef',
		'union',
		'unsigned',
		'void',
		'volatile',
		'while',
		'new',
		'namespace',
		'class',
		'typename'
	]
)

struct Gen {
	out                  strings.Builder
	cheaders             strings.Builder
	includes             strings.Builder // all C #includes required by V modules
	typedefs             strings.Builder
	typedefs2            strings.Builder
	type_definitions     strings.Builder // typedefs, defines etc (everything that goes to the top of the file)
	definitions          strings.Builder // typedefs, defines etc (everything that goes to the top of the file)
	inits                strings.Builder // contents of `void _vinit(){}`
	cleanups             strings.Builder // contents of `void _vcleanup(){}`
	gowrappers           strings.Builder // all go callsite wrappers
	stringliterals       strings.Builder // all string literals (they depend on tos3() beeing defined
	auto_str_funcs       strings.Builder // function bodies of all auto generated _str funcs
	comptime_defines     strings.Builder // custom defines, given by -d/-define flags on the CLI
	pcs_declarations     strings.Builder // -prof profile counter declarations for each function
	hotcode_definitions  strings.Builder // -live declarations & functions
	options              strings.Builder // `Option_xxxx` types
	table                &table.Table
	pref                 &pref.Preferences
	module_built         string
mut:
	file                 ast.File
	fn_decl              &ast.FnDecl // pointer to the FnDecl we are currently inside otherwise 0
	last_fn_c_name       string
	tmp_count            int
	variadic_args        map[string]int
	is_c_call            bool // e.g. `C.printf("v")`
	is_assign_lhs        bool // inside left part of assign expr (for array_set(), etc)
	is_assign_rhs        bool // inside right part of assign after `=` (val expr)
	is_array_set         bool
	is_amp               bool // for `&Foo{}` to merge PrefixExpr `&` and StructInit `Foo{}`; also for `&byte(0)` etc
	optionals            []string // to avoid duplicates TODO perf, use map
	inside_ternary       int // ?: comma separated statements on a single line
	ternary_names        map[string]string
	ternary_level_names  map[string][]string
	stmt_path_pos        []int
	right_is_opt         bool
	autofree             bool
	indent               int
	empty_line           bool
	is_test              bool
	assign_op            token.Kind // *=, =, etc (for array_set)
	defer_stmts          []ast.DeferStmt
	defer_ifdef          string
	defer_profile_code   string
	str_types            []string // types that need automatic str() generation
	threaded_fns         []string // for generating unique wrapper types and fns for `go xxx()`
	array_fn_definitions []string // array equality functions that have been defined
	is_json_fn           bool // inside json.encode()
	json_types           []string // to avoid json gen duplicates
	pcs                  []ProfileCounterMeta // -prof profile counter fn_names => fn counter name
	attr                 string
	is_builtin_mod       bool
	hotcode_fn_names     []string
	fn_main              &ast.FnDecl // the FnDecl of the main function. Needed in order to generate the main function code *last*
	cur_fn               &ast.FnDecl
	cur_generic_type     table.Type // `int`, `string`, etc in `foo<T>()`
}

const (
	tabs = ['', '\t', '\t\t', '\t\t\t', '\t\t\t\t', '\t\t\t\t\t', '\t\t\t\t\t\t', '\t\t\t\t\t\t\t',
		'\t\t\t\t\t\t\t\t'
	]
)

pub fn cgen(files []ast.File, table &table.Table, pref &pref.Preferences) string {
	// println('start cgen2')
	mut g := Gen{
		out: strings.new_builder(1000)
		cheaders: strings.new_builder(8192)
		includes: strings.new_builder(100)
		typedefs: strings.new_builder(100)
		typedefs2: strings.new_builder(100)
		type_definitions: strings.new_builder(100)
		definitions: strings.new_builder(100)
		gowrappers: strings.new_builder(100)
		stringliterals: strings.new_builder(100)
		auto_str_funcs: strings.new_builder(100)
		comptime_defines: strings.new_builder(100)
		inits: strings.new_builder(100)
		cleanups: strings.new_builder(100)
		pcs_declarations: strings.new_builder(100)
		hotcode_definitions: strings.new_builder(100)
		options: strings.new_builder(100)
		table: table
		pref: pref
		fn_decl: 0
		fn_main: 0
		cur_fn: 0
		autofree: true
		indent: -1
		module_built: pref.path.after('vlib/')
	}
	g.init()
	//
	mut tests_inited := false
	mut autofree_used := false
	for file in files {
		g.file = file
		// println('\ncgen "$g.file.path" nr_stmts=$file.stmts.len')
		// building_v := true && (g.file.path.contains('/vlib/') || g.file.path.contains('cmd/v'))
		is_test := g.file.path.ends_with('.vv') || g.file.path.ends_with('_test.v')
		if g.file.path.ends_with('_test.v') {
			g.is_test = is_test
		}
		if g.file.path == '' || is_test || !g.pref.autofree {
			// cgen test or building V
			// println('autofree=false')
			g.autofree = false
		} else {
			g.autofree = true
			autofree_used = true
		}
		// anon fn may include assert and thus this needs
		// to be included before any test contents are written
		if g.is_test && !tests_inited {
			g.write_tests_main()
			tests_inited = true
		}
		g.stmts(file.stmts)
	}
	if autofree_used {
		g.autofree = true // so that void _vcleanup is generated
	}
	g.write_variadic_types()
	// g.write_str_definitions()
	if g.pref.build_mode != .build_module {
		// no init in builtin.o
		g.write_init_function()
	}
	//
	g.finish()
	//
	b := strings.new_builder(250000)
	b.writeln(g.hashes())
	b.writeln(g.comptime_defines.str())
	b.writeln('\n// V typedefs:')
	b.writeln(g.typedefs.str())
	b.writeln('\n// V typedefs2:')
	b.writeln(g.typedefs2.str())
	b.writeln('\n// V cheaders:')
	b.writeln(g.cheaders.str())
	b.writeln('\n// V includes:')
	b.writeln(g.includes.str())
	b.writeln('\n// V type definitions:')
	b.writeln(g.type_definitions.str())
	b.writeln('\n// V Option_xxx definitions:')
	b.writeln(g.options.str())
	b.writeln('\n// V definitions:')
	b.writeln(g.definitions.str())
	b.writeln('\n// V profile counters:')
	b.writeln(g.pcs_declarations.str())
	b.writeln('\n// V interface table:')
	b.writeln(g.interface_table())
	b.writeln('\n// V gowrappers:')
	b.writeln(g.gowrappers.str())
	b.writeln('\n// V hotcode definitions:')
	b.writeln(g.hotcode_definitions.str())
	b.writeln('\n// V stringliterals:')
	b.writeln(g.stringliterals.str())
	b.writeln('\n// V auto str functions:')
	b.writeln(g.auto_str_funcs.str())
	b.writeln('\n// V out')
	b.writeln(g.out.str())
	b.writeln('\n// THE END.')
	return b.str()
}

pub fn (g Gen) hashes() string {
	mut res := c_commit_hash_default.replace('@@@', util.vhash())
	res += c_current_commit_hash_default.replace('@@@', util.githash(g.pref.building_v))
	return res
}

pub fn (mut g Gen) init() {
	if g.pref.custom_prelude != '' {
		g.cheaders.writeln(g.pref.custom_prelude)
	} else if !g.pref.no_preludes {
		g.cheaders.writeln('// Generated by the V compiler')
		g.cheaders.writeln('#include <inttypes.h>') // int64_t etc
		g.cheaders.writeln(c_builtin_types)
		if g.pref.is_bare {
			g.cheaders.writeln(bare_c_headers)
		} else {
			g.cheaders.writeln(c_headers)
		}
		g.definitions.writeln('\nvoid _STR_PRINT_ARG(const char*, char**, int*, int*, int, ...);\n')
		g.definitions.writeln('\nstring _STR(const char*, int, ...);\n')
		g.definitions.writeln('\nstring _STR_TMP(const char*, ...);\n')
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
		g.comptime_defines.writeln('//     All custom defines      : ' + g.pref.compile_defines_all.join(','))
		g.comptime_defines.writeln('//     Turned ON custom defines: ' + g.pref.compile_defines.join(','))
		for cdefine in g.pref.compile_defines {
			g.comptime_defines.writeln('#define CUSTOM_DEFINE_${cdefine}')
		}
		g.comptime_defines.writeln('')
	}
	if g.pref.is_debug || 'debug' in g.pref.compile_defines {
		g.comptime_defines.writeln('#define _VDEBUG (1)')
	}
	if g.pref.is_livemain || g.pref.is_liveshared {
		g.generate_hotcode_reloading_declarations()
	}
}

pub fn (mut g Gen) finish() {
	if g.pref.build_mode != .build_module {
		g.stringliterals.writeln('}')
	}
	g.stringliterals.writeln('// << string literal consts')
	g.stringliterals.writeln('')
	if g.pref.is_prof {
		g.gen_vprint_profile_stats()
	}
	if g.pref.is_livemain || g.pref.is_liveshared {
		g.generate_hotcode_reloader_code()
	}
	if g.fn_main != voidptr(0) {
		g.out.writeln('')
		g.fn_decl = g.fn_main
		g.gen_fn_decl(g.fn_main)
	}
}

pub fn (mut g Gen) write_typeof_functions() {
	g.writeln('')
	g.writeln('// >> typeof() support for sum types')
	for typ in g.table.types {
		if typ.kind == .sum_type {
			sum_info := typ.info as table.SumType
			tidx := g.table.find_type_idx(typ.name)
			g.writeln('char * v_typeof_sumtype_${tidx}(int sidx) { /* ${typ.name} */ ')
			g.writeln('	switch(sidx) {')
			g.writeln('		case $tidx: return "$typ.name";')
			for v in sum_info.variants {
				subtype := g.table.get_type_symbol(v)
				g.writeln('		case $v: return "$subtype.name";')
			}
			g.writeln('		default: return "unknown ${typ.name}";')
			g.writeln('	}')
			g.writeln('}')
		}
	}
	g.writeln('// << typeof() support for sum types')
	g.writeln('')
}

// V type to C type
fn (mut g Gen) typ(t table.Type) string {
	mut styp := g.base_type(t)
	if styp.len == 1 && t == table.t_type && g.cur_generic_type != 0 {
		// T => int etc
		return g.typ(g.cur_generic_type)
	}
	if t.flag_is(.optional) {
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

fn (g &Gen) base_type(t table.Type) string {
	mut styp := g.cc_type(t)
	nr_muls := t.nr_muls()
	if nr_muls > 0 {
		styp += strings.repeat(`*`, nr_muls)
	}
	return styp
}

// TODO this really shouldnt be seperate from typ
// but I(emily) would rather have this generation 
// all unified in one place so that it doesnt break
// if one location changes
fn (g &Gen) optional_type_name(t table.Type) (string, string) {
	base := g.base_type(t)
	mut styp := 'Option_$base'
	if t.is_ptr() {
		styp = styp.replace('*', '_ptr')
	}
	return styp, base
}

fn (g &Gen) optional_type_text(styp, base string) string {
	x := styp // .replace('*', '_ptr')			// handle option ptrs
	// replace void with something else
	size := if base == 'void' {
		'int'
	} else {
		base
	}
	ret := 'struct $x {
 bool ok;
 bool is_none;
 string v_error;
 int ecode;
 byte data[sizeof($size)];
} '
	return ret
}

fn (mut g Gen) register_optional(t table.Type) string {
	// g.typedefs2.writeln('typedef Option $x;')
	styp, base := g.optional_type_name(t)
	if styp !in g.optionals {
		no_ptr := base.replace('*', '_ptr')
		typ := if base == 'void' { 'void*' } else { base }
		g.hotcode_definitions.writeln('typedef struct {
			$typ  data;
			string error;
			int    ecode;
			bool   ok;
			bool   is_none;
		} Option2_$no_ptr;')

		// println(styp)
		g.typedefs2.writeln('typedef struct $styp $styp;')
		g.options.write(g.optional_type_text(styp, base))
		g.options.writeln(';\n')
		g.optionals << styp
	}
	return styp
}

// cc_type returns the Cleaned Concrete Type name, *without ptr*,
// i.e. it's always just Cat, not Cat_ptr:
fn (g &Gen) cc_type(t table.Type) string {
	sym := g.table.get_type_symbol(t)
	mut styp := sym.name.replace('.', '__')
	if styp.starts_with('C__') {
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

//
pub fn (mut g Gen) write_typedef_types() {
	g.typedefs.writeln('
typedef struct {
	void* _object;
	int _interface_idx;
} _Interface;
')
	for typ in g.table.types {
		match typ.kind {
			.alias {
				parent := &g.table.types[typ.parent_idx]
				styp := typ.name.replace('.', '__')
				is_c_parent := parent.name.len > 2 && parent.name[0] == `C` && parent.name[1] ==
					`.`
				parent_styp := if is_c_parent { 'struct ' + parent.name[2..].replace('.', '__') } else { parent.name.replace('.',
						'__') }
				g.type_definitions.writeln('typedef $parent_styp $styp;')
			}
			.array {
				styp := typ.name.replace('.', '__')
				g.type_definitions.writeln('typedef array $styp;')
			}
			.interface_ {
				g.type_definitions.writeln('typedef _Interface ${c_name(typ.name)};')
			}
			.map {
				styp := typ.name.replace('.', '__')
				g.type_definitions.writeln('typedef map $styp;')
			}
			.function {
				info := typ.info as table.FnType
				func := info.func
				sym := g.table.get_type_symbol(func.return_type)
				is_multi := sym.kind == .multi_return
				is_fn_sig := func.name == ''
				not_anon := !info.is_anon
				if !info.has_decl && !is_multi && (not_anon || is_fn_sig) {
					fn_name := if func.language == .c {
						func.name.replace('.', '__')
					} else if info.is_anon {
						typ.name
					} else {
						c_name(func.name)
					}
					g.type_definitions.write('typedef ${g.typ(func.return_type)} (*$fn_name)(')
					for i, arg in func.args {
						g.type_definitions.write(g.typ(arg.typ))
						if i < func.args.len - 1 {
							g.type_definitions.write(',')
						}
					}
					g.type_definitions.writeln(');')
				}
			}
			else {
				continue
			}
		}
	}
}

pub fn (mut g Gen) write_multi_return_types() {
	g.type_definitions.writeln('// multi return structs')
	for typ in g.table.types {
		// sym := g.table.get_type_symbol(typ)
		if typ.kind != .multi_return {
			continue
		}
		name := typ.name.replace('.', '__')
		info := typ.info as table.MultiReturn
		g.type_definitions.writeln('typedef struct {')
		// TODO copy pasta StructDecl
		// for field in struct_info.fields {
		for i, mr_typ in info.types {
			type_name := g.typ(mr_typ)
			g.type_definitions.writeln('\t$type_name arg${i};')
		}
		g.type_definitions.writeln('} $name;\n')
		// g.typedefs.writeln('typedef struct $name $name;')
	}
}

pub fn (mut g Gen) write_variadic_types() {
	if g.variadic_args.size > 0 {
		g.type_definitions.writeln('// variadic structs')
	}
	for type_str, arg_len in g.variadic_args {
		typ := table.Type(type_str.int())
		type_name := g.typ(typ)
		struct_name := 'varg_' + type_name.replace('*', '_ptr')
		g.type_definitions.writeln('struct $struct_name {')
		g.type_definitions.writeln('\tint len;')
		g.type_definitions.writeln('\t$type_name args[$arg_len];')
		g.type_definitions.writeln('};\n')
		g.typedefs.writeln('typedef struct $struct_name $struct_name;')
	}
}

pub fn (g Gen) save() {
}

pub fn (mut g Gen) write(s string) {
	if g.indent > 0 && g.empty_line {
		g.out.write(tabs[g.indent])
		// g.line_len += g.indent * 4
	}
	g.out.write(s)
	g.empty_line = false
}

pub fn (mut g Gen) writeln(s string) {
	if g.indent > 0 && g.empty_line {
		g.out.write(tabs[g.indent])
	}
	g.out.writeln(s)
	g.empty_line = true
}

pub fn (mut g Gen) new_tmp_var() string {
	g.tmp_count++
	return '_t$g.tmp_count'
}

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
	g.indent++
	if g.inside_ternary > 0 {
		g.writeln('(')
	}
	for i, stmt in stmts {
		g.stmt(stmt)
		if g.inside_ternary > 0 && i < stmts.len - 1 {
			g.writeln(',')
		}
	}
	g.indent--
	if g.inside_ternary > 0 {
		g.writeln('')
		g.write(')')
	}
}

fn (mut g Gen) stmt(node ast.Stmt) {
	g.stmt_path_pos << g.out.len
	// println('cgen.stmt()')
	// g.writeln('//// stmt start')
	match node {
		ast.AssertStmt {
			g.gen_assert_stmt(it)
		}
		ast.AssignStmt {
			g.gen_assign_stmt(it)
		}
		ast.Attr {
			g.attr = it.name
			g.writeln('// Attr: [$it.name]')
		}
		ast.Block {
			g.writeln('{')
			g.stmts(it.stmts)
			g.writeln('}')
		}
		ast.BranchStmt {
			// continue or break
			g.write(it.tok.kind.str())
			g.writeln(';')
		}
		ast.ConstDecl {
			// if g.pref.build_mode != .build_module {
			g.const_decl(it)
			// }
		}
		ast.CompIf {
			g.comp_if(it)
		}
		ast.DeferStmt {
			mut defer_stmt := *it
			defer_stmt.ifdef = g.defer_ifdef
			g.defer_stmts << defer_stmt
		}
		ast.EnumDecl {
			enum_name := it.name.replace('.', '__')
			g.typedefs.writeln('typedef enum {')
			mut cur_enum_expr := ''
			mut cur_enum_offset := 0
			for field in it.fields {
				g.typedefs.write('\t${enum_name}_${field.name}')
				if field.has_expr {
					g.typedefs.write(' = ')
					pos := g.out.len
					g.expr(field.expr)
					expr_str := g.out.after(pos)
					g.out.go_back(expr_str.len)
					g.typedefs.write(expr_str)
					cur_enum_expr = expr_str
					cur_enum_offset = 0
				}
				cur_value := if cur_enum_offset > 0 { '${cur_enum_expr}+${cur_enum_offset}' } else { cur_enum_expr }
				g.typedefs.writeln(', // ${cur_value}')
				cur_enum_offset++
			}
			g.typedefs.writeln('} ${enum_name};\n')
		}
		ast.ExprStmt {
			g.expr(it.expr)
			if g.inside_ternary == 0 && !(it.expr is ast.IfExpr) {
				g.writeln(';')
			}
		}
		ast.FnDecl {
			mut skip := false
			pos := g.out.buf.len
			if g.pref.build_mode == .build_module {
				if !it.name.starts_with(g.module_built + '.') {
					// Skip functions that don't have to be generated
					// for this module.
					skip = true
				}
				if g.is_builtin_mod && g.module_built == 'builtin' {
					skip = false
				}
				if !skip {
					println('build module `$g.module_built` fn `$it.name`')
				}
			}
			keep_fn_decl := g.fn_decl
			g.fn_decl = it // &it
			if it.name == 'main' {
				// just remember `it`; main code will be generated in finish()
				g.fn_main = it
			} else {
				if it.name == 'backtrace' || it.name == 'backtrace_symbols' || it.name == 'backtrace_symbols_fd' {
					g.write('\n#ifndef __cplusplus\n')
				}
				g.gen_fn_decl(it)
				if it.name == 'backtrace' || it.name == 'backtrace_symbols' || it.name == 'backtrace_symbols_fd' {
					g.write('\n#endif\n')
				}
			}
			g.fn_decl = keep_fn_decl
			if skip {
				g.out.go_back_to(pos)
			}
			g.writeln('')
			// g.attr has to be reset after each function
			g.attr = ''
		}
		ast.ForCStmt {
			g.write('for (')
			if !it.has_init {
				g.write('; ')
			} else {
				g.stmt(it.init)
			}
			if it.has_cond {
				g.expr(it.cond)
			}
			g.write('; ')
			if it.has_inc {
				g.expr(it.inc)
			}
			g.writeln(') {')
			g.stmts(it.stmts)
			g.writeln('}')
		}
		ast.ForInStmt {
			g.for_in(it)
		}
		ast.ForStmt {
			g.write('while (')
			if it.is_inf {
				g.write('1')
			} else {
				g.expr(it.cond)
			}
			g.writeln(') {')
			g.stmts(it.stmts)
			g.writeln('}')
		}
		ast.GlobalDecl {
			styp := g.typ(it.typ)
			g.definitions.writeln('$styp $it.name; // global')
		}
		ast.GoStmt {
			g.go_stmt(it)
		}
		ast.GotoLabel {
			g.writeln('$it.name: {}')
		}
		ast.GotoStmt {
			g.writeln('goto $it.name;')
		}
		ast.HashStmt {
			// #include etc
			typ := it.val.all_before(' ')
			if typ == 'include' {
				g.includes.writeln('// added by module `$it.mod`:')
				g.includes.writeln('#$it.val')
			}
			if typ == 'define' {
				g.includes.writeln('#$it.val')
			}
		}
		ast.Import {}
		ast.InterfaceDecl {
			// definitions are sorted and added in write_types
		}
		ast.Module {
			g.is_builtin_mod = it.name == 'builtin'
		}
		ast.Return {
			g.write_defer_stmts_when_needed()
			g.write_autofree_stmts_when_needed(it)
			g.return_statement(it)
		}
		ast.StructDecl {
			name := if it.language == .c { it.name.replace('.', '__') } else { c_name(it.name) }
			// g.writeln('typedef struct {')
			// for field in it.fields {
			// field_type_sym := g.table.get_type_symbol(field.typ)
			// g.writeln('\t$field_type_sym.name $field.name;')
			// }
			// g.writeln('} $name;')
			if it.language == .c {
				return
			}
			if it.is_union {
				g.typedefs.writeln('typedef union $name $name;')
			} else {
				g.typedefs.writeln('typedef struct $name $name;')
			}
		}
		ast.TypeDecl {
			g.writeln('// TypeDecl')
		}
		ast.UnsafeStmt {
			g.stmts(it.stmts)
		}
		else {
			verror('cgen.stmt(): unhandled node ' + typeof(node))
		}
	}
	g.stmt_path_pos.delete(g.stmt_path_pos.len - 1)
}

fn (mut g Gen) write_defer_stmts() {
	for defer_stmt in g.defer_stmts {
		g.writeln('// defer')
		if defer_stmt.ifdef.len > 0 {
			g.writeln(defer_stmt.ifdef)
			g.stmts(defer_stmt.stmts)
			g.writeln('')
			g.writeln('#endif')
		} else {
			g.stmts(defer_stmt.stmts)
		}
	}
}

fn (mut g Gen) for_in(it ast.ForInStmt) {
	if it.is_range {
		// `for x in 1..10 {`
		i := if it.val_var == '_' { g.new_tmp_var() } else { c_name(it.val_var) }
		g.write('for (int $i = ')
		g.expr(it.cond)
		g.write('; $i < ')
		g.expr(it.high)
		g.writeln('; $i++) {')
		g.stmts(it.stmts)
		g.writeln('}')
	} else if it.kind == .array {
		// `for num in nums {`
		g.writeln('// FOR IN array')
		styp := g.typ(it.val_type)
		cond_type_is_ptr := it.cond_type.is_ptr()
		atmp := g.new_tmp_var()
		atmp_type := if cond_type_is_ptr { 'array *' } else { 'array' }
		g.write('${atmp_type} ${atmp} = ')
		g.expr(it.cond)
		g.writeln(';')
		i := if it.key_var in ['', '_'] { g.new_tmp_var() } else { it.key_var }
		op_field := if cond_type_is_ptr { '->' } else { '.' }
		g.writeln('for (int $i = 0; $i < ${atmp}${op_field}len; $i++) {')
		if it.val_var != '_' {
			g.writeln('\t$styp ${c_name(it.val_var)} = (($styp*)${atmp}${op_field}data)[$i];')
		}
		g.stmts(it.stmts)
		g.writeln('}')
	} else if it.kind == .map {
		// `for key, val in map {`
		g.writeln('// FOR IN map')
		key_styp := g.typ(it.key_type)
		val_styp := g.typ(it.val_type)
		keys_tmp := 'keys_' + g.new_tmp_var()
		idx := g.new_tmp_var()
		key := if it.key_var in ['', '_'] { g.new_tmp_var() } else { it.key_var }
		zero := g.type_default(it.val_type)
		g.write('array_$key_styp $keys_tmp = map_keys(&')
		g.expr(it.cond)
		g.writeln(');')
		g.writeln('for (int $idx = 0; $idx < ${keys_tmp}.len; $idx++) {')
		g.writeln('\t$key_styp $key = (($key_styp*)${keys_tmp}.data)[$idx];')
		if it.val_var != '_' {
			g.write('\t$val_styp ${c_name(it.val_var)} = (*($val_styp*)map_get3(')
			g.expr(it.cond)
			g.writeln(', $key, &($val_styp[]){ $zero }));')
		}
		g.stmts(it.stmts)
		g.writeln('}')
	} else if it.cond_type.flag_is(.variadic) {
		g.writeln('// FOR IN cond_type/variadic')
		i := if it.key_var in ['', '_'] { g.new_tmp_var() } else { it.key_var }
		styp := g.typ(it.cond_type)
		g.write('for (int $i = 0; $i < ')
		g.expr(it.cond)
		g.writeln('.len; $i++) {')
		g.write('$styp ${c_name(it.val_var)} = ')
		g.expr(it.cond)
		g.writeln('.args[$i];')
		g.stmts(it.stmts)
		g.writeln('}')
	} else if it.kind == .string {
		i := if it.key_var in ['', '_'] { g.new_tmp_var() } else { it.key_var }
		g.write('for (int $i = 0; $i < ')
		g.expr(it.cond)
		g.writeln('.len; $i++) {')
		if it.val_var != '_' {
			g.write('byte ${c_name(it.val_var)} = ')
			g.expr(it.cond)
			g.writeln('.str[$i];')
		}
		g.stmts(it.stmts)
		g.writeln('}')
	}
}

// use instead of expr() when you need to cast to sum type (can add other casts also)
fn (mut g Gen) expr_with_cast(expr ast.Expr, got_type, expected_type table.Type) {
	// cast to sum type
	if expected_type != table.void_type {
		exp_sym := g.table.get_type_symbol(expected_type)
		if exp_sym.kind == .sum_type {
			sum_info := exp_sym.info as table.SumType
			if got_type in sum_info.variants {
				got_sym := g.table.get_type_symbol(got_type)
				got_styp := g.typ(got_type)
				exp_styp := g.typ(expected_type)
				got_idx := got_type.idx()
				g.write('/* sum type cast */ ($exp_styp) {.obj = memdup(&(${got_styp}[]) {')
				g.expr(expr)
				g.write('}, sizeof($got_styp)), .typ = $got_idx /* $got_sym.name */}')
				return
			}
		}
	}
	// Generic dereferencing logic
	expected_sym := g.table.get_type_symbol(expected_type)
	got_is_ptr := got_type.is_ptr()
	expected_is_ptr := expected_type.is_ptr()
	neither_void := table.voidptr_type !in [got_type, expected_type]
	if got_is_ptr && !expected_is_ptr && neither_void && expected_sym.kind !in [.interface_,
		.placeholder
	] {
		got_deref_type := got_type.deref()
		deref_sym := g.table.get_type_symbol(got_deref_type)
		deref_will_match := expected_type in [got_type, got_deref_type, deref_sym.parent_idx]
		got_is_opt := got_type.flag_is(.optional)
		if deref_will_match || got_is_opt {
			g.write('*')
		}
	}
	// no cast
	g.expr(expr)
}

fn (mut g Gen) gen_assert_stmt(a ast.AssertStmt) {
	g.writeln('// assert')
	g.inside_ternary++
	g.write('if (')
	g.expr(a.expr)
	g.write(')')
	g.decrement_inside_ternary()
	s_assertion := a.expr.str().replace('"', "\'")
	mut mod_path := g.file.path
	$if windows {
		mod_path = g.file.path.replace('\\', '\\\\')
	}
	if g.is_test {
		g.writeln('{')
		g.writeln('	g_test_oks++;')
		g.writeln('	cb_assertion_ok( tos_lit("${mod_path}"), ${a.pos.line_nr+1}, tos_lit("assert ${s_assertion}"), tos_lit("${g.fn_decl.name}()") );')
		g.writeln('}else{')
		g.writeln('	g_test_fails++;')
		g.writeln('	cb_assertion_failed( tos_lit("${mod_path}"), ${a.pos.line_nr+1}, tos_lit("assert ${s_assertion}"), tos_lit("${g.fn_decl.name}()") );')
		g.writeln('	exit(1);')
		g.writeln('	// TODO')
		g.writeln('	// Maybe print all vars in a test function if it fails?')
		g.writeln('}')
		return
	}
	g.writeln('{}else{')
	g.writeln('	eprintln( tos_lit("${mod_path}:${a.pos.line_nr+1}: FAIL: fn ${g.fn_decl.name}(): assert $s_assertion"));')
	g.writeln(' v_panic(tos_lit("Assertion failed..."));')
	g.writeln('	exit(1);')
	g.writeln('}')
}

fn (mut g Gen) gen_assign_stmt(assign_stmt ast.AssignStmt) {
	if assign_stmt.is_static {
		g.write('static ')
	}
	mut return_type := table.void_type
	match assign_stmt.right[0] {
		ast.CallExpr { return_type = it.return_type }
		ast.IfExpr { return_type = it.typ }
		ast.MatchExpr { return_type = it.return_type }
		else {}
	}
	// json_test failed w/o this check
	if return_type != table.void_type && return_type != 0 {
		sym := g.table.get_type_symbol(return_type)
		// the left vs. right is ugly and should be removed
		if sym.kind == .multi_return || assign_stmt.left.len > assign_stmt.right.len || assign_stmt.left.len >
			1 {
			// multi return
			// TODO Handle in if_expr
			is_optional := return_type.flag_is(.optional)
			mr_var_name := 'mr_$assign_stmt.pos.pos'
			mr_styp := g.typ(return_type)
			g.write('$mr_styp $mr_var_name = ')
			g.is_assign_rhs = true
			g.expr(assign_stmt.right[0])
			g.is_assign_rhs = false
			if is_optional && assign_stmt.right[0] is ast.CallExpr {
				val := assign_stmt.right[0] as ast.CallExpr
				return_type = val.return_type
				g.or_block(mr_var_name, val.or_block, return_type)
			}
			g.writeln(';')
			for i, ident in assign_stmt.left {
				if ident.kind == .blank_ident {
					continue
				}
				ident_var_info := ident.var_info()
				styp := g.typ(ident_var_info.typ)
				if assign_stmt.op == .decl_assign {
					g.write('$styp ')
				}
				g.expr(ident)
				if is_optional {
					mr_base_styp := g.base_type(return_type)
					g.writeln(' = (*(${mr_base_styp}*)${mr_var_name}.data).arg$i;')
				} else {
					g.writeln(' = ${mr_var_name}.arg$i;')
				}
			}
			return
		}
	}
	if assign_stmt.has_cross_var {
		for ident in assign_stmt.left {
			type_str := g.typ(ident.var_info().typ)
			g.writeln('$type_str _var_$ident.pos.pos = $ident.name;')
		}
	}
	// `a := 1` | `a,b := 1,2`
	for i, ident in assign_stmt.left {
		val := assign_stmt.right[i]
		ident_var_info := ident.var_info()
		styp := g.typ(ident_var_info.typ)
		mut is_call := false
		blank_assign := ident.kind == .blank_ident
		match val {
			ast.CallExpr {
				is_call = true
				return_type = it.return_type
			}
			// TODO: no buffer fiddling
			ast.AnonFn {
				if blank_assign {
					g.write('{')
				}
				ret_styp := g.typ(it.decl.return_type)
				g.write('$ret_styp (*$ident.name) (')
				def_pos := g.definitions.len
				g.fn_args(it.decl.args, it.decl.is_variadic)
				g.definitions.go_back(g.definitions.len - def_pos)
				g.write(') = ')
				g.expr(*it)
				g.writeln(';')
				if blank_assign {
					g.write('}')
				}
				continue
			}
			else {}
		}
		g.is_assign_rhs = true
		if blank_assign {
			if is_call {
				g.expr(val)
			} else {
				g.gen_default_init_value(val)
				g.write('{$styp _ = ')
				g.expr(val)
				g.writeln(';}')
			}
		} else {
			right_sym := g.table.get_type_symbol(assign_stmt.right_types[i])
			mut is_fixed_array_init := false
			mut has_val := false
			is_fixed_array_init, has_val = g.gen_default_init_value(val)
			is_inside_ternary := g.inside_ternary != 0
			cur_line := if is_inside_ternary {
				g.register_ternary_name(ident.name)
				g.empty_line = false
				g.go_before_ternary()
			} else {
				''
			}
			is_decl := assign_stmt.op == .decl_assign
			if right_sym.kind == .function {
				if is_inside_ternary {
					g.out.write(tabs[g.indent - g.inside_ternary])
				}
				func := right_sym.info as table.FnType
				ret_styp := g.typ(func.func.return_type)
				g.write('$ret_styp (*${g.get_ternary_name(ident.name)}) (')
				def_pos := g.definitions.len
				g.fn_args(func.func.args, func.func.is_variadic)
				g.definitions.go_back(g.definitions.len - def_pos)
				g.write(')')
			} else {
				if is_decl {
					if is_inside_ternary {
						g.out.write(tabs[g.indent - g.inside_ternary])
					}
					g.write('$styp ')
				}
				g.ident(ident)
			}
			if is_inside_ternary {
				g.write(';\n$cur_line')
				g.out.write(tabs[g.indent])
				g.ident(ident)
			}
			if g.autofree && right_sym.kind in [.array, .string] {
				if g.gen_clone_assignment(val, right_sym, true) {
					g.writeln(';')
					// g.expr_var_name = ''
					return
				}
			}
			if is_fixed_array_init {
				if has_val {
					g.write(' = ')
					g.expr(val)
				} else {
					g.write(' = {0}')
				}
			} else {
				g.write(' = ')
				if is_decl {
					g.expr(val)
				} else {
					if assign_stmt.has_cross_var {
						g.gen_cross_tmp_variable(assign_stmt.left, val)
					} else {
						g.expr_with_cast(val, assign_stmt.left_types[i], ident_var_info.typ)
					}
				}
			}
		}
		g.is_assign_rhs = false
		if g.inside_ternary == 0 {
			g.writeln(';')
		}
	}
}

fn (mut g Gen) gen_cross_tmp_variable(idents []ast.Ident, val ast.Expr) {
	match val {
		ast.Ident {
			mut has_var := false
			for ident in idents {
				if it.name == ident.name {
					g.write('_var_${ident.pos.pos}')
					has_var = true
					break
				}
			}
			if !has_var {
				g.expr(val)
			}
		}
		ast.InfixExpr {
			g.gen_cross_tmp_variable(idents, it.left)
			g.write(it.op.str())
			g.gen_cross_tmp_variable(idents, it.right)
		}
		ast.PrefixExpr {
			g.write(it.op.str())
			g.gen_cross_tmp_variable(idents, it.right)
		}
		ast.PostfixExpr {
			g.gen_cross_tmp_variable(idents, it.expr)
			g.write(it.op.str())
		}
		else {
			g.expr(val)
		}
	}
}

fn (mut g Gen) gen_default_init_value(val ast.Expr) (bool, bool) {
	mut is_fixed_array_init := false
	mut has_val := false
	match val {
		ast.ArrayInit {
			is_fixed_array_init = it.is_fixed
			has_val = it.has_val
			elem_type_str := g.typ(it.elem_type)
			if it.has_default {
				g.write('$elem_type_str _val_$it.pos.pos = ')
				g.expr(it.default_expr)
				g.writeln(';')
			} else if it.has_len && it.elem_type == table.string_type {
				g.writeln('$elem_type_str _val_$it.pos.pos = tos_lit("");')
			}
		}
		ast.StructInit {
			for field in it.fields {
				g.gen_default_init_value(field.expr)
			}
		}
		else {}
	}
	return is_fixed_array_init, has_val
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
	mut is_ident := false
	match val {
		ast.Ident { is_ident = true }
		ast.SelectorExpr { is_ident = true }
		else { return false }
	}
	if g.autofree && right_sym.kind == .array && is_ident {
		// `arr1 = arr2` => `arr1 = arr2.clone()`
		if add_eq {
			g.write('=')
		}
		g.write(' array_clone_static(')
		g.expr(val)
		g.write(')')
	} else if g.autofree && right_sym.kind == .string && is_ident {
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

fn (mut g Gen) autofree_scope_vars(pos int) string {
	// eprintln('> free_scope_vars($pos)')
	mut freeing_code := ''
	scope := g.file.scope.innermost(pos)
	for _, obj in scope.objects {
		match obj {
			ast.Var {
				// if var.typ == 0 {
				// // TODO why 0?
				// continue
				// }
				v := *it
				is_optional := v.typ.flag_is(.optional)
				if is_optional {
					// TODO: free optionals
					continue
				}
				freeing_code += g.autofree_variable(v)
			}
			else {}
		}
	}
	return freeing_code
}

fn (g &Gen) autofree_variable(v ast.Var) string {
	sym := g.table.get_type_symbol(v.typ)
	// if v.name.contains('output2') {
	// eprintln('   > var name: ${v.name:-20s} | is_arg: ${v.is_arg.str():6} | var type: ${int(v.typ):8} | type_name: ${sym.name:-33s}')
	// }
	if sym.kind == .array {
		return g.autofree_var_call('array_free', v)
	}
	if sym.kind == .string {
		// Don't free simple string literals.
		match v.expr {
			ast.StringLiteral {
				return '// str literal\n'
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
		return g.autofree_var_call('string_free', v)
	}
	if sym.has_method('free') {
		return g.autofree_var_call(c_name(sym.name) + '_free', v)
	}
	return ''
}

fn (g &Gen) autofree_var_call(free_fn_name string, v ast.Var) string {
	if v.is_arg {
		// fn args should not be autofreed
		return ''
	}
	if v.typ.is_ptr() {
		return '\t${free_fn_name}($v.name); // autofreed ptr var\n'
	} else {
		return '\t${free_fn_name}(&$v.name); // autofreed var\n'
	}
}

fn (mut g Gen) expr(node ast.Expr) {
	// println('cgen expr() line_nr=$node.pos.line_nr')
	match node {
		ast.AnonFn {
			// TODO: dont fiddle with buffers
			pos := g.out.len
			def_pos := g.definitions.len
			g.stmt(it.decl)
			fn_body := g.out.after(pos)
			g.out.go_back(fn_body.len)
			g.definitions.go_back(g.definitions.len - def_pos)
			g.definitions.write(fn_body)
			fsym := g.table.get_type_symbol(it.typ)
			g.write('&${fsym.name}')
		}
		ast.ArrayInit {
			g.array_init(it)
		}
		ast.AsCast {
			g.as_cast(it)
		}
		ast.AssignExpr {
			g.assign_expr(it)
		}
		ast.Assoc {
			g.assoc(it)
		}
		ast.BoolLiteral {
			g.write(it.val.str())
		}
		ast.CallExpr {
			g.call_expr(it)
		}
		ast.CastExpr {
			// g.write('/*cast*/')
			if g.is_amp {
				// &Foo(0) => ((Foo*)0)
				g.out.go_back(1)
			}
			sym := g.table.get_type_symbol(it.typ)
			if sym.kind == .string && !it.typ.is_ptr() {
				// `string(x)` needs `tos()`, but not `&string(x)
				// `tos(str, len)`, `tos2(str)`
				if it.has_arg {
					g.write('tos((byteptr)')
				} else {
					g.write('tos2((byteptr)')
				}
				g.expr(it.expr)
				expr_sym := g.table.get_type_symbol(it.expr_type)
				if expr_sym.kind == .array {
					// if we are casting an array, we need to add `.data`
					g.write('.data')
				}
				if it.has_arg {
					// len argument
					g.write(', ')
					g.expr(it.arg)
				}
				g.write(')')
			} else if sym.kind == .sum_type {
				g.expr_with_cast(it.expr, it.expr_type, it.typ)
			} else {
				// styp := g.table.Type_to_str(it.typ)
				styp := g.typ(it.typ)
				// g.write('($styp)(')
				g.write('(($styp)(')
				// if g.is_amp {
				// g.write('*')
				// }
				// g.write(')(')
				g.expr(it.expr)
				g.write('))')
			}
		}
		ast.CharLiteral {
			g.write("'$it.val'")
		}
		ast.ComptimeCall {
			g.write('/*c*/')
		}
		ast.ConcatExpr {
			g.concat_expr(it)
		}
		ast.EnumVal {
			// g.write('${it.mod}${it.enum_name}_$it.val')
			styp := g.typ(it.typ)
			g.write('${styp}_$it.val')
		}
		ast.FloatLiteral {
			g.write(it.val)
		}
		ast.Ident {
			g.ident(it)
		}
		ast.IfExpr {
			g.if_expr(it)
		}
		ast.IfGuardExpr {
			g.write('/* guard */')
		}
		ast.IndexExpr {
			g.index_expr(it)
		}
		ast.InfixExpr {
			g.infix_expr(it)
		}
		ast.IntegerLiteral {
			if it.val.starts_with('0o') {
				g.write('0')
				g.write(it.val[2..])
			} else {
				g.write(it.val) // .int().str())
			}
		}
		ast.MatchExpr {
			g.match_expr(it)
		}
		ast.MapInit {
			key_typ_str := g.typ(it.key_type)
			value_typ_str := g.typ(it.value_type)
			size := it.vals.len
			if size > 0 {
				g.write('new_map_init($size, sizeof($value_typ_str), _MOV((${key_typ_str}[$size]){')
				for expr in it.keys {
					g.expr(expr)
					g.write(', ')
				}
				g.write('}), _MOV((${value_typ_str}[$size]){')
				for expr in it.vals {
					g.expr(expr)
					g.write(', ')
				}
				g.write('}))')
			} else {
				g.write('new_map_1(sizeof($value_typ_str))')
			}
		}
		ast.None {
			g.write('opt_none()')
		}
		ast.ParExpr {
			g.write('(')
			g.expr(it.expr)
			g.write(')')
		}
		ast.PostfixExpr {
			g.expr(it.expr)
			g.write(it.op.str())
		}
		ast.PrefixExpr {
			if it.op == .amp {
				g.is_amp = true
			}
			// g.write('/*pref*/')
			g.write(it.op.str())
			// g.write('(')
			g.expr(it.right)
			// g.write(')')
			g.is_amp = false
		}
		ast.SizeOf {
			mut styp := it.type_name
			if it.type_name == '' {
				styp = g.typ(it.typ)
			} else {
				sym := g.table.get_type_symbol(it.typ)
				if sym.kind == .struct_ {
					info := sym.info as table.Struct
					if !info.is_typedef {
						styp = 'struct ' + styp
					}
				}
			}
			/*
			if styp.starts_with('C__') {
				styp = styp[3..]
			}
			*/
			g.write('sizeof($styp)')
		}
		ast.StringLiteral {
			if it.is_raw {
				escaped_val := it.val.replace_each(['"', '\\"', '\\', '\\\\'])
				g.write('tos_lit("$escaped_val")')
				return
			}
			escaped_val := it.val.replace_each(['"', '\\"', '\r\n', '\\n', '\n', '\\n'])
			if g.is_c_call || it.language == .c {
				// In C calls we have to generate C strings
				// `C.printf("hi")` => `printf("hi");`
				g.write('"$escaped_val"')
			} else {
				// TODO calculate the literal's length in V, it's a bit tricky with all the
				// escape characters.
				// Clang and GCC optimize `strlen("lorem ipsum")` to `11`
				// g.write('tos4("$escaped_val", strlen("$escaped_val"))')
				// g.write('tos4("$escaped_val", $it.val.len)')
				// g.write('_SLIT("$escaped_val")')
				g.write('tos_lit("$escaped_val")')
				// g.write('tos_lit("$escaped_val")')
			}
		}
		ast.StringInterLiteral {
			g.string_inter_literal(it)
		}
		ast.StructInit {
			// `user := User{name: 'Bob'}`
			g.struct_init(it)
		}
		ast.SelectorExpr {
			g.expr(it.expr)
			if it.expr_type.is_ptr() {
				g.write('->')
			} else {
				// g.write('. /*typ=  $it.expr_type */') // ${g.typ(it.expr_type)} /')
				g.write('.')
			}
			if it.expr_type == 0 {
				verror('cgen: SelectorExpr | expr_type: 0 | it.expr: `${it.expr}` | field: `$it.field_name` | file: $g.file.path | line: $it.pos.line_nr')
			}
			g.write(c_name(it.field_name))
		}
		ast.Type {
			// match sum Type
			// g.write('/* Type */')
			type_idx := it.typ.idx()
			sym := g.table.get_type_symbol(it.typ)
			g.write('$type_idx /* $sym.name */')
		}
		ast.TypeOf {
			g.typeof_expr(it)
		}
		else {
			// #printf("node=%d\n", node.typ);
			println(term.red('cgen.expr(): bad node ' + typeof(node)))
		}
	}
}

fn (mut g Gen) typeof_expr(node ast.TypeOf) {
	sym := g.table.get_type_symbol(node.expr_type)
	if sym.kind == .sum_type {
		// When encountering a .sum_type, typeof() should be done at runtime,
		// because the subtype of the expression may change:
		sum_type_idx := node.expr_type.idx()
		g.write('tos3( /* ${sym.name} */ v_typeof_sumtype_${sum_type_idx}( (')
		g.expr(node.expr)
		g.write(').typ ))')
	} else if sym.kind == .array_fixed {
		fixed_info := sym.info as table.ArrayFixed
		typ_name := g.table.get_type_name(fixed_info.elem_type)
		g.write('tos_lit("[$fixed_info.size]${typ_name}")')
	} else if sym.kind == .function {
		info := sym.info as table.FnType
		fn_info := info.func
		mut repr := 'fn ('
		for i, arg in fn_info.args {
			if i > 0 {
				repr += ', '
			}
			repr += g.table.get_type_name(arg.typ)
		}
		repr += ')'
		if fn_info.return_type != table.void_type {
			repr += ' ${g.table.get_type_name(fn_info.return_type)}'
		}
		g.write('tos_lit("$repr")')
	} else {
		g.write('tos_lit("${sym.name}")')
	}
}

fn (mut g Gen) enum_expr(node ast.Expr) {
	match node {
		ast.EnumVal { g.write(it.val) }
		else { g.expr(node) }
	}
}

fn (mut g Gen) assign_expr(node ast.AssignExpr) {
	// g.write('/*assign_expr*/')
	mut is_call := false
	mut or_block := ast.OrExpr{}
	mut return_type := table.void_type
	match node.val {
		ast.CallExpr {
			is_call = true
			or_block = it.or_block
			return_type = it.return_type
		}
		else {}
	}
	gen_or := is_call && return_type.flag_is(.optional)
	tmp_opt := if gen_or { g.new_tmp_var() } else { '' }
	if gen_or {
		rstyp := g.typ(return_type)
		g.write('/*q*/ $rstyp $tmp_opt = ')
	}
	g.is_assign_rhs = true
	if ast.expr_is_blank_ident(node.left) {
		if is_call {
			g.expr(node.val)
		} else {
			// g.write('{${g.typ(node.left_type)} _ = ')
			g.write('{')
			g.expr(node.val)
			g.writeln(';}')
		}
	} else {
		g.is_assign_lhs = true
		if node.right_type.flag_is(.optional) {
			g.right_is_opt = true
		}
		mut str_add := false
		if node.left_type == table.string_type_idx && node.op == .plus_assign {
			// str += str2 => `str = string_add(str, str2)`
			g.expr(node.left)
			g.write(' = /*f*/string_add(')
			str_add = true
		}
		right_sym := g.table.get_type_symbol(node.right_type)
		if right_sym.kind == .array_fixed && node.op == .assign {
			right := node.val as ast.ArrayInit
			for j, expr in right.exprs {
				g.expr(node.left)
				g.write('[$j] = ')
				g.expr(expr)
				g.writeln(';')
			}
		} else {
			g.assign_op = node.op
			if !gen_or {
				// Don't need to generate `var = ` in `or {}` expressions, since we are doing
				// `Option_X tmp = ...; var = *(X*)tmp.data;`
				g.expr(node.left)
				// arr[i] = val => `array_set(arr, i, val)`, not `array_get(arr, i) = val`
				if !g.is_array_set && !str_add {
					g.write(' $node.op.str() ')
				} else if str_add {
					g.write(', ')
				}
			}
			g.is_assign_lhs = false
			// right_sym := g.table.get_type_symbol(node.right_type)
			// left_sym := g.table.get_type_symbol(node.left_type)
			mut cloned := false
			// !g.is_array_set
			if g.autofree && right_sym.kind in [.array, .string] {
				if g.gen_clone_assignment(node.val, right_sym, false) {
					cloned = true
				}
			}
			if !cloned {
				g.expr_with_cast(node.val, node.right_type, node.left_type)
			}
			if g.is_array_set {
				g.write(' })')
				g.is_array_set = false
			} else if str_add {
				g.write(')')
			}
		}
		g.right_is_opt = false
	}
	if gen_or {
		// g.write('/*777 $tmp_opt*/')
		g.or_block(tmp_opt, or_block, return_type)
		unwrapped_type_str := g.typ(return_type.set_flag(.unset))
		ident := node.left as ast.Ident
		if ident.kind != .blank_ident && ident.info is ast.IdentVar {
			ident_var := ident.info as ast.IdentVar
			if ident_var.is_optional {
				// var is already an optional, just copy the value
				// `var = tmp;`
				g.write('\n$ident.name = $tmp_opt')
			} else {
				// var = *(X*)tmp.data;`
				g.write('\n$ident.name = *($unwrapped_type_str*)${tmp_opt}.data')
			}
		}
		// g.expr(node.left)
	}
	g.is_assign_rhs = false
}

fn (mut g Gen) infix_expr(node ast.InfixExpr) {
	// println('infix_expr() op="$node.op.str()" line_nr=$node.pos.line_nr')
	// g.write('/*infix*/')
	// if it.left_type == table.string_type_idx {
	// g.write('/*$node.left_type str*/')
	// }
	// string + string, string == string etc
	// g.infix_op = node.op
	left_type := if node.left_type == table.t_type { g.cur_generic_type } else { node.left_type }
	left_sym := g.table.get_type_symbol(left_type)
	if node.op == .key_is {
		g.is_expr(node)
		return
	}
	right_sym := g.table.get_type_symbol(node.right_type)
	if left_type == table.ustring_type_idx && node.op != .key_in && node.op != .not_in {
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
	} else if left_type == table.string_type_idx && node.op != .key_in && node.op != .not_in {
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
	} else if node.op in [.eq, .ne] && left_sym.kind == .array && right_sym.kind == .array {
		styp := g.table.value_type(left_type)
		ptr_typ := g.typ(left_type).split('_')[1]
		if ptr_typ !in g.array_fn_definitions {
			sym := g.table.get_type_symbol(left_sym.array_info().elem_type)
			g.generate_array_equality_fn(ptr_typ, styp, sym)
		}
		if node.op == .eq {
			g.write('${ptr_typ}_arr_eq(')
		} else if node.op == .ne {
			g.write('!${ptr_typ}_arr_eq(')
		}
		g.expr(node.left)
		g.write(', ')
		g.expr(node.right)
		g.write(')')
	} else if node.op in [.key_in, .not_in] {
		if node.op == .not_in {
			g.write('!')
		}
		if right_sym.kind == .array {
			match node.right {
				ast.ArrayInit {
					// `a in [1,2,3]` optimization => `a == 1 || a == 2 || a == 3`
					// avoids an allocation
					// g.write('/*in opt*/')
					g.write('(')
					g.in_optimization(node.left, it)
					g.write(')')
					return
				}
				else {}
			}
			styp := g.typ(g.table.mktyp(left_type))
			g.write('_IN($styp, ')
			g.expr(node.left)
			g.write(', ')
			g.expr(node.right)
			g.write(')')
		} else if right_sym.kind == .map {
			g.write('_IN_MAP(')
			g.expr(node.left)
			g.write(', ')
			g.expr(node.right)
			g.write(')')
		} else if right_sym.kind == .string {
			g.write('string_contains(')
			g.expr(node.right)
			g.write(', ')
			g.expr(node.left)
			g.write(')')
		}
	} else if node.op == .left_shift && g.table.get_type_symbol(left_type).kind == .array {
		// arr << val
		tmp := g.new_tmp_var()
		sym := g.table.get_type_symbol(left_type)
		info := sym.info as table.Array
		if right_sym.kind == .array && info.elem_type != node.right_type {
			// push an array => PUSH_MANY, but not if pushing an array to 2d array (`[][]int << []int`)
			g.write('_PUSH_MANY(&')
			g.expr(node.left)
			g.write(', (')
			g.expr_with_cast(node.right, node.right_type, left_type)
			styp := g.typ(left_type)
			g.write('), $tmp, $styp)')
		} else {
			// push a single element
			elem_type_str := g.typ(info.elem_type)
			g.write('array_push(&')
			g.expr(node.left)
			g.write(', _MOV(($elem_type_str[]){ ')
			elem_sym := g.table.get_type_symbol(info.elem_type)
			if elem_sym.kind == .interface_ && node.right_type != info.elem_type {
				g.interface_call(node.right_type, info.elem_type)
			}
			g.expr_with_cast(node.right, node.right_type, info.elem_type)
			if elem_sym.kind == .interface_ && node.right_type != info.elem_type {
				g.write(')')
			}
			g.write(' }))')
		}
	} else if (left_type == node.right_type) && left_type.is_float() && node.op in [.eq, .ne] {
		// floats should be compared with epsilon
		if left_type == table.f64_type_idx {
			if node.op == .eq {
				g.write('f64_eq(')
			} else {
				g.write('f64_ne(')
			}
		} else {
			if node.op == .eq {
				g.write('f32_eq(')
			} else {
				g.write('f32_ne(')
			}
		}
		g.expr(node.left)
		g.write(',')
		g.expr(node.right)
		g.write(')')
	} else if node.op in [.plus, .minus, .mul, .div, .mod] && (left_sym.name[0].is_capital() ||
		left_sym.name.contains('.')) && left_sym.kind != .alias || left_sym.kind == .alias && (left_sym.info as table.Alias).language ==
		.c {
		// !left_sym.is_number() {
		g.write(g.typ(left_type))
		g.write('_')
		g.write(util.replace_op(node.op.str()))
		g.write('(')
		g.expr(node.left)
		g.write(', ')
		g.expr(node.right)
		g.write(')')
	} else {
		need_par := node.op in [.amp, .pipe, .xor] // `x & y == 0` => `(x & y) == 0` in C
		if need_par {
			g.write('(')
		}
		g.expr(node.left)
		g.write(' $node.op.str() ')
		g.expr(node.right)
		if need_par {
			g.write(')')
		}
	}
}

fn (mut g Gen) match_expr(node ast.MatchExpr) {
	// println('match expr typ=$it.expr_type')
	// TODO
	if node.cond_type == 0 {
		g.writeln('// match 0')
		return
	}
	is_expr := (node.is_expr && node.return_type != table.void_type) || g.inside_ternary >
		0
	if is_expr {
		g.inside_ternary++
		// g.write('/* EM ret type=${g.typ(node.return_type)}		expected_type=${g.typ(node.expected_type)}  */')
	}
	type_sym := g.table.get_type_symbol(node.cond_type)
	mut tmp := ''
	if type_sym.kind != .void {
		tmp = g.new_tmp_var()
	}
	// styp := g.typ(node.expr_type)
	// g.write('$styp $tmp = ')
	// g.expr(node.cond)
	// g.writeln(';') // $it.blocks.len')
	// mut sum_type_str = ''
	for j, branch in node.branches {
		is_last := j == node.branches.len - 1
		if branch.is_else || node.is_expr && is_last {
			if node.branches.len > 1 {
				if is_expr {
					// TODO too many branches. maybe separate ?: matches
					g.write(' : ')
				} else {
					g.writeln('else {')
				}
			}
		} else {
			if j > 0 {
				if is_expr {
					g.write(' : ')
				} else {
					g.write('else ')
				}
			}
			if is_expr {
				g.write('(')
			} else {
				g.write('if (')
			}
			for i, expr in branch.exprs {
				if node.is_sum_type {
					g.expr(node.cond)
					sym := g.table.get_type_symbol(node.cond_type)
					// branch_sym := g.table.get_type_symbol(branch.typ)
					if sym.kind == .sum_type {
						g.write('.typ == ')
					} else if sym.kind == .interface_ {
						// g.write('._interface_idx == _${sym.name}_${branch_sym} ')
						g.write('._interface_idx == ')
					}
				} else if type_sym.kind == .string {
					g.write('string_eq(')
					//
					g.expr(node.cond)
					g.write(', ')
					// g.write('string_eq($tmp, ')
				} else {
					g.expr(node.cond)
					g.write(' == ')
					// g.write('$tmp == ')
				}
				g.expr(expr)
				if type_sym.kind == .string {
					g.write(')')
				}
				if i < branch.exprs.len - 1 {
					g.write(' || ')
				}
			}
			if is_expr {
				g.write(') ? ')
			} else {
				g.writeln(') {')
			}
		}
		// g.writeln('/* M sum_type=$node.is_sum_type is_expr=$node.is_expr exp_type=${g.typ(node.expected_type)}*/')
		if node.is_sum_type && branch.exprs.len > 0 && !node.is_expr {
			// The first node in expr is an ast.Type
			// Use it to generate `it` variable.
			first_expr := branch.exprs[0]
			match first_expr {
				ast.Type {
					it_type := g.typ(it.typ)
					// g.writeln('$it_type* it = ($it_type*)${tmp}.obj; // ST it')
					g.write('\t$it_type* it = ($it_type*)')
					g.expr(node.cond)
					g.writeln('.obj; // ST it')
				}
				else {
					verror('match sum type')
				}
			}
		}
		g.stmts(branch.stmts)
		if g.inside_ternary == 0 && node.branches.len > 1 {
			g.write('}')
		}
	}
	if is_expr {
		g.decrement_inside_ternary()
	}
}

fn (mut g Gen) ident(node ast.Ident) {
	if node.name == 'lld' {
		return
	}
	if node.name.starts_with('C.') {
		g.write(node.name[2..].replace('.', '__'))
		return
	}
	if node.kind == .constant && !node.name.starts_with('g_') {
		// TODO globals hack
		g.write('_const_')
	}
	mut name := c_name(node.name)
	if node.info is ast.IdentVar {
		ident_var := node.info as ast.IdentVar
		// x ?int
		// `x = 10` => `x.data = 10` (g.right_is_opt == false)
		// `x = new_opt()` => `x = new_opt()` (g.right_is_opt == true)
		// `println(x)` => `println(*(int*)x.data)`
		if ident_var.is_optional && !(g.is_assign_lhs && g.right_is_opt) {
			g.write('/*opt*/')
			styp := g.base_type(ident_var.typ)
			g.write('(*($styp*)${name}.data)')
			return
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

fn (mut g Gen) if_expr(node ast.IfExpr) {
	if node.is_expr || g.inside_ternary != 0 {
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
	for i, branch in node.branches {
		if i == 0 {
			match branch.cond {
				ast.IfGuardExpr {
					is_guard = true
					g.write('{ /* if guard */ ${g.typ(it.expr_type)} $it.var_name = ')
					g.expr(it.expr)
					g.writeln(';')
					g.writeln('if (${it.var_name}.ok) {')
				}
				else {
					g.write('if (')
					g.expr(branch.cond)
					g.writeln(') {')
				}
			}
		} else if i < node.branches.len - 1 || !node.has_else {
			g.write('} else if (')
			g.expr(branch.cond)
			g.writeln(') {')
		} else if i == node.branches.len - 1 && node.has_else {
			g.writeln('} else {')
		}
		g.stmts(branch.stmts)
	}
	if is_guard {
		g.write('}')
	}
	g.writeln('}')
}

fn (mut g Gen) index_expr(node ast.IndexExpr) {
	// TODO else doesn't work with sum types
	mut is_range := false
	match node.index {
		ast.RangeExpr {
			sym := g.table.get_type_symbol(node.left_type)
			is_range = true
			if sym.kind == .string {
				g.write('string_substr(')
				g.expr(node.left)
			} else if sym.kind == .array {
				g.write('array_slice(')
				g.expr(node.left)
			} else if sym.kind == .array_fixed {
				// Convert a fixed array to V array when doing `fixed_arr[start..end]`
				g.write('array_slice(new_array_from_c_array(_ARR_LEN(')
				g.expr(node.left)
				g.write('), _ARR_LEN(')
				g.expr(node.left)
				g.write('), sizeof(')
				g.expr(node.left)
				g.write('[0]), ')
				g.expr(node.left)
				g.write(')')
			} else {
				g.expr(node.left)
			}
			g.write(', ')
			if it.has_low {
				g.expr(it.low)
			} else {
				g.write('0')
			}
			g.write(', ')
			if it.has_high {
				g.expr(it.high)
			} else {
				g.expr(node.left)
				g.write('.len')
			}
			g.write(')')
			return
		}
		else {}
	}
	if !is_range {
		sym := g.table.get_type_symbol(node.left_type)
		left_is_ptr := node.left_type.is_ptr()
		if node.left_type.flag_is(.variadic) {
			g.expr(node.left)
			g.write('.args')
			g.write('[')
			g.expr(node.index)
			g.write(']')
		} else if sym.kind == .array {
			info := sym.info as table.Array
			elem_type_str := g.typ(info.elem_type)
			// `vals[i].field = x` is an exception and requires `array_get`:
			// `(*(Val*)array_get(vals, i)).field = x;`
			is_selector := node.left is ast.SelectorExpr
			if g.is_assign_lhs && !is_selector && node.is_setter {
				g.is_array_set = true
				g.write('array_set(')
				if !left_is_ptr {
					g.write('&')
				}
				g.expr(node.left)
				g.write(', ')
				g.expr(node.index)
				mut need_wrapper := true
				/*
				match node.right {
					ast.EnumVal, ast.Ident {
						// `&x` is enough for variables and enums
						// `&(Foo[]){ ... }` is only needed for function calls and literals
						need_wrapper = false
					}
					else {}
				}
				*/
				if need_wrapper {
					g.write(', &($elem_type_str[]) { \n')
				} else {
					g.write(', &')
				}
				// `x[0] *= y`
				if g.assign_op != .assign && g.assign_op in token.assign_tokens {
					// TODO move this
					g.write('*($elem_type_str*)array_get(')
					if left_is_ptr {
						g.write('*')
					}
					g.expr(node.left)
					g.write(', ')
					g.expr(node.index)
					g.write(') ')
					op := match g.assign_op {
						.mult_assign { '*' }
						.plus_assign { '+' }
						.minus_assign { '-' }
						.div_assign { '/' }
						.xor_assign { '^' }
						.mod_assign { '%' }
						.or_assign { '|' }
						.and_assign { '&' }
						.left_shift_assign { '<<' }
						.right_shift_assign { '>>' }
						else { '' }
					}
					g.write(op)
				}
			} else {
				g.write('(*($elem_type_str*)array_get(')
				if left_is_ptr {
					g.write('*')
				}
				g.expr(node.left)
				g.write(', ')
				g.expr(node.index)
				g.write('))')
			}
		} else if sym.kind == .map {
			info := sym.info as table.Map
			elem_type_str := g.typ(info.value_type)
			if g.is_assign_lhs && !g.is_array_set {
				g.is_array_set = true
				g.write('map_set(')
				if !left_is_ptr {
					g.write('&')
				}
				g.expr(node.left)
				g.write(', ')
				g.expr(node.index)
				g.write(', &($elem_type_str[]) { \n')
			} else {
				/*
				g.write('(*($elem_type_str*)map_get2(')
				g.expr(node.left)
					g.write(', ')
					g.expr(node.index)
					g.write('))')
				*/
				zero := g.type_default(info.value_type)
				g.write('(*($elem_type_str*)map_get3(')
				g.expr(node.left)
				g.write(', ')
				g.expr(node.index)
				g.write(', &($elem_type_str[]){ $zero }))\n')
			}
		} else if sym.kind == .string && !node.left_type.is_ptr() {
			g.write('string_at(')
			g.expr(node.left)
			g.write(', ')
			g.expr(node.index)
			g.write(')')
		} else {
			g.expr(node.left)
			g.write('[')
			g.expr(node.index)
			g.write(']')
		}
	}
}

[inline]
fn (g Gen) expr_is_multi_return_call(expr ast.Expr) bool {
	match expr {
		ast.CallExpr { return g.table.get_type_symbol(it.return_type).kind == .multi_return }
		else { return false }
	}
}

fn (mut g Gen) return_statement(node ast.Return) {
	if g.fn_decl.name == 'main' {
		g.writeln('return 0;')
		return
	}
	// got to do a correct check for multireturn
	sym := g.table.get_type_symbol(g.fn_decl.return_type)
	fn_return_is_multi := sym.kind == .multi_return
	fn_return_is_optional := g.fn_decl.return_type.flag_is(.optional)
	// handle promoting none/error/function returning 'Option'
	if fn_return_is_optional {
		optional_none := node.exprs[0] is ast.None
		mut is_regular_option := g.typ(node.types[0]) == 'Option'
		if optional_none || is_regular_option {
			tmp := g.new_tmp_var()
			g.write('/*opt promotion*/ Option $tmp = ')
			g.expr_with_cast(node.exprs[0], node.types[0], g.fn_decl.return_type)
			g.write(';')

			styp := g.typ(g.fn_decl.return_type)
			g.writeln('return *($styp*)&$tmp;')
			return
		}
	}
	// regular cases
	if fn_return_is_multi { // not_optional_none { //&& !fn_return_is_optional {
		// typ_sym := g.table.get_type_symbol(g.fn_decl.return_type)
		// mr_info := typ_sym.info as table.MultiReturn
		mut styp := ''

		mut opt_tmp := ''
		mut opt_type := ''

		if fn_return_is_optional {
			opt_type = g.typ(g.fn_decl.return_type)
			// Create a tmp for this option
			opt_tmp = g.new_tmp_var()
			g.write('$opt_type $opt_tmp;')
			styp = g.base_type(g.fn_decl.return_type)

			g.write('opt_ok2(&($styp/*X*/[]) { ')
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
				tmp := g.new_tmp_var()
				s := g.go_before_stmt(0)
				expr_styp := g.typ(c.return_type)
				g.write('$expr_styp $tmp=')
				g.expr(expr)
				g.writeln(';')
				multi_unpack += g.go_before_stmt(0)
				g.write(s)

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
				g.write(',')
			}
		}
		g.write('}')
		if fn_return_is_optional {
			g.writeln(' }, (OptionBase*)(&$opt_tmp), sizeof($styp));')
			g.write('return $opt_tmp')
		}
		// Make sure to add our unpacks
		g.insert_before_stmt(multi_unpack)
	} else if node.exprs.len >= 1 {
		// normal return
		return_sym := g.table.get_type_symbol(node.types[0])
		// `return opt_ok(expr)` for functions that expect an optional
		if fn_return_is_optional && !node.types[0].flag_is(.optional) && return_sym.name !=
			'Option' {
			styp := g.base_type(g.fn_decl.return_type)
			opt_type := g.typ(g.fn_decl.return_type)
			// Create a tmp for this option
			opt_tmp := g.new_tmp_var()
			g.write('$opt_type $opt_tmp;')
			g.write('/*:)$return_sym.name*/opt_ok2(&($styp[]) { ')
			if !g.fn_decl.return_type.is_ptr() && node.types[0].is_ptr() {
				// Automatic Dereference for optional
				g.write('*')
			}
			for i, expr in node.exprs {
				g.expr(expr)
				if i < node.exprs.len - 1 {
					g.write(', ')
				}
			}
			g.writeln(' }, (OptionBase*)(&$opt_tmp), sizeof($styp));')
			g.writeln('return $opt_tmp;')
			return
		}
		g.write('return ')
		cast_interface := sym.kind == .interface_ && node.types[0] != g.fn_decl.return_type
		if cast_interface {
			g.interface_call(node.types[0], g.fn_decl.return_type)
		}
		g.expr_with_cast(node.exprs[0], node.types[0], g.fn_decl.return_type)
		if cast_interface {
			g.write(')')
		}
	} else {
		g.write('return')
	}
	g.writeln(';')
}

fn (mut g Gen) const_decl(node ast.ConstDecl) {
	for field in node.fields {
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
			ast.CharLiteral {
				g.const_decl_simple_define(name, val)
			}
			ast.FloatLiteral {
				g.const_decl_simple_define(name, val)
			}
			ast.IntegerLiteral {
				g.const_decl_simple_define(name, val)
			}
			ast.ArrayInit {
				if it.is_fixed {
					styp := g.typ(it.typ)
					g.definitions.writeln('$styp _const_$name = $val; // fixed array const')
				} else {
					g.const_decl_init_later(name, val, field.typ)
				}
			}
			ast.StringLiteral {
				g.definitions.writeln('string _const_$name; // a string literal, inited later')
				if g.pref.build_mode != .build_module {
					g.stringliterals.writeln('\t_const_$name = $val;')
				}
			}
			else {
				g.const_decl_init_later(name, val, field.typ)
			}
		}
	}
}

fn (mut g Gen) const_decl_simple_define(name, val string) {
	// Simple expressions should use a #define
	// so that we don't pollute the binary with unnecessary global vars
	// Do not do this when building a module, otherwise the consts
	// will not be accessible.
	g.definitions.write('#define _const_$name ')
	g.definitions.writeln(val)
}

fn (mut g Gen) const_decl_init_later(name, val string, typ table.Type) {
	// Initialize more complex consts in `void _vinit(){}`
	// (C doesn't allow init expressions that can't be resolved at compile time).
	styp := g.typ(typ)
	//
	cname := '_const_$name'
	g.definitions.writeln('$styp $cname; // inited later')
	g.inits.writeln('\t$cname = $val;')
	if g.pref.autofree {
		if styp.starts_with('array_') {
			g.cleanups.writeln('\tarray_free(&$cname);')
		}
		if styp == 'string' {
			g.cleanups.writeln('\tstring_free(&$cname);')
		}
	}
}

fn (mut g Gen) go_back_out(n int) {
	g.out.go_back(n)
}

fn (mut g Gen) struct_init(struct_init ast.StructInit) {
	skip_init := ['strconv__ftoa__Uf32', 'strconv__ftoa__Uf64', 'strconv__Float64u', 'struct stat',
		'struct addrinfo'
	]
	styp := g.typ(struct_init.typ)
	if styp in skip_init {
		g.go_back_out(3)
		return
	}
	sym := g.table.get_type_symbol(struct_init.typ)
	is_amp := g.is_amp
	g.is_amp = false // reset the flag immediately so that other struct inits in this expr are handled correctly
	if is_amp {
		g.out.go_back(1) // delete the `&` already generated in `prefix_expr()
		g.write('($styp*)memdup(&($styp){')
	} else {
		g.writeln('($styp){')
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
	// User set fields
	mut initialized := false
	for i, field in struct_init.fields {
		inited_fields[field.name] = i
		if sym.kind != .struct_ {
			field_name := c_name(field.name)
			g.write('\t.$field_name = ')
			field_type_sym := g.table.get_type_symbol(field.typ)
			mut cloned := false
			if g.autofree && field_type_sym.kind in [.array, .string] {
				g.write('/*clone1*/')
				if g.gen_clone_assignment(field.expr, field_type_sym, false) {
					cloned = true
				}
			}
			if !cloned {
				if field.expected_type.is_ptr() && !field.typ.is_ptr() && !field.typ.is_number() {
					g.write('/* autoref */&')
				}
				g.expr_with_cast(field.expr, field.typ, field.expected_type)
			}
			g.writeln(',')
			initialized = true
		}
	}
	// The rest of the fields are zeroed.
	mut nr_info_fields := 0
	if sym.kind == .struct_ {
		info := sym.info as table.Struct
		if info.is_union && struct_init.fields.len > 1 {
			verror('union must not have more than 1 initializer')
		}
		nr_info_fields = info.fields.len
		for field in info.fields {
			if field.name in inited_fields {
				sfield := struct_init.fields[inited_fields[field.name]]
				field_name := c_name(sfield.name)
				g.write('\t.$field_name = ')
				field_type_sym := g.table.get_type_symbol(sfield.typ)
				mut cloned := false
				if g.autofree && field_type_sym.kind in [.array, .string] {
					g.write('/*clone1*/')
					if g.gen_clone_assignment(sfield.expr, field_type_sym, false) {
						cloned = true
					}
				}
				if !cloned {
					if sfield.expected_type.is_ptr() && !sfield.typ.is_ptr() && !sfield.typ.is_number() {
						g.write('/* autoref */&')
					}
					g.expr_with_cast(sfield.expr, sfield.typ, sfield.expected_type)
				}
				g.writeln(',')
				initialized = true
				continue
			}
			if info.is_union {
				// unions thould have exactly one explicit initializer
				continue
			}
			if field.typ.flag_is(.optional) {
				// TODO handle/require optionals in inits
				continue
			}
			field_name := c_name(field.name)
			g.write('\t.$field_name = ')
			if field.has_default_expr {
				g.expr(ast.fe2ex(field.default_expr))
			} else {
				g.write(g.type_default(field.typ))
			}
			g.writeln(',')
			initialized = true
		}
	}
	// if struct_init.fields.len == 0 && info.fields.len == 0 {
	if !initialized {
		g.write('\n#ifndef __cplusplus\n0\n#endif\n')
	}
	g.write('}')
	if is_amp {
		g.write(', sizeof($styp))')
	}
}

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

fn (mut g Gen) generate_array_equality_fn(ptr_typ string, styp table.Type, sym &table.TypeSymbol) {
	g.array_fn_definitions << ptr_typ
	g.definitions.writeln('bool ${ptr_typ}_arr_eq(array_${ptr_typ} a, array_${ptr_typ} b) {')
	g.definitions.writeln('\tif (a.len != b.len) {')
	g.definitions.writeln('\t\treturn false;')
	g.definitions.writeln('\t}')
	g.definitions.writeln('\tfor (int i = 0; i < a.len; i++) {')
	if styp == table.string_type_idx {
		g.definitions.writeln('\t\tif (string_ne(*((${ptr_typ}*)((byte*)a.data+(i*a.element_size))), *((${ptr_typ}*)((byte*)b.data+(i*b.element_size))))) {')
	} else if sym.kind == .struct_ {
		g.definitions.writeln('\t\tif (memcmp((byte*)a.data+(i*a.element_size), (byte*)b.data+(i*b.element_size), a.element_size)) {')
	} else {
		g.definitions.writeln('\t\tif (*((${ptr_typ}*)((byte*)a.data+(i*a.element_size))) != *((${ptr_typ}*)((byte*)b.data+(i*b.element_size)))) {')
	}
	g.definitions.writeln('\t\t\treturn false;')
	g.definitions.writeln('\t\t}')
	g.definitions.writeln('\t}')
	g.definitions.writeln('\treturn true;')
	g.definitions.writeln('}')
}

fn verror(s string) {
	util.verror('cgen error', s)
}

fn (mut g Gen) write_init_function() {
	if g.pref.is_liveshared {
		return
	}
	fn_vinit_start_pos := g.out.len
	needs_constructor := g.pref.is_shared && g.pref.os != .windows
	if needs_constructor {
		g.writeln('__attribute__ ((constructor))')
		g.writeln('void _vinit() {')
		g.writeln('static bool once = false; if (once) {return;} once = true;')
	} else {
		g.writeln('void _vinit() {')
	}
	if g.pref.autofree {
		// Pre-allocate the string buffer
		// TODO make it configurable
		g.writeln('g_str_buf = malloc(1024 * 1000);')
	}
	g.writeln('\tbuiltin_init();')
	g.writeln('\tvinit_string_literals();')
	g.writeln(g.inits.str())
	for mod_name in g.table.imports {
		init_fn_name := '${mod_name}.init'
		if _ := g.table.find_fn(init_fn_name) {
			mod_c_name := mod_name.replace('.', '__')
			init_fn_c_name := '${mod_c_name}__init'
			g.writeln('\t${init_fn_c_name}();')
		}
	}
	g.writeln('}')
	if g.pref.printfn_list.len > 0 && '_vinit' in g.pref.printfn_list {
		println(g.out.after(fn_vinit_start_pos))
	}
	if g.autofree {
		fn_vcleanup_start_pos := g.out.len
		g.writeln('void _vcleanup() {')
		// g.writeln('puts("cleaning up...");')
		g.writeln(g.cleanups.str())
		g.writeln('}')
		if g.pref.printfn_list.len > 0 && '_vcleanup' in g.pref.printfn_list {
			println(g.out.after(fn_vcleanup_start_pos))
		}
	}
}

const (
	builtins = ['string', 'array', 'KeyValue', 'DenseArray', 'map', 'Option']
)

fn (mut g Gen) write_builtin_types() {
	mut builtin_types := []table.TypeSymbol{} // builtin types
	// builtin types need to be on top
	// everything except builtin will get sorted
	for builtin_name in builtins {
		builtin_types << g.table.types[g.table.type_idxs[builtin_name]]
	}
	g.write_types(builtin_types)
}

// C struct definitions, ordered
// Sort the types, make sure types that are referenced by other types
// are added before them.
fn (mut g Gen) write_sorted_types() {
	mut types := []table.TypeSymbol{} // structs that need to be sorted
	for typ in g.table.types {
		if typ.name !in builtins {
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
		name := typ.name.replace('.', '__')
		match typ.info {
			table.Struct {
				info := typ.info as table.Struct
				// TODO avoid buffer manip
				start_pos := g.type_definitions.len
				if info.is_union {
					g.type_definitions.writeln('union $name {')
				} else {
					g.type_definitions.writeln('struct $name {')
				}
				if info.fields.len > 0 {
					for field in info.fields {
						// Some of these structs may want to contain
						// optionals that may not be defined at this point
						// if this is the case then we are going to 
						// buffer manip out in front of the struct
						// write the optional in and then continue
						if field.typ.flag_is(.optional) {
							// Dont use g.typ() here becuase it will register
							// optional and we dont want that
							last_text := g.type_definitions.after(start_pos).clone()
							g.type_definitions.go_back_to(start_pos)
							styp, base := g.optional_type_name(field.typ)
							g.optionals << styp
							g.typedefs2.writeln('typedef struct $styp $styp;')
							g.type_definitions.writeln('${g.optional_type_text(styp, base)};')
							g.type_definitions.write(last_text)
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
				// table.Alias, table.SumType { TODO
			}
			table.SumType {
				g.type_definitions.writeln('')
				g.type_definitions.writeln('// Sum type $name = ')
				for sv in it.variants {
					g.type_definitions.writeln('//          | ${sv:4d} = ${g.typ(sv):-20s}')
				}
				g.type_definitions.writeln('typedef struct {')
				g.type_definitions.writeln('    void* obj;')
				g.type_definitions.writeln('    int typ;')
				g.type_definitions.writeln('} $name;')
				g.type_definitions.writeln('')
			}
			table.ArrayFixed {
				// .array_fixed {
				styp := typ.name.replace('.', '__')
				// array_fixed_char_300 => char x[300]
				mut fixed := styp[12..]
				len := styp.after('_')
				fixed = fixed[..fixed.len - len.len - 1]
				if fixed.starts_with('C__') {
					fixed = fixed[3..]
				}
				g.type_definitions.writeln('typedef $fixed $styp [$len];')
				// }
			}
			else {}
		}
	}
}

// sort structs by dependant fields
fn (g Gen) sort_structs(typesa []table.TypeSymbol) []table.TypeSymbol {
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
		match t.info {
			table.ArrayFixed {
				dep := g.table.get_type_symbol(it.elem_type).name
				if dep in type_names {
					field_deps << dep
				}
			}
			table.Struct {
				info := t.info as table.Struct
				// if info.is_interface {
				// continue
				// }
				for field in info.fields {
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
		verror('cgen.sort_structs(): the following structs form a dependency cycle:\n' + dep_graph_sorted.display_cycles() +
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

fn (mut g Gen) string_inter_literal(node ast.StringInterLiteral) {
	if g.pref.autofree {
		g.write('_STR_TMP("')
	} else {
		g.write('_STR("')
	}
	// Build the string with %
	mut fieldwidths := []int{}
	mut specs := []byte{}
	mut end_string := false
	for i, val in node.vals {
		escaped_val := val.replace_each(['"', '\\"', '\r\n', '\\n', '\n', '\\n', '%', '%%'])
		if i >= node.exprs.len {
			if escaped_val.len > 0 {
				end_string = true
				if !g.pref.autofree {
					g.write('\\000')
				}
				g.write(escaped_val)
			}
			continue
		}
		g.write(escaped_val)
		sym := g.table.get_type_symbol(node.expr_types[i])
		sfmt := node.expr_fmts[i]
		mut fspec := `_` // placeholder
		mut fmt := '' // field width and precision
		if sfmt.len > 0 {
			// analyze and validate format specifier
			if sfmt[sfmt.len - 1] in [`E`, `F`, `G`, `e`, `f`, `g`,
				`d`, `u`, `x`, `X`, `o`, `c`, `s`, `p`] {
				fspec = sfmt[sfmt.len - 1]
			}
			fmt = if fspec == `_` {
				sfmt[1..sfmt.len]
			} else {
				sfmt[1..sfmt.len - 1]
			}
		}
		if fspec == `_` { // set default representation for type if still missing
			if node.expr_types[i].is_float() {
				fspec = `g`
			} else if node.expr_types[i].is_signed() || node.expr_types[i].is_any_int() {
				fspec = `d`
			} else if node.expr_types[i].is_unsigned() {
				fspec = `u`
			} else if node.expr_types[i].is_pointer() {
				fspec = `p`
			} else if node.expr_types[i] in [table.string_type, table.bool_type] || sym.kind in
				[.enum_, .array, .array_fixed, .struct_, .map] || g.typ(node.expr_types[i]).starts_with('Option') ||
				sym.has_method('str') {
				fspec = `s`
			} else {
				// default to int - TODO: should better be checked
				fspec = `d`
			}
		}
		fields := fmt.split('.')
		// validate format
		// only floats should have precision specifier
		if fields.len > 2 || fields.len == 2 && !(node.expr_types[i].is_float()) || node.expr_types[i].is_signed() &&
			fspec !in [`d`, `c`, `x`, `X`, `o`] || node.expr_types[i].is_unsigned() && fspec !in [`u`, `x`,
			`X`, `o`, `c`] || node.expr_types[i].is_any_int() && fspec !in [`d`, `c`, `x`, `X`,
			`o`, `u`,
			`x`, `X`, `o`] || node.expr_types[i].is_float() && fspec !in [`E`, `F`, `G`, `e`,
			`f`, `g`] || node.expr_types[i].is_pointer() && fspec !in [`p`, `x`, `X`] {
			verror('illegal format specifier ${fspec:c} for type ${g.table.get_type_name(node.expr_types[i])}')
		}
		// make sure that format paramters are valid numbers
		for j, f in fields {
			for k, c in f {
				if (c < `0` || c > `9`) && !(j == 0 && k == 0 && (node.expr_types[i].is_number() &&
					c == `+` || c == `-`)) {
					verror('illegal character ${c:c} in format specifier ${fmt}')
				}
			}
		}
		specs << fspec
		fieldwidths << if fields.len == 0 {
			0
		} else {
			strconv.atoi(fields[0])
		}
		// write correct format specifier to intermediate string
		g.write('%')
		if fspec == `s` {
			if fields.len == 0 || strconv.atoi(fields[0]) == 0 {
				g.write('.*s')
			} else {
				g.write('*.*s')
			}
		} else if node.expr_types[i].is_float() || node.expr_types[i].is_pointer() {
			g.write('$fmt${fspec:c}')
		} else if node.expr_types[i].is_pointer() {
			if fspec == `p` {
				g.write('${fmt}p')
			} else {
				g.write('${fmt}"PRI${fspec:c}PTR"')
			}
		} else if node.expr_types[i].is_int() {
			if fspec == `c` {
				if node.expr_types[i].idx() in [table.i64_type_idx, table.f64_type_idx] {
					verror('64 bit integer types cannot be interpolated as character')
				} else {
					g.write('${fmt}c')
				}
			} else {
				g.write('${fmt}"PRI${fspec:c}')
				if node.expr_types[i] in [table.i8_type, table.byte_type] {
					g.write('8')
				} else if node.expr_types[i] in [table.i16_type, table.u16_type] {
					g.write('16')
				} else if node.expr_types[i] in [table.i64_type, table.u64_type] {
					g.write('64')
				} else {
					g.write('32')
				}
				g.write('"')
			}
		} else {
			// TODO: better check this case
			g.write('${fmt}"PRId32"')
		}
		if i < node.exprs.len - 1 && !g.pref.autofree {
			g.write('\\000')
		}
	}
	num_string_parts := if end_string { node.exprs.len + 1 } else { node.exprs.len }
	g.write('", $num_string_parts, ')
	// Build args
	for i, expr in node.exprs {
		if node.expr_types[i] == table.string_type {
			g.expr(expr)
		} else if node.expr_types[i] == table.bool_type {
			g.expr(expr)
			g.write(' ? _SLIT("true") : _SLIT("false")')
		} else if node.expr_types[i].is_number() || node.expr_types[i].is_pointer() || specs[i] ==
			`d` {
			if node.expr_types[i].is_signed() && specs[i] in [`x`, `X`, `o`] {
				// convert to unsigned first befors C's integer propagation strikes
				if node.expr_types[i] == table.i8_type {
					g.write('(byte)(')
				} else if node.expr_types[i] == table.i16_type {
					g.write('(u16)(')
				} else if node.expr_types[i] == table.int_type {
					g.write('(u32)(')
				} else {
					g.write('(u64)(')
				}
				g.expr(expr)
				g.write(')')
			} else {
				g.expr(expr)
			}
		} else if specs[i] == `s` {
			sym := g.table.get_type_symbol(node.expr_types[i])
			sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
			if node.expr_types[i].flag_is(.variadic) {
				str_fn_name := g.gen_str_for_type(node.expr_types[i])
				g.write('${str_fn_name}(')
				g.expr(expr)
				g.write(')')
			} else if sym.kind == .enum_ {
				is_var := match node.exprs[i] {
					ast.SelectorExpr { true }
					ast.Ident { true }
					else { false }
				}
				if is_var {
					str_fn_name := g.gen_str_for_type(node.expr_types[i])
					g.write('${str_fn_name}(')
					g.enum_expr(expr)
					g.write(')')
				} else {
					g.write('tos_lit("')
					g.enum_expr(expr)
					g.write('")')
				}
			} else if sym_has_str_method || sym.kind in [.array, .array_fixed, .map, .struct_] {
				is_p := node.expr_types[i].is_ptr()
				val_type := if is_p { node.expr_types[i].deref() } else { node.expr_types[i] }
				str_fn_name := g.gen_str_for_type(val_type)
				if is_p && str_method_expects_ptr {
					g.write('string_add(_SLIT("&"), ${str_fn_name}(  (')
				}
				if is_p && !str_method_expects_ptr {
					g.write('string_add(_SLIT("&"), ${str_fn_name}( *(')
				}
				if !is_p && !str_method_expects_ptr {
					g.write('${str_fn_name}(  ')
				}
				if !is_p && str_method_expects_ptr {
					g.write('${str_fn_name}( &')
				}
				g.expr(expr)
				if sym.kind == .struct_ && !sym_has_str_method {
					if is_p {
						g.write('),0))')
					} else {
						g.write(',0)')
					}
				} else {
					if is_p {
						g.write(')))')
					} else {
						g.write(')')
					}
				}
			} else if g.typ(node.expr_types[i]).starts_with('Option') {
				str_fn_name := 'OptionBase_str'
				g.write('${str_fn_name}(*(OptionBase*)&')
				g.expr(expr)
				g.write(')')
			} else {
				verror('cannot convert to string')
			}
		} else {
			g.expr(expr)
		}
		if specs[i] == `s` && fieldwidths[i] != 0 {
			g.write(', ${fieldwidths[i]}')
		}
		if i < node.exprs.len - 1 {
			g.write(', ')
		}
	}
	g.write(')')
}

// `nums.map(it % 2 == 0)`
fn (mut g Gen) gen_map(node ast.CallExpr) {
	tmp := g.new_tmp_var()
	s := g.go_before_stmt(0)
	// println('filter s="$s"')
	ret_typ := g.typ(node.return_type)
	// inp_typ := g.typ(node.receiver_type)
	ret_sym := g.table.get_type_symbol(node.return_type)
	inp_sym := g.table.get_type_symbol(node.receiver_type)
	ret_info := ret_sym.info as table.Array
	ret_elem_type := g.typ(ret_info.elem_type)
	inp_info := inp_sym.info as table.Array
	inp_elem_type := g.typ(inp_info.elem_type)
	if inp_sym.kind != .array {
		verror('map() requires an array')
	}
	g.writeln('')
	g.write('int ${tmp}_len = ')
	g.expr(node.left)
	g.writeln('.len;')
	g.writeln('$ret_typ $tmp = __new_array(0, ${tmp}_len, sizeof($ret_elem_type));')
	i := g.new_tmp_var()
	g.writeln('for (int $i = 0; $i < ${tmp}_len; $i++) {')
	g.write('\t$inp_elem_type it = (($inp_elem_type*) ')
	g.expr(node.left)
	g.writeln('.data)[$i];')
	g.write('\t$ret_elem_type ti = ')
	match node.args[0].expr {
		ast.Ident {
			if it.kind == .function {
				g.writeln('${it.name}(it)')
			} else {
				g.expr(node.args[0].expr)
			}
		}
		else {
			g.expr(node.args[0].expr)
		}
	}
	g.writeln(';')
	g.writeln('\tarray_push(&$tmp, &ti);')
	g.writeln('}')
	g.write(s)
	g.write(tmp)
}

// `nums.filter(it % 2 == 0)`
fn (mut g Gen) gen_filter(node ast.CallExpr) {
	tmp := g.new_tmp_var()
	s := g.go_before_stmt(0)
	// println('filter s="$s"')
	sym := g.table.get_type_symbol(node.return_type)
	if sym.kind != .array {
		verror('filter() requires an array')
	}
	info := sym.info as table.Array
	styp := g.typ(node.return_type)
	elem_type_str := g.typ(info.elem_type)
	g.write('\nint ${tmp}_len = ')
	g.expr(node.left)
	g.writeln('.len;')
	g.writeln('$styp $tmp = __new_array(0, ${tmp}_len, sizeof($elem_type_str));')
	g.writeln('for (int i = 0; i < ${tmp}_len; i++) {')
	g.write('  $elem_type_str it = (($elem_type_str*) ')
	g.expr(node.left)
	g.writeln('.data)[i];')
	g.write('if (')
	match node.args[0].expr {
		ast.Ident {
			if it.kind == .function {
				g.writeln('${node.args[0]}(it)')
			} else {
				g.expr(node.args[0].expr)
			}
		}
		else {
			g.expr(node.args[0].expr)
		}
	}
	g.writeln(') array_push(&$tmp, &it); \n }')
	g.write(s)
	g.write(' ')
	g.write(tmp)
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

// If user is accessing the return value eg. in assigment, pass the variable name.
// If the user is not using the optional return value. We need to pass a temp var
// to access its fields (`.ok`, `.error` etc)
// `os.cp(...)` => `Option bool tmp = os__cp(...); if (!tmp.ok) { ... }`
// Returns the type of the last stmt
fn (mut g Gen) or_block(var_name string, or_block ast.OrExpr, return_type table.Type) {
	cvar_name := c_name(var_name)
	mr_styp := g.base_type(return_type)
	g.writeln(';') // or')
	g.writeln('if (!${cvar_name}.ok) {')
	if or_block.kind == .block {
		g.writeln('\tstring err = ${cvar_name}.v_error;')
		g.writeln('\tint errcode = ${cvar_name}.ecode;')
		stmts := or_block.stmts
		if stmts.len > 0 && stmts[or_block.stmts.len - 1] is ast.ExprStmt && (stmts[stmts.len -
			1] as ast.ExprStmt).typ != table.void_type {
			g.indent++
			for i, stmt in stmts {
				if i == stmts.len - 1 {
					expr_stmt := stmt as ast.ExprStmt
					g.stmt_path_pos << g.out.len
					g.write('*(${mr_styp}*) ${cvar_name}.data = ')
					is_opt_call := expr_stmt.expr is ast.CallExpr && expr_stmt.typ.flag_is(.optional)
					if is_opt_call {
						g.write('*(${mr_styp}*) ')
					}
					g.expr(expr_stmt.expr)
					if is_opt_call {
						g.write('.data')
					}
					if g.inside_ternary == 0 && !(expr_stmt.expr is ast.IfExpr) {
						g.writeln(';')
					}
					g.stmt_path_pos.delete(g.stmt_path_pos.len - 1)
				} else {
					g.stmt(stmt)
				}
			}
			g.indent--
		} else {
			g.stmts(stmts)
		}
	} else if or_block.kind == .propagate {
		if g.file.mod.name == 'main' && g.cur_fn.name == 'main' {
			if g.pref.is_debug {
				paline, pafile, pamod, pafn := g.panic_debug_info(or_block.pos)
				g.writeln('panic_debug($paline, tos3("$pafile"), tos3("$pamod"), tos3("$pafn"), ${cvar_name}.v_error );')
			} else {
				g.writeln('\tv_panic(${cvar_name}.v_error);')
			}
		} else {
			// Now that option types are distinct we need a cast here
			styp := g.typ(g.fn_decl.return_type)
			g.writeln('\treturn *($styp *)&$cvar_name;')
		}
	}
	g.write('}')
}

fn (mut g Gen) type_of_call_expr(node ast.Expr) string {
	match node {
		ast.CallExpr { return g.typ(it.return_type) }
		else { return typeof(node) }
	}
	return ''
}

// `a in [1,2,3]` => `a == 1 || a == 2 || a == 3`
fn (mut g Gen) in_optimization(left ast.Expr, right ast.ArrayInit) {
	is_str := right.elem_type == table.string_type
	for i, array_expr in right.exprs {
		if is_str {
			g.write('string_eq(')
		}
		g.expr(left)
		if is_str {
			g.write(', ')
		} else {
			g.write(' == ')
		}
		g.expr(array_expr)
		if is_str {
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
		else { 'bad op $name' }
	}
}

fn (mut g Gen) comp_if_to_ifdef(name string, is_comptime_optional bool) string {
	match name {
		// platforms/os-es:
		'windows' {
			return '_WIN32'
		}
		'mac' {
			return '__APPLE__'
		}
		'macos' {
			return '__APPLE__'
		}
		'mach' {
			return '__MACH__'
		}
		'darwin' {
			return '__DARWIN__'
		}
		'hpux' {
			return '__HPUX__'
		}
		'gnu' {
			return '__GNU__'
		}
		'qnx' {
			return '__QNX__'
		}
		'linux' {
			return '__linux__'
		}
		'freebsd' {
			return '__FreeBSD__'
		}
		'openbsd' {
			return '__OpenBSD__'
		}
		'netbsd' {
			return '__NetBSD__'
		}
		'bsd' {
			return '__BSD__'
		}
		'dragonfly' {
			return '__DragonFly__'
		}
		'android' {
			return '__ANDROID__'
		}
		'solaris' {
			return '__sun'
		}
		'haiku' {
			return '__haiku__'
		}
		'linux_or_macos' {
			return ''
		}
		//
		'js' {
			return '_VJS'
		}
		// compilers:
		'tinyc' {
			return '__TINYC__'
		}
		'clang' {
			return '__clang__'
		}
		'mingw' {
			return '__MINGW32__'
		}
		'msvc' {
			return '_MSC_VER'
		}
		'cplusplus' {
			return '__cplusplus'
		}
		// other:
		'debug' {
			return '_VDEBUG'
		}
		'glibc' {
			return '__GLIBC__'
		}
		'prealloc' {
			return 'VPREALLOC'
		}
		'no_bounds_checking' {
			return 'CUSTOM_DEFINE_no_bounds_checking'
		}
		'x64' {
			return 'TARGET_IS_64BIT'
		}
		'x32' {
			return 'TARGET_IS_32BIT'
		}
		'little_endian' {
			return 'TARGET_ORDER_IS_LITTLE'
		}
		'big_endian' {
			return 'TARGET_ORDER_IS_BIG'
		}
		else {
			if is_comptime_optional || g.pref.compile_defines_all.len > 0 && name in g.pref.compile_defines_all {
				return 'CUSTOM_DEFINE_${name}'
			}
			verror('bad os ifdef name "$name"')
		}
	}
	// verror('bad os ifdef name "$name"')
	return ''
}

[inline]
fn c_name(name_ string) string {
	name := name_.replace('.', '__')
	if name in c_reserved {
		return 'v_$name'
	}
	return name
}

fn (g Gen) type_default(typ table.Type) string {
	sym := g.table.get_type_symbol(typ)
	if sym.kind == .array {
		elem_sym := g.typ(sym.array_info().elem_type)
		mut elem_type_str := elem_sym.replace('.', '__')
		if elem_type_str.starts_with('C__') {
			elem_type_str = elem_type_str[3..]
		}
		return '__new_array(0, 1, sizeof($elem_type_str))'
	}
	if sym.kind == .map {
		value_type_str := g.typ(sym.map_info().value_type)
		return 'new_map_1(sizeof($value_type_str))'
	}
	// Always set pointers to 0
	if typ.is_ptr() {
		return '0'
	}
	// User struct defined in another module.
	// if typ.contains('__') {
	if sym.kind == .struct_ {
		return '{0}'
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
		'string' { return '(string){.str=""}' }
		'rune' { return '0' }
		else {}
	}
	return '{0}'
	// TODO this results in
	// error: expected a field designator, such as '.field = 4'
	// - Empty ee= (Empty) { . =  {0}  } ;
	/*
	return match typ {
	'bool'{ '0'}
	'string'{ 'tos_lit("")'}
	'i8'{ '0'}
	'i16'{ '0'}
	'i64'{ '0'}
	'u16'{ '0'}
	'u32'{ '0'}
	'u64'{ '0'}
	'byte'{ '0'}
	'int'{ '0'}
	'rune'{ '0'}
	'f32'{ '0.0'}
	'f64'{ '0.0'}
	'byteptr'{ '0'}
	'voidptr'{ '0'}
	else { '{0} '}
}
	*/
}

pub fn (mut g Gen) write_tests_main() {
	g.definitions.writeln('int g_test_oks = 0;')
	g.definitions.writeln('int g_test_fails = 0;')
	$if windows {
		g.writeln('int wmain() {')
	} $else {
		g.writeln('int main() {')
	}
	g.writeln('\t_vinit();')
	g.writeln('')
	all_tfuncs := g.get_all_test_function_names()
	if g.pref.is_stats {
		g.writeln('\tBenchedTests bt = start_testing(${all_tfuncs.len}, tos_lit("$g.pref.path"));')
	}
	for t in all_tfuncs {
		g.writeln('')
		if g.pref.is_stats {
			g.writeln('\tBenchedTests_testing_step_start(&bt, tos_lit("$t"));')
		}
		g.writeln('\t${t}();')
		if g.pref.is_stats {
			g.writeln('\tBenchedTests_testing_step_end(&bt);')
		}
	}
	g.writeln('')
	if g.pref.is_stats {
		g.writeln('\tBenchedTests_end_testing(&bt);')
	}
	g.writeln('')
	if g.autofree {
		g.writeln('\t_vcleanup();')
	}
	g.writeln('\treturn g_test_fails > 0;')
	g.writeln('}')
}

fn (g Gen) get_all_test_function_names() []string {
	mut tfuncs := []string{}
	mut tsuite_begin := ''
	mut tsuite_end := ''
	for _, f in g.table.fns {
		if f.name == 'testsuite_begin' {
			tsuite_begin = f.name
			continue
		}
		if f.name == 'testsuite_end' {
			tsuite_end = f.name
			continue
		}
		if f.name.starts_with('test_') {
			tfuncs << f.name
			continue
		}
		// What follows is for internal module tests
		// (they are part of a V module, NOT in main)
		if f.name.contains('.test_') {
			tfuncs << f.name
			continue
		}
		if f.name.ends_with('.testsuite_begin') {
			tsuite_begin = f.name
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
	mut all_tfuncs_c := []string{}
	for f in all_tfuncs {
		all_tfuncs_c << f.replace('.', '__')
	}
	return all_tfuncs_c
}

fn (g Gen) is_importing_os() bool {
	return 'os' in g.table.imports
}

fn (mut g Gen) comp_if(it ast.CompIf) {
	ifdef := g.comp_if_to_ifdef(it.val, it.is_opt)
	if it.is_not {
		g.writeln('\n// \$if !${it.val} {\n#ifndef ' + ifdef)
	} else {
		g.writeln('\n// \$if  ${it.val} {\n#ifdef ' + ifdef)
	}
	// NOTE: g.defer_ifdef is needed for defers called witin an ifdef
	// in v1 this code would be completely excluded
	g.defer_ifdef = if it.is_not {
		'\n#ifndef ' + ifdef
	} else {
		'\n#ifdef ' + ifdef
	}
	// println('comp if stmts $g.file.path:$it.pos.line_nr')
	g.stmts(it.stmts)
	g.defer_ifdef = ''
	if it.has_else {
		g.writeln('\n#else')
		g.defer_ifdef = if it.is_not {
			'\n#ifdef ' + ifdef
		} else {
			'\n#ifndef ' + ifdef
		}
		g.stmts(it.else_stmts)
		g.defer_ifdef = ''
	}
	g.writeln('\n// } ${it.val}\n#endif\n')
}

fn (mut g Gen) go_stmt(node ast.GoStmt) {
	tmp := g.new_tmp_var()
	// x := node.call_expr as ast.CallEpxr // TODO
	match node.call_expr {
		ast.CallExpr {
			mut name := it.name // .replace('.', '__')
			if it.is_method {
				receiver_sym := g.table.get_type_symbol(it.receiver_type)
				name = receiver_sym.name + '_' + name
			}
			name = name.replace('.', '__')
			g.writeln('// go')
			wrapper_struct_name := 'thread_arg_' + name
			wrapper_fn_name := name + '_thread_wrapper'
			arg_tmp_var := 'arg_' + tmp
			g.writeln('$wrapper_struct_name *$arg_tmp_var = malloc(sizeof(thread_arg_$name));')
			if it.is_method {
				g.write('${arg_tmp_var}->arg0 = ')
				g.expr(it.left)
				g.writeln(';')
			}
			for i, arg in it.args {
				g.write('${arg_tmp_var}->arg${i+1} = ')
				g.expr(arg.expr)
				g.writeln(';')
			}
			if g.pref.os == .windows {
				g.writeln('CreateThread(0,0, (LPTHREAD_START_ROUTINE)$wrapper_fn_name, $arg_tmp_var, 0,0);')
			} else {
				g.writeln('pthread_t thread_$tmp;')
				g.writeln('pthread_create(&thread_$tmp, NULL, (void*)$wrapper_fn_name, $arg_tmp_var);')
			}
			g.writeln('// endgo\n')
			// Register the wrapper type and function
			if name in g.threaded_fns {
				return
			}
			g.type_definitions.writeln('\ntypedef struct $wrapper_struct_name {')
			if it.is_method {
				styp := g.typ(it.receiver_type)
				g.type_definitions.writeln('\t$styp arg0;')
			}
			for i, arg in it.args {
				styp := g.typ(arg.typ)
				g.type_definitions.writeln('\t$styp arg${i+1};')
			}
			g.type_definitions.writeln('} $wrapper_struct_name;')
			g.type_definitions.writeln('void* ${wrapper_fn_name}($wrapper_struct_name *arg);')
			g.gowrappers.writeln('void* ${wrapper_fn_name}($wrapper_struct_name *arg) {')
			g.gowrappers.write('\t${name}(')
			if it.is_method {
				g.gowrappers.write('arg->arg0')
				if it.args.len > 0 {
					g.gowrappers.write(', ')
				}
			}
			for i in 0 .. it.args.len {
				g.gowrappers.write('arg->arg${i+1}')
				if i < it.args.len - 1 {
					g.gowrappers.write(', ')
				}
			}
			g.gowrappers.writeln(');')
			g.gowrappers.writeln('\treturn 0;')
			g.gowrappers.writeln('}')
			g.threaded_fns << name
		}
		else {}
	}
}

fn (mut g Gen) as_cast(node ast.AsCast) {
	// Make sure the sum type can be cast to this type (the types
	// are the same), otherwise panic.
	// g.insert_before('
	styp := g.typ(node.typ)
	expr_type_sym := g.table.get_type_symbol(node.expr_type)
	if expr_type_sym.kind == .sum_type {
		/*
		g.write('*($styp*)')
		g.expr(node.expr)
		g.write('.obj')
		*/
		dot := if node.expr_type.is_ptr() { '->' } else { '.' }
		g.write('/* as */ ($styp*)__as_cast(')
		g.expr(node.expr)
		g.write(dot)
		g.write('obj, ')
		g.expr(node.expr)
		g.write(dot)
		g.write('typ, /*expected:*/$node.typ)')
	}
}

fn (mut g Gen) is_expr(node ast.InfixExpr) {
	g.expr(node.left)
	if node.left_type.is_ptr() {
		g.write('->')
	} else {
		g.write('.')
	}
	sym := g.table.get_type_symbol(node.left_type)
	if sym.kind == .interface_ {
		g.write('_interface_idx == ')
		// `_Animal_Dog_index`
		sub_type := node.right as ast.Type
		sub_sym := g.table.get_type_symbol(sub_type.typ)
		g.write('_${sym.name}_${sub_sym.name}_index')
		return
	} else if sym.kind == .sum_type {
		g.write('typ == ')
	}
	g.expr(node.right)
}

[inline]
fn styp_to_str_fn_name(styp string) string {
	return styp.replace('*', '_ptr') + '_str'
}

[inline]
fn (mut g Gen) gen_str_for_type(typ table.Type) string {
	styp := g.typ(typ)
	return g.gen_str_for_type_with_styp(typ, styp)
}

// already generated styp, reuse it
fn (mut g Gen) gen_str_for_type_with_styp(typ table.Type, styp string) string {
	sym := g.table.get_type_symbol(typ)
	str_fn_name := styp_to_str_fn_name(styp)
	sym_has_str_method, str_method_expects_ptr, str_nr_args := sym.str_method_info()
	// generate for type
	if sym_has_str_method && str_method_expects_ptr && str_nr_args == 1 {
		// TODO: optimize out this.
		// It is needed, so that println() can be called with &T and T has `fn (t &T).str() string`
		/*
		eprintln('>> gsftws: typ: $typ | typ_is_ptr $typ_is_ptr | styp: $styp ' +
			'| $str_fn_name | sym.name: $sym.name has_str: $sym_has_str_method ' +
			'| expects_ptr: $str_method_expects_ptr')
		*/
		str_fn_name_no_ptr := '${str_fn_name}_no_ptr'
		already_generated_key_no_ptr := '${styp}:${str_fn_name_no_ptr}'
		if already_generated_key_no_ptr !in g.str_types {
			g.str_types << already_generated_key_no_ptr
			g.type_definitions.writeln('string ${str_fn_name_no_ptr}(${styp} it); // auto no_ptr version')
			g.auto_str_funcs.writeln('string ${str_fn_name_no_ptr}(${styp} it){ return ${str_fn_name}(&it); }')
		}
		/*
		typ_is_ptr := typ.is_ptr()
		ret_type := if typ_is_ptr { str_fn_name } else { str_fn_name_no_ptr }
		eprintln('    ret_type: $ret_type')
		return ret_type
		*/
		return str_fn_name_no_ptr
	}
	already_generated_key := '${styp}:${str_fn_name}'
	if !sym_has_str_method && already_generated_key !in g.str_types {
		$if debugautostr ? {
			eprintln('> gen_str_for_type_with_styp: |typ: ${typ:5}, ${sym.name:20}|has_str: ${sym_has_str_method:5}|expects_ptr: ${str_method_expects_ptr:5}|nr_args: ${str_nr_args:1}|fn_name: ${str_fn_name:20}')
		}
		g.str_types << already_generated_key
		match sym.info {
			table.Alias { g.gen_str_default(sym, styp, str_fn_name) }
			table.Array { g.gen_str_for_array(it, styp, str_fn_name) }
			table.ArrayFixed { g.gen_str_for_array_fixed(it, styp, str_fn_name) }
			table.Enum { g.gen_str_for_enum(it, styp, str_fn_name) }
			table.Struct { g.gen_str_for_struct(it, styp, str_fn_name) }
			table.Map { g.gen_str_for_map(it, styp, str_fn_name) }
			else { verror("could not generate string method $str_fn_name for type \'${styp}\'") }
		}
	}
	// if varg, generate str for varg
	if typ.flag_is(.variadic) {
		varg_already_generated_key := 'varg_$already_generated_key'
		if varg_already_generated_key !in g.str_types {
			g.gen_str_for_varg(styp, str_fn_name, sym_has_str_method)
			g.str_types << varg_already_generated_key
		}
		return 'varg_$str_fn_name'
	}
	return str_fn_name
}

fn (mut g Gen) gen_str_default(sym table.TypeSymbol, styp, str_fn_name string) {
	mut convertor := ''
	mut typename_ := ''
	if sym.parent_idx in table.integer_type_idxs {
		convertor = 'int'
		typename_ = 'int'
	} else if sym.parent_idx == table.f32_type_idx {
		convertor = 'float'
		typename_ = 'f32'
	} else if sym.parent_idx == table.f64_type_idx {
		convertor = 'double'
		typename_ = 'f64'
	} else if sym.parent_idx == table.bool_type_idx {
		convertor = 'bool'
		typename_ = 'bool'
	} else {
		verror("could not generate string method for type \'${styp}\'")
	}
	g.type_definitions.writeln('string ${str_fn_name}($styp it); // auto')
	g.auto_str_funcs.writeln('string ${str_fn_name}($styp it) {')
	if convertor == 'bool' {
		g.auto_str_funcs.writeln('\tstring tmp1 = string_add(tos_lit("${styp}("), (${convertor})it ? tos_lit("true") : tos_lit("false"));')
	} else {
		g.auto_str_funcs.writeln('\tstring tmp1 = string_add(tos_lit("${styp}("), tos3(${typename_}_str((${convertor})it).str));')
	}
	g.auto_str_funcs.writeln('\tstring tmp2 = string_add(tmp1, tos_lit(")"));')
	g.auto_str_funcs.writeln('\tstring_free(&tmp1);')
	g.auto_str_funcs.writeln('\treturn tmp2;')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_enum(info table.Enum, styp, str_fn_name string) {
	s := styp.replace('.', '__')
	g.type_definitions.writeln('string ${str_fn_name}($styp it); // auto')
	g.auto_str_funcs.writeln('string ${str_fn_name}($styp it) { /* gen_str_for_enum */')
	g.auto_str_funcs.writeln('\tswitch(it) {')
	for val in info.vals {
		g.auto_str_funcs.writeln('\t\tcase ${s}_$val: return tos_lit("$val");')
	}
	g.auto_str_funcs.writeln('\t\tdefault: return tos_lit("unknown enum value");')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_struct(info table.Struct, styp, str_fn_name string) {
	// TODO: short it if possible
	// generates all definitions of substructs
	mut fnames2strfunc := {
		'': ''
	} // map[string]string // TODO vfmt bug
	for field in info.fields {
		sym := g.table.get_type_symbol(field.typ)
		if !sym.has_method('str') {
			field_styp := g.typ(field.typ)
			field_fn_name := g.gen_str_for_type_with_styp(field.typ, field_styp)
			fnames2strfunc[field_styp] = field_fn_name
		}
	}
	g.type_definitions.writeln('string ${str_fn_name}($styp x, int indent_count); // auto')
	g.auto_str_funcs.writeln('string ${str_fn_name}($styp x, int indent_count) {')
	mut clean_struct_v_type_name := styp.replace('__', '.')
	if styp.ends_with('*') {
		deref_typ := styp.replace('*', '')
		g.auto_str_funcs.writeln('\t${deref_typ} *it = x;')
		clean_struct_v_type_name = '&' + clean_struct_v_type_name.replace('*', '')
	} else {
		deref_typ := styp
		g.auto_str_funcs.writeln('\t${deref_typ} *it = &x;')
	}
	// generate ident / indent length = 4 spaces
	g.auto_str_funcs.writeln('\tstring indents = tos_lit("");')
	g.auto_str_funcs.writeln('\tfor (int i = 0; i < indent_count; i++) {')
	g.auto_str_funcs.writeln('\t\tindents = string_add(indents, tos_lit("    "));')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('\treturn _STR("${clean_struct_v_type_name} {\\n"')
	for field in info.fields {
		fmt := g.type_to_fmt(field.typ)
		g.auto_str_funcs.writeln('\t\t"%.*s\\000    ' + '$field.name: $fmt\\n"')
	}
	g.auto_str_funcs.write('\t\t"%.*s\\000}", ${2*(info.fields.len+1)}')
	if info.fields.len > 0 {
		g.auto_str_funcs.write(',\n\t\t')
		for i, field in info.fields {
			sym := g.table.get_type_symbol(field.typ)
			has_custom_str := sym.has_method('str')
			second_str_param := if has_custom_str { '' } else { ', indent_count + 1' }
			mut field_styp := g.typ(field.typ)
			if field_styp.ends_with('*') {
				field_styp = field_styp.replace('*', '')
			}
			field_styp_fn_name := if has_custom_str { '${field_styp}_str' } else { fnames2strfunc[field_styp] }
			if sym.kind == .enum_ {
				g.auto_str_funcs.write('indents, ')
				g.auto_str_funcs.write('${field_styp_fn_name}( it->${c_name(field.name)} ) ')
			} else if sym.kind == .struct_ {
				g.auto_str_funcs.write('indents, ')
				g.auto_str_funcs.write('${field_styp_fn_name}( it->${c_name(field.name)}${second_str_param} ) ')
			} else if sym.kind in [.array, .array_fixed, .map] {
				g.auto_str_funcs.write('indents, ')
				g.auto_str_funcs.write('${field_styp_fn_name}( it->${c_name(field.name)}) ')
			} else {
				g.auto_str_funcs.write('indents, it->${c_name(field.name)}')
				if field.typ == table.bool_type {
					g.auto_str_funcs.write(' ? _SLIT("true") : _SLIT("false")')
				}
			}
			if i < info.fields.len - 1 {
				g.auto_str_funcs.write(',\n\t\t')
			}
		}
	}
	g.auto_str_funcs.writeln(',')
	g.auto_str_funcs.writeln('\t\tindents);')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_array(info table.Array, styp, str_fn_name string) {
	sym := g.table.get_type_symbol(info.elem_type)
	field_styp := g.typ(info.elem_type)
	is_elem_ptr := info.elem_type.is_ptr()
	sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
	mut elem_str_fn_name := ''
	if sym_has_str_method {
		elem_str_fn_name = if is_elem_ptr {
			field_styp.replace('*', '') + '_str'
		} else {
			field_styp + '_str'
		}
	} else {
		elem_str_fn_name = styp_to_str_fn_name(field_styp)
	}
	if !sym_has_str_method {
		// eprintln('> sym.name: does not have method `str`')
		g.gen_str_for_type_with_styp(info.elem_type, field_styp)
	}
	g.type_definitions.writeln('string ${str_fn_name}($styp a); // auto')
	g.auto_str_funcs.writeln('string ${str_fn_name}($styp a) {')
	g.auto_str_funcs.writeln('\tstrings__Builder sb = strings__new_builder(a.len * 10);')
	g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, tos_lit("["));')
	g.auto_str_funcs.writeln('\tfor (int i = 0; i < a.len; i++) {')
	g.auto_str_funcs.writeln('\t\t${field_styp} it = (*(${field_styp}*)array_get(a, i));')
	if sym.kind == .struct_ && !sym_has_str_method {
		if is_elem_ptr {
			g.auto_str_funcs.writeln('\t\tstring x = ${elem_str_fn_name}(*it,0);')
		} else {
			g.auto_str_funcs.writeln('\t\tstring x = ${elem_str_fn_name}(it,0);')
		}
	} else if sym.kind in [.f32, .f64] {
		g.auto_str_funcs.writeln('\t\tstring x = _STR("%g", 1, it);')
	} else {
		// There is a custom .str() method, so use it.
		// NB: we need to take account of whether the user has defined
		// `fn (x T) str() {` or `fn (x &T) str() {`, and convert accordingly
		if str_method_expects_ptr && is_elem_ptr || !str_method_expects_ptr && !is_elem_ptr {
			g.auto_str_funcs.writeln('\t\tstring x = ${elem_str_fn_name}(it);')
		} else if str_method_expects_ptr && !is_elem_ptr {
			g.auto_str_funcs.writeln('\t\tstring x = ${elem_str_fn_name}(&it);')
		} else if !str_method_expects_ptr && is_elem_ptr {
			g.auto_str_funcs.writeln('\t\tstring x = ${elem_str_fn_name}(*it);')
		}
	}
	g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, x);')
	if g.pref.autofree && info.elem_type != table.bool_type {
		// no need to free "true"/"false" literals
		g.auto_str_funcs.writeln('\t\tstring_free(&x);')
	}
	g.auto_str_funcs.writeln('\t\tif (i < a.len-1) {')
	g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write(&sb, tos_lit(", "));')
	g.auto_str_funcs.writeln('\t\t}')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, tos_lit("]"));')
	g.auto_str_funcs.writeln('\tstring res = strings__Builder_str(&sb);')
	g.auto_str_funcs.writeln('\tstrings__Builder_free(&sb);')
	// g.auto_str_funcs.writeln('\treturn strings__Builder_str(&sb);')
	g.auto_str_funcs.writeln('\treturn res;')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_array_fixed(info table.ArrayFixed, styp, str_fn_name string) {
	sym := g.table.get_type_symbol(info.elem_type)
	field_styp := g.typ(info.elem_type)
	is_elem_ptr := info.elem_type.is_ptr()
	sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
	mut elem_str_fn_name := ''
	if sym_has_str_method {
		elem_str_fn_name = if is_elem_ptr {
			field_styp.replace('*', '') + '_str'
		} else {
			field_styp + '_str'
		}
	} else {
		elem_str_fn_name = styp_to_str_fn_name(field_styp)
	}
	if !sym.has_method('str') {
		g.gen_str_for_type_with_styp(info.elem_type, field_styp)
	}
	g.type_definitions.writeln('string ${str_fn_name}($styp a); // auto')
	g.auto_str_funcs.writeln('string ${str_fn_name}($styp a) {')
	g.auto_str_funcs.writeln('\tstrings__Builder sb = strings__new_builder($info.size * 10);')
	g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, tos_lit("["));')
	g.auto_str_funcs.writeln('\tfor (int i = 0; i < $info.size; i++) {')
	if sym.kind == .struct_ && !sym_has_str_method {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, ${elem_str_fn_name}(a[i],0));')
	} else if sym.kind in [.f32, .f64] {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, _STR("%g", 1, a[i]));')
	} else if sym.kind == .string {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, _STR("\'%.*s\\000\'", 2, a[i]));')
	} else {
		if str_method_expects_ptr && is_elem_ptr || !str_method_expects_ptr && !is_elem_ptr {
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, ${elem_str_fn_name}(a[i]));')
		} else if str_method_expects_ptr && !is_elem_ptr {
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, ${elem_str_fn_name}(&a[i]));')
		} else if !str_method_expects_ptr && is_elem_ptr {
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, ${elem_str_fn_name}(*a[i]));')
		}
	}
	g.auto_str_funcs.writeln('\t\tif (i < ${info.size-1}) {')
	g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write(&sb, tos_lit(", "));')
	g.auto_str_funcs.writeln('\t\t}')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, tos_lit("]"));')
	g.auto_str_funcs.writeln('\treturn strings__Builder_str(&sb);')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_map(info table.Map, styp, str_fn_name string) {
	key_sym := g.table.get_type_symbol(info.key_type)
	key_styp := g.typ(info.key_type)
	if !key_sym.has_method('str') {
		g.gen_str_for_type_with_styp(info.key_type, key_styp)
	}
	val_sym := g.table.get_type_symbol(info.value_type)
	val_styp := g.typ(info.value_type)
	elem_str_fn_name := val_styp.replace('*', '') + '_str'
	if !val_sym.has_method('str') {
		g.gen_str_for_type_with_styp(info.value_type, val_styp)
	}
	zero := g.type_default(info.value_type)
	g.type_definitions.writeln('string ${str_fn_name}($styp m); // auto')
	g.auto_str_funcs.writeln('string ${str_fn_name}($styp m) { /* gen_str_for_map */')
	g.auto_str_funcs.writeln('\tstrings__Builder sb = strings__new_builder(m.key_values.size*10);')
	g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, tos_lit("{"));')
	g.auto_str_funcs.writeln('\tfor (unsigned int i = 0; i < m.key_values.size; i++) {')
	g.auto_str_funcs.writeln('\t\tstring key = (*(string*)DenseArray_get(m.key_values, i));')
	g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, _STR("\'%.*s\\000\'", 2, key));')
	g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, tos_lit(": "));')
	g.auto_str_funcs.write('\t$val_styp it = (*($val_styp*)map_get3(')
	g.auto_str_funcs.write('m, (*(string*)DenseArray_get(m.key_values, i))')
	g.auto_str_funcs.write(', ')
	g.auto_str_funcs.writeln(' &($val_styp[]) { $zero }));')
	if val_sym.kind == .string {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, _STR("\'%.*s\\000\'", 2, it));')
	} else if val_sym.kind == .struct_ && !val_sym.has_method('str') {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, ${elem_str_fn_name}(it,0));')
	} else if val_sym.kind in [.f32, .f64] {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, _STR("%g", 1, it));')
	} else {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, ${elem_str_fn_name}(it));')
	}
	g.auto_str_funcs.writeln('\t\tif (i != m.key_values.size-1) {')
	g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write(&sb, tos_lit(", "));')
	g.auto_str_funcs.writeln('\t\t}')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, tos_lit("}"));')
	g.auto_str_funcs.writeln('\treturn strings__Builder_str(&sb);')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_varg(styp, str_fn_name string, has_str_method bool) {
	g.definitions.writeln('string varg_${str_fn_name}(varg_$styp it); // auto')
	g.auto_str_funcs.writeln('string varg_${str_fn_name}(varg_$styp it) {')
	g.auto_str_funcs.writeln('\tstrings__Builder sb = strings__new_builder(it.len);')
	g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, tos_lit("["));')
	g.auto_str_funcs.writeln('\tfor(int i=0; i<it.len; i++) {')
	if has_str_method {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, ${str_fn_name}(it.args[i]));')
	} else {
		// autogenerated str methods take the indent level as a second argument:
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, ${str_fn_name}(it.args[i], 0));')
	}
	g.auto_str_funcs.writeln('\t\tif (i < it.len-1) {')
	g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write(&sb, tos_lit(", "));')
	g.auto_str_funcs.writeln('\t\t}')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, tos_lit("]"));')
	g.auto_str_funcs.writeln('\treturn strings__Builder_str(&sb);')
	g.auto_str_funcs.writeln('}')
}

fn (g Gen) type_to_fmt(typ table.Type) string {
	sym := g.table.get_type_symbol(typ)
	if sym.kind in [.struct_, .array, .array_fixed, .map] {
		return '%.*s\\000'
	} else if typ == table.string_type {
		return "\'%.*s\\000\'"
	} else if typ == table.bool_type {
		return '%.*s\\000'
	} else if sym.kind == .enum_ {
		return '%.*s\\000'
	} else if typ in [table.f32_type, table.f64_type] {
		return '%g\\000' // g removes trailing zeros unlike %f
	}
	return '%d\\000'
}

// Generates interface table and interface indexes
fn (g &Gen) interface_table() string {
	mut sb := strings.new_builder(100)
	for ityp in g.table.types {
		if ityp.kind != .interface_ {
			continue
		}
		inter_info := ityp.info as table.Interface
		if inter_info.types.len == 0 {
			continue
		}
		sb.writeln('// NR interfaced types= $inter_info.types.len')
		// interface_name is for example Speaker
		interface_name := c_name(ityp.name)
		// generate a struct that references interface methods
		methods_struct_name := 'struct _${interface_name}_interface_methods'
		mut methods_typ_def := strings.new_builder(100)
		mut methods_struct_def := strings.new_builder(100)
		methods_struct_def.writeln('$methods_struct_name {')
		mut imethods := map[string]string{} // a map from speak -> _Speaker_speak_fn
		mut methodidx := map[string]int{}
		for k, method in ityp.methods {
			methodidx[method.name] = k
			typ_name := '_${interface_name}_${method.name}_fn'
			ret_styp := g.typ(method.return_type)
			methods_typ_def.write('typedef $ret_styp (*$typ_name)(void* _')
			// the first param is the receiver, it's handled by `void*` above
			for i in 1 .. method.args.len {
				arg := method.args[i]
				methods_typ_def.write(', ${g.typ(arg.typ)} $arg.name')
			}
			// TODO g.fn_args(method.args[1..], method.is_variadic)
			methods_typ_def.writeln(');')
			methods_struct_def.writeln('\t$typ_name ${c_name(method.name)};')
			imethods[method.name] = typ_name
		}
		methods_struct_def.writeln('};')
		// generate an array of the interface methods for the structs using the interface
		// as well as case functions from the struct to the interface
		mut methods_struct := strings.new_builder(100)
		methods_struct.writeln('$methods_struct_name ${interface_name}_name_table[$inter_info.types.len] = {')
		mut cast_functions := strings.new_builder(100)
		cast_functions.write('// Casting functions for interface "${interface_name}"')
		mut methods_wrapper := strings.new_builder(100)
		methods_wrapper.writeln('// Methods wrapper for interface "${interface_name}"')
		for i, st in inter_info.types {
			// cctype is the Cleaned Concrete Type name, *without ptr*,
			// i.e. cctype is always just Cat, not Cat_ptr:
			cctype := g.cc_type(st)
			// Speaker_Cat_index = 0
			interface_index_name := '_${interface_name}_${cctype}_index'
			cast_functions.writeln('
_Interface I_${cctype}_to_Interface_${interface_name}(${cctype}* x) {
	return (_Interface) {
		._object = (void*) (x),
		._interface_idx = ${interface_index_name}
	};
}

_Interface* I_${cctype}_to_Interface_${interface_name}_ptr(${cctype}* x) {
	/* TODO Remove memdup */
	return (_Interface*) memdup(&(_Interface) {
		._object = (void*) (x),
		._interface_idx = ${interface_index_name}
	}, sizeof(_Interface));
}')
			methods_struct.writeln('\t{')
			st_sym := g.table.get_type_symbol(st)
			mut method := table.Fn{}
			for _, m in ityp.methods {
				for mm in st_sym.methods {
					if mm.name == m.name {
						method = mm
						break
					}
				}
				if method.name !in imethods {
					// a method that is not part of the interface should be just skipped
					continue
				}
				// .speak = Cat_speak
				mut method_call := '${cctype}_${method.name}'
				if !method.args[0].typ.is_ptr() {
					// inline void Cat_speak_method_wrapper(Cat c) { return Cat_speak(*c); }
					methods_wrapper.write('static inline ${g.typ(method.return_type)}')
					methods_wrapper.write(' ${method_call}_method_wrapper(')
					methods_wrapper.write('${cctype}* ${method.args[0].name}')
					// TODO g.fn_args
					for j in 1 .. method.args.len {
						arg := method.args[j]
						methods_wrapper.write(', ${g.typ(arg.typ)} $arg.name')
					}
					methods_wrapper.writeln(') {')
					methods_wrapper.write('\t')
					if method.return_type != table.void_type {
						methods_wrapper.write('return ')
					}
					methods_wrapper.write('${method_call}(*${method.args[0].name}')
					for j in 1 .. method.args.len {
						methods_wrapper.write(', ${method.args[j].name}')
					}
					methods_wrapper.writeln(');')
					methods_wrapper.writeln('}')
					// .speak = Cat_speak_method_wrapper
					method_call += '_method_wrapper'
				}
				methods_struct.writeln('\t\t.${c_name(method.name)} = $method_call,')
			}
			methods_struct.writeln('\t},')
			sb.writeln('int ${interface_index_name} = $i;')
		}
		methods_struct.writeln('};')
		// add line return after interface index declarations
		sb.writeln('')
		sb.writeln(methods_wrapper.str())
		sb.writeln(methods_typ_def.str())
		sb.writeln(methods_struct_def.str())
		sb.writeln(methods_struct.str())
		sb.writeln(cast_functions.str())
	}
	return sb.str()
}

fn (mut g Gen) array_init(it ast.ArrayInit) {
	type_sym := g.table.get_type_symbol(it.typ)
	if type_sym.kind == .array_fixed {
		g.write('{')
		for i, expr in it.exprs {
			g.expr(expr)
			if i != it.exprs.len - 1 {
				g.write(', ')
			}
		}
		g.write('}')
		return
	}
	// elem_sym := g.table.get_type_symbol(it.elem_type)
	elem_type_str := g.typ(it.elem_type)
	if it.exprs.len == 0 {
		g.write('__new_array_with_default(')
		if it.has_len {
			g.expr(it.len_expr)
			g.write(', ')
		} else {
			g.write('0, ')
		}
		if it.has_cap {
			g.expr(it.cap_expr)
			g.write(', ')
		} else {
			g.write('0, ')
		}
		g.write('sizeof($elem_type_str), ')
		if it.has_default || (it.has_len && it.elem_type == table.string_type) {
			g.write('&_val_$it.pos.pos)')
		} else {
			g.write('0)')
		}
		return
	}
	len := it.exprs.len
	g.write('new_array_from_c_array($len, $len, sizeof($elem_type_str), _MOV(($elem_type_str[$len]){')
	g.writeln('')
	for i, expr in it.exprs {
		if it.is_interface {
			// sym := g.table.get_type_symbol(it.interface_types[i])
			// isym := g.table.get_type_symbol(it.interface_type)
			g.interface_call(it.interface_types[i], it.interface_type)
		}
		g.expr(expr)
		if it.is_interface {
			g.write(')')
		}
		g.write(', ')
	}
	g.writeln('')
	g.write('}))')
}

// `ui.foo(button)` =>
// `ui__foo(I_ui__Button_to_ui__Widget(` ...
fn (g &Gen) interface_call(typ, interface_type table.Type) {
	interface_styp := g.cc_type(interface_type)
	styp := g.cc_type(typ)
	mut cast_fn_name := 'I_${styp}_to_Interface_${interface_styp}'
	if interface_type.is_ptr() {
		cast_fn_name += '_ptr'
	}
	g.write('${cast_fn_name}(')
	if !typ.is_ptr() {
		g.write('&')
	}
}

fn (mut g Gen) panic_debug_info(pos token.Position) (int, string, string, string) {
	paline := pos.line_nr + 1
	pafile := g.fn_decl.file.replace('\\', '/')
	pafn := g.fn_decl.name.after('.')
	mut pamod := g.fn_decl.name.all_before_last('.')
	if pamod == pafn {
		pamod = if g.fn_decl.is_builtin {
			'builtin'
		} else {
			'main'
		}
	}
	return paline, pafile, pamod, pafn
}
