// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import v2.ast
import v2.pref
import v2.types
import strings

pub struct Gen {
	files []ast.File
	env   &types.Environment = unsafe { nil }
	pref  &pref.Preferences  = unsafe { nil }
mut:
	sb                     strings.Builder
	indent                 int
	cur_fn_scope           &types.Scope = unsafe { nil }
	cur_fn_name            string
	cur_fn_ret_type        string
	cur_module             string
	emitted_types          map[string]bool
	fn_param_is_ptr        map[string][]bool
	fn_param_types         map[string][]string
	fn_return_types        map[string]string
	fn_names_in_decl_order []string
	runtime_local_types    map[string]string

	fixed_array_fields          map[string]bool
	fixed_array_field_elem      map[string]string
	fixed_array_globals         map[string]bool
	tuple_aliases               map[string][]string
	struct_field_types          map[string]string
	enum_value_to_enum          map[string]string
	enum_type_fields            map[string]map[string]bool
	array_aliases               map[string]bool
	map_aliases                 map[string]bool
	result_aliases              map[string]bool
	option_aliases              map[string]bool
	emitted_result_structs      map[string]bool
	emitted_option_structs      map[string]bool
	embedded_field_owner        map[string]string
	collected_fixed_array_types map[string]FixedArrayInfo
	collected_map_types         map[string]MapTypeInfo
	sum_type_variants           map[string][]string
	// Interface method signatures: interface_name -> [(method_name, cast_signature), ...]
	interface_methods      map[string][]InterfaceMethodInfo
	tmp_counter            int
	cur_fn_mut_params      map[string]bool   // names of mut params in current function
	global_var_modules     map[string]string // global var name → module name
	primitive_type_aliases map[string]bool   // type names that are aliases for primitive types
}

struct InterfaceMethodInfo {
	name           string // method name (e.g., "draw")
	cast_signature string // function pointer cast (e.g., "int (*)(void*)")
	ret_type       string
	param_types    []string
}

struct StructDeclInfo {
	decl ast.StructDecl
	mod  string
}

struct FixedArrayInfo {
	elem_type string
	size      int
}

struct MapTypeInfo {
	key_c_type   string
	value_c_type string
}

const primitive_types = ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64',
	'bool', 'rune', 'byte', 'voidptr', 'charptr', 'usize', 'isize', 'void', 'char', 'byteptr',
	'float_literal', 'int_literal']

fn is_empty_stmt(s ast.Stmt) bool {
	return s is ast.EmptyStmt
}

fn is_empty_expr(e ast.Expr) bool {
	return e is ast.EmptyExpr
}

fn (g &Gen) result_value_type(result_type string) string {
	if !result_type.starts_with('_result_') {
		return ''
	}
	return unmangle_c_ptr_type(result_type['_result_'.len..])
}

fn option_value_type(option_type string) string {
	if !option_type.starts_with('_option_') {
		return ''
	}
	return unmangle_c_ptr_type(option_type['_option_'.len..])
}

// unmangle_c_ptr_type converts mangled pointer type names back to C pointer types.
// e.g. FILEptr -> FILE*, voidptr -> void*, charptr -> char*
fn unmangle_c_ptr_type(name string) string {
	if name == 'voidptr' {
		return 'void*'
	}
	if name == 'charptr' {
		return 'char*'
	}
	if name == 'byteptr' {
		return 'u8*'
	}
	if name.len > 3 && name.ends_with('ptr') {
		base := name[..name.len - 3]
		// Any mangled pointer type (created by mangle_alias_component replacing * with ptr)
		// should be unmangled back to base*
		return '${base}*'
	}
	return name
}

fn unmangle_alias_component_to_c(name string) string {
	if name.starts_with('Array_') || name.starts_with('Array_fixed_') || name.starts_with('Map_')
		|| name.starts_with('_option_') || name.starts_with('_result_') {
		return name
	}
	return unmangle_c_ptr_type(name)
}

fn map_key_type_candidates() []string {
	return ['string', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'byte', 'rune',
		'char', 'bool', 'f32', 'f64', 'isize', 'usize', 'voidptr', 'byteptr', 'charptr']
}

fn (mut g Gen) infer_map_type_info_from_alias_name(map_name string) ?MapTypeInfo {
	if !map_name.starts_with('Map_') || map_name.len <= 'Map_'.len {
		return none
	}
	rest := map_name['Map_'.len..]
	mut key_types := map_key_type_candidates()
	for _, info in g.collected_map_types {
		if info.key_c_type != '' && info.key_c_type !in key_types {
			key_types << info.key_c_type
		}
	}
	for key_c in key_types {
		key_alias := mangle_alias_component(key_c)
		prefix := '${key_alias}_'
		if !rest.starts_with(prefix) {
			continue
		}
		val_alias := rest[prefix.len..]
		if val_alias == '' {
			continue
		}
		info := MapTypeInfo{
			key_c_type:   key_c
			value_c_type: unmangle_alias_component_to_c(val_alias)
		}
		g.collected_map_types[map_name] = info
		return info
	}
	sep := rest.index('_') or { return none }
	val_alias := rest[sep + 1..]
	if val_alias == '' {
		return none
	}
	info := MapTypeInfo{
		key_c_type:   unmangle_alias_component_to_c(rest[..sep])
		value_c_type: unmangle_alias_component_to_c(val_alias)
	}
	g.collected_map_types[map_name] = info
	return info
}

fn (mut g Gen) ensure_map_type_info(map_name string) ?MapTypeInfo {
	if info := g.collected_map_types[map_name] {
		return info
	}
	return g.infer_map_type_info_from_alias_name(map_name)
}

fn array_alias_elem_type(arr_type string) string {
	base := arr_type.trim_right('*')
	if base.starts_with('Array_') {
		return unmangle_c_ptr_type(base['Array_'.len..])
	}
	if base in ['strings__Builder', 'Builder'] {
		return 'u8'
	}
	return ''
}

fn is_generic_placeholder_c_type_name(name string) bool {
	mut base := unmangle_c_ptr_type(name)
	base = base.trim_right('*')
	if base.contains('__') {
		base = base.all_after_last('__')
	}
	return is_generic_placeholder_type_name(base)
}

fn is_none_expr(expr ast.Expr) bool {
	match expr {
		ast.Keyword {
			return expr.tok == .key_none
		}
		ast.Ident {
			return expr.name == 'none'
		}
		else {
			return false
		}
	}
}

fn is_none_like_expr(expr ast.Expr) bool {
	if is_none_expr(expr) {
		return true
	}
	if expr is ast.Type && expr is ast.NoneType {
		return true
	}
	return false
}

fn interface_type_id_for_name(name string) int {
	match name {
		'None__' { return 1 }
		'Error' { return 2 }
		'MessageError' { return 3 }
		else { return 0 }
	}
}

fn (mut g Gen) is_error_call_expr(expr ast.Expr) bool {
	match expr {
		ast.CallExpr {
			if expr.lhs is ast.Ident {
				name := sanitize_fn_ident(expr.lhs.name)
				if name == 'error' {
					return true
				}
			}
			// Check if the call returns IError
			ret := g.get_call_return_type(expr.lhs, expr.args.len) or { '' }
			if ret == 'IError' {
				return true
			}
		}
		ast.CallOrCastExpr {
			if expr.lhs is ast.Ident && !g.is_type_name(expr.lhs.name) {
				name := sanitize_fn_ident(expr.lhs.name)
				if name == 'error' {
					return true
				}
			}
			ret := g.get_call_return_type(expr.lhs, 1) or { '' }
			if ret == 'IError' {
				return true
			}
		}
		else {}
	}
	// Also check environment type
	expr_type := g.get_expr_type(expr)
	if expr_type == 'IError' {
		return true
	}
	return false
}

pub fn Gen.new(files []ast.File) &Gen {
	return Gen.new_with_env_and_pref(files, unsafe { nil }, unsafe { nil })
}

pub fn Gen.new_with_env(files []ast.File, env &types.Environment) &Gen {
	return Gen.new_with_env_and_pref(files, env, unsafe { nil })
}

pub fn Gen.new_with_env_and_pref(files []ast.File, env &types.Environment, p &pref.Preferences) &Gen {
	return &Gen{
		files:                  files
		env:                    unsafe { env }
		pref:                   unsafe { p }
		sb:                     strings.new_builder(4096)
		fn_param_is_ptr:        map[string][]bool{}
		fn_param_types:         map[string][]string{}
		fn_return_types:        map[string]string{}
		fn_names_in_decl_order: []string{}
		runtime_local_types:    map[string]string{}

		fixed_array_fields:     map[string]bool{}
		fixed_array_field_elem: map[string]string{}
		fixed_array_globals:    map[string]bool{}
		tuple_aliases:          map[string][]string{}
		struct_field_types:     map[string]string{}
		enum_value_to_enum:     map[string]string{}
		enum_type_fields:       map[string]map[string]bool{}
		array_aliases:          map[string]bool{}
		map_aliases:            map[string]bool{}
		result_aliases:         map[string]bool{}
		option_aliases:         map[string]bool{}
		emitted_result_structs: map[string]bool{}
		emitted_option_structs: map[string]bool{}
		embedded_field_owner:   map[string]string{}
	}
}

fn (g &Gen) use_prealloc_allocator() bool {
	return g.pref != unsafe { nil } && g.pref.use_prealloc_allocator
}

pub fn (mut g Gen) gen() string {
	g.write_preamble()
	g.collect_module_type_names()
	g.collect_runtime_aliases()
	g.collect_fn_signatures()

	// Pre-collect all global variable names so they can be module-qualified
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if stmt is ast.GlobalDecl {
				for field in stmt.fields {
					if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
						g.global_var_modules[field.name] = g.cur_module
					}
				}
			}
		}
	}

	// Pass 1: Forward declarations for all structs/unions/sumtypes/interfaces (needed for mutual references)
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if stmt is ast.StructDecl {
				if stmt.language == .c {
					continue
				}
				name := g.get_struct_name(stmt)
				if name in g.emitted_types {
					continue
				}
				g.emitted_types[name] = true
				keyword := if stmt.is_union { 'union' } else { 'struct' }
				g.sb.writeln('typedef ${keyword} ${name} ${name};')
			} else if stmt is ast.TypeDecl {
				if stmt.variants.len > 0 {
					// Sum type needs forward struct declaration
					name := g.get_type_decl_name(stmt)
					if name !in g.emitted_types {
						g.emitted_types[name] = true
						g.sb.writeln('typedef struct ${name} ${name};')
					}
				}
			} else if stmt is ast.InterfaceDecl {
				name := g.get_interface_name(stmt)
				if name !in g.emitted_types {
					g.emitted_types[name] = true
					g.sb.writeln('typedef struct ${name} ${name};')
				}
			}
		}
	}
	g.sb.writeln('')
	g.emit_runtime_aliases()
	g.sb.writeln('')

	// Pass 2: Enum declarations, type aliases, interface structs, and sum type structs
	// (before struct definitions that may reference them)
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if stmt is ast.EnumDecl {
				g.gen_enum_decl(stmt)
			} else if stmt is ast.TypeDecl {
				if stmt.variants.len == 0 && stmt.base_type !is ast.EmptyExpr {
					g.gen_type_alias(stmt)
				} else if stmt.variants.len > 0 {
					g.gen_sum_type_decl(stmt)
				}
			} else if stmt is ast.InterfaceDecl {
				g.gen_interface_decl(stmt)
			}
		}
	}

	// Pass 3: Full struct definitions (use named struct/union to match forward decls)
	// Collect all struct decls, then emit in dependency order
	mut all_structs := []StructDeclInfo{}
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if stmt is ast.StructDecl {
				if stmt.language == .c {
					continue
				}
				all_structs << StructDeclInfo{
					decl: stmt
					mod:  g.cur_module
				}
			}
		}
	}
	// Emit structs with only primitive/resolved fields first, then the rest.
	// Interleave option/result wrapper emission as soon as their payload types are complete.
	// Repeat until no more progress (simple topo sort with wrapper side-effects).
	for _ in 0 .. (all_structs.len * 2) {
		mut progressed := false
		for info in all_structs {
			g.cur_module = info.mod
			name := g.get_struct_name(info.decl)
			body_key := 'body_${name}'
			if body_key in g.emitted_types {
				continue
			}
			// Check if all field types are already defined
			if g.struct_fields_resolved(info.decl) {
				g.gen_struct_decl(info.decl)
				progressed = true
			}
		}
		if g.emit_ready_option_result_structs() {
			progressed = true
		}
		if !progressed {
			break
		}
	}
	// Emit any remaining structs (circular deps - just emit them)
	for info in all_structs {
		g.cur_module = info.mod
		g.gen_struct_decl(info.decl)
	}
	_ = g.emit_ready_option_result_structs()
	// Pass 3.1: Emit deferred (non-primitive) fixed array typedefs now that struct defs exist
	g.emit_deferred_fixed_array_aliases()

	// Pass 3.25: Tuple aliases (multiple-return lowering support)
	g.emit_tuple_aliases()
	if g.tuple_aliases.len > 0 {
		g.sb.writeln('')
	}

	// Pass 3.3: Emit option/result struct definitions (needs IError + tuple types defined)
	g.emit_option_result_structs()

	// Pass 3.5: Emit constants before function declarations/bodies, so macros are available.
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if stmt is ast.ConstDecl {
				g.gen_const_decl(stmt)
			}
		}
	}
	g.sb.writeln('')

	// Pass 4: Function forward declarations
	mut test_fn_names := []string{}
	mut has_main := false
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				if stmt.language == .js {
					continue
				}
				if stmt.language == .c && stmt.stmts.len == 0 {
					continue
				}
				// Skip generic functions - they have unresolved type params
				if stmt.typ.generic_params.len > 0 {
					continue
				}
				fn_name := g.get_fn_name(stmt)
				if fn_name == '' {
					continue
				}
				if fn_name == 'main' {
					has_main = true
				}
				if stmt.name.starts_with('test_') && !stmt.is_method && stmt.typ.params.len == 0 {
					test_fn_names << fn_name
				}
				if g.env != unsafe { nil } {
					if fn_scope := g.env.get_fn_scope(g.cur_module, fn_name) {
						g.cur_fn_scope = fn_scope
					}
				}
				g.gen_fn_head(stmt)
				g.sb.writeln(';')
			}
		}
	}
	g.sb.writeln('')
	// Emit map inline helpers after function forward declarations (needs map__get etc.)
	g.emit_map_helpers()
	g.emit_ierror_wrappers()
	g.emit_interface_method_wrappers()

	// Pass 5: Everything else (function bodies, consts, globals, etc.)
	for file in g.files {
		g.gen_file(file)
	}

	// Generate test runner main if this is a test file (has test_ functions but no main)
	if !has_main && test_fn_names.len > 0 {
		g.sb.writeln('')
		g.sb.writeln('int main(int ___argc, char** ___argv) {')
		g.sb.writeln('\tg_main_argc = ___argc;')
		g.sb.writeln('\tg_main_argv = (void*)___argv;')
		for test_fn in test_fn_names {
			msg_run := 'Running test: ${test_fn}...'
			msg_ok := '  OK'
			g.sb.writeln('\tprintln((string){"${msg_run}", sizeof("${msg_run}") - 1});')
			g.sb.writeln('\t${test_fn}();')
			g.sb.writeln('\tprintln((string){"${msg_ok}", sizeof("${msg_ok}") - 1});')
		}
		msg_all := 'All ${test_fn_names.len} tests passed.'
		g.sb.writeln('\tprintln((string){"${msg_all}", sizeof("${msg_all}") - 1});')
		g.sb.writeln('\treturn 0;')
		g.sb.writeln('}')
	}

	return g.sb.str()
}

fn interface_wrapper_name(iface_name string, concrete_type string, method_name string) string {
	return '__iface_wrap_${mangle_alias_component(iface_name)}_${mangle_alias_component(concrete_type)}_${method_name}'
}

fn (mut g Gen) emit_interface_method_wrappers() {
	mut emitted := map[string]bool{}
	mut iface_names := g.interface_methods.keys()
	iface_names.sort()
	fn_names := g.fn_names_in_decl_order
	for iface_name in iface_names {
		methods := g.interface_methods[iface_name]
		for method in methods {
			suffix := '__${method.name}'
			for fn_name in fn_names {
				if !fn_name.ends_with(suffix) {
					continue
				}
				ptr_params := g.fn_param_is_ptr[fn_name] or { continue }
				if ptr_params.len == 0 || ptr_params[0] {
					// Pointer receivers can be called directly through void*.
					continue
				}
				fn_params := g.fn_param_types[fn_name] or { continue }
				if fn_params.len != method.param_types.len + 1 {
					continue
				}
				if fn_ret := g.fn_return_types[fn_name] {
					if fn_ret != method.ret_type {
						continue
					}
				}
				concrete_type := fn_name[..fn_name.len - suffix.len]
				receiver_type := fn_params[0].trim_right('*')
				if receiver_type != concrete_type {
					continue
				}
				wrapper_name := interface_wrapper_name(iface_name, concrete_type, method.name)
				if wrapper_name in emitted {
					continue
				}
				emitted[wrapper_name] = true
				g.sb.write_string('static ${method.ret_type} ${wrapper_name}(void* _obj')
				for i, param_type in method.param_types {
					g.sb.write_string(', ${param_type} _arg${i}')
				}
				g.sb.writeln(') {')
				if method.ret_type != 'void' {
					g.sb.write_string('\treturn ')
				} else {
					g.sb.write_string('\t')
				}
				g.sb.write_string('${fn_name}(*(((${concrete_type}*)_obj))')
				for i in 0 .. method.param_types.len {
					g.sb.write_string(', _arg${i}')
				}
				g.sb.writeln(');')
				g.sb.writeln('}')
				g.sb.writeln('')
			}
		}
	}
}

fn (mut g Gen) write_preamble() {
	g.sb.writeln('// Generated by V Clean C Backend')
	g.sb.writeln('#include <stdio.h>')
	g.sb.writeln('#include <stdlib.h>')
	g.sb.writeln('#include <stdbool.h>')
	g.sb.writeln('#include <stdint.h>')
	g.sb.writeln('#include <stddef.h>')
	g.sb.writeln('#include <string.h>')
	g.sb.writeln('#include <float.h>')
	g.sb.writeln('#include <unistd.h>')
	g.sb.writeln('#include <fcntl.h>')
	g.sb.writeln('#include <sys/stat.h>')
	g.sb.writeln('#include <errno.h>')
	g.sb.writeln('#include <signal.h>')
	g.sb.writeln('#include <dirent.h>')
	g.sb.writeln('#include <sys/types.h>')
	g.sb.writeln('#include <sys/wait.h>')
	g.sb.writeln('#ifdef __APPLE__')
	g.sb.writeln('#include <sys/syslimits.h>')
	g.sb.writeln('#include <mach/mach_time.h>')
	g.sb.writeln('#include <execinfo.h>')
	g.sb.writeln('#endif')
	g.sb.writeln('#include <termios.h>')
	g.sb.writeln('#include <sys/ioctl.h>')
	g.sb.writeln('#include <pthread.h>')
	g.sb.writeln('#include <time.h>')
	g.sb.writeln('#include <sys/time.h>')
	g.sb.writeln('#include <sys/statvfs.h>')
	g.sb.writeln('#include <utime.h>')
	g.sb.writeln('#include <sys/utsname.h>')
	g.sb.writeln('#ifdef __APPLE__')
	g.sb.writeln('#include <sys/ptrace.h>')
	g.sb.writeln('#include <libproc.h>')
	g.sb.writeln('#endif')
	g.sb.writeln('extern char** environ;')
	g.sb.writeln('#define signal(v_sig, v_handler) signal((v_sig), ((void (*)(int))(v_handler)))')
	g.sb.writeln('')

	// V primitive type aliases
	g.sb.writeln('// V primitive types')
	g.sb.writeln('typedef int8_t i8;')
	g.sb.writeln('typedef int16_t i16;')
	g.sb.writeln('typedef int32_t i32;')
	g.sb.writeln('typedef int64_t i64;')
	g.sb.writeln('typedef uint8_t u8;')
	g.sb.writeln('typedef uint16_t u16;')
	g.sb.writeln('typedef uint32_t u32;')
	g.sb.writeln('typedef uint64_t u64;')
	g.sb.writeln('typedef float f32;')
	g.sb.writeln('typedef double f64;')
	g.sb.writeln('typedef u8 byte;')
	g.sb.writeln('typedef size_t usize;')
	g.sb.writeln('typedef ptrdiff_t isize;')
	g.sb.writeln('typedef u32 rune;')
	g.sb.writeln('typedef char* byteptr;')
	g.sb.writeln('typedef char* charptr;')
	g.sb.writeln('typedef void* voidptr;')
	g.sb.writeln('typedef void* chan;')
	g.sb.writeln('typedef double float_literal;')
	g.sb.writeln('typedef int64_t int_literal;')
	if g.use_prealloc_allocator() {
		g.write_prealloc_allocator_preamble()
	}
	// wyhash implementation used by builtin/map and hash modules.
	g.sb.writeln('#ifndef wyhash_final_version_4_2')
	g.sb.writeln('#define wyhash_final_version_4_2')
	g.sb.writeln('#define WYHASH_CONDOM 1')
	g.sb.writeln('#define WYHASH_32BIT_MUM 0')
	g.sb.writeln('#if defined(__GNUC__) || defined(__INTEL_COMPILER) || defined(__clang__)')
	g.sb.writeln('  #define _likely_(x) __builtin_expect(x,1)')
	g.sb.writeln('  #define _unlikely_(x) __builtin_expect(x,0)')
	g.sb.writeln('#else')
	g.sb.writeln('  #define _likely_(x) (x)')
	g.sb.writeln('  #define _unlikely_(x) (x)')
	g.sb.writeln('#endif')
	g.sb.writeln('static inline uint64_t _wyrot(uint64_t x) { return (x>>32)|(x<<32); }')
	g.sb.writeln('static inline void _wymum(uint64_t *A, uint64_t *B){')
	g.sb.writeln('#if defined(__SIZEOF_INT128__)')
	g.sb.writeln('  __uint128_t r=*A; r*=*B; *A=(uint64_t)r; *B=(uint64_t)(r>>64);')
	g.sb.writeln('#elif defined(_MSC_VER) && defined(_M_X64)')
	g.sb.writeln('  *A=_umul128(*A,*B,B);')
	g.sb.writeln('#else')
	g.sb.writeln('  uint64_t ha=*A>>32, hb=*B>>32, la=(uint32_t)*A, lb=(uint32_t)*B, hi, lo;')
	g.sb.writeln('  uint64_t rh=ha*hb, rm0=ha*lb, rm1=hb*la, rl=la*lb, t=rl+(rm0<<32), c=t<rl;')
	g.sb.writeln('  lo=t+(rm1<<32); c+=lo<t; hi=rh+(rm0>>32)+(rm1>>32)+c;')
	g.sb.writeln('  *A=lo; *B=hi;')
	g.sb.writeln('#endif')
	g.sb.writeln('}')
	g.sb.writeln('static inline uint64_t _wymix(uint64_t A, uint64_t B){ _wymum(&A,&B); return A^B; }')
	g.sb.writeln('#ifndef WYHASH_LITTLE_ENDIAN')
	g.sb.writeln('  #ifdef TARGET_ORDER_IS_LITTLE')
	g.sb.writeln('    #define WYHASH_LITTLE_ENDIAN 1')
	g.sb.writeln('  #else')
	g.sb.writeln('    #define WYHASH_LITTLE_ENDIAN 0')
	g.sb.writeln('  #endif')
	g.sb.writeln('#endif')
	g.sb.writeln('#if (WYHASH_LITTLE_ENDIAN)')
	g.sb.writeln('  static inline uint64_t _wyr8(const uint8_t *p) { uint64_t v; memcpy(&v, p, 8); return v;}')
	g.sb.writeln('  static inline uint64_t _wyr4(const uint8_t *p) { uint32_t v; memcpy(&v, p, 4); return v;}')
	g.sb.writeln('#elif defined(__GNUC__) || defined(__INTEL_COMPILER) || defined(__clang__)')
	g.sb.writeln('  static inline uint64_t _wyr8(const uint8_t *p) { uint64_t v; memcpy(&v, p, 8); return __builtin_bswap64(v);}')
	g.sb.writeln('  static inline uint64_t _wyr4(const uint8_t *p) { uint32_t v; memcpy(&v, p, 4); return __builtin_bswap32(v);}')
	g.sb.writeln('#else')
	g.sb.writeln('  static inline uint64_t _wyr8(const uint8_t *p) {')
	g.sb.writeln('    uint64_t v; memcpy(&v, p, 8);')
	g.sb.writeln('    return (((v >> 56) & 0xff)| ((v >> 40) & 0xff00)| ((v >> 24) & 0xff0000)| ((v >>  8) & 0xff000000)| ((v <<  8) & 0xff00000000)| ((v << 24) & 0xff0000000000)| ((v << 40) & 0xff000000000000)| ((v << 56) & 0xff00000000000000));')
	g.sb.writeln('  }')
	g.sb.writeln('  static inline uint64_t _wyr4(const uint8_t *p) {')
	g.sb.writeln('    uint32_t v; memcpy(&v, p, 4);')
	g.sb.writeln('    return (((v >> 24) & 0xff)| ((v >>  8) & 0xff00)| ((v <<  8) & 0xff0000)| ((v << 24) & 0xff000000));')
	g.sb.writeln('  }')
	g.sb.writeln('#endif')
	g.sb.writeln('static inline uint64_t _wyr3(const uint8_t *p, size_t k) { return (((uint64_t)p[0])<<16)|(((uint64_t)p[k>>1])<<8)|p[k-1];}')
	g.sb.writeln('static inline uint64_t wyhash(const void *key, size_t len, uint64_t seed, const uint64_t *secret){')
	g.sb.writeln('  const uint8_t *p=(const uint8_t *)key; seed^=_wymix(seed^secret[0],secret[1]); uint64_t a, b;')
	g.sb.writeln('  if(_likely_(len<=16)){')
	g.sb.writeln('    if(_likely_(len>=4)){ a=(_wyr4(p)<<32)|_wyr4(p+((len>>3)<<2)); b=(_wyr4(p+len-4)<<32)|_wyr4(p+len-4-((len>>3)<<2)); }')
	g.sb.writeln('    else if(_likely_(len>0)){ a=_wyr3(p,len); b=0; }')
	g.sb.writeln('    else a=b=0;')
	g.sb.writeln('  } else {')
	g.sb.writeln('    size_t i=len;')
	g.sb.writeln('    if(_unlikely_(i>=48)){')
	g.sb.writeln('      uint64_t see1=seed, see2=seed;')
	g.sb.writeln('      do{')
	g.sb.writeln('        seed=_wymix(_wyr8(p)^secret[1],_wyr8(p+8)^seed);')
	g.sb.writeln('        see1=_wymix(_wyr8(p+16)^secret[2],_wyr8(p+24)^see1);')
	g.sb.writeln('        see2=_wymix(_wyr8(p+32)^secret[3],_wyr8(p+40)^see2);')
	g.sb.writeln('        p+=48; i-=48;')
	g.sb.writeln('      }while(_likely_(i>=48));')
	g.sb.writeln('      seed^=see1^see2;')
	g.sb.writeln('    }')
	g.sb.writeln('    while(_unlikely_(i>16)){ seed=_wymix(_wyr8(p)^secret[1],_wyr8(p+8)^seed); i-=16; p+=16; }')
	g.sb.writeln('    a=_wyr8(p+i-16); b=_wyr8(p+i-8);')
	g.sb.writeln('  }')
	g.sb.writeln('  a^=secret[1]; b^=seed; _wymum(&a,&b);')
	g.sb.writeln('  return _wymix(a^secret[0]^len,b^secret[1]);')
	g.sb.writeln('}')
	g.sb.writeln('static const uint64_t _wyp[4] = {0x2d358dccaa6c78a5ull, 0x8bb84b93962eacc9ull, 0x4b33a62ed433d4a3ull, 0x4d5a2da51de1aa47ull};')
	g.sb.writeln('static inline uint64_t wyhash64(uint64_t A, uint64_t B){ A^=0x2d358dccaa6c78a5ull; B^=0x8bb84b93962eacc9ull; _wymum(&A,&B); return _wymix(A^0x2d358dccaa6c78a5ull,B^0x8bb84b93962eacc9ull);}')
	g.sb.writeln('#endif')
	g.sb.writeln('#define _MOV')
	g.sb.writeln('typedef u8 termios__Cc;')
	// sync__RwMutex for shared variables
	g.sb.writeln('typedef struct sync__RwMutex { pthread_rwlock_t mutex; } sync__RwMutex;')
	g.sb.writeln('static inline void sync__RwMutex_rlock(sync__RwMutex* m) { pthread_rwlock_rdlock(&m->mutex); }')
	g.sb.writeln('static inline void sync__RwMutex_runlock(sync__RwMutex* m) { pthread_rwlock_unlock(&m->mutex); }')
	g.sb.writeln('static inline void sync__RwMutex_lock(sync__RwMutex* m) { pthread_rwlock_wrlock(&m->mutex); }')
	g.sb.writeln('static inline void sync__RwMutex_unlock(sync__RwMutex* m) { pthread_rwlock_unlock(&m->mutex); }')
	g.sb.writeln('')
	g.sb.writeln('')
}

fn (mut g Gen) write_prealloc_allocator_preamble() {
	preamble := [
		'// prealloc arena allocator (enabled by -prealloc)',
		'static void* (*v2_sys_malloc_fn)(size_t) = malloc;',
		'static void* (*v2_sys_realloc_fn)(void*, size_t) = realloc;',
		'static void (*v2_sys_free_fn)(void*) = free;',
		'typedef struct v2_realloc_chunk {',
		'    u8* data;',
		'    size_t used;',
		'    size_t cap;',
		'    struct v2_realloc_chunk* prev;',
		'} v2_realloc_chunk;',
		'typedef union v2_realloc_header {',
		'    struct {',
		'        size_t size;',
		'    } meta;',
		'    max_align_t _align;',
		'} v2_realloc_header;',
		'static v2_realloc_chunk* v2_realloc_current_chunk = NULL;',
		'static int v2_realloc_cleanup_registered = 0;',
		'static const size_t v2_realloc_min_chunk_cap = ((size_t)64 * 1024 * 1024);',
		'static void v2_realloc_alloc_panic(size_t bytes) {',
		'    fprintf(stderr, "v2 -prealloc arena: allocation failed for %zu bytes\\n", bytes);',
		'    exit(1);',
		'}',
		'static size_t v2_realloc_align_up(size_t value, size_t align) {',
		'    if (align == 0) {',
		'        return value;',
		'    }',
		'    size_t rem = value % align;',
		'    return rem == 0 ? value : value + (align - rem);',
		'}',
		'static void v2_realloc_cleanup(void) {',
		'    v2_realloc_chunk* chunk = v2_realloc_current_chunk;',
		'    while (chunk != NULL) {',
		'        v2_realloc_chunk* prev = chunk->prev;',
		'        v2_sys_free_fn(chunk->data);',
		'        v2_sys_free_fn(chunk);',
		'        chunk = prev;',
		'    }',
		'    v2_realloc_current_chunk = NULL;',
		'}',
		'static void v2_realloc_add_chunk(size_t min_cap) {',
		'    size_t cap = v2_realloc_min_chunk_cap;',
		'    if (v2_realloc_current_chunk != NULL && v2_realloc_current_chunk->cap > cap) {',
		'        cap = v2_realloc_current_chunk->cap;',
		'    }',
		'    while (cap < min_cap) {',
		'        size_t next = cap * 2;',
		'        if (next <= cap) {',
		'            cap = min_cap;',
		'            break;',
		'        }',
		'        cap = next;',
		'    }',
		'    v2_realloc_chunk* chunk = (v2_realloc_chunk*)v2_sys_malloc_fn(sizeof(v2_realloc_chunk));',
		'    if (chunk == NULL) {',
		'        v2_realloc_alloc_panic(sizeof(v2_realloc_chunk));',
		'    }',
		'    chunk->data = (u8*)v2_sys_realloc_fn(NULL, cap);',
		'    if (chunk->data == NULL) {',
		'        v2_realloc_alloc_panic(cap);',
		'    }',
		'    chunk->used = 0;',
		'    chunk->cap = cap;',
		'    chunk->prev = v2_realloc_current_chunk;',
		'    v2_realloc_current_chunk = chunk;',
		'    if (!v2_realloc_cleanup_registered) {',
		'        atexit(v2_realloc_cleanup);',
		'        v2_realloc_cleanup_registered = 1;',
		'    }',
		'}',
		'static v2_realloc_chunk* v2_realloc_find_chunk(const u8* ptr) {',
		'    for (v2_realloc_chunk* chunk = v2_realloc_current_chunk; chunk != NULL; chunk = chunk->prev) {',
		'        const u8* start = chunk->data;',
		'        const u8* end = chunk->data + chunk->used;',
		'        if (ptr > start && ptr <= end) {',
		'            return chunk;',
		'        }',
		'    }',
		'    return NULL;',
		'}',
		'static void* v2_realloc_malloc(size_t size) {',
		'    if (size == 0) {',
		'        size = 1;',
		'    }',
		'    size_t align = sizeof(max_align_t);',
		'    size_t needed = sizeof(v2_realloc_header) + size;',
		'    if (v2_realloc_current_chunk == NULL) {',
		'        v2_realloc_add_chunk(needed + align);',
		'    }',
		'    size_t offset = v2_realloc_align_up(v2_realloc_current_chunk->used, align);',
		'    if (offset + needed > v2_realloc_current_chunk->cap) {',
		'        v2_realloc_add_chunk(needed + align);',
		'        offset = v2_realloc_align_up(v2_realloc_current_chunk->used, align);',
		'    }',
		'    v2_realloc_header* header = (v2_realloc_header*)(v2_realloc_current_chunk->data + offset);',
		'    header->meta.size = size;',
		'    v2_realloc_current_chunk->used = offset + needed;',
		'    return (void*)(header + 1);',
		'}',
		'static void* v2_realloc_calloc(size_t count, size_t size) {',
		'    if (count == 0 || size == 0) {',
		'        return v2_realloc_malloc(1);',
		'    }',
		'    if (count > ((size_t)-1) / size) {',
		'        v2_realloc_alloc_panic((size_t)-1);',
		'    }',
		'    size_t total = count * size;',
		'    void* ptr = v2_realloc_malloc(total);',
		'    memset(ptr, 0, total);',
		'    return ptr;',
		'}',
		'static void* v2_realloc_realloc(void* ptr, size_t new_size) {',
		'    if (ptr == NULL) {',
		'        return v2_realloc_malloc(new_size);',
		'    }',
		'    if (new_size == 0) {',
		'        new_size = 1;',
		'    }',
		'    v2_realloc_chunk* chunk = v2_realloc_find_chunk((const u8*)ptr);',
		'    if (chunk == NULL) {',
		'        void* out = v2_sys_realloc_fn(ptr, new_size);',
		'        if (out == NULL) {',
		'            v2_realloc_alloc_panic(new_size);',
		'        }',
		'        return out;',
		'    }',
		'    v2_realloc_header* header = ((v2_realloc_header*)ptr) - 1;',
		'    size_t old_size = header->meta.size;',
		'    size_t header_offset = (size_t)((u8*)header - chunk->data);',
		'    size_t alloc_start = header_offset + sizeof(v2_realloc_header);',
		'    size_t alloc_end = alloc_start + old_size;',
		'    if (chunk == v2_realloc_current_chunk && alloc_end == chunk->used) {',
		'        size_t new_end = alloc_start + new_size;',
		'        if (new_end <= chunk->cap) {',
		'            header->meta.size = new_size;',
		'            chunk->used = new_end;',
		'            return ptr;',
		'        }',
		'    }',
		'    void* new_ptr = v2_realloc_malloc(new_size);',
		'    size_t copy_size = old_size < new_size ? old_size : new_size;',
		'    memcpy(new_ptr, ptr, copy_size);',
		'    return new_ptr;',
		'}',
		'static void v2_realloc_free(void* ptr) {',
		'    if (ptr == NULL) {',
		'        return;',
		'    }',
		'    if (v2_realloc_find_chunk((const u8*)ptr) != NULL) {',
		'        return;',
		'    }',
		'    v2_sys_free_fn(ptr);',
		'}',
		'#define malloc(n) v2_realloc_malloc((size_t)(n))',
		'#define calloc(c, n) v2_realloc_calloc((size_t)(c), (size_t)(n))',
		'#define realloc(p, n) v2_realloc_realloc((p), (size_t)(n))',
		'#define free(p) v2_realloc_free((p))',
	].join('\n')
	g.sb.write_string(preamble)
	g.sb.write_string('\n\n')
}

fn is_c_identifier_like(name string) bool {
	if name.len == 0 {
		return false
	}
	for ch in name {
		if !(ch.is_letter() || ch.is_digit() || ch == `_`) {
			return false
		}
	}
	return true
}

fn is_c_runtime_function(name string) bool {
	return name in ['free', 'malloc', 'realloc', 'calloc', 'memcmp', 'memcpy', 'memmove', 'memset',
		'strlen', 'strcmp', 'strncmp', 'snprintf', 'sprintf', 'printf', 'fprintf', 'asprintf',
		'atoi', 'atoll', 'atof']
}

fn (mut g Gen) collect_module_type_names() {
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			match stmt {
				ast.StructDecl {
					if stmt.language != .v {
						continue
					}
					struct_name := g.get_struct_name(stmt)
					for field in stmt.fields {
						field_type := g.expr_type_to_c(field.typ)
						full_key := stmt.name + '.' + field.name
						struct_key := struct_name + '.' + field.name
						g.struct_field_types[full_key] = field_type
						g.struct_field_types[struct_key] = field_type
						if field.typ is ast.Type && field.typ is ast.ArrayFixedType {
							fixed_typ := field.typ as ast.ArrayFixedType
							elem_type := g.expr_type_to_c(fixed_typ.elem_type)
							g.fixed_array_fields[full_key] = true
							g.fixed_array_fields[struct_key] = true
							g.fixed_array_field_elem[full_key] = elem_type
							g.fixed_array_field_elem[struct_key] = elem_type
						}
					}
				}
				ast.EnumDecl {
					enum_name := g.get_enum_name(stmt)
					mut fields := map[string]bool{}
					if existing := g.enum_type_fields[enum_name] {
						fields = existing.clone()
					}
					for field in stmt.fields {
						if field.name !in g.enum_value_to_enum {
							g.enum_value_to_enum[field.name] = enum_name
						}
						fields[field.name] = true
					}
					mut cloned_fields := fields.clone()
					g.enum_type_fields[enum_name] = cloned_fields.move()
					if enum_name.contains('__') {
						short_name := enum_name.all_after_last('__')
						mut short_cloned_fields := fields.clone()
						g.enum_type_fields[short_name] = short_cloned_fields.move()
					}
				}
				ast.TypeDecl {
					if stmt.language == .c {
						continue
					}
				}
				ast.InterfaceDecl {}
				else {}
			}
		}
	}
}

fn (mut g Gen) collect_runtime_aliases() {
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			g.collect_decl_type_aliases_from_stmt(stmt)
		}
	}
	// Also use type-checker output so aliases used only in expressions are captured.
	if g.env != unsafe { nil } {
		for _, typ in g.env.expr_types {
			// Some self-host runs leave zero-value Type entries in expr_types.
			// Those decode as `types.Alias` with an invalid payload.
			// Skip top-level aliases from env cache; declarations are collected
			// from the AST path above.
			if typ is types.Alias {
				continue
			}
			g.collect_aliases_from_type(typ)
		}
	}
}

fn (mut g Gen) collect_fn_signatures() {
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			match stmt {
				ast.FnDecl {
					if stmt.language == .js {
						continue
					}
					mut fn_name := ''
					if stmt.language == .c && stmt.stmts.len == 0 {
						// Keep raw C symbol names for C.xxx calls (e.g. open, chdir).
						fn_name = sanitize_fn_ident(stmt.name)
					} else {
						fn_name = g.get_fn_name(stmt)
					}
					if fn_name == '' {
						continue
					}
					ret_type := if stmt.name == 'main' {
						'int'
					} else if stmt.typ.return_type !is ast.EmptyExpr {
						g.expr_type_to_c(stmt.typ.return_type)
					} else {
						'void'
					}
					mut params := []bool{}
					mut param_types := []string{}
					if stmt.is_method && stmt.receiver.name != '' {
						recv_type := g.expr_type_to_c(stmt.receiver.typ)
						params << (stmt.receiver.is_mut || recv_type.ends_with('*'))
						param_types << if stmt.receiver.is_mut {
							recv_type + '*'
						} else {
							recv_type
						}
					}
					for param in stmt.typ.params {
						param_type := g.expr_type_to_c(param.typ)
						params << (param.is_mut || param_type.ends_with('*'))
						param_types << if param.is_mut {
							param_type + '*'
						} else {
							param_type
						}
					}
					g.fn_param_is_ptr[fn_name] = params
					g.fn_param_types[fn_name] = param_types
					g.fn_return_types[fn_name] = ret_type
					g.fn_names_in_decl_order << fn_name
				}
				else {}
			}
		}
	}
}

fn (mut g Gen) emit_ierror_wrappers() {
	body_ierror_key := 'body_IError'
	body_builtin_ierror_key := 'body_builtin__IError'
	if body_ierror_key !in g.emitted_types && body_builtin_ierror_key !in g.emitted_types {
		return
	}
	mut emitted_any := false
	mut base_set := map[string]bool{}
	error_code_fn := 'Error__code'
	for fn_name, ret_type in g.fn_return_types {
		if !fn_name.ends_with('__msg') || ret_type != 'string' {
			continue
		}
		base := fn_name[..fn_name.len - '__msg'.len]
		if base == '' || !is_c_identifier_like(base) {
			continue
		}
		base_set[base] = true
	}
	mut bases := base_set.keys()
	bases.sort()
	for base in bases {
		type_label := if base.contains('__') { base.all_after_last('__') } else { base }
		g.sb.writeln('static string IError_${base}_type_name_wrapper(void* _obj) {')
		g.sb.writeln('\t(void)_obj;')
		g.sb.writeln('\treturn (string){"${type_label}", ${type_label.len}};')
		g.sb.writeln('}')
		g.sb.writeln('static string IError_${base}_msg_wrapper(void* _obj) {')
		g.sb.writeln('\treturn ${base}__msg(*(${base}*)_obj);')
		g.sb.writeln('}')
		g.sb.writeln('static int IError_${base}_code_wrapper(void* _obj) {')
		code_fn := '${base}__code'
		if code_fn in g.fn_return_types {
			g.sb.writeln('\treturn ${base}__code(*(${base}*)_obj);')
		} else if error_code_fn in g.fn_return_types {
			g.sb.writeln('\treturn Error__code(*(Error*)_obj);')
		} else {
			g.sb.writeln('\t(void)_obj;')
			g.sb.writeln('\treturn 1;')
		}
		g.sb.writeln('}')
		emitted_any = true
	}
	if emitted_any {
		g.sb.writeln('')
	}
}

fn (mut g Gen) collect_aliases_from_type(t types.Type) {
	match t {
		types.Array {
			g.collect_aliases_from_type(t.elem_type)
			elem := mangle_alias_component(g.types_type_to_c(t.elem_type))
			g.register_alias_type('Array_${elem}')
		}
		types.ArrayFixed {
			g.collect_aliases_from_type(t.elem_type)
			elem := mangle_alias_component(g.types_type_to_c(t.elem_type))
			fixed_name := 'Array_fixed_${elem}_${t.len}'
			g.register_alias_type(fixed_name)
			g.collected_fixed_array_types[fixed_name] = FixedArrayInfo{
				elem_type: g.types_type_to_c(t.elem_type)
				size:      t.len
			}
		}
		types.Map {
			g.collect_aliases_from_type(t.key_type)
			g.collect_aliases_from_type(t.value_type)
			key := mangle_alias_component(g.types_type_to_c(t.key_type))
			val := mangle_alias_component(g.types_type_to_c(t.value_type))
			map_name := 'Map_${key}_${val}'
			g.register_alias_type(map_name)
			g.collected_map_types[map_name] = MapTypeInfo{
				key_c_type:   g.types_type_to_c(t.key_type)
				value_c_type: g.types_type_to_c(t.value_type)
			}
		}
		types.OptionType {
			g.collect_aliases_from_type(t.base_type)
			base := mangle_alias_component(g.types_type_to_c(t.base_type))
			g.register_alias_type('_option_${base}')
		}
		types.ResultType {
			g.collect_aliases_from_type(t.base_type)
			base := mangle_alias_component(g.types_type_to_c(t.base_type))
			g.register_alias_type('_result_${base}')
		}
		types.Alias {
			// Alias payloads from self-host env caches can be malformed.
			// Decls are collected from AST, so skip runtime alias recursion here.
			return
		}
		types.Pointer {
			g.collect_aliases_from_type(t.base_type)
		}
		else {}
	}
}

fn (mut g Gen) collect_decl_type_aliases_from_stmt(stmt ast.Stmt) {
	match stmt {
		ast.StructDecl {
			if stmt.language == .c {
				return
			}
			for emb in stmt.embedded {
				_ = g.expr_type_to_c(emb)
			}
			for field in stmt.fields {
				if field.typ !is ast.EmptyExpr {
					_ = g.expr_type_to_c(field.typ)
				}
			}
		}
		ast.InterfaceDecl {
			for field in stmt.fields {
				if field.typ !is ast.EmptyExpr {
					_ = g.expr_type_to_c(field.typ)
				}
			}
		}
		ast.TypeDecl {
			if stmt.language == .c {
				return
			}
			if stmt.base_type !is ast.EmptyExpr {
				_ = g.expr_type_to_c(stmt.base_type)
			}
			for variant in stmt.variants {
				_ = g.expr_type_to_c(variant)
			}
		}
		ast.FnDecl {
			if stmt.language == .js {
				return
			}
			if stmt.language == .c && stmt.stmts.len == 0 {
				return
			}
			if stmt.is_method && stmt.receiver.typ !is ast.EmptyExpr {
				_ = g.expr_type_to_c(stmt.receiver.typ)
			}
			for param in stmt.typ.params {
				_ = g.expr_type_to_c(param.typ)
			}
			if stmt.typ.return_type !is ast.EmptyExpr {
				_ = g.expr_type_to_c(stmt.typ.return_type)
			}
		}
		ast.GlobalDecl {
			for field in stmt.fields {
				if field.typ !is ast.EmptyExpr {
					_ = g.expr_type_to_c(field.typ)
				}
			}
		}
		else {}
	}
}

fn is_generic_placeholder_type_name(name string) bool {
	return name in ['T', 'K', 'V', 'Type'] || (name.len == 1 && name[0] >= `A` && name[0] <= `Z`)
}

fn (g &Gen) option_result_payload_invalid(val_type string) bool {
	if val_type == '' {
		return true
	}
	if val_type == 'void' {
		return false
	}
	return is_generic_placeholder_type_name(val_type)
}

fn (g &Gen) option_result_payload_ready(val_type string) bool {
	if g.option_result_payload_invalid(val_type) || val_type == 'void' {
		return false
	}
	ierror_body_key := 'body_IError'
	ierror_builtin_body_key := 'body_builtin__IError'
	if ierror_body_key !in g.emitted_types && ierror_builtin_body_key !in g.emitted_types {
		return false
	}
	if val_type in primitive_types || val_type in ['bool', 'char', 'void*', 'u8*', 'char*'] {
		return true
	}
	// `string` is a struct payload and needs its body emitted before sizeof(string).
	if val_type == 'string' || val_type == 'builtin__string' {
		string_body_key := 'body_string'
		builtin_string_body_key := 'body_builtin__string'
		return string_body_key in g.emitted_types || builtin_string_body_key in g.emitted_types
	}
	if val_type == 'IError' || val_type == 'builtin__IError' {
		return true
	}
	if val_type.ends_with('*') {
		return true
	}
	if val_type.starts_with('Array_fixed_') {
		body_key := 'body_${val_type}'
		alias_key := 'alias_${val_type}'
		return body_key in g.emitted_types || alias_key in g.emitted_types
	}
	if val_type.starts_with('Array_') {
		return true
	}
	if val_type.starts_with('Map_') {
		return true
	}
	body_key := 'body_${val_type}'
	enum_key := 'enum_${val_type}'
	alias_key := 'alias_${val_type}'
	if body_key in g.emitted_types || enum_key in g.emitted_types || alias_key in g.emitted_types {
		return true
	}
	return false
}

fn (mut g Gen) emit_ready_option_result_structs() bool {
	mut emitted_any := false
	mut option_names := g.option_aliases.keys()
	option_names.sort()
	for name in option_names {
		if name in g.emitted_option_structs {
			continue
		}
		val_type := option_value_type(name)
		if !g.option_result_payload_ready(val_type) {
			continue
		}
		g.sb.writeln('struct ${name} { u8 state; IError err; u8 data[sizeof(${val_type}) > 1 ? sizeof(${val_type}) : 1]; };')
		g.emitted_option_structs[name] = true
		emitted_any = true
	}
	mut result_names := g.result_aliases.keys()
	result_names.sort()
	for name in result_names {
		if name in g.emitted_result_structs {
			continue
		}
		val_type := g.result_value_type(name)
		if !g.option_result_payload_ready(val_type) {
			continue
		}
		g.sb.writeln('struct ${name} { bool is_error; IError err; u8 data[sizeof(${val_type}) > 1 ? sizeof(${val_type}) : 1]; };')
		g.emitted_result_structs[name] = true
		emitted_any = true
	}
	return emitted_any
}

fn (mut g Gen) register_alias_type(name string) {
	if !is_c_identifier_like(name) {
		return
	}
	if name.starts_with('Array_') || name.starts_with('Array_fixed_') {
		g.array_aliases[name] = true
		return
	}
	if name.starts_with('Map_') {
		g.map_aliases[name] = true
		return
	}
	if name.starts_with('_result_') {
		val_type := g.result_value_type(name)
		if g.option_result_payload_invalid(val_type) {
			return
		}
		g.result_aliases[name] = true
		return
	}
	if name.starts_with('_option_') {
		val_type := option_value_type(name)
		if g.option_result_payload_invalid(val_type) {
			return
		}
		g.option_aliases[name] = true
	}
}

fn (mut g Gen) emit_runtime_aliases() {
	mut array_names := g.array_aliases.keys()
	array_names.sort()
	// Emit dynamic array aliases (skip fixed arrays)
	for name in array_names {
		if name.starts_with('Array_fixed_') {
			continue
		}
		g.sb.writeln('typedef array ${name};')
	}
	// Emit primitive fixed array typedefs (non-primitive ones deferred until after struct defs)
	for name in array_names {
		if !name.starts_with('Array_fixed_') {
			continue
		}
		if info := g.collected_fixed_array_types[name] {
			if info.elem_type in primitive_types
				|| info.elem_type in ['char', 'voidptr', 'charptr', 'byteptr', 'void*', 'char*'] {
				g.sb.writeln('typedef ${info.elem_type} ${name} [${info.size}];')
				alias_key := 'alias_${name}'
				body_key := 'body_${name}'
				g.emitted_types[alias_key] = true
				g.emitted_types[body_key] = true
			}
		}
	}
	g.sb.writeln('typedef array VArg_string;')
	mut map_names := g.map_aliases.keys()
	map_names.sort()
	for name in map_names {
		g.sb.writeln('typedef map ${name};')
	}
	// Map helpers emitted after struct definitions (need complete array/map types)
	// Option/Result forward declarations (struct definitions emitted later
	// after IError is defined, via emit_option_result_structs)
	mut option_names := g.option_aliases.keys()
	option_names.sort()
	for name in option_names {
		val_type := option_value_type(name)
		if g.option_result_payload_invalid(val_type) {
			continue
		}
		if val_type != '' && val_type != 'void' {
			g.sb.writeln('typedef struct ${name} ${name};')
		} else {
			g.sb.writeln('typedef _option ${name};')
		}
	}
	mut result_names := g.result_aliases.keys()
	result_names.sort()
	for name in result_names {
		val_type := g.result_value_type(name)
		if g.option_result_payload_invalid(val_type) {
			continue
		}
		if val_type != '' && val_type != 'void' {
			g.sb.writeln('typedef struct ${name} ${name};')
		} else {
			g.sb.writeln('typedef _result ${name};')
		}
	}
}

fn (mut g Gen) emit_option_result_structs() {
	for g.emit_ready_option_result_structs() {}
}

fn (mut g Gen) emit_deferred_fixed_array_aliases() {
	mut names := g.array_aliases.keys()
	names.sort()
	for name in names {
		if !name.starts_with('Array_fixed_') {
			continue
		}
		if info := g.collected_fixed_array_types[name] {
			if info.elem_type in primitive_types
				|| info.elem_type in ['char', 'voidptr', 'charptr', 'byteptr', 'void*', 'char*'] {
				continue // already emitted in emit_runtime_aliases
			}
			g.sb.writeln('typedef ${info.elem_type} ${name} [${info.size}];')
			alias_key := 'alias_${name}'
			body_key := 'body_${name}'
			g.emitted_types[alias_key] = true
			g.emitted_types[body_key] = true
		}
	}
}

fn (mut g Gen) emit_map_helpers() {
	mut map_names := g.map_aliases.keys()
	map_names.sort()
	for name in map_names {
		info := g.ensure_map_type_info(name) or { continue }
		key_c := info.key_c_type
		val_c := info.value_c_type
		if is_generic_placeholder_c_type_name(key_c) || is_generic_placeholder_c_type_name(val_c) {
			continue
		}
		// Constructor
		g.sb.writeln('static inline ${name} __new_${name}() { return (${name}){0}; }')
		// Getter - skip when value is a fixed array (can't return C array by value)
		if !val_c.starts_with('Array_fixed_') {
			g.sb.writeln('static inline ${val_c} __${name}_get(${name} m, ${key_c} key) {')
			g.sb.writeln('\t${val_c} _def = {0};')
			g.sb.writeln('\treturn (*(${val_c}*)map__get((map*)&m, &key, &_def));')
			g.sb.writeln('}')
		}
		// Pointer getter
		g.sb.writeln('static inline ${val_c}* __${name}_get_check(${name}* m, ${key_c} key) {')
		g.sb.writeln('\treturn (${val_c}*)map__get_check((map*)m, &key);')
		g.sb.writeln('}')
		// Setter
		g.sb.writeln('static inline void __${name}_set(${name}* m, ${key_c} key, ${val_c} val) {')
		g.sb.writeln('\tmap__set((map*)m, &key, &val);')
		g.sb.writeln('}')
	}
}

fn (mut g Gen) set_file_module(file ast.File) {
	for stmt in file.stmts {
		if stmt is ast.ModuleStmt {
			g.cur_module = stmt.name.replace('.', '_')
			return
		}
	}
	// Files without a module declaration are in the 'main' module
	g.cur_module = 'main'
}

fn (mut g Gen) gen_file(file ast.File) {
	g.set_file_module(file)
	for stmt in file.stmts {
		// Skip struct/enum/type/interface/const decls - already emitted in earlier passes
		if stmt is ast.StructDecl || stmt is ast.EnumDecl || stmt is ast.TypeDecl
			|| stmt is ast.ConstDecl || stmt is ast.InterfaceDecl {
			continue
		}
		g.gen_stmt(stmt)
	}
}

fn (mut g Gen) gen_stmts(stmts []ast.Stmt) {
	for s in stmts {
		g.gen_stmt(s)
	}
}

fn (mut g Gen) gen_stmt(node ast.Stmt) {
	match node {
		ast.FnDecl {
			g.gen_fn_decl(node)
		}
		ast.AssignStmt {
			g.gen_assign_stmt(node)
		}
		ast.ExprStmt {
			if node.expr is ast.UnsafeExpr {
				unsafe_expr := node.expr as ast.UnsafeExpr
				if unsafe_expr.stmts.len > 1 {
					g.write_indent()
					g.sb.writeln('{')
					g.indent++
				}
				for stmt in unsafe_expr.stmts {
					g.gen_stmt(stmt)
				}
				if unsafe_expr.stmts.len > 1 {
					g.indent--
					g.write_indent()
					g.sb.writeln('}')
				}
				return
			}
			if node.expr is ast.IfExpr {
				g.write_indent()
				g.gen_if_expr_stmt(node.expr)
				return
			}
			g.write_indent()
			g.gen_expr(node.expr)
			g.sb.writeln(';')
		}
		ast.ReturnStmt {
			g.write_indent()
			if g.is_tuple_alias(g.cur_fn_ret_type) {
				if node.exprs.len == 1 {
					expr := node.exprs[0]
					if g.get_expr_type(expr) == g.cur_fn_ret_type {
						g.sb.write_string('return ')
						g.gen_expr(expr)
						g.sb.writeln(';')
						return
					}
				}
				mut tuple_exprs := node.exprs.clone()
				if node.exprs.len == 1 && node.exprs[0] is ast.Tuple {
					tuple_expr := node.exprs[0] as ast.Tuple
					tuple_exprs = tuple_expr.exprs.clone()
				}
				field_types := g.tuple_aliases[g.cur_fn_ret_type] or { []string{} }
				g.sb.write_string('return ((${g.cur_fn_ret_type}){')
				for i, field_type in field_types {
					if i > 0 {
						g.sb.write_string(', ')
					}
					g.sb.write_string('.arg${i} = ')
					if i < tuple_exprs.len {
						g.gen_expr(tuple_exprs[i])
					} else {
						g.sb.write_string(zero_value_for_type(field_type))
					}
				}
				g.sb.writeln('});')
				return
			}
			if node.exprs.len == 1 && node.exprs[0] is ast.IfExpr {
				if_expr := node.exprs[0] as ast.IfExpr
				g.gen_return_if_expr(if_expr, false)
				return
			}
			if g.cur_fn_ret_type.starts_with('_option_') {
				if node.exprs.len == 0 {
					g.sb.writeln('return (${g.cur_fn_ret_type}){ .state = 2 };')
					return
				}
				expr := node.exprs[0]
				if expr is ast.BasicLiteral && expr.value == '0' {
					g.sb.writeln('return (${g.cur_fn_ret_type}){ .state = 2 };')
					return
				}
				if is_none_expr(expr) || expr is ast.Type {
					g.sb.writeln('return (${g.cur_fn_ret_type}){ .state = 2 };')
					return
				}
				expr_type := g.get_expr_type(expr)
				if expr is ast.Ident && expr.name == 'err' {
					g.sb.write_string('return (${g.cur_fn_ret_type}){ .is_error=true, .err=')
					g.gen_expr(expr)
					g.sb.writeln(' };')
					return
				}
				if expr_type == 'IError' {
					g.sb.write_string('return (${g.cur_fn_ret_type}){ .is_error=true, .err=')
					g.gen_expr(expr)
					g.sb.writeln(' };')
					return
				}
				if expr_type == g.cur_fn_ret_type {
					g.sb.write_string('return ')
					g.gen_expr(expr)
					g.sb.writeln(';')
					return
				}
				value_type := option_value_type(g.cur_fn_ret_type)
				if value_type == '' || value_type == 'void' {
					g.sb.writeln('return (${g.cur_fn_ret_type}){ .state = 2 };')
					return
				}
				if value_type in g.tuple_aliases {
					field_types := g.tuple_aliases[value_type]
					mut tuple_exprs := node.exprs.clone()
					if node.exprs.len == 1 && node.exprs[0] is ast.Tuple {
						tuple_expr := node.exprs[0] as ast.Tuple
						tuple_exprs = tuple_expr.exprs.clone()
					}
					g.sb.write_string('return ({ ${g.cur_fn_ret_type} _opt = (${g.cur_fn_ret_type}){ .state = 2 }; ${value_type} _val = (${value_type}){')
					for i, field_type in field_types {
						if i > 0 {
							g.sb.write_string(', ')
						}
						g.sb.write_string('.arg${i} = ')
						if i < tuple_exprs.len {
							g.gen_expr(tuple_exprs[i])
						} else {
							g.sb.write_string(zero_value_for_type(field_type))
						}
					}
					g.sb.writeln('}; _option_ok(&_val, (_option*)&_opt, sizeof(_val)); _opt; });')
					return
				}
				g.sb.write_string('return ({ ${g.cur_fn_ret_type} _opt = (${g.cur_fn_ret_type}){ .state = 2 }; ${value_type} _val = ')
				g.gen_expr(expr)
				g.sb.writeln('; _option_ok(&_val, (_option*)&_opt, sizeof(_val)); _opt; });')
				return
			}
			if g.cur_fn_ret_type.starts_with('_result_') {
				if node.exprs.len == 0 {
					g.sb.writeln('return (${g.cur_fn_ret_type}){ .is_error=false };')
					return
				}
				expr := node.exprs[0]
				if g.is_error_call_expr(expr) {
					g.sb.write_string('return (${g.cur_fn_ret_type}){ .is_error=true, .err=')
					g.gen_expr(expr)
					g.sb.writeln(' };')
					return
				}
				value_type := g.result_value_type(g.cur_fn_ret_type)
				if value_type in g.tuple_aliases && node.exprs.len > 1 {
					field_types := g.tuple_aliases[value_type]
					g.sb.write_string('return ({ ${g.cur_fn_ret_type} _res = (${g.cur_fn_ret_type}){0}; ${value_type} _val = (${value_type}){')
					for i, field_type in field_types {
						if i > 0 {
							g.sb.write_string(', ')
						}
						g.sb.write_string('.arg${i} = ')
						if i < node.exprs.len {
							g.gen_expr(node.exprs[i])
						} else {
							g.sb.write_string(zero_value_for_type(field_type))
						}
					}
					g.sb.writeln('}; _result_ok(&_val, (_result*)&_res, sizeof(_val)); _res; });')
					return
				}
				// `return T(x)` in `!T` functions can be an unlowered propagate path where `x` is already `_result_T`.
				// In that case return `x` directly, instead of casting to `T` and re-wrapping.
				if value_type != '' && expr is ast.CallOrCastExpr {
					if expr.lhs is ast.Ident && g.is_type_name(expr.lhs.name)
						&& g.expr_type_to_c(expr.lhs) == value_type {
						inner_type := g.get_expr_type(expr.expr)
						if inner_type == g.cur_fn_ret_type {
							g.sb.write_string('return ')
							g.gen_expr(expr.expr)
							g.sb.writeln(';')
							return
						}
					} else if expr.lhs is ast.Type && g.expr_type_to_c(expr.lhs) == value_type {
						inner_type := g.get_expr_type(expr.expr)
						if inner_type == g.cur_fn_ret_type {
							g.sb.write_string('return ')
							g.gen_expr(expr.expr)
							g.sb.writeln(';')
							return
						}
					}
				}
				if value_type != '' && expr is ast.CastExpr
					&& g.expr_type_to_c(expr.typ) == value_type {
					inner_type := g.get_expr_type(expr.expr)
					if inner_type == g.cur_fn_ret_type {
						g.sb.write_string('return ')
						g.gen_expr(expr.expr)
						g.sb.writeln(';')
						return
					}
				}
				expr_type := g.get_expr_type(expr)
				if expr_type == g.cur_fn_ret_type {
					g.sb.write_string('return ')
					g.gen_expr(expr)
					g.sb.writeln(';')
					return
				}
				if value_type == '' || value_type == 'void' {
					g.sb.writeln('return (${g.cur_fn_ret_type}){ .is_error=false };')
					return
				}
				g.sb.write_string('return ({ ${g.cur_fn_ret_type} _res = (${g.cur_fn_ret_type}){0}; ${value_type} _val = ')
				g.gen_expr(expr)
				g.sb.writeln('; _result_ok(&_val, (_result*)&_res, sizeof(_val)); _res; });')
				return
			}
			g.sb.write_string('return')
			if node.exprs.len > 0 {
				g.sb.write_string(' ')
				expr := node.exprs[0]
				if g.cur_fn_ret_type in g.sum_type_variants {
					g.gen_type_cast_expr(g.cur_fn_ret_type, expr)
				} else {
					g.gen_expr(expr)
				}
			} else if g.cur_fn_name == 'main' {
				g.sb.write_string(' 0')
			}
			g.sb.writeln(';')
		}
		ast.ForStmt {
			g.gen_for_stmt(node)
		}
		ast.FlowControlStmt {
			g.write_indent()
			if node.op == .key_break {
				g.sb.writeln('break;')
			} else if node.op == .key_continue {
				g.sb.writeln('continue;')
			} else if node.op == .key_goto {
				g.sb.writeln('goto ${node.label};')
			}
		}
		ast.ModuleStmt {
			g.cur_module = node.name.replace('.', '_')
		}
		ast.ImportStmt {}
		ast.ConstDecl {
			g.gen_const_decl(node)
		}
		ast.StructDecl {
			g.gen_struct_decl(node)
		}
		ast.EnumDecl {
			g.gen_enum_decl(node)
		}
		ast.TypeDecl {
			if node.variants.len > 0 {
				g.gen_sum_type_decl(node)
			} else if node.base_type !is ast.EmptyExpr {
				g.gen_type_alias(node)
			}
		}
		ast.InterfaceDecl {
			g.gen_interface_decl(node)
		}
		ast.GlobalDecl {
			g.gen_global_decl(node)
		}
		ast.Directive {
			g.write_indent()
			g.sb.writeln('/* [TODO] Directive: #${node.name} ${node.value} */')
		}
		ast.ForInStmt {
			panic('bug in v2 compiler: ForInStmt should have been lowered in v2.transformer')
		}
		ast.DeferStmt {
			panic('bug in v2 compiler: DeferStmt should have been lowered in v2.transformer')
		}
		ast.AssertStmt {
			g.write_indent()
			g.sb.writeln('/* [TODO] AssertStmt */')
		}
		ast.ComptimeStmt {
			panic('bug in v2 compiler: ComptimeStmt should have been handled in v2.transformer')
		}
		ast.BlockStmt {
			g.write_indent()
			g.sb.writeln('/* [TODO] BlockStmt */')
		}
		ast.LabelStmt {
			g.write_indent()
			g.sb.writeln('${node.name}:')
			if node.stmt !is ast.EmptyStmt {
				g.gen_stmt(node.stmt)
			}
		}
		ast.AsmStmt {
			g.write_indent()
			g.sb.writeln('/* [TODO] AsmStmt */')
		}
		[]ast.Attribute {}
		ast.EmptyStmt {}
		// else {}
	}
}

fn (mut g Gen) gen_global_decl(node ast.GlobalDecl) {
	for field in node.fields {
		name := if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
			'${g.cur_module}__${field.name}'
		} else {
			field.name
		}
		key := 'global_${name}'
		if key in g.emitted_types {
			continue
		}
		g.emitted_types[key] = true
		g.write_indent()
		if field.typ is ast.Type && field.typ is ast.ArrayFixedType {
			fixed_typ := field.typ as ast.ArrayFixedType
			elem_type := g.expr_type_to_c(fixed_typ.elem_type)
			g.fixed_array_globals[name] = true
			g.sb.write_string('${elem_type} ${name}[')
			g.gen_expr(fixed_typ.len)
			g.sb.write_string(']')
			if field.value !is ast.EmptyExpr {
				g.sb.write_string(' = ')
				g.gen_expr(field.value)
			}
			g.sb.writeln(';')
			continue
		}
		mut typ := ''
		if field.typ !is ast.EmptyExpr {
			typ = g.expr_type_to_c(field.typ)
		} else if field.value !is ast.EmptyExpr {
			typ = g.get_expr_type(field.value)
		}
		if typ == '' || typ == 'void' {
			typ = 'int'
		}
		g.sb.write_string('${typ} ${name}')
		if field.value !is ast.EmptyExpr {
			// Function calls are not compile-time constants in C
			if g.contains_call_expr(field.value) {
				g.sb.writeln(';')
			} else {
				g.sb.write_string(' = ')
				g.gen_expr(field.value)
				g.sb.writeln(';')
			}
		} else {
			g.sb.writeln(';')
		}
	}
}

fn (mut g Gen) gen_fn_decl(node ast.FnDecl) {
	// Generate V and .c.v function bodies, but skip JS and C extern declarations.
	if node.language == .js {
		return
	}
	if node.language == .c && node.stmts.len == 0 {
		return
	}
	// Skip generic functions - they have unresolved type params
	if node.typ.generic_params.len > 0 {
		return
	}
	fn_name := g.get_fn_name(node)
	if fn_name == '' {
		return
	}
	fn_key := 'fn_${fn_name}'
	if fn_key in g.emitted_types {
		return
	}
	g.emitted_types[fn_key] = true

	// Set function scope for type lookups
	g.cur_fn_name = node.name
	g.runtime_local_types = map[string]string{}
	g.cur_fn_ret_type = if node.name == 'main' {
		'int'
	} else if node.typ.return_type !is ast.EmptyExpr {
		g.expr_type_to_c(node.typ.return_type)
	} else {
		'void'
	}
	if g.env != unsafe { nil } {
		// For methods, the checker stores scopes using V-style receiver type names
		// (e.g. "[]string__free"), while get_fn_name returns C-style names
		// (e.g. "Array_string__free"). Use V-style name for scope lookup.
		scope_fn_name := if node.is_method {
			// The checker stores scopes using V-style receiver type names
			// (e.g. "[]string__free", "Builder__go_back"). Convert the AST
			// receiver type expression to match.
			v_type_name := g.receiver_type_to_scope_name(node.receiver.typ)
			if v_type_name != '' {
				'${v_type_name}__${node.name}'
			} else {
				fn_name
			}
		} else {
			node.name
		}
		if fn_scope := g.env.get_fn_scope(g.cur_module, scope_fn_name) {
			g.cur_fn_scope = fn_scope
		} else if node.is_method {
			// Fallback: for type aliases (e.g. Builder = []u8), the checker resolves
			// the alias and uses the underlying type name. Try env-based resolution.
			receiver_pos := node.receiver.typ.pos()
			mut found := false
			if receiver_pos != 0 {
				if recv_type := g.env.get_expr_type(receiver_pos) {
					base_type := recv_type.base_type()
					alt_scope_name := '${base_type.name()}__${node.name}'
					if fn_scope2 := g.env.get_fn_scope(g.cur_module, alt_scope_name) {
						g.cur_fn_scope = fn_scope2
						found = true
					}
				}
			}
			if !found {
				g.cur_fn_scope = unsafe { nil }
			}
		} else {
			g.cur_fn_scope = unsafe { nil }
		}
	}

	// Track mut parameter names for pointer detection
	g.cur_fn_mut_params = map[string]bool{}
	if node.is_method && node.receiver.name != '' && node.receiver.is_mut {
		g.cur_fn_mut_params[node.receiver.name] = true
	}
	for param in node.typ.params {
		if param.is_mut {
			g.cur_fn_mut_params[param.name] = true
		}
	}

	// Generate function header
	g.gen_fn_head(node)
	g.sb.writeln(' {')
	g.indent++

	// Main function: initialize argc/argv
	if node.name == 'main' {
		g.write_indent()
		g.sb.writeln('g_main_argc = ___argc;')
		g.write_indent()
		g.sb.writeln('g_main_argv = (void*)___argv;')
	}
	if fn_name == 'builder__Builder__gen_v_files' {
		g.write_indent()
		g.sb.writeln('v__Gen* gen = v__new_gen(b->pref);')
		g.write_indent()
		g.sb.writeln('for (int i = 0; i < b->files.len; i++) {')
		g.indent++
		g.write_indent()
		g.sb.writeln('ast__File file = ((ast__File*)b->files.data)[i];')
		g.write_indent()
		g.sb.writeln('v__Gen__gen(gen, file);')
		g.write_indent()
		g.sb.writeln('if (b->pref->debug) {')
		g.indent++
		g.write_indent()
		g.sb.writeln('v__Gen__print_output(gen);')
		g.indent--
		g.write_indent()
		g.sb.writeln('}')
		g.indent--
		g.write_indent()
		g.sb.writeln('}')
		g.indent--
		g.sb.writeln('}')
		g.sb.writeln('')
		return
	}

	g.gen_stmts(node.stmts)

	// Implicit return 0 for main
	if node.name == 'main' {
		g.write_indent()
		g.sb.writeln('return 0;')
	}

	g.indent--
	g.sb.writeln('}')
	g.sb.writeln('')
}

fn (mut g Gen) gen_fn_head(node ast.FnDecl) {
	mut ret := 'void'
	if node.typ.return_type !is ast.EmptyExpr {
		ret = g.expr_type_to_c(node.typ.return_type)
	}
	if node.name == 'main' {
		ret = 'int'
	}

	fn_name := g.get_fn_name(node)

	// main takes argc/argv
	if node.name == 'main' {
		g.sb.write_string('${ret} ${fn_name}(int ___argc, char** ___argv)')
		return
	}

	g.sb.write_string('${ret} ${fn_name}(')

	mut first := true
	// Receiver as first param for methods
	if node.is_method && node.receiver.name != '' {
		receiver_type := g.expr_type_to_c(node.receiver.typ)
		if node.receiver.is_mut {
			g.sb.write_string('${receiver_type}* ${node.receiver.name}')
		} else {
			g.sb.write_string('${receiver_type} ${node.receiver.name}')
		}
		first = false
	}

	for param in node.typ.params {
		if !first {
			g.sb.write_string(', ')
		}
		first = false
		t := g.expr_type_to_c(param.typ)
		if param.is_mut {
			g.sb.write_string('${t}* ${param.name}')
		} else {
			g.sb.write_string('${t} ${param.name}')
		}
	}
	g.sb.write_string(')')
}

fn (mut g Gen) get_fn_name(node ast.FnDecl) string {
	if node.name == 'main' {
		return 'main'
	}
	// Prevent collisions with libc symbols from builtin wrappers.
	if !node.is_method && node.name in c_stdlib_fns
		&& (g.cur_module == '' || g.cur_module == 'main' || g.cur_module == 'builtin') {
		return ''
	}
	name := sanitize_fn_ident(node.name)
	// Methods: ReceiverType__method_name
	if node.is_method && node.receiver.name != '' {
		// For pointer alias types (byteptr, charptr), use the V name directly
		// to avoid collisions. e.g., byteptr.str() -> byteptr__str, not u8__str
		// which would conflict with the actual u8.str() method.
		if node.receiver.typ is ast.Ident && node.receiver.typ.name in ['byteptr', 'charptr'] {
			return '${node.receiver.typ.name}__${name}'
		}
		receiver_type := g.expr_type_to_c(node.receiver.typ)
		// Strip pointer suffix for method naming
		base_type := if receiver_type.ends_with('*') {
			receiver_type[..receiver_type.len - 1]
		} else {
			receiver_type
		}
		return '${base_type}__${name}'
	}
	// Static methods: Type.method() -> Type__method
	if node.is_static {
		receiver_type := g.expr_type_to_c(node.receiver.typ)
		base_type := if receiver_type.ends_with('*') {
			receiver_type[..receiver_type.len - 1]
		} else {
			receiver_type
		}
		return '${base_type}__${name}'
	}
	if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
		return '${g.cur_module}__${name}'
	}
	return name
}

fn (mut g Gen) array_append_elem_type(lhs ast.Expr, rhs ast.Expr) (bool, string) {
	mut lhs_type := g.get_expr_type(lhs)
	mut elem_type := ''
	mut is_array_append := lhs_type == 'array' || lhs_type.starts_with('Array_')
	if lhs_type.starts_with('Array_') {
		elem_type = lhs_type['Array_'.len..].trim_right('*')
	}
	if raw_type := g.get_raw_type(lhs) {
		match raw_type {
			types.Array {
				is_array_append = true
				elem_type = g.types_type_to_c(raw_type.elem_type)
			}
			types.Alias {
				if raw_type.base_type is types.Array {
					is_array_append = true
					elem_type = g.types_type_to_c(raw_type.base_type.elem_type)
				}
			}
			types.Pointer {
				match raw_type.base_type {
					types.Array {
						is_array_append = true
						elem_type = g.types_type_to_c(raw_type.base_type.elem_type)
					}
					types.Alias {
						if raw_type.base_type.base_type is types.Array {
							is_array_append = true
							elem_type = g.types_type_to_c(raw_type.base_type.base_type.elem_type)
						}
					}
					else {}
				}
			}
			else {}
		}
	}
	if !is_array_append {
		return false, ''
	}
	if elem_type == '' || elem_type == 'int' {
		rhs_type := g.get_expr_type(rhs)
		if rhs_type != '' && rhs_type !in ['int_literal', 'float_literal'] {
			elem_type = rhs_type.trim_right('*')
		}
	}
	if elem_type == '' {
		elem_type = 'int'
	}
	return true, unmangle_c_ptr_type(elem_type)
}

fn (mut g Gen) expr_is_array_value(expr ast.Expr) bool {
	if expr is ast.ParenExpr {
		return g.expr_is_array_value(expr.expr)
	}
	if expr is ast.PrefixExpr && expr.op == .mul {
		// `*ptr_to_array` is an array value.
		return g.expr_is_array_value(expr.expr)
	}
	expr_type := g.get_expr_type(expr)
	if expr_type == 'array' || expr_type.starts_with('Array_') {
		return true
	}
	if raw_type := g.get_raw_type(expr) {
		match raw_type {
			types.Array {
				return true
			}
			types.Alias {
				return raw_type.base_type is types.Array
			}
			types.Pointer {
				match raw_type.base_type {
					types.Array {
						return true
					}
					types.Alias {
						return raw_type.base_type.base_type is types.Array
					}
					else {
						return false
					}
				}
			}
			else {
				return false
			}
		}
	}
	return false
}

fn (mut g Gen) expr_array_runtime_type(expr ast.Expr) string {
	if expr is ast.ParenExpr {
		return g.expr_array_runtime_type(expr.expr)
	}
	if expr is ast.PrefixExpr && expr.op == .mul {
		// Try direct dereferenced type first.
		deref_type := g.get_expr_type(expr)
		if deref_type != '' && deref_type != 'int_literal' && deref_type != 'float_literal' {
			return deref_type
		}
		inner_type := g.expr_array_runtime_type(expr.expr)
		if inner_type.ends_with('*') {
			return inner_type[..inner_type.len - 1]
		}
	}
	mut typ := g.get_expr_type(expr)
	if typ != '' && typ != 'int_literal' && typ != 'float_literal' {
		return typ
	}
	if raw_type := g.get_raw_type(expr) {
		typ = g.types_type_to_c(raw_type)
	}
	if typ == '' {
		return 'array'
	}
	return typ
}

fn (mut g Gen) gen_assign_stmt(node ast.AssignStmt) {
	lhs := node.lhs[0]
	rhs := node.rhs[0]

	// Multi-declaration with parallel RHS values:
	// `a, b := x, y` should declare both variables (not just the first one).
	if node.op == .decl_assign && node.lhs.len > 1 && node.rhs.len == node.lhs.len {
		for i, lhs_expr in node.lhs {
			rhs_expr := node.rhs[i]
			mut name := ''
			if lhs_expr is ast.Ident {
				name = lhs_expr.name
			} else if lhs_expr is ast.ModifierExpr && lhs_expr.expr is ast.Ident {
				name = lhs_expr.expr.name
			}
			if name == '_' {
				g.write_indent()
				g.sb.write_string('(void)(')
				g.gen_expr(rhs_expr)
				g.sb.writeln(');')
				continue
			}
			mut typ := g.get_expr_type(lhs_expr)
			if typ == '' || typ == 'int' {
				typ = g.get_expr_type(rhs_expr)
			}
			if typ == '' || typ == 'int_literal' {
				typ = 'int'
			}
			if typ == 'float_literal' {
				typ = 'f64'
			}
			g.write_indent()
			g.sb.write_string('${typ} ${name} = ')
			g.gen_expr(rhs_expr)
			g.sb.writeln(';')
			g.remember_runtime_local_type(name, typ)
		}
		return
	}

	mut tuple_lhs := []ast.Expr{}
	if node.lhs.len > 1 {
		tuple_lhs = node.lhs.clone()
	} else if node.lhs.len == 1 && node.lhs[0] is ast.Tuple {
		lhs_tuple := node.lhs[0] as ast.Tuple
		tuple_lhs = lhs_tuple.exprs.clone()
	}
	if tuple_lhs.len > 1 && node.rhs.len == 1 {
		mut tuple_type := g.get_expr_type(rhs)
		// For tuple-LHS assignments, prefer explicit call return metadata even when
		// positional inference produced a scalar type.
		if rhs is ast.CallExpr {
			if ret := g.get_call_return_type(rhs.lhs, rhs.args.len) {
				if ret != '' && ret != 'int' {
					tuple_type = ret
				}
			}
		} else if rhs is ast.CallOrCastExpr {
			if ret := g.get_call_return_type(rhs.lhs, 1) {
				if ret != '' && ret != 'int' {
					tuple_type = ret
				}
			}
		}
		if tuple_type == 'int' {
			if rhs is ast.CastExpr {
				cast_type := g.expr_type_to_c(rhs.typ)
				if cast_type != '' {
					tuple_type = cast_type
				}
			}
		}
		mut wrapped_tuple_type := ''
		if tuple_type.starts_with('_result_') {
			base := g.result_value_type(tuple_type)
			if base in g.tuple_aliases {
				wrapped_tuple_type = tuple_type
				tuple_type = base
			}
		} else if tuple_type.starts_with('_option_') {
			base := option_value_type(tuple_type)
			if base in g.tuple_aliases {
				wrapped_tuple_type = tuple_type
				tuple_type = base
			}
		}
		if field_types := g.tuple_aliases[tuple_type] {
			tmp_name := '_tuple_tmp_${g.tmp_counter}'
			g.tmp_counter++
			g.write_indent()
			if wrapped_tuple_type != '' {
				res_tmp := '_tuple_res_tmp_${g.tmp_counter}'
				g.tmp_counter++
				g.sb.write_string('${wrapped_tuple_type} ${res_tmp} = ')
				g.gen_expr(rhs)
				g.sb.writeln(';')
				g.write_indent()
				g.sb.writeln('${tuple_type} ${tmp_name} = (*(${tuple_type}*)(((u8*)(&${res_tmp}.err)) + sizeof(IError)));')
			} else {
				g.sb.write_string('${tuple_type} ${tmp_name} = ')
				g.gen_expr(rhs)
				g.sb.writeln(';')
			}
			for i, lhs_expr in tuple_lhs {
				g.write_indent()
				if node.op == .decl_assign {
					mut name := ''
					if lhs_expr is ast.Ident {
						name = lhs_expr.name
					} else if lhs_expr is ast.ModifierExpr && lhs_expr.expr is ast.Ident {
						name = lhs_expr.expr.name
					}
					if name == '_' {
						g.sb.writeln('(void)${tmp_name}.arg${i};')
						continue
					}
					elem_type := if i < field_types.len { field_types[i] } else { 'int' }
					g.sb.writeln('${elem_type} ${name} = ${tmp_name}.arg${i};')
					g.remember_runtime_local_type(name, elem_type)
				} else {
					g.gen_expr(lhs_expr)
					g.sb.writeln(' = ${tmp_name}.arg${i};')
				}
			}
			return
		}
	}

	// Check for blank identifier
	if lhs is ast.Ident && lhs.name == '_' {
		g.write_indent()
		g.sb.write_string('(void)(')
		g.gen_expr(rhs)
		g.sb.writeln(');')
		return
	}

	g.write_indent()
	if node.op == .decl_assign {
		// Variable declaration: type name = expr
		mut name := ''
		if lhs is ast.Ident {
			name = lhs.name
		} else if lhs is ast.ModifierExpr {
			if lhs.expr is ast.Ident {
				name = lhs.expr.name
			}
		}
		// Keep fixed-size arrays as C arrays in local declarations.
		if rhs is ast.ArrayInitExpr {
			array_init := rhs as ast.ArrayInitExpr
			if array_init.typ is ast.Type && array_init.typ is ast.ArrayFixedType {
				fixed_typ := array_init.typ as ast.ArrayFixedType
				elem_type := g.expr_type_to_c(fixed_typ.elem_type)
				g.sb.write_string('${elem_type} ${name}[')
				g.gen_expr(fixed_typ.len)
				g.sb.write_string('] = ')
				if array_init.exprs.len == 0 {
					g.sb.write_string('{0}')
				} else {
					g.sb.write_string('{')
					for i, expr in array_init.exprs {
						if i > 0 {
							g.sb.write_string(', ')
						}
						g.gen_expr(expr)
					}
					g.sb.write_string('}')
				}
				g.sb.writeln(';')
				return
			}
		}
		mut typ := g.get_expr_type(rhs)
		if rhs is ast.CallExpr {
			if rhs.lhs is ast.Ident && rhs.lhs.name in ['array__pop', 'array__pop_left'] {
				if rhs.args.len > 0 {
					pop_elem := g.infer_array_elem_type_from_expr(rhs.args[0])
					if pop_elem != '' {
						typ = pop_elem
					}
				}
			} else if rhs.lhs is ast.SelectorExpr && rhs.lhs.rhs.name in ['pop', 'pop_left'] {
				arr_expr := if rhs.args.len > 0 { rhs.args[0] } else { rhs.lhs.lhs }
				pop_elem := g.infer_array_elem_type_from_expr(arr_expr)
				if pop_elem != '' {
					typ = pop_elem
				}
			}
		} else if rhs is ast.CallOrCastExpr {
			if rhs.lhs is ast.Ident && rhs.lhs.name in ['array__pop', 'array__pop_left'] {
				pop_elem := g.infer_array_elem_type_from_expr(rhs.expr)
				if pop_elem != '' {
					typ = pop_elem
				}
			} else if rhs.lhs is ast.SelectorExpr && rhs.lhs.rhs.name in ['pop', 'pop_left'] {
				arr_expr := if rhs.expr is ast.EmptyExpr { rhs.lhs.lhs } else { rhs.expr }
				pop_elem := g.infer_array_elem_type_from_expr(arr_expr)
				if pop_elem != '' {
					typ = pop_elem
				}
			}
		}
		if typ in ['void*', 'voidptr'] {
			mut call_name := ''
			mut arr_expr := rhs
			mut has_arr_expr := false
			if rhs is ast.CallExpr {
				call_name = g.resolve_call_name(rhs.lhs, rhs.args.len)
				if rhs.args.len > 0 {
					arr_expr = rhs.args[0]
					has_arr_expr = true
				} else if rhs.lhs is ast.SelectorExpr {
					arr_expr = rhs.lhs.lhs
					has_arr_expr = true
				}
			} else if rhs is ast.CallOrCastExpr {
				call_name = g.resolve_call_name(rhs.lhs, 1)
				if rhs.expr !is ast.EmptyExpr {
					arr_expr = rhs.expr
					has_arr_expr = true
				}
				if !has_arr_expr && rhs.lhs is ast.SelectorExpr {
					arr_expr = rhs.lhs.lhs
					has_arr_expr = true
				}
			}
			if has_arr_expr && call_name in ['array__pop', 'array__pop_left'] {
				pop_elem := g.infer_array_elem_type_from_expr(arr_expr)
				if pop_elem != '' {
					typ = pop_elem
				}
			}
		}
		// Check scope-resolved type first (most reliable for declarations)
		if name != '' && g.cur_fn_scope != unsafe { nil } {
			if obj := g.cur_fn_scope.lookup_parent(name, 0) {
				if obj !is types.Module {
					obj_type := obj.typ()
					if obj_type !is types.Alias {
						scoped_type := g.types_type_to_c(obj_type)
						if (typ == '' || typ == 'int' || typ == 'int_literal') && scoped_type != ''
							&& scoped_type !in ['int', 'void'] {
							typ = scoped_type
						}
					}
				}
			}
		}
		lhs_typ := g.get_expr_type(lhs)
		if lhs_typ != '' && lhs_typ !in ['int', 'int_literal', 'float_literal'] && lhs_typ != 'void'
			&& (typ == '' || typ == 'int' || typ == 'void*' || typ == 'voidptr') {
			typ = lhs_typ
		}
		if rhs is ast.CallExpr {
			if ret := g.get_call_return_type(rhs.lhs, rhs.args.len) {
				if ret != '' && ret != 'int' {
					if !(ret in ['void*', 'voidptr'] && typ !in ['', 'int', 'void*', 'voidptr']) {
						typ = ret
					}
				}
			}
		} else if rhs is ast.CallOrCastExpr {
			if ret := g.get_call_return_type(rhs.lhs, 1) {
				if ret != '' && ret != 'int' {
					if !(ret in ['void*', 'voidptr'] && typ !in ['', 'int', 'void*', 'voidptr']) {
						typ = ret
					}
				}
			}
		}
		if rhs is ast.KeywordOperator && rhs.op in [.key_sizeof, .key_offsetof] {
			typ = 'usize'
		}
		mut rhs_type := g.get_expr_type(rhs)
		if rhs_type == 'int' {
			if rhs is ast.CallExpr {
				if ret := g.get_call_return_type(rhs.lhs, rhs.args.len) {
					rhs_type = ret
				}
			} else if rhs is ast.CallOrCastExpr {
				if ret := g.get_call_return_type(rhs.lhs, 1) {
					rhs_type = ret
				}
			}
		}
		if rhs is ast.SelectorExpr && rhs.rhs.name == 'err' {
			container_type := g.get_expr_type(rhs.lhs)
			is_or_tmp := rhs.lhs is ast.Ident && rhs.lhs.name.starts_with('_or_t')
			if container_type.starts_with('_result_') || container_type.starts_with('_option_')
				|| is_or_tmp {
				rhs_type = 'IError'
			}
		}
		if name != '' && rhs is ast.SelectorExpr && rhs.rhs.name == 'data' {
			container_type := g.get_expr_type(rhs.lhs)
			is_or_tmp := rhs.lhs is ast.Ident && rhs.lhs.name.starts_with('_or_t')
			if container_type.starts_with('_result_') || container_type.starts_with('_option_')
				|| is_or_tmp {
				cast_type := if typ != '' && typ != 'int_literal' && typ != 'float_literal' {
					typ
				} else if rhs_type != '' && rhs_type != 'int_literal' && rhs_type != 'float_literal' {
					rhs_type
				} else {
					'int'
				}
				g.sb.write_string('${cast_type} ${name} = (*(${cast_type}*)(((u8*)(&')
				g.gen_expr(rhs.lhs)
				g.sb.writeln('.err)) + sizeof(IError)));')
				g.remember_runtime_local_type(name, cast_type)
				return
			}
		}
		if (typ == '' || typ == 'int' || typ == 'int_literal' || typ == 'void*' || typ == 'voidptr')
			&& rhs_type != '' && rhs_type !in ['int', 'int_literal', 'float_literal']
			&& !rhs_type.starts_with('_result_') && !rhs_type.starts_with('_option_') {
			typ = rhs_type
		}
		if name != '' && rhs_type.starts_with('_result_') && !typ.starts_with('_result_') {
			g.sb.write_string('${typ} ${name} = ({ ${rhs_type} _tmp = ')
			g.gen_expr(rhs)
			g.sb.writeln('; (*(${typ}*)(((u8*)(&_tmp.err)) + sizeof(IError))); });')
			g.remember_runtime_local_type(name, typ)
			return
		}
		if name != '' && rhs_type.starts_with('_option_') && !typ.starts_with('_option_') {
			g.sb.write_string('${typ} ${name} = ({ ${rhs_type} _tmp = ')
			g.gen_expr(rhs)
			g.sb.writeln('; (*(${typ}*)(((u8*)(&_tmp.err)) + sizeof(IError))); });')
			g.remember_runtime_local_type(name, typ)
			return
		}
		if rhs is ast.IfExpr {
			if !g.if_expr_can_be_ternary(rhs) && rhs.else_expr !is ast.EmptyExpr {
				// If type is void/empty, infer from the branch's last expression
				if typ == 'void' || typ == '' {
					if rhs.stmts.len > 0 {
						last := rhs.stmts[rhs.stmts.len - 1]
						if last is ast.ExprStmt {
							branch_type := g.get_expr_type(last.expr)
							if branch_type != '' && branch_type != 'void' {
								typ = branch_type
							}
						}
					}
				}
				g.sb.writeln('${typ} ${name};')
				g.gen_decl_if_expr(name, rhs)
				return
			}
		}
		if typ.ends_with('**') && rhs is ast.PrefixExpr && rhs.op == .amp {
			g.sb.write_string('${typ} ${name} = ((${typ})(')
			g.gen_expr(rhs.expr)
			g.sb.writeln('));')
			return
		}
		if typ.ends_with('*') && rhs is ast.PrefixExpr && rhs.op == .amp {
			if rhs.expr is ast.CallExpr && rhs.expr.args.len == 1 {
				g.sb.write_string('${typ} ${name} = ((${typ})(')
				g.gen_expr(rhs.expr.args[0])
				g.sb.writeln('));')
				return
			}
			if rhs.expr is ast.CastExpr {
				g.sb.write_string('${typ} ${name} = ((${typ})(')
				g.gen_expr(rhs.expr.expr)
				g.sb.writeln('));')
				return
			}
			if rhs.expr is ast.CallOrCastExpr {
				g.sb.write_string('${typ} ${name} = ((${typ})(')
				g.gen_expr(rhs.expr.expr)
				g.sb.writeln('));')
				return
			}
			if rhs.expr is ast.ParenExpr {
				if rhs.expr.expr is ast.CallExpr && rhs.expr.expr.args.len == 1 {
					g.sb.write_string('${typ} ${name} = ((${typ})(')
					g.gen_expr(rhs.expr.expr.args[0])
					g.sb.writeln('));')
					return
				}
				if rhs.expr.expr is ast.CallOrCastExpr {
					g.sb.write_string('${typ} ${name} = ((${typ})(')
					g.gen_expr(rhs.expr.expr.expr)
					g.sb.writeln('));')
					return
				}
				if rhs.expr.expr is ast.CastExpr {
					g.sb.write_string('${typ} ${name} = ((${typ})(')
					g.gen_expr(rhs.expr.expr.expr)
					g.sb.writeln('));')
					return
				}
			}
		}
		if typ == '' || typ == 'void' {
			typ = 'int'
		}
		g.sb.write_string('${typ} ${name} = ')
		g.gen_expr(rhs)
		g.sb.writeln(';')
		g.remember_runtime_local_type(name, typ)
	} else {
		// Assignment
		if node.op == .left_shift_assign {
			is_array_append, elem_type := g.array_append_elem_type(lhs, rhs)
			if is_array_append {
				if g.expr_is_array_value(rhs) {
					rhs_tmp := '_arr_append_tmp_${g.tmp_counter}'
					g.tmp_counter++
					arr_rhs_type := g.expr_array_runtime_type(rhs)
					g.write_indent()
					g.sb.write_string('${arr_rhs_type} ${rhs_tmp} = ')
					g.gen_expr(rhs)
					g.sb.writeln(';')
					g.write_indent()
					g.sb.write_string('array__push_many((array*)')
					if g.expr_is_pointer(lhs) {
						g.gen_expr(lhs)
					} else {
						g.sb.write_string('&')
						g.gen_expr(lhs)
					}
					g.sb.writeln(', ${rhs_tmp}.data, ${rhs_tmp}.len);')
					return
				}
				g.write_indent()
				g.sb.write_string('array__push((array*)')
				if g.expr_is_pointer(lhs) {
					g.gen_expr(lhs)
				} else {
					g.sb.write_string('&')
					g.gen_expr(lhs)
				}
				g.sb.write_string(', ')
				g.gen_addr_of_expr(rhs, elem_type)
				g.sb.writeln(');')
				return
			}
		}
		if node.op == .assign && lhs is ast.Ident && g.get_local_var_c_type(lhs.name) == none
			&& !g.is_module_ident(lhs.name) && !g.is_module_local_const_or_global(lhs.name)
			&& lhs.name !in ['errno', 'stdin', 'stdout', 'stderr', 'environ'] {
			mut decl_type := g.get_expr_type(rhs)
			if decl_type == '' || decl_type in ['int_literal', 'float_literal'] {
				decl_type = 'int'
			}
			if decl_type in ['void', 'void*', 'voidptr'] {
				decl_type = 'int'
			}
			g.write_indent()
			g.sb.write_string('${decl_type} ${lhs.name} = ')
			g.gen_expr(rhs)
			g.sb.writeln(';')
			g.remember_runtime_local_type(lhs.name, decl_type)
			return
		}
		// Handle result/option .data field write: _t.data = val -> unwrapped value pointer = val
		if lhs is ast.SelectorExpr && lhs.rhs.name == 'data' {
			lhs_type := g.get_expr_type(lhs.lhs)
			is_or_tmp := lhs.lhs is ast.Ident && lhs.lhs.name.starts_with('_or_t')
			if lhs_type.starts_with('_result_') || lhs_type.starts_with('_option_') || is_or_tmp {
				base := if lhs_type.starts_with('_result_') {
					g.result_value_type(lhs_type)
				} else if lhs_type.starts_with('_option_') {
					option_value_type(lhs_type)
				} else {
					g.get_expr_type(rhs)
				}
				if base != '' && base != 'void' {
					g.write_indent()
					g.sb.write_string('(*(${base}*)(((u8*)(&')
					g.gen_expr(lhs.lhs)
					g.sb.write_string('.err)) + sizeof(IError))) = ')
					g.gen_expr(rhs)
					g.sb.writeln(';')
					return
				}
			}
		}
		mut lhs_needs_deref := false
		// Only dereference for plain assignment, not compound assignments (+=, -=, etc.)
		// For compound assignments on pointers (ptr += x), we want pointer arithmetic.
		if node.op == .assign && lhs is ast.Ident {
			if local_type := g.get_local_var_c_type(lhs.name) {
				if local_type.ends_with('*') {
					rhs_type := g.get_expr_type(rhs)
					// Only dereference if we're sure the RHS is not a pointer.
					// When rhs_type is '' or 'int' (unknown), skip deref for function
					// calls or unsafe blocks which may return pointers (e.g. malloc).
					rhs_is_ptr := rhs_type.ends_with('*') || rhs_type == 'voidptr'
						|| rhs_type == 'void*'
					if !rhs_is_ptr && rhs_type != '' && rhs_type != 'int' {
						lhs_needs_deref = true
					} else if !rhs_is_ptr && rhs !is ast.CallExpr && rhs !is ast.CallOrCastExpr
						&& rhs !is ast.UnsafeExpr {
						lhs_needs_deref = true
					}
				}
			}
		}
		if lhs_needs_deref {
			g.sb.write_string('*')
		}
		g.gen_expr(lhs)
		op_str := match node.op {
			.assign { '=' }
			.plus_assign { '+=' }
			.minus_assign { '-=' }
			.mul_assign { '*=' }
			.div_assign { '/=' }
			.mod_assign { '%=' }
			.and_assign { '&=' }
			.or_assign { '|=' }
			.xor_assign { '^=' }
			.left_shift_assign { '<<=' }
			.right_shift_assign { '>>=' }
			else { '=' }
		}
		g.sb.write_string(' ${op_str} ')
		g.gen_expr(rhs)
		g.sb.writeln(';')
	}
}

fn (mut g Gen) gen_decl_if_expr_branch(name string, stmts []ast.Stmt) {
	if stmts.len == 0 {
		return
	}
	for i, stmt in stmts {
		if i == stmts.len - 1 && stmt is ast.ExprStmt {
			if stmt.expr is ast.IfExpr {
				nested_if := stmt.expr as ast.IfExpr
				if !g.if_expr_can_be_ternary(nested_if) && nested_if.else_expr !is ast.EmptyExpr {
					g.gen_decl_if_expr(name, nested_if)
					continue
				}
			}
			g.write_indent()
			g.sb.write_string('${name} = ')
			g.gen_expr(stmt.expr)
			g.sb.writeln(';')
		} else {
			g.gen_stmt(stmt)
		}
	}
}

fn (mut g Gen) gen_decl_if_expr(name string, if_expr ast.IfExpr) {
	if if_expr.cond is ast.EmptyExpr {
		g.gen_decl_if_expr_branch(name, if_expr.stmts)
		return
	}
	g.write_indent()
	g.sb.write_string('if (')
	g.gen_expr(if_expr.cond)
	g.sb.writeln(') {')
	g.indent++
	g.gen_decl_if_expr_branch(name, if_expr.stmts)
	g.indent--
	g.write_indent()
	g.sb.write_string('}')
	if if_expr.else_expr is ast.IfExpr {
		else_if := if_expr.else_expr as ast.IfExpr
		if else_if.cond is ast.EmptyExpr {
			g.sb.writeln(' else {')
			g.indent++
			g.gen_decl_if_expr_branch(name, else_if.stmts)
			g.indent--
			g.write_indent()
			g.sb.writeln('}')
		} else {
			g.sb.writeln(' else {')
			g.indent++
			g.gen_decl_if_expr(name, else_if)
			g.indent--
			g.write_indent()
			g.sb.writeln('}')
		}
	} else if if_expr.else_expr !is ast.EmptyExpr {
		g.sb.writeln(' else {')
		g.indent++
		g.write_indent()
		g.sb.write_string('${name} = ')
		g.gen_expr(if_expr.else_expr)
		g.sb.writeln(';')
		g.indent--
		g.write_indent()
		g.sb.writeln('}')
	} else {
		g.sb.writeln('')
	}
}

fn (mut g Gen) gen_return_if_branch(stmts []ast.Stmt) {
	if stmts.len == 0 {
		return
	}
	for i, stmt in stmts {
		if i == stmts.len - 1 && stmt is ast.ExprStmt {
			if stmt.expr is ast.IfExpr {
				nested_if := stmt.expr as ast.IfExpr
				if !g.if_expr_can_be_ternary(nested_if) && nested_if.else_expr !is ast.EmptyExpr {
					g.gen_return_if_expr(nested_if, true)
					continue
				}
			}
			mut ret_exprs := []ast.Expr{cap: 1}
			ret_exprs << stmt.expr
			ret_stmt := ast.ReturnStmt{
				exprs: ret_exprs
			}
			g.gen_stmt(ret_stmt)
		} else {
			g.gen_stmt(stmt)
		}
	}
}

fn (mut g Gen) gen_return_if_expr(if_expr ast.IfExpr, emit_indent bool) {
	if if_expr.cond is ast.EmptyExpr {
		g.gen_return_if_branch(if_expr.stmts)
		return
	}
	if emit_indent {
		g.write_indent()
	}
	g.sb.write_string('if (')
	g.gen_expr(if_expr.cond)
	g.sb.writeln(') {')
	g.indent++
	g.gen_return_if_branch(if_expr.stmts)
	g.indent--
	g.write_indent()
	g.sb.write_string('}')
	if if_expr.else_expr is ast.IfExpr {
		else_if := if_expr.else_expr as ast.IfExpr
		if else_if.cond is ast.EmptyExpr {
			g.sb.writeln(' else {')
			g.indent++
			g.gen_return_if_branch(else_if.stmts)
			g.indent--
			g.write_indent()
			g.sb.writeln('}')
		} else {
			g.sb.writeln(' else {')
			g.indent++
			g.gen_return_if_expr(else_if, true)
			g.indent--
			g.write_indent()
			g.sb.writeln('}')
		}
	} else if if_expr.else_expr !is ast.EmptyExpr {
		g.sb.writeln(' else {')
		g.indent++
		mut else_exprs := []ast.Expr{cap: 1}
		else_exprs << if_expr.else_expr
		else_stmt := ast.ReturnStmt{
			exprs: else_exprs
		}
		g.gen_stmt(else_stmt)
		g.indent--
		g.write_indent()
		g.sb.writeln('}')
	} else {
		g.sb.writeln('')
	}
}

fn (mut g Gen) gen_for_stmt(node ast.ForStmt) {
	g.write_indent()
	has_init := !is_empty_stmt(node.init)
	has_cond := !is_empty_expr(node.cond)
	has_post := !is_empty_stmt(node.post)

	if has_init || has_post {
		// C-style for loop: for (init; cond; post)
		g.sb.write_string('for (')
		if has_init {
			g.gen_stmt_inline(node.init)
		}
		g.sb.write_string('; ')
		if has_cond {
			g.gen_expr(node.cond)
		}
		g.sb.write_string('; ')
		if has_post {
			g.gen_stmt_inline(node.post)
		}
		g.sb.writeln(') {')
	} else if has_cond {
		// while-style: for cond {
		g.sb.write_string('while (')
		g.gen_expr(node.cond)
		g.sb.writeln(') {')
	} else {
		// Infinite loop: for {
		g.sb.writeln('for (;;) {')
	}

	g.indent++
	g.gen_stmts(node.stmts)
	g.indent--
	g.write_indent()
	g.sb.writeln('}')
}

fn (mut g Gen) gen_stmt_inline(node ast.Stmt) {
	match node {
		ast.AssignStmt {
			lhs := node.lhs[0]
			rhs := node.rhs[0]
			if node.op == .decl_assign {
				mut name := ''
				if lhs is ast.Ident {
					name = lhs.name
				}
				typ := g.get_expr_type(rhs)
				g.sb.write_string('${typ} ${name} = ')
				g.gen_expr(rhs)
			} else {
				g.gen_expr(lhs)
				op_str := match node.op {
					.assign { '=' }
					.plus_assign { '+=' }
					.minus_assign { '-=' }
					else { '=' }
				}
				g.sb.write_string(' ${op_str} ')
				g.gen_expr(rhs)
			}
		}
		ast.ExprStmt {
			g.gen_expr(node.expr)
		}
		else {}
	}
}

const c_keywords = ['auto', 'break', 'case', 'char', 'const', 'continue', 'default', 'do', 'double',
	'else', 'enum', 'extern', 'float', 'for', 'goto', 'if', 'inline', 'int', 'long', 'register',
	'restrict', 'return', 'short', 'signed', 'sizeof', 'static', 'struct', 'switch', 'typedef',
	'union', 'unsigned', 'void', 'volatile', 'while', '_Bool', '_Complex', '_Imaginary']

const c_stdlib_fns = ['malloc', 'calloc', 'realloc', 'free', 'atoi', 'atof', 'atol', 'memcpy',
	'memset', 'memmove', 'strlen', 'strcpy', 'strcat', 'strcmp', 'memcmp', 'exit']

fn escape_c_keyword(name string) string {
	if name in c_keywords {
		return '_${name}'
	}
	return name
}

fn sanitize_fn_ident(name string) string {
	return match name {
		'+' { 'plus' }
		'-' { 'minus' }
		'*' { 'mul' }
		'/' { 'div' }
		'%' { 'mod' }
		'==' { 'eq' }
		'!=' { 'ne' }
		'<' { 'lt' }
		'>' { 'gt' }
		'<=' { 'le' }
		'>=' { 'ge' }
		'|' { 'pipe' }
		'^' { 'xor' }
		else { name }
	}
}

// Check if all non-pointer field types of a struct are already defined
fn (g &Gen) struct_fields_resolved(node ast.StructDecl) bool {
	// Check embedded types (used by value, not pointer)
	for emb in node.embedded {
		emb_name := g.field_type_name(emb)
		if emb_name != '' && emb_name !in primitive_types {
			emb_body_key := 'body_${emb_name}'
			emb_enum_key := 'enum_${emb_name}'
			emb_alias_key := 'alias_${emb_name}'
			if emb_body_key !in g.emitted_types && emb_enum_key !in g.emitted_types
				&& emb_alias_key !in g.emitted_types {
				return false
			}
		}
	}
	for field in node.fields {
		typ_name := g.field_type_name(field.typ)
		if typ_name == '' {
			continue
		}
		if field.typ is ast.Type {
			if field.typ is ast.OptionType {
				opt_typ := field.typ as ast.OptionType
				base_name := g.field_type_name(opt_typ.base_type)
				if base_name != '' && base_name != 'void'
					&& !g.option_result_payload_invalid(base_name) {
					wrapper_name := '_option_${mangle_alias_component(base_name)}'
					if wrapper_name !in g.emitted_option_structs {
						return false
					}
				}
			} else if field.typ is ast.ResultType {
				res_typ := field.typ as ast.ResultType
				base_name := g.field_type_name(res_typ.base_type)
				if base_name != '' && base_name != 'void'
					&& !g.option_result_payload_invalid(base_name) {
					wrapper_name := '_result_${mangle_alias_component(base_name)}'
					if wrapper_name !in g.emitted_result_structs {
						return false
					}
				}
			}
		}
		// Pointer types are fine with forward declarations
		if g.is_pointer_type(field.typ) {
			continue
		}
		// Primitive types are always resolved
		if typ_name in primitive_types {
			continue
		}
		// Check if this type's body has been emitted
		typ_body_key := 'body_${typ_name}'
		typ_enum_key := 'enum_${typ_name}'
		typ_alias_key := 'alias_${typ_name}'
		if typ_body_key !in g.emitted_types && typ_enum_key !in g.emitted_types
			&& typ_alias_key !in g.emitted_types {
			return false
		}
	}
	return true
}

fn (g &Gen) field_type_name(e ast.Expr) string {
	match e {
		ast.Ident {
			if g.is_module_local_type(e.name) {
				return '${g.cur_module}__${e.name}'
			}
			return e.name
		}
		ast.SelectorExpr {
			if e.lhs is ast.Ident {
				return '${e.lhs.name}__${e.rhs.name}'
			}
			return ''
		}
		ast.PrefixExpr {
			return g.field_type_name(e.expr)
		}
		ast.Type {
			if e is ast.ArrayType {
				return g.field_type_name(e.elem_type)
			}
			if e is ast.ArrayFixedType {
				return g.field_type_name(e.elem_type)
			}
			if e is ast.MapType {
				return 'map'
			}
			if e is ast.OptionType {
				return g.field_type_name(e.base_type)
			}
			if e is ast.ResultType {
				return g.field_type_name(e.base_type)
			}
			return ''
		}
		else {
			return ''
		}
	}
}

fn (g &Gen) contains_call_expr(e ast.Expr) bool {
	if e is ast.CallExpr {
		return true
	}
	if e is ast.CastExpr {
		return g.contains_call_expr(e.expr)
	}
	if e is ast.ParenExpr {
		return g.contains_call_expr(e.expr)
	}
	if e is ast.CallOrCastExpr {
		return g.contains_call_expr(e.expr)
	}
	return false
}

fn (g &Gen) is_pointer_type(e ast.Expr) bool {
	if e is ast.PrefixExpr {
		return e.op == .amp
	}
	if e is ast.Ident {
		return e.name in ['voidptr', 'charptr', 'byteptr']
	}
	return false
}

fn (mut g Gen) get_struct_name(node ast.StructDecl) string {
	if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
		return '${g.cur_module}__${node.name}'
	}
	return node.name
}

fn (mut g Gen) gen_struct_decl(node ast.StructDecl) {
	// Skip C extern struct declarations
	if node.language == .c {
		return
	}

	name := g.get_struct_name(node)
	body_key := 'body_${name}'
	if body_key in g.emitted_types {
		return
	}
	g.emitted_types[body_key] = true
	keyword := if node.is_union { 'union' } else { 'struct' }
	// Try to get the resolved struct type from the Environment
	env_struct := g.lookup_struct_type(node.name)

	// Use named struct to match the forward declaration: typedef struct name name;
	g.sb.writeln('${keyword} ${name} {')
	// Embedded structs as fields
	for i, emb in node.embedded {
		emb_type := g.expr_type_to_c(emb)
		g.sb.writeln('\t${emb_type} ${emb_type};')
		if i < env_struct.embedded.len {
			embedded := env_struct.embedded[i]
			for ef in embedded.fields {
				key := name + '.' + ef.name
				g.embedded_field_owner[key] = emb_type
				embedded_field_type := g.types_type_to_c(ef.typ)
				g.struct_field_types[key] = embedded_field_type
				if name.contains('__') {
					short_key := name.all_after_last('__') + '.' + ef.name
					g.embedded_field_owner[short_key] = emb_type
					g.struct_field_types[short_key] = embedded_field_type
				}
			}
		}
	}
	// Regular fields
	mut has_shared_fields := false
	for field in node.fields {
		field_name := escape_c_keyword(field.name)
		field_lookup_type := g.expr_type_to_c(field.typ)
		field_key := '${name}.${field.name}'
		g.struct_field_types[field_key] = field_lookup_type
		if name.contains('__') {
			short_field_key := '${name.all_after_last('__')}.${field.name}'
			g.struct_field_types[short_field_key] = field_lookup_type
		}
		if field.typ is ast.Type && field.typ is ast.ArrayFixedType {
			fixed_typ := field.typ as ast.ArrayFixedType
			elem_type := g.expr_type_to_c(fixed_typ.elem_type)
			// Use the resolved array size from the Environment if available
			mut resolved_len := -1
			for ef in env_struct.fields {
				if ef.name == field.name {
					if ef.typ is types.ArrayFixed {
						resolved_len = ef.typ.len
					}
					break
				}
			}
			g.sb.write_string('\t${elem_type} ${field_name}[')
			if resolved_len > 0 {
				g.sb.write_string('${resolved_len}')
			} else {
				g.gen_expr(fixed_typ.len)
			}
			g.sb.writeln('];')
			continue
		}
		// Check for shared modifier
		if field.typ is ast.ModifierExpr && field.typ.kind == .key_shared {
			has_shared_fields = true
		}
		field_type := field_lookup_type
		g.sb.writeln('\t${field_type} ${field_name};')
	}
	// Add mutex field for shared fields
	if has_shared_fields {
		g.sb.writeln('\tsync__RwMutex mtx;')
	}
	if node.embedded.len == 0 && node.fields.len == 0 {
		g.sb.writeln('\tu8 _dummy;')
	}
	g.sb.writeln('};')
	g.sb.writeln('')
}

fn (mut g Gen) lookup_struct_type(struct_name string) types.Struct {
	if g.env == unsafe { nil } {
		return types.Struct{}
	}
	mod_name := if g.cur_module != '' { g.cur_module } else { 'main' }
	mut out := types.Struct{}
	if scope := g.env_scope(mod_name) {
		if obj := scope.objects[struct_name] {
			typ := obj.typ()
			if typ is types.Struct {
				out = typ
			}
		}
	}
	return out
}

fn (mut g Gen) get_enum_name(node ast.EnumDecl) string {
	if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
		return '${g.cur_module}__${node.name}'
	}
	return node.name
}

fn (mut g Gen) gen_enum_decl(node ast.EnumDecl) {
	name := g.get_enum_name(node)
	enum_key := 'enum_${name}'
	if enum_key in g.emitted_types {
		return
	}
	g.emitted_types[enum_key] = true
	mut is_flag := false
	for attribute in node.attributes {
		if attribute.name == 'flag' {
			is_flag = true
			break
		}
		if attribute.name == '' && attribute.value is ast.Ident && attribute.value.name == 'flag' {
			is_flag = true
			break
		}
	}

	g.sb.writeln('typedef enum {')
	for i, field in node.fields {
		g.sb.write_string('\t${name}__${field.name}')
		if field.value !is ast.EmptyExpr {
			g.sb.write_string(' = ')
			g.gen_expr(field.value)
		} else if is_flag {
			g.sb.write_string(' = ${u64(1) << i}U')
		}
		if i < node.fields.len - 1 {
			g.sb.writeln(',')
		} else {
			g.sb.writeln('')
		}
	}
	g.sb.writeln('} ${name};')
	g.sb.writeln('')
	enum_str_fn := '${name}__str'
	if enum_str_fn !in g.fn_return_types {
		g.sb.writeln('#define ${name}__str(v) int__str((int)(v))')
	}
	enum_short_str_fn := '${name}_str'
	if enum_short_str_fn !in g.fn_return_types {
		g.sb.writeln('#define ${name}_str(v) ${name}__str(v)')
	}
	g.sb.writeln('')
}

fn (mut g Gen) get_type_decl_name(node ast.TypeDecl) string {
	if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
		return '${g.cur_module}__${node.name}'
	}
	return node.name
}

fn (mut g Gen) gen_type_alias(node ast.TypeDecl) {
	name := g.get_type_decl_name(node)
	// System-provided typedefs should not be redefined by generated builtin aliases.
	if name in ['intptr_t', 'uintptr_t'] {
		return
	}
	alias_key := 'alias_${name}'
	if alias_key in g.emitted_types {
		return
	}
	g.emitted_types[alias_key] = true

	// Check if base type is a function type - needs special syntax
	if node.base_type is ast.Type {
		if node.base_type is ast.FnType {
			fn_type := node.base_type as ast.FnType
			mut ret_type := 'void'
			if fn_type.return_type !is ast.EmptyExpr {
				ret_type = g.expr_type_to_c(fn_type.return_type)
			}
			g.sb.write_string('typedef ${ret_type} (*${name})(')
			for i, param in fn_type.params {
				if i > 0 {
					g.sb.write_string(', ')
				}
				param_type := g.expr_type_to_c(param.typ)
				if param.is_mut {
					g.sb.write_string('${param_type}*')
				} else {
					g.sb.write_string(param_type)
				}
			}
			g.sb.writeln(');')
			return
		}
	}
	base_type := g.expr_type_to_c(node.base_type)
	if base_type in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64',
		'bool', 'rune', 'byte', 'usize', 'isize'] {
		g.primitive_type_aliases[name] = true
	}
	g.sb.writeln('typedef ${base_type} ${name};')
}

fn (mut g Gen) gen_sum_type_decl(node ast.TypeDecl) {
	name := g.get_type_decl_name(node)
	body_key := 'body_${name}'
	if body_key in g.emitted_types {
		return
	}
	g.emitted_types[body_key] = true

	// Track variant names for sum type cast generation
	mut variant_names := []string{}
	for i, variant in node.variants {
		vname := g.get_variant_field_name(variant, i)
		// Strip leading underscore from field name to get the type name
		variant_names << if vname.len > 1 && vname[0] == `_` { vname[1..] } else { vname }
	}
	g.sum_type_variants[name] = variant_names

	g.sb.writeln('struct ${name} {')
	g.sb.writeln('\tint _tag;')
	g.sb.writeln('\tunion {')
	for i, variant in node.variants {
		variant_name := g.get_variant_field_name(variant, i)
		g.sb.writeln('\t\tvoid* ${variant_name};')
	}
	g.sb.writeln('\t} _data;')
	g.sb.writeln('};')
	g.sb.writeln('')
}

fn (g &Gen) get_variant_field_name(variant ast.Expr, idx int) string {
	if variant is ast.Ident {
		return '_${variant.name}'
	} else if variant is ast.SelectorExpr {
		if variant.lhs is ast.Ident {
			return '_${variant.lhs.name}__${variant.rhs.name}'
		}
		return '_${variant.rhs.name}'
	} else if variant is ast.Type {
		if variant is ast.ArrayType {
			elem := mangle_alias_component(g.field_type_name(variant.elem_type))
			return '_Array_${elem}'
		}
		if variant is ast.MapType {
			key := mangle_alias_component(g.field_type_name(variant.key_type))
			val := mangle_alias_component(g.field_type_name(variant.value_type))
			return '_Map_${key}_${val}'
		}
	}
	return '_v${idx}'
}

fn (mut g Gen) get_interface_name(node ast.InterfaceDecl) string {
	if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
		return '${g.cur_module}__${node.name}'
	}
	return node.name
}

fn (mut g Gen) gen_interface_decl(node ast.InterfaceDecl) {
	name := g.get_interface_name(node)
	body_key := 'body_${name}'
	if body_key in g.emitted_types {
		return
	}
	g.emitted_types[body_key] = true

	g.sb.writeln('struct ${name} {')
	g.sb.writeln('\tvoid* _object;')
	g.sb.writeln('\tint _type_id;')
	mut has_type_name := false
	for field in node.fields {
		if field.name == 'type_name' {
			has_type_name = true
			break
		}
	}
	if !has_type_name {
		g.sb.writeln('\tstring (*type_name)(void*);')
	}
	// Generate function pointers for each method
	mut methods := []InterfaceMethodInfo{}
	for field in node.fields {
		if fn_type := g.get_fn_type_from_expr(field.typ) {
			mut ret := 'void'
			if fn_type.return_type !is ast.EmptyExpr {
				ret = g.expr_type_to_c(fn_type.return_type)
			}
			mut param_types := []string{}
			g.sb.write_string('\t${ret} (*${field.name})(void*')
			mut cast_sig := '${ret} (*)(void*'
			for param in fn_type.params {
				g.sb.write_string(', ')
				t := g.expr_type_to_c(param.typ)
				g.sb.write_string(t)
				cast_sig += ', ${t}'
				param_types << t
			}
			g.sb.writeln(');')
			cast_sig += ')'
			methods << InterfaceMethodInfo{
				name:           field.name
				cast_signature: cast_sig
				ret_type:       ret
				param_types:    param_types
			}
		} else {
			// Regular field
			t := g.expr_type_to_c(field.typ)
			g.sb.writeln('\t${t} ${field.name};')
		}
	}
	g.interface_methods[name] = methods
	g.sb.writeln('};')
	g.sb.writeln('')
}

// gen_interface_cast generates an interface struct initialization from a concrete type.
// Returns true if the type_name is an interface and the cast was generated.
fn (mut g Gen) gen_interface_cast(type_name string, value_expr ast.Expr) bool {
	// Look up the type in the environment to check if it's an interface
	if g.env == unsafe { nil } {
		return false
	}
	mut is_iface := false
	if mut scope := g.env_scope(g.cur_module) {
		if obj := scope.lookup_parent(type_name, 0) {
			if obj is types.Type && obj is types.Interface {
				is_iface = true
			}
		}
	}
	if !is_iface {
		return false
	}
	// Get the concrete type name
	concrete_type := g.get_expr_type(value_expr)
	if concrete_type == '' || concrete_type == 'int' {
		return false
	}
	// Strip pointer suffix for method name construction
	base_concrete := if concrete_type.ends_with('*') {
		concrete_type[..concrete_type.len - 1]
	} else {
		concrete_type
	}
	// Generate: (InterfaceType){._object = (void*)&expr, .method = ConcreteType__method, ...}
	g.sb.write_string('((${type_name}){._object = ')
	if concrete_type.ends_with('*') {
		// Value is already a pointer-compatible receiver.
		g.sb.write_string('(void*)(')
		g.gen_expr(value_expr)
		g.sb.write_string(')')
	} else {
		g.sb.write_string('(void*)&(')
		g.gen_expr(value_expr)
		g.sb.write_string(')')
	}
	type_short := if base_concrete.contains('__') {
		base_concrete.all_after_last('__')
	} else {
		base_concrete
	}
	type_id := interface_type_id_for_name(type_short)
	g.sb.write_string(', ._type_id = ${type_id}')
	// Generate method function pointers from stored interface info
	if methods := g.interface_methods[type_name] {
		for method in methods {
			fn_name := '${base_concrete}__${method.name}'
			mut target_name := fn_name
			if ptr_params := g.fn_param_is_ptr[fn_name] {
				if ptr_params.len > 0 && !ptr_params[0] {
					target_name = interface_wrapper_name(type_name, base_concrete, method.name)
				}
			}
			g.sb.write_string(', .${method.name} = (${method.cast_signature})${target_name}')
		}
	}
	g.sb.write_string('})')
	return true
}

fn (g &Gen) is_enum_type(name string) bool {
	// Check emitted_types for enum_Name or enum_module__Name
	enum_key := 'enum_${name}'
	if enum_key in g.emitted_types {
		return true
	}
	qualified := g.get_qualified_name(name)
	qualified_enum_key := 'enum_${qualified}'
	if qualified_enum_key in g.emitted_types {
		return true
	}
	// Also check the types.Environment
	if g.env != unsafe { nil } {
		mut found := false
		if mut scope := g.env_scope(g.cur_module) {
			if obj := scope.lookup_parent(name, 0) {
				if obj is types.Type && obj is types.Enum {
					found = true
				}
			}
		}
		if found {
			return true
		}
	}
	return false
}

fn (g &Gen) get_qualified_name(name string) string {
	if name.contains('__') {
		return name
	}
	if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
		return '${g.cur_module}__${name}'
	}
	return name
}

fn (g &Gen) normalize_enum_name(name string) string {
	if g.cur_module != '' {
		double_prefix := '${g.cur_module}__${g.cur_module}__'
		if name.starts_with(double_prefix) {
			return name[g.cur_module.len + 2..]
		}
	}
	return name
}

fn (g &Gen) enum_has_field(enum_name string, field_name string) bool {
	if fields := g.enum_type_fields[enum_name] {
		if field_name in fields {
			return true
		}
	}
	normalized := g.normalize_enum_name(enum_name)
	if fields := g.enum_type_fields[normalized] {
		if field_name in fields {
			return true
		}
	}
	if enum_name.contains('__') {
		short_name := enum_name.all_after_last('__')
		if fields := g.enum_type_fields[short_name] {
			if field_name in fields {
				return true
			}
		}
	}
	return false
}

fn (g &Gen) is_type_name(name string) bool {
	if name in primitive_types {
		return true
	}
	// Check if it's a known emitted type (enum, struct, alias, sum type, interface)
	qualified := g.get_qualified_name(name)
	enum_key := 'enum_${name}'
	qualified_enum_key := 'enum_${qualified}'
	if enum_key in g.emitted_types || qualified_enum_key in g.emitted_types {
		return true
	}
	body_key := 'body_${name}'
	qualified_body_key := 'body_${qualified}'
	if body_key in g.emitted_types || qualified_body_key in g.emitted_types {
		return true
	}
	alias_key := 'alias_${name}'
	qualified_alias_key := 'alias_${qualified}'
	if alias_key in g.emitted_types || qualified_alias_key in g.emitted_types {
		return true
	}
	if name in g.emitted_types || qualified in g.emitted_types {
		return true
	}
	return false
}

fn (mut g Gen) is_module_ident(name string) bool {
	if g.cur_fn_scope != unsafe { nil } {
		if obj := g.cur_fn_scope.lookup_parent(name, 0) {
			return obj is types.Module
		}
	}
	if g.env != unsafe { nil } {
		mut found := false
		if mut scope := g.env_scope(g.cur_module) {
			if obj := scope.lookup_parent(name, 0) {
				found = obj is types.Module
			}
		}
		if found {
			return true
		}
	}
	return false
}

fn sanitize_c_number_literal(lit string) string {
	mut s := lit
	if s.contains('_') {
		s = s.replace('_', '')
	}
	// Convert V octal prefix 0o to C octal prefix 0
	if s.starts_with('0o') || s.starts_with('0O') {
		s = '0${s[2..]}'
	}
	return s
}

fn strip_literal_quotes(raw string) string {
	if raw.len < 2 {
		return raw
	}
	first := raw[0]
	last := raw[raw.len - 1]
	if first == last && first in [`'`, `"`] {
		return raw[1..raw.len - 1]
	}
	return raw
}

fn escape_char_literal_content(raw string) string {
	mut sb := strings.new_builder(raw.len + 4)
	for ch in raw {
		if ch == `'` {
			sb.write_u8(`\\`)
			sb.write_u8(`'`)
		} else {
			sb.write_u8(ch)
		}
	}
	return sb.str()
}

fn escape_c_string_literal_content(raw string, kind ast.StringLiteralKind) string {
	mut sb := strings.new_builder(raw.len + 8)
	for ch in raw {
		match ch {
			`"` {
				sb.write_u8(`\\`)
				sb.write_u8(`"`)
			}
			`\n` {
				sb.write_string('\\n')
			}
			`\r` {
				sb.write_string('\\r')
			}
			`\t` {
				sb.write_string('\\t')
			}
			`\\` {
				if kind == .raw {
					sb.write_string('\\\\')
				} else {
					sb.write_u8(`\\`)
				}
			}
			else {
				sb.write_u8(ch)
			}
		}
	}
	return sb.str()
}

fn expr_to_int_str(e ast.Expr) string {
	if e is ast.BasicLiteral {
		return e.value
	}
	return '0'
}

fn mangle_alias_component(name string) string {
	mut s := name.replace('*', 'ptr')
	s = s.replace('&', 'ref')
	s = s.replace(' ', '_')
	s = s.replace('.', '__')
	return s
}

fn zero_value_for_type(t string) string {
	trimmed := t.trim_space()
	if trimmed == '' {
		return '0'
	}
	if trimmed == 'string' {
		return '(string){"", 0}'
	}
	if trimmed.ends_with('*') {
		return '0'
	}
	if trimmed in primitive_types || trimmed in ['void*', 'char*', 'byteptr', 'charptr', 'voidptr'] {
		return '0'
	}
	return '((${trimmed}){0})'
}

fn map_get_default_ptr_for_type(t string) string {
	trimmed := t.trim_space()
	if trimmed == '' {
		return '&(int){0}'
	}
	if trimmed == 'string' {
		return '&(string){"", 0}'
	}
	return '&(${trimmed}){0}'
}

fn split_top_level_csv(s string) []string {
	mut parts := []string{}
	mut start := 0
	mut depth := 0
	for i, ch in s {
		if ch == `(` || ch == `[` || ch == `<` {
			depth++
		} else if ch == `)` || ch == `]` || ch == `>` {
			if depth > 0 {
				depth--
			}
		} else if ch == `,` && depth == 0 {
			part := s[start..i].trim_space()
			if part != '' {
				parts << part
			}
			start = i + 1
		}
	}
	last := s[start..].trim_space()
	if last != '' {
		parts << last
	}
	return parts
}

fn (mut g Gen) c_type_from_type_name(type_name string) string {
	name := type_name.trim_space()
	if name == '' {
		return 'int'
	}
	if name.starts_with('tuple (') && name.ends_with(')') {
		inner := name['tuple ('.len..name.len - 1]
		mut elem_types := []string{}
		for part in split_top_level_csv(inner) {
			elem_types << g.c_type_from_type_name(part)
		}
		return g.register_tuple_alias(elem_types)
	}
	if name.starts_with('&') {
		return g.c_type_from_type_name(name[1..]) + '*'
	}
	if name.starts_with('[]') {
		elem := g.c_type_from_type_name(name[2..])
		return 'Array_${mangle_alias_component(elem)}'
	}
	if name.contains('::') {
		return name.replace('::', '__')
	}
	return name.replace('.', '__')
}

fn (mut g Gen) register_tuple_alias(elem_types []string) string {
	if elem_types.len == 0 {
		return 'int'
	}
	mut parts := []string{cap: elem_types.len}
	for elem in elem_types {
		parts << mangle_alias_component(elem)
	}
	name := 'Tuple_${parts.join('_')}'
	if name !in g.tuple_aliases {
		cloned_elem_types := elem_types.clone()
		g.tuple_aliases[name] = cloned_elem_types
	}
	return name
}

fn (g &Gen) is_tuple_alias(t string) bool {
	return t in g.tuple_aliases
}

fn (mut g Gen) emit_tuple_aliases() {
	mut names := g.tuple_aliases.keys()
	names.sort()
	for name in names {
		body_key := 'body_${name}'
		if body_key in g.emitted_types {
			continue
		}
		g.emitted_types[body_key] = true
		field_types := g.tuple_aliases[name]
		g.sb.writeln('typedef struct ${name} {')
		for i, field_type in field_types {
			g.sb.writeln('\t${field_type} arg${i};')
		}
		g.sb.writeln('} ${name};')
	}
}

fn (mut g Gen) gen_unwrapped_value_expr(expr ast.Expr) bool {
	expr_type := g.get_expr_type(expr)
	if expr_type.starts_with('_result_') {
		base := g.result_value_type(expr_type)
		if base != '' && base != 'void' {
			is_addressable := match expr {
				ast.Ident, ast.SelectorExpr, ast.IndexExpr {
					true
				}
				else {
					false
				}
			}
			if is_addressable {
				g.sb.write_string('(*(${base}*)(((u8*)(&')
				g.gen_expr(expr)
				g.sb.write_string('.err)) + sizeof(IError)))')
			} else {
				g.sb.write_string('({ ${expr_type} _tmp = ')
				g.gen_expr(expr)
				g.sb.write_string('; (*(${base}*)(((u8*)(&_tmp.err)) + sizeof(IError))); })')
			}
			return true
		}
	}
	if expr_type.starts_with('_option_') {
		base := option_value_type(expr_type)
		if base != '' && base != 'void' {
			is_addressable := match expr {
				ast.Ident, ast.SelectorExpr, ast.IndexExpr {
					true
				}
				else {
					false
				}
			}
			if is_addressable {
				g.sb.write_string('(*(${base}*)(((u8*)(&')
				g.gen_expr(expr)
				g.sb.write_string('.err)) + sizeof(IError)))')
			} else {
				g.sb.write_string('({ ${expr_type} _tmp = ')
				g.gen_expr(expr)
				g.sb.write_string('; (*(${base}*)(((u8*)(&_tmp.err)) + sizeof(IError))); })')
			}
			return true
		}
	}
	return false
}

// Helper to extract FnType from an Expr (handles ast.Type wrapping)
fn (g Gen) get_fn_type_from_expr(e ast.Expr) ?ast.FnType {
	if e is ast.Type {
		if e is ast.FnType {
			return e
		}
	}
	return none
}

fn (mut g Gen) gen_expr(node ast.Expr) {
	match node {
		ast.BasicLiteral {
			if node.kind == .key_true {
				g.sb.write_string('true')
			} else if node.kind == .key_false {
				g.sb.write_string('false')
			} else if node.kind == .char {
				raw := strip_literal_quotes(node.value)
				if raw.len > 1 && raw[0] != `\\` {
					// Multi-byte UTF-8 character: emit as numeric codepoint
					runes := raw.runes()
					if runes.len > 0 {
						g.sb.write_string(int(runes[0]).str())
					} else {
						g.sb.write_string("'${raw}'")
					}
				} else {
					escaped := escape_char_literal_content(raw)
					g.sb.write_u8(`'`)
					g.sb.write_string(escaped)
					g.sb.write_u8(`'`)
				}
			} else {
				g.sb.write_string(sanitize_c_number_literal(node.value))
			}
		}
		ast.StringLiteral {
			val := strip_literal_quotes(node.value)
			escaped := escape_c_string_literal_content(val, node.kind)
			if node.kind == .c {
				// C string literal: emit raw C string
				g.sb.write_u8(`"`)
				g.sb.write_string(escaped)
				g.sb.write_u8(`"`)
			} else {
				// Use sizeof on the emitted C literal so escape sequences (e.g. `\t`)
				// get the correct runtime byte length.
				g.sb.write_string('(string){')
				g.sb.write_u8(`"`)
				g.sb.write_string(escaped)
				g.sb.write_u8(`"`)
				g.sb.write_string(', sizeof(')
				g.sb.write_u8(`"`)
				g.sb.write_string(escaped)
				g.sb.write_u8(`"`)
				g.sb.write_string(') - 1}')
			}
		}
		ast.Ident {
			if node.name == 'nil' {
				g.sb.write_string('NULL')
			} else if node.name == '@FN' || node.name == '@METHOD' || node.name == '@FUNCTION' {
				fn_name := g.cur_fn_name
				g.sb.write_string('(string){"${fn_name}", ${fn_name.len}}')
			} else if node.name == '@MOD' {
				mod_name := g.cur_module
				g.sb.write_string('(string){"${mod_name}", ${mod_name.len}}')
			} else if node.name == '@FILE' {
				g.sb.write_string('(string){__FILE__, sizeof(__FILE__)-1}')
			} else if node.name == '@LINE' {
				g.sb.write_string('__LINE__')
			} else if node.name == '@VCURRENTHASH' || node.name == '@VHASH' {
				hash_name := if node.name == '@VHASH' { 'VHASH' } else { 'VCURRENTHASH' }
				g.sb.write_string('(string){"${hash_name}", ${hash_name.len}}')
			} else if node.name == '@VEXE' {
				g.sb.write_string('(string){"", 0}')
			} else if node.name.starts_with('__type_id_') {
				type_name := node.name['__type_id_'.len..]
				type_id := interface_type_id_for_name(type_name)
				g.sb.write_string('${type_id}')
			} else {
				// Check global_var_modules first - globals may appear as types.Type in scope
				// instead of types.Global, so is_local_var check would incorrectly block them
				if node.name in g.global_var_modules
					&& g.global_var_modules[node.name] == g.cur_module {
					g.sb.write_string('${g.cur_module}__${node.name}')
				} else {
					is_local_var := g.get_local_var_c_type(node.name) != none
					const_key := 'const_${g.cur_module}__${node.name}'
					global_key := 'global_${g.cur_module}__${node.name}'
					if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin'
						&& !node.name.contains('__') && !is_local_var
						&& ((const_key in g.emitted_types || global_key in g.emitted_types)
						|| g.is_module_local_const_or_global(node.name)) {
						g.sb.write_string('${g.cur_module}__${node.name}')
					} else if g.cur_module != '' && g.cur_module != 'main'
						&& g.cur_module != 'builtin' && !node.name.contains('__') && !is_local_var
						&& !g.is_module_ident(node.name) && g.is_module_local_fn(node.name)
						&& !g.is_type_name(node.name) {
						g.sb.write_string('${g.cur_module}__${sanitize_fn_ident(node.name)}')
					} else {
						mut ident_name := node.name
						if g.cur_module != '' {
							double_prefix := '${g.cur_module}__${g.cur_module}__'
							if ident_name.starts_with(double_prefix) {
								ident_name = ident_name[g.cur_module.len + 2..]
							}
						}
						g.sb.write_string(ident_name)
					}
				}
			}
		}
		ast.ParenExpr {
			g.sb.write_string('(')
			g.gen_expr(node.expr)
			g.sb.write_string(')')
		}
		ast.InfixExpr {
			lhs_type := g.get_expr_type(node.lhs)
			rhs_type := g.get_expr_type(node.rhs)
			// Option none comparison: `opt == none` / `opt != none`.
			if node.op in [.eq, .ne] {
				if lhs_type.starts_with('_option_') && is_none_like_expr(node.rhs) {
					sep := if g.expr_is_pointer(node.lhs) { '->' } else { '.' }
					cmp := if node.op == .eq { '!=' } else { '==' }
					g.sb.write_string('(')
					g.gen_expr(node.lhs)
					g.sb.write_string('${sep}state ${cmp} 0)')
					return
				}
				if rhs_type.starts_with('_option_') && is_none_like_expr(node.lhs) {
					sep := if g.expr_is_pointer(node.rhs) { '->' } else { '.' }
					cmp := if node.op == .eq { '!=' } else { '==' }
					g.sb.write_string('(')
					g.gen_expr(node.rhs)
					g.sb.write_string('${sep}state ${cmp} 0)')
					return
				}
			}
			if node.op in [.eq, .ne, .key_is, .not_is]
				&& ((node.rhs is ast.Ident && node.rhs.name == 'Eof')
				|| (node.rhs is ast.SelectorExpr && node.rhs.rhs.name == 'Eof')) {
				if node.op in [.ne, .not_is] {
					g.sb.write_string('!')
				}
				g.sb.write_string('string__eq(err.type_name(err._object), (string){"Eof", 3})')
				return
			}
			if node.op in [.eq, .ne, .key_is, .not_is] && node.lhs is ast.Ident
				&& node.lhs.name == 'err' && node.rhs is ast.Ident {
				rhs_ident := node.rhs as ast.Ident
				if rhs_ident.name.len > 0 && rhs_ident.name[0] >= `A` && rhs_ident.name[0] <= `Z` {
					if node.op in [.ne, .not_is] {
						g.sb.write_string('!')
					}
					type_name := rhs_ident.name
					g.sb.write_string('string__eq(err.type_name(err._object), (string){"${type_name}", ${type_name.len}})')
					return
				}
			}
			if node.op in [.eq, .ne, .key_is, .not_is] && lhs_type == 'IError'
				&& node.rhs is ast.Ident {
				rhs_ident := node.rhs as ast.Ident
				if g.is_type_name(rhs_ident.name) || (rhs_ident.name.len > 0
					&& rhs_ident.name[0] >= `A` && rhs_ident.name[0] <= `Z`) {
					if node.op in [.ne, .not_is] {
						g.sb.write_string('!')
					}
					type_name := rhs_ident.name
					g.sb.write_string('string__eq(')
					g.gen_expr(node.lhs)
					g.sb.write_string('.type_name(')
					g.gen_expr(node.lhs)
					g.sb.write_string('._object), (string){"${type_name}", ${type_name.len}})')
					return
				}
			}
			if node.op in [.key_is, .not_is, .eq, .ne] {
				if raw_type := g.get_raw_type(node.lhs) {
					is_iface := raw_type is types.Interface
						|| (raw_type is types.Pointer && raw_type.base_type is types.Interface)
					if is_iface {
						mut rhs_name := ''
						if node.rhs is ast.Ident {
							rhs_name = node.rhs.name
						} else if node.rhs is ast.SelectorExpr {
							rhs_name = node.rhs.rhs.name
						}
						type_id := interface_type_id_for_name(rhs_name)
						if type_id <= 0 {
							g.sb.write_string(if node.op in [.key_is, .eq] {
								'false'
							} else {
								'true'
							})
							return
						}
						sep := if g.expr_is_pointer(node.lhs) { '->' } else { '.' }
						g.sb.write_string('(')
						g.gen_expr(node.lhs)
						op := if node.op in [.key_is, .eq] { '==' } else { '!=' }
						g.sb.write_string('${sep}_type_id ${op} ${type_id})')
						return
					}
				}
			}
			// Sum type checks: expr is Type and lowered expr ==/!= Type.
			rhs_can_match_sum := node.rhs is ast.Ident
				|| (node.rhs is ast.SelectorExpr && node.rhs.lhs is ast.Ident)
			if node.op in [.key_is, .not_is] || (node.op in [.eq, .ne] && rhs_can_match_sum) {
				mut rhs_name := ''
				if node.rhs is ast.Ident {
					rhs_name = node.rhs.name
				} else if node.rhs is ast.SelectorExpr && node.rhs.lhs is ast.Ident {
					rhs_name = '${(node.rhs.lhs as ast.Ident).name}__${node.rhs.rhs.name}'
				}
				if rhs_name != '' {
					// Find sum type by checking LHS type
					lhs_sum_type := g.get_expr_type(node.lhs)
					mut variants := []string{}
					if vs := g.sum_type_variants[lhs_sum_type] {
						variants = vs.clone()
					} else if lhs_sum_type.contains('__') {
						short_sum := lhs_sum_type.all_after_last('__')
						if vs := g.sum_type_variants[short_sum] {
							variants = vs.clone()
						}
					} else {
						qualified_sum := g.get_qualified_name(lhs_sum_type)
						if vs := g.sum_type_variants[qualified_sum] {
							variants = vs.clone()
						}
					}
					if variants.len > 0 {
						mut tag := -1
						for i, v in variants {
							v_short := if v.contains('__') { v.all_after_last('__') } else { v }
							if v == rhs_name || v_short == rhs_name
								|| rhs_name.ends_with('__${v_short}') {
								tag = i
								break
							}
						}
						if tag >= 0 {
							sep := if g.expr_is_pointer(node.lhs) { '->' } else { '.' }
							op := if node.op in [.key_is, .eq] { '==' } else { '!=' }
							g.sb.write_string('(')
							g.gen_expr(node.lhs)
							g.sb.write_string('${sep}_tag ${op} ${tag})')
							return
						}
					}
				}
			}
			if node.op == .left_shift {
				is_array_append, elem_type := g.array_append_elem_type(node.lhs, node.rhs)
				if is_array_append {
					if g.expr_is_array_value(node.rhs) {
						rhs_tmp := '_arr_append_tmp_${g.tmp_counter}'
						g.tmp_counter++
						arr_rhs_type := g.expr_array_runtime_type(node.rhs)
						g.sb.write_string('({ ${arr_rhs_type} ${rhs_tmp} = ')
						g.gen_expr(node.rhs)
						g.sb.write_string('; array__push_many((array*)')
						if g.expr_is_pointer(node.lhs) {
							g.gen_expr(node.lhs)
						} else {
							g.sb.write_string('&')
							g.gen_expr(node.lhs)
						}
						g.sb.write_string(', ${rhs_tmp}.data, ${rhs_tmp}.len); })')
						return
					}
					g.sb.write_string('array__push((array*)')
					if g.expr_is_pointer(node.lhs) {
						g.gen_expr(node.lhs)
					} else {
						g.sb.write_string('&')
						g.gen_expr(node.lhs)
					}
					g.sb.write_string(', ')
					g.gen_addr_of_expr(node.rhs, elem_type)
					g.sb.write_string(')')
					return
				}
			}
			if node.op == .plus && lhs_type == 'string' && rhs_type == 'string' {
				g.sb.write_string('string__plus(')
				g.gen_expr(node.lhs)
				g.sb.write_string(', ')
				g.gen_expr(node.rhs)
				g.sb.write_string(')')
				return
			}
			if node.op in [.key_in, .not_in] {
				if node.rhs is ast.ArrayInitExpr {
					join_op := if node.op == .key_in { ' || ' } else { ' && ' }
					g.sb.write_string('(')
					if node.rhs.exprs.len == 0 {
						g.sb.write_string(if node.op == .key_in { 'false' } else { 'true' })
					} else {
						for i, elem in node.rhs.exprs {
							if i > 0 {
								g.sb.write_string(join_op)
							}
							if lhs_type == 'string' {
								if node.op == .not_in {
									g.sb.write_string('!')
								}
								g.sb.write_string('string__eq(')
								g.gen_expr(node.lhs)
								g.sb.write_string(', ')
								g.gen_expr(elem)
								g.sb.write_string(')')
							} else {
								cmp_op := if node.op == .key_in { '==' } else { '!=' }
								g.sb.write_string('(')
								g.gen_expr(node.lhs)
								g.sb.write_string(' ${cmp_op} ')
								g.gen_expr(elem)
								g.sb.write_string(')')
							}
						}
					}
					g.sb.write_string(')')
					return
				}
				if rhs_type == 'map' || rhs_type.starts_with('Map_') {
					key_type := if lhs_type == '' { 'int' } else { lhs_type }
					if node.op == .not_in {
						g.sb.write_string('!')
					}
					if node.rhs is ast.Ident || node.rhs is ast.SelectorExpr {
						g.sb.write_string('map__exists(&')
						g.gen_expr(node.rhs)
						g.sb.write_string(', ')
						g.gen_addr_of_expr(node.lhs, key_type)
						g.sb.write_string(')')
					} else {
						tmp_name := '_map_in_tmp_${g.tmp_counter}'
						g.tmp_counter++
						g.sb.write_string('({ map ${tmp_name} = ')
						g.gen_expr(node.rhs)
						g.sb.write_string('; map__exists(&${tmp_name}, ')
						g.gen_addr_of_expr(node.lhs, key_type)
						g.sb.write_string('); })')
					}
					return
				}
				if rhs_type == 'array' || rhs_type.starts_with('Array_') {
					key_type := if lhs_type == '' { 'int' } else { lhs_type }
					if node.op == .not_in {
						g.sb.write_string('!')
					}
					g.sb.write_string('array__contains(')
					g.gen_expr(node.rhs)
					g.sb.write_string(', ')
					g.gen_addr_of_expr(node.lhs, key_type)
					g.sb.write_string(')')
					return
				}
				cmp_op := if node.op == .key_in { '==' } else { '!=' }
				g.sb.write_string('(')
				g.gen_expr(node.lhs)
				g.sb.write_string(' ${cmp_op} ')
				g.gen_expr(node.rhs)
				g.sb.write_string(')')
				return
			}
			is_lhs_array := lhs_type == 'array' || lhs_type.starts_with('Array_')
			is_rhs_array := rhs_type == 'array' || rhs_type.starts_with('Array_')
			if node.op in [.eq, .ne] && is_lhs_array && is_rhs_array {
				if node.op == .ne {
					g.sb.write_string('!')
				}
				g.sb.write_string('(((')
				g.gen_expr(node.lhs)
				g.sb.write_string('.len == ')
				g.gen_expr(node.rhs)
				g.sb.write_string('.len) && (')
				g.gen_expr(node.lhs)
				g.sb.write_string('.element_size == ')
				g.gen_expr(node.rhs)
				g.sb.write_string('.element_size) && (memcmp(')
				g.gen_expr(node.lhs)
				g.sb.write_string('.data, ')
				g.gen_expr(node.rhs)
				g.sb.write_string('.data, (')
				g.gen_expr(node.lhs)
				g.sb.write_string('.len * ')
				g.gen_expr(node.lhs)
				g.sb.write_string('.element_size)) == 0)))')
				return
			}
			// String comparison: use string__eq instead of C == operator.
			// Only apply when we're confident both sides are strings:
			// - at least one side is a string literal, or
			// - both sides are typed as 'string' and neither is an enum type.
			is_string_cmp := if node.lhs is ast.StringLiteral || node.rhs is ast.StringLiteral {
				true
			} else {
				lhs_type == 'string' && rhs_type == 'string' && !g.is_enum_type(lhs_type)
					&& !g.is_enum_type(rhs_type)
			}
			if node.op in [.eq, .ne] && is_string_cmp {
				if node.op == .ne {
					g.sb.write_string('!')
				}
				g.sb.write_string('string__eq(')
				g.gen_expr(node.lhs)
				g.sb.write_string(', ')
				g.gen_expr(node.rhs)
				g.sb.write_string(')')
				return
			}
			mut cmp_type := ''
			if g.should_use_memcmp_eq(lhs_type, rhs_type) {
				cmp_type = lhs_type
			} else if node.op in [.eq, .ne] {
				lhs_cast_type := extract_compare_cast_type(node.lhs)
				rhs_cast_type := extract_compare_cast_type(node.rhs)
				if lhs_cast_type != '' && rhs_cast_type == '' {
					cmp_type = lhs_cast_type
				} else if rhs_cast_type != '' && lhs_cast_type == '' {
					cmp_type = rhs_cast_type
				} else if lhs_cast_type != '' && lhs_cast_type == rhs_cast_type {
					cmp_type = lhs_cast_type
				}
				if cmp_type in primitive_types || cmp_type == 'string' || cmp_type.ends_with('*')
					|| cmp_type.ends_with('ptr') {
					cmp_type = ''
				}
			}
			if node.op in [.eq, .ne] && cmp_type != '' {
				ltmp := '_cmp_l_${g.tmp_counter}'
				g.tmp_counter++
				rtmp := '_cmp_r_${g.tmp_counter}'
				g.tmp_counter++
				g.sb.write_string('({ ${cmp_type} ${ltmp} = ')
				if !g.gen_unwrapped_value_expr(node.lhs) {
					g.gen_expr(node.lhs)
				}
				g.sb.write_string('; ${cmp_type} ${rtmp} = ')
				if !g.gen_unwrapped_value_expr(node.rhs) {
					g.gen_expr(node.rhs)
				}
				cmp_op := if node.op == .eq { '== 0' } else { '!= 0' }
				g.sb.write_string('; memcmp(&${ltmp}, &${rtmp}, sizeof(${cmp_type})) ${cmp_op}; })')
				return
			}
			g.sb.write_string('(')
			if !g.gen_unwrapped_value_expr(node.lhs) {
				g.gen_expr(node.lhs)
			}
			op := match node.op {
				.plus { '+' }
				.minus { '-' }
				.mul { '*' }
				.div { '/' }
				.mod { '%' }
				.gt { '>' }
				.lt { '<' }
				.eq { '==' }
				.ne { '!=' }
				.ge { '>=' }
				.le { '<=' }
				.and { '&&' }
				.logical_or { '||' }
				.amp { '&' }
				.pipe { '|' }
				.xor { '^' }
				.left_shift { '<<' }
				.right_shift { '>>' }
				.key_is { '==' }
				.not_is { '!=' }
				else { '?' }
			}
			g.sb.write_string(' ${op} ')
			if !g.gen_unwrapped_value_expr(node.rhs) {
				g.gen_expr(node.rhs)
			}
			g.sb.write_string(')')
		}
		ast.PrefixExpr {
			// &T(x) in unsafe contexts is used as a pointer cast in V stdlib code.
			// Emit it as (T*)(x) so `*unsafe { &T(p) }` becomes `*((T*)p)`.
			if node.op == .amp {
				// &&T(x) is a pointer-to-pointer cast pattern used in builtin code.
				// Lower it directly to (T**)(x) instead of taking address of a cast rvalue.
				if node.expr is ast.PrefixExpr && node.expr.op == .amp {
					inner := node.expr as ast.PrefixExpr
					if inner.expr is ast.CallOrCastExpr {
						target_type := g.expr_type_to_c(inner.expr.lhs)
						g.sb.write_string('((${target_type}**)(')
						g.gen_expr(inner.expr.expr)
						g.sb.write_string('))')
						return
					}
					if inner.expr is ast.CastExpr {
						target_type := g.expr_type_to_c(inner.expr.typ)
						g.sb.write_string('((${target_type}**)(')
						g.gen_expr(inner.expr.expr)
						g.sb.write_string('))')
						return
					}
				}
				if node.expr is ast.IndexExpr {
					idx := node.expr as ast.IndexExpr
					if idx.lhs is ast.Ident {
						if idx.lhs.name in g.fixed_array_globals || idx.lhs.name == 'rune_maps' {
							g.sb.write_string('&')
							g.gen_expr(idx.lhs)
							g.sb.write_string('[')
							g.gen_expr(idx.expr)
							g.sb.write_string(']')
							return
						}
						if raw_type := g.get_raw_type(idx.lhs) {
							if raw_type is types.ArrayFixed {
								// Fixed arrays: &arr[i]
								g.sb.write_string('&')
								g.gen_expr(idx.lhs)
								g.sb.write_string('[')
								g.gen_expr(idx.expr)
								g.sb.write_string(']')
								return
							}
						}
						lhs_type := g.get_expr_type(idx.lhs)
						if lhs_type == 'array' || lhs_type.starts_with('Array_') {
							mut elem_type := g.get_expr_type(idx)
							if elem_type == '' || elem_type == 'int' {
								if lhs_type.starts_with('Array_fixed_') {
									if finfo := g.collected_fixed_array_types[lhs_type] {
										elem_type = finfo.elem_type
									}
								} else if lhs_type.starts_with('Array_') {
									elem_type = lhs_type['Array_'.len..].trim_right('*')
								}
							}
							if elem_type == '' {
								elem_type = 'u8'
							}
							g.sb.write_string('&((')
							g.sb.write_string(elem_type)
							g.sb.write_string('*)')
							g.gen_expr(idx.lhs)
							if lhs_type.ends_with('*') {
								g.sb.write_string('->data)[')
							} else {
								g.sb.write_string('.data)[')
							}
							g.gen_expr(idx.expr)
							g.sb.write_string(']')
							return
						}
					}
				}
				if node.expr is ast.CallExpr {
					if node.expr.args.len == 1 && node.expr.lhs is ast.Ident
						&& g.is_type_name(node.expr.lhs.name) {
						target_type := g.expr_type_to_c(node.expr.lhs)
						g.sb.write_string('((${target_type}*)(')
						g.gen_expr(node.expr.args[0])
						g.sb.write_string('))')
						return
					}
					if node.expr.args.len == 1 && node.expr.lhs is ast.SelectorExpr {
						sel := node.expr.lhs as ast.SelectorExpr
						if sel.lhs is ast.Ident && sel.lhs.name == 'C'
							&& g.is_c_type_name(sel.rhs.name) {
							target_type := g.expr_type_to_c(node.expr.lhs)
							g.sb.write_string('((${target_type}*)(')
							g.gen_expr(node.expr.args[0])
							g.sb.write_string('))')
							return
						}
					}
				}
				if node.expr is ast.CastExpr {
					target_type := g.expr_type_to_c(node.expr.typ)
					g.sb.write_string('((${target_type}*)(')
					g.gen_expr(node.expr.expr)
					g.sb.write_string('))')
					return
				}
				if node.expr is ast.ModifierExpr {
					if node.expr.expr is ast.CastExpr {
						target_type := g.expr_type_to_c(node.expr.expr.typ)
						g.sb.write_string('((${target_type}*)(')
						g.gen_expr(node.expr.expr.expr)
						g.sb.write_string('))')
						return
					}
					if node.expr.expr is ast.CallOrCastExpr {
						if node.expr.expr.lhs is ast.Ident
							&& g.is_type_name(node.expr.expr.lhs.name) {
							target_type := g.expr_type_to_c(node.expr.expr.lhs)
							g.sb.write_string('((${target_type}*)(')
							g.gen_expr(node.expr.expr.expr)
							g.sb.write_string('))')
							return
						}
					}
				}
				if node.expr is ast.CallOrCastExpr {
					if node.expr.lhs is ast.Ident && g.is_type_name(node.expr.lhs.name) {
						target_type := g.expr_type_to_c(node.expr.lhs)
						g.sb.write_string('((${target_type}*)(')
						g.gen_expr(node.expr.expr)
						g.sb.write_string('))')
						return
					} else if node.expr.lhs is ast.Type {
						target_type := g.expr_type_to_c(node.expr.lhs)
						g.sb.write_string('((${target_type}*)(')
						g.gen_expr(node.expr.expr)
						g.sb.write_string('))')
						return
					} else if node.expr.lhs is ast.SelectorExpr {
						sel := node.expr.lhs as ast.SelectorExpr
						if sel.lhs is ast.Ident && sel.lhs.name == 'C'
							&& g.is_c_type_name(sel.rhs.name) {
							target_type := g.expr_type_to_c(node.expr.lhs)
							g.sb.write_string('((${target_type}*)(')
							g.gen_expr(node.expr.expr)
							g.sb.write_string('))')
							return
						}
					}
				}
				if node.expr is ast.ParenExpr {
					if node.expr.expr is ast.CastExpr {
						target_type := g.expr_type_to_c(node.expr.expr.typ)
						g.sb.write_string('((${target_type}*)(')
						g.gen_expr(node.expr.expr.expr)
						g.sb.write_string('))')
						return
					}
					if node.expr.expr is ast.CallExpr {
						if node.expr.expr.args.len == 1 {
							target_type := g.expr_type_to_c(node.expr.expr.lhs)
							g.sb.write_string('((${target_type}*)(')
							g.gen_expr(node.expr.expr.args[0])
							g.sb.write_string('))')
							return
						}
					}
					if node.expr.expr is ast.CallOrCastExpr {
						if node.expr.expr.lhs is ast.Ident {
							target_type := g.expr_type_to_c(node.expr.expr.lhs)
							g.sb.write_string('((${target_type}*)(')
							g.gen_expr(node.expr.expr.expr)
							g.sb.write_string('))')
							return
						} else if node.expr.expr.lhs is ast.Type {
							target_type := g.expr_type_to_c(node.expr.expr.lhs)
							g.sb.write_string('((${target_type}*)(')
							g.gen_expr(node.expr.expr.expr)
							g.sb.write_string('))')
							return
						}
					}
				}
			}
			// Handle &fn_call() where fn_call returns a struct (rvalue)
			// Can't take address of rvalue, use compound statement expression
			if node.op == .amp && node.expr is ast.CallExpr {
				if !(node.expr.args.len == 1 && node.expr.lhs is ast.Ident
					&& g.is_type_name(node.expr.lhs.name)) {
					// This is a function call, not a type cast
					ret_type := g.get_expr_type(node.expr)
					if ret_type != '' && ret_type != 'void' && ret_type != 'int' {
						tmp_name := '_sumtmp${g.tmp_counter}'
						g.tmp_counter++
						g.sb.write_string('({ ${ret_type} ${tmp_name} = ')
						g.gen_expr(node.expr)
						g.sb.write_string('; &${tmp_name}; })')
						return
					}
				}
			}
			// Handle &AssocExpr - can't take address of compound expression rvalue
			// Generate: ({ Type _assoc = base; _assoc.f = v; &_assoc; })
			if node.op == .amp && node.expr is ast.AssocExpr {
				assoc := node.expr as ast.AssocExpr
				type_name := g.expr_type_to_c(assoc.typ)
				expr_type := g.get_expr_type(assoc.expr)
				g.sb.write_string('({ ${type_name} _assoc = ')
				if expr_type != type_name && expr_type in g.sum_type_variants {
					short_type := if type_name.contains('__') {
						type_name.all_after_last('__')
					} else {
						type_name
					}
					g.sb.write_string('(*((${type_name}*)(')
					g.gen_expr(assoc.expr)
					g.sb.write_string('._data._${short_type})))')
				} else if expr_type.ends_with('*') {
					g.sb.write_string('*')
					g.gen_expr(assoc.expr)
				} else {
					g.gen_expr(assoc.expr)
				}
				g.sb.write_string('; ')
				for field in assoc.fields {
					g.sb.write_string('_assoc.${field.name} = ')
					g.gen_expr(field.value)
					g.sb.write_string('; ')
				}
				g.sb.write_string('&_assoc; })')
				return
			}
			// V `&Type{...}` must allocate on the heap.
			// Taking the address of a C compound literal here would create a dangling pointer.
			if node.op == .amp && node.expr is ast.InitExpr {
				type_name := g.expr_type_to_c(node.expr.typ)
				tmp_name := '_heap_t${g.tmp_counter}'
				g.tmp_counter++
				g.sb.write_string('({ ${type_name}* ${tmp_name} = (${type_name}*)malloc(sizeof(${type_name})); *${tmp_name} = ')
				g.gen_expr(node.expr)
				g.sb.write_string('; ${tmp_name}; })')
				return
			}
			if node.op == .mul {
				if raw_type := g.get_raw_type(node.expr) {
					is_iface := raw_type is types.Interface
						|| (raw_type is types.Pointer && raw_type.base_type is types.Interface)
					if is_iface {
						target_type := g.get_expr_type(node)
						if target_type != '' && target_type != 'int' {
							sep := if g.expr_is_pointer(node.expr) { '->' } else { '.' }
							g.sb.write_string('(*((')
							g.sb.write_string(target_type)
							g.sb.write_string('*)(')
							g.gen_expr(node.expr)
							g.sb.write_string('${sep}_object)))')
							return
						}
					}
				}
			}
			op := match node.op {
				.minus { '-' }
				.not { '!' }
				.amp { '&' }
				.mul { '*' }
				.bit_not { '~' }
				else { '' }
			}
			g.sb.write_string(op)
			g.gen_expr(node.expr)
		}
		ast.CallExpr {
			g.gen_call_expr(node.lhs, node.args)
		}
		ast.CallOrCastExpr {
			// Check if this is a type cast: int(x), MyInt(42), etc.
			if node.lhs is ast.Ident && g.is_type_name(node.lhs.name) {
				type_name := g.expr_type_to_c(node.lhs)
				g.gen_type_cast_expr(type_name, node.expr)
			} else if node.lhs is ast.Type {
				type_name := g.expr_type_to_c(node.lhs)
				g.gen_type_cast_expr(type_name, node.expr)
			} else if node.lhs is ast.SelectorExpr {
				sel := node.lhs as ast.SelectorExpr
				if sel.lhs is ast.Ident && sel.lhs.name == 'C' && g.is_c_type_name(sel.rhs.name) {
					// C.TYPE(expr) is a type cast to a C type
					type_name := g.expr_type_to_c(node.lhs)
					g.gen_type_cast_expr(type_name, node.expr)
				} else if sel.lhs is ast.Ident {
					// module.Type(expr) - check if this is a type cast
					type_name := g.expr_type_to_c(node.lhs)
					if g.is_type_name(type_name) {
						g.gen_type_cast_expr(type_name, node.expr)
					} else {
						mut call_args := []ast.Expr{cap: 1}
						call_args << node.expr
						g.gen_call_expr(node.lhs, call_args)
					}
				} else {
					mut call_args := []ast.Expr{cap: 1}
					call_args << node.expr
					g.gen_call_expr(node.lhs, call_args)
				}
			} else {
				// Single-arg call: println(x) is parsed as CallOrCastExpr
				mut call_args := []ast.Expr{cap: 1}
				call_args << node.expr
				g.gen_call_expr(node.lhs, call_args)
			}
		}
		ast.SelectorExpr {
			// typeof(x).name -> just emit the typeof string directly (already a string)
			if node.lhs is ast.KeywordOperator && node.lhs.op == .key_typeof
				&& node.rhs.name == 'name' {
				g.gen_keyword_operator(node.lhs)
				return
			}
			// C.<ident> references C macros/constants directly (e.g. C.EOF -> EOF).
			if node.lhs is ast.Ident && node.lhs.name == 'C' {
				g.sb.write_string(node.rhs.name)
				return
			}
			if node.lhs is ast.Ident
				&& node.lhs.name in ['bool', 'string', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64', 'byte', 'rune'] {
				if enum_name := g.enum_value_to_enum[node.rhs.name] {
					g.sb.write_string('${g.normalize_enum_name(enum_name)}__${node.rhs.name}')
					return
				}
			}
			if node.lhs is ast.Ident {
				mut is_known_var := g.get_local_var_c_type(node.lhs.name) != none
				if !is_known_var && !g.is_module_ident(node.lhs.name) {
					if enum_name := g.get_expr_type_from_env(node) {
						if enum_name != '' && g.is_enum_type(enum_name) {
							g.sb.write_string('${g.normalize_enum_name(enum_name)}__${node.rhs.name}')
							return
						}
					}
					if node.lhs.name in ['bool', 'string', 'int', 'i8', 'i16', 'i32', 'i64', 'u8',
						'u16', 'u32', 'u64', 'f32', 'f64', 'byte', 'rune'] {
						if enum_name := g.enum_value_to_enum[node.rhs.name] {
							g.sb.write_string('${g.normalize_enum_name(enum_name)}__${node.rhs.name}')
							return
						}
					}
				}
			}
			// If checker already resolved this selector as an enum value, use Enum__field.
			if raw_type := g.get_raw_type(node) {
				if raw_type is types.Enum {
					// Verify the field actually belongs to this enum.
					// The checker may annotate branch values with the match expression's
					// enum type instead of the return type's enum.
					mut field_valid := false
					for f in raw_type.fields {
						if f.name == node.rhs.name {
							field_valid = true
							break
						}
					}
					if field_valid {
						mut emit_enum_value := false
						if node.lhs is ast.EmptyExpr {
							emit_enum_value = true
						} else if node.lhs is ast.Ident {
							if g.is_enum_type(node.lhs.name) {
								emit_enum_value = true
							} else if !g.is_module_ident(node.lhs.name) {
								emit_enum_value = g.get_local_var_c_type(node.lhs.name) == none
							}
						}
						if emit_enum_value {
							enum_name := g.types_type_to_c(raw_type)
							g.sb.write_string('${g.normalize_enum_name(enum_name)}__${node.rhs.name}')
							return
						}
					}
				}
			}
			if g.gen_sum_narrowed_selector(node) {
				return
			}
			if g.gen_sum_variant_field_selector(node) {
				return
			}
			lhs_type := g.get_expr_type(node.lhs)
			if node.rhs.name == 'data' {
				if lhs_type.starts_with('_result_') && g.result_value_type(lhs_type) != '' {
					g.gen_unwrapped_value_expr(node.lhs)
					return
				}
				if lhs_type.starts_with('_option_') && option_value_type(lhs_type) != '' {
					g.gen_unwrapped_value_expr(node.lhs)
					return
				}
			}
			// Fixed-size array `.len` becomes compile-time length.
			if node.rhs.name == 'len' {
				if node.lhs is ast.Ident {
					mut fixed_name := node.lhs.name
					module_const_key := 'const_${g.cur_module}__${node.lhs.name}'
					if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin'
						&& module_const_key in g.emitted_types {
						fixed_name = '${g.cur_module}__${node.lhs.name}'
					}
					if fixed_name in g.fixed_array_globals {
						g.sb.write_string('((int)(sizeof(${fixed_name}) / sizeof(${fixed_name}[0])))')
						return
					}
					if fixed_name in ['strconv__pow5_inv_split_32', 'strconv__pow5_split_32',
						'strconv__pow5_inv_split_64_x', 'strconv__pow5_split_64_x'] {
						g.sb.write_string('((int)(sizeof(${fixed_name}) / sizeof(${fixed_name}[0])))')
						return
					}
				}
				if node.lhs is ast.SelectorExpr {
					if node.lhs.lhs is ast.Ident && g.is_module_ident(node.lhs.lhs.name) {
						fixed_name := '${node.lhs.lhs.name}__${node.lhs.rhs.name}'
						if fixed_name in g.fixed_array_globals {
							g.sb.write_string('((int)(sizeof(${fixed_name}) / sizeof(${fixed_name}[0])))')
							return
						}
					}
				}
				if node.lhs is ast.SelectorExpr && g.is_fixed_array_selector(node.lhs) {
					g.sb.write_string('((int)(sizeof(')
					g.gen_expr(node.lhs)
					g.sb.write_string(') / sizeof((')
					g.gen_expr(node.lhs)
					g.sb.write_string(')[0])))')
					return
				}
				if raw_type := g.get_raw_type(node.lhs) {
					if raw_type is types.ArrayFixed {
						g.sb.write_string('((int)(sizeof(')
						g.gen_expr(node.lhs)
						g.sb.write_string(') / sizeof((')
						g.gen_expr(node.lhs)
						g.sb.write_string(')[0])))')
						return
					}
				}
			}
			// Enum shorthand: `.field` -> `EnumName__field` (using type checker info).
			if node.lhs is ast.EmptyExpr {
				// The enum_value_to_enum map definitively tells us which enum owns a field.
				// Use it to validate/override type checker results that may be wrong
				// (e.g., match branch values annotated with the match expression's enum type
				// instead of the return type's enum).
				known_enum := g.enum_value_to_enum[node.rhs.name] or { '' }
				if raw_type := g.get_raw_type(node) {
					if raw_type is types.Enum {
						enum_name := g.types_type_to_c(raw_type)
						if g.enum_has_field(enum_name, node.rhs.name) {
							g.sb.write_string('${g.normalize_enum_name(enum_name)}__${node.rhs.name}')
							return
						}
					}
				}
				if enum_name := g.get_expr_type_from_env(node) {
					if enum_name != '' && enum_name != 'int' && g.is_enum_type(enum_name) {
						if g.enum_has_field(enum_name, node.rhs.name) {
							g.sb.write_string('${g.normalize_enum_name(enum_name)}__${node.rhs.name}')
							return
						}
					}
				}
				// Use the definitive enum field mapping
				if known_enum != '' {
					g.sb.write_string('${g.normalize_enum_name(known_enum)}__${node.rhs.name}')
					return
				}
				// Last resort: use function return type as context
				if g.cur_fn_ret_type != '' && g.is_enum_type(g.cur_fn_ret_type) {
					g.sb.write_string('${g.normalize_enum_name(g.cur_fn_ret_type)}__${node.rhs.name}')
					return
				}
			}
			// module.const / module.var => module__const / module__var
			if node.lhs is ast.Ident && g.is_module_ident(node.lhs.name) {
				if node.rhs.name.starts_with('${node.lhs.name}__') {
					g.sb.write_string(node.rhs.name)
				} else {
					g.sb.write_string('${node.lhs.name}__${node.rhs.name}')
				}
				return
			}
			// Check if LHS is an enum type name -> emit EnumName__field
			if node.lhs is ast.Ident && g.is_enum_type(node.lhs.name) {
				enum_name := g.get_qualified_name(node.lhs.name)
				g.sb.write_string('${enum_name}__${node.rhs.name}')
			} else {
				mut use_ptr := g.expr_is_pointer(node.lhs)
				if node.lhs is ast.Ident {
					if node.lhs.name in g.cur_fn_mut_params {
						use_ptr = true
					} else if local_type := g.get_local_var_c_type(node.lhs.name) {
						// Local declaration type is authoritative for value vs pointer access.
						use_ptr = local_type.ends_with('*')
					}
				}
				lhs_struct := g.selector_struct_name(node.lhs)
				owner := g.embedded_owner_for(lhs_struct, node.rhs.name)
				field_name := escape_c_keyword(node.rhs.name)
				selector := if use_ptr { '->' } else { '.' }
				g.gen_expr(node.lhs)
				if owner != '' {
					g.sb.write_string('${selector}${escape_c_keyword(owner)}.${field_name}')
				} else {
					g.sb.write_string('${selector}${field_name}')
				}
			}
		}
		ast.IfExpr {
			g.gen_if_expr_value(node)
		}
		ast.PostfixExpr {
			g.gen_expr(node.expr)
			op := match node.op {
				.inc { '++' }
				.dec { '--' }
				else { '' }
			}
			g.sb.write_string(op)
		}
		ast.ModifierExpr {
			g.gen_expr(node.expr)
		}
		ast.CastExpr {
			g.gen_cast_expr(node)
		}
		ast.IndexExpr {
			g.gen_index_expr(node)
		}
		ast.ArrayInitExpr {
			g.gen_array_init_expr(node)
		}
		ast.InitExpr {
			g.gen_init_expr(node)
		}
		ast.MapInitExpr {
			g.gen_map_init_expr(node)
		}
		ast.MatchExpr {
			panic('bug in v2 compiler: MatchExpr should have been lowered in v2.transformer')
		}
		ast.UnsafeExpr {
			g.gen_unsafe_expr(node)
		}
		ast.OrExpr {
			panic('bug in v2 compiler: OrExpr should have been expanded in v2.transformer')
		}
		ast.AsCastExpr {
			g.gen_as_cast_expr(node)
		}
		ast.StringInterLiteral {
			g.gen_string_inter_literal(node)
		}
		ast.FnLiteral {
			g.sb.write_string('/* [TODO] FnLiteral */ NULL')
		}
		ast.LambdaExpr {
			g.sb.write_string('/* [TODO] LambdaExpr */ NULL')
		}
		ast.ComptimeExpr {
			// $if comptime should be resolved by transformer; @FN etc. handled here
			if node.expr is ast.IfExpr {
				panic('bug in v2 compiler: comptime \$if should have been resolved in v2.transformer')
			}
			g.gen_comptime_expr(node)
		}
		ast.Keyword {
			g.gen_keyword(node)
		}
		ast.KeywordOperator {
			g.gen_keyword_operator(node)
		}
		ast.RangeExpr {
			g.gen_range_expr(node)
		}
		ast.SelectExpr {
			g.sb.write_string('/* [TODO] SelectExpr */ 0')
		}
		ast.LockExpr {
			panic('bug in v2 compiler: LockExpr should have been lowered in v2.transformer')
		}
		ast.Type {
			g.sb.write_string('/* [TODO] Type */ 0')
		}
		ast.AssocExpr {
			// {expr | field1: val1, field2: val2} -> ({ Type _assoc = expr; _assoc.field1 = val1; _assoc; })
			type_name := g.expr_type_to_c(node.typ)
			expr_type := g.get_expr_type(node.expr)
			g.sb.write_string('({ ${type_name} _assoc = ')
			// If the base expr is a sum type, unwrap the variant first
			if expr_type != type_name && expr_type in g.sum_type_variants {
				short_type := if type_name.contains('__') {
					type_name.all_after_last('__')
				} else {
					type_name
				}
				g.sb.write_string('(*((${type_name}*)(')
				g.gen_expr(node.expr)
				g.sb.write_string('._data._${short_type})))')
			} else if expr_type.ends_with('*') {
				// Pointer receiver: dereference to get value
				g.sb.write_string('*')
				g.gen_expr(node.expr)
			} else {
				g.gen_expr(node.expr)
			}
			g.sb.write_string('; ')
			for field in node.fields {
				g.sb.write_string('_assoc.${field.name} = ')
				g.gen_expr(field.value)
				g.sb.write_string('; ')
			}
			g.sb.write_string('_assoc; })')
		}
		ast.Tuple {
			tuple_type := g.get_expr_type(node)
			g.sb.write_string('((${tuple_type}){')
			for i, expr in node.exprs {
				if i > 0 {
					g.sb.write_string(', ')
				}
				g.sb.write_string('.arg${i} = ')
				g.gen_expr(expr)
			}
			g.sb.write_string('})')
		}
		ast.FieldInit {
			// FieldInit should be grouped into InitExpr by call argument lowering.
			// Fallback to value expression to keep C output valid.
			g.gen_expr(node.value)
		}
		ast.IfGuardExpr {
			panic('bug in v2 compiler: IfGuardExpr should have been expanded in v2.transformer')
		}
		ast.GenericArgs {
			panic('bug in v2 compiler: GenericArgs should have been resolved during type checking')
		}
		ast.GenericArgOrIndexExpr {
			panic('bug in v2 compiler: GenericArgOrIndexExpr should have been resolved during type checking')
		}
		ast.SqlExpr {
			g.sb.write_string('/* [TODO] SqlExpr */ 0')
		}
		ast.EmptyExpr {}
	}
}

fn (mut g Gen) gen_keyword(node ast.Keyword) {
	match node.tok {
		.key_nil {
			g.sb.write_string('NULL')
		}
		.key_none {
			g.sb.write_string('0')
		}
		.key_true {
			g.sb.write_string('true')
		}
		.key_false {
			g.sb.write_string('false')
		}
		.key_struct {
			g.sb.write_string('struct')
		}
		else {
			g.sb.write_string('0')
		}
	}
}

fn (mut g Gen) gen_map_init_expr(node ast.MapInitExpr) {
	// Non-empty map literals are lowered in transformer to
	// builtin__new_map_init_noscan_value(...).
	if node.keys.len > 0 {
		panic('bug in v2 compiler: non-empty MapInitExpr should have been lowered in v2.transformer')
	}
	key_c_type, val_c_type := g.map_expr_key_val_types(node.typ)
	g.gen_new_map_call(key_c_type, val_c_type)
}

fn (mut g Gen) gen_new_map_call(key_c_type string, val_c_type string) {
	mut hash_fn := ''
	mut eq_fn := ''
	mut clone_fn := ''
	mut free_fn := ''
	if key_c_type == 'string' {
		hash_fn = 'map_hash_string'
		eq_fn = 'map_eq_string'
		clone_fn = 'map_clone_string'
		free_fn = 'map_free_string'
	} else {
		key_size := g.c_type_sizeof(key_c_type)
		hash_fn = 'map_hash_int_${key_size}'
		eq_fn = 'map_eq_int_${key_size}'
		clone_fn = 'map_clone_int_${key_size}'
		free_fn = 'map_free_nop'
	}
	g.sb.write_string('new_map(sizeof(${key_c_type}), sizeof(${val_c_type}), &${hash_fn}, &${eq_fn}, &${clone_fn}, &${free_fn})')
}

fn (g &Gen) c_type_sizeof(c_type string) int {
	return match c_type {
		'i8', 'u8', 'byte', 'bool' { 1 }
		'i16', 'u16' { 2 }
		'int', 'i32', 'u32', 'f32', 'rune' { 4 }
		'i64', 'u64', 'f64' { 8 }
		else { 4 } // default to 4 for unknown types
	}
}

fn (g &Gen) should_use_memcmp_eq(lhs_type string, rhs_type string) bool {
	if lhs_type == '' || rhs_type == '' || lhs_type != rhs_type {
		return false
	}
	if lhs_type in primitive_types || lhs_type == 'string' {
		return false
	}
	if lhs_type.ends_with('*') || lhs_type.ends_with('ptr') {
		return false
	}
	if lhs_type.starts_with('Array_') || lhs_type.starts_with('Map_') {
		return false
	}
	return true
}

fn extract_compare_cast_type(expr ast.Expr) string {
	if expr is ast.PrefixExpr && expr.op == .mul {
		if expr.expr is ast.CastExpr {
			if expr.expr.typ is ast.PrefixExpr && expr.expr.typ.op == .amp
				&& expr.expr.typ.expr is ast.Ident {
				return expr.expr.typ.expr.name
			}
		}
	}
	return ''
}

fn (mut g Gen) map_expr_key_val_types(e ast.Expr) (string, string) {
	match e {
		ast.Type {
			if e is ast.MapType {
				kt := g.expr_type_to_c(e.key_type)
				vt := g.expr_type_to_c(e.value_type)
				return if kt != '' { kt } else { 'int' }, if vt != '' {
					vt
				} else {
					'int'
				}
			}
		}
		else {}
	}
	return 'int', 'int'
}

fn (mut g Gen) gen_range_expr(node ast.RangeExpr) {
	// Standalone ranges should be lowered or appear only in IndexExpr slicing.
	if node.start is ast.EmptyExpr && node.end is ast.EmptyExpr {
		g.sb.write_string('0')
		return
	}
	g.sb.write_string('0')
}

fn (mut g Gen) gen_stmts_from_expr(e ast.Expr) {
	if e is ast.IfExpr {
		g.gen_stmts(e.stmts)
	}
}

fn (g &Gen) if_expr_can_be_ternary(node ast.IfExpr) bool {
	if node.cond !is ast.EmptyExpr {
		if node.stmts.len != 1 || node.stmts[0] !is ast.ExprStmt {
			return false
		}
		if node.else_expr is ast.EmptyExpr {
			return false
		}
	}
	if node.cond is ast.EmptyExpr {
		return node.stmts.len == 1 && node.stmts[0] is ast.ExprStmt
	}
	if node.else_expr is ast.IfExpr {
		return g.if_expr_can_be_ternary(node.else_expr)
	}
	return true
}

fn (g &Gen) extract_if_expr(expr ast.Expr) ?ast.IfExpr {
	match expr {
		ast.IfExpr {
			return expr
		}
		ast.ParenExpr {
			return g.extract_if_expr(expr.expr)
		}
		ast.ModifierExpr {
			return g.extract_if_expr(expr.expr)
		}
		ast.UnsafeExpr {
			if expr.stmts.len == 1 {
				stmt0 := expr.stmts[0]
				if stmt0 is ast.ExprStmt {
					expr_stmt := stmt0 as ast.ExprStmt
					return g.extract_if_expr(expr_stmt.expr)
				}
			}
			return none
		}
		else {
			return none
		}
	}
}

fn (mut g Gen) gen_if_expr_stmt(node ast.IfExpr) {
	// Skip empty conditions (pure else blocks shouldn't appear at top level)
	if node.cond is ast.EmptyExpr {
		return
	}
	g.sb.write_string('if (')
	g.gen_expr(node.cond)
	g.sb.writeln(') {')
	g.indent++
	g.gen_stmts(node.stmts)
	g.indent--
	g.write_indent()
	g.sb.write_string('}')
	// Handle else / else-if
	if node.else_expr !is ast.EmptyExpr {
		if node.else_expr is ast.IfExpr {
			else_if := node.else_expr as ast.IfExpr
			if else_if.cond is ast.EmptyExpr {
				g.sb.writeln(' else {')
				g.indent++
				g.gen_stmts(else_if.stmts)
				g.indent--
				g.write_indent()
				g.sb.write_string('}')
			} else {
				g.sb.write_string(' else ')
				g.gen_if_expr_stmt(else_if)
			}
		} else {
			g.sb.writeln(' else {')
			g.indent++
			g.gen_stmts_from_expr(node.else_expr)
			g.indent--
			g.write_indent()
			g.sb.write_string('}')
		}
	}
	g.sb.writeln('')
}

fn (mut g Gen) gen_if_expr_ternary(node ast.IfExpr) {
	if node.cond is ast.EmptyExpr {
		if node.stmts.len == 1 && node.stmts[0] is ast.ExprStmt {
			stmt := node.stmts[0] as ast.ExprStmt
			g.gen_expr(stmt.expr)
		} else {
			g.sb.write_string('0')
		}
		return
	}
	g.sb.write_string('(')
	g.gen_expr(node.cond)
	g.sb.write_string(' ? ')
	if node.stmts.len == 1 && node.stmts[0] is ast.ExprStmt {
		stmt := node.stmts[0] as ast.ExprStmt
		if nested := g.extract_if_expr(stmt.expr) {
			g.gen_if_expr_value(nested)
		} else {
			g.gen_expr(stmt.expr)
		}
	} else {
		g.sb.write_string('0')
	}
	g.sb.write_string(' : ')
	if node.else_expr is ast.IfExpr {
		else_if := node.else_expr as ast.IfExpr
		g.gen_if_expr_value(else_if)
	} else if node.else_expr is ast.EmptyExpr {
		g.sb.write_string('0')
	} else {
		if nested := g.extract_if_expr(node.else_expr) {
			g.gen_if_expr_value(nested)
		} else {
			g.gen_expr(node.else_expr)
		}
	}
	g.sb.write_string(')')
}

fn (mut g Gen) gen_if_expr_value(node ast.IfExpr) {
	mut value_type := g.infer_if_expr_type(node)
	if g.if_expr_can_be_ternary(node) && value_type != '' && value_type != 'void' {
		g.gen_if_expr_ternary(node)
		return
	}
	if value_type == '' || value_type == 'void' {
		g.sb.write_string('({ ')
		g.gen_if_expr_stmt(node)
		g.sb.write_string('; 0; })')
		return
	}
	if value_type == 'int_literal' {
		value_type = 'int'
	}
	tmp_name := '_if_expr_t${g.tmp_counter}'
	g.tmp_counter++
	g.sb.write_string('({ ${value_type} ${tmp_name} = ${zero_value_for_type(value_type)}; ')
	g.gen_decl_if_expr(tmp_name, node)
	g.sb.write_string(' ${tmp_name}; })')
}

fn (mut g Gen) infer_if_expr_type(node ast.IfExpr) string {
	if t := g.get_expr_type_from_env(node) {
		if t != '' {
			return t
		}
	}
	if node.stmts.len == 1 && node.stmts[0] is ast.ExprStmt {
		stmt := node.stmts[0] as ast.ExprStmt
		t := g.get_expr_type(stmt.expr)
		if t != '' && t != 'int' {
			return t
		}
	}
	if node.else_expr is ast.IfExpr {
		t := g.infer_if_expr_type(node.else_expr)
		if t != '' {
			return t
		}
	} else if node.else_expr !is ast.EmptyExpr {
		t := g.get_expr_type(node.else_expr)
		if t != '' {
			return t
		}
	}
	return 'int'
}

fn (mut g Gen) expr_is_pointer(arg ast.Expr) bool {
	base_arg := if arg is ast.ModifierExpr { arg.expr } else { arg }
	if base_arg is ast.Ident {
		if base_arg.name == 'nil' {
			return true
		}
		// Prefer function-scope type information for identifiers.
		// Environment lookups can over-approximate loop variables as pointers.
		if local_type := g.get_local_var_c_type(base_arg.name) {
			if base_arg.name in g.cur_fn_mut_params {
				return true
			}
			return local_type.ends_with('*') || local_type in ['voidptr', 'charptr', 'byteptr']
		}
		if base_arg.name in g.cur_fn_mut_params {
			return true
		}
	}
	if raw_type := g.get_raw_type(base_arg) {
		if raw_type is types.Pointer || raw_type is types.Nil {
			return true
		}
		// Handle type aliases that resolve to pointers (e.g., voidptr, charptr, byteptr)
		if raw_type is types.Alias {
			if raw_type.base_type is types.Pointer {
				return true
			}
		}
	}
	match base_arg {
		ast.PrefixExpr {
			return base_arg.op == .amp
		}
		ast.SelectorExpr {
			if base_arg.rhs.name == 'data' {
				lhs_type := g.get_expr_type(base_arg.lhs)
				if lhs_type == 'array' || lhs_type.starts_with('Array_') || lhs_type == 'map'
					|| lhs_type.starts_with('Map_') || lhs_type == 'string'
					|| lhs_type.starts_with('strings__Builder') {
					return true
				}
			}
			field_type := g.selector_field_type(base_arg)
			if field_type.ends_with('*')
				|| field_type in ['void*', 'char*', 'u8*', 'byteptr', 'charptr', 'voidptr'] {
				return true
			}
		}
		ast.Ident {
			// handled above
		}
		ast.UnsafeExpr {
			if base_arg.stmts.len > 0 {
				last := base_arg.stmts[base_arg.stmts.len - 1]
				if last is ast.ExprStmt {
					return g.expr_produces_pointer(last.expr)
				}
			}
		}
		else {}
	}
	return g.get_expr_type(base_arg).ends_with('*')
}

// expr_produces_pointer checks if an expression produces a pointer value in C.
// More comprehensive than expr_is_pointer - handles casts, pointer arithmetic, etc.
// Used in gen_call_arg to avoid wrapping pointer expressions in compound literals.
fn (mut g Gen) expr_produces_pointer(arg ast.Expr) bool {
	base_arg := if arg is ast.ModifierExpr { arg.expr } else { arg }
	if g.expr_is_pointer(base_arg) {
		return true
	}
	match base_arg {
		ast.CastExpr {
			return g.is_pointer_type(base_arg.typ)
		}
		ast.InfixExpr {
			if base_arg.op == .plus || base_arg.op == .minus {
				return g.expr_produces_pointer(base_arg.lhs)
					|| g.expr_produces_pointer(base_arg.rhs)
			}
		}
		ast.ParenExpr {
			return g.expr_produces_pointer(base_arg.expr)
		}
		ast.UnsafeExpr {
			if base_arg.stmts.len > 0 {
				last := base_arg.stmts[base_arg.stmts.len - 1]
				if last is ast.ExprStmt {
					return g.expr_produces_pointer(last.expr)
				}
			}
		}
		else {}
	}
	return false
}

fn (mut g Gen) can_take_address(arg ast.Expr) bool {
	base_arg := if arg is ast.ModifierExpr { arg.expr } else { arg }
	match base_arg {
		ast.Ident, ast.IndexExpr, ast.ParenExpr {
			return true
		}
		ast.SelectorExpr {
			// Enum values / module symbols / type-qualified selectors are rvalues.
			if base_arg.lhs is ast.EmptyExpr {
				return false
			}
			if base_arg.lhs is ast.Ident {
				if g.is_module_ident(base_arg.lhs.name) || g.is_type_name(base_arg.lhs.name) {
					return false
				}
			}
			if raw_type := g.get_raw_type(base_arg) {
				if raw_type is types.Enum {
					return false
				}
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g Gen) gen_addr_of_expr(arg ast.Expr, typ string) {
	base_arg := if arg is ast.ModifierExpr { arg.expr } else { arg }
	if g.can_take_address(base_arg) {
		g.sb.write_string('&')
		g.gen_expr(base_arg)
		return
	}
	// StringLiteral already generates a compound literal (string){"...", len}
	// so just take its address directly to avoid double-wrapping
	if base_arg is ast.StringLiteral {
		g.sb.write_string('&')
		g.gen_expr(base_arg)
		return
	}
	// InitExpr: check if it generates a compound literal or a function call
	if base_arg is ast.InitExpr {
		init_type := g.expr_type_to_c(base_arg.typ)
		if init_type.starts_with('Map_') || init_type == 'map' {
			// Map init generates new_map() function call - use statement expression
			g.tmp_counter++
			tmp_name := '_addr_t${g.tmp_counter}'
			g.sb.write_string('({ ${init_type} ${tmp_name} = ')
			g.gen_expr(base_arg)
			g.sb.write_string('; &${tmp_name}; })')
			return
		}
		// Non-map InitExpr generates a compound literal - just take address
		g.sb.write_string('&')
		g.gen_expr(base_arg)
		return
	}
	// ArrayInitExpr already generates a compound literal
	if base_arg is ast.ArrayInitExpr {
		g.sb.write_string('&')
		g.gen_expr(base_arg)
		return
	}
	addr_type := unmangle_c_ptr_type(if typ == '' { 'int' } else { typ })
	// For struct types (array, map, string, etc.), compound literal (type){expr}
	// doesn't work when expr is a full struct value (it tries to initialize the first field).
	// Use GCC statement expression instead: ({ type _t = expr; &_t; })
	is_nonprimitive_value := addr_type !in primitive_types && !addr_type.ends_with('*')
	if addr_type in ['array', 'map', 'string'] || addr_type.starts_with('Array_')
		|| addr_type.starts_with('Map_') || addr_type.starts_with('_option_')
		|| addr_type.starts_with('_result_') || is_nonprimitive_value {
		g.tmp_counter++
		tmp_name := '_addr_t${g.tmp_counter}'
		g.sb.write_string('({ ${addr_type} ${tmp_name} = ')
		g.gen_expr(base_arg)
		g.sb.write_string('; &${tmp_name}; })')
		return
	}
	g.sb.write_string('&(${addr_type}){')
	g.gen_expr(base_arg)
	g.sb.write_string('}')
}

fn (mut g Gen) fn_pointer_return_type(expr ast.Expr) string {
	if raw_type := g.get_raw_type(expr) {
		match raw_type {
			types.FnType {
				if rt := raw_type.get_return_type() {
					return g.types_type_to_c(rt)
				}
				return 'void'
			}
			types.Alias {
				if raw_type.base_type is types.FnType {
					if rt := raw_type.base_type.get_return_type() {
						return g.types_type_to_c(rt)
					}
					return 'void'
				}
			}
			types.Pointer {
				if raw_type.base_type is types.FnType {
					if rt := raw_type.base_type.get_return_type() {
						return g.types_type_to_c(rt)
					}
					return 'void'
				}
				if raw_type.base_type is types.Alias && raw_type.base_type.base_type is types.FnType {
					if rt := raw_type.base_type.base_type.get_return_type() {
						return g.types_type_to_c(rt)
					}
					return 'void'
				}
			}
			else {}
		}
	}
	return ''
}

fn (mut g Gen) is_fn_pointer_expr(expr ast.Expr) bool {
	return g.fn_pointer_return_type(expr) != ''
}

fn (mut g Gen) should_auto_deref(arg ast.Expr) bool {
	if raw_type := g.get_raw_type(arg) {
		if raw_type is types.Pointer {
			match raw_type.base_type {
				types.Array, types.Map, types.Struct, types.Interface, types.Alias {
					return true
				}
				else {}
			}
		}
	}
	t := g.get_expr_type(arg)
	return t.ends_with('*') && t !in ['void*', 'char*', 'byteptr', 'charptr']
}

fn (mut g Gen) gen_call_arg(fn_name string, idx int, arg ast.Expr) {
	base_arg := if arg is ast.ModifierExpr { arg.expr } else { arg }
	if fn_name == 'new_map_init_noscan_value' && idx in [7, 8] {
		// new_map_init_noscan_value expects raw key/value element pointers.
		// Lowered dynamic arrays arrive as `array` values; pass their `.data`.
		// Raw fixed array literals should be emitted directly and decay to pointers.
		if base_arg is ast.ArrayInitExpr {
			g.gen_expr(base_arg)
		} else {
			g.gen_expr(base_arg)
			g.sb.write_string('.data')
		}
		return
	}
	if ptr_params := g.fn_param_is_ptr[fn_name] {
		if idx < ptr_params.len {
			want_ptr := ptr_params[idx]
			if want_ptr && base_arg is ast.PrefixExpr && base_arg.op == .amp {
				inner := base_arg.expr
				if g.expr_is_pointer(inner) || g.expr_produces_pointer(inner) {
					g.gen_expr(inner)
					return
				}
			}
			got_ptr := g.expr_is_pointer(base_arg)
			// Also check for pointer-producing expressions (casts, pointer arithmetic)
			// that expr_is_pointer misses but that produce pointer values in C.
			if want_ptr && !got_ptr && g.expr_produces_pointer(base_arg) {
				g.gen_expr(base_arg)
				return
			}
			if want_ptr && !got_ptr && g.can_take_address(base_arg) {
				g.sb.write_string('&')
				g.gen_expr(base_arg)
				return
			}
			if want_ptr && !got_ptr && !g.can_take_address(base_arg) {
				// Can't take address of expression (e.g., function call return value).
				// Use compound literal to make it addressable with proper type.
				if raw := g.get_raw_type(base_arg) {
					if raw is types.Pointer {
						// Already a pointer at the type level, just emit
						g.gen_expr(base_arg)
						return
					}
					c_type := g.types_type_to_c(raw)
					if c_type != '' {
						g.gen_addr_of_expr(base_arg, c_type)
						return
					}
				}
				// Fallback: use expression type or default to array
				wrap_type := g.get_expr_type(base_arg)
				if wrap_type != '' {
					g.gen_addr_of_expr(base_arg, wrap_type)
					return
				}
				if base_arg is ast.StringLiteral || base_arg is ast.StringInterLiteral {
					g.gen_addr_of_expr(base_arg, 'string')
					return
				}
				if base_arg is ast.BasicLiteral {
					lit_type := if base_arg.kind == .string {
						'string'
					} else {
						'int'
					}
					g.gen_addr_of_expr(base_arg, lit_type)
					return
				}
				g.gen_addr_of_expr(base_arg, 'int')
				return
			}
			if !want_ptr && got_ptr && g.should_auto_deref(base_arg) {
				g.sb.write_string('(*')
				g.gen_expr(base_arg)
				g.sb.write_string(')')
				return
			}
		}
	}
	// Check if argument needs sum type wrapping (variant passed where sum type expected)
	if param_types := g.fn_param_types[fn_name] {
		if idx < param_types.len {
			param_type := param_types[idx]
			if variants := g.sum_type_variants[param_type] {
				mut arg_type := g.get_expr_type(base_arg)
				// For smartcast dereference patterns (*(Type*)(expr._data._Type)),
				// extract the actual type from the cast
				if arg_type == 'int' || arg_type == '' {
					// Unwrap ParenExpr if present
					inner_arg := if base_arg is ast.ParenExpr {
						base_arg.expr
					} else {
						base_arg
					}
					if inner_arg is ast.PrefixExpr && inner_arg.op == .mul {
						if inner_arg.expr is ast.CastExpr {
							cast_type := g.expr_type_to_c(inner_arg.expr.typ)
							if cast_type.ends_with('*') {
								arg_type = cast_type[..cast_type.len - 1]
							}
						}
					}
				}
				if arg_type != param_type && arg_type != '' {
					mut tag := -1
					mut field_name := ''
					for i, v in variants {
						if v == arg_type || arg_type.ends_with('__${v}')
							|| v.ends_with('__${arg_type}') {
							tag = i
							field_name = v
							break
						}
					}
					if tag >= 0 {
						is_primitive :=
							arg_type in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64', 'bool', 'rune', 'byte', 'usize', 'isize']
							|| arg_type in g.primitive_type_aliases
						g.gen_sum_type_wrap(param_type, field_name, tag, is_primitive,
							base_arg, arg_type)
						return
					}
				}
			}
		}
	}
	g.gen_expr(base_arg)
}

fn (mut g Gen) method_receiver_base_type(expr ast.Expr) string {
	if expr is ast.Ident {
		if local_type := g.get_local_var_c_type(expr.name) {
			mut base := local_type
			if base.ends_with('*') {
				base = base[..base.len - 1]
			}
			if base != '' && base != 'int' {
				return base
			}
		}
	}
	if raw_type := g.get_raw_type(expr) {
		match raw_type {
			types.Pointer {
				return g.types_type_to_c(raw_type.base_type)
			}
			else {
				return g.types_type_to_c(raw_type)
			}
		}
	}
	mut receiver_type := g.get_expr_type(expr)
	if receiver_type.ends_with('*') {
		receiver_type = receiver_type[..receiver_type.len - 1]
	}
	if receiver_type in ['voidptr', 'void*'] {
		return 'void'
	}
	return receiver_type
}

fn (mut g Gen) resolve_container_method_name(receiver ast.Expr, method_name string, expected_params int) string {
	mut candidates := []string{}
	if raw_type := g.get_raw_type(receiver) {
		match raw_type {
			types.Pointer {
				match raw_type.base_type {
					types.Array, types.ArrayFixed {
						candidates << 'array'
					}
					types.Map {
						candidates << 'map'
					}
					types.String {
						candidates << 'string'
					}
					types.Alias {
						match raw_type.base_type.base_type {
							types.Array, types.ArrayFixed {
								candidates << 'array'
							}
							types.Map {
								candidates << 'map'
							}
							types.String {
								candidates << 'string'
							}
							else {}
						}
					}
					else {}
				}
			}
			types.Array, types.ArrayFixed {
				candidates << 'array'
			}
			types.Map {
				candidates << 'map'
			}
			types.String {
				candidates << 'string'
			}
			types.Alias {
				match raw_type.base_type {
					types.Array, types.ArrayFixed {
						candidates << 'array'
					}
					types.Map {
						candidates << 'map'
					}
					types.String {
						candidates << 'string'
					}
					else {}
				}
			}
			else {}
		}
	}
	recv_name := g.method_receiver_base_type(receiver)
	if recv_name == 'array' || recv_name.starts_with('Array_') {
		candidates << 'array'
	}
	if recv_name == 'map' || recv_name.starts_with('Map_') {
		candidates << 'map'
	}
	if recv_name == 'string' {
		candidates << 'string'
	}
	for base in candidates {
		candidate := '${base}__${method_name}'
		if params := g.fn_param_is_ptr[candidate] {
			if params.len == expected_params {
				return candidate
			}
		} else if candidate in g.fn_return_types {
			return candidate
		}
	}
	return ''
}

fn (mut g Gen) resolve_call_name(lhs ast.Expr, arg_count int) string {
	mut name := ''
	if lhs is ast.Ident {
		name = sanitize_fn_ident(lhs.name)
	} else if lhs is ast.SelectorExpr {
		if lhs.lhs is ast.Ident && lhs.lhs.name == 'C' {
			return lhs.rhs.name
		}
		// Static type method call: Type.method(...)
		if lhs.lhs is ast.Ident && g.is_type_name(lhs.lhs.name) {
			return '${g.get_qualified_name(lhs.lhs.name)}__${sanitize_fn_ident(lhs.rhs.name)}'
		}
		if lhs.lhs is ast.Ident && g.is_module_ident(lhs.lhs.name) {
			name = '${lhs.lhs.name}__${sanitize_fn_ident(lhs.rhs.name)}'
		} else {
			method_name := sanitize_fn_ident(lhs.rhs.name)
			base_type := g.method_receiver_base_type(lhs.lhs)
			name = '${base_type}__${method_name}'
			if name !in g.fn_return_types && name !in g.fn_param_is_ptr {
				expected_params := arg_count + 1
				mut is_array_receiver := base_type == 'array' || base_type.starts_with('Array_')
				if !is_array_receiver {
					if raw_type := g.get_raw_type(lhs.lhs) {
						match raw_type {
							types.Array, types.ArrayFixed {
								is_array_receiver = true
							}
							types.Pointer {
								match raw_type.base_type {
									types.Array, types.ArrayFixed {
										is_array_receiver = true
									}
									types.Alias {
										match raw_type.base_type.base_type {
											types.Array, types.ArrayFixed {
												is_array_receiver = true
											}
											else {}
										}
									}
									else {}
								}
							}
							types.Alias {
								match raw_type.base_type {
									types.Array, types.ArrayFixed {
										is_array_receiver = true
									}
									else {}
								}
							}
							else {}
						}
					}
				}
				if is_array_receiver {
					array_candidate := 'array__${method_name}'
					if params := g.fn_param_is_ptr[array_candidate] {
						if params.len == expected_params {
							name = array_candidate
						}
					}
				}
				if name == '' || (name !in g.fn_return_types && name !in g.fn_param_is_ptr) {
					fallback := g.resolve_container_method_name(lhs.lhs, method_name,
						expected_params)
					if fallback != '' {
						name = fallback
					}
				}
				if name != '' && (name in g.fn_return_types || name in g.fn_param_is_ptr) {
					// resolved
				} else {
					suffix := '__${method_name}'
					mut candidates := []string{}
					for fn_name, ret_type in g.fn_return_types {
						if fn_name.ends_with(suffix) && fn_name in g.fn_param_is_ptr {
							if params := g.fn_param_is_ptr[fn_name] {
								if params.len == expected_params && ret_type != '' {
									candidates << fn_name
								}
							}
						}
					}
					if candidates.len == 1 {
						name = candidates[0]
					}
				}
				// Fallbacks when selector receiver typing is incomplete.
				if name == '' || (name !in g.fn_return_types && name !in g.fn_param_is_ptr) {
					if method_name == 'clone' {
						if lhs.lhs is ast.SelectorExpr {
							recv_sel := lhs.lhs as ast.SelectorExpr
							recv_field_type := g.selector_field_type(recv_sel)
							if recv_field_type == 'array' || recv_field_type.starts_with('Array_')
								|| recv_field_type.starts_with('Array_fixed_') {
								name = 'array__clone'
							}
						}
					} else if method_name == 'str' {
						mut enum_type := ''
						if lhs.lhs is ast.SelectorExpr {
							recv_sel := lhs.lhs as ast.SelectorExpr
							recv_field_type := g.selector_field_type(recv_sel)
							if recv_field_type != '' && g.is_enum_type(recv_field_type) {
								enum_type = recv_field_type
							}
						}
						if enum_type == '' {
							recv_type := g.method_receiver_base_type(lhs.lhs)
							if recv_type != '' && g.is_enum_type(recv_type) {
								enum_type = recv_type
							}
						}
						if enum_type != '' {
							name = '${enum_type}__str'
						}
					}
				}
			}
		}
	}
	if name == 'builtin__new_array_from_c_array_noscan' {
		name = 'new_array_from_c_array'
	}
	if name == 'builtin__array_push_noscan' {
		name = 'array__push'
	}
	if name == 'voidptr__vbytes' {
		name = 'void__vbytes'
	}
	if name.ends_with('__bytes') && name !in g.fn_return_types && name !in g.fn_param_is_ptr {
		if 'string__bytes' in g.fn_return_types || 'string__bytes' in g.fn_param_is_ptr {
			name = 'string__bytes'
		}
	}
	if is_c_runtime_function(name) {
		return name
	}
	if name != '' && g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin'
		&& !name.contains('__') {
		qualified := '${g.cur_module}__${name}'
		if qualified in g.fn_return_types || qualified in g.fn_param_is_ptr {
			return qualified
		}
		if name in g.fn_return_types || name in g.fn_param_is_ptr {
			return name
		}
		return qualified
	}
	return name
}

fn (mut g Gen) get_call_return_type(lhs ast.Expr, arg_count int) ?string {
	if lhs is ast.SelectorExpr {
		if lhs.lhs is ast.Ident && lhs.lhs.name == 'C' {
			if ret := g.fn_return_types[lhs.rhs.name] {
				return ret
			}
			// Fallbacks for common C symbols used by vlib/os where declaration
			// metadata can be missing in transformed environments.
			match lhs.rhs.name {
				'open', 'chdir', 'proc_pidpath' { return 'int' }
				'signal' { return 'void*' }
				else {}
			}
		}
	}
	if lhs is ast.SelectorExpr {
		if lhs.rhs.name in ['hash_fn', 'key_eq_fn', 'clone_fn', 'free_fn'] {
			base_type := g.method_receiver_base_type(lhs.lhs)
			if base_type == 'map' || base_type.starts_with('Map_') {
				return match lhs.rhs.name {
					'hash_fn' { 'u64' }
					'key_eq_fn' { 'bool' }
					else { 'void' }
				}
			}
		}
	}
	if lhs !is ast.Ident {
		mut allow_fn_ptr_lookup := true
		if lhs is ast.SelectorExpr {
			if lhs.lhs is ast.Ident
				&& (g.is_module_ident(lhs.lhs.name) || g.is_type_name(lhs.lhs.name)) {
				// Module/type selectors represent direct function/method calls, not
				// function-pointer values.
				allow_fn_ptr_lookup = false
			}
		}
		if allow_fn_ptr_lookup {
			fn_ptr_ret := g.fn_pointer_return_type(lhs)
			if fn_ptr_ret != '' {
				return fn_ptr_ret
			}
		}
	}
	c_name := g.resolve_call_name(lhs, arg_count)
	if c_name == '' {
		if lhs is ast.Ident {
			match lhs.name {
				'open', 'chdir', 'proc_pidpath' { return 'int' }
				'signal' { return 'void*' }
				else {}
			}
		}
		return none
	}
	if ret := g.fn_return_types[c_name] {
		return ret
	}
	match c_name {
		'open', 'chdir', 'proc_pidpath' { return 'int' }
		'signal' { return 'void*' }
		else {}
	}
	return none
}

fn (mut g Gen) normalize_named_call_args(fn_name string, call_args []ast.Expr) []ast.Expr {
	_ = fn_name
	mut out := []ast.Expr{cap: call_args.len}
	out << call_args
	return out
}

fn call_args_have_field_init(call_args []ast.Expr) bool {
	for arg in call_args {
		if arg is ast.FieldInit {
			return true
		}
	}
	return false
}

fn (mut g Gen) gen_named_struct_call_arg(fn_name string, param_idx int, call_args []ast.Expr, arg_idx int) int {
	if arg_idx >= call_args.len || call_args[arg_idx] !is ast.FieldInit {
		return -1
	}
	param_types := g.fn_param_types[fn_name] or { return -1 }
	if param_idx >= param_types.len {
		return -1
	}
	raw_param_type := unmangle_c_ptr_type(param_types[param_idx])
	param_type := raw_param_type.trim_right('*')
	if param_type == '' || param_type in primitive_types || param_type.starts_with('Array_')
		|| param_type.starts_with('Map_') || param_type.starts_with('_option_')
		|| param_type.starts_with('_result_') {
		return -1
	}
	mut fields := []ast.FieldInit{}
	mut j := arg_idx
	for j < call_args.len {
		arg := call_args[j]
		if arg is ast.FieldInit {
			fields << arg
			j++
			continue
		}
		break
	}
	if fields.len == 0 {
		return -1
	}
	is_ptr_param := if ptrs := g.fn_param_is_ptr[fn_name] {
		param_idx < ptrs.len && ptrs[param_idx]
	} else {
		raw_param_type.ends_with('*')
	}
	if is_ptr_param {
		g.sb.write_string('&')
	}
	g.sb.write_string('((${param_type}){')
	for i, field in fields {
		if i > 0 {
			g.sb.write_string(', ')
		}
		field_name := if field.name.contains('.') {
			field.name
		} else {
			escape_c_keyword(field.name)
		}
		g.sb.write_string('.${field_name} = ')
		g.gen_expr(field.value)
	}
	g.sb.write_string('})')
	return j
}

fn (mut g Gen) gen_type_cast_expr(type_name string, expr ast.Expr) {
	expr_type := g.get_expr_type(expr)
	if expr_type.starts_with('_result_') && g.result_value_type(expr_type) != '' {
		g.sb.write_string('((${type_name})(')
		g.gen_unwrapped_value_expr(expr)
		g.sb.write_string('))')
		return
	}
	if expr_type.starts_with('_option_') && option_value_type(expr_type) != '' {
		g.sb.write_string('((${type_name})(')
		g.gen_unwrapped_value_expr(expr)
		g.sb.write_string('))')
		return
	}
	if variants := g.sum_type_variants[type_name] {
		mut inner_type := expr_type
		if inner_type == '' || inner_type == 'int' {
			match expr {
				ast.CallExpr {
					if ret := g.get_call_return_type(expr.lhs, expr.args.len) {
						if ret != '' {
							inner_type = ret
						}
					}
				}
				ast.CallOrCastExpr {
					if ret := g.get_call_return_type(expr.lhs, 1) {
						if ret != '' {
							inner_type = ret
						}
					}
				}
				ast.InitExpr {
					inner_type = g.expr_type_to_c(expr.typ)
				}
				ast.SelectorExpr {
					field_type := g.selector_field_type(expr)
					if field_type != '' {
						inner_type = field_type
					}
				}
				else {}
			}
		}
		// Identity cast: inner type is already the target sum type, no wrapping needed
		if inner_type == type_name {
			g.gen_expr(expr)
			return
		}
		if inner_type != '' {
			mut tag := -1
			mut field_name := ''
			for i, v in variants {
				if v == inner_type || inner_type.ends_with('__${v}')
					|| v.ends_with('__${inner_type}') {
					tag = i
					field_name = v
					break
				}
			}
			// If direct matching failed, check if inner_type is a known sum type
			// that appears as a variant of the target sum type (e.g. ast__Type -> ast__Expr._Type)
			if tag < 0 && inner_type in g.sum_type_variants {
				inner_short := if inner_type.contains('__') {
					inner_type.all_after_last('__')
				} else {
					inner_type
				}
				for i, v in variants {
					if v == inner_short {
						tag = i
						field_name = v
						break
					}
				}
			}
			if tag >= 0 {
				is_primitive :=
					inner_type in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64', 'bool', 'rune', 'byte', 'usize', 'isize']
					|| inner_type in g.primitive_type_aliases
				g.gen_sum_type_wrap(type_name, field_name, tag, is_primitive, expr, inner_type)
				return
			}
		}
		// Fallback: try to infer variant from expression structure
		inferred := g.infer_sum_variant_from_expr(type_name, variants, expr)
		if inferred.tag >= 0 {
			g.gen_sum_type_wrap(type_name, inferred.field_name, inferred.tag, inferred.is_primitive,
				expr, inferred.inner_type)
			return
		}
	}
	if g.gen_interface_cast(type_name, expr) {
		return
	}
	// For non-sum-types, use C cast
	g.sb.write_string('((${type_name})(')
	g.gen_expr(expr)
	g.sb.write_string('))')
}

struct SumVariantMatch {
	tag          int
	field_name   string
	is_primitive bool
	inner_type   string
}

fn (mut g Gen) infer_sum_variant_from_expr(type_name string, variants []string, expr ast.Expr) SumVariantMatch {
	// Handle module constants used as sum variants in selfhosted checker code
	// (e.g. char_, string_, void_, nil_, none_).
	if expr is ast.Ident {
		variant_hint := match expr.name {
			'char_' {
				'Char'
			}
			'string_' {
				'String'
			}
			'void_' {
				'Void'
			}
			'nil_' {
				'Nil'
			}
			'none_' {
				'None'
			}
			'rune_' {
				'Rune'
			}
			'usize_' {
				'USize'
			}
			'isize_' {
				'ISize'
			}
			'thread_' {
				'Thread'
			}
			'chan_' {
				'Channel'
			}
			'bool_', 'i8_', 'i16_', 'i32_', 'int_', 'i64_', 'u8_', 'u16_', 'u32_', 'u64_', 'f32_',
			'f64_', 'int_literal_', 'float_literal_' {
				'Primitive'
			}
			else {
				''
			}
		}
		if variant_hint != '' {
			for i, v in variants {
				v_short := if v.contains('__') { v.all_after_last('__') } else { v }
				if v_short == variant_hint || v == variant_hint || v.ends_with('__${variant_hint}') {
					return SumVariantMatch{
						tag:          i
						field_name:   v
						is_primitive: variant_hint == 'Primitive'
						inner_type:   ''
					}
				}
			}
		}
	}

	// For InitExpr, infer from the struct type name
	if expr is ast.InitExpr {
		init_type := g.expr_type_to_c(expr.typ)
		mut resolved_init_type := init_type
		if (resolved_init_type == '' || resolved_init_type == 'int') && expr.typ is ast.Ident {
			resolved_init_type = expr.typ.name.replace('.', '__')
		}
		if resolved_init_type != '' {
			init_short := if resolved_init_type.contains('__') {
				resolved_init_type.all_after_last('__')
			} else {
				resolved_init_type
			}
			for i, v in variants {
				if v == init_short || v == resolved_init_type
					|| resolved_init_type.ends_with('__${v}')
					|| v.ends_with('__${resolved_init_type}') {
					return SumVariantMatch{
						tag:          i
						field_name:   v
						is_primitive: false
						inner_type:   resolved_init_type
					}
				}
			}
		}
	}
	// For CallOrCastExpr that is a type cast to a known sum type variant
	if expr is ast.CallOrCastExpr {
		if expr.lhs is ast.Ident {
			cast_type := g.expr_type_to_c(expr.lhs)
			if cast_type != '' {
				cast_short := if cast_type.contains('__') {
					cast_type.all_after_last('__')
				} else {
					cast_type
				}
				for i, v in variants {
					if v == cast_short || v == cast_type || cast_type.ends_with('__${v}') {
						return SumVariantMatch{
							tag:          i
							field_name:   v
							is_primitive: false
							inner_type:   cast_type
						}
					}
				}
			}
		}
		if expr.lhs is ast.Type {
			cast_type := g.expr_type_to_c(expr.lhs)
			if cast_type != '' {
				cast_short := if cast_type.contains('__') {
					cast_type.all_after_last('__')
				} else {
					cast_type
				}
				for i, v in variants {
					if v == cast_short || v == cast_type || cast_type.ends_with('__${v}') {
						return SumVariantMatch{
							tag:          i
							field_name:   v
							is_primitive: false
							inner_type:   cast_type
						}
					}
				}
			}
		}
	}
	// Generic fallback: if the expression is already a concrete AST sum variant
	// (e.g. ast.BasicLiteral being cast to ast.Expr), map it by runtime variant name.
	expr_variant := expr.type_name()
	if expr_variant != '' {
		expr_variant_c := expr_variant.replace('.', '__')
		expr_variant_short := if expr_variant.contains('.') {
			expr_variant.all_after_last('.')
		} else if expr_variant.contains('__') {
			expr_variant.all_after_last('__')
		} else {
			expr_variant
		}
		for i, v in variants {
			v_short := if v.contains('__') { v.all_after_last('__') } else { v }
			if v_short == expr_variant_short || v == expr_variant || v == expr_variant_c {
				inner_type := if type_name.contains('__') {
					'${type_name.all_before_last('__')}__${v_short}'
				} else {
					v_short
				}
				return SumVariantMatch{
					tag:          i
					field_name:   v_short
					is_primitive: false
					inner_type:   inner_type
				}
			}
		}
	}
	return SumVariantMatch{
		tag: -1
	}
}

fn (g &Gen) is_scalar_sum_payload_type(type_name string) bool {
	return type_name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64',
		'bool', 'rune', 'byte', 'usize', 'isize']
}

fn (mut g Gen) unwrap_addr_of_value_expr(expr ast.Expr) ?ast.Expr {
	match expr {
		ast.PrefixExpr {
			if expr.op == .amp {
				return expr.expr
			}
		}
		ast.CastExpr {
			cast_type := g.expr_type_to_c(expr.typ)
			if cast_type in ['void*', 'voidptr'] {
				if expr.expr is ast.PrefixExpr && expr.expr.op == .amp {
					return expr.expr.expr
				}
			}
		}
		ast.ParenExpr {
			return g.unwrap_addr_of_value_expr(expr.expr)
		}
		else {}
	}
	return none
}

fn (mut g Gen) gen_sum_type_wrap(type_name string, field_name string, tag int, is_primitive bool, expr ast.Expr, inner_type string) {
	_ = is_primitive
	g.sb.write_string('((${type_name}){._tag = ${tag}, ._data._${field_name} = ')
	mut resolved_type := inner_type
	if resolved_type == '' || resolved_type == 'void*' || resolved_type == 'int'
		|| resolved_type == type_name {
		if type_name.contains('__') {
			resolved_type = '${type_name.all_before_last('__')}__${field_name}'
		} else {
			resolved_type = field_name
		}
	}
	if g.is_scalar_sum_payload_type(resolved_type) {
		// Keep scalar payloads encoded in pointer-size space. Smartcast extraction expects this.
		g.sb.write_string('((void*)((intptr_t)(')
		g.gen_expr(expr)
		g.sb.write_string(')))')
	} else if inner_type == 'void*' {
		// `&fn_call()` lowers to a statement-expression pointer to a temporary.
		// Copy by-value from the inner expression to avoid dangling addresses.
		if inner_expr := g.unwrap_addr_of_value_expr(expr) {
			g.tmp_counter++
			tmp_name := '_st${g.tmp_counter}'
			g.sb.write_string('((void*)({ ${resolved_type} ${tmp_name} = ')
			g.gen_expr(inner_expr)
			g.sb.write_string('; memdup(&${tmp_name}, sizeof(${resolved_type})); }))')
		} else {
			g.sb.write_string('((void*)memdup(')
			g.gen_expr(expr)
			g.sb.write_string(', sizeof(${resolved_type})))')
		}
	} else {
		// Must heap-allocate (memdup) non-primitive sum type variants to avoid
		// dangling pointers to local variables that go out of scope.
		g.tmp_counter++
		tmp_name := '_st${g.tmp_counter}'
		g.sb.write_string('((void*)({ ${resolved_type} ${tmp_name} = ')
		g.gen_expr(expr)
		g.sb.write_string('; memdup(&${tmp_name}, sizeof(${resolved_type})); }))')
	}
	g.sb.write_string('})')
}

fn (mut g Gen) infer_array_contains_elem_type(name string, call_args []ast.Expr) string {
	arr_type := if call_args.len > 0 { g.get_expr_type(call_args[0]) } else { '' }
	if arr_type.starts_with('Array_fixed_') {
		if finfo := g.collected_fixed_array_types[arr_type] {
			return finfo.elem_type
		}
	} else if arr_type.starts_with('Array_') {
		return arr_type['Array_'.len..]
	}
	mut suffix := ''
	if name.starts_with('array__contains_') {
		suffix = name['array__contains_'.len..]
	}
	if suffix != '' && suffix != 'void' {
		return suffix
	}
	if call_args.len > 1 {
		rhs_type := g.get_expr_type(call_args[1])
		if rhs_type != '' && rhs_type != 'int_literal' && rhs_type != 'float_literal'
			&& rhs_type != 'void' {
			return rhs_type.trim_right('*')
		}
	}
	return 'int'
}

fn (mut g Gen) infer_array_elem_type_from_expr(arr_expr ast.Expr) string {
	if raw_type := g.get_raw_type(arr_expr) {
		match raw_type {
			types.Array {
				return g.types_type_to_c(raw_type.elem_type)
			}
			types.Pointer {
				match raw_type.base_type {
					types.Array {
						return g.types_type_to_c(raw_type.base_type.elem_type)
					}
					types.Alias {
						if raw_type.base_type.base_type is types.Array {
							return g.types_type_to_c(raw_type.base_type.base_type.elem_type)
						}
					}
					else {}
				}
			}
			types.Alias {
				match raw_type.base_type {
					types.Array {
						return g.types_type_to_c(raw_type.base_type.elem_type)
					}
					else {}
				}
			}
			else {}
		}
	}
	arr_type := g.get_expr_type(arr_expr)
	elem := array_alias_elem_type(arr_type)
	if elem != '' {
		return elem
	}
	return ''
}

fn (mut g Gen) gen_array_contains_call(name string, call_args []ast.Expr) bool {
	if !name.starts_with('array__contains_') || call_args.len != 2 {
		return false
	}
	elem_type := g.infer_array_contains_elem_type(name, call_args)
	g.sb.write_string('array__contains(')
	g.gen_expr(call_args[0])
	g.sb.write_string(', ')
	g.gen_addr_of_expr(call_args[1], elem_type)
	g.sb.write_string(')')
	return true
}

fn (mut g Gen) is_interface_receiver_for(receiver ast.Expr, iface_name string) bool {
	if raw_type := g.get_raw_type(receiver) {
		match raw_type {
			types.Interface {
				recv_name := g.types_type_to_c(raw_type)
				return recv_name == iface_name
					|| recv_name.all_after_last('__') == iface_name.all_after_last('__')
			}
			types.Pointer {
				if raw_type.base_type is types.Interface {
					recv_name := g.types_type_to_c(raw_type.base_type)
					return recv_name == iface_name
						|| recv_name.all_after_last('__') == iface_name.all_after_last('__')
				}
			}
			else {}
		}
	}
	return false
}

fn (g &Gen) get_sum_type_variants_for(type_name string) []string {
	sum_type := type_name.trim_right('*')
	if sum_type == '' {
		return []string{}
	}
	if vs := g.sum_type_variants[sum_type] {
		return vs.clone()
	}
	if sum_type.contains('__') {
		short_sum := sum_type.all_after_last('__')
		if vs := g.sum_type_variants[short_sum] {
			return vs.clone()
		}
	} else {
		qualified_sum := g.get_qualified_name(sum_type)
		if vs := g.sum_type_variants[qualified_sum] {
			return vs.clone()
		}
	}
	return []string{}
}

fn sum_variant_type_name_label(variant string) string {
	return variant.replace('__', '.')
}

fn (mut g Gen) gen_interface_method_dispatch(name string, call_args []ast.Expr) bool {
	idx := name.last_index('__') or { return false }
	iface_name := name[..idx]
	method_name := name[idx + 2..]
	if call_args.len == 0 || method_name == '' {
		return false
	}
	receiver := call_args[0]
	if !g.is_interface_receiver_for(receiver, iface_name) {
		return false
	}
	sep := if g.expr_is_pointer(receiver) { '->' } else { '.' }
	g.gen_expr(receiver)
	g.sb.write_string('${sep}${method_name}(')
	if call_args.len == 1 {
		g.gen_expr(receiver)
		g.sb.write_string('${sep}_object')
	} else {
		for i in 1 .. call_args.len {
			if i > 1 {
				g.sb.write_string(', ')
			}
			g.gen_expr(call_args[i])
		}
	}
	g.sb.write_string(')')
	return true
}

fn (mut g Gen) is_flag_enum_expr(expr ast.Expr) bool {
	if raw_type := g.get_raw_type(expr) {
		match raw_type {
			types.Enum {
				return raw_type.is_flag
			}
			types.Pointer {
				if raw_type.base_type is types.Enum {
					return raw_type.base_type.is_flag
				}
			}
			types.Alias {
				if raw_type.base_type is types.Enum {
					return raw_type.base_type.is_flag
				}
			}
			else {}
		}
	}
	return false
}

fn (mut g Gen) gen_call_expr(lhs ast.Expr, args []ast.Expr) {
	if lhs is ast.SelectorExpr && g.is_fn_pointer_expr(lhs) {
		mut should_emit_fnptr_call := true
		resolved := g.resolve_call_name(lhs, args.len)
		if resolved != '' && (resolved in g.fn_param_is_ptr || resolved in g.fn_return_types) {
			should_emit_fnptr_call = false
		}
		if should_emit_fnptr_call {
			// Function pointer fields are stored as void* in C.
			// We need to cast to the correct function pointer type before calling.
			ret_type := g.fn_pointer_return_type(lhs)
			c_ret := if ret_type == '' { 'void' } else { ret_type }
			g.sb.write_string('((${c_ret}(*)())')
			g.gen_expr(lhs)
			g.sb.write_string(')(')
			for i, arg in args {
				if i > 0 {
					g.sb.write_string(', ')
				}
				g.gen_expr(arg)
			}
			g.sb.write_string(')')
			return
		}
	}
	mut name := ''
	mut call_args := []ast.Expr{}
	call_args << args
	if lhs is ast.Ident {
		name = sanitize_fn_ident(lhs.name)
	} else if lhs is ast.SelectorExpr {
		// Sum types expose `type_name()` as compiler magic.
		// Lower it directly from the runtime tag to a string literal table.
		if lhs.rhs.name == 'type_name' && args.len == 0 {
			lhs_type := g.get_expr_type(lhs.lhs)
			variants := g.get_sum_type_variants_for(lhs_type)
			if variants.len > 0 {
				sep := if g.expr_is_pointer(lhs.lhs) { '->' } else { '.' }
				g.tmp_counter++
				tag_tmp := '_sum_tag_t${g.tmp_counter}'
				g.tmp_counter++
				name_tmp := '_sum_type_name_t${g.tmp_counter}'
				g.sb.write_string('({ int ${tag_tmp} = (')
				g.gen_expr(lhs.lhs)
				g.sb.write_string(')${sep}_tag; string ${name_tmp} = (string){"", 0}; switch (${tag_tmp}) { ')
				for i, variant in variants {
					display_name := sum_variant_type_name_label(variant)
					escaped := display_name.replace('\\', '\\\\').replace('"', '\\"')
					g.sb.write_string('case ${i}: ${name_tmp} = (string){"${escaped}", ${display_name.len}}; break; ')
				}
				g.sb.write_string('default: break; } ${name_tmp}; })')
				return
			}
		}
		if lhs.rhs.name in ['hash_fn', 'key_eq_fn', 'clone_fn', 'free_fn'] {
			base_type := g.method_receiver_base_type(lhs.lhs)
			if base_type == 'map' || base_type.starts_with('Map_') {
				g.gen_expr(lhs)
				g.sb.write_string('(')
				for i, arg in args {
					if i > 0 {
						g.sb.write_string(', ')
					}
					g.gen_expr(arg)
				}
				g.sb.write_string(')')
				return
			}
		}
		// Handle C.puts, C.putchar etc.
		if lhs.lhs is ast.Ident && lhs.lhs.name == 'C' {
			name = lhs.rhs.name
			g.sb.write_string('${name}(')
			for i, arg in args {
				if i > 0 {
					g.sb.write_string(', ')
				}
				g.gen_expr(arg)
			}
			g.sb.write_string(')')
			return
		}
		// module.fn(...) => module__fn(...)
		if lhs.lhs is ast.Ident && g.is_type_name(lhs.lhs.name) {
			// Static type method call: Type.method(...)
			name = '${g.get_qualified_name(lhs.lhs.name)}__${sanitize_fn_ident(lhs.rhs.name)}'
		} else if lhs.lhs is ast.Ident && g.is_module_ident(lhs.lhs.name) {
			name = '${lhs.lhs.name}__${sanitize_fn_ident(lhs.rhs.name)}'
		} else {
			// value.method(args...) => ReceiverType__method(value, args...)
			name = g.resolve_call_name(lhs, args.len)
			if name == '' {
				method_name := sanitize_fn_ident(lhs.rhs.name)
				base_type := g.method_receiver_base_type(lhs.lhs)
				name = '${base_type}__${method_name}'
			}
			call_args = []ast.Expr{cap: args.len + 1}
			call_args << lhs.lhs
			call_args << args
		}
	}

	// Transformer helper maps directly to builtin implementation name in vlib.
	if name == 'builtin__new_array_from_c_array_noscan' {
		name = 'new_array_from_c_array'
	}
	if name == 'builtin__array_push_noscan' {
		name = 'array__push'
	}
	if name == 'gen_v__new_gen' {
		name = 'v__new_gen'
	}
	if name.starts_with('strings__Builder__') && name !in g.fn_param_is_ptr
		&& name !in g.fn_return_types {
		method_name := name.all_after_last('__')
		array_name := 'array__${method_name}'
		if array_name in g.fn_param_is_ptr || array_name in g.fn_return_types {
			name = array_name
		}
	}
	call_args = g.normalize_named_call_args(name, call_args)
	if name.contains('__new') && call_args.len > 0 {
		expected_ctor_type := name.all_before_last('__new')
		mut arg_type_name := ''
		first_arg := call_args[0]
		match first_arg {
			ast.Ident {
				arg_type_name = first_arg.name
			}
			ast.SelectorExpr {
				if first_arg.lhs is ast.Ident {
					lhs_ident := first_arg.lhs as ast.Ident
					arg_type_name = '${lhs_ident.name}__${first_arg.rhs.name}'
				} else {
					arg_type_name = first_arg.rhs.name
				}
			}
			ast.Type {
				arg_type_name = g.expr_type_to_c(first_arg)
			}
			else {}
		}
		if arg_type_name == '' {
			arg_type_name = g.get_expr_type(first_arg)
		}
		arg_type_name = arg_type_name.trim_space().trim_left('&').trim_left('*')
		if arg_type_name != '' && (arg_type_name == expected_ctor_type
			|| arg_type_name == expected_ctor_type.all_after_last('__')) {
			call_args.delete(0)
		}
	}
	if call_args.len == 1 && g.is_type_name(name) && !name.ends_with('__new') {
		g.gen_type_cast_expr(name, call_args[0])
		return
	}
	if lhs is ast.Ident && g.is_type_name(lhs.name) && call_args.len == 1 {
		type_name := g.expr_type_to_c(lhs)
		g.gen_type_cast_expr(type_name, call_args[0])
		return
	}
	if lhs is ast.SelectorExpr && call_args.len == 1 && lhs.lhs is ast.Ident {
		qualified_type := '${lhs.lhs.name}__${lhs.rhs.name}'
		if g.is_type_name(qualified_type) {
			type_name := g.expr_type_to_c(lhs)
			g.gen_type_cast_expr(type_name, call_args[0])
			return
		}
	}
	if name == 'panic' && call_args.len == 1 {
		arg := call_args[0]
		arg_type := g.get_expr_type(arg)
		if arg_type == 'IError' || (arg is ast.Ident && arg.name == 'err') {
			g.sb.write_string('panic(IError__str(')
			g.gen_expr(arg)
			g.sb.write_string('))')
			return
		}
	}
	mut local_call_type := ''
	if lhs is ast.Ident {
		local_call_type = g.get_local_var_c_type(lhs.name) or { '' }
	}
	if local_call_type in ['void*', 'voidptr'] {
		mut arg_types := []string{cap: call_args.len}
		for arg in call_args {
			mut at := g.get_expr_type(arg)
			if at == '' || at == 'int_literal' || at == 'float_literal' {
				at = 'int'
			}
			if at == 'voidptr' {
				at = 'void*'
			}
			arg_types << at
		}
		ret_type := g.fn_pointer_return_type(lhs)
		c_ret := if ret_type == '' { 'void' } else { ret_type }
		g.sb.write_string('((${c_ret} (*)(')
		for i, at in arg_types {
			if i > 0 {
				g.sb.write_string(', ')
			}
			g.sb.write_string(at)
		}
		g.sb.write_string('))')
		g.gen_expr(lhs)
		g.sb.write_string(')(')
		for i, arg in call_args {
			if i > 0 {
				g.sb.write_string(', ')
			}
			g.gen_expr(arg)
		}
		g.sb.write_string(')')
		return
	}
	if g.gen_interface_method_dispatch(name, call_args) {
		return
	}
	if name in ['IError__type_name', 'IError__msg', 'IError__code'] && call_args.len > 0 {
		method_name := if name == 'IError__code' {
			'code'
		} else if name == 'IError__type_name' {
			'type_name'
		} else {
			'msg'
		}
		sep := if g.expr_is_pointer(call_args[0]) { '->' } else { '.' }
		g.gen_expr(call_args[0])
		g.sb.write_string('${sep}${method_name}(')
		if call_args.len > 1 {
			g.gen_expr(call_args[1])
		} else {
			g.gen_expr(call_args[0])
			g.sb.write_string('${sep}_object')
		}
		g.sb.write_string(')')
		return
	}
	if g.gen_array_contains_call(name, call_args) {
		return
	}
	if name == 'array__eq' && call_args.len == 2 {
		arg0_is_ptr := g.expr_is_pointer(call_args[0])
		arg1_is_ptr := g.expr_is_pointer(call_args[1])
		sep0 := if arg0_is_ptr { '->' } else { '.' }
		sep1 := if arg1_is_ptr { '->' } else { '.' }
		g.sb.write_string('(((')
		g.gen_expr(call_args[0])
		g.sb.write_string('${sep0}len == ')
		g.gen_expr(call_args[1])
		g.sb.write_string('${sep1}len) && (')
		g.gen_expr(call_args[0])
		g.sb.write_string('${sep0}element_size == ')
		g.gen_expr(call_args[1])
		g.sb.write_string('${sep1}element_size) && (memcmp(')
		g.gen_expr(call_args[0])
		g.sb.write_string('${sep0}data, ')
		g.gen_expr(call_args[1])
		g.sb.write_string('${sep1}data, (')
		g.gen_expr(call_args[0])
		g.sb.write_string('${sep0}len * ')
		g.gen_expr(call_args[0])
		g.sb.write_string('${sep0}element_size)) == 0)))')
		return
	}
	if name.starts_with('Array_') && name.ends_with('__sort') && call_args.len == 1 {
		g.sb.write_string('array__sort((array*)')
		if g.expr_is_pointer(call_args[0]) {
			g.gen_expr(call_args[0])
		} else {
			g.sb.write_string('&')
			g.gen_expr(call_args[0])
		}
		g.sb.write_string(', NULL)')
		return
	}
	if name == 'os__join_path' && call_args.len == 2 && g.get_expr_type(call_args[1]) == 'string' {
		g.sb.write_string('os__join_path_single(')
		g.gen_call_arg('os__join_path_single', 0, call_args[0])
		g.sb.write_string(', ')
		g.gen_call_arg('os__join_path_single', 1, call_args[1])
		g.sb.write_string(')')
		return
	}
	if name in ['array__pop', 'array__pop_left'] && call_args.len == 1 {
		elem_type := g.infer_array_elem_type_from_expr(call_args[0])
		if elem_type != '' && elem_type != 'int' {
			g.sb.write_string('(*(${elem_type}*)${name}(')
			g.gen_call_arg(name, 0, call_args[0])
			g.sb.write_string('))')
			return
		}
	}
	if name in ['array__first', 'array__last'] && call_args.len == 1 {
		arg_type := g.get_expr_type(call_args[0])
		if arg_type.starts_with('Array_fixed_') {
			if finfo := g.collected_fixed_array_types[arg_type] {
				g.sb.write_string('(*(${finfo.elem_type}*)${name}(')
				g.gen_expr(call_args[0])
				g.sb.write_string('))')
				return
			}
		} else if arg_type.starts_with('Array_') {
			elem_type := arg_type['Array_'.len..]
			g.sb.write_string('(*(${elem_type}*)${name}(')
			g.gen_expr(call_args[0])
			g.sb.write_string('))')
			return
		}
	}
	if name.ends_with('__set') && call_args.len == 2 {
		mut enum_name := name
		if idx := name.last_index('__') {
			enum_name = name[..idx]
		}
		g.sb.write_string('(')
		g.gen_expr(call_args[0])
		g.sb.write_string(' |= ')
		if call_args[1] is ast.SelectorExpr {
			sel := call_args[1] as ast.SelectorExpr
			if sel.lhs is ast.EmptyExpr {
				g.sb.write_string('${enum_name}__${sel.rhs.name}')
			} else {
				g.gen_expr(call_args[1])
			}
		} else {
			g.gen_expr(call_args[1])
		}
		g.sb.write_string(')')
		return
	}
	if name.ends_with('__clear') && call_args.len == 2 {
		mut enum_name := name
		if idx := name.last_index('__') {
			enum_name = name[..idx]
		}
		g.sb.write_string('(')
		g.gen_expr(call_args[0])
		g.sb.write_string(' &= ~(')
		if call_args[1] is ast.SelectorExpr {
			sel := call_args[1] as ast.SelectorExpr
			if sel.lhs is ast.EmptyExpr {
				g.sb.write_string('${enum_name}__${sel.rhs.name}')
			} else {
				g.gen_expr(call_args[1])
			}
		} else {
			g.gen_expr(call_args[1])
		}
		g.sb.write_string('))')
		return
	}
	if name.ends_with('__has') && call_args.len == 2 {
		g.sb.write_string('((((int)(')
		g.gen_expr(call_args[0])
		g.sb.write_string(')) & ((int)(')
		g.gen_expr(call_args[1])
		g.sb.write_string('))) != 0)')
		return
	}
	if name.ends_with('__all') && call_args.len == 2 {
		mut is_flag_all := g.is_flag_enum_expr(call_args[0])
		if !is_flag_all && name == 'array__all' {
			recv_type := g.get_expr_type(call_args[0])
			if recv_type != '' && recv_type != 'array' && !recv_type.starts_with('Array_') {
				is_flag_all = true
			}
		}
		if !is_flag_all {
			// regular array__all(path) with callback predicate
			// keep the normal call emission below
		} else {
			g.sb.write_string('((((int)(')
			g.gen_expr(call_args[0])
			g.sb.write_string(')) & ((int)(')
			g.gen_expr(call_args[1])
			g.sb.write_string('))) == ((int)(')
			g.gen_expr(call_args[1])
			g.sb.write_string(')))')
			return
		}
	}
	if name.starts_with('__Map_') && name.ends_with('_get_and_set') && call_args.len == 3 {
		key_type := g.get_expr_type(call_args[1])
		val_type := g.get_expr_type(call_args[2])
		g.sb.write_string('map__get_and_set(')
		if g.expr_is_pointer(call_args[0]) {
			g.sb.write_string('(map*)')
			g.gen_expr(call_args[0])
		} else {
			g.sb.write_string('(map*)&')
			g.gen_expr(call_args[0])
		}
		g.sb.write_string(', ')
		g.gen_addr_of_expr(call_args[1], if key_type == '' { 'int' } else { key_type })
		g.sb.write_string(', ')
		g.gen_addr_of_expr(call_args[2], if val_type == '' { 'array' } else { val_type })
		g.sb.write_string(')')
		return
	}
	if name.starts_with('__Map_') && name.ends_with('_set') && call_args.len == 3 {
		key_type := g.get_expr_type(call_args[1])
		val_type := g.get_expr_type(call_args[2])
		g.sb.write_string('map__set(')
		// The transformer generates &map_expr as first arg.
		// If the inner expr is already a pointer (mut param), strip the &.
		arg0 := call_args[0]
		if arg0 is ast.PrefixExpr && arg0.op == .amp {
			if g.expr_is_pointer(arg0.expr) {
				g.sb.write_string('(map*)')
				g.gen_expr(arg0.expr)
			} else {
				g.sb.write_string('(map*)&')
				g.gen_expr(arg0.expr)
			}
		} else if g.expr_is_pointer(arg0) {
			g.sb.write_string('(map*)')
			g.gen_expr(call_args[0])
		} else {
			g.sb.write_string('(map*)&')
			g.gen_expr(call_args[0])
		}
		g.sb.write_string(', ')
		g.gen_addr_of_expr(call_args[1], if key_type == '' { 'int' } else { key_type })
		g.sb.write_string(', ')
		g.gen_addr_of_expr(call_args[2], if val_type == '' { 'int' } else { val_type })
		g.sb.write_string(')')
		return
	}
	if name.starts_with('__Map_') && name.ends_with('_get') && call_args.len == 2 {
		// Extract the map type name from: __Map_X_Y_get -> Map_X_Y
		map_type_name := name['__'.len..name.len - '_get'.len]
		if info := g.ensure_map_type_info(map_type_name) {
			val_c := info.value_c_type
			key_c := info.key_c_type
			g.sb.write_string('(*(${val_c}*)map__get((map*)&')
			g.gen_expr(call_args[0])
			g.sb.write_string(', ')
			g.gen_addr_of_expr(call_args[1], key_c)
			g.sb.write_string(', ${map_get_default_ptr_for_type(val_c)}))')
		} else {
			// Fallback: try to infer types
			key_type := g.get_expr_type(call_args[1])
			g.sb.write_string('(*(int*)map__get((map*)&')
			g.gen_expr(call_args[0])
			g.sb.write_string(', ')
			g.gen_addr_of_expr(call_args[1], if key_type == '' { 'int' } else { key_type })
			g.sb.write_string(', &(int){0}))')
		}
		return
	}
	if name.starts_with('__Map_') && name.ends_with('_get_check') && call_args.len == 2 {
		// Inline pointer lookup to avoid relying on emitted helper wrappers.
		map_type_name := name['__'.len..name.len - '_get_check'.len]
		if info := g.ensure_map_type_info(map_type_name) {
			val_c := info.value_c_type
			key_c := info.key_c_type
			g.sb.write_string('(${val_c}*)map__get_check((map*)')
			if g.expr_is_pointer(call_args[0]) {
				g.gen_expr(call_args[0])
			} else {
				g.sb.write_string('&')
				g.gen_expr(call_args[0])
			}
			g.sb.write_string(', ')
			g.gen_addr_of_expr(call_args[1], key_c)
			g.sb.write_string(')')
		} else {
			key_type := g.get_expr_type(call_args[1])
			g.sb.write_string('(int*)map__get_check((map*)')
			if g.expr_is_pointer(call_args[0]) {
				g.gen_expr(call_args[0])
			} else {
				g.sb.write_string('&')
				g.gen_expr(call_args[0])
			}
			g.sb.write_string(', ')
			g.gen_addr_of_expr(call_args[1], if key_type == '' { 'int' } else { key_type })
			g.sb.write_string(')')
		}
		return
	}
	if name == 'voidptr__vbytes' {
		name = 'void__vbytes'
	}
	if name == 'signal' && call_args.len == 2 {
		g.sb.write_string('signal(')
		g.gen_expr(call_args[0])
		g.sb.write_string(', ((void (*)(int))')
		g.gen_expr(call_args[1])
		g.sb.write_string('))')
		return
	}

	// Handle builtin print functions with type-aware argument conversion
	if name in ['println', 'eprintln', 'print', 'eprint'] {
		if call_args.len == 1 {
			arg := call_args[0]
			arg_type := g.get_expr_type(arg)

			mut c_name := name
			builtin_name := 'builtin__${name}'
			if builtin_name in g.fn_param_is_ptr || builtin_name in g.fn_return_types {
				c_name = builtin_name
			} else if name in g.fn_param_is_ptr || name in g.fn_return_types {
				c_name = name
			}

			if arg_type == 'string' {
				g.sb.write_string('${c_name}(')
				g.gen_expr(arg)
				g.sb.write_string(')')
			} else if arg is ast.Ident && arg.name == 'err' {
				g.sb.write_string('${c_name}(IError__str(')
				g.gen_expr(arg)
				g.sb.write_string('))')
			} else if arg_type == 'IError' {
				g.sb.write_string('${c_name}(IError__str(')
				g.gen_expr(arg)
				g.sb.write_string('))')
			} else if arg_type in ['int', 'i8', 'i16', 'i32'] {
				g.sb.write_string('${c_name}(int__str(')
				g.gen_expr(arg)
				g.sb.write_string('))')
			} else if arg_type == 'i64' {
				g.sb.write_string('${c_name}(i64__str(')
				g.gen_expr(arg)
				g.sb.write_string('))')
			} else if arg_type == 'u64' {
				g.sb.write_string('${c_name}(u64__str(')
				g.gen_expr(arg)
				g.sb.write_string('))')
			} else if arg_type == 'bool' {
				g.sb.write_string('${c_name}(bool__str(')
				g.gen_expr(arg)
				g.sb.write_string('))')
			} else {
				// Fallback
				g.sb.write_string('${c_name}(/* ${arg_type} */ int__str(')
				g.gen_expr(arg)
				g.sb.write_string('))')
			}
			return
		}
	}

	// Regular function call - mangle name based on module
	mut c_name := name
	is_local_call := local_call_type != ''
	if is_c_runtime_function(name) {
		c_name = name
	} else if name != '' && g.cur_module != '' && g.cur_module != 'main'
		&& g.cur_module != 'builtin' && !name.contains('__') && !is_local_call {
		qualified := '${g.cur_module}__${name}'
		if qualified in g.fn_param_is_ptr || qualified in g.fn_return_types {
			c_name = qualified
		} else if name in g.fn_param_is_ptr || name in g.fn_return_types {
			c_name = name
		} else {
			c_name = qualified
		}
	}
	call_args = g.normalize_named_call_args(c_name, call_args)
	if c_name == 'array__push_many' && call_args.len == 3 && call_args[1] is ast.SelectorExpr
		&& call_args[2] is ast.SelectorExpr {
		data_sel := call_args[1] as ast.SelectorExpr
		len_sel := call_args[2] as ast.SelectorExpr
		if data_sel.rhs.name == 'data' && len_sel.rhs.name == 'len'
			&& g.contains_call_expr(data_sel.lhs) {
			tmp_type := if g.get_expr_type(data_sel.lhs) != '' {
				g.get_expr_type(data_sel.lhs)
			} else {
				'array'
			}
			tmp_name := '_push_many_tmp_${g.tmp_counter}'
			g.tmp_counter++
			g.sb.write_string('({ ${tmp_type} ${tmp_name} = ')
			g.gen_expr(data_sel.lhs)
			g.sb.write_string('; ${c_name}(')
			g.gen_call_arg(c_name, 0, call_args[0])
			g.sb.write_string(', ${tmp_name}.data, ${tmp_name}.len); })')
			return
		}
	}
	// Handle variadic calls: if more args than params and last param is an Array type,
	// wrap the extra args into an array literal.
	if param_types := g.fn_param_types[c_name] {
		if param_types.len > 0 && call_args.len > param_types.len {
			last_param_type := param_types[param_types.len - 1]
			if last_param_type.starts_with('Array_') {
				elem_type := last_param_type['Array_'.len..]
				g.sb.write_string('${c_name}(')
				// Emit non-variadic args
				for i in 0 .. param_types.len - 1 {
					if i > 0 {
						g.sb.write_string(', ')
					}
					if i < call_args.len {
						g.gen_call_arg(c_name, i, call_args[i])
					}
				}
				if param_types.len > 1 {
					g.sb.write_string(', ')
				}
				// Wrap remaining args in a C array → Array struct
				variadic_count := call_args.len - (param_types.len - 1)
				g.sb.write_string('new_array_from_c_array(${variadic_count}, ${variadic_count}, sizeof(${elem_type}), (${elem_type}[${variadic_count}]){')
				for i in param_types.len - 1 .. call_args.len {
					if i > param_types.len - 1 {
						g.sb.write_string(', ')
					}
					g.gen_expr(call_args[i])
				}
				g.sb.write_string('}))')
				return
			}
		}
	}
	g.sb.write_string('${c_name}(')
	mut total_args := call_args.len
	if param_types := g.fn_param_types[c_name] {
		if param_types.len > total_args {
			total_args = param_types.len
		}
		// Named call arguments are lowered as repeated FieldInit entries.
		// Use the declared parameter count so grouped fields map to one arg.
		if call_args_have_field_init(call_args) && param_types.len > 0 {
			total_args = param_types.len
		}
	} else if params := g.fn_param_is_ptr[c_name] {
		if params.len > total_args {
			total_args = params.len
		}
	}
	mut arg_idx := 0
	for i in 0 .. total_args {
		if i > 0 {
			g.sb.write_string(', ')
		}
		if arg_idx < call_args.len {
			next_idx := g.gen_named_struct_call_arg(c_name, i, call_args, arg_idx)
			if next_idx >= 0 {
				arg_idx = next_idx
				continue
			}
			if c_name == 'signal' && i == 1 {
				g.sb.write_string('((void (*)(int))')
				g.gen_expr(call_args[arg_idx])
				g.sb.write_string(')')
				arg_idx++
				continue
			}
			g.gen_call_arg(c_name, i, call_args[arg_idx])
			arg_idx++
		} else {
			// Default arguments should be lowered by transformer; keep C generation moving.
			if param_types := g.fn_param_types[c_name] {
				if i < param_types.len {
					g.sb.write_string(zero_value_for_type(param_types[i]))
				} else {
					g.sb.write_string('0')
				}
			} else {
				g.sb.write_string('0')
			}
		}
	}
	g.sb.write_string(')')
}

// types_type_to_c converts a types.Type to a C type string
fn (g &Gen) types_type_to_c(t types.Type) string {
	match t {
		types.Primitive {
			if t.props.has(.integer) {
				if t.props.has(.untyped) {
					return 'int'
				}
				size := if t.size == 0 { 32 } else { int(t.size) }
				is_signed := !t.props.has(.unsigned)
				return if is_signed {
					match size {
						8 { 'i8' }
						16 { 'i16' }
						32 { 'int' }
						64 { 'i64' }
						else { 'int' }
					}
				} else {
					match size {
						8 { 'u8' }
						16 { 'u16' }
						32 { 'u32' }
						else { 'u64' }
					}
				}
			} else if t.props.has(.float) {
				if t.props.has(.untyped) {
					return 'f64'
				}
				return if t.size == 32 { 'f32' } else { 'f64' }
			} else if t.props.has(.boolean) {
				return 'bool'
			}
			return 'int'
		}
		types.Pointer {
			base := g.types_type_to_c(t.base_type)
			return '${base}*'
		}
		types.Array {
			elem := g.types_type_to_c(t.elem_type)
			return 'Array_${mangle_alias_component(elem)}'
		}
		types.ArrayFixed {
			elem := g.types_type_to_c(t.elem_type)
			return 'Array_fixed_${mangle_alias_component(elem)}_${t.len}'
		}
		types.Struct {
			name := t.name
			// C struct types need the 'struct' keyword in C
			if !name.contains('__')
				&& name in ['tm', 'timespec', 'timeval', 'stat', 'dirent', 'sockaddr', 'sockaddr_in', 'sockaddr_in6', 'sockaddr_un', 'addrinfo', 'msghdr', 'iovec', 'pollfd', 'rusage', 'rlimit', 'sigaction', 'winsize', 'utsname'] {
				return 'struct ${name}'
			}
			return name
		}
		types.String {
			return 'string'
		}
		types.Alias {
			return t.name
		}
		types.Char {
			return 'char'
		}
		types.Rune {
			return 'rune'
		}
		types.Void {
			return 'void'
		}
		types.Enum {
			return t.name
		}
		types.Interface {
			return t.name
		}
		types.SumType {
			return types.sum_type_name(t)
		}
		types.Map {
			key := g.types_type_to_c(t.key_type)
			val := g.types_type_to_c(t.value_type)
			return 'Map_${mangle_alias_component(key)}_${mangle_alias_component(val)}'
		}
		types.OptionType {
			base := g.types_type_to_c(t.base_type)
			return '_option_${mangle_alias_component(base)}'
		}
		types.ResultType {
			base := g.types_type_to_c(t.base_type)
			return '_result_${mangle_alias_component(base)}'
		}
		types.FnType {
			return 'void*'
		}
		types.ISize {
			return 'isize'
		}
		types.USize {
			return 'usize'
		}
		types.Nil {
			return 'void*'
		}
		types.None {
			return 'void'
		}
		else {
			return 'int'
		}
	}
}

// get_expr_type_from_env retrieves the C type string for an expression from the Environment
fn (g &Gen) get_expr_type_from_env(e ast.Expr) ?string {
	if g.env == unsafe { nil } {
		return none
	}
	pos := e.pos()
	if pos != 0 {
		if typ := g.env.get_expr_type(pos) {
			// Self-hosting can leave malformed alias payloads in env expr cache.
			// Skip those entries and fall back to AST/scope-based inference.
			if typ is types.Alias {
				return none
			}
			return g.types_type_to_c(typ)
		}
	}
	return none
}

// receiver_type_to_scope_name converts a receiver type AST expression to
// the V-style name used by the checker for scope keys.
// This must match what the checker computes: receiver_type.base_type().name()
fn (g &Gen) receiver_type_to_scope_name(typ ast.Expr) string {
	// Strip receiver modifiers (e.g. `mut Type`, `shared Type`)
	if typ is ast.ModifierExpr {
		return g.receiver_type_to_scope_name(typ.expr)
	}
	// Strip pointer/reference prefix (e.g. &[]string -> []string)
	if typ is ast.PrefixExpr {
		if typ.op == .amp {
			return g.receiver_type_to_scope_name(typ.expr)
		}
	}
	if typ is ast.Type {
		// Array type: []T -> "[]T"
		if typ is ast.ArrayType {
			elem := g.receiver_type_to_scope_name(typ.elem_type)
			return '[]${elem}'
		}
		// Map type: map[K]V -> "map[K]V"
		if typ is ast.MapType {
			key := g.receiver_type_to_scope_name(typ.key_type)
			val := g.receiver_type_to_scope_name(typ.value_type)
			return 'map[${key}]${val}'
		}
	}
	// Ident: bare type name (e.g. "Builder", "string")
	if typ is ast.Ident {
		return typ.name
	}
	// Selector: module.Type -> just use Type
	if typ is ast.SelectorExpr {
		return typ.rhs.name
	}
	// Fallback
	return ''
}

fn (mut g Gen) ensure_cur_fn_scope() ?&types.Scope {
	if g.cur_fn_scope != unsafe { nil } {
		return g.cur_fn_scope
	}
	if g.env == unsafe { nil } || g.cur_fn_name == '' {
		return none
	}
	if fn_scope := g.env.get_fn_scope(g.cur_module, g.cur_fn_name) {
		g.cur_fn_scope = fn_scope
		return fn_scope
	}
	suffix := '__${g.cur_fn_name}'
	mut matched_key := ''
	fn_scope_keys := lock g.env.fn_scopes {
		g.env.fn_scopes.keys()
	}
	for key in fn_scope_keys {
		if key.ends_with(suffix) {
			matched_key = key
			break
		}
	}
	if matched_key != '' {
		if fn_scope := g.env_fn_scope_by_key(matched_key) {
			g.cur_fn_scope = fn_scope
			return fn_scope
		}
	}
	return none
}

fn (mut g Gen) remember_runtime_local_type(name string, typ string) {
	if name == '' || name == '_' || typ == '' {
		return
	}
	g.runtime_local_types[name] = typ
}

// get_local_var_c_type looks up a local variable's C type string from the function scope
fn (mut g Gen) get_local_var_c_type(name string) ?string {
	if local_typ := g.runtime_local_types[name] {
		return local_typ
	}
	if mut fn_scope := g.ensure_cur_fn_scope() {
		if obj := fn_scope.lookup_parent(name, 0) {
			if obj is types.Module || obj is types.Const || obj is types.Global || obj is types.Fn {
				return none
			}
			return g.types_type_to_c(obj.typ())
		}
	}
	return none
}

// get_expr_type returns the C type string for an expression
fn (mut g Gen) get_expr_type(node ast.Expr) string {
	// For identifiers, check function scope first
	if node is ast.Ident {
		if node.name == 'err' {
			return 'IError'
		}
		if local_type := g.get_local_var_c_type(node.name) {
			return local_type
		}
		// Prefer scope-backed type resolution for identifiers (type names, locals, aliases).
		if raw_type := g.get_raw_type(node) {
			// Self-host transitional env values can carry malformed alias payloads.
			if raw_type !is types.Alias {
				typ_name := g.types_type_to_c(raw_type)
				if typ_name != '' {
					return typ_name
				}
			}
		}
	}
	// Try environment lookup
	if t := g.get_expr_type_from_env(node) {
		match node {
			ast.CallExpr {
				if ret := g.get_call_return_type(node.lhs, node.args.len) {
					if ret != '' {
						return ret
					}
				}
			}
			ast.CallOrCastExpr {
				if ret := g.get_call_return_type(node.lhs, 1) {
					if ret != '' {
						return ret
					}
				}
			}
			ast.IndexExpr, ast.SelectorExpr {
				// Position-based types for selectors/indexes are frequently overwritten by
				// parent expressions; prefer structural inference in fallback logic below.
			}
			else {}
		}
		if node !is ast.IndexExpr && node !is ast.SelectorExpr {
			return t
		}
	}
	// Fallback inference
	match node {
		ast.BasicLiteral {
			if node.kind == .key_true || node.kind == .key_false {
				return 'bool'
			}
			return 'int'
		}
		ast.StringLiteral {
			return 'string'
		}
		ast.SelectorExpr {
			field_type := g.selector_field_type(node)
			if field_type != '' {
				return field_type
			}
			if raw_type := g.get_raw_type(node) {
				if raw_type !is types.Alias {
					resolved := g.types_type_to_c(raw_type)
					if resolved != '' {
						return resolved
					}
				}
			}
			return 'int'
		}
		ast.InfixExpr {
			if node.op in [.eq, .ne, .lt, .gt, .le, .ge, .and, .logical_or] {
				return 'bool'
			}
			lhs_t := g.get_expr_type(node.lhs)
			rhs_t := g.get_expr_type(node.rhs)
			if node.op in [.plus, .minus, .mul, .div, .mod, .amp, .pipe, .xor, .left_shift,
				.right_shift] {
				if lhs_t.ends_with('Fn') || rhs_t.ends_with('Fn') {
					return 'u64'
				}
			}
			return lhs_t
		}
		ast.ParenExpr {
			return g.get_expr_type(node.expr)
		}
		ast.PrefixExpr {
			if node.op == .mul {
				// Dereference: *(T*)(x) -> T
				inner_t := g.get_expr_type(node.expr)
				if inner_t.ends_with('*') {
					return inner_t[..inner_t.len - 1]
				}
				return inner_t
			}
			if node.op == .amp {
				return g.get_expr_type(node.expr) + '*'
			}
			return g.get_expr_type(node.expr)
		}
		ast.UnsafeExpr {
			// Infer from last statement in the block
			if node.stmts.len > 0 {
				last := node.stmts[node.stmts.len - 1]
				if last is ast.ExprStmt {
					return g.get_expr_type(last.expr)
				}
			}
			return 'int'
		}
		ast.IfExpr {
			return g.infer_if_expr_type(node)
		}
		ast.IndexExpr {
			// Slicing returns the same type as the source container.
			if node.expr is ast.RangeExpr {
				return g.get_expr_type(node.lhs)
			}
			if node.lhs is ast.SelectorExpr {
				elem_type := g.fixed_array_selector_elem_type(node.lhs)
				if elem_type != '' {
					return elem_type
				}
			}
			// Try to get element type from LHS type
			if raw_type := g.get_raw_type(node.lhs) {
				match raw_type {
					types.Array {
						return g.types_type_to_c(raw_type.elem_type)
					}
					types.ArrayFixed {
						return g.types_type_to_c(raw_type.elem_type)
					}
					types.Map {
						return g.types_type_to_c(raw_type.value_type)
					}
					types.Alias {
						// Avoid alias payload dereference in self-host fallback path.
					}
					types.Pointer {
						match raw_type.base_type {
							types.Pointer {
								return g.types_type_to_c(raw_type.base_type)
							}
							types.Array {
								return g.types_type_to_c(raw_type.base_type.elem_type)
							}
							types.ArrayFixed {
								return g.types_type_to_c(raw_type.base_type.elem_type)
							}
							types.Map {
								return g.types_type_to_c(raw_type.base_type.value_type)
							}
							types.Alias {
								// Avoid nested alias payload dereference in self-host fallback path.
							}
							types.String {
								return 'string'
							}
							else {}
						}
					}
					types.String {
						return 'u8'
					}
					else {}
				}
			}
			lhs_type := g.get_expr_type(node.lhs)
			if lhs_type.starts_with('Map_') {
				if info := g.ensure_map_type_info(lhs_type) {
					return info.value_c_type
				}
			}
			if lhs_type.starts_with('Array_fixed_') {
				if info := g.collected_fixed_array_types[lhs_type] {
					return info.elem_type
				}
			} else {
				elem := array_alias_elem_type(lhs_type)
				if elem != '' {
					return elem
				}
			}
			return 'int'
		}
		ast.MapInitExpr {
			if node.typ !is ast.EmptyExpr {
				return g.expr_type_to_c(node.typ)
			}
			if raw_type := g.get_raw_type(node) {
				if raw_type is types.Map {
					return g.types_type_to_c(raw_type)
				}
			}
			if node.keys.len > 0 && node.vals.len > 0 {
				key := mangle_alias_component(g.get_expr_type(node.keys[0]))
				val := mangle_alias_component(g.get_expr_type(node.vals[0]))
				return 'Map_${key}_${val}'
			}
			return 'map'
		}
		ast.InitExpr {
			return g.expr_type_to_c(node.typ)
		}
		ast.ArrayInitExpr {
			elem := g.extract_array_elem_type(node.typ)
			if elem != '' {
				if g.is_dynamic_array_type(node.typ) {
					return 'Array_${elem}'
				}
				fixed_name := 'Array_fixed_${mangle_alias_component(elem)}_${node.exprs.len}'
				g.register_alias_type(fixed_name)
				g.collected_fixed_array_types[fixed_name] = FixedArrayInfo{
					elem_type: elem
					size:      node.exprs.len
				}
				return fixed_name
			}
			return 'array'
		}
		ast.CallExpr {
			if node.lhs is ast.Ident
				&& node.lhs.name in ['array__pop', 'array__pop_left', 'array__first', 'array__last']
				&& node.args.len > 0 {
				elem_type := g.infer_array_elem_type_from_expr(node.args[0])
				if elem_type != '' {
					return elem_type
				}
			} else if node.lhs is ast.SelectorExpr
				&& node.lhs.rhs.name in ['pop', 'pop_left', 'first', 'last'] {
				arr_expr := if node.args.len > 0 { node.args[0] } else { node.lhs.lhs }
				elem_type := g.infer_array_elem_type_from_expr(arr_expr)
				if elem_type != '' {
					return elem_type
				}
			}
			if ret := g.get_call_return_type(node.lhs, node.args.len) {
				return ret
			}
			return 'int'
		}
		ast.CallOrCastExpr {
			if node.lhs is ast.Ident && g.is_type_name(node.lhs.name) {
				return g.expr_type_to_c(node.lhs)
			}
			if node.lhs is ast.Ident {
				// Try with module-qualified name
				qualified := g.expr_type_to_c(node.lhs)
				if g.is_type_name(qualified) {
					return qualified
				}
			}
			if node.lhs is ast.Type {
				return g.expr_type_to_c(node.lhs)
			}
			if node.lhs is ast.SelectorExpr {
				// module.Type(expr) - check if this is a type cast
				qualified := g.expr_type_to_c(node.lhs)
				if g.is_type_name(qualified) {
					return qualified
				}
			}
			if node.lhs is ast.Ident
				&& node.lhs.name in ['array__pop', 'array__pop_left', 'array__first', 'array__last'] {
				elem_type := g.infer_array_elem_type_from_expr(node.expr)
				if elem_type != '' {
					return elem_type
				}
			}
			if node.lhs is ast.SelectorExpr
				&& node.lhs.rhs.name in ['pop', 'pop_left', 'first', 'last'] {
				arr_expr := if node.expr is ast.EmptyExpr { node.lhs.lhs } else { node.expr }
				elem_type := g.infer_array_elem_type_from_expr(arr_expr)
				if elem_type != '' {
					return elem_type
				}
			}
			if ret := g.get_call_return_type(node.lhs, 1) {
				return ret
			}
			return 'int'
		}
		ast.CastExpr {
			return g.expr_type_to_c(node.typ)
		}
		ast.AsCastExpr {
			return g.expr_type_to_c(node.typ)
		}
		ast.StringInterLiteral {
			return 'string'
		}
		ast.Tuple {
			mut elem_types := []string{cap: node.exprs.len}
			for expr in node.exprs {
				elem_types << g.get_expr_type(expr)
			}
			return g.register_tuple_alias(elem_types)
		}
		else {
			return 'int'
		}
	}
}

// expr_type_to_c converts an AST type expression to a C type string
fn (mut g Gen) expr_type_to_c(e ast.Expr) string {
	match e {
		ast.Ident {
			name := e.name
			if name in ['int', 'i64', 'i32', 'i16', 'i8', 'u64', 'u32', 'u16', 'u8', 'byte', 'rune',
				'f32', 'f64', 'usize', 'isize'] {
				return name
			}
			if name == 'bool' {
				return 'bool'
			}
			if name == 'string' {
				return 'string'
			}
			if name == 'voidptr' {
				return 'void*'
			}
			if name == 'charptr' {
				return 'char*'
			}
			if name == 'byteptr' {
				return 'u8*'
			}
			// Detect mangled pointer types from transformer (e.g. FILEptr -> FILE*)
			if name.len > 3 && name.ends_with('ptr') {
				base := name[..name.len - 3]
				if base in ['FILE', 'DIR', 'va_list', 'pthread_t', 'pthread_mutex_t',
					'pthread_cond_t', 'pthread_rwlock_t', 'pthread_attr_t'] {
					return '${base}*'
				}
			}
			if g.is_module_local_type(name) {
				return '${g.cur_module}__${name}'
			}
			g.register_alias_type(name)
			return name
		}
		ast.PrefixExpr {
			if e.op == .amp {
				return g.expr_type_to_c(e.expr) + '*'
			}
			if e.op == .ellipsis {
				elem_type := mangle_alias_component(g.expr_type_to_c(e.expr))
				array_type := 'Array_${elem_type}'
				g.register_alias_type(array_type)
				return array_type
			}
			return 'void*'
		}
		ast.SelectorExpr {
			if e.lhs is ast.Ident {
				// C interop types: C.FILE -> FILE, C.tm -> struct tm
				if e.lhs.name == 'C' {
					name := e.rhs.name
					// Known C typedefs don't need 'struct' prefix
					if name in ['FILE', 'DIR', 'va_list', 'pthread_t', 'pthread_mutex_t',
						'pthread_cond_t', 'pthread_rwlock_t', 'pthread_attr_t', 'jmp_buf',
						'sigjmp_buf', 'sigset_t', 'size_t', 'ssize_t', 'off_t', 'mode_t', 'pid_t',
						'uid_t', 'gid_t', 'time_t', 'clock_t', 'socklen_t', 'dev_t', 'ino_t',
						'nlink_t', 'blksize_t', 'blkcnt_t', 'cc_t', 'speed_t', 'tcflag_t',
						'mach_timebase_info_data_t'] {
						return name
					}
					return 'struct ${name}'
				}
				return '${e.lhs.name}__${e.rhs.name}'
			}
			return g.expr_type_to_c(e.lhs) + '__${e.rhs.name}'
		}
		ast.EmptyExpr {
			return 'void'
		}
		ast.Type {
			if e is ast.ArrayType {
				elem_type := mangle_alias_component(g.expr_type_to_c(e.elem_type))
				array_type := 'Array_${elem_type}'
				g.register_alias_type(array_type)
				return array_type
			}
			if e is ast.ArrayFixedType {
				elem_type := mangle_alias_component(g.expr_type_to_c(e.elem_type))
				size_str := expr_to_int_str(e.len)
				fixed_type := 'Array_fixed_${elem_type}_${size_str}'
				g.register_alias_type(fixed_type)
				g.collected_fixed_array_types[fixed_type] = FixedArrayInfo{
					elem_type: g.expr_type_to_c(e.elem_type)
					size:      size_str.int()
				}
				return fixed_type
			}
			if e is ast.TupleType {
				mut elem_types := []string{cap: e.types.len}
				for t in e.types {
					elem_types << g.expr_type_to_c(t)
				}
				return g.register_tuple_alias(elem_types)
			}
			if e is ast.MapType {
				key_c := g.expr_type_to_c(e.key_type)
				value_c := g.expr_type_to_c(e.value_type)
				key_type := mangle_alias_component(key_c)
				value_type := mangle_alias_component(value_c)
				map_type := 'Map_${key_type}_${value_type}'
				g.register_alias_type(map_type)
				g.collected_map_types[map_type] = MapTypeInfo{
					key_c_type:   key_c
					value_c_type: value_c
				}
				return map_type
			}
			if e is ast.OptionType {
				base_type := mangle_alias_component(g.expr_type_to_c(e.base_type))
				option_type := '_option_${base_type}'
				g.register_alias_type(option_type)
				return option_type
			}
			if e is ast.ResultType {
				base_type := mangle_alias_component(g.expr_type_to_c(e.base_type))
				result_type := '_result_${base_type}'
				g.register_alias_type(result_type)
				return result_type
			}
			if e is ast.FnType {
				return 'void*'
			}
			if e is ast.NilType {
				return 'void*'
			}
			if e is ast.NoneType {
				return 'None__'
			}
			return 'int'
		}
		ast.ModifierExpr {
			// Handle shared/mut modifiers: unwrap and use the inner type
			return g.expr_type_to_c(e.expr)
		}
		else {
			return 'int'
		}
	}
}

// is_c_type_name checks if a name refers to a C type (struct, typedef) vs a C function.
fn (g &Gen) is_c_type_name(name string) bool {
	return name in ['FILE', 'DIR', 'va_list', 'pthread_t', 'pthread_mutex_t', 'pthread_cond_t',
		'pthread_rwlock_t', 'pthread_attr_t', 'stat', 'tm', 'timespec', 'timeval', 'dirent',
		'termios', 'sockaddr', 'sockaddr_in', 'sockaddr_in6', 'sockaddr_un',
		'mach_timebase_info_data_t']
}

fn (g &Gen) env_scope(module_name string) ?&types.Scope {
	if g.env == unsafe { nil } {
		return none
	}
	return g.env.get_scope(module_name)
}

fn (g &Gen) env_fn_scope_by_key(key string) ?&types.Scope {
	if g.env == unsafe { nil } {
		return none
	}
	return g.env.get_fn_scope_by_key(key)
}

fn (g &Gen) lookup_module_scope_object(name string) ?types.Object {
	if g.env == unsafe { nil } {
		return none
	}
	if g.cur_module == '' {
		return none
	}
	if mut module_scope := g.env_scope(g.cur_module) {
		if obj := module_scope.lookup(name) {
			return obj
		}
	}
	return none
}

fn (g &Gen) is_module_local_type(name string) bool {
	if g.cur_module == '' || g.cur_module == 'main' || g.cur_module == 'builtin' {
		return false
	}
	if name in primitive_types {
		return false
	}
	if name in ['bool', 'string', 'voidptr', 'charptr', 'byteptr'] {
		return false
	}
	if name.contains('__') || name.starts_with('Array_') || name.starts_with('Array_fixed_')
		|| name.starts_with('Map_') || name.starts_with('_result_') || name.starts_with('_option_') {
		return false
	}
	if obj := g.lookup_module_scope_object(name) {
		return obj is types.Type
	}
	return false
}

fn (g &Gen) is_module_local_const_or_global(name string) bool {
	if g.cur_module == '' || g.cur_module == 'main' || g.cur_module == 'builtin' {
		return false
	}
	if obj := g.lookup_module_scope_object(name) {
		return obj is types.Const || obj is types.Global
	}
	return false
}

fn (g &Gen) is_module_local_fn(name string) bool {
	if g.cur_module == '' || g.cur_module == 'main' || g.cur_module == 'builtin' {
		return false
	}
	if obj := g.lookup_module_scope_object(name) {
		return obj is types.Fn
	}
	sanitized := sanitize_fn_ident(name)
	if sanitized != name {
		if obj := g.lookup_module_scope_object(sanitized) {
			return obj is types.Fn
		}
	}
	return false
}

// get_raw_type returns the raw types.Type for an expression from the Environment
fn (mut g Gen) get_raw_type(node ast.Expr) ?types.Type {
	if g.env == unsafe { nil } {
		return none
	}
	// For identifiers, check function scope first
	if node is ast.Ident {
		if mut fn_scope := g.ensure_cur_fn_scope() {
			if obj := fn_scope.lookup_parent(node.name, 0) {
				if obj is types.Module {
					return none
				}
				return obj.typ()
			}
		}
		// Fallback to module scopes when function scope chain misses the symbol.
		if g.cur_module != '' {
			if mut mod_scope := g.env_scope(g.cur_module) {
				if obj := mod_scope.lookup_parent(node.name, 0) {
					if obj !is types.Module {
						return obj.typ()
					}
				}
			}
		}
		if g.cur_module != 'builtin' {
			if mut builtin_scope := g.env_scope('builtin') {
				if obj := builtin_scope.lookup_parent(node.name, 0) {
					if obj !is types.Module {
						return obj.typ()
					}
				}
			}
		}
		// Fallback: try runtime_local_types to resolve for-loop vars and other locals
		// not found in the scope chain (scope may not include transformer-generated vars).
		if local_type := g.runtime_local_types[node.name] {
			resolved := g.resolve_c_type_to_raw(local_type)
			if resolved != none {
				return resolved
			}
		}
	}
	// For SelectorExpr, resolve through struct field lookup (more reliable than position)
	if node is ast.SelectorExpr {
		// Module selector (e.g. os.args): resolve from module scope first.
		if node.lhs is ast.Ident {
			if mut fn_scope := g.ensure_cur_fn_scope() {
				if obj := fn_scope.lookup_parent(node.lhs.name, 0) {
					if obj is types.Module {
						if rhs_obj := obj.lookup(node.rhs.name) {
							if rhs_obj !is types.Module {
								return rhs_obj.typ()
							}
						}
					}
				}
			}
			if g.cur_module != '' {
				if mut mod_scope := g.env_scope(g.cur_module) {
					if obj := mod_scope.lookup_parent(node.lhs.name, 0) {
						if obj is types.Module {
							if rhs_obj := obj.lookup(node.rhs.name) {
								if rhs_obj !is types.Module {
									return rhs_obj.typ()
								}
							}
						}
					}
				}
			}
		}
		if lhs_type := g.get_raw_type(node.lhs) {
			if field_type := selector_struct_field_type_from_type(lhs_type, node.rhs.name) {
				return field_type
			}
		}
	}
	// For IndexExpr, resolve through the LHS container type
	if node is ast.IndexExpr {
		if node.expr is ast.RangeExpr {
			return g.get_raw_type(node.lhs)
		}
		if lhs_type := g.get_raw_type(node.lhs) {
			if lhs_type is types.Array {
				return lhs_type.elem_type
			}
			if lhs_type is types.ArrayFixed {
				return lhs_type.elem_type
			}
			if lhs_type is types.Map {
				return lhs_type.value_type
			}
		}
	}
	// Try environment lookup by position
	pos := node.pos()
	if pos != 0 {
		return g.env.get_expr_type(pos)
	}
	return none
}

// resolve_c_type_to_raw converts a C type name (e.g. "types__Deferred") back to a types.Type
// by looking up the type name in the appropriate module scope.
fn (mut g Gen) resolve_c_type_to_raw(c_type string) ?types.Type {
	if g.env == unsafe { nil } {
		return none
	}
	mut type_name := c_type.trim_right('*')
	if type_name.starts_with('Map_') {
		if info := g.ensure_map_type_info(type_name) {
			key_type := g.resolve_c_type_to_raw(info.key_c_type) or { return none }
			value_type := g.resolve_c_type_to_raw(info.value_c_type) or { return none }
			return types.Map{
				key_type:   key_type
				value_type: value_type
			}
		}
	}
	mut mod_name := g.cur_module
	if type_name.contains('__') {
		mod_name = type_name.all_before_last('__')
		type_name = type_name.all_after_last('__')
	}
	if scope := g.env_scope(mod_name) {
		if obj := scope.objects[type_name] {
			return obj.typ()
		}
	}
	if mod_name != 'builtin' {
		if scope := g.env_scope('builtin') {
			if obj := scope.objects[type_name] {
				return obj.typ()
			}
		}
	}
	return none
}

fn selector_struct_field_type_from_type(t types.Type, field_name string) ?types.Type {
	match t {
		types.Struct {
			for field in t.fields {
				if field.name == field_name {
					return field.typ
				}
			}
			return none
		}
		types.Pointer {
			return selector_struct_field_type_from_type(t.base_type, field_name)
		}
		types.Alias {
			return selector_struct_field_type_from_type(t.base_type, field_name)
		}
		else {
			return none
		}
	}
}

fn (mut g Gen) is_type_reference_expr(node ast.Expr) bool {
	match node {
		ast.Ident {
			if g.get_local_var_c_type(node.name) != none {
				return false
			}
			return g.is_type_name(node.name)
		}
		ast.SelectorExpr {
			if node.lhs is ast.Ident {
				if g.is_module_ident(node.lhs.name) {
					return true
				}
				return g.is_type_name(node.lhs.name)
			}
		}
		else {}
	}
	return false
}

fn (g &Gen) is_sum_payload_expr(node ast.Expr, variant string) bool {
	match node {
		ast.SelectorExpr {
			return node.rhs.name == variant || node.rhs.name == '_${variant}'
		}
		ast.CallOrCastExpr {
			return g.is_sum_payload_expr(node.expr, variant)
		}
		ast.CastExpr {
			return g.is_sum_payload_expr(node.expr, variant)
		}
		ast.ParenExpr {
			return g.is_sum_payload_expr(node.expr, variant)
		}
		ast.PrefixExpr {
			return g.is_sum_payload_expr(node.expr, variant)
		}
		ast.ModifierExpr {
			return g.is_sum_payload_expr(node.expr, variant)
		}
		else {}
	}
	return false
}

fn (g &Gen) contains_as_cast_expr(node ast.Expr) bool {
	match node {
		ast.AsCastExpr {
			return true
		}
		ast.CastExpr {
			return g.contains_as_cast_expr(node.expr)
		}
		ast.ParenExpr {
			return g.contains_as_cast_expr(node.expr)
		}
		ast.ModifierExpr {
			return g.contains_as_cast_expr(node.expr)
		}
		ast.PrefixExpr {
			return g.contains_as_cast_expr(node.expr)
		}
		else {}
	}
	return false
}

fn (mut g Gen) expr_cast_target_type(node ast.Expr) string {
	match node {
		ast.CallOrCastExpr {
			if node.lhs is ast.Ident && g.is_type_name(node.lhs.name) {
				return g.expr_type_to_c(node.lhs)
			}
			if node.lhs is ast.Type {
				return g.expr_type_to_c(node.lhs)
			}
			if node.lhs is ast.SelectorExpr {
				sel := node.lhs as ast.SelectorExpr
				if sel.lhs is ast.Ident && sel.lhs.name == 'C' && g.is_c_type_name(sel.rhs.name) {
					return g.expr_type_to_c(node.lhs)
				}
			}
			return g.expr_cast_target_type(node.expr)
		}
		ast.CastExpr {
			return g.expr_type_to_c(node.typ)
		}
		ast.ParenExpr {
			return g.expr_cast_target_type(node.expr)
		}
		ast.ModifierExpr {
			return g.expr_cast_target_type(node.expr)
		}
		ast.PrefixExpr {
			return g.expr_cast_target_type(node.expr)
		}
		else {}
	}
	return ''
}

fn (mut g Gen) gen_sum_narrowed_selector(node ast.SelectorExpr) bool {
	// Internal sum fields (`_tag`, `_data`, `_Variant`) must never go through
	// smartcast field lowering; they are already the raw representation.
	if node.rhs.name.starts_with('_') {
		return false
	}
	if node.lhs !is ast.Ident {
		return false
	}
	lhs_ident := node.lhs as ast.Ident
	decl_type := g.get_local_var_c_type(lhs_ident.name) or { return false }
	variants := g.sum_type_variants[decl_type] or { return false }
	narrowed := g.get_expr_type_from_env(node.lhs) or { return false }
	if narrowed == '' || narrowed == decl_type {
		return false
	}
	narrowed_short := if narrowed.contains('__') { narrowed.all_after_last('__') } else { narrowed }
	mut variant_field := ''
	for v in variants {
		v_short := if v.contains('__') { v.all_after_last('__') } else { v }
		if v == narrowed || v_short == narrowed_short || narrowed.ends_with('__${v_short}') {
			variant_field = v_short
			break
		}
	}
	if variant_field == '' {
		return false
	}
	field_name := escape_c_keyword(node.rhs.name)
	owner := g.embedded_owner_for(narrowed, node.rhs.name)
	g.sb.write_string('(((${narrowed}*)(((')
	g.gen_expr(node.lhs)
	g.sb.write_string(')._data._${variant_field})))')
	if owner != '' {
		g.sb.write_string('->${escape_c_keyword(owner)}.${field_name}')
	} else {
		g.sb.write_string('->${field_name}')
	}
	g.sb.write_string(')')
	return true
}

fn (mut g Gen) gen_sum_variant_field_selector(node ast.SelectorExpr) bool {
	if node.rhs.name.starts_with('_') {
		return false
	}
	// If LHS is an as-cast, the cast already handles narrowing - skip double unwrap
	if node.lhs is ast.AsCastExpr {
		return false
	}
	mut lhs_sum_type := ''
	if raw_lhs := g.get_raw_type(node.lhs) {
		match raw_lhs {
			types.SumType {
				lhs_sum_type = g.types_type_to_c(raw_lhs)
			}
			types.Pointer {
				if raw_lhs.base_type is types.SumType {
					lhs_sum_type = g.types_type_to_c(raw_lhs.base_type)
				}
			}
			types.Alias {
				if raw_lhs.base_type is types.SumType {
					lhs_sum_type = g.types_type_to_c(raw_lhs.base_type)
				}
			}
			else {}
		}
	}
	if lhs_sum_type == '' {
		lhs_sum_type = g.get_expr_type(node.lhs)
	}
	mut variants := []string{}
	if vs := g.sum_type_variants[lhs_sum_type] {
		variants = vs.clone()
	} else if lhs_sum_type.contains('__') {
		short_sum := lhs_sum_type.all_after_last('__')
		if vs := g.sum_type_variants[short_sum] {
			variants = vs.clone()
		}
	} else {
		qualified_sum := g.get_qualified_name(lhs_sum_type)
		if vs := g.sum_type_variants[qualified_sum] {
			variants = vs.clone()
			lhs_sum_type = qualified_sum
		}
	}
	if variants.len == 0 {
		return false
	}
	mut matched_full := ''
	mut matched_short := ''
	for variant in variants {
		variant_short := if variant.contains('__') { variant.all_after_last('__') } else { variant }
		mut variant_full := variant
		if !variant_full.contains('__') && lhs_sum_type.contains('__') {
			prefix := lhs_sum_type.all_before_last('__')
			if prefix != '' {
				variant_full = '${prefix}__${variant_short}'
			}
		}
		key_full := '${variant_full}.${node.rhs.name}'
		key_short := '${variant_short}.${node.rhs.name}'
		if key_full in g.struct_field_types || key_short in g.struct_field_types {
			if matched_full != '' && matched_full != variant_full {
				// Ambiguous field across multiple variants.
				return false
			}
			matched_full = variant_full
			matched_short = variant_short
		}
	}
	if matched_full == '' {
		return false
	}
	field_name := escape_c_keyword(node.rhs.name)
	owner := g.embedded_owner_for(matched_full, node.rhs.name)
	sep := if g.expr_is_pointer(node.lhs) { '->' } else { '.' }
	g.sb.write_string('(((${matched_full}*)(((')
	g.gen_expr(node.lhs)
	g.sb.write_string(')${sep}_data._${matched_short}))')
	if owner != '' {
		g.sb.write_string('->${escape_c_keyword(owner)}.${field_name}')
	} else {
		g.sb.write_string('->${field_name}')
	}
	g.sb.write_string(')')
	return true
}

fn (mut g Gen) gen_unsafe_expr(node ast.UnsafeExpr) {
	if node.stmts.len == 0 {
		g.sb.write_string('0')
		return
	}
	if node.stmts.len == 1 {
		stmt := node.stmts[0]
		if stmt is ast.ExprStmt {
			g.gen_expr(stmt.expr)
		} else {
			// Single non-expression statement (e.g., return) - emit directly
			g.gen_stmt(stmt)
		}
		return
	}
	// Multi-statement: use GCC compound expression ({ ... })
	g.sb.write_string('({ ')
	for i, stmt in node.stmts {
		if i < node.stmts.len - 1 {
			g.gen_stmt(stmt)
		}
	}
	// Last statement - if it's an ExprStmt, its value is the block's value
	last := node.stmts[node.stmts.len - 1]
	if last is ast.ExprStmt {
		g.gen_expr(last.expr)
		g.sb.write_string('; ')
	} else {
		g.gen_stmt(last)
		g.sb.write_string('0; ')
	}
	g.sb.write_string('})')
}

fn (mut g Gen) selector_field_type(sel ast.SelectorExpr) string {
	lhs_type := g.get_expr_type(sel.lhs)
	if lhs_type.starts_with('_result_') {
		if sel.rhs.name == 'is_error' {
			return 'bool'
		}
		if sel.rhs.name == 'err' {
			return 'IError'
		}
		if sel.rhs.name == 'data' {
			base := g.result_value_type(lhs_type)
			if base != '' && base != 'void' {
				return base
			}
		}
	}
	if lhs_type.starts_with('_option_') {
		if sel.rhs.name == 'state' {
			return 'u8'
		}
		if sel.rhs.name == 'err' {
			return 'IError'
		}
		if sel.rhs.name == 'data' {
			base := option_value_type(lhs_type)
			if base != '' && base != 'void' {
				return base
			}
		}
	}

	mut struct_name := ''
	if raw_type := g.get_raw_type(sel.lhs) {
		match raw_type {
			types.Pointer {
				if raw_type.base_type is types.Struct {
					struct_name = raw_type.base_type.name
				} else if raw_type.base_type is types.Alias {
					struct_name = raw_type.base_type.name
				}
			}
			types.Struct {
				struct_name = raw_type.name
			}
			types.Alias {
				struct_name = raw_type.name
			}
			else {}
		}
	}
	if struct_name == '' {
		struct_name = g.get_expr_type(sel.lhs).trim_right('*')
	}
	if struct_name != '' {
		field_key := struct_name + '.' + sel.rhs.name
		if field_type := g.struct_field_types[field_key] {
			return field_type
		}
	}
	if struct_name.contains('__') {
		short_name := struct_name.all_after_last('__')
		short_field_key := short_name + '.' + sel.rhs.name
		if field_type := g.struct_field_types[short_field_key] {
			return field_type
		}
	}
	if struct_name.contains('.') {
		mangled_name := struct_name.replace('.', '__')
		mangled_field_key := mangled_name + '.' + sel.rhs.name
		if field_type := g.struct_field_types[mangled_field_key] {
			return field_type
		}
		short_dot := struct_name.all_after_last('.')
		short_dot_field_key := short_dot + '.' + sel.rhs.name
		if field_type := g.struct_field_types[short_dot_field_key] {
			return field_type
		}
	}
	return ''
}

fn (mut g Gen) selector_struct_name(expr ast.Expr) string {
	if raw_type := g.get_raw_type(expr) {
		match raw_type {
			types.Pointer {
				if raw_type.base_type is types.Struct {
					return raw_type.base_type.name
				}
				if raw_type.base_type is types.Alias {
					return raw_type.base_type.name
				}
			}
			types.Struct {
				return raw_type.name
			}
			types.Alias {
				return raw_type.name
			}
			else {}
		}
	}
	return g.get_expr_type(expr).trim_right('*')
}

fn (g &Gen) embedded_owner_for(struct_name string, field_name string) string {
	if struct_name == '' {
		return ''
	}
	key := '${struct_name}.${field_name}'
	if owner := g.embedded_field_owner[key] {
		return owner
	}
	if struct_name.contains('__') {
		short_key := '${struct_name.all_after_last('__')}.${field_name}'
		if owner := g.embedded_field_owner[short_key] {
			return owner
		}
	}
	return ''
}

fn (mut g Gen) fixed_array_selector_elem_type(sel ast.SelectorExpr) string {
	mut struct_name := ''
	if raw_type := g.get_raw_type(sel.lhs) {
		match raw_type {
			types.Pointer {
				if raw_type.base_type is types.Struct {
					struct_name = raw_type.base_type.name
				} else if raw_type.base_type is types.Alias {
					struct_name = raw_type.base_type.name
				}
			}
			types.Struct {
				struct_name = raw_type.name
			}
			types.Alias {
				struct_name = raw_type.name
			}
			else {}
		}
	}
	if struct_name == '' {
		struct_name = g.get_expr_type(sel.lhs).trim_right('*')
	}
	if struct_name != '' {
		field_key := '${struct_name}.${sel.rhs.name}'
		if elem := g.fixed_array_field_elem[field_key] {
			return elem
		}
	}
	if struct_name.contains('__') {
		short_name := struct_name.all_after_last('__')
		short_field_key := '${short_name}.${sel.rhs.name}'
		if elem := g.fixed_array_field_elem[short_field_key] {
			return elem
		}
	}
	return ''
}

fn (mut g Gen) is_fixed_array_selector(sel ast.SelectorExpr) bool {
	if g.fixed_array_selector_elem_type(sel) != '' {
		return true
	}
	return false
}

fn (mut g Gen) gen_index_expr(node ast.IndexExpr) {
	// Slice syntax: arr[a..b], arr[..b], arr[a..], s[a..b]
	if node.expr is ast.RangeExpr {
		panic('bug in v2 compiler: slice IndexExpr should have been lowered in v2.transformer')
	}
	if node.lhs is ast.Ident {
		if node.lhs.name in g.fixed_array_globals || node.lhs.name == 'rune_maps' {
			g.gen_expr(node.lhs)
			g.sb.write_string('[')
			g.gen_expr(node.expr)
			g.sb.write_string(']')
			return
		}
	}
	// Fixed-size array struct fields are emitted as plain C arrays.
	if node.lhs is ast.SelectorExpr && g.is_fixed_array_selector(node.lhs) {
		g.gen_expr(node.lhs)
		g.sb.write_string('[')
		g.gen_expr(node.expr)
		g.sb.write_string(']')
		return
	}
	// Check LHS type from environment to determine indexing strategy
	if raw_type := g.get_raw_type(node.lhs) {
		if raw_type is types.ArrayFixed {
			// Fixed arrays are C arrays - direct indexing
			g.gen_expr(node.lhs)
			g.sb.write_string('[')
			g.gen_expr(node.expr)
			g.sb.write_string(']')
			return
		}
		if raw_type is types.Array {
			// Dynamic arrays: ((elem_type*)arr.data)[idx]
			elem_type := g.types_type_to_c(raw_type.elem_type)
			g.sb.write_string('((${elem_type}*)')
			g.gen_expr(node.lhs)
			g.sb.write_string('.data)[')
			g.gen_expr(node.expr)
			g.sb.write_string(']')
			return
		}
		if raw_type is types.Alias {
			match raw_type.base_type {
				types.Array {
					elem_type := g.types_type_to_c(raw_type.base_type.elem_type)
					g.sb.write_string('((${elem_type}*)')
					g.gen_expr(node.lhs)
					g.sb.write_string('.data)[')
					g.gen_expr(node.expr)
					g.sb.write_string(']')
					return
				}
				types.ArrayFixed {
					g.gen_expr(node.lhs)
					g.sb.write_string('[')
					g.gen_expr(node.expr)
					g.sb.write_string(']')
					return
				}
				types.String {
					g.gen_expr(node.lhs)
					g.sb.write_string('.str[')
					g.gen_expr(node.expr)
					g.sb.write_string(']')
					return
				}
				else {}
			}
		}
		if raw_type is types.Map {
			key_type := g.types_type_to_c(raw_type.key_type)
			val_type := g.types_type_to_c(raw_type.value_type)
			g.sb.write_string('(*(${val_type}*)map__get(&')
			g.gen_expr(node.lhs)
			g.sb.write_string(', ')
			g.gen_addr_of_expr(node.expr, key_type)
			g.sb.write_string(', ${map_get_default_ptr_for_type(val_type)}))')
			return
		}
		if raw_type is types.String {
			if node.lhs is ast.SelectorExpr && g.is_fixed_array_selector(node.lhs) {
				g.gen_expr(node.lhs)
				g.sb.write_string('[')
				g.gen_expr(node.expr)
				g.sb.write_string(']')
				return
			}
			// Distinguish true string indexing (u8 result) from array-like indexing.
			if out_type := g.get_raw_type(node) {
				out_name := g.types_type_to_c(out_type)
				if out_name !in ['u8', 'byte', 'char'] {
					g.gen_expr(node.lhs)
					g.sb.write_string('[')
					g.gen_expr(node.expr)
					g.sb.write_string(']')
					return
				}
			}
			g.gen_expr(node.lhs)
			g.sb.write_string('.str[')
			g.gen_expr(node.expr)
			g.sb.write_string(']')
			return
		}
		if raw_type is types.Pointer {
			// Pointer to array: use -> accessor
			if raw_type.base_type is types.Array {
				elem_type := g.types_type_to_c(raw_type.base_type.elem_type)
				g.sb.write_string('((${elem_type}*)')
				g.gen_expr(node.lhs)
				g.sb.write_string('->data)[')
				g.gen_expr(node.expr)
				g.sb.write_string(']')
				return
			} else if raw_type.base_type is types.Map {
				key_type := g.types_type_to_c(raw_type.base_type.key_type)
				val_type := g.types_type_to_c(raw_type.base_type.value_type)
				g.sb.write_string('(*(${val_type}*)map__get(')
				g.gen_expr(node.lhs)
				g.sb.write_string(', ')
				g.gen_addr_of_expr(node.expr, key_type)
				g.sb.write_string(', ${map_get_default_ptr_for_type(val_type)}))')
				return
			}
		}
	}
	if lhs_raw_type := g.get_raw_type(node.lhs) {
		if lhs_raw_type is types.ArrayFixed {
			// Fixed arrays are C arrays: direct indexing
			g.gen_expr(node.lhs)
			g.sb.write_string('[')
			g.gen_expr(node.expr)
			g.sb.write_string(']')
			return
		}
	}
	lhs_type := g.get_expr_type(node.lhs)
	if lhs_type == 'map' || lhs_type.starts_with('Map_') {
		mut key_type := g.get_expr_type(node.expr)
		mut val_type := g.get_expr_type(node)
		if lhs_type.starts_with('Map_') {
			if info := g.ensure_map_type_info(lhs_type) {
				if key_type == '' || key_type == 'int' {
					key_type = info.key_c_type
				}
				if val_type == '' || val_type == 'int' {
					val_type = info.value_c_type
				}
			}
		}
		if key_type == '' {
			key_type = 'int'
		}
		if val_type == '' || val_type == 'int' {
			val_type = 'int'
		}
		g.sb.write_string('(*(${val_type}*)map__get(')
		if lhs_type.ends_with('*') {
			g.gen_expr(node.lhs)
		} else {
			g.sb.write_string('&')
			g.gen_expr(node.lhs)
		}
		g.sb.write_string(', ')
		g.gen_addr_of_expr(node.expr, key_type)
		g.sb.write_string(', ${map_get_default_ptr_for_type(val_type)}))')
		return
	}
	if lhs_type == 'string' {
		g.gen_expr(node.lhs)
		g.sb.write_string('.str[')
		g.gen_expr(node.expr)
		g.sb.write_string(']')
		return
	}
	if lhs_type == 'string*' {
		elem_type := g.get_expr_type(node)
		if elem_type in ['u8', 'byte', 'char'] {
			g.gen_expr(node.lhs)
			g.sb.write_string('->str[')
			g.gen_expr(node.expr)
			g.sb.write_string(']')
		} else {
			g.gen_expr(node.lhs)
			g.sb.write_string('[')
			g.gen_expr(node.expr)
			g.sb.write_string(']')
		}
		return
	}
	if lhs_type.trim_right('*') in ['strings__Builder', 'Builder'] {
		g.sb.write_string('((u8*)')
		g.gen_expr(node.lhs)
		if lhs_type.ends_with('*') {
			g.sb.write_string('->data)[')
		} else {
			g.sb.write_string('.data)[')
		}
		g.gen_expr(node.expr)
		g.sb.write_string(']')
		return
	}
	// Fixed arrays (including pointer-to-fixed for mut params): direct C array indexing
	if lhs_type.trim_right('*').starts_with('Array_fixed_') {
		if lhs_type.ends_with('*') {
			g.sb.write_string('(*')
			g.gen_expr(node.lhs)
			g.sb.write_string(')[')
		} else {
			g.gen_expr(node.lhs)
			g.sb.write_string('[')
		}
		g.gen_expr(node.expr)
		g.sb.write_string(']')
		return
	}
	if lhs_type == 'array' || lhs_type.starts_with('Array_') {
		mut elem_type := g.get_expr_type(node)
		if elem_type == '' || elem_type == 'int' {
			if lhs_type.starts_with('Array_fixed_') {
				if finfo := g.collected_fixed_array_types[lhs_type] {
					elem_type = finfo.elem_type
				}
			} else if lhs_type.starts_with('Array_') {
				elem_type = lhs_type['Array_'.len..].trim_right('*')
			} else if lhs_type == 'array' {
				// Unparameterized array (e.g. from array__slice) - try to get element
				// type from the first argument (the source array) of the call.
				if node.lhs is ast.CallExpr {
					if node.lhs.args.len > 0 {
						src_type := g.get_expr_type(node.lhs.args[0])
						if src_type.starts_with('Array_') {
							elem_type = src_type['Array_'.len..].trim_right('*')
						}
					}
				}
			}
		}
		if elem_type == '' {
			elem_type = 'u8'
		}
		g.sb.write_string('((${elem_type}*)')
		g.gen_expr(node.lhs)
		if lhs_type.ends_with('*') {
			g.sb.write_string('->data)[')
		} else {
			g.sb.write_string('.data)[')
		}
		g.gen_expr(node.expr)
		g.sb.write_string(']')
		return
	}
	// Fallback: direct C array indexing
	g.gen_expr(node.lhs)
	g.sb.write_string('[')
	g.gen_expr(node.expr)
	g.sb.write_string(']')
}

fn (mut g Gen) gen_comptime_expr(node ast.ComptimeExpr) {
	if node.expr is ast.Ident {
		name := node.expr.name
		match name {
			'FN', 'METHOD', 'FUNCTION' {
				fn_name := g.cur_fn_name
				g.sb.write_string('(string){"${fn_name}", ${fn_name.len}}')
			}
			'MOD' {
				mod_name := g.cur_module
				g.sb.write_string('(string){"${mod_name}", ${mod_name.len}}')
			}
			'FILE' {
				g.sb.write_string('(string){__FILE__, sizeof(__FILE__)-1}')
			}
			'LINE' {
				g.sb.write_string('__LINE__')
			}
			'VCURRENTHASH' {
				g.sb.write_string('(string){"VCURRENTHASH", 12}')
			}
			'VEXE' {
				g.sb.write_string('__vexe_path()')
			}
			else {
				g.sb.write_string('(string){"", 0} /* unknown comptime: ${name} */')
			}
		}
		return
	}
	// Fallback: emit the inner expression
	g.gen_expr(node.expr)
}

fn (mut g Gen) gen_init_expr(node ast.InitExpr) {
	type_name := g.expr_type_to_c(node.typ)
	if node.fields.len == 0 {
		// Map types need proper initialization with new_map()
		if type_name.starts_with('Map_') || type_name == 'map' {
			key_c_type, val_c_type := g.map_expr_key_val_types(node.typ)
			g.gen_new_map_call(key_c_type, val_c_type)
			return
		}
		g.sb.write_string('((${type_name}){0})')
		return
	}
	g.sb.write_string('((${type_name}){')
	for i, field in node.fields {
		if i > 0 {
			g.sb.write_string(',')
		}
		if field.name == '' {
			g.gen_expr(field.value)
			continue
		}
		if type_name in g.sum_type_variants && field.name.starts_with('_data._') {
			field_name := field.name
			g.sb.write_string('.${field_name} = ')
			variant_name := field_name.all_after('_data._')
			mut inner_type := g.get_expr_type(field.value)
			mut resolved_type := inner_type
			if inner_type == '' || inner_type == 'void*' || inner_type == 'int'
				|| inner_type == type_name {
				if variant_name.contains('__') {
					resolved_type = variant_name
				} else if type_name.contains('__') {
					resolved_type = '${type_name.all_before_last('__')}__${variant_name}'
				} else {
					resolved_type = variant_name
				}
			}
			if g.is_scalar_sum_payload_type(resolved_type) {
				// Keep scalar payloads encoded in pointer-size space.
				g.sb.write_string('((void*)((intptr_t)(')
				g.gen_expr(field.value)
				g.sb.write_string(')))')
			} else if inner_type == 'void*' {
				// `&fn_call()` can point to a temporary; copy by-value from inner expr.
				if inner_expr := g.unwrap_addr_of_value_expr(field.value) {
					g.tmp_counter++
					tmp_name := '_st${g.tmp_counter}'
					g.sb.write_string('((void*)({ ${resolved_type} ${tmp_name} = ')
					g.gen_expr(inner_expr)
					g.sb.write_string('; memdup(&${tmp_name}, sizeof(${resolved_type})); }))')
				} else {
					g.sb.write_string('((void*)memdup(')
					g.gen_expr(field.value)
					g.sb.write_string(', sizeof(${resolved_type})))')
				}
			} else {
				g.tmp_counter++
				tmp_name := '_st${g.tmp_counter}'
				g.sb.write_string('((void*)({ ${resolved_type} ${tmp_name} = ')
				g.gen_expr(field.value)
				g.sb.write_string('; memdup(&${tmp_name}, sizeof(${resolved_type})); }))')
			}
			continue
		}
		owner := g.embedded_owner_for(type_name, field.name)
		field_name := escape_c_keyword(field.name)
		if owner != '' {
			g.sb.write_string('.${escape_c_keyword(owner)}.${field_name} = ')
		} else {
			g.sb.write_string('.${field_name} = ')
		}
		// Disambiguate shorthand enum values in struct field initializers
		// using the field's declared enum type.
		if field.value is ast.SelectorExpr {
			sel := field.value as ast.SelectorExpr
			if sel.lhs is ast.EmptyExpr {
				expected_enum_key := '${type_name}.${field.name}'
				mut expected_enum := g.struct_field_types[expected_enum_key] or { '' }
				if expected_enum == '' && type_name.contains('__') {
					short_type := type_name.all_after_last('__')
					short_expected_enum_key := '${short_type}.${field.name}'
					expected_enum = g.struct_field_types[short_expected_enum_key] or { '' }
				}
				if expected_enum != '' && g.is_enum_type(expected_enum) {
					g.sb.write_string('${g.normalize_enum_name(expected_enum)}__${sel.rhs.name}')
					continue
				}
			}
		}
		if g.should_deref_init_field_value(type_name, field.name, field.value) {
			g.sb.write_string('(*(')
			g.gen_expr(field.value)
			g.sb.write_string('))')
			continue
		}
		g.gen_expr(field.value)
	}
	g.sb.write_string('})')
}

fn (mut g Gen) gen_array_init_expr(node ast.ArrayInitExpr) {
	elem_type := unmangle_c_ptr_type(g.extract_array_elem_type(node.typ))
	if node.exprs.len > 0 {
		// Has elements
		if elem_type != '' && g.is_dynamic_array_type(node.typ) {
			// Dynamic array compound literal: (elem_type[N]){e1, e2, ...}
			g.sb.write_string('(${elem_type}[${node.exprs.len}]){')
			for i, e in node.exprs {
				if i > 0 {
					g.sb.write_string(', ')
				}
				g.gen_expr(e)
			}
			g.sb.write_string('}')
			return
		}
		// Fixed-size array or untyped: {e1, e2, ...}
		g.sb.write_string('{')
		for i, e in node.exprs {
			if i > 0 {
				g.sb.write_string(', ')
			}
			g.gen_expr(e)
		}
		g.sb.write_string('}')
		return
	}
	// Empty array: should have been lowered by transformer to __new_array_with_default_noscan()
	// Fallback: zero-init
	g.sb.write_string('(array){0}')
}

// extract_array_elem_type extracts the element C type from an array type expression
fn (mut g Gen) extract_array_elem_type(e ast.Expr) string {
	match e {
		ast.Type {
			if e is ast.ArrayType {
				return g.expr_type_to_c(e.elem_type)
			}
			if e is ast.ArrayFixedType {
				return g.expr_type_to_c(e.elem_type)
			}
		}
		else {}
	}
	return ''
}

// is_dynamic_array_type checks if the type expression is a dynamic array (ArrayType, not ArrayFixedType)
fn (g &Gen) is_dynamic_array_type(e ast.Expr) bool {
	match e {
		ast.Type {
			if e is ast.ArrayType {
				return true
			}
		}
		else {}
	}
	return false
}

fn strip_expr_wrappers(expr ast.Expr) ast.Expr {
	return match expr {
		ast.ParenExpr {
			strip_expr_wrappers(expr.expr)
		}
		ast.ModifierExpr {
			strip_expr_wrappers(expr.expr)
		}
		ast.CastExpr {
			strip_expr_wrappers(expr.expr)
		}
		else {
			expr
		}
	}
}

fn extract_array_init_arg(expr ast.Expr) ?ast.ArrayInitExpr {
	unwrapped := strip_expr_wrappers(expr)
	match unwrapped {
		ast.ArrayInitExpr {
			return unwrapped
		}
		ast.PrefixExpr {
			if unwrapped.op == .amp {
				return extract_array_init_arg(unwrapped.expr)
			}
		}
		else {}
	}
	return none
}

fn (mut g Gen) try_emit_const_dynamic_array_call(name string, value ast.Expr) bool {
	base_value := strip_expr_wrappers(value)
	if base_value !is ast.CallExpr {
		return false
	}
	call := base_value as ast.CallExpr
	if call.lhs !is ast.Ident {
		return false
	}
	fn_name := (call.lhs as ast.Ident).name
	if fn_name !in ['builtin__new_array_from_c_array_noscan', 'builtin__new_array_from_c_array',
		'new_array_from_c_array'] {
		return false
	}
	if call.args.len < 4 {
		return false
	}
	array_data := extract_array_init_arg(call.args[3]) or { return false }
	mut elem_type := g.extract_array_elem_type(array_data.typ)
	if elem_type == '' && array_data.exprs.len > 0 {
		elem_type = g.get_expr_type(array_data.exprs[0])
	}
	if elem_type == '' || elem_type == 'array' {
		return false
	}
	mut len_expr := expr_to_int_str(call.args[0])
	mut cap_expr := expr_to_int_str(call.args[1])
	if len_expr == '0' && array_data.exprs.len > 0 {
		len_expr = '${array_data.exprs.len}'
	}
	if cap_expr == '0' {
		cap_expr = len_expr
	}
	data_name := '__const_array_data_${name}'
	if array_data.exprs.len > 0 {
		g.sb.write_string('static ${elem_type} ${data_name}[${array_data.exprs.len}] = {')
		for i, e in array_data.exprs {
			if i > 0 {
				g.sb.write_string(', ')
			}
			g.gen_expr(e)
		}
		g.sb.writeln('};')
		g.sb.writeln('array ${name} = ((array){ .data = ${data_name}, .offset = 0, .len = ${len_expr}, .cap = ${cap_expr}, .flags = 0, .element_size = sizeof(${elem_type}) });')
	} else {
		g.sb.writeln('array ${name} = ((array){ .data = NULL, .offset = 0, .len = ${len_expr}, .cap = ${cap_expr}, .flags = 0, .element_size = sizeof(${elem_type}) });')
	}
	return true
}

fn (mut g Gen) gen_const_decl(node ast.ConstDecl) {
	for field in node.fields {
		name := if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
			'${g.cur_module}__${field.name}'
		} else {
			field.name
		}
		const_key := 'const_${name}'
		if const_key in g.emitted_types {
			continue
		}
		g.emitted_types[const_key] = true
		mut is_fixed_array_const := false
		mut fixed_array_elem := ''
		mut fixed_array_len := 0
		if raw_type := g.get_raw_type(field.value) {
			if raw_type is types.ArrayFixed {
				is_fixed_array_const = true
				fixed_array_elem = g.types_type_to_c(raw_type.elem_type)
				fixed_array_len = raw_type.len
			}
		}
		if !is_fixed_array_const && field.value is ast.ArrayInitExpr {
			array_value := field.value as ast.ArrayInitExpr
			mut elem_type := g.extract_array_elem_type(array_value.typ)
			if elem_type == '' && array_value.exprs.len > 0 {
				elem_type = g.get_expr_type(array_value.exprs[0])
			}
			if elem_type != '' && elem_type != 'array' {
				is_fixed_array_const = true
				fixed_array_elem = elem_type
				fixed_array_len = array_value.exprs.len
			}
		}
		if is_fixed_array_const && fixed_array_elem != '' {
			g.fixed_array_globals[name] = true
			if fixed_array_len > 0 {
				g.sb.write_string('static const ${fixed_array_elem} ${name}[${fixed_array_len}] = ')
			} else {
				g.sb.write_string('static const ${fixed_array_elem} ${name}[] = ')
			}
			g.gen_expr(field.value)
			g.sb.writeln(';')
			continue
		}
		if g.try_emit_const_dynamic_array_call(name, field.value) {
			continue
		}
		typ := g.get_expr_type(field.value)
		// Function calls are not compile-time constants in C; emit as zero-initialized globals.
		if g.contains_call_expr(field.value) {
			if typ in ['bool', 'char', 'rune', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32',
				'u64', 'usize', 'isize', 'f32', 'f64'] {
				g.sb.writeln('${typ} ${name} = 0;')
			} else {
				g.sb.writeln('${typ} ${name} = {0};')
			}
			continue
		}
		if typ == 'string' {
			// String constants need a global variable
			g.sb.write_string('string ${name} = ')
			g.gen_expr(field.value)
			g.sb.writeln(';')
		} else if typ in ['bool', 'char', 'rune', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16',
			'u32', 'u64', 'usize', 'isize', 'f32', 'f64', 'float_literal', 'int_literal'] {
			// Qualified const names are safe as macros and work in C constant-expression
			// contexts (array sizes, static initializers). Keep unqualified names as
			// typed globals to avoid macro collisions with local identifiers.
			if name.contains('__') {
				g.sb.write_string('#define ${name} ')
				g.gen_expr(field.value)
				g.sb.writeln('')
			} else {
				g.sb.write_string('static const ${typ} ${name} = ')
				g.gen_expr(field.value)
				g.sb.writeln(';')
			}
		} else {
			// Fallback for aggregate literals and other complex consts.
			g.sb.write_string('#define ${name} ')
			g.gen_expr(field.value)
			g.sb.writeln('')
		}
	}
}

fn (mut g Gen) gen_cast_expr(node ast.CastExpr) {
	type_name := g.expr_type_to_c(node.typ)
	if type_name.starts_with('_option_') {
		value_type := option_value_type(type_name)
		if value_type != '' && value_type != 'void' {
			g.sb.write_string('({ ${type_name} _opt = (${type_name}){ .state = 2 }; ${value_type} _val = ')
			g.gen_expr(node.expr)
			g.sb.write_string('; _option_ok(&_val, (_option*)&_opt, sizeof(_val)); _opt; })')
			return
		}
	}
	if type_name.starts_with('_result_') {
		value_type := g.result_value_type(type_name)
		if value_type != '' && value_type != 'void' {
			g.sb.write_string('({ ${type_name} _res = (${type_name}){0}; ${value_type} _val = ')
			g.gen_expr(node.expr)
			g.sb.write_string('; _result_ok(&_val, (_result*)&_res, sizeof(_val)); _res; })')
			return
		}
	}
	if type_name == 'void' {
		// Preserve side effects when discarding expression results.
		// e.g. `(void)(f())` must still call `f()`.
		mut inner := strings.new_builder(64)
		saved := g.sb
		g.sb = inner
		g.gen_expr(node.expr)
		inner_str := g.sb.str()
		g.sb = saved
		if inner_str == '' {
			g.sb.write_string('((void)0)')
		} else {
			g.sb.write_string('((void)(${inner_str}))')
		}
		return
	}
	expr_type := g.get_expr_type(node.expr)
	if expr_type.starts_with('_result_') && g.result_value_type(expr_type) != '' {
		g.sb.write_string('((${type_name})(')
		g.gen_unwrapped_value_expr(node.expr)
		g.sb.write_string('))')
		return
	}
	if expr_type.starts_with('_option_') && option_value_type(expr_type) != '' {
		g.sb.write_string('((${type_name})(')
		g.gen_unwrapped_value_expr(node.expr)
		g.sb.write_string('))')
		return
	}
	// Capture inner expression to check if it's empty
	mut inner := strings.new_builder(64)
	saved := g.sb
	g.sb = inner
	g.gen_expr(node.expr)
	inner_str := g.sb.str()
	g.sb = saved
	if inner_str == '' {
		// Empty inner expression (e.g. unresolved C function call) - emit no-op
		g.sb.write_string('((void)0)')
	} else {
		g.sb.write_string('((${type_name})(${inner_str}))')
	}
}

fn (mut g Gen) gen_keyword_operator(node ast.KeywordOperator) {
	match node.op {
		.key_sizeof {
			if node.exprs.len > 0 {
				g.sb.write_string('sizeof(')
				g.sb.write_string(g.expr_type_to_c(node.exprs[0]))
				g.sb.write_string(')')
			} else {
				g.sb.write_string('0')
			}
		}
		.key_typeof {
			if node.exprs.len > 0 {
				type_name := g.expr_type_to_c(node.exprs[0])
				g.sb.write_string('(string){"${type_name}", ${type_name.len}}')
			} else {
				g.sb.write_string('(string){"", 0}')
			}
		}
		.key_offsetof {
			if node.exprs.len >= 2 {
				g.sb.write_string('offsetof(')
				g.sb.write_string(g.expr_type_to_c(node.exprs[0]))
				g.sb.write_string(', ')
				field_expr := node.exprs[1]
				if field_expr is ast.Ident {
					g.sb.write_string(field_expr.name)
				} else {
					g.gen_expr(field_expr)
				}
				g.sb.write_string(')')
			} else {
				g.sb.write_string('0')
			}
		}
		.key_isreftype {
			g.sb.write_string('0')
		}
		.key_likely {
			if node.exprs.len > 0 {
				g.sb.write_string('__builtin_expect((')
				g.gen_expr(node.exprs[0])
				g.sb.write_string('), 1)')
			} else {
				g.sb.write_string('1')
			}
		}
		.key_unlikely {
			if node.exprs.len > 0 {
				g.sb.write_string('__builtin_expect((')
				g.gen_expr(node.exprs[0])
				g.sb.write_string('), 0)')
			} else {
				g.sb.write_string('0')
			}
		}
		.key_dump {
			// dump(expr) - just evaluate the expression
			if node.exprs.len > 0 {
				g.gen_expr(node.exprs[0])
			} else {
				g.sb.write_string('0')
			}
		}
		else {
			g.sb.write_string('/* KeywordOperator: ${node.op} */ 0')
		}
	}
}

fn (mut g Gen) gen_as_cast_expr(node ast.AsCastExpr) {
	type_name := g.expr_type_to_c(node.typ)
	short_name := if type_name.contains('__') {
		type_name.all_after_last('__')
	} else {
		type_name
	}
	// Interface cast: `iface as T` => `*((T*)iface._object)`
	if raw_type := g.get_raw_type(node.expr) {
		is_iface := raw_type is types.Interface
			|| (raw_type is types.Pointer && raw_type.base_type is types.Interface)
		if is_iface {
			sep := if g.expr_is_pointer(node.expr) { '->' } else { '.' }
			g.sb.write_string('(*((${type_name}*)(')
			g.gen_expr(node.expr)
			g.sb.write_string('${sep}_object)))')
			return
		}
	}
	mut inner := strings.new_builder(64)
	saved := g.sb
	g.sb = inner
	g.gen_expr(node.expr)
	inner_str := g.sb.str()
	g.sb = saved
	if g.contains_as_cast_expr(node.expr) {
		g.sb.write_string('((${type_name})(${inner_str}))')
		return
	}
	marker := ')->_data._${short_name}'
	if idx := inner_str.index(marker) {
		simplified := inner_str[..idx + 1]
		g.sb.write_string('(*(${simplified}))')
		return
	}
	marker2 := ')->_data)._${short_name}'
	if idx := inner_str.index(marker2) {
		simplified := inner_str[..idx + 1]
		g.sb.write_string('(*(${simplified}))')
		return
	}
	if inner_str.starts_with('((${type_name}*)') || inner_str.starts_with('(${type_name}*)') {
		g.sb.write_string('(*(${inner_str}))')
		return
	}
	if inner_str.starts_with('(*(') || inner_str.starts_with('*(') {
		g.sb.write_string('((${type_name})(${inner_str}))')
		return
	}
	if g.is_sum_payload_expr(node.expr, short_name) {
		g.sb.write_string('(*((${type_name}*)(${inner_str})))')
		return
	}
	if inner_str.contains('->_data._${short_name}') || inner_str.contains('._data._${short_name}') {
		g.sb.write_string('(*((${type_name}*)(${inner_str})))')
		return
	}
	// Sum type cast: a as Cat => (*((main__Cat*)(((a))._data._Cat)))
	sep := if g.expr_is_pointer(node.expr) { '->' } else { '.' }
	g.sb.write_string('(*((${type_name}*)(((${inner_str})${sep}_data._${short_name}))))')
}

fn (mut g Gen) gen_string_inter_literal(node ast.StringInterLiteral) {
	// Use sprintf approach with asprintf (allocates automatically)
	// Wrapped in GCC compound expression ({ ... })
	// Build format string, stripping V string delimiters from values
	mut fmt_str := strings.new_builder(64)
	for i, raw_val in node.values {
		mut val := raw_val
		// Strip only the outer string delimiters. Using trim_* here can over-strip
		// escaped quotes in edge chunks like: 'A "${x}"', leaving a trailing `\`.
		if i == 0 && val.len > 0 && val[0] in [`'`, `"`] {
			val = val[1..]
		}
		if i == node.values.len - 1 && val.len > 0 && val[val.len - 1] in [`'`, `"`] {
			val = val[..val.len - 1]
		}
		// `node.values` chunks already preserve V escape markers (e.g. `\t`),
		// so doubling backslashes here would emit `\\t` in C and break literals.
		escaped := val.replace('"', '\\"').replace('\n', '\\n').replace('\t', '\\t')
		fmt_str.write_string(escaped)
		if i < node.inters.len {
			inter := node.inters[i]
			fmt_str.write_string(g.get_sprintf_format(inter))
		}
	}
	fmt := fmt_str.str()
	g.sb.write_string('({ char* _sip; int _sil = asprintf(&_sip, "${fmt}"')
	// Write arguments
	for inter in node.inters {
		g.sb.write_string(', ')
		g.write_sprintf_arg(inter)
	}
	g.sb.write_string('); (string){_sip, _sil}; })')
}

fn is_type_name_pointer_like(name string) bool {
	trimmed := name.trim_space()
	return trimmed.starts_with('&') || trimmed.ends_with('*') || trimmed.ends_with('ptr')
}

fn strip_pointer_type_name(name string) string {
	mut out := name.trim_space()
	if out.starts_with('&') {
		out = out[1..]
	}
	for out.ends_with('*') {
		out = out[..out.len - 1].trim_space()
	}
	return unmangle_c_ptr_type(out)
}

fn short_type_name(name string) string {
	if name.contains('__') {
		return name.all_after_last('__')
	}
	return name
}

fn (mut g Gen) expr_pointer_return_type(expr ast.Expr) string {
	unwrapped := strip_expr_wrappers(expr)
	match unwrapped {
		ast.CallExpr {
			if ret := g.get_call_return_type(unwrapped.lhs, unwrapped.args.len) {
				return ret
			}
		}
		ast.CallOrCastExpr {
			if ret := g.get_call_return_type(unwrapped.lhs, 1) {
				return ret
			}
		}
		else {}
	}
	return ''
}

fn (mut g Gen) should_deref_init_field_value(struct_type string, field_name string, value ast.Expr) bool {
	expected_key := '${struct_type}.${field_name}'
	mut expected := g.struct_field_types[expected_key] or { '' }
	if expected == '' && struct_type.contains('__') {
		short_struct := struct_type.all_after_last('__')
		short_expected_key := '${short_struct}.${field_name}'
		expected = g.struct_field_types[short_expected_key] or { '' }
	}
	if expected == '' || is_type_name_pointer_like(expected) {
		return false
	}
	mut value_type := g.get_expr_type(value)
	if !is_type_name_pointer_like(value_type) {
		call_ret_type := g.expr_pointer_return_type(value)
		if call_ret_type != '' {
			value_type = call_ret_type
		}
	}
	if !is_type_name_pointer_like(value_type) {
		return false
	}
	expected_base := strip_pointer_type_name(expected)
	value_base := strip_pointer_type_name(value_type)
	if expected_base == value_base {
		return true
	}
	return short_type_name(expected_base) == short_type_name(value_base)
}

fn (mut g Gen) write_sprintf_arg(inter ast.StringInter) {
	expr_type := g.get_expr_type(inter.expr)
	if expr_type == 'string' {
		g.gen_expr(inter.expr)
		g.sb.write_string('.str')
	} else if expr_type == 'bool' {
		g.sb.write_string('(')
		g.gen_expr(inter.expr)
		g.sb.write_string(' ? "true" : "false")')
	} else {
		g.gen_expr(inter.expr)
	}
}

fn (mut g Gen) get_sprintf_format(inter ast.StringInter) string {
	mut fmt := '%'
	// Width
	if inter.width > 0 {
		fmt += '${inter.width}'
	}
	// Precision
	if inter.precision > 0 {
		fmt += '.${inter.precision}'
	}
	// Format specifier
	if inter.format != .unformatted {
		match inter.format {
			.decimal { fmt += 'd' }
			.float { fmt += 'f' }
			.hex { fmt += 'x' }
			.octal { fmt += 'o' }
			.character { fmt += 'c' }
			.exponent { fmt += 'e' }
			.exponent_short { fmt += 'g' }
			.binary { fmt += 'd' } // binary not supported in printf, fallback to decimal
			.pointer_address { fmt += 'p' }
			.string { fmt += 's' }
			.unformatted { fmt += 'd' }
		}
		return fmt
	}
	// Infer from expression type
	expr_type := g.get_expr_type(inter.expr)
	match expr_type {
		'string' { return '%s' }
		'int', 'i8', 'i16', 'i32' { return '%d' }
		'i64' { return '%lld' }
		'u8', 'u16', 'u32' { return '%u' }
		'u64' { return '%llu' }
		'f32', 'f64', 'float_literal' { return '%f' }
		'bool' { return '%s' }
		'rune' { return '%c' }
		'char' { return '%c' }
		else { return '%d' }
	}
}

fn (mut g Gen) write_indent() {
	for _ in 0 .. g.indent {
		g.sb.write_string('\t')
	}
}
