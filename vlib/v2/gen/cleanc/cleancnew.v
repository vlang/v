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
	sb              strings.Builder
	indent          int
	cur_fn_scope    &types.Scope = unsafe { nil }
	cur_fn_name     string
	cur_fn_ret_type string
	cur_module      string
	emitted_types   map[string]bool
	fn_param_is_ptr map[string][]bool
	fn_param_types  map[string][]string
	fn_return_types map[string]string

	fixed_array_fields     map[string]bool
	fixed_array_field_elem map[string]string
	fixed_array_globals    map[string]bool
	tuple_aliases          map[string][]string
	struct_field_types     map[string]string
	enum_value_to_enum     map[string]string
	array_aliases          map[string]bool
	map_aliases            map[string]bool
	result_aliases         map[string]bool
	option_aliases         map[string]bool
	module_type_names      map[string]bool
	tmp_counter            int
}

struct StructDeclInfo {
	decl ast.StructDecl
	mod  string
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
	return result_type['_result_'.len..]
}

fn option_value_type(option_type string) string {
	if !option_type.starts_with('_option_') {
		return ''
	}
	return option_type['_option_'.len..]
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
				return sanitize_fn_ident(expr.lhs.name) == 'error'
			}
		}
		ast.CallOrCastExpr {
			if expr.lhs is ast.Ident && !g.is_type_name(expr.lhs.name) {
				return sanitize_fn_ident(expr.lhs.name) == 'error'
			}
		}
		else {}
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
		files:           files
		env:             unsafe { env }
		pref:            unsafe { p }
		sb:              strings.new_builder(4096)
		fn_param_is_ptr: map[string][]bool{}
		fn_param_types:  map[string][]string{}
		fn_return_types: map[string]string{}

		fixed_array_fields:     map[string]bool{}
		fixed_array_field_elem: map[string]string{}
		fixed_array_globals:    map[string]bool{}
		tuple_aliases:          map[string][]string{}
		struct_field_types:     map[string]string{}
		enum_value_to_enum:     map[string]string{}
		array_aliases:          map[string]bool{}
		map_aliases:            map[string]bool{}
		result_aliases:         map[string]bool{}
		option_aliases:         map[string]bool{}
		module_type_names:      map[string]bool{}
	}
}

pub fn (mut g Gen) gen() string {
	g.write_preamble()
	g.collect_module_type_names()
	g.collect_runtime_aliases()
	g.collect_fn_signatures()

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
	// Emit structs with only primitive fields first, then the rest
	// Repeat until all are emitted (simple topo sort)
	for _ in 0 .. all_structs.len {
		mut emitted_any := false
		for info in all_structs {
			g.cur_module = info.mod
			name := g.get_struct_name(info.decl)
			if 'body_${name}' in g.emitted_types {
				continue
			}
			// Check if all field types are already defined
			if g.struct_fields_resolved(info.decl) {
				g.gen_struct_decl(info.decl)
				emitted_any = true
			}
		}
		if !emitted_any {
			break
		}
	}
	// Emit any remaining structs (circular deps - just emit them)
	for info in all_structs {
		g.cur_module = info.mod
		g.gen_struct_decl(info.decl)
	}
	// Pass 3.25: Tuple aliases (multiple-return lowering support)
	g.emit_tuple_aliases()
	if g.tuple_aliases.len > 0 {
		g.sb.writeln('')
	}

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
				fn_name := g.get_fn_name(stmt)
				if fn_name == '' {
					continue
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
	g.emit_ierror_wrappers()

	// Pass 5: Everything else (function bodies, consts, globals, etc.)
	for file in g.files {
		g.gen_file(file)
	}

	return g.sb.str()
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
	// Minimal wyhash symbols used by builtin/map and hash modules.
	g.sb.writeln('static const u64 _wyp[4] = {0xa0761d6478bd642full, 0xe7037ed1a0b428dbull, 0x8ebc6af09c88c6e3ull, 0x589965cc75374cc3ull};')
	g.sb.writeln('static inline u64 wyhash(const void* key, u64 len, u64 seed, const u64* secret) { (void)key; (void)len; (void)seed; (void)secret; return 0; }')
	g.sb.writeln('static inline u64 wyhash64(u64 a, u64 b) { (void)a; (void)b; return 0; }')
	g.sb.writeln('')
	g.sb.writeln('')
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
					g.module_type_names['${g.cur_module}::${stmt.name}'] = true
					struct_name := g.get_struct_name(stmt)
					for field in stmt.fields {
						field_type := g.expr_type_to_c(field.typ)
						g.struct_field_types['${stmt.name}.${field.name}'] = field_type
						g.struct_field_types['${struct_name}.${field.name}'] = field_type
						if field.typ is ast.Type && field.typ is ast.ArrayFixedType {
							fixed_typ := field.typ as ast.ArrayFixedType
							elem_type := g.expr_type_to_c(fixed_typ.elem_type)
							g.fixed_array_fields['${stmt.name}.${field.name}'] = true
							g.fixed_array_fields['${struct_name}.${field.name}'] = true
							g.fixed_array_field_elem['${stmt.name}.${field.name}'] = elem_type
							g.fixed_array_field_elem['${struct_name}.${field.name}'] = elem_type
						}
					}
				}
				ast.EnumDecl {
					g.module_type_names['${g.cur_module}::${stmt.name}'] = true
					enum_name := g.get_enum_name(stmt)
					for field in stmt.fields {
						if field.name !in g.enum_value_to_enum {
							g.enum_value_to_enum[field.name] = enum_name
						}
					}
				}
				ast.TypeDecl {
					if stmt.language == .c {
						continue
					}
					g.module_type_names['${g.cur_module}::${stmt.name}'] = true
				}
				ast.InterfaceDecl {
					g.module_type_names['${g.cur_module}::${stmt.name}'] = true
				}
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
					if stmt.language == .c && stmt.stmts.len == 0 {
						continue
					}
					fn_name := g.get_fn_name(stmt)
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
				}
				else {}
			}
		}
	}
}

fn (mut g Gen) emit_ierror_wrappers() {
	if 'body_IError' !in g.emitted_types && 'body_builtin__IError' !in g.emitted_types {
		return
	}
	mut emitted_any := false
	if 'Error__msg' in g.fn_return_types && 'Error__code' in g.fn_return_types {
		g.sb.writeln('static string IError_Error_type_name_wrapper(void* _obj) {')
		g.sb.writeln('\t(void)_obj;')
		g.sb.writeln('\treturn (string){"Error", 5};')
		g.sb.writeln('}')
		g.sb.writeln('static string IError_Error_msg_wrapper(void* _obj) {')
		g.sb.writeln('\treturn Error__msg(*(Error*)_obj);')
		g.sb.writeln('}')
		g.sb.writeln('static int IError_Error_code_wrapper(void* _obj) {')
		g.sb.writeln('\treturn Error__code(*(Error*)_obj);')
		g.sb.writeln('}')
		emitted_any = true
	}
	if 'MessageError__msg' in g.fn_return_types && 'MessageError__code' in g.fn_return_types {
		g.sb.writeln('static string IError_MessageError_type_name_wrapper(void* _obj) {')
		g.sb.writeln('\t(void)_obj;')
		g.sb.writeln('\treturn (string){"MessageError", 12};')
		g.sb.writeln('}')
		g.sb.writeln('static string IError_MessageError_msg_wrapper(void* _obj) {')
		g.sb.writeln('\treturn MessageError__msg(*(MessageError*)_obj);')
		g.sb.writeln('}')
		g.sb.writeln('static int IError_MessageError_code_wrapper(void* _obj) {')
		g.sb.writeln('\treturn MessageError__code(*(MessageError*)_obj);')
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
			g.register_alias_type('Array_${elem}')
		}
		types.Map {
			g.collect_aliases_from_type(t.key_type)
			g.collect_aliases_from_type(t.value_type)
			key := mangle_alias_component(g.types_type_to_c(t.key_type))
			val := mangle_alias_component(g.types_type_to_c(t.value_type))
			g.register_alias_type('Map_${key}_${val}')
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
			g.collect_aliases_from_type(t.base_type)
			g.register_alias_type(t.name)
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
		g.result_aliases[name] = true
		return
	}
	if name.starts_with('_option_') {
		g.option_aliases[name] = true
	}
}

fn (mut g Gen) emit_runtime_aliases() {
	mut array_names := g.array_aliases.keys()
	array_names.sort()
	for name in array_names {
		g.sb.writeln('typedef array ${name};')
	}
	mut map_names := g.map_aliases.keys()
	map_names.sort()
	for name in map_names {
		g.sb.writeln('typedef map ${name};')
	}
	mut option_names := g.option_aliases.keys()
	option_names.sort()
	for name in option_names {
		g.sb.writeln('typedef _option ${name};')
	}
	mut result_names := g.result_aliases.keys()
	result_names.sort()
	for name in result_names {
		g.sb.writeln('typedef _result ${name};')
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
				if is_none_expr(expr) {
					g.sb.writeln('return (${g.cur_fn_ret_type}){ .state = 2 };')
					return
				}
				expr_type := g.get_expr_type(expr)
				if expr_type == g.cur_fn_ret_type {
					g.sb.write_string('return ')
					g.gen_expr(expr)
					g.sb.writeln(';')
					return
				}
				value_type := g.cur_fn_ret_type['_option_'.len..]
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
				g.gen_expr(node.exprs[0])
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
			g.sb.writeln('/* [TODO] LabelStmt: ${node.name} */')
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
			g.sb.write_string(' = ')
			g.gen_expr(field.value)
		}
		g.sb.writeln(';')
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
		scope_fn_name := if node.is_method && node.receiver.name != '' {
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
		} else if node.is_method && node.receiver.name != '' {
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

	// Generate function header
	g.gen_fn_head(node)
	g.sb.writeln(' {')
	g.indent++

	// Main function: initialize argc/argv
	if node.name == 'main' {
		g.write_indent()
		g.sb.writeln('(void)___argc; (void)___argv;')
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
		receiver_type := g.expr_type_to_c(node.receiver.typ)
		// Strip pointer suffix for method naming
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
		if tuple_type == 'int' {
			if rhs is ast.CallExpr {
				if ret := g.get_call_return_type(rhs.lhs, rhs.args.len) {
					tuple_type = ret
				}
			} else if rhs is ast.CallOrCastExpr {
				if ret := g.get_call_return_type(rhs.lhs, 1) {
					tuple_type = ret
				}
			} else if rhs is ast.CastExpr {
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
		lhs_typ := g.get_expr_type(lhs)
		if lhs_typ != '' && lhs_typ !in ['int_literal', 'float_literal'] {
			typ = lhs_typ
		}
		mut rhs_has_explicit_call_ret := false
		if rhs is ast.CallExpr {
			if ret := g.get_call_return_type(rhs.lhs, rhs.args.len) {
				if ret != '' && ret != 'int' {
					typ = ret
					rhs_has_explicit_call_ret = true
				}
			}
		} else if rhs is ast.CallOrCastExpr {
			if ret := g.get_call_return_type(rhs.lhs, 1) {
				if ret != '' && ret != 'int' {
					typ = ret
					rhs_has_explicit_call_ret = true
				}
			}
		}
		if rhs is ast.KeywordOperator && rhs.op in [.key_sizeof, .key_offsetof] {
			typ = 'usize'
		}
		if name != '' && g.cur_fn_scope != unsafe { nil } {
			if obj := g.cur_fn_scope.lookup_parent(name, 0) {
				if obj !is types.Module {
					scoped_type := g.types_type_to_c(obj.typ())
					if scoped_type != '' && scoped_type != 'int' && !rhs_has_explicit_call_ret
						&& (typ == '' || typ == 'int') {
						typ = scoped_type
					}
				}
			}
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
			if container_type.starts_with('_result_') || container_type.starts_with('_option_') {
				rhs_type = 'IError'
			}
		}
		if (typ == '' || typ == 'int' || typ == 'int_literal') && rhs_type != ''
			&& rhs_type !in ['int', 'int_literal', 'float_literal']
			&& !rhs_type.starts_with('_result_') && !rhs_type.starts_with('_option_') {
			typ = rhs_type
		}
		if name != '' && rhs_type.starts_with('_result_') && !typ.starts_with('_result_') {
			g.sb.write_string('${typ} ${name} = ({ ${rhs_type} _tmp = ')
			g.gen_expr(rhs)
			g.sb.writeln('; (*(${typ}*)(((u8*)(&_tmp.err)) + sizeof(IError))); });')
			return
		}
		if name != '' && rhs_type.starts_with('_option_') && !typ.starts_with('_option_') {
			g.sb.write_string('${typ} ${name} = ({ ${rhs_type} _tmp = ')
			g.gen_expr(rhs)
			g.sb.writeln('; (*(${typ}*)(((u8*)(&_tmp.err)) + sizeof(IError))); });')
			return
		}
		if rhs is ast.IfExpr {
			if !g.if_expr_can_be_ternary(rhs) && rhs.else_expr !is ast.EmptyExpr {
				g.sb.writeln('${typ} ${name};')
				g.gen_decl_if_expr(name, rhs)
				return
			}
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
		g.sb.write_string('${typ} ${name} = ')
		g.gen_expr(rhs)
		g.sb.writeln(';')
	} else {
		// Assignment
		// Handle result/option .data field write: _t.data = val -> unwrapped value pointer = val
		if lhs is ast.SelectorExpr && lhs.rhs.name == 'data' {
			lhs_type := g.get_expr_type(lhs.lhs)
			if lhs_type.starts_with('_result_') || lhs_type.starts_with('_option_') {
				base := if lhs_type.starts_with('_result_') {
					g.result_value_type(lhs_type)
				} else {
					option_value_type(lhs_type)
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
		if lhs is ast.Ident {
			if local_type := g.get_local_var_c_type(lhs.name) {
				if local_type.ends_with('*') {
					rhs_type := g.get_expr_type(rhs)
					if !rhs_type.ends_with('*') {
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
			g.write_indent()
			g.sb.write_string('return ')
			g.gen_expr(stmt.expr)
			g.sb.writeln(';')
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
		g.write_indent()
		g.sb.write_string('return ')
		g.gen_expr(if_expr.else_expr)
		g.sb.writeln(';')
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
	'memset', 'memmove', 'strlen', 'strcpy', 'strcat', 'strcmp', 'memcmp']

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
	for field in node.fields {
		typ_name := g.field_type_name(field.typ)
		if typ_name == '' {
			continue
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
		if 'body_${typ_name}' !in g.emitted_types && 'enum_${typ_name}' !in g.emitted_types
			&& 'alias_${typ_name}' !in g.emitted_types {
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

	// Use named struct to match the forward declaration: typedef struct name name;
	g.sb.writeln('${keyword} ${name} {')
	// Embedded structs as fields
	for emb in node.embedded {
		emb_type := g.expr_type_to_c(emb)
		g.sb.writeln('\t${emb_type} ${emb_type};')
	}
	// Regular fields
	for field in node.fields {
		field_name := escape_c_keyword(field.name)
		if field.typ is ast.Type && field.typ is ast.ArrayFixedType {
			fixed_typ := field.typ as ast.ArrayFixedType
			elem_type := g.expr_type_to_c(fixed_typ.elem_type)
			g.sb.write_string('\t${elem_type} ${field_name}[')
			g.gen_expr(fixed_typ.len)
			g.sb.writeln('];')
			continue
		}
		field_type := g.expr_type_to_c(field.typ)
		g.sb.writeln('\t${field_type} ${field_name};')
	}
	g.sb.writeln('};')
	g.sb.writeln('')
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
	is_flag := node.attributes.has('flag')

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
	g.sb.writeln('typedef ${base_type} ${name};')
}

fn (mut g Gen) gen_sum_type_decl(node ast.TypeDecl) {
	name := g.get_type_decl_name(node)
	body_key := 'body_${name}'
	if body_key in g.emitted_types {
		return
	}
	g.emitted_types[body_key] = true

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
	for field in node.fields {
		if fn_type := g.get_fn_type_from_expr(field.typ) {
			mut ret := 'void'
			if fn_type.return_type !is ast.EmptyExpr {
				ret = g.expr_type_to_c(fn_type.return_type)
			}
			g.sb.write_string('\t${ret} (*${field.name})(void*')
			for param in fn_type.params {
				g.sb.write_string(', ')
				t := g.expr_type_to_c(param.typ)
				g.sb.write_string(t)
			}
			g.sb.writeln(');')
		} else {
			// Regular field
			t := g.expr_type_to_c(field.typ)
			g.sb.writeln('\t${t} ${field.name};')
		}
	}
	g.sb.writeln('};')
	g.sb.writeln('')
}

fn (g &Gen) is_enum_type(name string) bool {
	// Check emitted_types for enum_Name or enum_module__Name
	if 'enum_${name}' in g.emitted_types {
		return true
	}
	qualified := g.get_qualified_name(name)
	if 'enum_${qualified}' in g.emitted_types {
		return true
	}
	// Also check the types.Environment
	if g.env != unsafe { nil } {
		mut scope := lock g.env.scopes {
			g.env.scopes[g.cur_module] or { unsafe { nil } }
		}
		if scope != unsafe { nil } {
			if obj := scope.lookup_parent(name, 0) {
				if obj is types.Type {
					if obj is types.Enum {
						return true
					}
				}
			}
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

fn (g &Gen) is_type_name(name string) bool {
	if name in primitive_types {
		return true
	}
	// Check if it's a known emitted type (enum, struct, alias, sum type, interface)
	qualified := g.get_qualified_name(name)
	if 'enum_${name}' in g.emitted_types || 'enum_${qualified}' in g.emitted_types {
		return true
	}
	if 'body_${name}' in g.emitted_types || 'body_${qualified}' in g.emitted_types {
		return true
	}
	if 'alias_${name}' in g.emitted_types || 'alias_${qualified}' in g.emitted_types {
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
		mut scope := lock g.env.scopes {
			g.env.scopes[g.cur_module] or { unsafe { nil } }
		}
		if scope != unsafe { nil } {
			if obj := scope.lookup_parent(name, 0) {
				return obj is types.Module
			}
		}
	}
	return false
}

fn sanitize_c_number_literal(lit string) string {
	if lit.contains('_') {
		return lit.replace('_', '')
	}
	return lit
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
	if trimmed.ends_with('*') {
		return '0'
	}
	if trimmed in primitive_types || trimmed in ['void*', 'char*', 'byteptr', 'charptr', 'voidptr'] {
		return '0'
	}
	return '((${trimmed}){0})'
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
		g.tuple_aliases[name] = elem_types.clone()
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
				escaped := node.value.replace('\\', '\\\\').replace("'", "\\'")
				g.sb.write_string("'${escaped}'")
			} else {
				g.sb.write_string(sanitize_c_number_literal(node.value))
			}
		}
		ast.StringLiteral {
			val := node.value.trim("'").trim('"')
			escaped := val.replace('"', '\\"')
			if node.kind == .c {
				// C string literal: emit raw C string
				g.sb.write_string('"${escaped}"')
			} else {
				g.sb.write_string('(string){"${escaped}", ${val.len}}')
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
				if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin'
					&& !node.name.contains('__')
					&& 'const_${g.cur_module}__${node.name}' in g.emitted_types {
					g.sb.write_string('${g.cur_module}__${node.name}')
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
		ast.ParenExpr {
			g.sb.write_string('(')
			g.gen_expr(node.expr)
			g.sb.write_string(')')
		}
		ast.InfixExpr {
			lhs_type := g.get_expr_type(node.lhs)
			rhs_type := g.get_expr_type(node.rhs)
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
								if lhs_type.starts_with('Array_') {
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
					if node.expr.args.len == 1 {
						target_type := g.expr_type_to_c(node.expr.lhs)
						g.sb.write_string('((${target_type}*)(')
						g.gen_expr(node.expr.args[0])
						g.sb.write_string('))')
						return
					}
				}
				if node.expr is ast.CastExpr {
					target_type := g.expr_type_to_c(node.expr.typ)
					g.sb.write_string('((${target_type}*)(')
					g.gen_expr(node.expr.expr)
					g.sb.write_string('))')
					return
				}
				if node.expr is ast.CallOrCastExpr {
					if node.expr.lhs is ast.Ident {
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
				g.sb.write_string('((${type_name})(')
				g.gen_expr(node.expr)
				g.sb.write_string('))')
			} else if node.lhs is ast.Type {
				type_name := g.expr_type_to_c(node.lhs)
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
				g.sb.write_string('((${type_name})(')
				g.gen_expr(node.expr)
				g.sb.write_string('))')
			} else {
				// Single-arg call: println(x) is parsed as CallOrCastExpr
				g.gen_call_expr(node.lhs, [node.expr])
			}
		}
		ast.SelectorExpr {
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
					if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin'
						&& 'const_${g.cur_module}__${node.lhs.name}' in g.emitted_types {
						fixed_name = '${g.cur_module}__${node.lhs.name}'
					}
					if fixed_name in g.fixed_array_globals
						|| ('const_${fixed_name}' in g.emitted_types
						&& g.get_expr_type(node.lhs).starts_with('Array_')) {
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
				if raw_type := g.get_raw_type(node) {
					if raw_type is types.Enum {
						g.sb.write_string('${g.normalize_enum_name(raw_type.name)}__${node.rhs.name}')
						return
					}
				}
				if enum_name := g.get_expr_type_from_env(node) {
					if enum_name != '' && enum_name != 'int' {
						g.sb.write_string('${g.normalize_enum_name(enum_name)}__${node.rhs.name}')
						return
					}
				}
				if enum_name := g.enum_value_to_enum[node.rhs.name] {
					g.sb.write_string('${g.normalize_enum_name(enum_name)}__${node.rhs.name}')
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
				if !use_ptr && node.lhs is ast.Ident {
					if local_type := g.get_local_var_c_type(node.lhs.name) {
						use_ptr = local_type.ends_with('*')
					}
				}
				selector := if use_ptr { '->' } else { '.' }
				g.gen_expr(node.lhs)
				g.sb.write_string('${selector}${node.rhs.name}')
			}
		}
		ast.IfExpr {
			// If-expression used as a value: prefer ternary when it has a value type.
			if g.if_expr_can_be_ternary(node) && g.infer_if_expr_type(node) != 'void' {
				g.gen_if_expr_ternary(node)
				return
			}
			g.gen_if_expr_stmt(node)
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
			g.sb.write_string('/* [TODO] AssocExpr */ {0}')
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
	mut map_type := ''
	if node.typ !is ast.EmptyExpr {
		map_type = g.expr_type_to_c(node.typ)
	}
	if map_type == '' {
		if raw_type := g.get_raw_type(node) {
			if raw_type is types.Map {
				map_type = g.types_type_to_c(raw_type)
			}
		}
	}
	if map_type == '' {
		if env_type := g.get_expr_type_from_env(node) {
			if env_type.starts_with('Map_') {
				map_type = env_type
			}
		}
	}
	if map_type == '' {
		map_type = 'map'
	}
	g.sb.write_string('((${map_type}){0})')
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
			if expr.stmts.len == 1 && expr.stmts[0] is ast.ExprStmt {
				return g.extract_if_expr((expr.stmts[0] as ast.ExprStmt).expr)
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
	if raw_type := g.get_raw_type(base_arg) {
		if raw_type is types.Pointer || raw_type is types.Nil {
			return true
		}
	}
	match base_arg {
		ast.PrefixExpr {
			return base_arg.op == .amp
		}
		ast.Ident {
			return base_arg.name == 'nil'
		}
		else {}
	}
	return g.get_expr_type(base_arg).ends_with('*')
}

fn (mut g Gen) can_take_address(arg ast.Expr) bool {
	base_arg := if arg is ast.ModifierExpr { arg.expr } else { arg }
	match base_arg {
		ast.Ident, ast.SelectorExpr, ast.IndexExpr, ast.ParenExpr {
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
	addr_type := if typ == '' { 'int' } else { typ }
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
	if ptr_params := g.fn_param_is_ptr[fn_name] {
		if idx < ptr_params.len {
			want_ptr := ptr_params[idx]
			if want_ptr && base_arg is ast.PrefixExpr && base_arg.op == .amp {
				inner := base_arg.expr
				if g.expr_is_pointer(inner) {
					g.gen_expr(inner)
					return
				}
			}
			got_ptr := g.expr_is_pointer(base_arg)
			if want_ptr && !got_ptr && g.can_take_address(base_arg) {
				g.sb.write_string('&')
				g.gen_expr(base_arg)
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
	g.gen_expr(base_arg)
}

fn (mut g Gen) method_receiver_base_type(expr ast.Expr) string {
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
		fn_ptr_ret := g.fn_pointer_return_type(lhs)
		if fn_ptr_ret != '' {
			return fn_ptr_ret
		}
	}
	c_name := g.resolve_call_name(lhs, arg_count)
	if c_name == '' {
		return none
	}
	if ret := g.fn_return_types[c_name] {
		return ret
	}
	return none
}

fn (mut g Gen) normalize_named_call_args(fn_name string, call_args []ast.Expr) []ast.Expr {
	mut first_field_idx := -1
	for i, arg in call_args {
		if arg is ast.FieldInit {
			first_field_idx = i
			break
		}
	}
	if first_field_idx < 0 {
		return call_args
	}
	mut param_types := g.fn_param_types[fn_name] or { []string{} }
	if param_types.len == 0 && fn_name != '' && !fn_name.contains('__') && g.cur_module != ''
		&& g.cur_module != 'main' && g.cur_module != 'builtin' {
		qualified := '${g.cur_module}__${fn_name}'
		param_types = g.fn_param_types[qualified] or { []string{} }
	}
	if param_types.len == 0 {
		return call_args
	}
	if first_field_idx >= param_types.len {
		return call_args
	}
	param_type := param_types[first_field_idx]
	if param_type == '' {
		return call_args
	}
	mut fields := []ast.FieldInit{}
	mut trailing_args := []ast.Expr{}
	for i in first_field_idx .. call_args.len {
		arg := call_args[i]
		if arg is ast.FieldInit {
			fields << arg
		} else {
			trailing_args << arg
		}
	}
	if fields.len == 0 {
		return call_args
	}
	mut init_type := param_type.trim_right('*')
	if init_type == '' {
		init_type = param_type
	}
	mut grouped_arg := ast.Expr(ast.InitExpr{
		typ:    ast.Expr(ast.Ident{
			name: init_type
		})
		fields: fields
	})
	if param_type.ends_with('*') {
		grouped_arg = ast.Expr(ast.PrefixExpr{
			op:   .amp
			expr: grouped_arg
		})
	}
	mut out := []ast.Expr{cap: first_field_idx + 1 + trailing_args.len}
	for i in 0 .. first_field_idx {
		out << call_args[i]
	}
	out << grouped_arg
	out << trailing_args
	return out
}

fn (mut g Gen) infer_array_contains_elem_type(name string, call_args []ast.Expr) string {
	arr_type := if call_args.len > 0 { g.get_expr_type(call_args[0]) } else { '' }
	if arr_type.starts_with('Array_') {
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

fn (mut g Gen) gen_call_expr(lhs ast.Expr, args []ast.Expr) {
	if lhs is ast.SelectorExpr && g.is_fn_pointer_expr(lhs) {
		mut should_emit_fnptr_call := true
		resolved := g.resolve_call_name(lhs, args.len)
		if resolved != '' && (resolved in g.fn_param_is_ptr || resolved in g.fn_return_types) {
			should_emit_fnptr_call = false
		}
		if should_emit_fnptr_call {
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
	mut name := ''
	mut call_args := []ast.Expr{}
	call_args << args
	if lhs is ast.Ident {
		name = sanitize_fn_ident(lhs.name)
	} else if lhs is ast.SelectorExpr {
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
		if lhs.lhs is ast.Ident && g.is_module_ident(lhs.lhs.name) {
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
	if name.starts_with('strings__Builder__') && name !in g.fn_param_is_ptr
		&& name !in g.fn_return_types {
		method_name := name.all_after_last('__')
		array_name := 'array__${method_name}'
		if array_name in g.fn_param_is_ptr || array_name in g.fn_return_types {
			name = array_name
		}
	}
	call_args = g.normalize_named_call_args(name, call_args)
	if g.gen_interface_method_dispatch(name, call_args) {
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
	if name in ['array__first', 'array__last'] && call_args.len == 1 {
		arg_type := g.get_expr_type(call_args[0])
		if arg_type.starts_with('Array_') {
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
	if name.starts_with('__Map_') && name.ends_with('_set') && call_args.len == 3 {
		key_type := g.get_expr_type(call_args[1])
		val_type := g.get_expr_type(call_args[2])
		g.sb.write_string('map__set(')
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
		g.gen_addr_of_expr(call_args[2], if val_type == '' { 'int' } else { val_type })
		g.sb.write_string(')')
		return
	}
	if name == 'voidptr__vbytes' {
		name = 'void__vbytes'
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
	if is_c_runtime_function(name) {
		c_name = name
	} else if name != '' && g.cur_module != '' && g.cur_module != 'main'
		&& g.cur_module != 'builtin' && !name.contains('__') {
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
	g.sb.write_string('${c_name}(')
	mut total_args := call_args.len
	if params := g.fn_param_is_ptr[c_name] {
		if params.len > total_args {
			total_args = params.len
		}
	}
	for i in 0 .. total_args {
		if i > 0 {
			g.sb.write_string(', ')
		}
		if i < call_args.len {
			g.gen_call_arg(c_name, i, call_args[i])
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
			return 'Array_${mangle_alias_component(elem)}'
		}
		types.Struct {
			return t.name
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
			return t.get_name()
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
			return g.types_type_to_c(typ)
		}
	}
	return none
}

// receiver_type_to_scope_name converts a receiver type AST expression to
// the V-style name used by the checker for scope keys.
// This must match what the checker computes: receiver_type.base_type().name()
fn (g &Gen) receiver_type_to_scope_name(typ ast.Expr) string {
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

// get_local_var_c_type looks up a local variable's C type string from the function scope
fn (mut g Gen) get_local_var_c_type(name string) ?string {
	if g.cur_fn_scope != unsafe { nil } {
		if obj := g.cur_fn_scope.lookup_parent(name, 0) {
			if obj is types.Module {
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
		if local_type := g.get_local_var_c_type(node.name) {
			return local_type
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
			else {}
		}
		return t
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
					types.Alias {
						match raw_type.base_type {
							types.Array {
								return g.types_type_to_c(raw_type.base_type.elem_type)
							}
							types.ArrayFixed {
								return g.types_type_to_c(raw_type.base_type.elem_type)
							}
							else {}
						}
					}
					types.Pointer {
						match raw_type.base_type {
							types.Array {
								return g.types_type_to_c(raw_type.base_type.elem_type)
							}
							types.ArrayFixed {
								return g.types_type_to_c(raw_type.base_type.elem_type)
							}
							types.Alias {
								match raw_type.base_type.base_type {
									types.Array {
										return g.types_type_to_c(raw_type.base_type.base_type.elem_type)
									}
									types.ArrayFixed {
										return g.types_type_to_c(raw_type.base_type.base_type.elem_type)
									}
									else {}
								}
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
			if lhs_type.starts_with('Array_') {
				return lhs_type['Array_'.len..].trim_right('*')
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
				return 'Array_fixed_${elem}_${node.exprs.len}'
			}
			return 'array'
		}
		ast.CallExpr {
			if ret := g.get_call_return_type(node.lhs, node.args.len) {
				return ret
			}
			return 'int'
		}
		ast.CallOrCastExpr {
			if node.lhs is ast.Ident && g.is_type_name(node.lhs.name) {
				return g.expr_type_to_c(node.lhs)
			}
			if node.lhs is ast.Type {
				return g.expr_type_to_c(node.lhs)
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
				array_type := 'Array_${elem_type}'
				g.register_alias_type(array_type)
				return array_type
			}
			if e is ast.TupleType {
				mut elem_types := []string{cap: e.types.len}
				for t in e.types {
					elem_types << g.expr_type_to_c(t)
				}
				return g.register_tuple_alias(elem_types)
			}
			if e is ast.MapType {
				key_type := mangle_alias_component(g.expr_type_to_c(e.key_type))
				value_type := mangle_alias_component(g.expr_type_to_c(e.value_type))
				map_type := 'Map_${key_type}_${value_type}'
				g.register_alias_type(map_type)
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
			return 'int'
		}
		else {
			return 'int'
		}
	}
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
	return '${g.cur_module}::${name}' in g.module_type_names
}

// get_raw_type returns the raw types.Type for an expression from the Environment
fn (mut g Gen) get_raw_type(node ast.Expr) ?types.Type {
	if g.env == unsafe { nil } {
		return none
	}
	// For identifiers, check function scope first
	if node is ast.Ident {
		if g.cur_fn_scope != unsafe { nil } {
			if obj := g.cur_fn_scope.lookup_parent(node.name, 0) {
				if obj is types.Module {
					return none
				}
				return obj.typ()
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
		if field_type := g.struct_field_types['${struct_name}.${sel.rhs.name}'] {
			return field_type
		}
	}
	if struct_name.contains('__') {
		short_name := struct_name.all_after_last('__')
		if field_type := g.struct_field_types['${short_name}.${sel.rhs.name}'] {
			return field_type
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
		if elem := g.fixed_array_field_elem['${struct_name}.${sel.rhs.name}'] {
			return elem
		}
	}
	if struct_name.contains('__') {
		short_name := struct_name.all_after_last('__')
		if elem := g.fixed_array_field_elem['${short_name}.${sel.rhs.name}'] {
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
		if raw_type is types.Map {
			key_type := g.types_type_to_c(raw_type.key_type)
			val_type := g.types_type_to_c(raw_type.value_type)
			g.sb.write_string('(*(${val_type}*)map__get(&')
			g.gen_expr(node.lhs)
			g.sb.write_string(', ')
			g.gen_addr_of_expr(node.expr, key_type)
			g.sb.write_string(', &(${val_type}){0}))')
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
				g.sb.write_string(', &(${val_type}){0}))')
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
		g.sb.write_string(', &(${val_type}){0}))')
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
	if lhs_type == 'array' || lhs_type.starts_with('Array_') {
		mut elem_type := g.get_expr_type(node)
		if elem_type == '' || elem_type == 'int' {
			if lhs_type.starts_with('Array_') {
				elem_type = lhs_type['Array_'.len..].trim_right('*')
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
		g.sb.write_string('((${type_name}){0})')
		return
	}
	g.sb.write_string('((${type_name}){')
	for i, field in node.fields {
		if i > 0 {
			g.sb.write_string(',')
		}
		g.sb.write_string('.${field.name} = ')
		g.gen_expr(field.value)
	}
	g.sb.write_string('})')
}

fn (mut g Gen) gen_array_init_expr(node ast.ArrayInitExpr) {
	elem_type := g.extract_array_elem_type(node.typ)
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
		typ := g.get_expr_type(field.value)
		if typ == 'string' {
			// String constants need a global variable
			g.sb.write_string('string ${name} = ')
			g.gen_expr(field.value)
			g.sb.writeln(';')
		} else if typ in ['bool', 'char', 'rune', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16',
			'u32', 'u64', 'usize', 'isize', 'f32', 'f64', 'float_literal', 'int_literal'] {
			// Prefer typed globals over macros to avoid textual replacement collisions.
			g.sb.write_string('static const ${typ} ${name} = ')
			g.gen_expr(field.value)
			g.sb.writeln(';')
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
	g.sb.write_string('((${type_name})(')
	g.gen_expr(node.expr)
	g.sb.write_string('))')
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
	// Sum type cast: a as Cat => (*((main__Cat*)a._data._Cat))
	// Short variant name for _data._ accessor (strip module prefix)
	short_name := if type_name.contains('__') {
		type_name.all_after_last('__')
	} else {
		type_name
	}
	g.sb.write_string('(*((${type_name}*)(')
	g.gen_expr(node.expr)
	g.sb.write_string(')._data._${short_name}))')
}

fn (mut g Gen) gen_string_inter_literal(node ast.StringInterLiteral) {
	// Use sprintf approach with asprintf (allocates automatically)
	// Wrapped in GCC compound expression ({ ... })
	// Build format string, stripping V string delimiters from values
	mut fmt_str := strings.new_builder(64)
	for i, raw_val in node.values {
		mut val := raw_val
		// Strip V string delimiters: leading quote from first value, trailing from last
		if i == 0 {
			val = val.trim_left('\'"')
		}
		if i == node.values.len - 1 {
			val = val.trim_right('\'"')
		}
		escaped := val.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n').replace('\t',
			'\\t')
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
