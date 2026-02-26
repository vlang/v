// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import v2.ast
import v2.types

// set_export_const_symbols controls whether emitted module const macros
// also get exported as linkable global symbols.
pub fn (mut g Gen) set_export_const_symbols(enable bool) {
	g.export_const_symbols = enable
}

// set_cache_bundle_name sets the cache bundle label used for emitting a
// deterministic cache-init function (e.g. __v2_cached_init_builtin).
pub fn (mut g Gen) set_cache_bundle_name(name string) {
	g.cache_bundle_name = name.trim_space()
}

fn (mut g Gen) gen_file_extern_globals(file ast.File) {
	g.set_file_module(file)
	for stmt in file.stmts {
		if !stmt_has_valid_data(stmt) {
			continue
		}
		if stmt is ast.GlobalDecl {
			g.gen_global_decl_extern(stmt)
		}
	}
}

fn (mut g Gen) gen_file_extern_consts(file ast.File) {
	g.set_file_module(file)
	for stmt in file.stmts {
		if !stmt_has_valid_data(stmt) {
			continue
		}
		if stmt is ast.ConstDecl {
			g.gen_const_decl_extern(stmt)
		}
	}
}

fn (mut g Gen) gen_const_decl_extern(node ast.ConstDecl) {
	for field in node.fields {
		name := if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
			'${g.cur_module}__${field.name}'
		} else {
			field.name
		}
		is_type_only := is_header_type_only_const_expr(field.value)
		if is_type_only {
			key := 'extern_const_${name}'
			if key in g.emitted_types {
				continue
			}
			typ := g.expr_type_to_c(field.value)
			if typ == '' || typ == 'void' {
				continue
			}
			if typ.starts_with('Array_fixed_') || typ.contains(' ') || typ.contains('literal')
				|| typ == 'mach_timebase_info_data_t' {
				continue
			}
			g.emitted_types[key] = true
			g.sb.writeln('extern ${typ} ${name};')
			continue
		}
		macro_key := 'extern_const_macro_${name}'
		if macro_key in g.emitted_types {
			continue
		}
		value_expr := g.expr_to_string(field.value)
		if value_expr.len == 0 {
			continue
		}
		mut macro_expr := value_expr
		if macro_expr.contains('\n') {
			// Keep macro definitions valid when value expressions are rendered
			// across multiple lines (e.g. large string literals).
			macro_expr = macro_expr.replace('\n', ' \\\n')
		}
		g.emitted_types[macro_key] = true
		g.sb.writeln('#define ${name} ${macro_expr}')
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
			g.expr(fixed_typ.len)
			g.sb.write_string(']')
			if field.value !is ast.EmptyExpr {
				g.sb.write_string(' = ')
				g.expr(field.value)
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
				g.expr(field.value)
				g.sb.writeln(';')
			}
		} else {
			g.sb.writeln(';')
		}
	}
}

fn (mut g Gen) gen_global_decl_extern(node ast.GlobalDecl) {
	for field in node.fields {
		name := if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
			'${g.cur_module}__${field.name}'
		} else {
			field.name
		}
		key := 'extern_global_${name}'
		if key in g.emitted_types {
			continue
		}
		g.emitted_types[key] = true
		g.write_indent()
		if field.typ is ast.Type && field.typ is ast.ArrayFixedType {
			fixed_typ := field.typ as ast.ArrayFixedType
			elem_type := g.expr_type_to_c(fixed_typ.elem_type)
			g.sb.write_string('extern ${elem_type} ${name}[')
			g.expr(fixed_typ.len)
			g.sb.writeln('];')
			continue
		}
		mut typ := ''
		if field.typ !is ast.EmptyExpr {
			typ = g.expr_type_to_c(field.typ)
		} else if field.value !is ast.EmptyExpr {
			typ = g.get_expr_type(field.value)
		}
		if typ == 'mach_timebase_info_data_t' {
			continue
		}
		if typ == '' || typ == 'void' {
			typ = 'int'
		}
		g.sb.writeln('extern ${typ} ${name};')
	}
}

fn (mut g Gen) queue_exported_const_symbol(name string, typ string, value string) {
	if name in g.exported_const_seen {
		return
	}
	g.exported_const_seen[name] = true
	g.exported_const_symbols << ExportedConstSymbol{
		name:  name
		typ:   typ
		value: value
	}
}

fn (mut g Gen) emit_exported_const_symbols() {
	if !g.export_const_symbols || g.exported_const_symbols.len == 0 {
		return
	}
	g.sb.writeln('')
	for sym in g.exported_const_symbols {
		g.sb.writeln('#ifdef ${sym.name}')
		g.sb.writeln('#undef ${sym.name}')
		g.sb.writeln('#endif')
		// Resolve references to other constants so the initializer is a
		// compile-time constant expression (required by TCC and strict C).
		resolved := g.resolve_const_expr(sym.value)
		g.sb.writeln('const ${sym.typ} ${sym.name} = ${resolved};')
	}
}

fn (g &Gen) cached_init_function_name() string {
	if g.cache_bundle_name.len == 0 {
		return ''
	}
	return '__v2_cached_init_${g.cache_bundle_name}'
}

fn (mut g Gen) emit_cached_module_init_function() {
	if !g.export_const_symbols || g.emit_modules.len == 0 || g.cache_bundle_name.len == 0 {
		return
	}
	mut modules := g.emit_modules.keys()
	modules.sort()
	mut init_modules := []string{}
	for module_name in modules {
		if g.module_has_const_init_fn(module_name) {
			init_modules << module_name
		}
	}
	g.sb.writeln('')
	init_fn_name := g.cached_init_function_name()
	g.sb.writeln('void ${init_fn_name}(void) {')
	for module_name in init_modules {
		init_fn := '${module_name}____v_init_consts_${module_name}'
		g.sb.writeln('\t${init_fn}();')
	}
	g.sb.writeln('}')
}

fn (g &Gen) module_has_const_init_fn(module_name string) bool {
	init_fn := '${module_name}____v_init_consts_${module_name}'
	return init_fn in g.fn_return_types
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
		if !g.should_emit_module(g.cur_module) && is_header_type_only_const_expr(field.value) {
			continue
		}
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
				// Check that no element contains a function call (not valid in C static initializers)
				mut has_call := false
				for elem in array_value.exprs {
					if g.contains_call_expr(elem) {
						has_call = true
						break
					}
				}
				if !has_call {
					is_fixed_array_const = true
					fixed_array_elem = elem_type
					fixed_array_len = array_value.exprs.len
				}
			}
		}
		if is_fixed_array_const && fixed_array_elem != '' {
			g.fixed_array_globals[name] = true
			if fixed_array_len > 0 {
				g.sb.write_string('static const ${fixed_array_elem} ${name}[${fixed_array_len}] = ')
			} else {
				g.sb.write_string('static const ${fixed_array_elem} ${name}[] = ')
			}
			g.expr(field.value)
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
			g.expr(field.value)
			g.sb.writeln(';')
		} else if typ in ['bool', 'char', 'rune', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16',
			'u32', 'u64', 'usize', 'isize', 'f32', 'f64', 'float_literal', 'int_literal'] {
			value_expr := g.expr_to_string(field.value)
			g.const_exprs[name] = value_expr
			// Qualified const names are safe as macros and work in C constant-expression
			// contexts (array sizes, static initializers).
			if name.contains('__') {
				g.sb.writeln('#define ${name} ${value_expr}')
				if g.export_const_symbols {
					g.queue_exported_const_symbol(name, typ, value_expr)
				}
			} else {
				// Unqualified names use static const to avoid macro collisions.
				// If the expression references other constants, inline their values
				// so that tcc (and strict C) accepts the initializer.
				resolved := if expr_contains_ident(field.value) {
					g.resolve_const_expr(value_expr)
				} else {
					value_expr
				}
				g.sb.writeln('static const ${typ} ${name} = ${resolved};')
			}
		} else {
			// Fallback for aggregate literals and other complex consts.
			g.sb.write_string('#define ${name} ')
			g.expr(field.value)
			g.sb.writeln('')
		}
	}
}

fn expr_contains_ident(e ast.Expr) bool {
	if e is ast.Ident {
		return true
	}
	if e is ast.InfixExpr {
		return expr_contains_ident(e.lhs) || expr_contains_ident(e.rhs)
	}
	if e is ast.CastExpr {
		return expr_contains_ident(e.expr)
	}
	if e is ast.ParenExpr {
		return expr_contains_ident(e.expr)
	}
	if e is ast.PrefixExpr {
		return expr_contains_ident(e.expr)
	}
	return false
}

// resolve_const_expr replaces references to other constants in a C expression
// string with their expanded values, so that static const initializers
// contain only literal values (required by tcc and the C standard).
fn (g &Gen) resolve_const_expr(expr string) string {
	mut result := expr
	for _ in 0 .. 10 {
		mut changed := false
		for cname, cval in g.const_exprs {
			new_result := replace_whole_word(result, cname, cval)
			if new_result != result {
				result = new_result
				changed = true
			}
		}
		if !changed {
			break
		}
	}
	return result
}

fn replace_whole_word(s string, word string, replacement string) string {
	mut result := s
	mut pos := 0
	for {
		idx := result.index_after(word, pos) or { break }
		// Check word boundary before.
		if idx > 0 {
			c := result[idx - 1]
			if c == `_` || (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`)
				|| (c >= `0` && c <= `9`) {
				pos = idx + word.len
				continue
			}
		}
		// Check word boundary after.
		end := idx + word.len
		if end < result.len {
			c := result[end]
			if c == `_` || (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`)
				|| (c >= `0` && c <= `9`) {
				pos = idx + word.len
				continue
			}
		}
		result = result[..idx] + replacement + result[end..]
		pos = idx + replacement.len
	}
	return result
}
