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

fn runtime_const_target_key(mod string, name string) string {
	return '${mod}::${name}'
}

fn generated_decl_name_for_module(mod string, field_name string) ?string {
	if field_name == '' || field_name.starts_with('C.') {
		return none
	}
	if mod != '' && mod != 'main' && mod != 'builtin' {
		return '${mod}__${field_name}'
	}
	return field_name
}

fn (g &Gen) generated_decl_name(field_name string) ?string {
	return generated_decl_name_for_module(g.cur_module, field_name)
}

fn (g &Gen) is_scalar_zero_init_type(typ string) bool {
	if typ in primitive_types || typ.ends_with('*') || typ in ['byteptr', 'charptr', 'voidptr'] {
		return true
	}
	return g.is_enum_type(typ)
}

fn (mut g Gen) collect_runtime_const_targets() {
	g.runtime_const_targets = map[string]bool{}
	mut module_consts := map[string]map[string]bool{}
	for file in g.files {
		mut const_names := if file.mod in module_consts {
			module_consts[file.mod].clone()
		} else {
			map[string]bool{}
		}
		for stmt in file.stmts {
			if stmt !is ast.ConstDecl {
				continue
			}
			const_decl := stmt as ast.ConstDecl
			for field in const_decl.fields {
				if field.name != '' && !field.name.starts_with('C.') {
					const_names[field.name] = true
				}
			}
		}
		module_consts[file.mod] = const_names.clone()
	}
	for file in g.files {
		g.set_file_module(file)
		const_names := if g.cur_module in module_consts {
			module_consts[g.cur_module].clone()
		} else {
			map[string]bool{}
		}
		if const_names.len == 0 {
			continue
		}
		for stmt in file.stmts {
			if stmt !is ast.FnDecl {
				continue
			}
			fn_decl := stmt as ast.FnDecl
			if !fn_decl.name.starts_with('__v_init_consts_') {
				continue
			}
			for body_stmt in fn_decl.stmts {
				if body_stmt !is ast.AssignStmt {
					continue
				}
				assign_stmt := body_stmt as ast.AssignStmt
				if assign_stmt.lhs.len != 1 {
					continue
				}
				lhs_expr := assign_stmt.lhs[0]
				if lhs_expr !is ast.Ident {
					continue
				}
				lhs_ident := lhs_expr as ast.Ident
				if lhs_ident.name !in const_names {
					continue
				}
				g.runtime_const_targets[runtime_const_target_key(g.cur_module, lhs_ident.name)] = true
			}
		}
	}
}

fn (g &Gen) is_runtime_const_target(field_name string) bool {
	return runtime_const_target_key(g.cur_module, field_name) in g.runtime_const_targets
}

fn (mut g Gen) lookup_const_expr(mod string, name string) ?ast.Expr {
	for file in g.files {
		if file.mod != mod {
			continue
		}
		for stmt in file.stmts {
			if stmt !is ast.ConstDecl {
				continue
			}
			const_decl := stmt as ast.ConstDecl
			for field in const_decl.fields {
				if field.name == name {
					return field.value
				}
			}
		}
	}
	return none
}

fn (mut g Gen) lookup_generated_const_expr(generated_name string) ?ast.Expr {
	for file in g.files {
		for stmt in file.stmts {
			if stmt !is ast.ConstDecl {
				continue
			}
			const_decl := stmt as ast.ConstDecl
			for field in const_decl.fields {
				name := generated_decl_name_for_module(file.mod, field.name) or { continue }
				if name == generated_name {
					return field.value
				}
			}
		}
	}
	return none
}

fn (mut g Gen) enum_const_selector_type(expr ast.Expr) string {
	if expr !is ast.SelectorExpr {
		return ''
	}
	sel := expr as ast.SelectorExpr
	if raw_type := g.get_raw_type(sel) {
		match raw_type {
			types.Enum {
				enum_type := g.types_type_to_c(raw_type)
				if enum_type != '' {
					return enum_type
				}
			}
			types.Alias {
				if raw_type.base_type is types.Enum {
					enum_type := g.types_type_to_c(raw_type)
					if enum_type != '' {
						return enum_type
					}
				}
			}
			else {}
		}
	}
	lhs_type := g.get_expr_type(sel.lhs)
	if lhs_type != '' && g.is_enum_type(lhs_type) && g.enum_has_field(lhs_type, sel.rhs.name) {
		return lhs_type
	}
	return ''
}

fn (mut g Gen) const_decl_storage_type(expr ast.Expr) string {
	if expr is ast.Ident {
		if const_expr := g.lookup_const_expr(g.cur_module, expr.name) {
			return g.const_decl_storage_type(const_expr)
		}
		if g.cur_module != 'builtin' {
			if const_expr := g.lookup_const_expr('builtin', expr.name) {
				return g.const_decl_storage_type(const_expr)
			}
		}
		if const_expr := g.lookup_generated_const_expr(expr.name) {
			return g.const_decl_storage_type(const_expr)
		}
	}
	if expr is ast.SelectorExpr && expr.lhs is ast.Ident {
		if const_expr := g.lookup_const_expr(expr.lhs.name, expr.rhs.name) {
			return g.const_decl_storage_type(const_expr)
		}
	}
	enum_type := g.enum_const_selector_type(expr)
	if enum_type != '' {
		return enum_type
	}
	if expr is ast.InfixExpr {
		if expr.op in [.eq, .ne, .lt, .gt, .le, .ge, .and, .logical_or] {
			return 'bool'
		}
		if expr.op in [.plus, .minus, .mul, .div, .mod] {
			lhs_type := g.const_decl_storage_type(expr.lhs)
			rhs_type := g.const_decl_storage_type(expr.rhs)
			if expr.op == .plus && (lhs_type == 'string' || rhs_type == 'string') {
				return 'string'
			}
			return promote_numeric_c_types(lhs_type, rhs_type)
		}
	}
	if raw_type := g.get_raw_type(expr) {
		raw_c_type := g.types_type_to_c(raw_type)
		if raw_c_type != '' && raw_c_type != 'void' && !raw_c_type.contains('literal') {
			return raw_c_type
		}
	}
	mut typ := g.get_expr_type(expr)
	if typ == '' || typ == 'void' || typ == 'int_literal' || typ == 'float_literal' {
		if expr is ast.InfixExpr {
			numeric_type := g.infer_numeric_expr_type(expr)
			if numeric_type != '' && !numeric_type.contains('literal') {
				return numeric_type
			}
		}
		if expr is ast.BasicLiteral {
			if expr.kind == .number {
				if expr.value.contains('.') || expr.value.contains('e') || expr.value.contains('E') {
					return 'f64'
				}
				return 'int'
			}
		}
		if typ == 'float_literal' {
			return 'f64'
		}
		return 'int'
	}
	if expr is ast.InfixExpr && typ == 'string' {
		numeric_type := g.infer_numeric_expr_type(expr)
		if numeric_type != '' && !numeric_type.contains('literal') && numeric_type != 'string' {
			return numeric_type
		}
	}
	return typ
}

fn (mut g Gen) emit_runtime_const_storage_decl(name string, typ string) {
	if typ == 'string' {
		g.sb.writeln('string ${name} = {0};')
		return
	}
	if g.is_scalar_zero_init_type(typ) {
		g.sb.writeln('${typ} ${name} = 0;')
		return
	}
	g.sb.writeln('${typ} ${name} = {0};')
}

fn (g &Gen) is_generated_non_type_ident(expr ast.Expr) bool {
	if expr !is ast.Ident {
		return false
	}
	value_ident := expr as ast.Ident
	return value_ident.name.contains('__') && !g.is_type_name(value_ident.name)
}

fn (mut g Gen) gen_const_decl_extern(node ast.ConstDecl) {
	for field in node.fields {
		name := g.generated_decl_name(field.name) or { continue }
		// Skip consts that shadow a function with the same name — the
		// function declaration takes precedence and a #define would break it.
		// Only skip when a forward declaration was actually emitted (fn_owner_file).
		if 'fn_${name}' in g.fn_owner_file {
			continue
		}
		// Skip constants already emitted as either extern or #define
		// (can happen when both .vh and full source files are parsed).
		extern_key := 'extern_const_${name}'
		macro_key := 'extern_const_macro_${name}'
		if extern_key in g.emitted_types || macro_key in g.emitted_types {
			continue
		}
		mut is_type_only := is_header_type_only_const_expr(field.value)
		if is_type_only && g.is_generated_non_type_ident(field.value) {
			is_type_only = false
		}
		if g.is_runtime_const_target(field.name) {
			typ := g.const_decl_storage_type(field.value)
			if typ == '' || typ == 'void' {
				continue
			}
			g.emitted_types[extern_key] = true
			g.sb.writeln('extern ${typ} ${name};')
			continue
		}
		if is_type_only {
			key := extern_key
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
		value_expr := g.expr_to_string(field.value)
		if value_expr.len == 0 {
			continue
		}
		// Array consts with static backing data or inline compound literals
		// cannot be #defined across translation units. Emit extern declarations instead.
		if value_expr.contains('__const_array_data_')
			|| value_expr.contains('new_array_from_c_array') {
			g.emitted_types[macro_key] = true
			g.sb.writeln('extern array ${name};')
			continue
		}
		mut macro_expr := value_expr
		if macro_expr.contains('\n') {
			// Keep macro definitions valid when value expressions are rendered
			// across multiple lines (e.g. large string literals).
			macro_expr = macro_expr.replace('\n', ' \\\n')
		}
		g.emitted_types[macro_key] = true
		// Unqualified numeric consts use #define with fully-resolved literal
		// values and also store into const_exprs so that downstream consts
		// that reference them can be resolved too. This avoids macro
		// collisions with local variables of the same name (e.g. max_len
		// from sorted_map.v vs max_len parameter in eprint_space_padding).
		if !name.contains('__') && is_numeric_const_expr(field.value) {
			typ := g.const_decl_storage_type(field.value)
			if typ != '' && typ != 'void' {
				resolved := if expr_contains_ident(field.value) {
					g.resolve_const_expr(macro_expr)
				} else {
					macro_expr
				}
				g.const_exprs[name] = resolved
				g.sb.writeln('static const ${typ} ${name} = ${resolved};')
				continue
			}
		}
		if !name.contains('__') {
			typ := g.const_decl_storage_type(field.value)
			if g.is_scalar_zero_init_type(typ) || typ == 'string' {
				resolved := if expr_contains_ident(field.value) {
					g.resolve_const_expr(macro_expr)
				} else {
					macro_expr
				}
				g.sb.writeln('static const ${typ} ${name} = ${resolved};')
				continue
			}
		}
		// C struct zero-init consts are emitted as global variables in
		// gen_const_decl, so the extern declaration must match.
		if is_c_struct_init(field.value) {
			typ := g.const_decl_storage_type(field.value)
			if typ != '' && typ != 'void' && typ != 'int' {
				g.sb.writeln('extern ${typ} ${name};')
				continue
			}
		}
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
		g.global_var_types[name] = typ
		// With prealloc, g_memory_block must be thread-local so each thread
		// gets its own arena and the bump allocator is safe without locks.
		if name == 'g_memory_block' && g.pref != unsafe { nil } && g.pref.prealloc {
			g.sb.write_string('_Thread_local ${typ} ${name}')
		} else {
			g.sb.write_string('${typ} ${name}')
		}
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
		g.global_var_types[name] = typ
		if name == 'g_memory_block' && g.pref != unsafe { nil } && g.pref.prealloc {
			g.sb.writeln('extern _Thread_local ${typ} ${name};')
		} else {
			g.sb.writeln('extern ${typ} ${name};')
		}
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
		name := g.generated_decl_name(field.name) or { continue }
		// Skip consts that shadow a function with the same name —
		// but only when a forward declaration was actually emitted
		// for that function (fn_owner_file is populated in pass 4).
		// Using fn_return_types alone is too broad: generic functions
		// like math.max[f64] register math__max_f64 in fn_return_types
		// but never emit a concrete function, falsely suppressing
		// the math__max_f64 constant.
		if 'fn_${name}' in g.fn_owner_file {
			continue
		}
		const_key := 'const_${name}'
		if const_key in g.emitted_types {
			continue
		}
		g.emitted_types[const_key] = true
		mut is_type_only := is_header_type_only_const_expr(field.value)
		if is_type_only && g.is_generated_non_type_ident(field.value) {
			is_type_only = false
		}
		if !g.should_emit_module(g.cur_module) && is_type_only {
			continue
		}
		// Type-only consts from .vh headers have no value — emit as zero-initialized globals
		// for simple scalar types only. Complex types (fixed arrays, structs) are skipped
		// because they need typedefs/initializer-lists that aren't available from .vh data.
		if is_type_only {
			typ := g.expr_type_to_c(field.value)
			if typ == '' || typ == 'void' || typ.starts_with('Array_fixed_') || typ.contains(' ')
				|| typ.contains('literal') {
				continue
			}
			if typ == 'string' {
				g.sb.writeln('string ${name} = {0};')
			} else {
				g.sb.writeln('${typ} ${name} = 0;')
			}
			continue
		}
		if g.is_runtime_const_target(field.name) {
			typ := g.const_decl_storage_type(field.value)
			if typ == '' || typ == 'void' {
				continue
			}
			g.emit_runtime_const_storage_decl(name, typ)
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
				for i in 0 .. array_value.exprs.len {
					elem := array_value.exprs[i]
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
		typ := g.const_decl_storage_type(field.value)
		// Function calls are not compile-time constants in C; emit as zero-initialized globals.
		if g.contains_call_expr(field.value) {
			g.emit_runtime_const_storage_decl(name, typ)
			continue
		}
		if typ == 'string' {
			// String constants need a global variable
			g.sb.write_string('string ${name} = ')
			g.expr(field.value)
			g.sb.writeln(';')
		} else if
			typ in ['bool', 'char', 'rune', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'usize', 'isize', 'f32', 'f64']
			|| g.is_enum_type(typ) {
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
			// C struct zero-init consts must be real global variables, not
			// #define macros, because macros expand to temporaries — taking
			// their address (&) or mutating them in-place has no effect.
			if typ != '' && typ != 'void' && typ != 'int' && is_c_struct_init(field.value) {
				g.sb.writeln('${typ} ${name} = {0};')
			} else if typ != '' && typ != 'void' && typ != 'int' && !typ.starts_with('Array_')
				&& !typ.starts_with('Map_') && !typ.contains('*') && !typ.contains('(')
				&& field.value is ast.InitExpr && (field.value as ast.InitExpr).fields.len == 0 {
				// Zero-initialized struct const — emit as global variable.
				g.sb.writeln('${typ} ${name} = {0};')
			} else {
				// Fallback for aggregate literals and other complex consts.
				g.sb.write_string('#define ${name} ')
				g.expr(field.value)
				g.sb.writeln('')
			}
			if typ != '' && typ != 'int' {
				g.const_types[name] = typ
			}
		}
	}
}

// is_c_struct_init returns true if the expression is a C struct zero-initialization
// like `C.mbedtls_ctr_drbg_context{}`. These must be emitted as global variables,
// not #define macros, because they need a stable memory address.
fn is_c_struct_init(e ast.Expr) bool {
	if e is ast.InitExpr {
		init := e as ast.InitExpr
		if init.typ is ast.Ident {
			ident := init.typ as ast.Ident
			return ident.name.starts_with('C.')
		}
	}
	return false
}

fn is_numeric_const_expr(e ast.Expr) bool {
	if e is ast.BasicLiteral {
		return true
	}
	if e is ast.InfixExpr {
		return is_numeric_const_expr(e.lhs) && is_numeric_const_expr(e.rhs)
	}
	if e is ast.CastExpr {
		return is_numeric_const_expr(e.expr)
	}
	if e is ast.ParenExpr {
		return is_numeric_const_expr(e.expr)
	}
	if e is ast.PrefixExpr {
		return is_numeric_const_expr(e.expr)
	}
	if e is ast.Ident {
		return true // references to other consts are still numeric
	}
	if e is ast.CallExpr {
		// sizeof() is a compile-time numeric expression
		if e.lhs is ast.Ident {
			return e.lhs.name == 'sizeof'
		}
	}
	return false
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
