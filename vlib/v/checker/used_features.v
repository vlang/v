// Copyright (c) 2019-2024 Felipe Pena. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast

@[inline]
fn (mut c Checker) markused_comptime_call(check bool, key string) {
	if check {
		c.table.used_features.comptime_calls[key] = true
	}
}

fn comptime_call_last_arg_type(arg ast.CallArg) ast.Type {
	return if arg.expr is ast.ArrayDecompose {
		arg.expr.expr_type
	} else {
		arg.typ
	}
}

fn (mut c Checker) markused_assertstmt_auto_str(mut node ast.AssertStmt) {
	if !c.is_builtin_mod && mut node.expr is ast.InfixExpr {
		left_type := c.unwrap_generic(node.expr.left_type)
		right_type := c.unwrap_generic(node.expr.right_type)
		c.markused_scalar_ptr_str(left_type)
		c.markused_scalar_ptr_str(right_type)
		c.markused_auto_str_dependencies(left_type)
		c.markused_auto_str_dependencies(right_type)
		if !c.table.used_features.auto_str && !c.table.sym(left_type).has_method('str') {
			c.table.used_features.auto_str = true
			return
		}
		if !c.table.used_features.auto_str && !c.table.sym(right_type).has_method('str') {
			c.table.used_features.auto_str = true
		}
	}
}

fn (mut c Checker) markused_dumpexpr(mut node ast.DumpExpr) {
	if c.is_builtin_mod {
		return
	}
	unwrapped_type := c.unwrap_generic(node.expr_type)
	c.markused_scalar_ptr_str(unwrapped_type)
	if node.expr_type.has_flag(.generic) {
		c.table.used_features.comptime_syms[unwrapped_type] = true
	}
	if !c.table.sym(unwrapped_type).has_method('str') {
		c.table.used_features.auto_str = true
		c.markused_auto_str_dependencies(unwrapped_type)
		if node.expr_type.is_ptr() {
			c.table.used_features.auto_str_ptr = true
		}
		if !c.table.used_features.auto_str_arr {
			c.table.used_features.auto_str_arr = c.table.final_sym(unwrapped_type).kind == .array
		}
	} else {
		c.markused_generic_str_method(unwrapped_type, c.table.sym(unwrapped_type))
		c.table.used_features.print_types[node.expr_type.idx()] = true
	}
	c.table.used_features.print_types[ast.int_type_idx] = true
}

@[inline]
fn (mut c Checker) markused_used_maps(check bool) {
	if check {
		c.table.used_features.used_maps++
	}
}

fn (mut c Checker) markused_castexpr(mut _ ast.CastExpr, _ ast.Type, mut final_to_sym ast.TypeSymbol) {
	if c.is_builtin_mod {
		return
	}
	if c.table.used_features.used_maps == 0 && mut final_to_sym.info is ast.SumType {
		if final_to_sym.info.variants.any(c.table.final_sym(it).kind == .map) {
			c.table.used_features.used_maps++
		}
	}
}

fn (mut c Checker) markused_comptimecall(mut node ast.ComptimeCall) {
	c.markused_comptime_call(true,
		'${int(c.unwrap_generic(c.comptime.comptime_for_method.receiver_type))}.${c.comptime.comptime_for_method.name}')
	if c.inside_anon_fn {
		// $method passed to anon fn, mark all methods as used
		sym := c.table.sym(c.unwrap_generic(node.left_type))
		for m in sym.get_methods() {
			c.table.used_features.comptime_calls['${int(c.unwrap_generic(m.receiver_type))}.${m.name}'] = true
			if node.args.len > 0 && m.params.len > 0 {
				last_param := m.params.last().typ
				last_arg_type := comptime_call_last_arg_type(node.args.last())
				if (last_param.is_int() || last_param.is_float()
					|| last_param.is_bool()) && c.table.final_sym(last_arg_type).kind == .array {
					c.table.used_features.comptime_calls['${ast.string_type_idx}.${c.table.type_to_str(m.params.last().typ)}'] = true
				}
			}
		}
	} else {
		m := c.comptime.comptime_for_method
		if node.args.len > 0 && m.params.len > 0 {
			last_param := m.params.last().typ
			last_arg_type := comptime_call_last_arg_type(node.args.last())
			if (last_param.is_int() || last_param.is_float() || last_param.is_bool())
				&& c.table.final_sym(last_arg_type).kind == .array {
				c.table.used_features.comptime_calls['${ast.string_type_idx}.${c.table.type_to_str(m.params.last().typ)}'] = true
			}
		}
	}
}

fn (mut c Checker) markused_comptimefor(mut _ ast.ComptimeFor, unwrapped_expr_type ast.Type) {
	if c.table.used_features.used_maps == 0 {
		final_sym := c.table.final_sym(unwrapped_expr_type)
		if final_sym.info is ast.Map {
			c.table.used_features.used_maps++
		} else if final_sym.info is ast.SumType {
			if final_sym.info.variants.any(c.table.final_sym(it).kind == .map) {
				c.table.used_features.used_maps++
			}
		}
	}
}

fn (mut c Checker) markused_call_expr(left_type ast.Type, mut node ast.CallExpr) {
	if left_type != 0 && node.name == 'str' {
		c.markused_scalar_ptr_str(left_type)
	}
	if left_type != 0 && left_type.is_ptr() && !c.table.used_features.auto_str_ptr
		&& node.name == 'str' {
		c.table.used_features.auto_str_ptr = true
		if !c.table.used_features.auto_str_arr {
			c.table.used_features.auto_str_arr = c.table.final_sym(left_type).kind == .array
		}
	}
}

fn (mut c Checker) markused_print_call(mut node ast.CallExpr) {
	if !c.is_builtin_mod && c.mod != 'math.bits' && node.args[0].expr !is ast.StringLiteral {
		arg_typ := c.unwrap_generic(node.args[0].typ)
		if arg_typ == 0 {
			return
		}
		c.markused_scalar_ptr_str(arg_typ)
		c.markused_auto_str_dependencies(arg_typ)
		if (node.args[0].expr is ast.CallExpr && node.args[0].expr.is_method
			&& node.args[0].expr.name == 'str')
			|| !c.table.sym(arg_typ).has_method('str') {
			c.table.used_features.auto_str = true
		} else {
			c.mark_type_str_method_as_referenced(arg_typ)
			if arg_typ.has_option_or_result() {
				c.table.used_features.print_options = true
			}
			c.table.used_features.print_types[arg_typ.idx()] = true
			if !c.table.used_features.auto_str_ptr && node.args[0].expr is ast.Ident {
				var_obj := node.args[0].expr.obj
				if var_obj is ast.Var {
					if var_obj.orig_type != 0 {
						fsym := c.table.final_sym(var_obj.orig_type)
						if fsym.kind == .interface {
							c.table.used_features.auto_str_ptr = true
						} else if fsym.kind == .array {
							c.table.used_features.auto_str_arr = true
						}
						return
					}
				}
			}
		}
		if arg_typ.is_ptr() {
			c.table.used_features.auto_str_ptr = true
		}
		if !c.table.used_features.auto_str_arr || !c.table.used_features.auto_str_ptr {
			sym := c.table.final_sym(arg_typ)
			if sym.kind == .array {
				c.table.used_features.auto_str_arr = true
			} else if sym.info is ast.Struct {
				if !c.table.used_features.auto_str_ptr {
					c.table.used_features.auto_str_ptr = sym.info.fields.any(it.typ.is_ptr()
						|| it.typ.is_pointer())
				}
				c.table.used_features.auto_str_arr =
					sym.info.fields.any(c.table.final_sym(it.typ).kind == .array)
			}
		}
	}
}

fn (mut c Checker) markused_scalar_ptr_str(typ ast.Type) {
	if c.is_builtin_mod || typ == 0 || !c.table.is_scalar_ptr_type(c.unwrap_generic(typ)) {
		return
	}
	c.mark_type_str_method_as_referenced(ast.voidptr_type)
	c.table.used_features.print_types[ast.voidptr_type_idx] = true
}

fn (mut c Checker) markused_method_call(mut node ast.CallExpr, mut left_expr ast.Expr, left_type ast.Type) {
	if !left_type.has_flag(.generic) && mut left_expr is ast.Ident {
		if left_expr.obj is ast.Var && left_expr.obj.ct_type_var == .smartcast {
			c.table.used_features.comptime_calls['${int(left_type)}.${node.name}'] = true
		}
		if c.table.sym(left_type).kind == .alias {
			c.table.used_features.comptime_calls['${int(left_type)}.${node.name}'] = true
			// Alias method calls can also auto-resolve to pointer receivers in cgen.
			if !left_type.is_ptr() {
				c.table.used_features.comptime_calls['${int(left_type.ref())}.${node.name}'] = true
			}
		}
	} else if !left_type.has_flag(.generic) && left_expr is ast.ComptimeSelector {
		// `x.$(field.name).method()` — the concrete receiver type varies per
		// outer `$for f in T.fields` iteration, and skip-unused has no way to
		// see the runtime dispatch. Mark the per-iteration receiver type so
		// the method survives -skip-unused.
		c.table.used_features.comptime_calls['${int(left_type)}.${node.name}'] = true
		if !left_type.is_ptr() {
			c.table.used_features.comptime_calls['${int(left_type.ref())}.${node.name}'] = true
		}
	} else if left_type.has_flag(.generic) {
		unwrapped_left := c.unwrap_generic(left_type)
		c.table.used_features.comptime_calls['${int(unwrapped_left)}.${node.name}'] = true
		// Generic method calls can resolve to pointer receivers during cgen (`x.name()` -> `(&x).name()`).
		// Mark both forms so skip-unused does not drop pointer receiver methods.
		if !unwrapped_left.is_ptr() {
			c.table.used_features.comptime_calls['${int(unwrapped_left.ref())}.${node.name}'] = true
		}
	}
}

fn string_inter_lit_format_uses_str(fmt u8) bool {
	return fmt == `s`
}

fn (mut c Checker) markused_string_inter_lit(mut _ ast.StringInterLiteral, ftyp ast.Type, fmt u8) {
	if c.is_builtin_mod {
		return
	}
	uses_str_format := string_inter_lit_format_uses_str(fmt)
	if !c.table.sym(ftyp).has_method('str') {
		if uses_str_format {
			c.table.used_features.auto_str = true
			c.markused_auto_str_dependencies(ftyp)
		}
	} else {
		if uses_str_format {
			c.markused_generic_str_method(ftyp, c.table.sym(ftyp))
			c.mark_type_str_method_as_referenced(ftyp)
			c.table.used_features.print_types[ftyp.idx()] = true
		}
	}
	if ftyp.is_ptr() {
		c.table.used_features.auto_str_ptr = true
	}
	if !c.table.used_features.auto_str_arr {
		c.table.used_features.auto_str_arr = c.table.final_sym(ftyp).kind == .array
	}
	if ftyp.has_option_or_result() {
		c.table.used_features.print_options = true
	}
}

fn (mut c Checker) markused_auto_str_dependencies(typ ast.Type) {
	mut visited := map[ast.Type]bool{}
	c.markused_auto_str_dependencies_for_type(typ, mut visited)
}

fn (mut c Checker) markused_auto_str_dependencies_for_type(typ ast.Type, mut visited map[ast.Type]bool) {
	base_typ := c.unwrap_generic(typ.clear_option_and_result().clear_flags(.variadic, .shared_f,
		.atomic_f).clear_ref())
	if base_typ == 0 || visited[base_typ] {
		return
	}
	visited[base_typ] = true
	mut sym := c.table.sym(base_typ)
	has_parent_str := sym.has_method_with_generic_parent('str')
	has_exact_str := sym.has_method('str')
	if has_parent_str || has_exact_str {
		c.markused_generic_str_method(base_typ, sym)
		return
	}
	if sym.info is ast.Alias {
		c.markused_auto_str_dependencies_for_type(sym.info.parent_type.derive(base_typ), mut
			visited)
		return
	}
	sym = c.table.final_sym(base_typ)
	if sym.has_method_with_generic_parent('str') || sym.has_method('str') {
		c.markused_generic_str_method(ast.idx_to_type(sym.idx), sym)
		return
	}
	match sym.info {
		ast.Array {
			c.markused_auto_str_dependencies_for_type(sym.info.elem_type, mut visited)
		}
		ast.ArrayFixed {
			c.markused_auto_str_dependencies_for_type(sym.info.elem_type, mut visited)
		}
		ast.Chan {
			c.markused_auto_str_dependencies_for_type(sym.info.elem_type, mut visited)
		}
		ast.Interface {
			for concrete_type in sym.info.types {
				c.markused_auto_str_dependencies_for_type(concrete_type, mut visited)
			}
		}
		ast.Map {
			c.markused_auto_str_dependencies_for_type(sym.info.key_type, mut visited)
			c.markused_auto_str_dependencies_for_type(sym.info.value_type, mut visited)
		}
		ast.MultiReturn {
			for ret_type in sym.info.types {
				c.markused_auto_str_dependencies_for_type(ret_type, mut visited)
			}
		}
		ast.Struct {
			for field in sym.info.fields {
				if attr := field.attrs.find_first('str') {
					if attr.arg == 'skip' {
						continue
					}
				}
				c.markused_auto_str_dependencies_for_type(field.typ, mut visited)
			}
		}
		ast.SumType {
			for variant in sym.info.variants {
				c.markused_auto_str_dependencies_for_type(variant, mut visited)
			}
		}
		ast.Thread {
			c.markused_auto_str_dependencies_for_type(sym.info.return_type, mut visited)
		}
		else {}
	}
}

fn (mut c Checker) markused_generic_str_method(typ ast.Type, sym &ast.TypeSymbol) {
	mut method := ast.Fn{}
	mut concrete_types := []ast.Type{}
	if structured_method := c.table.find_structured_receiver_method_with_types(typ, 'str') {
		if exact_method := sym.find_method('str') {
			method = exact_method
		} else if exact_method := c.table.find_alias_parent_exact_method(typ, 'str') {
			method = exact_method
		} else {
			method = structured_method.method
			concrete_types = structured_method.concrete_types.map(c.unwrap_generic(it))
		}
	} else {
		method = sym.find_method_with_generic_parent('str') or {
			sym.find_method('str') or { return }
		}
	}
	if concrete_types.len == 0 {
		concrete_types = c.concrete_types_for_generic_str_receiver(typ, sym, method)
	}
	if method.generic_names.len != concrete_types.len || concrete_types.len == 0
		|| concrete_types.any(it.has_flag(.generic) || c.type_has_unresolved_generic_parts(it)) {
		return
	}
	mut registered := c.register_auto_str_fn_concrete_types(method.fkey(), concrete_types)
	decl_fkey := method_decl_fkey(method)
	if decl_fkey != method.fkey() {
		registered = c.register_auto_str_fn_concrete_types(decl_fkey, concrete_types) || registered
	}
	for resolved_decl_fkey in c.str_method_decl_fkeys_for_type(typ, method) {
		if resolved_decl_fkey != method.fkey() && resolved_decl_fkey != decl_fkey {
			registered = c.register_auto_str_fn_concrete_types(resolved_decl_fkey, concrete_types)
				|| registered
		}
	}
	if registered {
		c.need_recheck_generic_fns = true
	}
}

fn (mut c Checker) register_auto_str_fn_concrete_types(fkey string, concrete_types []ast.Type) bool {
	if fkey == '' {
		return false
	}
	if _ := c.table.fn_generic_types[fkey] {
		// The generic function key is already registered.
	} else {
		c.table.register_fn_generic_types(fkey)
	}
	return c.table.register_fn_concrete_types(fkey, concrete_types)
}

fn method_decl_fkey(method ast.Fn) string {
	if method.source_fn != unsafe { nil } {
		fndecl := unsafe { &ast.FnDecl(method.source_fn) }
		return fndecl.fkey()
	}
	return method.fkey()
}

fn (c &Checker) generic_parent_str_method_fkey(sym &ast.TypeSymbol) string {
	match sym.info {
		ast.Struct, ast.Interface, ast.SumType {
			if sym.info.parent_type.has_flag(.generic) {
				parent_sym := c.table.sym(sym.info.parent_type)
				if method := parent_sym.find_method('str') {
					return method_decl_fkey(method)
				}
			}
		}
		ast.GenericInst {
			if sym.info.parent_idx > 0 {
				parent_sym := c.table.sym(ast.idx_to_type(sym.info.parent_idx))
				if method := parent_sym.find_method('str') {
					return method_decl_fkey(method)
				}
			}
		}
		else {}
	}

	return ''
}

fn add_str_method_decl_fkey(mut fkeys []string, fkey string) {
	if fkey != '' && fkey !in fkeys {
		fkeys << fkey
	}
}

fn add_matching_str_method_decl_fkeys(mut fkeys []string, candidate_fkeys []string, resolved_decl_fkey string) {
	if resolved_decl_fkey in candidate_fkeys {
		for fkey in candidate_fkeys {
			add_str_method_decl_fkey(mut fkeys, fkey)
		}
	}
}

fn (c &Checker) str_method_decl_fkeys_for_type(typ ast.Type, method ast.Fn) []string {
	mut fkeys := []string{}
	if typ == 0 {
		return fkeys
	}
	resolved_decl_fkey := method_decl_fkey(method)
	for fkey in c.str_method_decl_fkeys_for_candidate(typ) {
		add_str_method_decl_fkey(mut fkeys, fkey)
	}
	unaliased := c.table.unaliased_type(typ)
	if unaliased != 0 && unaliased != typ {
		add_matching_str_method_decl_fkeys(mut fkeys,
			c.str_method_decl_fkeys_for_candidate(unaliased), resolved_decl_fkey)
	}
	if !typ.is_ptr() {
		ref_typ := typ.ref()
		if ref_typ != 0 {
			add_matching_str_method_decl_fkeys(mut fkeys,
				c.str_method_decl_fkeys_for_candidate(ref_typ), resolved_decl_fkey)
		}
	} else {
		deref_typ := typ.deref()
		if deref_typ != 0 {
			add_matching_str_method_decl_fkeys(mut fkeys,
				c.str_method_decl_fkeys_for_candidate(deref_typ), resolved_decl_fkey)
		}
	}
	return fkeys
}

fn (c &Checker) str_method_decl_fkeys_for_candidate(candidate ast.Type) []string {
	mut fkeys := []string{}
	sym := c.table.sym(candidate)
	parent_fkey := c.generic_parent_str_method_fkey(sym)
	if parent_fkey != '' {
		add_str_method_decl_fkey(mut fkeys, parent_fkey)
	}
	if method := sym.find_method_with_generic_parent('str') {
		add_str_method_decl_fkey(mut fkeys, method_decl_fkey(method))
	}
	if method := c.table.find_method(sym, 'str') {
		add_str_method_decl_fkey(mut fkeys, method_decl_fkey(method))
	}
	method, embed_types := c.table.find_method_from_embeds(c.table.final_sym(candidate), 'str') or {
		ast.Fn{}, []ast.Type{}
	}
	if embed_types.len != 0 {
		add_str_method_decl_fkey(mut fkeys, method_decl_fkey(method))
	}
	return fkeys
}

fn (mut c Checker) concrete_types_for_generic_str_receiver(typ ast.Type, sym &ast.TypeSymbol, method ast.Fn) []ast.Type {
	mut concrete_types := c.concrete_types_for_type_symbol(sym)
	if concrete_types.len == 0 && sym.info is ast.Alias {
		concrete_types = c.alias_parent_concrete_types(sym.info)
	}
	if method.generic_names.len == 0
		|| (concrete_types.len > 0 && concrete_types.len == method.generic_names.len) {
		return concrete_types
	}
	return c.infer_method_receiver_concrete_types(method, typ)
}

fn (c &Checker) alias_parent_concrete_types(info ast.Alias) []ast.Type {
	parent_sym := c.table.sym(info.parent_type)
	match parent_sym.info {
		ast.Struct, ast.SumType, ast.Interface {
			mut concrete_types := parent_sym.info.concrete_types.clone()
			if concrete_types.len == 0
				&& parent_sym.generic_types.len == parent_sym.info.generic_types.len
				&& parent_sym.generic_types != parent_sym.info.generic_types {
				concrete_types = parent_sym.generic_types.clone()
			}
			return concrete_types
		}
		ast.GenericInst {
			return parent_sym.info.concrete_types.clone()
		}
		ast.Array {
			return [parent_sym.info.elem_type]
		}
		ast.ArrayFixed {
			return [parent_sym.info.elem_type]
		}
		ast.Chan {
			return [parent_sym.info.elem_type]
		}
		ast.Map {
			return [parent_sym.info.key_type, parent_sym.info.value_type]
		}
		ast.Alias {
			return c.alias_parent_concrete_types(parent_sym.info)
		}
		else {}
	}

	return []ast.Type{}
}

fn (mut c Checker) infer_method_receiver_concrete_types(method ast.Fn, typ ast.Type) []ast.Type {
	mut concrete_types := []ast.Type{cap: method.generic_names.len}
	for generic_name in method.generic_names {
		mut concrete_type := c.infer_composite_generic_type(generic_name, method.receiver_type, typ)
		if concrete_type == ast.void_type {
			unaliased_receiver_type := c.table.fully_unaliased_type(method.receiver_type)
			unaliased_typ := c.table.fully_unaliased_type(typ)
			if unaliased_receiver_type != method.receiver_type || unaliased_typ != typ {
				concrete_type = c.infer_composite_generic_type(generic_name,
					unaliased_receiver_type, unaliased_typ)
			}
		}
		if concrete_type == ast.void_type {
			return []ast.Type{}
		}
		concrete_types << c.unwrap_generic(concrete_type)
	}
	return concrete_types
}

fn (mut c Checker) markused_array_method(check bool, method_name string) {
	if !check {
		return
	}
	match method_name {
		'' { // array init
		}
		'first' {
			c.table.used_features.arr_first = true
		}
		'last' {
			c.table.used_features.arr_last = true
		}
		'pop_left' {
			c.table.used_features.arr_pop_left = true
		}
		'pop' {
			c.table.used_features.arr_pop = true
		}
		'delete' {
			c.table.used_features.arr_delete = true
		}
		'map' {
			c.table.used_features.arr_map = true
		}
		else {}
	}
}
