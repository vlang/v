// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import v2.ast
import v2.types

fn (mut g Gen) gen_assign_stmt(node ast.AssignStmt) {
	lhs := node.lhs[0]
	rhs := node.rhs[0]

	// Multi-assignment with parallel RHS values (non-declaration):
	// `p, q = q, p` needs temp variables for correct swap semantics.
	if node.op != .decl_assign && node.lhs.len > 1 && node.rhs.len == node.lhs.len {
		// First, evaluate all RHS values into temporaries
		for i, rhs_expr in node.rhs {
			mut typ := g.get_expr_type(rhs_expr)
			if typ == '' || typ == 'int_literal' {
				typ = 'int'
			}
			if typ == 'float_literal' {
				typ = 'f64'
			}
			g.write_indent()
			g.sb.write_string('${typ} _swap_${g.tmp_counter}_${i} = ')
			g.expr(rhs_expr)
			g.sb.writeln(';')
		}
		// Then assign from temporaries to LHS
		for i, lhs_expr in node.lhs {
			g.write_indent()
			g.expr(lhs_expr)
			g.sb.writeln(' = _swap_${g.tmp_counter}_${i};')
		}
		g.tmp_counter++
		return
	}

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
				g.expr(rhs_expr)
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
			g.expr(rhs_expr)
			g.sb.writeln(';')
			g.remember_runtime_local_type(name, typ)
		}
		return
	}

	mut tuple_lhs := []ast.Expr{}
	if node.lhs.len > 1 {
		tuple_lhs = shallow_copy_exprs(node.lhs)
	} else if node.lhs.len == 1 && node.lhs[0] is ast.Tuple {
		lhs_tuple := node.lhs[0] as ast.Tuple
		tuple_lhs = shallow_copy_exprs(lhs_tuple.exprs)
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
		} else if rhs is ast.UnsafeExpr {
			// Handle inline or-block expansion: the transformer wraps or-blocks in
			// UnsafeExpr (GCC statement expressions) when it can't expand them to
			// separate statements. The stmts are: [_or_t := call(), if error, _or_t.data]
			// Extract the call return type from the first AssignStmt inside.
			for stmt_inner in rhs.stmts {
				if stmt_inner is ast.AssignStmt && stmt_inner.op == .decl_assign
					&& stmt_inner.rhs.len == 1 {
					inner_rhs := stmt_inner.rhs[0]
					mut inner_ret := ''
					if inner_rhs is ast.CallExpr {
						if ret := g.get_call_return_type(inner_rhs.lhs, inner_rhs.args.len) {
							inner_ret = ret
						}
					}
					// Strip _option_/_result_ wrapper since the UnsafeExpr already
					// unwraps the value (last expr is _or_t.data)
					if inner_ret.starts_with('_option_') {
						tuple_type = option_value_type(inner_ret)
					} else if inner_ret.starts_with('_result_') {
						tuple_type = g.result_value_type(inner_ret)
					}
					break
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
				g.expr(rhs)
				g.sb.writeln(';')
				g.write_indent()
				g.sb.writeln('${tuple_type} ${tmp_name} = (*(${tuple_type}*)(((u8*)(&${res_tmp}.err)) + sizeof(IError)));')
			} else {
				g.sb.write_string('${tuple_type} ${tmp_name} = ')
				g.expr(rhs)
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
					g.expr(lhs_expr)
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
		g.expr(rhs)
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
		// Rename V variables that clash with C type names
		if name == 'array' {
			name = '_v_array'
		}
		// Keep fixed-size arrays as C arrays in local declarations.
		if rhs is ast.ArrayInitExpr {
			array_init := rhs as ast.ArrayInitExpr
			if array_init.typ is ast.Type && array_init.typ is ast.ArrayFixedType {
				fixed_typ := array_init.typ as ast.ArrayFixedType
				elem_type := g.expr_type_to_c(fixed_typ.elem_type)
				mut fixed_arr_size := array_init.exprs.len
				if fixed_arr_size == 0 {
					// For init-based fixed arrays, get size from the type annotation
					if fixed_typ.len is ast.BasicLiteral && fixed_typ.len.kind == .number {
						fixed_arr_size = fixed_typ.len.value.int()
					}
				}
				fixed_name := 'Array_fixed_' + mangle_alias_component(elem_type) + '_' +
					fixed_arr_size.str()
				g.remember_runtime_local_type(name, fixed_name)
				is_literal_size := fixed_typ.len is ast.BasicLiteral
					&& (fixed_typ.len as ast.BasicLiteral).kind == .number
				g.sb.write_string('${elem_type} ${name}[')
				g.expr(fixed_typ.len)
				if array_init.exprs.len == 0 {
					if array_init.init !is ast.EmptyExpr && is_literal_size {
						// Has init: clause - expand to repeated init values
						g.sb.write_string('] = ')
						g.gen_array_init_expr(array_init)
						g.sb.writeln(';')
					} else if is_literal_size {
						g.sb.writeln('] = {0};')
					} else {
						// Non-literal sizes are VLAs in C99 and cannot use = {0}
						g.sb.writeln('];')
						g.sb.writeln('memset(${name}, 0, sizeof(${name}));')
					}
				} else {
					g.sb.write_string('] = {')
					for i, expr in array_init.exprs {
						if i > 0 {
							g.sb.write_string(', ')
						}
						g.expr(expr)
					}
					g.sb.writeln('};')
				}
				return
			}
		}
		mut typ := g.get_expr_type(rhs)
		// For temp variables registered by the transformer with a specific type,
		// prefer the scope-registered type over the RHS expression type.
		if name.starts_with('_or_t') || name.starts_with('_tmp_') {
			if raw_type := g.get_raw_type(lhs) {
				scope_type := g.types_type_to_c(raw_type)
				if scope_type != '' && scope_type != 'int' {
					typ = scope_type
				} else if scope_type == 'int' && typ == 'bool' {
					// Fix: literal like `1` mistyped as bool in env
					typ = 'int'
				}
			}
		}
		// Fix: &T(x) pattern - the checker may assign only the inner type T instead of T*.
		// Derive the pointer type directly from the expression structure.
		if rhs is ast.PrefixExpr && rhs.op == .amp && rhs.expr is ast.CastExpr {
			target_type := g.expr_type_to_c(rhs.expr.typ)
			if target_type != '' {
				typ = target_type + '*'
			}
		}
		mut elem_type_from_array := false
		if rhs is ast.CallExpr {
			if rhs.lhs is ast.Ident
				&& rhs.lhs.name in ['array__pop', 'array__pop_left', 'array__first', 'array__last'] {
				if rhs.args.len > 0 {
					pop_elem := g.infer_array_elem_type_from_expr(rhs.args[0])
					if pop_elem != '' {
						typ = pop_elem
						elem_type_from_array = true
					}
				}
			} else if rhs.lhs is ast.SelectorExpr
				&& rhs.lhs.rhs.name in ['pop', 'pop_left', 'first', 'last'] {
				arr_expr := if rhs.args.len > 0 { rhs.args[0] } else { rhs.lhs.lhs }
				pop_elem := g.infer_array_elem_type_from_expr(arr_expr)
				if pop_elem != '' {
					typ = pop_elem
					elem_type_from_array = true
				}
			}
		}
		if typ in ['void*', 'voidptr'] {
			// Handle array__first/array__last/pop/pop_left for element type inference
			if rhs is ast.CallExpr {
				if rhs.lhs is ast.SelectorExpr {
					if rhs.lhs.rhs.name in ['first', 'last', 'pop', 'pop_left'] {
						arr2 := if rhs.args.len > 0 { rhs.args[0] } else { rhs.lhs.lhs }
						elem := g.infer_array_elem_type_from_expr(arr2)
						if elem != '' {
							typ = elem
						}
					}
				}
			}
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
			}
			if has_arr_expr
				&& call_name in ['array__pop', 'array__pop_left', 'array__first', 'array__last'] {
				pop_elem := g.infer_array_elem_type_from_expr(arr_expr)
				if pop_elem != '' {
					typ = pop_elem
				}
			}
		}
		// Check scope-resolved type first (most reliable for declarations).
		// Treat `void*`/`voidptr` as an "unknown" fallback when we have a concrete
		// type from the (checker/transformer) scope; this is important for patterns
		// like `tmp := map__get_check(...)` where the C builtin returns `void*` but
		// the variable is known to be `T*`.
		// But skip if element type was already inferred from array methods
		if !elem_type_from_array && name != '' && g.cur_fn_scope != unsafe { nil } {
			if obj := g.cur_fn_scope.lookup_parent(name, 0) {
				if obj !is types.Module {
					obj_type := obj.typ()
					if obj_type !is types.Alias {
						scoped_type := g.types_type_to_c(obj_type)
						if (typ == '' || typ == 'int' || typ == 'int_literal' || typ == 'void*'
							|| typ == 'voidptr') && scoped_type != ''
							&& scoped_type !in ['int', 'void', 'void*', 'voidptr'] {
							typ = scoped_type
						}
					}
				}
			}
		}
		// For decl_assign (:=), the LHS is a brand new variable, so don't use
		// cached types from runtime_local_types (which may hold stale types
		// from a previous variable with the same name, e.g. _filter_it).
		if node.op != .decl_assign {
			lhs_typ := g.get_expr_type(lhs)
			if lhs_typ != '' && lhs_typ !in ['int', 'int_literal', 'float_literal']
				&& lhs_typ != 'void'
				&& (typ == '' || typ == 'int' || typ == 'void*' || typ == 'voidptr') {
				typ = lhs_typ
			}
		}
		if !elem_type_from_array && rhs is ast.CallExpr {
			if ret := g.get_call_return_type(rhs.lhs, rhs.args.len) {
				if ret != ''
					&& (ret != 'int' || typ in ['', 'void*', 'voidptr'] || typ.starts_with('Array_')
					|| typ.starts_with('Map_')) {
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
				g.expr(rhs.lhs)
				g.sb.writeln('.err)) + sizeof(IError)));')
				g.remember_runtime_local_type(name, cast_type)
				return
			}
		}
		if !elem_type_from_array && (typ == '' || typ == 'int'
			|| typ == 'int_literal' || typ == 'void*' || typ == 'voidptr') && rhs_type != ''
			&& rhs_type !in ['int', 'int_literal', 'float_literal']
			&& !rhs_type.starts_with('_result_') && !rhs_type.starts_with('_option_') {
			typ = rhs_type
		}
		if name != '' && rhs_type.starts_with('_result_') && !typ.starts_with('_result_') {
			g.sb.write_string('${typ} ${name} = ({ ${rhs_type} _tmp = ')
			g.expr(rhs)
			g.sb.writeln('; (*(${typ}*)(((u8*)(&_tmp.err)) + sizeof(IError))); });')
			g.remember_runtime_local_type(name, typ)
			return
		}
		if name != '' && rhs_type.starts_with('_option_') && !typ.starts_with('_option_') {
			g.sb.write_string('${typ} ${name} = ({ ${rhs_type} _tmp = ')
			g.expr(rhs)
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
			g.expr(rhs.expr)
			g.sb.writeln('));')
			return
		}
		if typ.ends_with('*') && rhs is ast.PrefixExpr && rhs.op == .amp {
			if rhs.expr is ast.CallExpr && rhs.expr.args.len == 1 {
				g.sb.write_string('${typ} ${name} = ((${typ})(')
				g.expr(rhs.expr.args[0])
				g.sb.writeln('));')
				return
			}
			if rhs.expr is ast.CastExpr {
				g.sb.write_string('${typ} ${name} = ((${typ})(')
				g.expr(rhs.expr.expr)
				g.sb.writeln('));')
				return
			}
			if rhs.expr is ast.ParenExpr {
				if rhs.expr.expr is ast.CallExpr && rhs.expr.expr.args.len == 1 {
					g.sb.write_string('${typ} ${name} = ((${typ})(')
					g.expr(rhs.expr.expr.args[0])
					g.sb.writeln('));')
					return
				}
				if rhs.expr.expr is ast.CastExpr {
					g.sb.write_string('${typ} ${name} = ((${typ})(')
					g.expr(rhs.expr.expr.expr)
					g.sb.writeln('));')
					return
				}
			}
		}
		// FnLiteral: generate proper function pointer declaration
		if rhs is ast.FnLiteral {
			ret_type := if rhs.typ.return_type !is ast.EmptyExpr {
				g.expr_type_to_c(rhs.typ.return_type)
			} else {
				'void'
			}
			mut params_str := ''
			for i, param in rhs.typ.params {
				if i > 0 {
					params_str += ', '
				}
				params_str += g.expr_type_to_c(param.typ)
			}
			if params_str == '' {
				params_str = 'void'
			}
			g.sb.write_string('${ret_type} (*${name})(${params_str}) = ')
			g.expr(rhs)
			g.sb.writeln(';')
			// Register the return type so map/filter can infer result type
			g.fn_return_types[name] = ret_type
			g.remember_runtime_local_type(name, 'fn_ptr')
			return
		}
		if typ == '' || typ == 'void' {
			typ = 'int'
		}
		g.sb.write_string('${typ} ${name} = ')
		g.expr(rhs)
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
					g.expr(rhs)
					g.sb.writeln(';')
					g.write_indent()
					g.sb.write_string('array__push_many((array*)')
					if g.expr_is_pointer(lhs) {
						g.expr(lhs)
					} else {
						g.sb.write_string('&')
						g.expr(lhs)
					}
					g.sb.writeln(', ${rhs_tmp}.data, ${rhs_tmp}.len);')
					return
				}
				g.write_indent()
				g.sb.write_string('array__push((array*)')
				if g.expr_is_pointer(lhs) {
					g.expr(lhs)
				} else {
					g.sb.write_string('&')
					g.expr(lhs)
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
			mut rhs_array_elem_type := g.infer_array_method_elem_type(rhs)
			if rhs_array_elem_type != '' && decl_type in ['void*', 'voidptr'] {
				decl_type = rhs_array_elem_type
			}
			if decl_type == '' || decl_type in ['int_literal', 'float_literal'] {
				decl_type = 'int'
			}
			if decl_type in ['void', 'void*', 'voidptr'] {
				decl_type = 'int'
			}
			g.write_indent()
			g.sb.write_string('${decl_type} ${lhs.name} = ')
			if rhs_array_elem_type != '' && decl_type !in ['void*', 'voidptr']
				&& !decl_type.ends_with('*') {
				g.sb.write_string('(*(${decl_type}*)')
				g.expr(rhs)
				g.sb.write_string(')')
			} else {
				g.expr(rhs)
			}
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
					g.expr(lhs.lhs)
					g.sb.write_string('.err)) + sizeof(IError))) = ')
					g.expr(rhs)
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
					} else if !rhs_is_ptr && rhs !is ast.CallExpr && rhs !is ast.UnsafeExpr {
						lhs_needs_deref = true
					}
				}
			}
		}
		if lhs_needs_deref {
			g.sb.write_string('*')
		}
		g.expr(lhs)
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
		mut rhs_array_elem_type := g.infer_array_method_elem_type(rhs)
		mut assign_lhs_type := g.get_expr_type(lhs)
		if lhs is ast.Ident {
			if local_type := g.get_local_var_c_type(lhs.name) {
				assign_lhs_type = local_type
			}
		}
		if rhs_array_elem_type != '' && assign_lhs_type !in ['', 'void*', 'voidptr']
			&& !assign_lhs_type.ends_with('*') {
			g.sb.write_string('(*(${assign_lhs_type}*)')
			g.expr(rhs)
			g.sb.write_string(')')
		} else {
			g.expr(rhs)
		}
		g.sb.writeln(';')
	}
}
