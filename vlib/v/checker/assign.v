// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast
import v.pref

// TODO 600 line function
fn (mut c Checker) assign_stmt(mut node ast.AssignStmt) {
	prev_inside_assign := c.inside_assign
	c.inside_assign = true
	c.expected_type = ast.none_type // TODO a hack to make `x := if ... work`
	defer {
		c.expected_type = ast.void_type
		c.inside_assign = prev_inside_assign
	}
	is_decl := node.op == .decl_assign
	mut right_first := node.right[0]
	node.left_types = []
	mut right_len := node.right.len
	mut right_first_type := ast.void_type
	for i, mut right in node.right {
		if right in [ast.CallExpr, ast.IfExpr, ast.LockExpr, ast.MatchExpr, ast.DumpExpr,
			ast.SelectorExpr, ast.ParExpr] {
			if right in [ast.IfExpr, ast.MatchExpr] && node.left.len == node.right.len && !is_decl
				&& node.left[i] in [ast.Ident, ast.SelectorExpr] && !node.left[i].is_blank_ident() {
				mut expr := node.left[i]
				c.expected_type = c.expr(mut expr)
			}
			mut right_type := c.expr(mut right)
			if right in [ast.CallExpr, ast.IfExpr, ast.LockExpr, ast.MatchExpr, ast.DumpExpr] {
				c.fail_if_unreadable(right, right_type, 'right-hand side of assignment')
			}
			right_type_sym := c.table.sym(right_type)
			// fixed array returns an struct, but when assigning it must be the array type
			right_type = c.cast_fixed_array_ret(right_type, right_type_sym)
			if i == 0 {
				right_first_type = right_type
				node.right_types = [
					c.check_expr_option_or_result_call(right, right_first_type),
				]
			}
			if right_type_sym.kind == .multi_return {
				if node.right.len > 1 {
					c.error('cannot use multi-value ${right_type_sym.name} in single-value context',
						right.pos())
				}
				node.right_types = right_type_sym.mr_info().types
				right_len = node.right_types.len
			} else if right_type == ast.void_type {
				right_len = 0
			}
		}
		if mut right is ast.InfixExpr {
			if right.op == .arrow {
				c.error('cannot use `<-` on the right-hand side of an assignment, as it does not return any values',
					right.pos)
			}
		}
		if mut right is ast.Ident {
			if right.is_mut {
				c.error('unexpected `mut` on right-hand side of assignment', right.mut_pos)
			}
		}
		if is_decl && mut right is ast.None {
			c.error('cannot assign a `none` value to a variable', right.pos)
		}
		// Handle `left_name := unsafe { none }`
		if mut right is ast.UnsafeExpr {
			if mut right.expr is ast.None {
				c.error('cannot use `none` in `unsafe` blocks', right.expr.pos)
			}
		}
		if mut right is ast.AnonFn {
			if right.decl.generic_names.len > 0 {
				c.error('cannot assign generic function to a variable', right.decl.pos)
			}
		}
	}
	if node.left.len != right_len {
		if mut right_first is ast.CallExpr {
			if node.left_types.len > 0 && node.left_types[0] == ast.void_type {
				// If it's a void type, it's an unknown variable, already had an error earlier.
				return
			}
			c.error('assignment mismatch: ${node.left.len} variable(s) but `${right_first.get_name()}()` returns ${right_len} value(s)',
				node.pos)
		} else if mut right_first is ast.ParExpr {
			mut right_next := right_first
			for {
				if mut right_next.expr is ast.CallExpr {
					if right_next.expr.return_type == ast.void_type {
						c.error('assignment mismatch: expected ${node.left.len} value(s) but `${right_next.expr.get_name()}()` returns ${right_len} value(s)',
							node.pos)
					}
					break
				} else if mut right_next.expr is ast.ParExpr {
					right_next = right_next.expr
				} else {
					break
				}
			}
		} else {
			c.error('assignment mismatch: ${node.left.len} variable(s) ${right_len} value(s)',
				node.pos)
		}
		return
	}

	for i, mut left in node.left {
		if mut left is ast.CallExpr {
			// ban `foo() = 10`
			c.error('cannot call function `${left.name}()` on the left side of an assignment',
				left.pos)
		} else if mut left is ast.PrefixExpr {
			// ban `*foo() = 10`
			if left.right is ast.CallExpr && left.op == .mul {
				c.error('cannot dereference a function call on the left side of an assignment, use a temporary variable',
					left.pos)
			}
		} else if mut left is ast.IndexExpr {
			if left.index is ast.RangeExpr {
				c.error('cannot reassign using range expression on the left side of an assignment',
					left.pos)
			}
		}
		is_blank_ident := left.is_blank_ident()
		mut left_type := ast.void_type
		mut var_option := false
		mut is_shared_re_assign := false
		if !is_decl && !is_blank_ident {
			if left in [ast.Ident, ast.SelectorExpr] {
				c.prevent_sum_type_unwrapping_once = true
			}
			if left is ast.IndexExpr {
				c.is_index_assign = true
			}
			left_type = c.expr(mut left)
			c.is_index_assign = false
			c.expected_type = c.unwrap_generic(left_type)
			is_shared_re_assign = left is ast.Ident && left.info is ast.IdentVar
				&& ((left.info as ast.IdentVar).share == .shared_t || left_type.has_flag(.shared_f))
				&& c.table.sym(left_type).kind in [.array, .map, .struct_]
		}
		if c.comptime.comptime_for_field_var != '' && mut left is ast.ComptimeSelector {
			left_type = c.comptime.comptime_for_field_type
			c.expected_type = c.unwrap_generic(left_type)
		}
		if node.right_types.len < node.left.len { // first type or multi return types added above
			old_inside_ref_lit := c.inside_ref_lit
			if mut left is ast.Ident {
				if mut left.info is ast.IdentVar {
					c.inside_ref_lit = c.inside_ref_lit || left.info.share == .shared_t
				}
			}
			c.inside_decl_rhs = is_decl
			mut expr := node.right[i]
			right_type := c.expr(mut expr)
			c.inside_decl_rhs = false
			c.inside_ref_lit = old_inside_ref_lit
			if node.right_types.len == i {
				node.right_types << c.check_expr_option_or_result_call(node.right[i],
					right_type)
			}
		}
		mut right := if i < node.right.len { node.right[i] } else { node.right[0] }
		mut right_type := node.right_types[i]
		if mut right is ast.Ident {
			// resolve shared right variable
			if right_type.has_flag(.shared_f) {
				if c.fail_if_unreadable(right, right_type, 'right-hand side of assignment') {
					return
				}
			}
			right_sym := c.table.sym(right_type)
			if right_sym.info is ast.Struct {
				if right_sym.info.generic_types.len > 0 {
					if obj := right.scope.find(right.name) {
						right_type = obj.typ
					}
				}
			}
			if right.or_expr.kind in [.propagate_option, .block] {
				right_type = right_type.clear_flag(.option)
			}
		} else if right is ast.ComptimeSelector {
			right_type = c.comptime.comptime_for_field_type
		}
		if is_decl || is_shared_re_assign {
			// check generic struct init and return unwrap generic struct type
			if mut right is ast.StructInit {
				if right.typ.has_flag(.generic) {
					c.expr(mut right)
					right_type = right.typ
				}
			} else if mut right is ast.PrefixExpr {
				if right.op == .amp && right.right is ast.StructInit {
					right_type = c.expr(mut right)
				} else if right.op == .arrow {
					right_type = c.expr(mut right)
					right_type = c.cast_fixed_array_ret(right_type, c.table.sym(right_type))
				}
			} else if mut right is ast.Ident {
				if right.kind == .function {
					c.expr(mut right)
				}
			}
			if right.is_auto_deref_var() {
				left_type = ast.mktyp(right_type.deref())
			} else {
				left_type = ast.mktyp(right_type)
			}
			if left_type == ast.int_type {
				if mut right is ast.IntegerLiteral {
					mut is_large := right.val.len > 13
					if !is_large && right.val.len > 8 {
						val := right.val.i64()
						is_large = val > int_max || val < int_min
					}
					if is_large {
						c.error('overflow in implicit type `int`, use explicit type casting instead',
							right.pos)
					}
				}
			}
			if mut left is ast.Ident && mut right is ast.Ident {
				if !c.inside_unsafe && left_type.is_ptr() && left.is_mut() && right_type.is_ptr()
					&& !right.is_mut() {
					c.error('`${right.name}` is immutable, cannot have a mutable reference to an immutable object',
						right.pos)
				}
			}
		} else {
			// Make sure the variable is mutable
			c.fail_if_immutable(mut left)

			if !is_blank_ident && !left_type.has_flag(.option) && right_type.has_flag(.option) {
				c.error('cannot assign an Option value to a non-option variable', right.pos())
			}
			// left_type = c.expr(left)
			// if right is ast.None && !left_type.has_flag(.option) {
			// 	println(left_type)
			// 	c.error('cannot assign a `none` value to a non-option variable', right.pos())
			// }
		}
		if mut left is ast.Ident && left.info is ast.IdentVar && right is ast.Ident
			&& right.name in c.global_names {
			ident_var_info := left.info as ast.IdentVar
			if ident_var_info.share == .shared_t {
				c.error('cannot assign global variable to shared variable', right.pos())
			}
		}
		if right_type.is_ptr() && left_type.is_ptr() {
			if mut right is ast.Ident {
				c.fail_if_stack_struct_action_outside_unsafe(mut right, 'assigned')
			}
		}
		// Do not allow `a := 0; b := 0; a = &b`
		if !is_decl && left is ast.Ident && !is_blank_ident && !left_type.is_any_kind_of_pointer()
			&& right_type.is_any_kind_of_pointer() && !right_type.has_flag(.shared_f) {
			left_sym := c.table.sym(left_type)
			if left_sym.kind !in [.function, .array] {
				c.warn(
					'cannot assign a reference to a value (this will be an error soon) left=${c.table.type_str(left_type)} ${left_type.is_ptr()} ' +
					'right=${c.table.type_str(right_type)} ${right_type.is_any_kind_of_pointer()} ptr=${right_type.is_ptr()}',
					node.pos)
			}
		}
		node.left_types << left_type
		for left is ast.ParExpr {
			left = (left as ast.ParExpr).expr
		}
		match mut left {
			ast.Ident {
				if (is_decl || left.kind == .blank_ident) && left_type.is_ptr()
					&& mut right is ast.PrefixExpr && right.right_type == ast.int_literal_type_idx {
					if mut right.right is ast.Ident && right.right.obj is ast.ConstField {
						const_name := right.right.name.all_after_last('.')
						const_val := (right.right.obj as ast.ConstField).expr
						c.add_error_detail('Specify the type for the constant value. Example:')
						c.add_error_detail('         `const ${const_name} = int(${const_val})`')
						c.error('cannot assign a pointer to a constant with an integer literal value',
							right.right.pos)
					}
				} else if left.kind == .blank_ident {
					left_type = right_type
					node.left_types[i] = right_type
					if node.op !in [.assign, .decl_assign] {
						c.error('cannot modify blank `_` identifier', left.pos)
					}
				} else if left.info !is ast.IdentVar {
					c.error('cannot assign to ${left.kind} `${left.name}`', left.pos)
				} else {
					if is_decl {
						c.check_valid_snake_case(left.name, 'variable name', left.pos)
						if reserved_type_names_chk.matches(left.name) {
							c.error('invalid use of reserved type `${left.name}` as a variable name',
								left.pos)
						}
					}
					if (is_decl || is_shared_re_assign) && right is ast.Nil && !c.inside_unsafe {
						// `x := unsafe { nil }` is allowed,
						// as well as:
						// `unsafe {
						//    x := nil
						//    println(x)
						// }`
						c.error('use of untyped nil in assignment (use `unsafe` | ${c.inside_unsafe})',
							right.pos())
					}
					mut ident_var_info := left.info as ast.IdentVar
					if ident_var_info.share == .shared_t || is_shared_re_assign {
						left_type = left_type.set_flag(.shared_f)
						if is_decl || is_shared_re_assign {
							if left_type.nr_muls() > 1 {
								c.error('shared cannot be multi level reference', left.pos)
							}
							left_type = left_type.set_nr_muls(1)
						}
					} else if left_type.has_flag(.shared_f) {
						left_type = left_type.clear_flag(.shared_f).deref()
					}
					if ident_var_info.share == .atomic_t {
						left_type = left_type.set_flag(.atomic_f)
					}
					if ident_var_info.is_option {
						var_option = true
					}
					node.left_types[i] = left_type
					ident_var_info.typ = left_type
					left.info = ident_var_info
					if left_type != 0 {
						match mut left.obj {
							ast.Var {
								left.obj.typ = left_type
								if left.obj.is_auto_deref {
									left.obj.is_used = true
								}
								if !left_type.is_ptr() {
									if c.table.sym(left_type).is_heap() {
										left.obj.is_auto_heap = true
									}
								}
								if left_type in ast.unsigned_integer_type_idxs {
									if mut right is ast.IntegerLiteral {
										if right.val[0] == `-` {
											c.error('cannot assign negative value to unsigned integer type',
												right.pos)
										}
									}
								}
								if right is ast.ComptimeSelector {
									if is_decl {
										left.obj.ct_type_var = .field_var
										left.obj.typ = c.comptime.comptime_for_field_type
									}
								} else if mut right is ast.Ident && right.obj is ast.Var
									&& right.or_expr.kind == .absent {
									if (right.obj as ast.Var).ct_type_var != .no_comptime {
										ctyp := c.comptime.get_comptime_var_type(right)
										if ctyp != ast.void_type {
											left.obj.ct_type_var = (right.obj as ast.Var).ct_type_var
											left.obj.typ = ctyp
										}
									}
								} else if right is ast.DumpExpr
									&& right.expr is ast.ComptimeSelector {
									left.obj.ct_type_var = .field_var
									left.obj.typ = c.comptime.comptime_for_field_type
								}
							}
							ast.GlobalField {
								left.obj.typ = left_type
							}
							else {}
						}
					}
					if is_decl {
						full_name := '${left.mod}.${left.name}'
						if obj := c.file.global_scope.find(full_name) {
							if obj is ast.ConstField {
								c.warn('duplicate of a const name `${full_name}`', left.pos)
							}
						}
						if left.name == left.mod && left.name != 'main' {
							c.error('duplicate of a module name `${left.name}`', left.pos)
						}
						// Check if variable name is already registered as imported module symbol
						if c.check_import_sym_conflict(left.name) {
							c.error('duplicate of an import symbol `${left.name}`', left.pos)
						}
					}
					if node.op == .assign && left_type.has_flag(.option) && right is ast.UnsafeExpr
						&& right.expr.is_nil() {
						c.error('cannot assign `nil` to option value', right.pos())
					}
				}
			}
			ast.PrefixExpr {
				// Do now allow `*x = y` outside `unsafe`
				if left.op == .mul {
					if !c.inside_unsafe && !c.pref.translated && !c.file.is_translated {
						c.error('modifying variables via dereferencing can only be done in `unsafe` blocks',
							node.pos)
					} else if mut left.right is ast.Ident {
						// mark `p` in `*p = val` as used:
						if mut left.right.obj is ast.Var {
							left.right.obj.is_used = true
						}
					}
				} else if left.op == .amp {
					c.error('cannot use a reference on the left side of `${node.op}`',
						left.pos)
				} else {
					c.error('cannot use `${left.op}` on the left of `${node.op}`', left.pos)
				}
				if is_decl {
					c.error('non-name on the left side of `:=`', left.pos)
				}
			}
			ast.SelectorExpr {
				if mut left.expr is ast.IndexExpr {
					if left.expr.is_map {
						left.expr.is_setter = true
					}
				}
				if left_type in ast.unsigned_integer_type_idxs {
					if mut right is ast.IntegerLiteral {
						if right.val[0] == `-` {
							c.error('cannot assign negative value to unsigned integer type',
								right.pos)
						}
					}
				}
				if left_type.has_flag(.option) && right is ast.UnsafeExpr && right.expr.is_nil() {
					c.error('cannot assign `nil` to option value', right.pos())
				}
			}
			else {
				if mut left is ast.IndexExpr {
					// eprintln('>>> left.is_setter: ${left.is_setter:10} | left.is_map: ${left.is_map:10} | left.is_array: ${left.is_array:10}')
					if left.is_map && left.is_setter {
						left.recursive_mapset_is_setter(true)
					}
				}
				if is_decl {
					c.error('non-name `${left}` on left side of `:=`', left.pos())
				}

				if node.op == .assign && (left.is_literal() || left is ast.StructInit) {
					c.error('non-name literal value `${left}` on left side of `=`', left.pos())
				}
			}
		}
		left_type_unwrapped := c.unwrap_generic(ast.mktyp(left_type))
		right_type_unwrapped := c.unwrap_generic(right_type)
		if right_type_unwrapped == 0 {
			// right type was a generic `T`
			continue
		}
		if c.pref.translated || c.file.is_translated {
			// TODO fix this in C2V instead, for example cast enums to int before using `|` on them.
			// TODO replace all c.pref.translated checks with `$if !translated` for performance
			continue
		}
		if left_type_unwrapped == 0 {
			continue
		}
		left_sym := c.table.sym(left_type_unwrapped)
		right_sym := c.table.sym(right_type_unwrapped)

		old_assign_error_condition := left_sym.kind == .array && !c.inside_unsafe
			&& node.op in [.assign, .decl_assign] && right_sym.kind == .array && left is ast.Ident
			&& !left.is_blank_ident() && right is ast.Ident
		if old_assign_error_condition {
			// Do not allow `a = b`, only `a = b.clone()`
			c.error('use `array2 ${node.op.str()} array1.clone()` instead of `array2 ${node.op.str()} array1` (or use `unsafe`)',
				node.pos)
		}
		// Do not allow `a = val.array_field`, only `a = val.array_field.clone()`
		// TODO: turn this warning into an error after 2022/09/24
		// TODO: and remove the less strict check from above.
		if left_sym.kind == .array && !c.inside_unsafe && right_sym.kind == .array
			&& left is ast.Ident && !left.is_blank_ident() && right in [ast.Ident, ast.SelectorExpr]
			&& ((node.op == .decl_assign && left.is_mut) || node.op == .assign) {
			// no point to show the notice, if the old error was already shown:
			if !old_assign_error_condition {
				mut_str := if node.op == .decl_assign { 'mut ' } else { '' }
				c.warn('use `${mut_str}array2 ${node.op.str()} array1.clone()` instead of `${mut_str}array2 ${node.op.str()} array1` (or use `unsafe`)',
					node.pos)
			}
		}
		if left_sym.kind == .array && right_sym.kind == .array {
			right_info := right_sym.info as ast.Array
			right_elem_type := c.table.unaliased_type(right_info.elem_type)
			if node.op in [.decl_assign, .assign] {
				// Do not allow `mut arr := [&immutable_object]`
				if mut left is ast.Ident && right_elem_type.is_ptr() {
					if left.is_mut() || (left.obj is ast.Var && left.obj.is_mut) {
						if mut right is ast.ArrayInit && right.exprs.len > 0 {
							elem_expr := right.exprs[0]
							if elem_expr is ast.PrefixExpr && elem_expr.op == .amp {
								r := elem_expr.right
								if r is ast.Ident {
									obj := r.obj
									if obj is ast.Var && !obj.is_mut {
										c.warn('cannot add a reference to an immutable object to a mutable array',
											elem_expr.pos)
									}
								}
							}
						}
					}
				} else if mut left is ast.Ident && left.kind != .blank_ident
					&& right is ast.IndexExpr {
					if (right as ast.IndexExpr).left is ast.Ident
						&& (right as ast.IndexExpr).index is ast.RangeExpr
						&& ((right as ast.IndexExpr).left.is_mut() || left.is_mut())
						&& !c.inside_unsafe {
						// `mut a := arr[..]` auto add clone() -> `mut a := arr[..].clone()`
						c.add_error_detail_with_pos('To silence this notice, use either an explicit `a[..].clone()`,
or use an explicit `unsafe{ a[..] }`, if you do not want a copy of the slice.',
							right.pos())
						c.note('an implicit clone of the slice was done here', right.pos())
						right = ast.CallExpr{
							name: 'clone'
							left: right
							left_type: left_type
							is_method: true
							receiver_type: left_type
							return_type: left_type
							scope: c.fn_scope
						}
						right_type = c.expr(mut right)
						node.right[i] = right
					}
				}
			}
			if node.op == .assign {
				// `mut arr := [u8(1),2,3]`
				// `arr = [u8(4),5,6]`
				left_info := left_sym.info as ast.Array
				left_elem_type := c.table.unaliased_type(left_info.elem_type)
				if left_type_unwrapped.nr_muls() == right_type_unwrapped.nr_muls()
					&& left_info.nr_dims == right_info.nr_dims && left_elem_type == right_elem_type {
					continue
				}
			}
		}
		if left_sym.kind == .array_fixed && !c.inside_unsafe && node.op in [.assign, .decl_assign]
			&& right_sym.kind == .array_fixed && left is ast.Ident && !left.is_blank_ident()
			&& right is ast.Ident {
			if right_sym.info is ast.ArrayFixed {
				if right_sym.info.elem_type.is_ptr() {
					c.error('assignment from one fixed array to another with a pointer element type is prohibited outside of `unsafe`',
						node.pos)
				}
			}
		}
		if left_sym.kind == .map && node.op in [.assign, .decl_assign] && right_sym.kind == .map
			&& !left.is_blank_ident() && right.is_lvalue() && right !is ast.ComptimeSelector
			&& (!right_type.is_ptr() || (right is ast.Ident && right.is_auto_deref_var())) {
			// Do not allow `a = b`
			c.error('cannot copy map: call `move` or `clone` method (or use a reference)',
				right.pos())
		}
		if left_sym.kind == .function && right_sym.info is ast.FnType {
			return_sym := c.table.sym(right_sym.info.func.return_type)
			if return_sym.kind == .placeholder {
				c.error('unknown return type: cannot assign `${right}` as a function variable',
					right.pos())
			} else if (!right_sym.info.is_anon && return_sym.kind == .any)
				|| (return_sym.info is ast.Struct && return_sym.info.is_generic) {
				c.error('cannot assign `${right}` as a generic function variable', right.pos())
			}
		}
		if left_type.is_any_kind_of_pointer() && !left.is_auto_deref_var() {
			if !c.inside_unsafe && node.op !in [.assign, .decl_assign] {
				// ptr op=
				c.warn('pointer arithmetic is only allowed in `unsafe` blocks', node.pos)
			}
			right_is_ptr := right_type.is_any_kind_of_pointer()
			if !right_is_ptr && node.op == .assign && right_type_unwrapped.is_number() {
				c.error('cannot assign to `${left}`: ' +
					c.expected_msg(right_type_unwrapped, left_type_unwrapped), right.pos())
			}
			if !right_sym.is_number() && !left_type.has_flag(.shared_f)
				&& (right is ast.StructInit || !right_is_ptr) {
				left_name := c.table.type_to_str(left_type_unwrapped)
				mut rtype := right_type_unwrapped
				if rtype.is_ptr() {
					rtype = rtype.deref()
				}
				right_name := c.table.type_to_str(rtype)
				if !(left_type.has_flag(.option) && right_type == ast.none_type) {
					c.error('mismatched types `${left_name}` and `${right_name}`', node.pos)
				}
			}
		}
		// Single side check
		match node.op {
			.assign {} // No need to do single side check for =. But here put it first for speed.
			.plus_assign, .minus_assign {
				if left_type == ast.string_type {
					if node.op != .plus_assign {
						c.error('operator `${node.op}` not defined on left operand type `${left_sym.name}`',
							left.pos())
					}
					if right_type != ast.string_type {
						c.error('invalid right operand: ${left_sym.name} ${node.op} ${right_sym.name}',
							right.pos())
					}
				} else if !left_sym.is_number()
					&& left_sym.kind !in [.byteptr, .charptr, .struct_, .alias] {
					c.error('operator `${node.op}` not defined on left operand type `${left_sym.name}`',
						left.pos())
				} else if !right_sym.is_number()
					&& left_sym.kind !in [.byteptr, .charptr, .struct_, .alias] {
					c.error('invalid right operand: ${left_sym.name} ${node.op} ${right_sym.name}',
						right.pos())
				}
			}
			.mult_assign, .div_assign {
				if !left_sym.is_number() && !c.table.final_sym(left_type_unwrapped).is_int()
					&& left_sym.kind !in [.struct_, .alias] {
					c.error('operator ${node.op.str()} not defined on left operand type `${left_sym.name}`',
						left.pos())
				} else if !right_sym.is_number() && !c.table.final_sym(left_type_unwrapped).is_int()
					&& left_sym.kind !in [.struct_, .alias] {
					c.error('operator ${node.op.str()} not defined on right operand type `${right_sym.name}`',
						right.pos())
				}
			}
			.and_assign, .or_assign, .xor_assign, .mod_assign, .left_shift_assign,
			.right_shift_assign {
				if !left_sym.is_int() && !c.table.final_sym(left_type_unwrapped).is_int() {
					c.error('operator ${node.op.str()} not defined on left operand type `${left_sym.name}`',
						left.pos())
				} else if !right_sym.is_int() && !c.table.final_sym(right_type_unwrapped).is_int() {
					c.error('operator ${node.op.str()} not defined on right operand type `${right_sym.name}`',
						right.pos())
				}
			}
			.unsigned_right_shift_assign {
				if node.left.len != 1 || node.right.len != 1 {
					c.error('unsupported operation: unable to lower expression for unsigned shift assignment.',
						node.pos)
				}

				modified_left_type := if !left_type.is_int() {
					c.error('invalid operation: shift on type `${c.table.sym(left_type).name}`',
						node.pos)
					ast.void_type_idx
				} else if left_type.is_int_literal() {
					// int literal => i64
					ast.u32_type_idx
				} else if left_type.is_unsigned() {
					left_type
				} else {
					// signed types' idx adds with 5 will get correct relative unsigned type
					// i8 		=> byte
					// i16 		=> u16
					// int  	=> u32
					// i64  	=> u64
					// isize	=> usize
					// i128 	=> u128 NOT IMPLEMENTED YET
					left_type.idx() + ast.u32_type_idx - ast.int_type_idx
				}

				node = ast.AssignStmt{
					op: .assign
					pos: node.pos
					end_comments: node.end_comments
					left: node.left
					right: [
						ast.Expr(ast.InfixExpr{
							left: ast.CastExpr{
								expr: node.left[0]
								typ: modified_left_type
								typname: c.table.type_str(modified_left_type)
								expr_type: left_type
								pos: node.pos
							}
							op: .right_shift
							right: node.right[0]
							left_type: modified_left_type
							right_type: right_type
							pos: node.pos
						}),
					]
					left_types: node.left_types
					right_types: node.right_types
					is_static: node.is_static
					is_simple: node.is_simple
					has_cross_var: node.has_cross_var
				}
			}
			else {}
		}
		if node.op in [.plus_assign, .minus_assign, .mod_assign, .mult_assign, .div_assign]
			&& (left_sym.kind == .alias || (left_sym.kind == .struct_
			&& right_sym.kind == .struct_)) {
			left_name := c.table.type_to_str(left_type_unwrapped)
			right_name := c.table.type_to_str(right_type_unwrapped)
			parent_sym := c.table.final_sym(left_type_unwrapped)
			if left_sym.kind == .alias && right_sym.kind != .alias {
				if !parent_sym.is_primitive() {
					c.error('mismatched types `${left_name}` and `${right_name}`', node.pos)
				}
			}
			extracted_op := match node.op {
				.plus_assign { '+' }
				.minus_assign { '-' }
				.div_assign { '/' }
				.mod_assign { '%' }
				.mult_assign { '*' }
				else { 'unknown op' }
			}
			if left_sym.kind == .struct_ && (left_sym.info as ast.Struct).generic_types.len > 0 {
				continue
			}
			if method := left_sym.find_method(extracted_op) {
				if method.return_type != left_type_unwrapped {
					c.error('operator `${extracted_op}` must return `${left_name}` to be used as an assignment operator',
						node.pos)
				}
			} else {
				if !parent_sym.is_primitive() {
					if left_name == right_name {
						c.error('undefined operation `${left_name}` ${extracted_op} `${right_name}`',
							node.pos)
					} else {
						c.error('mismatched types `${left_name}` and `${right_name}`',
							node.pos)
					}
				}
			}
		}
		if !is_blank_ident && right_sym.kind != .placeholder && left_sym.kind != .interface_
			&& ((!right_type.has_flag(.generic) && !left_type.has_flag(.generic))
			|| right_sym.kind != left_sym.kind) {
			// Dual sides check (compatibility check)
			c.check_expected(right_type_unwrapped, left_type_unwrapped) or {
				// allow literal values to auto deref var (e.g.`for mut v in values { v = 1.0 }`)
				if left.is_auto_deref_var() || right.is_auto_deref_var() {
					left_deref := if left.is_auto_deref_var() {
						left_type.deref()
					} else {
						left_type
					}
					right_deref := if right.is_pure_literal() {
						right.get_pure_type()
					} else if right.is_auto_deref_var() {
						right_type.deref()
					} else {
						right_type
					}
					if c.check_types(left_deref, right_deref) {
						continue
					}
				}
				// allow for ptr += 2
				if left_type_unwrapped.is_ptr() && right_type_unwrapped.is_int()
					&& node.op in [.plus_assign, .minus_assign] {
					if !c.inside_unsafe {
						c.warn('pointer arithmetic is only allowed in `unsafe` blocks',
							node.pos)
					}
				} else {
					// allow `t.$(field.name) = 0` where `t.$(field.name)` is a enum
					if c.comptime.comptime_for_field_var != '' && left is ast.ComptimeSelector {
						field_sym := c.table.sym(c.unwrap_generic(c.comptime.comptime_for_field_type))

						if field_sym.kind == .enum_ && !right_type.is_int() {
							c.error('enums can only be assigned `int` values', right.pos())
						}
					} else {
						if right_type_unwrapped != ast.void_type {
							if !var_option || (var_option && right_type_unwrapped != ast.none_type) {
								if left_sym.kind == .array_fixed && right_sym.kind == .array
									&& right is ast.ArrayInit {
									c.add_error_detail('try `${left} = ${right}!` instead (with `!` after the array literal)')
									c.error('cannot assign to `${left}`: ${err.msg()}',
										right.pos())
								} else {
									c.error('cannot assign to `${left}`: ${err.msg()}',
										right.pos())
								}
							}
						}
					}
				}
			}
		}
		if left_sym.kind == .interface_ {
			if c.type_implements(right_type, left_type, right.pos()) {
				if !right_type.is_any_kind_of_pointer() && right_sym.kind != .interface_
					&& !c.inside_unsafe {
					c.mark_as_referenced(mut &node.right[i], true)
				}
			}
		}
		if left_sym.info is ast.Struct && !left_sym.info.is_anon && right is ast.StructInit
			&& right.is_anon {
			c.error('cannot assign anonymous `struct` to a typed `struct`', right.pos())
		}
		if right_sym.kind == .alias && right_sym.name == 'byte' {
			c.warn('byte is deprecated, use u8 instead', right.pos())
		}
	}
	// this needs to run after the assign stmt left exprs have been run through checker
	// so that ident.obj is set
	// Check `x := &y` and `mut x := <-ch`
	if mut right_first is ast.PrefixExpr {
		mut right_node := right_first
		left_first := node.left[0]
		if left_first is ast.Ident {
			assigned_var := left_first
			mut is_shared := false
			if left_first.info is ast.IdentVar {
				is_shared = left_first.info.share == .shared_t
			}
			old_inside_ref_lit := c.inside_ref_lit
			c.inside_ref_lit = c.inside_ref_lit || right_node.op == .amp || is_shared
			c.expr(mut right_node.right)
			c.inside_ref_lit = old_inside_ref_lit
			if right_node.op == .amp {
				mut expr := right_node.right
				for mut expr is ast.ParExpr {
					expr = expr.expr
				}
				if mut expr is ast.Ident {
					if mut expr.obj is ast.Var {
						v := expr.obj
						right_first_type = v.typ
					}
					if !c.inside_unsafe && assigned_var.is_mut() && !expr.is_mut() {
						c.error('`${expr.name}` is immutable, cannot have a mutable reference to it',
							right_node.pos)
					}
				}
			}
			if right_node.op == .arrow {
				if assigned_var.is_mut {
					right_sym := c.table.sym(right_first_type)
					if right_sym.kind == .chan {
						chan_info := right_sym.chan_info()
						if chan_info.elem_type.is_ptr() && !chan_info.is_mut {
							c.error('cannot have a mutable reference to object from `${right_sym.name}`',
								right_node.pos)
						}
					}
				}
			}
		}
	}
	if node.left_types.len != node.left.len {
		c.error('assign statement left type number mismatch', node.pos)
	}
}
