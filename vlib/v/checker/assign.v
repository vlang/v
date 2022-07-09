// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast
import v.pref

// TODO 600 line function
pub fn (mut c Checker) assign_stmt(mut node ast.AssignStmt) {
	c.expected_type = ast.none_type // TODO a hack to make `x := if ... work`
	defer {
		c.expected_type = ast.void_type
	}
	is_decl := node.op == .decl_assign
	right_first := node.right[0]
	node.left_types = []
	mut right_len := node.right.len
	mut right_type0 := ast.void_type
	for i, mut right in node.right {
		if right in [ast.CallExpr, ast.IfExpr, ast.LockExpr, ast.MatchExpr] {
			if right in [ast.IfExpr, ast.MatchExpr] && node.left.len == node.right.len && !is_decl
				&& node.left[i] in [ast.Ident, ast.SelectorExpr] && !node.left[i].is_blank_ident() {
				c.expected_type = c.expr(node.left[i])
			}
			right_type := c.expr(right)
			c.fail_if_unreadable(right, right_type, 'right-hand side of assignment')
			if i == 0 {
				right_type0 = right_type
				node.right_types = [
					c.check_expr_opt_call(right, right_type0),
				]
			}
			right_type_sym := c.table.sym(right_type)
			if right_type_sym.kind == .multi_return {
				if node.right.len > 1 {
					c.error('cannot use multi-value $right_type_sym.name in single-value context',
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
		if mut right is ast.None {
			c.error('you can not assign a `none` value to a variable', right.pos)
		}
	}
	if node.left.len != right_len {
		if right_first is ast.CallExpr {
			if node.left_types.len > 0 && node.left_types[0] == ast.void_type {
				// If it's a void type, it's an unknown variable, already had an error earlier.
				return
			}
			c.error('assignment mismatch: $node.left.len variable(s) but `${right_first.name}()` returns $right_len value(s)',
				node.pos)
		} else {
			c.error('assignment mismatch: $node.left.len variable(s) $right_len value(s)',
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
		if !is_decl && !is_blank_ident {
			if left in [ast.Ident, ast.SelectorExpr] {
				c.prevent_sum_type_unwrapping_once = true
			}
			left_type = c.expr(left)
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
			right_type := c.expr(node.right[i])
			c.inside_decl_rhs = false
			c.inside_ref_lit = old_inside_ref_lit
			if node.right_types.len == i {
				node.right_types << c.check_expr_opt_call(node.right[i], right_type)
			}
		}
		mut right := if i < node.right.len { node.right[i] } else { node.right[0] }
		mut right_type := node.right_types[i]
		if mut right is ast.Ident {
			right_sym := c.table.sym(right_type)
			if right_sym.info is ast.Struct {
				if right_sym.info.generic_types.len > 0 {
					if obj := right.scope.find(right.name) {
						right_type = obj.typ
					}
				}
			}
		} else if right is ast.ComptimeSelector {
			right_type = c.comptime_fields_default_type
		}
		if is_decl {
			// check generic struct init and return unwrap generic struct type
			if mut right is ast.StructInit {
				if right.typ.has_flag(.generic) {
					c.expr(right)
					right_type = right.typ
				}
			} else if mut right is ast.PrefixExpr {
				if right.op == .amp && right.right is ast.StructInit {
					right_type = c.expr(right)
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
					c.error('`$right.name` is immutable, cannot have a mutable reference to an immutable object',
						right.pos)
				}
			}
		} else {
			// Make sure the variable is mutable
			c.fail_if_immutable(left)
			// left_type = c.expr(left)
		}
		if right_type.is_ptr() && left_type.is_ptr() {
			if mut right is ast.Ident {
				if mut right.obj is ast.Var {
					mut obj := unsafe { &right.obj }
					if c.fn_scope != voidptr(0) {
						obj = c.fn_scope.find_var(right.obj.name) or { obj }
					}
					if obj.is_stack_obj && !c.inside_unsafe {
						type_sym := c.table.sym(obj.typ.set_nr_muls(0))
						if !type_sym.is_heap() && !c.pref.translated && !c.file.is_translated {
							suggestion := if type_sym.kind == .struct_ {
								'declaring `$type_sym.name` as `[heap]`'
							} else {
								'wrapping the `$type_sym.name` object in a `struct` declared as `[heap]`'
							}
							c.error('`$right.name` cannot be assigned outside `unsafe` blocks as it might refer to an object stored on stack. Consider ${suggestion}.',
								right.pos)
						}
					}
				}
			}
		}
		// Do not allow `a := 0; b := 0; a = &b`
		if !is_decl && left is ast.Ident && !is_blank_ident && !left_type.is_real_pointer()
			&& right_type.is_real_pointer() && !right_type.has_flag(.shared_f) {
			left_sym := c.table.sym(left_type)
			if left_sym.kind != .function {
				c.warn(
					'cannot assign a reference to a value (this will be an error soon) left=${c.table.type_str(left_type)} $left_type.is_ptr() ' +
					'right=${c.table.type_str(right_type)} $right_type.is_real_pointer() ptr=$right_type.is_ptr()',
					node.pos)
			}
		}
		node.left_types << left_type
		match mut left {
			ast.Ident {
				if left.kind == .blank_ident {
					left_type = right_type
					node.left_types[i] = right_type
					if node.op !in [.assign, .decl_assign] {
						c.error('cannot modify blank `_` identifier', left.pos)
					}
				} else if left.info !is ast.IdentVar {
					c.error('cannot assign to $left.kind `$left.name`', left.pos)
				} else {
					if is_decl {
						c.check_valid_snake_case(left.name, 'variable name', left.pos)
						if left.name in reserved_type_names {
							c.error('invalid use of reserved type `$left.name` as a variable name',
								left.pos)
						}
					}
					mut ident_var_info := left.info as ast.IdentVar
					if ident_var_info.share == .shared_t {
						left_type = left_type.set_flag(.shared_f)
						if is_decl {
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
											c.error('Cannot assign negative value to unsigned integer type',
												right.pos)
										}
									}
								}
							}
							ast.GlobalField {
								left.obj.typ = left_type
							}
							else {}
						}
					}
					if is_decl {
						full_name := '${left.mod}.$left.name'
						if obj := c.file.global_scope.find(full_name) {
							if obj is ast.ConstField {
								c.warn('duplicate of a const name `$full_name`', left.pos)
							}
						}
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
			}
			else {
				if mut left is ast.IndexExpr {
					// eprintln('>>> left.is_setter: ${left.is_setter:10} | left.is_map: ${left.is_map:10} | left.is_array: ${left.is_array:10}')
					if left.is_map && left.is_setter {
						left.recursive_mapset_is_setter(true)
					}
				}
				if is_decl {
					c.error('non-name `$left` on left side of `:=`', left.pos())
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
		if left_sym.kind == .array && !c.inside_unsafe && node.op in [.assign, .decl_assign]
			&& right_sym.kind == .array && left is ast.Ident && !left.is_blank_ident()
			&& right is ast.Ident {
			// Do not allow `a = b`, only `a = b.clone()`
			c.error('use `array2 $node.op.str() array1.clone()` instead of `array2 $node.op.str() array1` (or use `unsafe`)',
				node.pos)
		}
		if left_sym.kind == .array && right_sym.kind == .array {
			// `mut arr := [u8(1),2,3]`
			// `arr = [byte(4),5,6]`
			left_info := left_sym.info as ast.Array
			left_elem_type := c.table.unaliased_type(left_info.elem_type)
			right_info := right_sym.info as ast.Array
			right_elem_type := c.table.unaliased_type(right_info.elem_type)
			if left_type_unwrapped.nr_muls() == right_type_unwrapped.nr_muls()
				&& left_info.nr_dims == right_info.nr_dims && left_elem_type == right_elem_type {
				continue
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
			&& !left.is_blank_ident() && right.is_lvalue()
			&& (!right_type.is_ptr() || (right is ast.Ident && right.is_auto_deref_var())) {
			// Do not allow `a = b`
			c.error('cannot copy map: call `move` or `clone` method (or use a reference)',
				right.pos())
		}
		left_is_ptr := left_type.is_ptr() || left_sym.is_pointer()
		if left_is_ptr && !left.is_auto_deref_var() {
			if !c.inside_unsafe && node.op !in [.assign, .decl_assign] {
				// ptr op=
				c.warn('pointer arithmetic is only allowed in `unsafe` blocks', node.pos)
			}
			right_is_ptr := right_type.is_ptr() || right_sym.is_pointer()
			if !right_is_ptr && node.op == .assign && right_type_unwrapped.is_number() {
				c.error('cannot assign to `$left`: ' +
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
				c.error('mismatched types `$left_name` and `$right_name`', node.pos)
			}
		}
		// Single side check
		match node.op {
			.assign {} // No need to do single side check for =. But here put it first for speed.
			.plus_assign, .minus_assign {
				if left_type == ast.string_type {
					if node.op != .plus_assign {
						c.error('operator `$node.op` not defined on left operand type `$left_sym.name`',
							left.pos())
					}
					if right_type != ast.string_type {
						c.error('invalid right operand: $left_sym.name $node.op $right_sym.name',
							right.pos())
					}
				} else if !left_sym.is_number()
					&& left_sym.kind !in [.byteptr, .charptr, .struct_, .alias] {
					c.error('operator `$node.op` not defined on left operand type `$left_sym.name`',
						left.pos())
				} else if !right_sym.is_number()
					&& left_sym.kind !in [.byteptr, .charptr, .struct_, .alias] {
					c.error('invalid right operand: $left_sym.name $node.op $right_sym.name',
						right.pos())
				}
			}
			.mult_assign, .div_assign {
				if !left_sym.is_number() && !c.table.final_sym(left_type_unwrapped).is_int()
					&& left_sym.kind !in [.struct_, .alias] {
					c.error('operator $node.op.str() not defined on left operand type `$left_sym.name`',
						left.pos())
				} else if !right_sym.is_number() && !c.table.final_sym(left_type_unwrapped).is_int()
					&& left_sym.kind !in [.struct_, .alias] {
					c.error('operator $node.op.str() not defined on right operand type `$right_sym.name`',
						right.pos())
				}
			}
			.and_assign, .or_assign, .xor_assign, .mod_assign, .left_shift_assign,
			.right_shift_assign {
				if !left_sym.is_int() && !c.table.final_sym(left_type_unwrapped).is_int() {
					c.error('operator $node.op.str() not defined on left operand type `$left_sym.name`',
						left.pos())
				} else if !right_sym.is_int() && !c.table.final_sym(right_type_unwrapped).is_int() {
					c.error('operator $node.op.str() not defined on right operand type `$right_sym.name`',
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
					comments: node.comments
					end_comments: node.end_comments
					left: node.left
					right: [
						ast.Expr(ast.InfixExpr{
							left: ast.CastExpr{
								expr: node.left[0]
								typ: modified_left_type
								typname: c.table.type_str(modified_left_type)
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
				c.error('mismatched types `$left_name` and `$right_name`', node.pos)
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
					c.error('operator `$extracted_op` must return `$left_name` to be used as an assignment operator',
						node.pos)
				}
			} else {
				if parent_sym.is_primitive() {
					c.error('cannot use operator methods on type alias for `$parent_sym.name`',
						node.pos)
				}
				if left_name == right_name {
					c.error('undefined operation `$left_name` $extracted_op `$right_name`',
						node.pos)
				} else {
					c.error('mismatched types `$left_name` and `$right_name`', node.pos)
				}
			}
		}
		if !is_blank_ident && !left.is_auto_deref_var() && !right.is_auto_deref_var()
			&& right_sym.kind != .placeholder && left_sym.kind != .interface_
			&& !right_type.has_flag(.generic) && !left_type.has_flag(.generic) {
			// Dual sides check (compatibility check)
			c.check_expected(right_type_unwrapped, left_type_unwrapped) or {
				// allow for ptr += 2
				if left_type_unwrapped.is_ptr() && right_type_unwrapped.is_int()
					&& node.op in [.plus_assign, .minus_assign] {
					if !c.inside_unsafe {
						c.warn('pointer arithmetic is only allowed in `unsafe` blocks',
							node.pos)
					}
				} else {
					c.error('cannot assign to `$left`: $err.msg()', right.pos())
				}
			}
		}
		if left_sym.kind == .interface_ {
			if c.type_implements(right_type, left_type, right.pos()) {
				if !right_type.is_ptr() && !right_type.is_pointer() && right_sym.kind != .interface_
					&& !c.inside_unsafe {
					c.mark_as_referenced(mut &node.right[i], true)
				}
			}
		}
	}
	// this needs to run after the assign stmt left exprs have been run through checker
	// so that ident.obj is set
	// Check `x := &y` and `mut x := <-ch`
	if right_first is ast.PrefixExpr {
		right_node := right_first
		left_first := node.left[0]
		if left_first is ast.Ident {
			assigned_var := left_first
			mut is_shared := false
			if left_first.info is ast.IdentVar {
				is_shared = left_first.info.share == .shared_t
			}
			old_inside_ref_lit := c.inside_ref_lit
			c.inside_ref_lit = c.inside_ref_lit || right_node.op == .amp || is_shared
			c.expr(right_node.right)
			c.inside_ref_lit = old_inside_ref_lit
			if right_node.op == .amp {
				if right_node.right is ast.Ident {
					if right_node.right.obj is ast.Var {
						v := right_node.right.obj
						right_type0 = v.typ
					}
					if !c.inside_unsafe && assigned_var.is_mut() && !right_node.right.is_mut() {
						c.error('`$right_node.right.name` is immutable, cannot have a mutable reference to it',
							right_node.pos)
					}
				}
			}
			if right_node.op == .arrow {
				if assigned_var.is_mut {
					right_sym := c.table.sym(right_type0)
					if right_sym.kind == .chan {
						chan_info := right_sym.chan_info()
						if chan_info.elem_type.is_ptr() && !chan_info.is_mut {
							c.error('cannot have a mutable reference to object from `$right_sym.name`',
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
