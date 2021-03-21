// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast
import v.table
import v.pref

pub fn (mut c Checker) assign_stmt(mut assign_stmt ast.AssignStmt) {
	c.expected_type = table.none_type // TODO a hack to make `x := if ... work`
	defer {
		c.expected_type = table.void_type
	}
	right_first := assign_stmt.right[0]
	mut right_len := assign_stmt.right.len
	mut right_type0 := table.void_type
	for i, right in assign_stmt.right {
		if right is ast.CallExpr || right is ast.IfExpr || right is ast.LockExpr
			|| right is ast.MatchExpr {
			right_type := c.expr(right)
			if i == 0 {
				right_type0 = right_type
				assign_stmt.right_types = [
					c.check_expr_opt_call(right, right_type0),
				]
			}
			right_type_sym := c.table.get_type_symbol(right_type)
			if right_type_sym.kind == .multi_return {
				if assign_stmt.right.len > 1 {
					c.error('cannot use multi-value $right_type_sym.name in single-value context',
						right.position())
				}
				assign_stmt.right_types = right_type_sym.mr_info().types
				right_len = assign_stmt.right_types.len
			} else if right_type == table.void_type {
				right_len = 0
			}
		}
	}
	if assign_stmt.left.len != right_len {
		if right_first is ast.CallExpr {
			c.error('assignment mismatch: $assign_stmt.left.len variable(s) but `${right_first.name}()` returns $right_len value(s)',
				assign_stmt.pos)
		} else {
			c.error('assignment mismatch: $assign_stmt.left.len variable(s) $right_len value(s)',
				assign_stmt.pos)
		}
		return
	}
	//
	is_decl := assign_stmt.op == .decl_assign
	for i, left in assign_stmt.left {
		if left is ast.CallExpr {
			c.error('cannot call function `${left.name}()` on the left side of an assignment',
				left.pos)
		}
		is_blank_ident := left.is_blank_ident()
		mut left_type := table.void_type
		if !is_decl && !is_blank_ident {
			if left is ast.Ident || left is ast.SelectorExpr {
				c.prevent_sum_type_unwrapping_once = true
			}
			left_type = c.expr(left)
			c.expected_type = c.unwrap_generic(left_type)
			// `map = {}`
			sym := c.table.get_type_symbol(left_type)
			if sym.kind == .map && assign_stmt.right[i] is ast.StructInit {
				c.warn('assigning a struct literal to a map is deprecated - use `map{}` instead',
					assign_stmt.right[i].position())
				assign_stmt.right[i] = ast.MapInit{}
			}
		}
		if assign_stmt.right_types.len < assign_stmt.left.len { // first type or multi return types added above
			old_inside_ref_lit := c.inside_ref_lit
			if left is ast.Ident {
				if left.info is ast.IdentVar {
					c.inside_ref_lit = c.inside_ref_lit || left.info.share == .shared_t
				}
			}
			right_type := c.expr(assign_stmt.right[i])
			c.inside_ref_lit = old_inside_ref_lit
			if assign_stmt.right_types.len == i {
				assign_stmt.right_types << c.check_expr_opt_call(assign_stmt.right[i],
					right_type)
			}
		}
		right := if i < assign_stmt.right.len { assign_stmt.right[i] } else { assign_stmt.right[0] }
		mut right_type := assign_stmt.right_types[i]
		if is_decl {
			if right.is_auto_deref_var() {
				left_type = c.table.mktyp(right_type.deref())
			} else {
				left_type = c.table.mktyp(right_type)
			}
			if left_type == table.int_type {
				if right is ast.IntegerLiteral {
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
		} else {
			// Make sure the variable is mutable
			c.fail_if_immutable(left)
			// left_type = c.expr(left)
		}
		assign_stmt.left_types << left_type
		match mut left {
			ast.Ident {
				if left.kind == .blank_ident {
					left_type = right_type
					assign_stmt.left_types[i] = right_type
					if assign_stmt.op !in [.assign, .decl_assign] {
						c.error('cannot modify blank `_` identifier', left.pos)
					}
				} else if left.info !is ast.IdentVar {
					c.error('cannot assign to $left.kind `$left.name`', left.pos)
				} else {
					if is_decl {
						c.check_valid_snake_case(left.name, 'variable name', left.pos)
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
						left_type = left_type.clear_flag(.shared_f)
					}
					if ident_var_info.share == .atomic_t {
						left_type = left_type.set_flag(.atomic_f)
					}
					assign_stmt.left_types[i] = left_type
					ident_var_info.typ = left_type
					left.info = ident_var_info
					if left_type != 0 {
						match mut left.obj {
							ast.Var { left.obj.typ = left_type }
							ast.GlobalField { left.obj.typ = left_type }
							else {}
						}
						/*
						if left.obj is ast.Var as v {
							v.typ = left_type
						} else if left.obj is ast.GlobalDecl as v {
							v.typ = left_type
						}
						*/
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
				if left.op == .mul && !c.inside_unsafe {
					c.error('modifying variables via dereferencing can only be done in `unsafe` blocks',
						assign_stmt.pos)
				}
				if is_decl {
					c.error('non-name on the left side of `:=`', left.pos)
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
					c.error('non-name `$left` on left side of `:=`', left.position())
				}
			}
		}
		left_type_unwrapped := c.unwrap_generic(left_type)
		right_type_unwrapped := c.unwrap_generic(right_type)
		if right_type_unwrapped == 0 {
			// right type was a generic `T`
			continue
		}
		left_sym := c.table.get_type_symbol(left_type_unwrapped)
		right_sym := c.table.get_type_symbol(right_type_unwrapped)
		if c.pref.translated {
			// TODO fix this in C2V instead, for example cast enums to int before using `|` on them.
			// TODO replace all c.pref.translated checks with `$if !translated` for performance
			continue
		}
		if left_sym.kind == .array && !c.inside_unsafe && assign_stmt.op in [.assign, .decl_assign]
			&& right_sym.kind == .array && (left is ast.Ident && !left.is_blank_ident())
			&& right is ast.Ident {
			// Do not allow `a = b`, only `a = b.clone()`
			c.error('use `array2 $assign_stmt.op.str() array1.clone()` instead of `array2 $assign_stmt.op.str() array1` (or use `unsafe`)',
				assign_stmt.pos)
		}
		if left_sym.kind == .map && assign_stmt.op in [.assign, .decl_assign]
			&& right_sym.kind == .map && ((right is ast.Ident && right.is_auto_deref_var())
			|| !right_type.is_ptr()) && !left.is_blank_ident() && right.is_lvalue() {
			// Do not allow `a = b`
			c.error('cannot copy map: call `move` or `clone` method (or use a reference)',
				right.position())
		}
		left_is_ptr := left_type.is_ptr() || left_sym.is_pointer()
		if left_is_ptr && !left.is_auto_deref_var() {
			if !c.inside_unsafe && assign_stmt.op !in [.assign, .decl_assign] {
				// ptr op=
				c.warn('pointer arithmetic is only allowed in `unsafe` blocks', assign_stmt.pos)
			}
			right_is_ptr := right_type.is_ptr() || right_sym.is_pointer()
			if !right_is_ptr && assign_stmt.op == .assign && right_type_unwrapped.is_number() {
				c.error('cannot assign to `$left`: ' +
					c.expected_msg(right_type_unwrapped, left_type_unwrapped), right.position())
			}
			if (right is ast.StructInit || !right_is_ptr) && !(right_sym.is_number()
				|| left_type.has_flag(.shared_f)) {
				left_name := c.table.type_to_str(left_type_unwrapped)
				mut rtype := right_type_unwrapped
				if rtype.is_ptr() {
					rtype = rtype.deref()
				}
				right_name := c.table.type_to_str(rtype)
				c.error('mismatched types `$left_name` and `$right_name`', assign_stmt.pos)
			}
		}
		// Single side check
		match assign_stmt.op {
			.assign {} // No need to do single side check for =. But here put it first for speed.
			.plus_assign, .minus_assign {
				if left_type == table.string_type {
					if assign_stmt.op != .plus_assign {
						c.error('operator `$assign_stmt.op` not defined on left operand type `$left_sym.name`',
							left.position())
					}
					if right_type != table.string_type {
						c.error('invalid right operand: $left_sym.name $assign_stmt.op $right_sym.name',
							right.position())
					}
				} else if !left_sym.is_number()
					&& left_sym.kind !in [.byteptr, .charptr, .struct_, .alias] {
					c.error('operator `$assign_stmt.op` not defined on left operand type `$left_sym.name`',
						left.position())
				} else if !right_sym.is_number()
					&& left_sym.kind !in [.byteptr, .charptr, .struct_, .alias] {
					c.error('invalid right operand: $left_sym.name $assign_stmt.op $right_sym.name',
						right.position())
				}
			}
			.mult_assign, .div_assign {
				if !left_sym.is_number()
					&& !c.table.get_final_type_symbol(left_type_unwrapped).is_int()
					&& left_sym.kind !in [.struct_, .alias] {
					c.error('operator $assign_stmt.op.str() not defined on left operand type `$left_sym.name`',
						left.position())
				} else if !right_sym.is_number()
					&& !c.table.get_final_type_symbol(left_type_unwrapped).is_int()
					&& left_sym.kind !in [.struct_, .alias] {
					c.error('operator $assign_stmt.op.str() not defined on right operand type `$right_sym.name`',
						right.position())
				}
			}
			.and_assign, .or_assign, .xor_assign, .mod_assign, .left_shift_assign, .right_shift_assign
			 {
				if !left_sym.is_int()
					&& !c.table.get_final_type_symbol(left_type_unwrapped).is_int() {
					c.error('operator $assign_stmt.op.str() not defined on left operand type `$left_sym.name`',
						left.position())
				} else if !right_sym.is_int()
					&& !c.table.get_final_type_symbol(right_type_unwrapped).is_int() {
					c.error('operator $assign_stmt.op.str() not defined on right operand type `$right_sym.name`',
						right.position())
				}
			}
			else {}
		}
		if assign_stmt.op in [.plus_assign, .minus_assign, .mod_assign, .mult_assign, .div_assign]
			&& ((left_sym.kind == .struct_ && right_sym.kind == .struct_)
			|| left_sym.kind == .alias) {
			left_name := c.table.type_to_str(left_type)
			right_name := c.table.type_to_str(right_type)
			parent_sym := c.table.get_final_type_symbol(left_type)
			if left_sym.kind == .alias && right_sym.kind != .alias {
				c.error('mismatched types `$left_name` and `$right_name`', assign_stmt.pos)
			}
			extracted_op := match assign_stmt.op {
				.plus_assign { '+' }
				.minus_assign { '-' }
				.div_assign { '/' }
				.mod_assign { '%' }
				.mult_assign { '*' }
				else { 'unknown op' }
			}
			if method := left_sym.find_method(extracted_op) {
				if method.return_type != left_type {
					c.error('operator `$extracted_op` must return `$left_name` to be used as an assignment operator',
						assign_stmt.pos)
				}
			} else {
				if parent_sym.is_primitive() {
					c.error('cannot use operator methods on type alias for `$parent_sym.name`',
						assign_stmt.pos)
				}
				if left_name == right_name {
					c.error('undefined operation `$left_name` $extracted_op `$right_name`',
						assign_stmt.pos)
				} else {
					c.error('mismatched types `$left_name` and `$right_name`', assign_stmt.pos)
				}
			}
		}
		if !is_blank_ident && !right.is_auto_deref_var() && right_sym.kind != .placeholder
			&& left_sym.kind != .interface_ {
			// Dual sides check (compatibility check)
			c.check_expected(right_type_unwrapped, left_type_unwrapped) or {
				c.error('cannot assign to `$left`: $err.msg', right.position())
			}
		}
		if left_sym.kind == .interface_ {
			c.type_implements(right_type, left_type, right.position())
		}
	}
	// this needs to run after the assign stmt left exprs have been run through checker so that ident.obj is set
	// Check `x := &y` and `mut x := <-ch`
	if right_first is ast.PrefixExpr {
		node := right_first
		left_first := assign_stmt.left[0]
		if left_first is ast.Ident {
			assigned_var := left_first
			mut is_shared := false
			if left_first.info is ast.IdentVar {
				is_shared = left_first.info.share == .shared_t
			}
			old_inside_ref_lit := c.inside_ref_lit
			c.inside_ref_lit = (c.inside_ref_lit || node.op == .amp || is_shared)
			c.expr(node.right)
			c.inside_ref_lit = old_inside_ref_lit
			if node.right is ast.Ident {
				if node.right.obj is ast.Var {
					v := node.right.obj
					right_type0 = v.typ
					if node.op == .amp {
						if !v.is_mut && assigned_var.is_mut && !c.inside_unsafe {
							c.error('`$node.right.name` is immutable, cannot have a mutable reference to it',
								node.pos)
						}
					}
				}
			}
			if node.op == .arrow {
				if assigned_var.is_mut {
					right_sym := c.table.get_type_symbol(right_type0)
					if right_sym.kind == .chan {
						chan_info := right_sym.chan_info()
						if chan_info.elem_type.is_ptr() && !chan_info.is_mut {
							c.error('cannot have a mutable reference to object from `$right_sym.name`',
								node.pos)
						}
					}
				}
			}
		}
	}
	// right_sym := c.table.get_type_symbol(right_type_unwrapped)
}
