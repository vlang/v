module checker

import v.ast
import v.token

fn (mut c Checker) infix_expr(mut node ast.InfixExpr) ast.Type {
	former_expected_type := c.expected_type
	defer {
		c.expected_type = former_expected_type
	}
	mut left_type := c.expr(mut node.left)
	mut left_sym := c.table.sym(left_type)
	node.left_type = left_type
	c.expected_type = left_type

	if left_sym.kind == .chan {
		chan_info := left_sym.chan_info()
		c.expected_type = chan_info.elem_type
	}

	if !node.left_ct_expr && !node.left.is_literal() {
		node.left_ct_expr = c.comptime.is_comptime(node.left)
	}
	if !node.right_ct_expr && !node.right.is_literal() {
		node.right_ct_expr = c.comptime.is_comptime(node.right)
	}

	// `if n is ast.Ident && n.is_mut { ... }`
	if !c.inside_sql && node.op == .and {
		mut left_node := node.left
		for mut left_node is ast.InfixExpr {
			if left_node.op == .and && mut left_node.right is ast.InfixExpr {
				if left_node.right.op == .key_is {
					// search last `n is ast.Ident` in the left
					from_type := c.expr(mut left_node.right.left)
					to_type := c.expr(mut left_node.right.right)
					c.autocast_in_if_conds(mut node.right, left_node.right.left, from_type,
						to_type)
				}
			}
			if left_node.op == .key_is {
				// search `n is ast.Ident`
				from_type := c.expr(mut left_node.left)
				to_type := c.expr(mut left_node.right)
				c.autocast_in_if_conds(mut node.right, left_node.left, from_type, to_type)
				break
			} else if left_node.op == .and {
				left_node = left_node.left
			} else {
				break
			}
		}
	}

	if node.op == .key_is {
		c.inside_x_is_type = true
	}
	// `arr << if n > 0 { 10 } else { 11 }` set the right c.expected_type
	if node.op == .left_shift && c.table.sym(left_type).kind == .array {
		if left_type.has_flag(.option) {
			c.error('cannot push to Option array that was not unwrapped first', node.left.pos())
		}
		c.markused_infixexpr(!c.is_builtin_mod && c.mod != 'strings')
		if mut node.right is ast.IfExpr {
			if node.right.is_expr && node.right.branches.len > 0 {
				mut last_stmt := node.right.branches[0].stmts.last()
				if mut last_stmt is ast.ExprStmt {
					expr_typ := c.expr(mut last_stmt.expr)
					info := c.table.sym(left_type).array_info()
					if expr_typ == info.elem_type {
						c.expected_type = info.elem_type
					}
				}
			}
		}
	}
	mut right_type := c.expr(mut node.right)
	if node.op == .key_is {
		c.inside_x_is_type = false
	}
	if node.op == .amp && left_type.is_bool() && right_type.is_bool() && right_type.is_ptr() {
		pos := node.pos.extend(node.right.pos())
		c.error('the right expression should be separated from the `&&` by a space', pos)
		node.promoted_type = ast.bool_type
		return ast.bool_type
	}
	node.right_type = right_type
	if !left_type.has_flag(.option) && left_type.is_number() && !left_type.is_ptr()
		&& right_type in [ast.int_literal_type, ast.float_literal_type] {
		node.right_type = left_type
		if left_type in [ast.f32_type_idx, ast.f64_type_idx] && right_type == ast.float_literal_type {
			defer {
				node.right = ast.CastExpr{
					expr:      node.right
					typ:       left_type
					typname:   c.table.get_type_name(left_type)
					expr_type: right_type
					pos:       node.right.pos()
				}
			}
		}
	}
	if right_type.is_number() && !right_type.is_ptr()
		&& left_type in [ast.int_literal_type, ast.float_literal_type] {
		node.left_type = right_type
		if right_type in [ast.f32_type_idx, ast.f64_type_idx] && left_type == ast.float_literal_type {
			defer {
				node.left = ast.CastExpr{
					expr:      node.left
					typ:       right_type
					typname:   c.table.get_type_name(right_type)
					expr_type: left_type
				}
			}
		}
	}
	mut right_sym := c.table.sym(right_type)
	right_final_sym := c.table.final_sym(c.unwrap_generic(right_type))
	left_final_sym := c.table.final_sym(c.unwrap_generic(left_type))
	left_pos := node.left.pos()
	right_pos := node.right.pos()
	left_right_pos := left_pos.extend(right_pos)
	if left_sym.kind == .none && right_sym.kind == .none {
		c.invalid_operator_error(node.op, left_type, right_type, left_right_pos)
	}
	if left_sym.kind == .multi_return && right_sym.kind == .multi_return {
		c.error('invalid number of operand for `${node.op}`. Only one allowed on each side.',
			left_right_pos)
	}
	if left_type.is_any_kind_of_pointer() && !node.left.is_auto_deref_var()
		&& node.op in [.plus, .minus, .mul, .div, .mod, .xor, .amp, .pipe] {
		if !c.pref.translated && ((right_type.is_any_kind_of_pointer() && node.op != .minus)
			|| (!right_type.is_any_kind_of_pointer() && node.op !in [.plus, .minus])) {
			if _ := left_sym.find_method(node.op.str()) {
				if left_sym.kind == .alias && right_sym.kind == .alias {
					// allow an explicit operator override `fn (x &AliasType) OP (y &AliasType) &AliasType {`
				} else {
					c.invalid_operator_error(node.op, left_type, right_type, left_right_pos)
				}
			} else {
				c.invalid_operator_error(node.op, left_type, right_type, left_right_pos)
			}
		} else if node.op in [.plus, .minus] {
			if !c.inside_unsafe && !node.left.is_auto_deref_var() && !node.right.is_auto_deref_var() {
				if !c.pref.translated && !c.file.is_translated {
					c.warn('pointer arithmetic is only allowed in `unsafe` blocks', left_right_pos)
				}
			}
			if (left_type == ast.voidptr_type || left_type == ast.nil_type) && !c.pref.translated {
				c.error('`${node.op}` cannot be used with `voidptr`', left_pos)
			}
		}
	}
	mut return_type := left_type

	if node.op != .key_is {
		match mut node.left {
			ast.Ident, ast.SelectorExpr {
				// mut foo != none is allowed for unwrapping option
				if !(node.op == .ne && node.right is ast.None) {
					if node.left.is_mut {
						c.error('the `mut` keyword is invalid here', node.left.mut_pos)
					}
				}
			}
			else {}
		}
	}
	match mut node.right {
		ast.Ident, ast.SelectorExpr {
			if node.right.is_mut {
				c.error('the `mut` keyword is invalid here', node.right.mut_pos)
			}
		}
		else {}
	}
	eq_ne := node.op in [.eq, .ne]
	// Single side check
	// Place these branches according to ops' usage frequency to accelerate.
	// TODO: First branch includes ops where single side check is not needed, or needed but hasn't been implemented.
	// TODO: Some of the checks are not single side. Should find a better way to organize them.
	match node.op {
		// .eq, .ne, .gt, .lt, .ge, .le, .and, .logical_or, .dot, .key_as, .right_shift {}
		.eq, .ne {
			if node.left is ast.CallExpr && node.left.or_block.stmts.len > 0 {
				c.check_expr_option_or_result_call(node.left, left_type)
			}
			if node.right is ast.CallExpr && node.right.or_block.stmts.len > 0 {
				c.check_expr_option_or_result_call(node.right, right_type)
			}
			if left_type in ast.integer_type_idxs && right_type in ast.integer_type_idxs {
				is_left_type_signed := left_type in ast.signed_integer_type_idxs
				is_right_type_signed := right_type in ast.signed_integer_type_idxs
				if !is_left_type_signed && mut node.right is ast.IntegerLiteral {
					if node.right.val.int() < 0 && left_type in ast.int_promoted_type_idxs {
						lt := c.table.sym(left_type).name
						c.error('`${lt}` cannot be compared with negative value', node.right.pos)
					}
				} else if !is_right_type_signed && mut node.left is ast.IntegerLiteral {
					if node.left.val.int() < 0 && right_type in ast.int_promoted_type_idxs {
						rt := c.table.sym(right_type).name
						c.error('negative value cannot be compared with `${rt}`', node.left.pos)
					}
				} else if is_left_type_signed != is_right_type_signed
					&& left_type != ast.int_literal_type_idx
					&& right_type != ast.int_literal_type_idx {
					ls, _ := c.table.type_size(left_type)
					rs, _ := c.table.type_size(right_type)
					// prevent e.g. `u32 == i16` but not `u16 == i32` as max_u16 fits in i32
					// TODO: u32 == i32, change < to <=
					if !c.pref.translated && ((is_left_type_signed && ls < rs)
						|| (is_right_type_signed && rs < ls)) {
						lt := c.table.sym(left_type).name
						rt := c.table.sym(right_type).name
						c.error('`${lt}` cannot be compared with `${rt}`', node.pos)
					}
				}
			}

			// Do not allow comparing nil to non-pointers
			if node.left.is_nil() {
				mut final_type := right_type
				if mut right_sym.info is ast.Alias
					&& right_sym.info.parent_type.is_any_kind_of_pointer() {
					final_type = right_sym.info.parent_type
				}
				if !final_type.is_any_kind_of_pointer() && (right_final_sym.kind != .function
					|| (right_final_sym.language != .c && right_final_sym.kind == .placeholder))
					&& !right_final_sym.is_heap() {
					rt := c.table.sym(right_type).name
					c.error('cannot compare with `nil` because `${rt}` is not a pointer',
						node.pos)
				}
			}

			if node.right.is_nil() {
				mut final_type := left_type
				if mut left_sym.info is ast.Alias
					&& left_sym.info.parent_type.is_any_kind_of_pointer() {
					final_type = left_sym.info.parent_type
				}
				if !final_type.is_any_kind_of_pointer() && (left_final_sym.kind != .function
					|| (left_final_sym.language != .c && left_final_sym.kind == .placeholder))
					&& !left_final_sym.is_heap() {
					lt := c.table.sym(left_type).name
					c.error('cannot compare with `nil` because `${lt}` is not a pointer',
						node.pos)
				}
			}
		}
		.key_in, .not_in {
			match right_final_sym.kind {
				.array {
					if left_sym.kind !in [.sum_type, .interface] {
						elem_type := right_final_sym.array_info().elem_type
						if node.left.is_auto_deref_var() {
							left_type = left_type.deref()
						}
						c.check_expected(left_type, elem_type) or {
							c.error('left operand to `${node.op}` does not match the array element type: ${err.msg()}',
								left_right_pos)
						}
						if mut node.right is ast.ArrayInit {
							c.check_duplicated_items(node.right)
						}
					} else {
						if mut node.right is ast.ArrayInit {
							for i, typ in node.right.expr_types {
								c.ensure_type_exists(typ, node.right.exprs[i].pos())
							}
						} else {
							elem_type := right_final_sym.array_info().elem_type
							if node.left.is_auto_deref_var() {
								left_type = left_type.deref()
							}
							c.check_expected(left_type, elem_type) or {
								c.error('left operand to `${node.op}` does not match the array element type: ${err.msg()}',
									left_right_pos)
							}
						}
					}
				}
				.map {
					map_info := right_final_sym.map_info()
					c.check_expected(left_type, map_info.key_type) or {
						c.error('left operand to `${node.op}` does not match the map key type: ${err.msg()}',
							left_right_pos)
					}
					node.left_type = map_info.key_type
				}
				.array_fixed {
					if left_sym.kind !in [.sum_type, .interface] {
						elem_type := right_final_sym.array_fixed_info().elem_type
						c.check_expected(left_type, elem_type) or {
							c.error('left operand to `${node.op}` does not match the fixed array element type: ${err.msg()}',
								left_right_pos)
						}
					}
				}
				else {
					if mut node.right is ast.RangeExpr {
						if !left_final_sym.is_number() && left_final_sym.kind != .rune {
							c.error('`${left_final_sym.name}` is an invalid type for range expression',
								node.pos)
						}
					} else {
						c.error('`${node.op.str()}` can only be used with arrays and maps',
							node.pos)
					}
				}
			}
			if mut node.left is ast.CallExpr {
				if node.left.return_type.has_flag(.option)
					|| node.left.return_type.has_flag(.result) {
					option_or_result := if node.left.return_type.has_flag(.option) {
						'option'
					} else {
						'result'
					}
					c.error('unwrapped ${option_or_result} cannot be used with `${node.op.str()}`',
						left_pos)
				}
			}
			node.promoted_type = ast.bool_type
			return ast.bool_type
		}
		.plus, .minus, .mul, .div, .mod, .xor, .amp, .pipe {
			// binary operators that expect matching types
			unwrapped_left_type := c.unwrap_generic(left_type)
			left_sym = c.table.sym(unwrapped_left_type)
			unwrapped_right_type := c.unwrap_generic(right_type)
			right_sym = c.table.sym(unwrapped_right_type)
			if mut right_sym.info is ast.Alias && (right_sym.info.language != .c
				&& c.mod == c.table.type_to_str(unwrapped_right_type).split('.')[0]
				&& (right_final_sym.is_primitive() || right_final_sym.kind == .enum)) {
				right_sym = unsafe { right_final_sym }
			}
			if mut left_sym.info is ast.Alias && (left_sym.info.language != .c
				&& c.mod == c.table.type_to_str(unwrapped_left_type).split('.')[0]
				&& (left_final_sym.is_primitive() || left_final_sym.kind == .enum)) {
				left_sym = unsafe { left_final_sym }
			}
			op_str := node.op.str()
			if c.pref.translated && node.op in [.plus, .minus, .mul]
				&& unwrapped_left_type.is_any_kind_of_pointer()
				&& unwrapped_right_type.is_any_kind_of_pointer() {
				return_type = left_type
			} else if !c.pref.translated && left_sym.info is ast.Alias
				&& !left_final_sym.is_primitive() {
				if left_sym.has_method(op_str) {
					if method := left_sym.find_method(op_str) {
						return_type = method.return_type
					} else {
						return_type = left_type
					}
				} else if left_final_sym.has_method_with_generic_parent(op_str) {
					if method := left_final_sym.find_method_with_generic_parent(op_str) {
						return_type = method.return_type
					} else {
						return_type = left_type
					}
				} else {
					left_name := c.table.type_to_str(unwrapped_left_type)
					right_name := c.table.type_to_str(unwrapped_right_type)
					if left_name == right_name {
						c.error('undefined operation `${left_name}` ${op_str} `${right_name}`',
							left_right_pos)
					} else {
						c.error('mismatched types `${left_name}` and `${right_name}`',
							left_right_pos)
					}
				}
			} else if !c.pref.translated && right_sym.info is ast.Alias
				&& !right_final_sym.is_primitive() {
				if right_sym.has_method(op_str) {
					if method := right_sym.find_method(op_str) {
						return_type = method.return_type
					} else {
						return_type = right_type
					}
				} else if right_final_sym.has_method_with_generic_parent(op_str) {
					if method := right_final_sym.find_method_with_generic_parent(op_str) {
						return_type = method.return_type
					} else {
						return_type = right_type
					}
				} else if left_sym.has_method(op_str) {
					if method := left_sym.find_method(op_str) {
						return_type = method.return_type
					} else {
						return_type = left_type
					}
				} else if left_final_sym.has_method_with_generic_parent(op_str) {
					if method := left_final_sym.find_method_with_generic_parent(op_str) {
						return_type = method.return_type
					} else {
						return_type = left_type
					}
				} else {
					left_name := c.table.type_to_str(unwrapped_left_type)
					right_name := c.table.type_to_str(unwrapped_right_type)
					if left_name == right_name {
						c.error('undefined operation `${left_name}` ${op_str} `${right_name}`',
							left_right_pos)
					} else {
						c.error('mismatched types `${left_name}` and `${right_name}`',
							left_right_pos)
					}
				}
			}

			if ((unwrapped_left_type.is_ptr() && !node.left.is_auto_deref_var())
				|| (unwrapped_right_type.is_ptr() && !node.right.is_auto_deref_var()))
				&& node.op !in [.plus, .minus] {
				c.error('infix `${node.op}` is not defined for pointer values', left_right_pos)
			}

			if !c.pref.translated && left_sym.kind in [.array, .array_fixed, .map, .struct] {
				if left_sym.has_method_with_generic_parent(op_str) {
					if method := left_sym.find_method_with_generic_parent(op_str) {
						return_type = method.return_type
					} else {
						return_type = left_type
					}
				} else {
					left_name := c.table.type_to_str(unwrapped_left_type)
					right_name := c.table.type_to_str(unwrapped_right_type)
					if left_name == right_name {
						c.error('undefined operation `${left_name}` ${op_str} `${right_name}`',
							left_right_pos)
					} else {
						c.error('mismatched types `${left_name}` and `${right_name}`',
							left_right_pos)
					}
				}
			} else if !c.pref.translated && right_sym.kind in [.array, .array_fixed, .map, .struct] {
				if right_sym.has_method_with_generic_parent(op_str) {
					if method := right_sym.find_method_with_generic_parent(op_str) {
						return_type = method.return_type
					} else {
						return_type = right_type
					}
				} else {
					left_name := c.table.type_to_str(unwrapped_left_type)
					right_name := c.table.type_to_str(unwrapped_right_type)
					if left_name == right_name {
						c.error('undefined operation `${left_name}` ${op_str} `${right_name}`',
							left_right_pos)
					} else {
						c.error('mismatched types `${left_name}` and `${right_name}`',
							left_right_pos)
					}
				}
			} else if node.left.is_auto_deref_var() || node.right.is_auto_deref_var() {
				deref_left_type := if node.left.is_auto_deref_var() {
					unwrapped_left_type.deref()
				} else {
					unwrapped_left_type
				}
				deref_right_type := if node.right.is_auto_deref_var() {
					unwrapped_right_type.deref()
				} else {
					unwrapped_right_type
				}
				left_name := c.table.type_to_str(ast.mktyp(deref_left_type))
				right_name := c.table.type_to_str(ast.mktyp(deref_right_type))
				if left_name != right_name {
					c.error('mismatched types `${left_name}` and `${right_name}`', left_right_pos)
				}
			} else {
				unaliased_left_type := c.table.unalias_num_type(unwrapped_left_type)
				unalias_right_type := c.table.unalias_num_type(unwrapped_right_type)
				mut promoted_type := c.promote_keeping_aliases(unaliased_left_type, unalias_right_type,
					left_sym.kind, right_sym.kind)
				// subtract pointers is allowed in unsafe block
				is_allowed_pointer_arithmetic := left_type.is_any_kind_of_pointer()
					&& right_type.is_any_kind_of_pointer() && node.op == .minus
				if is_allowed_pointer_arithmetic {
					promoted_type = ast.int_type
				}
				if promoted_type.idx() == ast.void_type_idx {
					left_name := c.table.type_to_str(unwrapped_left_type)
					right_name := c.table.type_to_str(unwrapped_right_type)
					c.error('mismatched types `${left_name}` and `${right_name}`', left_right_pos)
				} else if promoted_type.has_option_or_result() {
					s := c.table.type_to_str(promoted_type)
					c.error('`${node.op}` cannot be used with `${s}`', node.pos)
				} else if promoted_type.is_float() {
					if node.op in [.mod, .xor, .amp, .pipe] {
						side := if unwrapped_left_type == promoted_type { 'left' } else { 'right' }
						pos := if unwrapped_left_type == promoted_type {
							left_pos
						} else {
							right_pos
						}
						name := if unwrapped_left_type == promoted_type {
							left_sym.name
						} else {
							right_sym.name
						}
						if node.op == .mod {
							c.error('float modulo not allowed, use math.fmod() instead',
								pos)
						} else {
							c.error('${side} type of `${op_str}` cannot be non-integer type `${name}`',
								pos)
						}
					}
				}
				if node.op in [.div, .mod] {
					c.check_div_mod_by_zero(node.right, node.op)
				}

				left_sym = c.table.sym(unwrapped_left_type)
				right_sym = c.table.sym(unwrapped_right_type)
				if left_sym.info is ast.Alias && left_final_sym.is_primitive() {
					if left_sym.has_method(op_str) {
						if method := left_sym.find_method(op_str) {
							return_type = method.return_type
						}
					}
				} else if right_sym.info is ast.Alias && right_final_sym.is_primitive() {
					if right_sym.has_method(op_str) {
						if method := right_sym.find_method(op_str) {
							return_type = method.return_type
						}
					}
				}
				return_sym := c.table.sym(return_type)
				if return_sym.info !is ast.Alias {
					return_type = promoted_type
				}
			}
		}
		.gt, .lt, .ge, .le {
			unwrapped_left_type := c.unwrap_generic(left_type)
			left_sym = c.table.sym(unwrapped_left_type)
			if left_sym.kind == .alias && !left_sym.has_method_with_generic_parent(node.op.str()) {
				left_sym = c.table.final_sym(unwrapped_left_type)
			}
			unwrapped_right_type := c.unwrap_generic(right_type)
			right_sym = c.table.sym(unwrapped_right_type)
			if right_sym.kind == .alias && !right_sym.has_method_with_generic_parent(node.op.str()) {
				right_sym = c.table.final_sym(unwrapped_right_type)
			}
			if left_sym.kind in [.array, .array_fixed] && right_sym.kind in [.array, .array_fixed] {
				c.error('only `==` and `!=` are defined on arrays', node.pos)
			} else if left_sym.info is ast.Struct && left_sym.info.generic_types.len > 0 {
				node.promoted_type = ast.bool_type
				return ast.bool_type
			} else if left_sym.kind == .struct && right_sym.kind == .struct && node.op in [.eq, .lt] {
				if !(left_sym.has_method(node.op.str()) && right_sym.has_method(node.op.str())) {
					left_name := c.table.type_to_str(unwrapped_left_type)
					right_name := c.table.type_to_str(unwrapped_right_type)
					if left_name == right_name {
						if !(node.op == .lt && c.pref.translated) {
							// Allow `&Foo < &Foo` in translated code.
							// TODO: maybe in unsafe as well?
							c.error('undefined operation `${left_name}` ${node.op.str()} `${right_name}`',
								left_right_pos)
						}
					} else {
						c.error('mismatched types `${left_name}` and `${right_name}`',
							left_right_pos)
					}
				}
			}
			if left_sym.kind == .struct && right_sym.kind == .struct {
				if !left_sym.has_method('<') && node.op in [.ge, .le] {
					c.error('cannot use `${node.op}` as `<` operator method is not defined',
						left_right_pos)
				} else if !left_sym.has_method('<') && node.op == .gt {
					c.error('cannot use `>` as `<=` operator method is not defined', left_right_pos)
				}
			} else if left_type.has_flag(.generic) && right_type.has_flag(.generic) {
				// Try to unwrap the generic type to make sure that
				// the below check works as expected
				left_gen_type := c.unwrap_generic(left_type)
				gen_sym := c.table.sym(left_gen_type)
				need_overload := gen_sym.kind in [.struct, .interface]
				if need_overload && !gen_sym.has_method_with_generic_parent('<')
					&& node.op in [.ge, .le] {
					c.error('cannot use `${node.op}` as `<` operator method is not defined',
						left_right_pos)
				} else if need_overload && !gen_sym.has_method_with_generic_parent('<')
					&& node.op == .gt {
					c.error('cannot use `>` as `<=` operator method is not defined', left_right_pos)
				}
			} else if left_type in ast.integer_type_idxs && right_type in ast.integer_type_idxs {
				is_left_type_signed := left_type in ast.signed_integer_type_idxs
					|| left_type == ast.int_literal_type_idx
				is_right_type_signed := right_type in ast.signed_integer_type_idxs
					|| right_type == ast.int_literal_type_idx
				if is_left_type_signed != is_right_type_signed {
					if is_right_type_signed {
						if mut node.right is ast.IntegerLiteral {
							if node.right.val.int() < 0 {
								c.error('unsigned integer cannot be compared with negative value',
									node.right.pos)
							}
						}
					} else if is_left_type_signed {
						if mut node.left is ast.IntegerLiteral {
							if node.left.val.int() < 0 {
								c.error('unsigned integer cannot be compared with negative value',
									node.left.pos)
							}
						}
					}
				}
			} else if node.left !in [ast.Ident, ast.SelectorExpr, ast.ComptimeSelector]
				&& (left_type.has_flag(.option) || right_type.has_flag(.option)) {
				opt_comp_pos := if left_type.has_flag(.option) { left_pos } else { right_pos }
				c.error('unwrapped Option cannot be compared in an infix expression',
					opt_comp_pos)
			}
			if node.left.is_nil() || node.right.is_nil() {
				c.error('cannot use `${node.op.str()}` with `nil`', node.pos)
			}
		}
		.key_like {
			node.promoted_type = ast.bool_type

			return c.check_like_operator(node)
		}
		.key_ilike {
			node.promoted_type = ast.bool_type

			return c.check_like_operator(node)
		}
		.left_shift {
			if left_final_sym.kind == .array
				|| c.table.sym(c.unwrap_generic(left_type)).kind == .array {
				return c.check_append(mut node, left_type, right_type, right_final_sym)
			} else {
				node.promoted_type = c.check_shift(mut node, left_type, right_type)
				return node.promoted_type
			}
		}
		.right_shift {
			node.promoted_type = c.check_shift(mut node, left_type, right_type)
			return node.promoted_type
		}
		.unsigned_right_shift {
			modified_left_type := if !left_type.is_int() {
				c.error('invalid operation: shift on type `${c.table.sym(left_type).name}`',
					left_pos)
				ast.void_type
			} else if left_type.is_int_literal() {
				// int literal => i64
				ast.u32_type
			} else if left_type.is_unsigned() {
				left_type
			} else {
				// signed types' idx adds with 5 will get correct relative unsigned type
				// i8 	=> byte
				// i16 	=> u16
				// int  	=> u32
				// i64  	=> u64
				// isize	=> usize
				// i128 	=> u128 NOT IMPLEMENTED YET
				ast.idx_to_type(match left_type.idx() {
					ast.i8_type_idx { ast.u8_type_idx }
					ast.i16_type_idx { ast.u16_type_idx }
					ast.i32_type_idx { ast.u32_type_idx }
					ast.int_type_idx { ast.u32_type_idx }
					ast.i64_type_idx { ast.u64_type_idx }
					ast.isize_type_idx { ast.usize_type_idx }
					else { 0 }
				})
			}

			if modified_left_type == 0 {
				return ast.void_type
			}

			node = ast.InfixExpr{
				left:        ast.CastExpr{
					expr:      node.left
					typ:       modified_left_type
					typname:   c.table.type_str(modified_left_type)
					expr_type: left_type
					pos:       node.pos
				}
				left_type:   left_type
				op:          .right_shift
				right:       node.right
				right_type:  right_type
				is_stmt:     false
				pos:         node.pos
				auto_locked: node.auto_locked
				or_block:    node.or_block
			}

			node.promoted_type = c.check_shift(mut node, left_type, right_type)

			return node.promoted_type
		}
		.key_is, .not_is {
			right_expr := node.right
			mut typ := match right_expr {
				ast.TypeNode {
					right_expr.typ
				}
				ast.None {
					ast.none_type_idx
				}
				ast.Ident {
					if right_expr.name == c.comptime.comptime_for_variant_var {
						c.type_resolver.get_ct_type_or_default('${c.comptime.comptime_for_variant_var}.typ',
							ast.void_type)
					} else {
						c.error('invalid type `${right_expr}`', right_expr.pos)
						ast.no_type
					}
				}
				else {
					c.error('invalid type `${right_expr}`', right_expr.pos())
					ast.no_type
				}
			}
			if typ != ast.no_type {
				typ_sym := c.table.sym(typ)
				op := node.op.str()
				if left_type.has_flag(.option) && !c.inside_sql {
					c.error('${node.left} is an Optional, it needs to be unwrapped first',
						node.left.pos())
				}
				if typ_sym.kind == .placeholder {
					c.error('${op}: type `${typ_sym.name}` does not exist', right_expr.pos())
				}
				if mut left_sym.info is ast.Aggregate {
					parent_left_type := left_sym.info.sum_type
					left_sym = c.table.sym(parent_left_type)
				}
				if c.inside_sql {
					if typ != ast.none_type_idx {
						c.error('`${op}` can only be used to test for none in sql', node.pos)
					}
				} else if left_sym.kind !in [.interface, .sum_type]
					&& !c.comptime.is_comptime(node.left) {
					c.error('`${op}` can only be used with interfaces and sum types',
						node.pos) // can be used in sql too, but keep err simple
				} else if mut left_sym.info is ast.SumType {
					if typ !in left_sym.info.variants
						&& c.unwrap_generic(typ) !in left_sym.info.variants {
						c.error('`${left_sym.name}` has no variant `${right_sym.name}`',
							right_pos)
					}
				} else if left_sym.info is ast.Interface {
					if typ_sym.kind != .interface && !c.type_implements(typ, left_type, right_pos) {
						c.error("`${typ_sym.name}` doesn't implement interface `${left_sym.name}`",
							right_pos)
					}
				}
			}
			node.promoted_type = ast.bool_type
			return ast.bool_type
		}
		.arrow { // `chan <- elem`
			if left_sym.kind == .chan {
				chan_info := left_sym.chan_info()
				elem_type := chan_info.elem_type
				if !c.check_types(right_type, elem_type) {
					c.error('cannot push `${c.table.type_to_str(right_type)}` on `${left_sym.name}`',
						right_pos)
				}
				if chan_info.is_mut {
					// TODO: The error message of the following could be more specific...
					c.fail_if_immutable(mut node.right)
				}
				if elem_type.is_ptr() && !right_type.is_ptr() {
					c.error('cannot push non-reference `${right_sym.name}` on `${left_sym.name}`',
						right_pos)
				} else if right_type.is_ptr() != elem_type.is_ptr() {
					c.error('cannot push `${c.table.type_to_str(right_type)}` on `${left_sym.name}`',
						right_pos)
				}
				c.stmts_ending_with_expression(mut node.or_block.stmts, c.expected_or_type)
			} else {
				c.error('cannot push on non-channel `${left_sym.name}`', left_pos)
			}
			return ast.void_type
		}
		.and, .logical_or {
			if !c.pref.translated && !c.file.is_translated {
				// TODO Bring back once I split void into void and bad
				// if left_final_sym.kind !in [.bool, .void] {
				if left_final_sym.kind != .bool {
					c.error('left operand for `${node.op}` is not a boolean', node.left.pos())
				}
				// if right_final_sym.kind !in [.bool, .void] {
				if right_final_sym.kind != .bool {
					c.error('right operand for `${node.op}` is not a boolean', node.right.pos())
				}
			}
			if mut node.left is ast.InfixExpr {
				if node.left.op != node.op && node.left.op in [.logical_or, .and] {
					// for example: `(a && b) || c` instead of `a && b || c`
					c.error('ambiguous boolean expression. use `()` to ensure correct order of operations',
						node.pos)
				}
			}
		}
		else {}
	}
	// Do an ambiguous expression check for << >> and &, since they all have the same precedence (unlike in C)
	if !c.is_builtin_mod && node.op in [.amp, .left_shift, .right_shift] {
		if mut node.left is ast.InfixExpr {
			if node.left.op != node.op && node.left.op in [.amp, .left_shift, .right_shift] {
				// for example: `(a << b) & c` instead of `a << b & c`
				c.note('ambiguous expression. use `()` to ensure correct order of operations',
					node.pos)
			}
		}
	}
	// TODO: Absorb this block into the above single side check block to accelerate.
	if left_type == ast.bool_type && node.op !in [.eq, .ne, .logical_or, .and] {
		c.error('bool types only have the following operators defined: `==`, `!=`, `||`, and `&&`',
			node.pos)
	} else if left_type == ast.string_type && node.op !in [.plus, .eq, .ne, .lt, .gt, .le, .ge] {
		// TODO: broken !in
		c.error('string types only have the following operators defined: `==`, `!=`, `<`, `>`, `<=`, `>=`, and `+`',
			node.pos)
	} else if !eq_ne && mut left_sym.info is ast.Enum && mut right_sym.info is ast.Enum {
		if left_sym.info.is_flag && right_sym.info.is_flag {
			// `@[flag]` tagged enums are a special case that allow also `|` and `&` binary operators
			if node.op !in [.pipe, .amp, .xor, .bit_not] {
				c.error('only `==`, `!=`, `|`, `&`, `^` and `~` are defined on `@[flag]` tagged `enum`, use an explicit cast to `int` if needed',
					node.pos)
			}
		} else if !c.pref.translated && !c.file.is_translated && !left_type.has_flag(.generic)
			&& !right_type.has_flag(.generic) {
			// Regular enums
			c.error('only `==` and `!=` are defined on `enum`, use an explicit cast to `int` if needed',
				node.pos)
		}
	}
	// sum types can't have any infix operation except of `is`, `eq`, `ne`.
	// `is` is checked before and doesn't reach this.
	if c.table.type_kind(left_type) == .sum_type && !eq_ne {
		c.error('cannot use operator `${node.op}` with `${left_sym.name}`', node.pos)
	} else if c.table.type_kind(right_type) == .sum_type && !eq_ne {
		c.error('cannot use operator `${node.op}` with `${right_sym.name}`', node.pos)
	}
	// TODO: move this to symmetric_check? Right now it would break `return 0` for `fn()?int `
	left_is_option := left_type.has_flag(.option)
	right_is_option := right_type.has_flag(.option)
	if left_is_option || right_is_option {
		opt_infix_pos := if left_is_option { left_pos } else { right_pos }
		if (node.left !in [ast.Ident, ast.IndexExpr, ast.SelectorExpr, ast.ComptimeSelector]
			|| (node.op in [.eq, .ne] && !right_is_option)
			|| node.op in [.lt, .gt, .le, .ge]) && right_sym.kind != .none && !c.inside_sql {
			c.error('unwrapped Option cannot be used in an infix expression', opt_infix_pos)
		}
	}

	left_is_result := left_type.has_flag(.result)
	right_is_result := right_type.has_flag(.result)
	if left_is_result || right_is_result {
		opt_infix_pos := if left_is_result { left_pos } else { right_pos }
		c.error('unwrapped Result cannot be used in an infix expression', opt_infix_pos)
	}

	// Dual sides check (compatibility check)
	if node.left !is ast.ComptimeCall && node.right !is ast.ComptimeCall {
		if left_type.has_flag(.generic) {
			left_type = c.unwrap_generic(left_type)
			left_sym = c.table.sym(left_type)
		}
		if right_type.has_flag(.generic) {
			right_type = c.unwrap_generic(right_type)
			right_sym = c.table.sym(right_type)
		}
		if !(c.symmetric_check(left_type, right_type) && c.symmetric_check(right_type, left_type))
			&& !c.pref.translated && !c.file.is_translated && !node.left.is_auto_deref_var()
			&& !node.right.is_auto_deref_var() {
			// for type-unresolved consts
			if left_type == ast.void_type || right_type == ast.void_type {
				return ast.void_type
			}
			if left_type.nr_muls() > 0 && right_type.is_int() {
				// pointer arithmetic is fine, it is checked in other places
				node.promoted_type = return_type
				return return_type
			}
			if node.right is ast.None && left_is_option {
				return ast.bool_type
			}
			c.error('infix expr: cannot use `${right_sym.name}` (right expression) as `${left_sym.name}`',
				left_right_pos)
		} else if left_type.is_ptr() {
			for_ptr_op := c.table.type_is_for_pointer_arithmetic(left_type)
			if left_sym.language == .v && !c.pref.translated && !c.inside_unsafe && !for_ptr_op
				&& right_type.is_int() {
				sugg := ' (you can use it inside an `unsafe` block)'
				c.error('infix expr: cannot use `${right_sym.name}` (right expression) as `${left_sym.name}` ${sugg}',
					left_right_pos)
			}
		}
	}
	if node.op == .plus && c.pref.warn_about_allocs && left_type == ast.string_type_idx
		&& right_type == ast.string_type_idx {
		c.warn_alloc('string concatenation', node.pos)
	}
	/*
	if (node.left is ast.InfixExpr && node.left.op == .inc) ||
		(node.right is ast.InfixExpr && node.right.op == .inc) {
		c.warn('`++` and `--` are statements, not expressions', node.pos)
	}
	*/
	node.promoted_type = if node.op.is_relational() { ast.bool_type } else { return_type }
	return node.promoted_type
}

fn (mut c Checker) check_div_mod_by_zero(expr ast.Expr, op_kind token.Kind) {
	match expr {
		ast.FloatLiteral {
			if expr.val.f64() == 0.0 {
				oper := if op_kind == .div { 'division' } else { 'modulo' }
				c.error('${oper} by zero', expr.pos)
			}
		}
		ast.IntegerLiteral {
			if expr.val.int() == 0 {
				oper := if op_kind == .div { 'division' } else { 'modulo' }
				c.error('${oper} by zero', expr.pos)
			}
		}
		ast.CastExpr {
			c.check_div_mod_by_zero(expr.expr, op_kind)
		}
		else {}
	}
}

fn (mut c Checker) check_duplicated_items(node &ast.ArrayInit) {
	mut unique_items := []string{cap: node.exprs.len}
	for item in node.exprs {
		item_str := item.str()
		if item_str in unique_items {
			c.note('item `${item_str}` is duplicated in the list', item.pos())
		} else {
			unique_items << item_str
		}
	}
}

fn (mut c Checker) check_like_operator(node &ast.InfixExpr) ast.Type {
	if node.left !is ast.Ident || !node.left_type.is_string() {
		c.error('the left operand of the `like` operator must be an identifier with a string type',
			node.left.pos())
	}

	if !node.right_type.is_string() {
		c.error('the right operand of the `like` operator must be a string type', node.right.pos())
	}

	return node.promoted_type
}

fn (mut c Checker) invalid_operator_error(op token.Kind, left_type ast.Type, right_type ast.Type, pos token.Pos) {
	left_name := c.table.type_to_str(left_type)
	right_name := c.table.type_to_str(right_type)
	c.error('invalid operator `${op}` to `${left_name}` and `${right_name}`', pos)
}

// `if node is ast.Ident && node.is_mut { ... }` -> `if node is ast.Ident && (node as ast.Ident).is_mut { ... }`
fn (mut c Checker) autocast_in_if_conds(mut right ast.Expr, from_expr ast.Expr, from_type ast.Type, to_type ast.Type) {
	if '${right}' == from_expr.str() {
		right = ast.AsCast{
			typ:       to_type
			expr:      from_expr
			expr_type: from_type
		}
		return
	}

	match mut right {
		ast.SelectorExpr {
			if right.expr.str() == from_expr.str() {
				right.expr = ast.ParExpr{
					expr: ast.AsCast{
						typ:       to_type
						expr:      from_expr
						expr_type: from_type
					}
				}
			} else {
				c.autocast_in_if_conds(mut right.expr, from_expr, from_type, to_type)
			}
		}
		ast.ParExpr {
			c.autocast_in_if_conds(mut right.expr, from_expr, from_type, to_type)
		}
		ast.AsCast {
			if right.typ != to_type {
				c.autocast_in_if_conds(mut right.expr, from_expr, from_type, to_type)
			}
		}
		ast.PrefixExpr {
			c.autocast_in_if_conds(mut right.right, from_expr, from_type, to_type)
		}
		ast.CallExpr {
			if right.left.str() == from_expr.str()
				&& c.table.sym(to_type).has_method_with_generic_parent(right.name) {
				right.left = ast.ParExpr{
					expr: ast.AsCast{
						typ:       to_type
						expr:      from_expr
						expr_type: from_type
					}
				}
			}
			if right.left !is ast.Ident {
				c.autocast_in_if_conds(mut right.left, from_expr, from_type, to_type)
			}
			for mut arg in right.args {
				c.autocast_in_if_conds(mut arg.expr, from_expr, from_type, to_type)
			}
		}
		ast.InfixExpr {
			c.autocast_in_if_conds(mut right.left, from_expr, from_type, to_type)
			c.autocast_in_if_conds(mut right.right, from_expr, from_type, to_type)
		}
		ast.IndexExpr {
			c.autocast_in_if_conds(mut right.left, from_expr, from_type, to_type)
			c.autocast_in_if_conds(mut right.index, from_expr, from_type, to_type)
		}
		ast.RangeExpr {
			c.autocast_in_if_conds(mut right.low, from_expr, from_type, to_type)
			c.autocast_in_if_conds(mut right.high, from_expr, from_type, to_type)
		}
		ast.StringInterLiteral {
			for mut expr in right.exprs {
				c.autocast_in_if_conds(mut expr, from_expr, from_type, to_type)
			}
		}
		ast.UnsafeExpr {
			c.autocast_in_if_conds(mut right.expr, from_expr, from_type, to_type)
		}
		else {}
	}
}

fn (mut c Checker) check_sort_external_variable_access(node ast.Expr) bool {
	match node {
		ast.InfixExpr {
			if !c.check_sort_external_variable_access(node.left) {
				return false
			}
			if !c.check_sort_external_variable_access(node.right) {
				return false
			}
		}
		ast.Ident {
			if node.name !in ['a', 'b'] {
				c.error('can not access external variable `${node.name}`', node.pos)
				return false
			}
		}
		ast.CallExpr {
			return c.check_sort_external_variable_access(node.left)
		}
		ast.SelectorExpr {
			return c.check_sort_external_variable_access(node.expr)
		}
		ast.IndexExpr {
			if !c.check_sort_external_variable_access(node.left) {
				return false
			}
			if !c.check_sort_external_variable_access(node.index) {
				return false
			}
		}
		else {
			return true
		}
	}
	return true
}
