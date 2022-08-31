module checker

import v.ast
import v.pref
import v.token

pub fn (mut c Checker) infix_expr(mut node ast.InfixExpr) ast.Type {
	former_expected_type := c.expected_type
	defer {
		c.expected_type = former_expected_type
	}
	mut left_type := c.expr(node.left)
	node.left_type = left_type
	c.expected_type = left_type

	if node.op == .key_is {
		c.inside_x_is_type = true
	}
	mut right_type := c.expr(node.right)
	if node.op == .key_is {
		c.inside_x_is_type = false
	}
	if node.op == .amp && left_type.is_bool() && right_type.is_bool() && right_type.is_ptr() {
		pos := node.pos.extend(node.right.pos())
		c.error('the right expression should be separated from the `&&` by a space', pos)
		return ast.bool_type
	}
	node.right_type = right_type
	if left_type.is_number() && !left_type.is_ptr()
		&& right_type in [ast.int_literal_type, ast.float_literal_type] {
		node.right_type = left_type
	}
	if right_type.is_number() && !right_type.is_ptr()
		&& left_type in [ast.int_literal_type, ast.float_literal_type] {
		node.left_type = right_type
	}
	mut right_sym := c.table.sym(right_type)
	right_final_sym := c.table.final_sym(right_type)
	mut left_sym := c.table.sym(left_type)
	left_final_sym := c.table.final_sym(left_type)
	left_pos := node.left.pos()
	right_pos := node.right.pos()
	left_right_pos := left_pos.extend(right_pos)
	if left_type.is_any_kind_of_pointer()
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
				c.warn('pointer arithmetic is only allowed in `unsafe` blocks', left_right_pos)
			}
			if left_type == ast.voidptr_type && !c.pref.translated {
				c.error('`$node.op` cannot be used with `voidptr`', left_pos)
			}
		}
	}
	mut return_type := left_type

	if node.op != .key_is {
		match mut node.left {
			ast.Ident, ast.SelectorExpr {
				if node.left.is_mut {
					c.error('the `mut` keyword is invalid here', node.left.mut_pos)
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
			is_mismatch :=
				(left_sym.kind == .alias && right_sym.kind in [.struct_, .array, .sum_type])
				|| (right_sym.kind == .alias && left_sym.kind in [.struct_, .array, .sum_type])
			if is_mismatch {
				c.add_error_detail('left type: `${c.table.type_to_str(left_type)}` vs right type: `${c.table.type_to_str(right_type)}`')
				c.error('possible type mismatch of compared values of `$node.op` operation',
					left_right_pos)
			} else if left_type in ast.integer_type_idxs && right_type in ast.integer_type_idxs {
				is_left_type_signed := left_type in ast.signed_integer_type_idxs
				is_right_type_signed := right_type in ast.signed_integer_type_idxs
				if !is_left_type_signed && mut node.right is ast.IntegerLiteral {
					if node.right.val.int() < 0 && left_type in ast.int_promoted_type_idxs {
						lt := c.table.sym(left_type).name
						c.error('`$lt` cannot be compared with negative value', node.right.pos)
					}
				} else if !is_right_type_signed && mut node.left is ast.IntegerLiteral {
					if node.left.val.int() < 0 && right_type in ast.int_promoted_type_idxs {
						rt := c.table.sym(right_type).name
						c.error('negative value cannot be compared with `$rt`', node.left.pos)
					}
				} else if is_left_type_signed != is_right_type_signed
					&& left_type != ast.int_literal_type_idx
					&& right_type != ast.int_literal_type_idx {
					ls, _ := c.table.type_size(left_type)
					rs, _ := c.table.type_size(right_type)
					// prevent e.g. `u32 == i16` but not `u16 == i32` as max_u16 fits in i32
					// TODO u32 == i32, change < to <=
					if !c.pref.translated && ((is_left_type_signed && ls < rs)
						|| (is_right_type_signed && rs < ls)) {
						lt := c.table.sym(left_type).name
						rt := c.table.sym(right_type).name
						c.error('`$lt` cannot be compared with `$rt`', node.pos)
					}
				}
			}
		}
		.key_in, .not_in {
			match right_final_sym.kind {
				.array {
					if left_sym.kind !in [.sum_type, .interface_] {
						elem_type := right_final_sym.array_info().elem_type
						c.check_expected(left_type, elem_type) or {
							c.error('left operand to `$node.op` does not match the array element type: $err.msg()',
								left_right_pos)
						}
					}
				}
				.map {
					map_info := right_final_sym.map_info()
					c.check_expected(left_type, map_info.key_type) or {
						c.error('left operand to `$node.op` does not match the map key type: $err.msg()',
							left_right_pos)
					}
					node.left_type = map_info.key_type
				}
				.array_fixed {
					if left_sym.kind !in [.sum_type, .interface_] {
						elem_type := right_final_sym.array_fixed_info().elem_type
						c.check_expected(left_type, elem_type) or {
							c.error('left operand to `$node.op` does not match the fixed array element type: $err.msg()',
								left_right_pos)
						}
					}
				}
				else {
					c.error('`$node.op.str()` can only be used with arrays and maps',
						node.pos)
				}
			}
			return ast.bool_type
		}
		.plus, .minus, .mul, .div, .mod, .xor, .amp, .pipe {
			// binary operators that expect matching types
			if right_sym.info is ast.Alias && (right_sym.info as ast.Alias).language != .c
				&& c.mod == c.table.type_to_str(right_type).split('.')[0]
				&& c.table.sym((right_sym.info as ast.Alias).parent_type).is_primitive() {
				right_sym = c.table.sym((right_sym.info as ast.Alias).parent_type)
			}
			if left_sym.info is ast.Alias && (left_sym.info as ast.Alias).language != .c
				&& c.mod == c.table.type_to_str(left_type).split('.')[0]
				&& c.table.sym((left_sym.info as ast.Alias).parent_type).is_primitive() {
				left_sym = c.table.sym((left_sym.info as ast.Alias).parent_type)
			}

			if c.pref.translated && node.op in [.plus, .minus, .mul]
				&& left_type.is_any_kind_of_pointer() && right_type.is_any_kind_of_pointer() {
				return_type = left_type
			} else if !c.pref.translated && left_sym.kind == .alias && left_sym.info is ast.Alias
				&& !(c.table.sym((left_sym.info as ast.Alias).parent_type).is_primitive()) {
				if left_sym.has_method(node.op.str()) {
					if method := left_sym.find_method(node.op.str()) {
						return_type = method.return_type
					} else {
						return_type = left_type
					}
				} else if left_final_sym.has_method(node.op.str()) {
					if method := left_final_sym.find_method(node.op.str()) {
						return_type = method.return_type
					} else {
						return_type = left_type
					}
				} else {
					left_name := c.table.type_to_str(left_type)
					right_name := c.table.type_to_str(right_type)
					if left_name == right_name {
						c.error('undefined operation `$left_name` $node.op.str() `$right_name`',
							left_right_pos)
					} else {
						c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
					}
				}
			} else if !c.pref.translated && right_sym.kind == .alias && right_sym.info is ast.Alias
				&& !(c.table.sym((right_sym.info as ast.Alias).parent_type).is_primitive()) {
				if right_sym.has_method(node.op.str()) {
					if method := right_sym.find_method(node.op.str()) {
						return_type = method.return_type
					} else {
						return_type = right_type
					}
				} else if right_final_sym.has_method(node.op.str()) {
					if method := right_final_sym.find_method(node.op.str()) {
						return_type = method.return_type
					} else {
						return_type = right_type
					}
				} else {
					left_name := c.table.type_to_str(left_type)
					right_name := c.table.type_to_str(right_type)
					if left_name == right_name {
						c.error('undefined operation `$left_name` $node.op.str() `$right_name`',
							left_right_pos)
					} else {
						c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
					}
				}
			}

			if !c.pref.translated && left_sym.kind in [.array, .array_fixed, .map, .struct_] {
				if left_sym.has_method_with_generic_parent(node.op.str()) {
					if method := left_sym.find_method_with_generic_parent(node.op.str()) {
						return_type = method.return_type
					} else {
						return_type = left_type
					}
				} else {
					left_name := c.table.type_to_str(left_type)
					right_name := c.table.type_to_str(right_type)
					if left_name == right_name {
						c.error('undefined operation `$left_name` $node.op.str() `$right_name`',
							left_right_pos)
					} else {
						c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
					}
				}
			} else if !c.pref.translated && right_sym.kind in [.array, .array_fixed, .map, .struct_] {
				if right_sym.has_method_with_generic_parent(node.op.str()) {
					if method := right_sym.find_method_with_generic_parent(node.op.str()) {
						return_type = method.return_type
					} else {
						return_type = right_type
					}
				} else {
					left_name := c.table.type_to_str(left_type)
					right_name := c.table.type_to_str(right_type)
					if left_name == right_name {
						c.error('undefined operation `$left_name` $node.op.str() `$right_name`',
							left_right_pos)
					} else {
						c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
					}
				}
			} else if node.left.is_auto_deref_var() || node.right.is_auto_deref_var() {
				deref_left_type := if node.left.is_auto_deref_var() {
					left_type.deref()
				} else {
					left_type
				}
				deref_right_type := if node.right.is_auto_deref_var() {
					right_type.deref()
				} else {
					right_type
				}
				left_name := c.table.type_to_str(ast.mktyp(deref_left_type))
				right_name := c.table.type_to_str(ast.mktyp(deref_right_type))
				if left_name != right_name {
					c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
				}
			} else {
				unaliased_left_type := c.table.unalias_num_type(left_type)
				unalias_right_type := c.table.unalias_num_type(right_type)
				mut promoted_type := c.promote_keeping_aliases(unaliased_left_type, unalias_right_type,
					left_sym.kind, right_sym.kind)
				// substract pointers is allowed in unsafe block
				is_allowed_pointer_arithmetic := left_type.is_any_kind_of_pointer()
					&& right_type.is_any_kind_of_pointer() && node.op == .minus
				if is_allowed_pointer_arithmetic {
					promoted_type = ast.int_type
				}
				if promoted_type.idx() == ast.void_type_idx {
					left_name := c.table.type_to_str(left_type)
					right_name := c.table.type_to_str(right_type)
					c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
				} else if promoted_type.has_flag(.optional) {
					s := c.table.type_to_str(promoted_type)
					c.error('`$node.op` cannot be used with `$s`', node.pos)
				} else if promoted_type.is_float() {
					if node.op in [.mod, .xor, .amp, .pipe] {
						side := if left_type == promoted_type { 'left' } else { 'right' }
						pos := if left_type == promoted_type { left_pos } else { right_pos }
						name := if left_type == promoted_type {
							left_sym.name
						} else {
							right_sym.name
						}
						if node.op == .mod {
							c.error('float modulo not allowed, use math.fmod() instead',
								pos)
						} else {
							c.error('$side type of `$node.op.str()` cannot be non-integer type `$name`',
								pos)
						}
					}
				}
				if node.op in [.div, .mod] {
					c.check_div_mod_by_zero(node.right, node.op)
				}

				return_type = promoted_type
			}
		}
		.gt, .lt, .ge, .le {
			if left_sym.kind in [.array, .array_fixed] && right_sym.kind in [.array, .array_fixed] {
				c.error('only `==` and `!=` are defined on arrays', node.pos)
			} else if left_sym.kind == .struct_
				&& (left_sym.info as ast.Struct).generic_types.len > 0 {
				return ast.bool_type
			} else if left_sym.kind == .struct_ && right_sym.kind == .struct_
				&& node.op in [.eq, .lt] {
				if !(left_sym.has_method(node.op.str()) && right_sym.has_method(node.op.str())) {
					left_name := c.table.type_to_str(left_type)
					right_name := c.table.type_to_str(right_type)
					if left_name == right_name {
						if !(node.op == .lt && c.pref.translated) {
							// Allow `&Foo < &Foo` in translated code.
							// TODO maybe in unsafe as well?
							c.error('undefined operation `$left_name` $node.op.str() `$right_name`',
								left_right_pos)
						}
					} else {
						c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
					}
				}
			}
			if left_sym.kind == .struct_ && right_sym.kind == .struct_ {
				if !left_sym.has_method('<') && node.op in [.ge, .le] {
					c.error('cannot use `$node.op` as `<` operator method is not defined',
						left_right_pos)
				} else if !left_sym.has_method('<') && node.op == .gt {
					c.error('cannot use `>` as `<=` operator method is not defined', left_right_pos)
				}
			} else if left_type.has_flag(.generic) && right_type.has_flag(.generic) {
				// Try to unwrap the generic type to make sure that
				// the below check works as expected
				left_gen_type := c.unwrap_generic(left_type)
				gen_sym := c.table.sym(left_gen_type)
				need_overload := gen_sym.kind in [.struct_, .interface_]
				if need_overload && !gen_sym.has_method_with_generic_parent('<')
					&& node.op in [.ge, .le] {
					c.error('cannot use `$node.op` as `<` operator method is not defined',
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
			} else if left_type.has_flag(.optional) || right_type.has_flag(.optional) {
				opt_comp_pos := if left_type.has_flag(.optional) { left_pos } else { right_pos }
				c.error('unwrapped optional cannot be compared in an infix expression',
					opt_comp_pos)
			}
		}
		.left_shift {
			if left_final_sym.kind == .array {
				if !node.is_stmt {
					c.error('array append cannot be used in an expression', node.pos)
				}
				// `array << elm`
				c.check_expr_opt_call(node.right, right_type)
				node.auto_locked, _ = c.fail_if_immutable(node.left)
				left_value_type := c.table.value_type(c.unwrap_generic(left_type))
				left_value_sym := c.table.sym(c.unwrap_generic(left_value_type))
				if left_value_sym.kind == .interface_ {
					if right_final_sym.kind != .array {
						// []Animal << Cat
						if c.type_implements(right_type, left_value_type, right_pos) {
							if !right_type.is_ptr() && !right_type.is_pointer() && !c.inside_unsafe
								&& right_sym.kind != .interface_ {
								c.mark_as_referenced(mut &node.right, true)
							}
						}
					} else {
						// []Animal << []Cat
						c.type_implements(c.table.value_type(right_type), left_value_type,
							right_pos)
					}
					return ast.void_type
				} else if left_value_sym.kind == .sum_type {
					if right_sym.kind != .array {
						if !c.table.is_sumtype_or_in_variant(left_value_type, ast.mktyp(right_type)) {
							c.error('cannot append `$right_sym.name` to `$left_sym.name`',
								right_pos)
						}
					} else {
						right_value_type := c.table.value_type(right_type)
						if !c.table.is_sumtype_or_in_variant(left_value_type, ast.mktyp(right_value_type)) {
							c.error('cannot append `$right_sym.name` to `$left_sym.name`',
								right_pos)
						}
					}
					return ast.void_type
				}
				// []T << T or []T << []T
				unwrapped_right_type := c.unwrap_generic(right_type)
				if c.check_types(unwrapped_right_type, left_value_type) {
					// []&T << T is wrong: we check for that, !(T.is_ptr()) && ?(&T).is_ptr()
					if !(!unwrapped_right_type.is_ptr() && left_value_type.is_ptr()
						&& left_value_type.share() == .mut_t) {
						return ast.void_type
					}
				} else if c.check_types(unwrapped_right_type, c.unwrap_generic(left_type)) {
					return ast.void_type
				}
				c.error('cannot append `$right_sym.name` to `$left_sym.name`', right_pos)
				return ast.void_type
			} else {
				return c.check_shift(mut node, left_type, right_type)
			}
		}
		.right_shift {
			return c.check_shift(mut node, left_type, right_type)
		}
		.unsigned_right_shift {
			modified_left_type := if !left_type.is_int() {
				c.error('invalid operation: shift on type `${c.table.sym(left_type).name}`',
					left_pos)
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

			if modified_left_type == 0 {
				return ast.void_type
			}

			node = ast.InfixExpr{
				left: ast.CastExpr{
					expr: node.left
					typ: modified_left_type
					typname: c.table.type_str(modified_left_type)
					pos: node.pos
				}
				left_type: left_type
				op: .right_shift
				right: node.right
				right_type: right_type
				is_stmt: false
				pos: node.pos
				auto_locked: node.auto_locked
				or_block: node.or_block
			}

			return c.check_shift(mut node, left_type, right_type)
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
				else {
					c.error('invalid type `$right_expr`', right_expr.pos())
					ast.Type(0)
				}
			}
			if typ != ast.Type(0) {
				typ_sym := c.table.sym(typ)
				op := node.op.str()
				if typ_sym.kind == .placeholder {
					c.error('$op: type `$typ_sym.name` does not exist', right_expr.pos())
				}
				if left_sym.kind == .aggregate {
					parent_left_type := (left_sym.info as ast.Aggregate).sum_type
					left_sym = c.table.sym(parent_left_type)
				}
				if left_sym.kind !in [.interface_, .sum_type] {
					c.error('`$op` can only be used with interfaces and sum types', node.pos)
				} else if mut left_sym.info is ast.SumType {
					if typ !in left_sym.info.variants {
						c.error('`$left_sym.name` has no variant `$right_sym.name`', node.pos)
					}
				}
			}
			return ast.bool_type
		}
		.arrow { // `chan <- elem`
			if left_sym.kind == .chan {
				chan_info := left_sym.chan_info()
				elem_type := chan_info.elem_type
				if !c.check_types(right_type, elem_type) {
					c.error('cannot push `$right_sym.name` on `$left_sym.name`', right_pos)
				}
				if chan_info.is_mut {
					// TODO: The error message of the following could be more specific...
					c.fail_if_immutable(node.right)
				}
				if elem_type.is_ptr() && !right_type.is_ptr() {
					c.error('cannot push non-reference `$right_sym.name` on `$left_sym.name`',
						right_pos)
				}
				c.stmts_ending_with_expression(node.or_block.stmts)
			} else {
				c.error('cannot push on non-channel `$left_sym.name`', left_pos)
			}
			return ast.void_type
		}
		.and, .logical_or {
			if !c.pref.translated && !c.file.is_translated {
				if node.left_type != ast.bool_type_idx {
					c.error('left operand for `$node.op` is not a boolean', node.left.pos())
				}
				if node.right_type != ast.bool_type_idx {
					c.error('right operand for `$node.op` is not a boolean', node.right.pos())
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
	// TODO: Absorb this block into the above single side check block to accelerate.
	if left_type == ast.bool_type && node.op !in [.eq, .ne, .logical_or, .and] {
		c.error('bool types only have the following operators defined: `==`, `!=`, `||`, and `&&`',
			node.pos)
	} else if left_type == ast.string_type && node.op !in [.plus, .eq, .ne, .lt, .gt, .le, .ge] {
		// TODO broken !in
		c.error('string types only have the following operators defined: `==`, `!=`, `<`, `>`, `<=`, `>=`, and `+`',
			node.pos)
	} else if left_sym.kind == .enum_ && right_sym.kind == .enum_ && !eq_ne {
		left_enum := left_sym.info as ast.Enum
		right_enum := right_sym.info as ast.Enum
		if left_enum.is_flag && right_enum.is_flag {
			// `[flag]` tagged enums are a special case that allow also `|` and `&` binary operators
			if node.op !in [.pipe, .amp] {
				c.error('only `==`, `!=`, `|` and `&` are defined on `[flag]` tagged `enum`, use an explicit cast to `int` if needed',
					node.pos)
			}
		} else if !c.pref.translated && !c.file.is_translated {
			// Regular enums
			c.error('only `==` and `!=` are defined on `enum`, use an explicit cast to `int` if needed',
				node.pos)
		}
	}
	// sum types can't have any infix operation except of `is`, `eq`, `ne`.
	// `is` is checked before and doesn't reach this.
	if c.table.type_kind(left_type) == .sum_type && !eq_ne {
		c.error('cannot use operator `$node.op` with `$left_sym.name`', node.pos)
	} else if c.table.type_kind(right_type) == .sum_type && !eq_ne {
		c.error('cannot use operator `$node.op` with `$right_sym.name`', node.pos)
	}
	// TODO move this to symmetric_check? Right now it would break `return 0` for `fn()?int `
	left_is_optional := left_type.has_flag(.optional)
	right_is_optional := right_type.has_flag(.optional)
	if left_is_optional && right_is_optional {
		c.error('unwrapped optionals cannot be used in an infix expression', left_right_pos)
	} else if left_is_optional || right_is_optional {
		opt_infix_pos := if left_is_optional { left_pos } else { right_pos }
		c.error('unwrapped optional cannot be used in an infix expression', opt_infix_pos)
	}
	// Dual sides check (compatibility check)
	if node.left !is ast.ComptimeCall && node.right !is ast.ComptimeCall {
		if !(c.symmetric_check(left_type, right_type) && c.symmetric_check(right_type, left_type))
			&& !c.pref.translated && !c.file.is_translated && !node.left.is_auto_deref_var()
			&& !node.right.is_auto_deref_var() {
			// for type-unresolved consts
			if left_type == ast.void_type || right_type == ast.void_type {
				return ast.void_type
			}
			if left_type.nr_muls() > 0 && right_type.is_int() {
				// pointer arithmetic is fine, it is checked in other places
				return return_type
			}
			c.error('infix expr: cannot use `$right_sym.name` (right expression) as `$left_sym.name`',
				left_right_pos)
		} else if left_type.is_ptr() {
			for_ptr_op := c.table.type_is_for_pointer_arithmetic(left_type)
			if left_sym.language == .v && !c.pref.translated && !c.inside_unsafe && !for_ptr_op
				&& right_type.is_int() {
				sugg := ' (you can use it inside an `unsafe` block)'
				c.error('infix expr: cannot use `$right_sym.name` (right expression) as `$left_sym.name` $sugg',
					left_right_pos)
			}
		}
	}
	/*
	if (node.left is ast.InfixExpr &&
		(node.left as ast.InfixExpr).op == .inc) ||
		(node.right is ast.InfixExpr && (node.right as ast.InfixExpr).op == .inc) {
		c.warn('`++` and `--` are statements, not expressions', node.pos)
	}
	*/
	return if node.op.is_relational() { ast.bool_type } else { return_type }
}

fn (mut c Checker) check_div_mod_by_zero(expr ast.Expr, op_kind token.Kind) {
	match expr {
		ast.FloatLiteral {
			if expr.val.f64() == 0.0 {
				oper := if op_kind == .div { 'division' } else { 'modulo' }
				c.error('$oper by zero', expr.pos)
			}
		}
		ast.IntegerLiteral {
			if expr.val.int() == 0 {
				oper := if op_kind == .div { 'division' } else { 'modulo' }
				c.error('$oper by zero', expr.pos)
			}
		}
		ast.CastExpr {
			c.check_div_mod_by_zero(expr.expr, op_kind)
		}
		else {}
	}
}

pub fn (mut c Checker) invalid_operator_error(op token.Kind, left_type ast.Type, right_type ast.Type, pos token.Pos) {
	left_name := c.table.type_to_str(left_type)
	right_name := c.table.type_to_str(right_type)
	c.error('invalid operator `$op` to `$left_name` and `$right_name`', pos)
}
