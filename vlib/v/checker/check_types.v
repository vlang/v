// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module checker

import v.ast

fn (mut c Checker) check_shift(mut node ast.InfixExpr, left_type ast.Type, right_type ast.Type) ast.Type {
	if !left_type.is_int() {
		left_sym := c.table.sym(left_type)
		// maybe it's an int alias? TODO move this to is_int()?
		if left_sym.kind == .alias && (left_sym.info as ast.Alias).parent_type.is_int() {
			return left_type
		}
		if c.pref.translated && left_type == ast.bool_type {
			// allow `bool << 2` in translated C code
			return ast.int_type
		}
		c.error('invalid operation: shift on type `$left_sym.name`', node.left.pos())
		return ast.void_type
	}
	if !right_type.is_int() && !c.pref.translated {
		left_sym := c.table.sym(left_type)
		right_sym := c.table.sym(right_type)
		c.error('cannot shift non-integer type `$right_sym.name` into type `$left_sym.name`',
			node.right.pos())
		return ast.void_type
	}
	// At this point, it is guaranteed that we have a `number1 << number2`, or `number1 >> number2`, or `number1 >>> number2`:
	if !node.ct_left_value_evaled {
		if lval := c.eval_comptime_const_expr(node.left, 0) {
			node.ct_left_value_evaled = true
			node.ct_left_value = lval
		}
	}
	if !node.ct_right_value_evaled {
		if rval := c.eval_comptime_const_expr(node.right, 0) {
			node.ct_right_value_evaled = true
			node.ct_right_value = rval
		}
	}
	// if node.ct_left_value_evaled && node.ct_right_value_evaled {
	//	c.note('>>> node.ct_left_value: $node.ct_left_value | node.ct_right_value: $node.ct_right_value', node.pos)
	// }
	match node.op {
		.left_shift, .right_shift, .unsigned_right_shift {
			// The following code tries to disallow C UBs and IDs at the V level.
			// From the C++ standart (see https://pvs-studio.com/en/docs/warnings/v610/):
			// 1. The type of the result is that of the promoted left operand.
			// The behavior is undefined (UB), if the right operand is negative,
			// or greater than or equal to the length in bits of the promoted left operand.
			// 2. The value of E1 << E2 is E1 left-shifted E2 bit positions;
			// vacated bits are zero-filled. If E1 has an unsigned type,
			// the value of the result is E1 * 2^E2, reduced modulo one more
			// than the maximum value representable in the result type.
			// Otherwise, if E1 has a signed type and non-negative value,
			// and E1*2^E2 is representable in the result type, then that is
			// the resulting value; otherwise, the behavior is undefined (UB).
			// 3. The value of E1 >> E2 is E1 right-shifted E2 bit positions.
			// If E1 has an unsigned type, or if E1 has a signed type and a
			// non-negative value, the value of the result is the integral
			// part of the quotient of E1/2^E2. If E1 has a signed type and
			// a negative value, the resulting value is implementation-defined (ID).
			left_sym_final := c.table.final_sym(left_type)
			left_type_final := ast.Type(left_sym_final.idx)
			if node.op == .left_shift && left_type_final.is_signed() && !(c.inside_unsafe
				&& c.is_generated) {
				c.note('shifting a value from a signed type `$left_sym_final.name` can change the sign',
					node.left.pos())
			}
			if node.ct_right_value_evaled {
				if node.ct_right_value !is ast.EmptyExpr {
					ival := node.ct_right_value.i64() or { -999 }
					if ival < 0 {
						c.error('invalid negative shift count', node.right.pos())
						return left_type
					}
					moffset := match left_type_final {
						ast.char_type { 7 }
						ast.i8_type { 7 }
						ast.i16_type { 15 }
						ast.int_type { 31 }
						ast.i64_type { 63 }
						//
						ast.u8_type { 7 }
						// ast.u8_type { 7 }
						ast.u16_type { 15 }
						ast.u32_type { 31 }
						ast.u64_type { 63 }
						else { 64 }
					}
					if ival > moffset && !c.pref.translated && !c.file.is_translated {
						c.error('shift count for type `$left_sym_final.name` too large (maximum: $moffset bits)',
							node.right.pos())
						return left_type
					}
					if node.ct_left_value_evaled {
						if lval := node.ct_left_value.i64() {
							if lval < 0 {
								c.error('invalid bitshift of a negative number', node.left.pos())
								return left_type
							}
						}
					}
				} else {
					// c.note('can not evaluate "$node.right" at comptime, err: $err', node.pos)
					return left_type
				}
			}
		}
		else {
			c.error('unknown shift operator: $node.op', node.pos)
			return left_type
		}
	}
	return left_type
}

pub fn (mut c Checker) check_expected(got ast.Type, expected ast.Type) ? {
	if !c.table.check_types(got, expected, c.pref.translated) {
		return error(c.expected_msg(got, expected))
	}
}

fn (c &Checker) expected_msg(got ast.Type, expected ast.Type) string {
	exps := c.table.type_to_str(expected)
	gots := c.table.type_to_str(got)
	return 'expected `$exps`, not `$gots`'
}

pub fn (mut c Checker) infer_fn_generic_types(func ast.Fn, mut node ast.CallExpr) {
	mut inferred_types := []ast.Type{}
	for gi, gt_name in func.generic_names {
		// skip known types
		if gi < node.concrete_types.len {
			inferred_types << node.concrete_types[gi]
			continue
		}
		mut typ := ast.void_type
		for i, param in func.params {
			mut to_set := ast.void_type
			// resolve generic struct receiver
			if node.is_method && param.typ.has_flag(.generic) {
				sym := c.table.final_sym(node.receiver_type)
				match sym.info {
					ast.Struct, ast.Interface, ast.SumType {
						if !isnil(c.table.cur_fn) && c.table.cur_fn.generic_names.len > 0 { // in generic fn
							if gt_name in c.table.cur_fn.generic_names
								&& c.table.cur_fn.generic_names.len == c.table.cur_concrete_types.len {
								idx := c.table.cur_fn.generic_names.index(gt_name)
								typ = c.table.cur_concrete_types[idx]
							}
						} else { // in non-generic fn
							receiver_generic_names := sym.info.generic_types.map(c.table.sym(it).name)
							if gt_name in receiver_generic_names
								&& sym.info.generic_types.len == sym.info.concrete_types.len {
								idx := receiver_generic_names.index(gt_name)
								typ = sym.info.concrete_types[idx]
							}
						}
					}
					else {}
				}
			}
			arg_i := if i != 0 && node.is_method { i - 1 } else { i }
			if node.args.len <= arg_i {
				break
			}
			mut arg := node.args[arg_i]
			arg.typ = c.unwrap_generic(arg.typ)
			param_type_sym := c.table.sym(param.typ)

			if param.typ.has_flag(.generic) && param_type_sym.name == gt_name {
				to_set = ast.mktyp(arg.typ)
				sym := c.table.sym(arg.typ)
				if sym.info is ast.FnType {
					mut func_ := sym.info.func
					func_.name = ''
					idx := c.table.find_or_register_fn_type(c.mod, func_, true, false)
					to_set = ast.new_type(idx).derive(arg.typ)
				}
				if arg.expr.is_auto_deref_var() {
					to_set = to_set.deref()
				}
				// resolve &T &&T ...
				if param.typ.nr_muls() > 0 && to_set.nr_muls() > 0 {
					to_set = to_set.set_nr_muls(0)
				}
				// If the parent fn param is a generic too
				if to_set.has_flag(.generic) {
					to_set = c.unwrap_generic(to_set)
				}
			} else if param.typ.has_flag(.generic) {
				arg_sym := c.table.sym(arg.typ)
				if param.typ.has_flag(.variadic) {
					to_set = ast.mktyp(arg.typ)
				} else if arg_sym.kind == .array && param_type_sym.kind == .array {
					mut arg_elem_info := arg_sym.info as ast.Array
					mut param_elem_info := param_type_sym.info as ast.Array
					mut arg_elem_sym := c.table.sym(arg_elem_info.elem_type)
					mut param_elem_sym := c.table.sym(param_elem_info.elem_type)
					for {
						if arg_elem_sym.kind == .array && param_elem_sym.kind == .array
							&& !isnil(c.table.cur_fn)
							&& param_elem_sym.name !in c.table.cur_fn.generic_names {
							arg_elem_info = arg_elem_sym.info as ast.Array
							arg_elem_sym = c.table.sym(arg_elem_info.elem_type)
							param_elem_info = param_elem_sym.info as ast.Array
							param_elem_sym = c.table.sym(param_elem_info.elem_type)
						} else {
							if param_elem_sym.name == gt_name {
								typ = arg_elem_info.elem_type
							}
							break
						}
					}
				} else if arg_sym.kind == .array_fixed && param_type_sym.kind == .array_fixed {
					mut arg_elem_info := arg_sym.info as ast.ArrayFixed
					mut param_elem_info := param_type_sym.info as ast.ArrayFixed
					mut arg_elem_sym := c.table.sym(arg_elem_info.elem_type)
					mut param_elem_sym := c.table.sym(param_elem_info.elem_type)
					for {
						if arg_elem_sym.kind == .array_fixed && param_elem_sym.kind == .array_fixed
							&& !isnil(c.table.cur_fn)
							&& param_elem_sym.name !in c.table.cur_fn.generic_names {
							arg_elem_info = arg_elem_sym.info as ast.ArrayFixed
							arg_elem_sym = c.table.sym(arg_elem_info.elem_type)
							param_elem_info = param_elem_sym.info as ast.ArrayFixed
							param_elem_sym = c.table.sym(param_elem_info.elem_type)
						} else {
							if param_elem_sym.name == gt_name {
								typ = arg_elem_info.elem_type
							}
							break
						}
					}
				} else if arg_sym.kind == .map && param_type_sym.kind == .map {
					arg_map_info := arg_sym.info as ast.Map
					param_map_info := param_type_sym.info as ast.Map
					if param_map_info.key_type.has_flag(.generic)
						&& c.table.sym(param_map_info.key_type).name == gt_name {
						typ = arg_map_info.key_type
					}
					if param_map_info.value_type.has_flag(.generic)
						&& c.table.sym(param_map_info.value_type).name == gt_name {
						typ = arg_map_info.value_type
					}
				} else if arg_sym.kind == .function && param_type_sym.kind == .function {
					arg_type_func := (arg_sym.info as ast.FnType).func
					param_type_func := (param_type_sym.info as ast.FnType).func
					if param_type_func.params.len == arg_type_func.params.len {
						for n, fn_param in param_type_func.params {
							if fn_param.typ.has_flag(.generic)
								&& c.table.sym(fn_param.typ).name == gt_name {
								typ = arg_type_func.params[n].typ
							}
						}
						if param_type_func.return_type.has_flag(.generic)
							&& c.table.sym(param_type_func.return_type).name == gt_name {
							typ = arg_type_func.return_type
						}
					}
				} else if arg_sym.kind in [.struct_, .interface_, .sum_type] {
					mut generic_types := []ast.Type{}
					mut concrete_types := []ast.Type{}
					match arg_sym.info {
						ast.Struct, ast.Interface, ast.SumType {
							generic_types = arg_sym.info.generic_types
							concrete_types = arg_sym.info.concrete_types
						}
						else {}
					}
					generic_names := generic_types.map(c.table.sym(it).name)
					if gt_name in generic_names && generic_types.len == concrete_types.len {
						idx := generic_names.index(gt_name)
						typ = concrete_types[idx]
					}
				}
			}

			if to_set != ast.void_type {
				if typ != ast.void_type {
					// try to promote
					// only numbers so we don't promote pointers
					if typ.is_number() && to_set.is_number() {
						promoted := c.table.promote_num(typ, to_set, c.pref.translated)
						if promoted != ast.void_type {
							to_set = promoted
						}
					}
					if !c.table.check_types(typ, to_set, c.pref.translated) {
						c.error('inferred generic type `$gt_name` is ambiguous: got `${c.table.sym(to_set).name}`, expected `${c.table.sym(typ).name}`',
							arg.pos)
					}
				}
				typ = to_set
			}
		}
		if typ == ast.void_type {
			c.error('could not infer generic type `$gt_name` in call to `$func.name`',
				node.pos)
			return
		}
		if c.pref.is_verbose {
			s := c.table.type_to_str(typ)
			println('inferred `$func.name<$s>`')
		}
		inferred_types << typ
		node.concrete_types << typ
	}

	if c.table.register_fn_concrete_types(func.fkey(), inferred_types) {
		c.need_recheck_generic_fns = true
	}
}
