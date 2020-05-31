// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module checker

import v.table
import v.token

pub fn (c &Checker) check_types(got, expected table.Type) bool {
	t := c.table
	got_idx := t.unalias_num_type(got).idx()
	exp_idx := t.unalias_num_type(expected).idx()
	// got_is_ptr := got.is_ptr()
	exp_is_ptr := expected.is_ptr()
	// println('check: $got_type_sym.name, $exp_type_sym.name')
	// # NOTE: use idxs here, and symbols below for perf
	if got_idx == exp_idx {
		// this is returning true even if one type is a ptr
		// and the other is not, is this correct behaviour?
		return true
	}
	if got_idx == table.none_type_idx && expected.flag_is(.optional) {
		return true
	}
	// allow pointers to be initialized with 0. TODO: use none instead
	if exp_is_ptr && got_idx == table.int_type_idx {
		return true
	}
	if exp_idx == table.voidptr_type_idx || got_idx == table.voidptr_type_idx {
		return true
	}
	if exp_idx == table.any_type_idx || got_idx == table.any_type_idx {
		return true
	}
	// TODO i64 as int etc
	if (exp_idx in table.pointer_type_idxs || exp_idx in table.number_type_idxs) && (got_idx in
		table.pointer_type_idxs || got_idx in table.number_type_idxs) {
		return true
	}
	// if exp_idx in pointer_type_idxs && got_idx in pointer_type_idxs {
	// return true
	// }
	// see hack in checker IndexExpr line #691
	if (got_idx == table.byte_type_idx && exp_idx == table.byteptr_type_idx) || (exp_idx ==
		table.byte_type_idx && got_idx == table.byteptr_type_idx) {
		return true
	}
	if (got_idx == table.char_type_idx && exp_idx == table.charptr_type_idx) || (exp_idx ==
		table.char_type_idx && got_idx == table.charptr_type_idx) {
		return true
	}
	if expected == table.t_type && got == table.t_type {
		return true
	}
	// # NOTE: use symbols from this point on for perf
	got_type_sym := t.get_type_symbol(got)
	exp_type_sym := t.get_type_symbol(expected)
	//
	if exp_type_sym.kind == .function && got_type_sym.kind == .int {
		// TODO temporary
		// fn == 0
		return true
	}
	// allow enum value to be used as int
	if (got_type_sym.is_int() && exp_type_sym.kind == .enum_) || (exp_type_sym.is_int() &&
		got_type_sym.kind == .enum_) {
		return true
	}
	// TODO
	// if got_type_sym.kind == .array && exp_type_sym.kind == .array {
	// return true
	// }
	if got_type_sym.kind == .array_fixed && exp_type_sym.kind == .byteptr {
		info := got_type_sym.info as table.ArrayFixed
		if info.elem_type.idx() == table.byte_type_idx {
			return true
		}
	}
	// TODO
	if exp_type_sym.name == 'array' || got_type_sym.name == 'array' {
		return true
	}
	// TODO
	// accept [] when an expected type is an array
	if got_type_sym.kind == .array && got_type_sym.name == 'array_void' && exp_type_sym.kind ==
		.array {
		return true
	}
	// type alias
	if (got_type_sym.kind == .alias && got_type_sym.parent_idx == exp_idx) || (exp_type_sym.kind ==
		.alias && exp_type_sym.parent_idx == got_idx) {
		return true
	}
	// sum type
	if got_type_sym.kind == .sum_type {
		sum_info := got_type_sym.info as table.SumType
		// TODO: handle `match SumType { &PtrVariant {} }` currently just checking base
		if expected.set_nr_muls(0) in sum_info.variants {
			return true
		}
	}
	if exp_type_sym.kind == .sum_type {
		sum_info := exp_type_sym.info as table.SumType
		// TODO: handle `match SumType { &PtrVariant {} }` currently just checking base
		if got.set_nr_muls(0) in sum_info.variants {
			return true
		}
	}
	// fn type
	if got_type_sym.kind == .function && exp_type_sym.kind == .function {
		got_info := got_type_sym.info as table.FnType
		exp_info := exp_type_sym.info as table.FnType
		got_fn := got_info.func
		exp_fn := exp_info.func
		// we are using check() to compare return type & args as they might include
		// functions themselves. TODO: optimize, only use check() when needed
		if got_fn.args.len == exp_fn.args.len && c.check_types(got_fn.return_type, exp_fn.return_type) {
			for i, got_arg in got_fn.args {
				exp_arg := exp_fn.args[i]
				if !c.check_types(got_arg.typ, exp_arg.typ) {
					return false
				}
			}
			return true
		}
	}
	return false
}

[inline]
fn (c &Checker) check_shift(left_type, right_type table.Type, left_pos, right_pos token.Position) table.Type {
	if !left_type.is_int() {
		c.error('cannot shift type ${c.table.get_type_symbol(right_type).name} into non-integer type ${c.table.get_type_symbol(left_type).name}', left_pos)
		return table.void_type
	} else if !right_type.is_int() {
		c.error('cannot shift non-integer type ${c.table.get_type_symbol(right_type).name} into type ${c.table.get_type_symbol(left_type).name}', right_pos)
		return table.void_type
	}
	return left_type
}

pub fn (c &Checker) promote(left_type, right_type table.Type) table.Type {
	if left_type.is_ptr() || left_type.is_pointer() {
		if right_type.is_int() {
			return left_type
		} else {
			return table.void_type
		}
	} else if right_type.is_ptr() || right_type.is_pointer() {
		if left_type.is_int() {
			return right_type
		} else {
			return table.void_type
		}
	}
	if left_type == right_type {
		return left_type // strings, self defined operators
	}
	if right_type.is_number() && left_type.is_number() {
		// sort the operands to save time
		mut type_hi := left_type
		mut type_lo := right_type
		if type_hi.idx() < type_lo.idx() {
			tmp := type_hi
			type_hi = type_lo
			type_lo = tmp
		}
		idx_hi := type_hi.idx()
		idx_lo := type_lo.idx()
		// the following comparisons rely on the order of the indices in atypes.v
		if idx_hi == table.any_int_type_idx {
			return type_lo
		} else if idx_hi == table.any_flt_type_idx {
			if idx_lo in table.float_type_idxs {
				return type_lo
			} else {
				return table.void_type
			}
		} else if type_hi.is_float() {
			if idx_hi == table.f32_type_idx {
				if idx_lo in [table.int_type_idx, table.i64_type_idx, table.u32_type_idx, table.u64_type_idx] {
					return table.void_type
				} else {
					return type_hi
				}
			} else { // f64, any_flt
				if idx_lo in [table.i64_type_idx, table.u64_type_idx] {
					return table.void_type
				} else {
					return type_hi
				}
			}
		} else if idx_lo >= table.byte_type_idx { // both operands are unsigned
			return type_hi
		} else if idx_lo >= table.i8_type_idx && idx_hi <= table.i64_type_idx { // both signed
			return type_hi
		} else if idx_hi - idx_lo < (table.byte_type_idx - table.i8_type_idx) {
			return type_lo // conversion unsigned -> signed if signed type is larger
		} else {
			return table.void_type // conversion signed -> unsigned not allowed
		}
	} else {
		return left_type // default to left if not automatic promotion possible
	}
}

// TODO: promote(), assign_check(), symmetric_check() and check() overlap - should be rearranged
pub fn (c &Checker) assign_check(got, expected table.Type) bool {
	exp_idx := expected.idx()
	got_idx := got.idx()
	if exp_idx == got_idx {
		return true
	}
	if exp_idx == table.voidptr_type_idx || exp_idx == table.byteptr_type_idx {
		if got.is_ptr() || got.is_pointer() {
			return true
		}
	}
	// allow direct int-literal assignment for pointers for now
	// maybe in the future optionals should be used for that
	if expected.is_ptr() || expected.is_pointer() {
		if got == table.any_int_type {
			return true
		}
	}
	if got_idx == table.voidptr_type_idx || got_idx == table.byteptr_type_idx {
		if expected.is_ptr() || expected.is_pointer() {
			return true
		}
	}
	if !c.check_types(got, expected) { // TODO: this should go away...
		return false
	}
	if c.promote(expected, got) != expected {
		println('could not promote ${c.table.get_type_symbol(got).name} to ${c.table.get_type_symbol(expected).name}')
		return false
	}
	return true
}

pub fn (c &Checker) symmetric_check(left, right table.Type) bool {
	// allow direct int-literal assignment for pointers for now
	// maybe in the future optionals should be used for that
	if right.is_ptr() || right.is_pointer() {
		if left == table.any_int_type {
			return true
		}
	}
	// allow direct int-literal assignment for pointers for now
	if left.is_ptr() || left.is_pointer() {
		if right == table.any_int_type {
			return true
		}
	}
	return c.check_types(left, right)
}
