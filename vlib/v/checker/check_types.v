// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module checker

import v.table

pub fn (c &Checker) check_types(got, expected table.Type) bool {
	t := c.table
	got_idx := got.idx()
	exp_idx := expected.idx()
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
