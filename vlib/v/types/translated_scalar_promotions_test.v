module types

import os

fn test_translated_enum_arithmetic_with_overflow_checks() {
	root := os.join_path(os.vtmp_dir(), 'v3_translated_enum_overflow_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, '@[translated]\nmodule main\nenum OverflowEnum as u32 {\n\tzero = 0\n}\nfn main() {\n\tassert (int(-1) + OverflowEnum.zero) > i64(0)\n\tassert (OverflowEnum.zero + int(-1)) > i64(0)\n}\n')!
	result := os.execute('${os.quoted_path(@VEXE)} -check-overflow run ${os.quoted_path(path)}')
	assert result.exit_code == 0, result.output
}

fn test_translated_promotions_do_not_leak_into_ordinary_files() {
	root := os.join_path(os.vtmp_dir(), 'v3_translated_promotions_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'translated.v'), '@[translated]\nmodule main\nfn translated() {}\n')!
	cases := [
		['values := [11,22]!; _ = values[true]', 'non-integer index `bool`'],
		['value := u64(0); _ = value == -1', 'cannot be compared with negative value'],
		['mut value := u64(0); value = -1', 'cannot assign negative value'],
		['value := u16(2); _ = value << 16', 'shift count for type `u16` too large'],
		['flag := false; _ = if flag { flag } else { 42 }', 'mismatched types'],
		['value := 1; mut p := &value; p += true', 'invalid right operand'],
		['callback := fn () {}; _ = callback == 0', 'infix expr:'],
		['value := 0xffff_ffff; _ = value', 'overflow in implicit type'],
		['value := 4294967295; _ = value', 'overflow in implicit type'],
	]
	for case in cases {
		os.write_file(os.join_path(root, 'main.v'), 'module main\nfn main() { ${case[0]}; translated() }\n')!
		for flags in ['', '-no-parallel'] {
			result := os.execute('${os.quoted_path(@VEXE)} ${flags} -check ${os.quoted_path(root)}')
			assert result.exit_code != 0, result.output
			assert result.output.contains(case[1]), result.output
		}
	}
}

fn test_translated_indices_and_shifts_still_reject_invalid_operands() {
	root := os.join_path(os.vtmp_dir(), 'v3_translated_integral_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	cases := [
		['values := [11,22]!; _ = values[1.5]', 'non-integer index'],
		['value := u16(2); _ = value << 32', 'shift count'],
		['value := int(1); _ = value << 40', 'shift count'],
		['value := f64(2.5); _ = value << 1', 'invalid operation: shift'],
		['value := f64(2.5); _ = ~value', 'can only be used with integer types'],
		['flag := false; _ = if flag { true } else { "text" }', 'mismatched types'],
	]
	for case in cases {
		os.write_file(os.join_path(root, 'main.v'), '@[translated]\nmodule main\nfn main() { ${case[0]} }\n')!
		result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(root)}')
		assert result.exit_code != 0, result.output
		assert result.output.contains(case[1]), result.output
	}
}

fn test_translated_int_alias_shift_count_uses_c_width() {
	root := os.join_path(os.vtmp_dir(), 'v3_translated_shift_alias_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, '@[translated]\nmodule main\ntype ShiftAlias = int\nfn main() { value := ShiftAlias(1); _ = value << 40 }\n')!
	result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('shift count for type `ShiftAlias` too large'), result.output
}

fn test_ordinary_literal_shifts_still_widen() {
	root := os.join_path(os.vtmp_dir(), 'v3_ordinary_literal_shift_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, 'module main\nconst wide = 1 << 40\nfn main() { value := 1 << 40; assert value == u64(1099511627776); assert wide == u64(1099511627776) }\n')!
	result := os.execute('${os.quoted_path(@VEXE)} run ${os.quoted_path(path)}')
	assert result.exit_code == 0, result.output
}
