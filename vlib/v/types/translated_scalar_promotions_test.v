module types

import os

fn test_translated_static_globals_use_c_int_width() {
	root := os.join_path(os.vtmp_dir(), 'v3_translated_static_globals_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'translated_static_globals' }")!
	os.mkdir_all(os.join_path(root, 'values'))!
	os.write_file(os.join_path(root, 'values', 'values.v'), 'module values\npub const unsigned = u32(0xffff_ffff)\n')!
	os.mkdir_all(os.join_path(root, 'other'))!
	os.write_file(os.join_path(root, 'other', 'other.v'), 'module other\npub const unsigned = u64(0x1_ffff_ffff)\n')!
	os.write_file(os.join_path(root, 'translated.v'), '@[translated]
module main
import values as data
type StaticInt = int
type StaticRow = [2]StaticInt
struct StaticHolder { value int }
const static_unsigned = u32(0xffff_ffff)
__global (
	translated_static int = u32(0xffff_ffff)
	translated_static_alias StaticInt = static_unsigned
	translated_static_parens int = (u32(0xffff_ffff))
	translated_static_float int = f64(-1.5)
	translated_static_wide i64 = u32(0xffff_ffff)
	translated_static_array [2]int = [u32(0xffff_ffff), u32(0x8000_0000)]!
	translated_static_nested [1][2]int = [[u32(0xffff_ffff), u32(0x8000_0000)]!]!
	translated_static_row StaticRow = [u32(0xffff_ffff), u32(0x8000_0000)]!
	translated_static_wide_array [1]i64 = [u32(0xffff_ffff)]!
	translated_static_struct StaticHolder = StaticHolder{value: u32(0xffff_ffff)}
)
@[cinit]
__global (
	translated_static_constant StaticHolder = StaticHolder{value: static_unsigned}
	translated_static_qualified StaticHolder = StaticHolder{value: data.unsigned}
)
')!
	os.write_file(os.join_path(root, 'main.v'), 'module main
import other as data
__global ordinary_static int = int(u32(0xffff_ffff))
__global ordinary_static_array [1]int = [int(u32(0xffff_ffff))]!
const ordinary_static_unsigned = int(u32(0xffff_ffff))
const ordinary_static_qualified_unsigned = int(data.unsigned)
@[cinit]
__global ordinary_static_struct = StaticHolder{value: ordinary_static_unsigned}
@[cinit]
__global ordinary_static_qualified = StaticHolder{value: ordinary_static_qualified_unsigned}
fn main() {
	assert i64(translated_static) == -1
	assert i64(translated_static_alias) == -1
	assert i64(translated_static_parens) == -1
	assert i64(translated_static_float) == -1
	assert translated_static_wide == i64(4294967295)
	assert i64(translated_static_array[0]) == -1
	assert i64(translated_static_array[1]) == -2147483648
	assert i64(translated_static_nested[0][0]) == -1
	assert i64(translated_static_nested[0][1]) == -2147483648
	assert i64(translated_static_row[0]) == -1
	assert i64(translated_static_row[1]) == -2147483648
	assert translated_static_wide_array[0] == i64(4294967295)
	assert i64(translated_static_struct.value) == -1
	assert i64(translated_static_constant.value) == -1
	assert i64(translated_static_qualified.value) == -1
	if sizeof(int) == 8 {
		assert i64(ordinary_static) == 4294967295
		assert i64(ordinary_static_array[0]) == 4294967295
		assert i64(ordinary_static_struct.value) == 4294967295
		assert i64(ordinary_static_qualified.value) == 8589934591
	}
}
')!
	for flags in ['', '-no-parallel'] {
		result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-enable-globals',
			'run', root])
		assert result.exit_code == 0, result.output
	}
}

fn test_translated_enum_arithmetic_with_overflow_checks() {
	root := os.join_path(os.vtmp_dir(), 'v3_translated_enum_overflow_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, '@[translated]\nmodule main\nenum OverflowEnum as u32 {\n\tzero = 0\n}\nfn main() {\n\tassert (int(-1) + OverflowEnum.zero) > i64(0)\n\tassert (OverflowEnum.zero + int(-1)) > i64(0)\n}\n')!
	result := os.exec([@VEXE, '-check-overflow', 'run', path])
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
			result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-check', root])
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
		result := os.exec([@VEXE, '-check', root])
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
	result := os.exec([@VEXE, '-check', path])
	assert result.exit_code != 0, result.output
	assert result.output.contains('shift count for type `ShiftAlias` too large'), result.output
}

fn test_ordinary_literal_shifts_still_widen() {
	root := os.join_path(os.vtmp_dir(), 'v3_ordinary_literal_shift_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, 'module main\nconst wide = 1 << 40\nfn main() { value := 1 << 40; assert value == u64(1099511627776); assert wide == u64(1099511627776) }\n')!
	result := os.exec([@VEXE, 'run', path])
	assert result.exit_code == 0, result.output
}

fn test_ordinary_int_casts_keep_the_native_width() {
	root := os.join_path(os.vtmp_dir(), 'v3_ordinary_int_cast_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'translated.v'), '@[translated]\nmodule main\nfn translated() {}\n')!
	os.write_file(os.join_path(root, 'main.v'), 'module main\ntype IntAlias = int\nfn main() { translated(); if sizeof(int) == 8 { value := u32(0xffff_ffff); assert i64(int(value)) == 4294967295; assert i64(IntAlias(value)) == 4294967295 } }\n')!
	result := os.exec([@VEXE, 'run', root])
	assert result.exit_code == 0, result.output
}

fn test_translated_compound_arithmetic_respects_overflow_checks() {
	root := os.join_path(os.vtmp_dir(), 'v3_translated_compound_overflow_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	cases := [
		'mut value := i64(9223372036854775807); value += i64(1)',
		'mut value := i64(-9223372036854775807); value -= i64(2)',
		'mut value := i64(9223372036854775807); value *= i64(2)',
		'mut value := int(2147483647); value += int(1)',
		'mut values := [i64(9223372036854775807)]!; values[0] += i64(1)',
	]
	for body in cases {
		os.write_file(path, '@[translated]\nmodule main\nfn main() { ${body} }\n')!
		result := os.exec([@VEXE, '-check-overflow', 'run', path])
		assert result.exit_code != 0, result.output
		assert result.output.contains('overflow'), result.output
	}
	os.write_file(path, '@[translated]\nmodule main\nfn main() { mut value := int(-1); value += u32(0); assert value == -1; mut narrow := u8(255); narrow += u8(1); assert narrow == 0 }\n')!
	result := os.exec([@VEXE, '-check-overflow', 'run', path])
	assert result.exit_code == 0, result.output
}

fn test_translated_postfix_uses_c_int_overflow_width() {
	root := os.join_path(os.vtmp_dir(), 'v3_translated_postfix_overflow_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	for body in [
		'mut value := int(2147483647); value++',
		'mut value := IntAlias(-2147483648); value--',
		'mut value := Holder{2147483647}; value.number++',
		'mut values := [int(-2147483648)]!; values[0]--',
		'mut values := {"a": int(2147483647)}; values["a"]++',
	] {
		os.write_file(path, '@[translated]\nmodule main\ntype IntAlias = int\nstruct Holder { mut: number int }\nfn main() { ${body} }\n')!
		result := os.exec([@VEXE, '-check-overflow', 'run', path])
		assert result.exit_code != 0, body + '\n' + result.output
		assert result.output.contains('overflow(i32('), body + '\n' + result.output
	}
	os.write_file(path, '@[translated]
module main
fn translated() {
 mut wide := i64(2147483647)
 wide++
 assert wide == i64(2147483648)
 mut low := int(-2147483647)
 low--
 assert low == -2147483648
 mut high := int(2147483646)
 high++
 assert high == 2147483647
}
')!
	os.write_file(os.join_path(root, 'ordinary.v'), 'module main
fn main() {
 translated()
 if sizeof(int) == 8 {
  mut value := int(2147483647)
  value++
  assert i64(value) == 2147483648
 }
}
')!
	result := os.exec([@VEXE, '-check-overflow', 'run', root])
	assert result.exit_code == 0, result.output
}
