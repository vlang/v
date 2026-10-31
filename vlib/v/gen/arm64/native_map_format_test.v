module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.types

fn test_native_map_pieces_format_primitive_widths_and_float_arrays() {
	$if macos && arm64 {
		run_native_map_format_fixture('pieces', 'module main
fn C.exit(int)
fn v3_map_str_piece(p voidptr, kind int, bytes int, fixed_len int) string { return "" }
fn main() {
	s := "hello"
	if v3_map_str_piece(&s, 1, 16, 0) != "\'hello\'" { C.exit(1) }
	i8_value := i8(-7)
	i16_value := i16(-1234)
	i32_value := -42
	i64_value := i64(-9223372036854775807) - 1
	if v3_map_str_piece(&i8_value, 2, 1, 0) != "-7" { C.exit(2) }
	if v3_map_str_piece(&i16_value, 2, 2, 0) != "-1234" { C.exit(3) }
	if v3_map_str_piece(&i32_value, 2, 4, 0) != "-42" { C.exit(4) }
	if v3_map_str_piece(&i64_value, 2, 8, 0) != "-9223372036854775808" { C.exit(5) }
	u8_value := u8(255)
	u16_value := u16(65535)
	u32_value := u32(4294967295)
	u64_value := u64(18446744073709551615)
	if v3_map_str_piece(&u8_value, 3, 1, 0) != "255" { C.exit(6) }
	if v3_map_str_piece(&u16_value, 3, 2, 0) != "65535" { C.exit(7) }
	if v3_map_str_piece(&u32_value, 3, 4, 0) != "4294967295" { C.exit(8) }
	if v3_map_str_piece(&u64_value, 3, 8, 0) != "18446744073709551615" { C.exit(9) }
	rune_value := u32(0x20ac)
	if v3_map_str_piece(&rune_value, 4, 4, 0) != "`€`" { C.exit(10) }
	float32_value := f32(1.5)
	float64_value := f64(-1.25)
	if v3_map_str_piece(&float32_value, 5, 4, 0) != "1.5" { C.exit(11) }
	if v3_map_str_piece(&float32_value, 8, 4, 0) != "1.5" { C.exit(12) }
	if v3_map_str_piece(&float64_value, 5, 8, 0) != "-1.25" { C.exit(13) }
	t := true
	f := false
	if v3_map_str_piece(&t, 7, 1, 0) != "true" { C.exit(14) }
	if v3_map_str_piece(&f, 7, 1, 0) != "false" { C.exit(15) }
	mut a32 := [2]f32{}
	a32[0] = f32(1.5)
	a32[1] = f32(-2.0)
	mut a64 := [2]f64{}
	a64[0] = f64(1.5)
	a64[1] = f64(-2.0)
	if v3_map_str_piece(&a32, 9, 8, 2) != "[1.5, -2.0]" { C.exit(16) }
	if v3_map_str_piece(&a64, 6, 16, 2) != "[1.5, -2.0]" { C.exit(17) }
	d32 := [f32(1.5), f32(-2.0)]
	d64 := [f64(1.5), f64(-2.0)]
	if v3_map_str_piece(&d32, 6, 32, 0) != "[1.5, -2.0]" { C.exit(18) }
	if v3_map_str_piece(&d64, 6, 32, 0) != "[1.5, -2.0]" { C.exit(19) }
}
')
	}
}

fn test_native_map_strings_read_state_storage_and_skip_deleted_entries() {
	$if macos && arm64 {
		run_native_map_format_fixture('maps', 'module main
fn C.exit(int)
fn v3_map_str(m map[string]int, key_kind int, val_kind int, fixed_len int) string { return "" }
fn v3_map_set_sized(m voidptr, key voidptr, val voidptr, key_size i64, val_size i64) {}
fn main() {
	mut numbers := map[string]int{"one": 1, "two": -2}
	if v3_map_str(numbers, 1, 2, 0) != "{\'one\': 1, \'two\': -2}" { C.exit(1) }
	numbers.delete("one")
	if v3_map_str(numbers, 1, 2, 0) != "{\'two\': -2}" { C.exit(2) }
	key := "three"
	value := 3
	v3_map_set_sized(&numbers, &key, &value, 16, 4)
	if v3_map_str(numbers, 1, 2, 0) != "{\'two\': -2, \'three\': 3}" { C.exit(3) }
	empty := map[string]int{}
	if v3_map_str(empty, 1, 2, 0) != "{}" { C.exit(4) }
}
')
	}
}

fn run_native_map_format_fixture(name string, source string) {
	path := os.join_path(os.vtmp_dir(), 'arm64_map_format_${name}_${os.getpid()}.v')
	output := path.all_before_last('.')
	defer {
		os.rm(path) or {}
		os.rm(output) or {}
	}
	os.write_file(path, source) or { panic(err) }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.annotate_types()
	assert tc.errors.len == 0, tc.errors.str()
	m := ssa.build_with_used(a, map[string]bool{}, tc)
	mut g := Gen.new(m)
	g.gen()
	g.write_and_link(output)
	result := os.exec([output])
	assert result.exit_code == 0, 'fixture ${name}: ${result.exit_code}: ${result.output}'
}
