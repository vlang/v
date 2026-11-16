module arm64

$if macos && arm64 {
	import os
	import v.parser
	import v.pref
	import v.ssa
	import v.transform
	import v.types
}

fn test_native_signed_and_unsigned_wide_integer_comparisons() {
	$if macos && arm64 {
		source := 'module main
fn C.exit(int)
fn compare_unsigned(a u128, b u128) int {
	mut mask := 0
	if a == b { mask += 1 }
	if a != b { mask += 2 }
	if a < b { mask += 4 }
	if a <= b { mask += 8 }
	if a > b { mask += 16 }
	if a >= b { mask += 32 }
	return mask
}
fn compare_signed(a i128, b i128) int {
	mut mask := 0
	if a == b { mask += 1 }
	if a != b { mask += 2 }
	if a < b { mask += 4 }
	if a <= b { mask += 8 }
	if a > b { mask += 16 }
	if a >= b { mask += 32 }
	return mask
}
fn compare_unsigned_signed(a u128, b i128) int {
	mut mask := 0
	if a == b { mask += 1 }
	if a != b { mask += 2 }
	if a < b { mask += 4 }
	if a <= b { mask += 8 }
	if a > b { mask += 16 }
	if a >= b { mask += 32 }
	return mask
}
fn compare_signed_unsigned(a i128, b u128) int {
	mut mask := 0
	if a == b { mask += 1 }
	if a != b { mask += 2 }
	if a < b { mask += 4 }
	if a <= b { mask += 8 }
	if a > b { mask += 16 }
	if a >= b { mask += 32 }
	return mask
}
fn compare_scalar_signed(a i64, b i128) bool {
	return a > b
}
fn compare_scalar_unsigned(a i64, b u128) bool {
	return a < b
}
fn main() {
	one := u128(1)
	high := one << 100
	if compare_unsigned(one, u128(2)) != 14 { C.exit(1) }
	if compare_unsigned(high, high) != 41 { C.exit(2) }
	if compare_unsigned(high + one, high) != 50 { C.exit(3) }
	if compare_unsigned(high, high + one) != 14 { C.exit(4) }
	if compare_unsigned(one << 127, high) != 50 { C.exit(5) }
	if compare_unsigned(u128(18446744073709551615), high) != 14 { C.exit(6) }
	negative := i128(-1)
	if compare_signed(negative, i128(0)) != 14 { C.exit(7) }
	if compare_signed(i128(0), negative) != 50 { C.exit(8) }
	if compare_signed(negative, negative) != 41 { C.exit(9) }
	if compare_signed(i128(-2), negative) != 14 { C.exit(10) }
	if compare_signed(i128(1) << 127, negative) != 14 { C.exit(11) }
	if compare_signed(i128(18446744073709551615), i128(1)) != 50 { C.exit(12) }
	matched := match one { u128(1) { true } else { false } }
	if !matched { C.exit(13) }
	match high { u128(0) { C.exit(14) } else {} }
	if !(one in u128(1)..u128(3)) { C.exit(15) }
	values := [one, high, high + one]
	if values.index(high) != 1 { C.exit(16) }
	if values.index(u128(7)) != -1 { C.exit(17) }
	if compare_unsigned_signed(one << 127, i128(0)) != 50 { C.exit(18) }
	if compare_unsigned_signed(u128(0), i128(1) << 127) != 14 { C.exit(19) }
	if compare_signed_unsigned(i128(1) << 127, u128(0)) != 14 { C.exit(20) }
	if compare_signed_unsigned(i128(0), one << 127) != 50 { C.exit(21) }
	if compare_unsigned_signed(high, i128(high)) != 41 { C.exit(22) }
	if compare_signed_unsigned(i128(high), high) != 41 { C.exit(23) }
	if compare_unsigned_signed(high + one, i128(high)) != 50 { C.exit(24) }
	if compare_unsigned_signed(high, i128(high + one)) != 14 { C.exit(25) }
	if compare_signed_unsigned(i128(high + one), high) != 50 { C.exit(26) }
	if compare_signed_unsigned(i128(high), high + one) != 14 { C.exit(27) }
	if !compare_scalar_signed(i64(0), negative) { C.exit(28) }
	if !compare_scalar_unsigned(i64(0), one << 127) { C.exit(29) }
}
'
		path := os.join_path(os.vtmp_dir(), 'arm64_wide_comparison_${os.getpid()}.v')
		output := path.all_before_last('.')
		defer {
			os.rm(path) or {}
			os.rm(output) or {}
			os.rm(output + '_c') or {}
		}
		os.write_file(path, source) or { panic(err) }
		mut p := parser.Parser.new(pref.new_preferences())
		mut a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := types.TypeChecker.new(a)
		tc.collect(a)
		tc.annotate_types()
		assert tc.errors.len == 0, tc.errors.str()
		transform.transform(mut a, tc)
		m := ssa.build_with_used(a, map[string]bool{}, tc)
		mut g := Gen.new(m)
		g.gen()
		g.write_and_link(output)
		result := os.exec([output])
		assert result.exit_code == 0, result.output
		for portable in [false, true] {
			mut args := [@VEXE, '-b', 'c', '-cc', 'clang', '-nocache', '-gc', 'none', '-o',
				output + '_c']
			if portable {
				args << ['-d', 'v3_no_native_int128']
			}
			args << path
			compiled := os.exec(args)
			assert compiled.exit_code == 0, compiled.output
			c_result := os.exec([output + '_c'])
			assert c_result.exit_code == 0, c_result.output
		}
	}
}
