module x64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_x64_wide_unsigned_integer_division_and_remainder() {
	$if linux && amd64 {
		path := os.join_path(os.vtmp_dir(), 'x64_wide_division_${os.getpid()}.v')
		output := path.all_before_last('.')
		defer {
			os.rm(path) or {}
			os.rm(output) or {}
		}
		os.write_file(path, r'module main
fn C.exit(int)
fn check(a u128, b u128, quotient u128, remainder u128, digit u8) {
	if a / b != quotient { C.exit(1) }
	if a % b != remainder { C.exit(2) }
	if u8(a % u128(10)) != digit { C.exit(3) }
}
fn main() {
	check((u128(1) << 64) | u128(3), u128(3), u128(0x5555555555555556),
		u128(1), u8(9))
	check((u128(1) << 127) + (u128(1) << 63) + u128(9), (u128(1) << 65) + u128(5),
		u128(0x3fffffffffffffff), u128(23058430092136939534), u8(5))
	check(~u128(0), u128(3),
		(u128(0x5555555555555555) << 64) | u128(0x5555555555555555), u128(0), u8(5))
	if u128(12345678901234567890).str() != ', 12345678901234567890, ' { C.exit(4) }
	if i128(-1234567890123456789).str() != ' - 1234567890123456789, ' { C.exit(5) }
}
')!
		mut preferences := pref.new_preferences()
		preferences.backend = 'x64'
		mut p := parser.Parser.new(preferences)
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
		assert result.exit_code == 0, 'exit ${result.exit_code}: ${result.output}'
	}
}
