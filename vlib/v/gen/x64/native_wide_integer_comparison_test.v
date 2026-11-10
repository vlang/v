module x64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_x64_wide_integer_comparisons() {
	$if linux && amd64 {
		path := os.join_path(os.vtmp_dir(), 'x64_wide_comparisons_${os.getpid()}.v')
		output := path.all_before_last('.')
		defer {
			os.rm(path) or {}
			os.rm(output) or {}
		}
		os.write_file(path, r'module main
fn C.exit(int)
fn check_u128(a u128, b u128, eq bool, ne bool, lt bool, le bool, gt bool, ge bool) {
	same := a == b
	different := a != b
	less := a < b
	less_equal := a <= b
	greater := a > b
	greater_equal := a >= b
	if same != eq { C.exit(1) }
	if different != ne { C.exit(2) }
	if less != lt { C.exit(3) }
	if less_equal != le { C.exit(4) }
	if greater != gt { C.exit(5) }
	if greater_equal != ge { C.exit(6) }
}
fn check_i128(a i128, b i128, eq bool, ne bool, lt bool, le bool, gt bool, ge bool) {
	same := a == b
	different := a != b
	less := a < b
	less_equal := a <= b
	greater := a > b
	greater_equal := a >= b
	if same != eq { C.exit(11) }
	if different != ne { C.exit(12) }
	if less != lt { C.exit(13) }
	if less_equal != le { C.exit(14) }
	if greater != gt { C.exit(15) }
	if greater_equal != ge { C.exit(16) }
}
fn compare_i128_scalar_lhs(a int, b i128) bool {
	return a <= b
}
fn compare_i128_scalar_rhs(a i128, b int) bool {
	return a <= b
}
fn main() {
	u_low_one := (u128(1) << 64) | u128(1)
	u_low_two := (u128(1) << 64) | u128(2)
	u_high := (u128(2) << 64) | u128(1)
	u_top := (u128(1) << 127) | u128(1)
	check_u128(u_low_one, u_low_two, false, true, true, true, false, false)
	check_u128(u_low_one, u_low_one, true, false, false, true, false, true)
	check_u128(u_high, u_low_one, false, true, false, false, true, true)
	check_u128(u_top, u_high, false, true, false, false, true, true)

	i_negative_two := i128(-2)
	i_negative_one := i128(-1)
	i_zero := i128(0)
	i_positive := i128(1) << 64
	i_minimum := i128(1) << 127
	check_i128(i_negative_two, i_negative_one, false, true, true, true, false, false)
	check_i128(i_negative_one, i_zero, false, true, true, true, false, false)
	check_i128(i_positive, i_negative_one, false, true, false, false, true, true)
	check_i128(i_minimum, i_negative_one, false, true, true, true, false, false)
	if compare_i128_scalar_lhs(0, i_negative_one) { C.exit(17) }
	if !compare_i128_scalar_rhs(i_negative_one, 0) { C.exit(18) }
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
