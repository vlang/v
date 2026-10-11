module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_int_limits_match_storage_and_capacity_guards_after_transform() {
	$if macos && arm64 {
		source := 'module main
fn C.exit(int)
const min_i32 = i32(-2147483648)
const max_i32 = i32(2147483647)
const min_i64 = i64(-9223372036854775807 - 1)
const max_i64 = i64(9223372036854775807)
const min_int = $if new_int ?&& x64 { int(min_i64) } $else { int(min_i32) }
const max_int = $if new_int ?&& x64 { int(max_i64) } $else { int(max_i32) }
fn required_capacity(initial int, required int) int {
    mut cap := if initial > 0 { i64(initial) } else { i64(2) }
    for required > cap { cap *= 2 }
    if cap > max_int { C.exit(1) }
    return int(cap)
}
fn main() {
    if max_int != 2147483647 || min_int != -2147483648 { C.exit(2) }
    if sizeof(int) != 4 || sizeof(i64) != 8 || sizeof(voidptr) != 8 { C.exit(3) }
    if required_capacity(8192, 8800) != 16384 { C.exit(4) }
    if required_capacity(0, 16384) != 16384 { C.exit(5) }
    if i64(max_i64) <= i64(max_int) || i64(min_i64) >= i64(min_int) { C.exit(6) }
}
'
		path := os.join_path(os.vtmp_dir(), 'arm64_integer_limits_${os.getpid()}.v')
		output := path.all_before_last('.')
		defer {
			os.rm(path) or {}
			os.rm(output) or {}
		}
		os.write_file(path, source) or { panic(err) }
		mut preferences := pref.new_preferences()
		preferences.backend = 'arm64'
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
		assert result.exit_code == 0, result.output
	}
}
