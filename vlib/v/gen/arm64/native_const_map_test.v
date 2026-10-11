module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_const_map_membership_and_value_addresses_use_typed_storage() {
	$if macos && arm64 {
		source := 'module main
fn C.exit(int)
struct Profile { first i64 second i64 }
const reserved = {"i8": true, "array": true, "available": false}
const profile = Profile{first: 17, second: 23}
const number = i64(71)
fn contains(name string) bool { return name in reserved }
fn total(value &Profile) i64 { return value.first + value.second }
fn make_profile() Profile { return Profile{first: 31, second: 37} }
fn main() {
    for i := 0; i < 100; i++ {
        if !contains("i8") || !contains("array") || !contains("available") { C.exit(1) }
        if contains("missing") || "missing" in reserved { C.exit(2) }
        if !reserved["i8"] { C.exit(3) }
        if reserved["available"] { C.exit(9) }
        if reserved["missing"] { C.exit(10) }
    }
    keys := reserved.keys()
    if keys.len != 3 || "i8" !in keys || "array" !in keys || "available" !in keys { C.exit(4) }
    if total(&profile) != 40 { C.exit(5) }
    number_address := &number
    if unsafe { *number_address } != 71 { C.exit(11) }
    created := &make_profile()
    if total(created) != 68 { C.exit(6) }
    mut local := i64(41)
    address := &(local)
    unsafe { *address = 43 }
    if local != 43 { C.exit(7) }
    mut other := i64(47)
    mut pointer := &local
    pointer_address := &(pointer)
    unsafe { *pointer_address = &other }
    if pointer != &other || unsafe { *pointer } != 47 { C.exit(8) }
}
'
		path := os.join_path(os.vtmp_dir(), 'arm64_const_map_${os.getpid()}.v')
		output := path.all_before_last('.')
		defer {
			os.rm(path) or {}
			os.rm(output) or {}
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
		assert result.exit_code == 0, 'exit ${result.exit_code}: ${result.output}'
	}
}
