module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_mut_pointer_parameters_preserve_forwarding_indexing_and_reassignment() {
	$if macos && arm64 {
		source := 'module main
fn C.exit(int)
fn write(code u32, mut buffer &u8) int {
    unsafe {
        buffer[0] = u8(code)
        buffer[1] = 0
    }
    return 1
}
fn forward(code u32, mut buffer &u8) int {
    length := write(code, mut buffer)
    unsafe { buffer[length] = 0 }
    return length
}
fn replace(mut buffer &u8, replacement &u8) {
    buffer = replacement
}
fn increment(mut value int) { value++ }
fn main() {
    mut first := [8]u8{}
    mut second := [8]u8{}
    mut pointer := &first[0]
    if forward(65, mut pointer) != 1 { C.exit(1) }
    if first[0] != 65 || first[1] != 0 || pointer != &first[0] { C.exit(2) }
    replace(mut pointer, &second[0])
    if pointer != &second[0] { C.exit(3) }
    if forward(66, mut pointer) != 1 { C.exit(4) }
    if second[0] != 66 || first[0] != 65 { C.exit(5) }
    mut value := 9
    increment(mut value)
    if value != 10 { C.exit(6) }
}
'
		path := os.join_path(os.vtmp_dir(), 'arm64_mut_pointer_${os.getpid()}.v')
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
		assert result.exit_code == 0, result.output
	}
}
