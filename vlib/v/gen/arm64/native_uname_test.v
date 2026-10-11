module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.types

fn test_native_uname_uses_inline_arrays_with_pointer_field_values() {
	$if !macos || !arm64 {
		return
	}
	path := os.join_path(os.vtmp_dir(), 'arm64_uname_${os.getpid()}.v')
	output := path.all_before_last('.')
	defer {
		os.rm(path) or {}
		os.rm(output) or {}
	}
	os.write_file(path, 'module main
pub struct C.utsname {
	sysname &char
	nodename &char
	release &char
	version &char
	machine &char
}
fn C.uname(&C.utsname) int
fn C.strlen(&char) usize
fn C.strcmp(&char, &char) int
fn C.exit(int)
struct UnameView { value &C.utsname }
struct PointerField { value &char }
fn copy_uname(value &C.utsname) C.utsname {
	return unsafe { *value }
}
fn main() {
	if sizeof(C.utsname) != 1280 { C.exit(1) }
	mut value := C.utsname{}
	if C.uname(&value) != 0 { C.exit(2) }
	pointer := &value
	if C.strcmp(value.sysname, c"Darwin") != 0 { C.exit(3) }
	if C.strlen(pointer.nodename) == 0 { C.exit(4) }
	if C.strlen(pointer.release) == 0 { C.exit(5) }
	if C.strlen(pointer.version) == 0 { C.exit(6) }
	if C.strcmp(pointer.machine, c"arm64") != 0 { C.exit(7) }
	view := UnameView{value: pointer}
	if C.strcmp(view.value.release, value.release) != 0 { C.exit(8) }
	if C.strcmp(copy_uname(pointer).machine, value.machine) != 0 { C.exit(9) }
	control := PointerField{value: c"pointer value"}
	if C.strcmp(control.value, c"pointer value") != 0 { C.exit(10) }
}
')!
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
	assert result.exit_code == 0, result.output
}
