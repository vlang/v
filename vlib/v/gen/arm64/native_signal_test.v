module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.types

fn test_native_signal_handler_calls_fallback_on_alternate_stack() {
	$if macos && arm64 {
		run_native_signal_fixture('alternate', 'module main
struct C.stack_t {
    ss_sp voidptr
    ss_size usize
    ss_flags int
}
fn C.exit(int)
fn C.sigaltstack(voidptr, &C.stack_t) int
fn C.raise(int) int
fn C.v_install_segfault_handler(voidptr, voidptr)
fn fallback(signal int) {
    if signal != 11 { C.exit(1) }
    mut stack := C.stack_t{}
    if C.sigaltstack(unsafe { nil }, &stack) != 0 { C.exit(2) }
    if stack.ss_flags & 1 == 0 { C.exit(3) }
    C.exit(77)
}
fn main() {
    C.v_install_segfault_handler(voidptr(fallback), unsafe { nil })
    C.raise(11)
    C.exit(4)
}
', 77)
	}
}

fn test_native_signal_installation_preserves_existing_handler() {
	$if macos && arm64 {
		run_native_signal_fixture('preserve', 'module main
fn C.exit(int)
fn C.signal(int, voidptr) voidptr
fn C.raise(int) int
fn C.v_install_segfault_handler(voidptr, voidptr)
fn existing(signal int) { C.exit(78) }
fn fallback(signal int) { C.exit(1) }
fn main() {
    C.signal(11, voidptr(existing))
    C.v_install_segfault_handler(voidptr(fallback), unsafe { nil })
    C.raise(11)
    C.exit(2)
}
', 78)
	}
}

fn run_native_signal_fixture(name string, source string, expected int) {
	path := os.join_path(os.vtmp_dir(), 'arm64_signal_${name}_${os.getpid()}.v')
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
	assert result.exit_code == expected, '${name}: ${result.exit_code}: ${result.output}'
}
