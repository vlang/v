module main

#flag @VEXEROOT/vlib/v/compiler_tests/testdata/freestanding_native_flags/native_flags.o

fn C.v_native_runtime_capabilities() i32

fn main() {
	capabilities := C.v_native_runtime_capabilities()
	$if freestanding {
		if capabilities != 1 { exit(100) }
	} $else {
		if capabilities & 7 != 4 { exit(100) }
	}
	println('native runtime flags passed')
}
