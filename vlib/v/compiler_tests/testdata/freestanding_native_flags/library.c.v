module main

#flag @VEXEROOT/vlib/v/compiler_tests/testdata/freestanding_native_flags/native_flags.o

fn C.v_native_runtime_capabilities() i32

@[export: 'v_native_flags_probe']
fn library_probe() i32 { return C.v_native_runtime_capabilities() }
