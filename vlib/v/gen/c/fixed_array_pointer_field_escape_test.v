module main

import os

fn testsuite_begin() {
	// The MSVC that a Windows job selects through VFLAGS cannot target Linux, and
	// this file generates Linux C; let those builds use the default compiler.
	if os.getenv('VFLAGS').contains('msvc') {
		os.unsetenv('VFLAGS')
	}
}

fn test_fixed_array_pointer_field_does_not_heap_promote_value_receiver() {
	root := os.join_path(os.vtmp_dir(), 'fixed_array_pointer_field_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'input.v')
	c_path := os.join_path(root, 'output.c')
	os.write_file(path, 'struct Coordinator {
mut:
	closed bool
	reasons [9]int
}
fn (mut coordinator Coordinator) is_closed() bool {
	return coordinator.closed
}
struct Dispatch {
	coordinator &Coordinator
	generation u64
}
fn (dispatch Dispatch) valid() bool {
	return !dispatch.coordinator.is_closed()
}
fn main() {
	coordinator := &Coordinator{}
	dispatch := Dispatch{coordinator: coordinator}
	assert dispatch.valid()
}
')!
	// V1-hosted unit tests get a shared V3 compiler from their runner.
	shared_v3 := os.getenv('V3_TEST_UNIT_SHARED_V3')
	mut args := if shared_v3 != '' { [shared_v3] } else { [@VEXE, '-new-compiler'] }
	args << ['-gc', 'none', '-os', 'linux', '-arch', 'amd64', '-o', c_path, path]
	compiled := os.exec(args)
	assert compiled.exit_code == 0, compiled.output
	generated := os.read_file(c_path)!
	body := generated.all_after_last('bool Dispatch__valid(').all_after('{').all_before('\n}')
	assert body.contains('Coordinator__is_closed'), body
	assert !body.contains('memdup'), body
	assert !body.contains('__fixed_array_value_param_'), body
}
