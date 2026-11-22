module main

import os

fn testsuite_begin() {
	// The MSVC that a Windows job selects through VFLAGS cannot target Linux, and
	// this file generates Linux C; let those builds use the default compiler.
	if os.getenv('VFLAGS').contains('msvc') {
		os.unsetenv('VFLAGS')
	}
}

fn test_map_equality_borrows_keys_but_authored_iteration_keeps_clones() {
	root := os.join_path(os.vtmp_dir(), 'map_equality_key_borrow_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'input.v')
	c_path := os.join_path(root, 'output.c')
	os.write_file(path, 'fn compare_maps(left map[string]f64, right map[string]f64) bool {
	return left == right
}
fn first_key(values map[string]int) string {
	for map_eq_key_0, _ in values { return map_eq_key_0 }
	return ""
}
struct Entry {
	value int
}
fn (left Entry) == (right Entry) bool {
	return left.value == right.value
}
fn compare_entries(left map[string][]Entry, right map[string][]Entry) bool {
	return left == right
}
fn main() {
	assert compare_maps({"one": f64(1)}, {"one": f64(1)})
	assert first_key({"one": 1}) == "one"
	assert compare_entries({"one": [Entry{1}]}, {"one": [Entry{1}]})
}
')!
	// V1-hosted unit tests get a shared V3 compiler from their runner.
	shared_v3 := os.getenv('V3_TEST_UNIT_SHARED_V3')
	mut args := if shared_v3 != '' { [shared_v3] } else { [@VEXE, '-new-compiler'] }
	args << ['-gc', 'none', '-os', 'linux', '-arch', 'amd64', '-o', c_path, path]
	compiled := os.exec(args)
	assert compiled.exit_code == 0, compiled.output
	generated := os.read_file(c_path)!
	equality := generated.all_after_last('bool compare_maps(').all_after('{').all_before('\n}')
	assert equality.contains('map__exists'), equality
	assert !equality.contains('string__clone'), equality
	iteration := generated.all_after_last('string first_key(').all_after('{').all_before('\n}')
	assert iteration.contains('string__clone(map_eq_key_0)'), iteration
	custom := generated.all_after_last('bool compare_entries(').all_after('{').all_before('\n}')
	assert custom.contains('string__clone'), custom
}
