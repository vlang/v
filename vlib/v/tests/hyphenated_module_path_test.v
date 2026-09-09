import os
import rand

const vexe = @VEXE

fn test_module_in_hyphenated_parent_path_compiles() {
	test_root := os.join_path(os.vtmp_dir(), 'hyphenated_module_path_${rand.ulid()}')
	module_dir := os.join_path(test_root, 'some-dir', 'somemodule')
	os.mkdir_all(module_dir)!
	defer {
		os.rmdir_all(test_root) or {}
	}
	os.write_file(os.join_path(module_dir, 'somemodule.v'), 'module somemodule

pub fn value() int {
	return 3
}
')!
	os.write_file(os.join_path(module_dir, 'some_test.v'), 'module somemodule

fn test_value() {
	assert value() == 3
}
')!
	old_wd := os.getwd()
	defer {
		os.chdir(old_wd) or {}
	}
	os.chdir(test_root)!
	module_path := os.join_path('some-dir', 'somemodule')
	cmd := '${os.quoted_path(vexe)} test ${os.quoted_path(module_path)}'
	res := os.execute(cmd)
	if res.exit_code != 0 {
		eprintln('> failing test cmd: ${cmd}')
		eprintln('> output:\n${res.output}')
	}
	assert res.exit_code == 0
}
