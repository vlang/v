import os
import v3.cmdexec

const single_file_test_v3_dir = os.dir(os.dir(@FILE))
const single_file_test_vlib_dir = os.dir(single_file_test_v3_dir)
const single_file_test_v3_src = os.join_path(single_file_test_v3_dir, 'v3.v')

fn test_explicit_main_test_excludes_moduleless_sibling() {
	root := os.join_path(os.vtmp_dir(), 'v3_single_file_test_input_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := os.join_path(root, 'v3')
	build := cmdexec.run(@VEXE, ['-gc', 'none', '-path',
		'${single_file_test_vlib_dir}|@vlib|@vmodules', '-o', v3_bin, single_file_test_v3_src])
	assert build.exit_code == 0, build.output

	test_file := os.join_path(root, 'selected_test.v')
	os.write_file(test_file, 'module main

fn test_selected_value() {
	assert helper_value() == 42
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'helper.v'), 'module main

fn helper_value() int {
	return 42
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'bench.v'), 'fn helper_value() int {
	return -1
}

fn main() {
	println(helper_value())
}
') or {
		panic(err)
	}

	result := cmdexec.run(v3_bin, ['-nocache', 'test', test_file])
	assert result.exit_code == 0, result.output
}
