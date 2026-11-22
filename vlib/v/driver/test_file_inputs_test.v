module driver

import os
import time
import v.cmdexec

fn batched_inputs_root() !string {
	root := os.join_path(os.vtmp_dir(), 'v_test_file_inputs_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	return root
}

fn test_only_distinct_test_files_of_one_directory_share_a_compilation() ! {
	root := batched_inputs_root()!
	defer {
		os.rmdir_all(root) or {}
	}
	other := os.join_path(root, 'other')
	os.mkdir_all(other)!
	first := os.join_path(root, 'first_test.v')
	second := os.join_path(root, 'second_test.v')
	elsewhere := os.join_path(other, 'third_test.v')
	plain := os.join_path(root, 'util.v')
	for path in [first, second, elsewhere, plain] {
		os.write_file(path, 'module util\n')!
	}
	assert v_test_batch_input_allowed(first, [], second)
	assert !v_test_batch_input_allowed(first, [second], second)
	assert !v_test_batch_input_allowed(first, [], first)
	assert !v_test_batch_input_allowed(first, [], elsewhere)
	assert !v_test_batch_input_allowed(first, [], plain)
	assert !v_test_batch_input_allowed(plain, [], first)
	assert !v_test_batch_input_allowed(first, [], os.join_path(root, 'missing_test.v'))
}

fn test_internal_tests_of_one_module_build_into_one_binary() ! {
	root := batched_inputs_root()!
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'util.v'), 'module util\n\npub fn twice(x int) int {\n\treturn 2 * x\n}\n')!
	first := os.join_path(root, 'first_test.v')
	second := os.join_path(root, 'second_test.v')
	failing := os.join_path(root, 'failing_test.v')
	os.write_file(first, 'module util\n\nfn test_first() {\n\tassert twice(1) == 2\n}\n')!
	os.write_file(second, 'module util\n\nfn test_second() {\n\tassert twice(2) == 4\n}\n')!
	os.write_file(failing, 'module util\n\nfn test_failing() {\n\tassert twice(3) == 7\n}\n')!
	env := {
		'VFLAGS': ''
	}
	binary := os.join_path(root, 'batch')
	built := cmdexec.run_in_merged_with_env(@VEXE, ['-skip-running', '-o', binary, first, second],
		root, env)
	assert built.exit_code == 0, built.output
	passed := cmdexec.run(binary, [])
	assert passed.exit_code == 0, passed.output
	// The module's sources are compiled once, and every file's tests run.
	failed_binary := os.join_path(root, 'failed')
	failed_build := cmdexec.run_in_merged_with_env(@VEXE, ['-skip-running', '-o', failed_binary,
		first, failing], root, env)
	assert failed_build.exit_code == 0, failed_build.output
	failed := cmdexec.run(failed_binary, [])
	assert failed.exit_code != 0
	assert failed.output.contains('failing_test.v'), failed.output
	for inputs in [[first, first], [first, os.join_path(root, 'util.v')]] {
		mut args := ['-skip-running', '-o', binary]
		args << inputs
		rejected := cmdexec.run_in_merged_with_env(@VEXE, args, root, env)
		assert rejected.exit_code != 0
		assert rejected.output.contains('multiple input paths are not supported'), rejected.output
	}
}

fn test_test_functions_of_one_name_in_several_files_all_run() ! {
	root := batched_inputs_root()!
	defer {
		os.rmdir_all(root) or {}
	}
	first := os.join_path(root, 'first_test.v')
	second := os.join_path(root, 'second_test.v')
	// One file names its module, the other one does not: both are `main`.
	os.write_file(first, 'module main\n\nfn test_main() {\n\tprintln("first main ran")\n}\n')!
	os.write_file(second, 'fn test_main() {\n\tprintln("second main ran")\n\tassert 1 + 1 == 2\n}\n')!
	env := {
		'VFLAGS': ''
	}
	binary := os.join_path(root, 'batch')
	built := cmdexec.run_in_merged_with_env(@VEXE, ['-skip-running', '-o', binary, first, second],
		root, env)
	assert built.exit_code == 0, built.output
	ran := cmdexec.run(binary, [])
	assert ran.exit_code == 0, ran.output
	assert ran.output.contains('first main ran'), ran.output
	assert ran.output.contains('second main ran'), ran.output
}
