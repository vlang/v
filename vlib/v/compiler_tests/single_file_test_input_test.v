import os
import v.cmdexec
import v.tempname

// An explicit `module main` test is compiled with the `main` sources beside it, and a file
// without a module declaration is in `main` too. This used to exclude the module-less sibling;
// 946a1aad85 includes it, as V1 did (a module test compiles every .v file of its directory).
fn test_explicit_main_test_includes_moduleless_sibling() {
	root := os.join_path(os.vtmp_dir(), 'v3_single_file_test_input_${tempname.unique_token()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or {}
	}

	test_file := os.join_path(root, 'selected_test.v')
	os.write_file(test_file, 'module main

fn test_selected_value() {
	assert helper_value() == 42
	assert bench_value() == 7
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
	os.write_file(os.join_path(root, 'bench.v'), 'fn bench_value() int {
	return 7
}
') or {
		panic(err)
	}

	// The outer runner's filters select this test, while the child must run its fixture.
	result := cmdexec.run_in_merged_with_env(@VEXE, ['-nocache', 'test', test_file], '', {
		'VTEST_ONLY':    ''
		'VTEST_ONLY_FN': ''
	})
	assert result.exit_code == 0, result.output
	assert result.output.contains('1 passed'), result.output
}
