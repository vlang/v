import os
import v.cmdexec

fn reflected_borrow_function(source string, name string) string {
	for line in source.split_into_lines() {
		if line.contains(' ${name}(') && line.ends_with(' {') {
			return source.all_after(line).all_before('\n}')
		}
	}
	panic('missing generated function ${name}')
}

fn test_reflected_method_calls_preserve_borrow_and_escape_information() {
	root := os.join_path(os.vtmp_dir(), 'comptime_method_borrow_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.cp(os.join_path(@DIR, 'testdata', 'comptime_method_borrow.vv'), source)!
	c_path := os.join_path(root, 'main.c')
	generated := cmdexec.run_with_timeout(@VEXE, ['-new-compiler', '-enable-globals', '-gc', 'none',
		'-o', c_path, source], 120_000)
	assert generated.exit_code == 0, generated.output
	c_source := os.read_file(c_path)!
	for name in ['direct_T_App', 'through_local_T_App', 'fixed_direct_T_FixedApp'] {
		body := reflected_borrow_function(c_source, name)
		assert !body.contains('memdup('), body
	}
	for name in ['fixed_direct_T_RetainingApp', 'pointer_result_T_ReturningApp'] {
		body := reflected_borrow_function(c_source, name)
		assert body.contains('memdup('), body
	}
	ran := cmdexec.run_with_timeout(@VEXE, ['-new-compiler', '-enable-globals', '-gc', 'none',
		'run', source], 120_000)
	assert ran.exit_code == 0, ran.output
	assert ran.output.trim_space() == 'OK', ran.output
}
