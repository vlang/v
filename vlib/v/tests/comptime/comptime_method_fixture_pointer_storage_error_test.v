import os

fn test_checker_fixture_rejects_forwarded_mut_value_for_pointer_storage() {
	fixture := 'vlib/v/checker/tests/modules/comptime_forwarded_mut_pointer_storage'
	root := os.join_path(os.vtmp_dir(), 'fixture_pointer_storage_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut process := os.new_process(@VEXE)
	process.set_work_folder(@VEXEROOT)
	process.set_args(['-new-compiler', '-no-retry-compilation', '-nocache', '-checker-fixture',
		'-b', 'c', '-prod', '-o', os.join_path(root, 'invalid'), fixture])
	process.set_redirect_stdio_merged()
	process.run()
	output := process.stdout_slurp()
	process.wait()
	exit_code := process.code
	process.close()
	expected := os.read_file(os.join_path(@VEXEROOT, fixture + '.out'))!
	assert exit_code == 1, output
	assert output == expected, output
}
