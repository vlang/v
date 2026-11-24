import os

fn test_nonempty_zero_allocation_failures_unwind_before_returning_or_clearing_null() ! {
	dir := os.join_path(os.vtmp_dir(), 'recover_zero_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	bin := os.join_path(dir, 'test' + $if windows { '.exe' } $else { '' })
	source := os.join_path(@VMODROOT, 'vlib/v/tests/recover/testdata/checked_zero/main.c.v')
	mut modes := [['-gc', 'none']]
	$if gcboehm ? {
		modes << ['-gc', 'boehm']
		modes << ['-gc', 'boehm', '-d', 'gcboehm_opt']
	}
	for mode in modes {
		mut command := [@VEXE, '-o', bin]
		command << mode
		command << source
		build := os.exec(command)
		assert build.exit_code == 0, build.output
		result := os.exec([bin])
		assert result.exit_code == 0, result.output
	}
}
