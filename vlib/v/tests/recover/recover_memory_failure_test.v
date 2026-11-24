import os

fn test_recover_from_an_allocator_that_continues_to_deny_memory() ! {
	dir := os.join_path(os.vtmp_dir(), 'recover_oom_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	bin := os.join_path(dir, 'test' + $if windows { '.exe' } $else { '' })
	source := os.join_path(@VMODROOT, 'vlib/v/tests/recover/testdata/memory_failure/main.c.v')
	build := os.exec([@VEXE, '-gc', 'none', '-o', bin, source])
	assert build.exit_code == 0, build.output
	result := os.exec([bin])
	assert result.exit_code == 0, result.output
}
