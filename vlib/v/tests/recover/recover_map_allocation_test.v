import os

fn test_map_remains_usable_when_an_allocation_panics() ! {
	dir := os.join_path(os.vtmp_dir(), 'recover_map_alloc_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	bin := os.join_path(dir, 'map_failure' + $if windows { '.exe' } $else { '' })
	source := os.join_path(@VMODROOT, 'vlib/v/tests/recover/testdata/map_failure/main.c.v')
	compile := os.exec([@VEXE, '-gc', 'none', '-o', bin, source])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
}
