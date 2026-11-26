import os

fn test_map_deletion_after_allocator_denial() ! {
	dir := os.join_path(os.vtmp_dir(), 'recover_map_delete_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	bin := os.join_path(dir, 'probe' + $if windows { '.exe' } $else { '' })
	src := os.join_path(@VMODROOT, 'vlib/v/tests/recover/testdata/map_delete_failure/main.c.v')
	build := os.exec([@VEXE, '-gc', 'none', '-o', bin, src])
	assert build.exit_code == 0, build.output
	first := os.exec([bin, 'first-hole'])
	compact := os.exec([bin, 'compact'])
	assert first.exit_code == 0, first.output
	assert compact.exit_code == 0, compact.output
}
