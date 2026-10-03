import os

fn test_vself_replacement_build_forces_nocache() {
	$if windows {
		return
	}
	vexe := @VEXE
	vroot := os.dir(vexe)
	noop := os.find_abs_path_of_executable('echo') or { return }
	tool := os.join_path(os.vtmp_dir(), 'vself_nocache_${os.getpid()}')
	defer {
		os.rm(tool) or {}
	}
	build := os.exec([vexe, '-nocache', '-o', '${tool}',
		os.join_path(vroot, 'cmd', 'tools', 'vself.v')])
	assert build.exit_code == 0, build.output
	result := os.exec(['env', 'VFLAGS=', 'VEXE=' + '${noop}', '${tool}', 'self', '-o',
		'/tmp/vself_nocache_test'])
	assert result.exit_code == 0, result.output
	assert result.output.contains('-nocache'), result.output
	assert result.output.contains('cmd/v'), result.output
}
