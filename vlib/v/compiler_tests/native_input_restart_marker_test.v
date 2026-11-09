import os

const vexe = @VEXE

// A shipped native header that cannot be assigned to cache units makes a build
// start over without the cache once it has parsed the program. The marker lets
// the next build with the same arguments start without the cache directly.
fn test_a_shipped_native_input_restarts_only_the_first_build() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'native_input_restart_marker_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'vtmp'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'),
		"import v.cmdexec\n\nfn main() {\n\tprintln(cmdexec.run('true', []string{}).exit_code)\n}\n")!
	output := os.join_path(root, 'program')
	saved_vtmp := os.getenv('VTMP')
	os.setenv('VTMP', os.join_path(root, 'vtmp'), true)
	os.setenv('V3_DISABLE_UNCHANGED_BUILD_CACHE', '1', true)
	os.setenv('V3_CACHE_TRACE', '1', true)
	defer {
		os.setenv('VTMP', saved_vtmp, true)
		os.unsetenv('V3_DISABLE_UNCHANGED_BUILD_CACHE')
		os.unsetenv('V3_CACHE_TRACE')
	}
	first := os.exec([vexe, '-o', output, os.join_path(root, 'main.v')])
	assert first.exit_code == 0, first.output
	assert first.output.contains('external C inputs cannot be assigned to cache units'), first.output
	second := os.exec([vexe, '-o', output, os.join_path(root, 'main.v')])
	assert second.exit_code == 0, second.output
	assert !second.output.contains('external C inputs cannot be assigned to cache units'), second.output
	assert second.output.contains('an earlier build with these arguments'), second.output
	assert os.exec([output]).output.trim_space() == '0'
}
