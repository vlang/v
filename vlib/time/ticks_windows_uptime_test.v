module time

import os
import v.cmdexec

fn test_windows_ticks_preserves_uptime_past_32_bits() {
	workspace := os.join_path(os.vtmp_dir(), 'time_ticks_${os.getpid()}')
	os.mkdir_all(workspace) or { panic(err) }
	defer {
		os.rmdir_all(workspace) or {}
	}
	mut executable := os.join_path(workspace, 'uptime_probe')
	$if windows {
		executable += '.exe'
	}
	fixture := os.join_path(@VEXEROOT, 'vlib', 'time', 'testdata', 'windows_ticks')
	compiler := os.find_abs_path_of_executable('cc') or {
		os.find_abs_path_of_executable('gcc') or { panic('C compiler required for uptime probe') }
	}
	compiled := cmdexec.run(compiler, ['-std=c99', '-Wall', '-Wextra', '-Werror', '-I', fixture,
		'-I', os.join_path(@VEXEROOT, 'vlib', 'time'), os.join_path(fixture, 'uptime.c'), '-o',
		executable])
	assert compiled.exit_code == 0, compiled.output
	ran := cmdexec.run(executable, [])
	assert ran.exit_code == 0, ran.output
}
