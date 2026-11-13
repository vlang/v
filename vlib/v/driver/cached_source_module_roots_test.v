module driver

import os
import v.cmdexec

fn test_cached_dependencies_keep_source_module_functions() ! {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'cached_source_module_roots_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	cache := os.join_path(root, 'cache')
	vtmp := os.join_path(root, 'tmp')
	os.mkdir_all(vtmp)!
	vroot := os.dir(@VEXE)
	source := os.join_path(vroot, 'vlib', 'sync', 'waitgroup_test.v')
	environment := {
		'VFLAGS':                              ''
		'V3CACHE':                             cache
		'VTMP':                                vtmp
		'V_MACOS_V3_NO_FALLBACK':              '1'
		'V3_CACHE_DISABLE_PROGRAM_EXECUTABLE': '1'
		'V3_DISABLE_UNCHANGED_BUILD_CACHE':    '1'
	}
	mut time_header := ''
	for phase in ['cold', 'warm'] {
		program := os.join_path(root, phase)
		// sync stays in the program unit while the cached time object calls its
		// mutex helpers. Those calls are absent from time's declaration header.
		compiled := cmdexec.run_in_merged_with_env(@VEXE, ['-new-compiler', '-no-retry-compilation',
			'-cc', 'cc', '-show-timings', '-skip-running', '-run-only', 'test_waitgroup_reuse',
			'-o', program, source], vroot, environment)
		assert compiled.exit_code == 0, '${phase}: ${compiled.output}'
		if phase == 'cold' {
			headers := os.walk_ext(cache, '.vh').filter(os.file_name(it).starts_with('time_'))
			assert headers.len > 0, compiled.output
			time_header = os.file_name(headers[0])
		} else {
			assert compiled.output.contains(time_header), compiled.output
			assert !compiled.output.contains('starting again with all of them'), compiled.output
		}
		result := cmdexec.run_with_timeout(program, [], 10_000)
		assert result.exit_code == 0, '${phase}: ${result.output}'
	}
}
