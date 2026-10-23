module c

import os

fn test_cached_dependencies_preserve_dispatch_and_module_test_functions() ! {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v_cached_dispatch_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	// These cover local io.Writer implementations, indirect json2.Any receivers,
	// and sqlite's calls back into the orm module under test.
	for test_file in ['io/buffered_writer_test.v', 'json2/tests/decode_any_alias_test.v',
		'orm/row_mapping_conversion_test.v'] {
		cache := os.join_path(root, os.base(test_file))
		for phase in ['cold', 'warm'] {
			binary := os.join_path(root, phase)
			// Compiler test builds can force source mode internally. Exercise the
			// actual module cache here, including warm declaration headers.
			compiled := os.exec(['env', '-u', 'V3_INTERNAL_CACHE_FORCE_SOURCE', '-u',
				'V3_CACHE_FORCE_SOURCE', 'V3CACHE=' + cache, 'V3_CACHE_DISABLE_PROGRAM_EXECUTABLE=1',
				@VEXE, '-cc', 'clang', '-show-timings', '-o', binary,
				os.join_path(@VMODROOT, 'vlib', test_file)])
			assert compiled.exit_code == 0, '${test_file} ${phase}: ${compiled.output}'
			if phase == 'warm' {
				assert compiled.output.contains('parsed .vh files'), compiled.output
			}
			result := os.exec([binary])
			assert result.exit_code == 0, '${test_file} ${phase}: ${result.output}'
		}
	}
}
