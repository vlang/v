module main

import os

fn test_cached_dependencies_preserve_dispatch_and_module_test_functions() ! {
	$if !macos && !linux {
		return
	}
	// The Ubuntu musl image links SQLite with musl-gcc. Nested cached V builds
	// must use the same C runtime; other hosts retain Clang.
	is_ubuntu_musl := os.getenv('V_CI_UBUNTU_MUSL') != ''
	cc := if is_ubuntu_musl { 'musl-gcc' } else { 'clang' }
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
			mut command := ['env', '-u', 'V3_INTERNAL_CACHE_FORCE_SOURCE', '-u', 'V3_CACHE_FORCE_SOURCE',
				'V3CACHE=' + cache, 'V3_CACHE_DISABLE_PROGRAM_EXECUTABLE=1', @VEXE, '-cc', cc,
				'-show-timings']
			// vtest-self removes VFLAGS before running each test. Nested musl builds
			// must keep the matching GC-free toolchain flags explicitly, even in full VLIB.
			if is_ubuntu_musl {
				command << ['-gc', 'none', '-d', 'use_bundled_libgc']
			}
			// Instrumented SQLite needs the same MemorySanitizer runtime at the final link.
			// The job's VFLAGS do not reach this build, so also keep its `-gc none`:
			// MemorySanitizer reports Boehm's partially initialized sigaction at GC_init.
			if os.getenv('SANITIZER_GITHUB_JOB') == 'sanitize-memory-clang' {
				command << ['-gc', 'none', '-cflags', '-fsanitize=memory']
			}
			command << ['-o', binary, os.join_path(@VMODROOT, 'vlib', test_file)]
			compiled := os.exec(command)
			assert compiled.exit_code == 0, '${test_file} ${phase}: ${compiled.output}'
			if phase == 'warm' {
				assert compiled.output.contains('parsed .vh files'), compiled.output
			}
			result := os.exec([binary])
			assert result.exit_code == 0, '${test_file} ${phase}: ${result.output}'
		}
	}
}
