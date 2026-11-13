import crypto.sha256
import os
import v.cmdexec

fn test_cached_builtin_exports_link_without_exports_in_main() {
	$if !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'cached_builtin_export_init_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	cache := os.join_path(root, 'cache')
	environment := {
		'VFLAGS':                              ''
		'VOSARGS':                             ''
		'CFLAGS':                              ''
		'LDFLAGS':                             ''
		'V3CACHE':                             cache
		'V3_INTERNAL_CACHE_FORCE_SOURCE':      ''
		'V3_CACHE_FORCE_SOURCE':               ''
		'V3_CACHE_DISABLE_PROGRAM_EXECUTABLE': '1'
		'V3_DISABLE_UNCHANGED_BUILD_CACHE':    '1'
		'V_MACOS_V3_NO_FALLBACK':              '1'
	}
	mut first_objects := map[string]string{}
	for phase in ['cold', 'warm'] {
		source := os.join_path(root, '${phase}.v')
		// Distinct entry sources require new C generation on the warm build.
		os.write_file(source, "import os

fn main() {
	assert os.args[1..] == ['alpha', 'beta']
	println(os.args[1..].join(','))
}
// ${phase}
")!
		executable := os.join_path(root, phase)
		// Debug builds retain musl's exported backtrace stubs, whose cached
		// wrappers call the initializer. This also reproduces on glibc hosts.
		compiled := cmdexec.run_in_merged_with_env(@VEXE, ['-new-compiler', '-no-retry-compilation',
			'-cc', 'cc', '-gc', 'none', '-d', 'musl', '-cg', '-show-timings', '-o', executable,
			source], os.dir(@VEXE), environment)
		assert compiled.exit_code == 0, '${phase}: ${compiled.output}'
		mut objects := map[string]string{}
		for path in os.walk_ext(cache, '.o') {
			if os.file_name(path).starts_with('builtin_') {
				objects[path] = sha256.hexhash(os.read_file(path)!)
			}
		}
		assert objects.len > 0, 'no cached builtin module in ${cache}'
		if phase == 'cold' {
			first_objects = objects.clone()
		} else {
			headers := os.walk_ext(cache, '.vh').filter(os.file_name(it).starts_with('builtin_'))
			assert headers.len > 0, compiled.output
			// Timing reports shorten paths below HOME; the cache filename still
			// identifies the exact builtin declaration header that was reused.
			assert compiled.output.contains(os.file_name(headers[0])), compiled.output
			assert !compiled.output.contains('Caching module builtin...'), compiled.output
			assert objects == first_objects, 'cached builtin objects changed'
		}
		run := cmdexec.run_with_timeout(executable, ['alpha', 'beta'], 10_000)
		assert run.exit_code == 0, '${phase}: ${run.output}'
		assert run.output.trim_space() == 'alpha,beta', run.output
	}
}
