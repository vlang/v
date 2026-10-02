import os
import v.cmdexec

fn test_result_error_propagation_compiles_with_cold_and_warm_builtin_cache() {
	os.find_abs_path_of_executable('cc') or {
		eprintln('skipping module cache test: cc is unavailable')
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_result_error_cache_${os.getpid()}')
	os.mkdir_all(root)!
	saved := os.environ()
	env_names := ['V3CACHE', 'V3_CACHE_TRACE', 'V3_CACHE_FORCE_SOURCE', 'VFLAGS', 'CFLAGS', 'LDFLAGS']
	defer {
		for name in env_names {
			if name in saved {
				os.setenv(name, saved[name], true)
			} else {
				os.unsetenv(name)
			}
		}
		os.rmdir_all(root) or { panic(err) }
	}
	cache := os.join_path(root, 'cache')
	os.setenv('V3CACHE', cache, true)
	os.setenv('V3_CACHE_TRACE', '1', true)
	os.unsetenv('V3_CACHE_FORCE_SOURCE')
	os.unsetenv('VFLAGS')
	os.unsetenv('CFLAGS')
	os.unsetenv('LDFLAGS')
	vexe := os.join_path(@VMODROOT, 'v' + $if windows { '.exe' } $else { '' })
	for attempt in 0 .. 2 {
		// Distinct programs must consume the shared builtin cache rather than a whole-program hit.
		source := os.join_path(root, 'program_${attempt}.v')
		os.write_file(source, "struct Fault {\n message string\n}\nfn (f Fault) msg() string { return f.message }\nfn (f Fault) code() int { return 17 }\nfn fail(boxed bool) !int {\n if boxed { return Fault{message: 'boxed'} }\n return error('boom')\n}\nfn caller(boxed bool) !int { return fail(boxed)! }\nfn main() {\n for boxed in [false, true] {\n caller(boxed) or {\n assert err.msg() == if boxed { 'boxed' } else { 'boom' }\n assert err.code() == if boxed { 17 } else { 0 }\n continue\n }\n panic('expected an error')\n }\n println('errors preserved ${attempt}')\n}\n")!
		output := os.join_path(root, 'program_${attempt}')
		build := cmdexec.run_with_timeout(vexe, ['-new-compiler', '-no-retry-compilation', '-gc',
			'none', '-cc', 'cc', '-showcc', '-o', output, source],
			120_000)
		assert build.exit_code == 0, build.output
		assert !build.output.contains('V3 module cache fallback'), build.output
		builtin_objects := os.walk_ext(cache, '.o').filter(os.base(it).starts_with('builtin_'))
		assert builtin_objects.len == 1, builtin_objects.str()
		assert build.output.contains(os.base(builtin_objects[0])), build.output
		if attempt == 0 {
			assert build.output.contains('module=builtin'), build.output
			assert os.walk_ext(cache, '.vh').any(os.base(it).starts_with('builtin_'))
		} else {
			assert !build.output.contains('miss: module=builtin'), build.output
		}
		assert !build.output.contains('drop_owned_T_IError'), build.output
		run := cmdexec.run_with_timeout(output, [], 10_000)
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'errors preserved ${attempt}', run.output
	}
}
