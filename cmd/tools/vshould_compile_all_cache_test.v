import os

fn test_should_compile_all_reuses_native_module_cache() ! {
	$if !(linux || macos) {
		return
	}
	if _ := os.find_abs_path_of_executable('cc') {
	} else {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vshould_compile_all_cache_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	cache := os.join_path(root, 'cache')
	examples := os.join_path(root, 'examples with spaces')
	os.mkdir_all(examples)!
	for name, count in {
		'first':  2
		'second': 3
	} {
		os.write_file(os.join_path(examples, '${name}.v'), 'import strconv\nimport strings\nfn main() {\ncount := strconv.atoi("${count}") or { panic(err) }\nassert strings.repeat(120, count).len == count\n}\n')!
	}
	mut environment := os.environ()
	for name in ['CFLAGS', 'LDFLAGS', 'VOSARGS', 'V3_CACHE_FORCE_SOURCE',
		'V3_INTERNAL_CACHE_FORCE_SOURCE'] {
		environment.delete(name)
	}
	environment['V3CACHE'] = cache
	environment['VFLAGS'] = '-cc cc'
	environment['VJOBS'] = '1'
	environment['V3_CACHE_TRACE'] = '1'
	environment['V3_DISABLE_UNCHANGED_BUILD_CACHE'] = '1'
	environment['V3_CACHE_DISABLE_PROGRAM_EXECUTABLE'] = '1'
	environment['V_MACOS_V3_NO_FALLBACK'] = '1'
	tool := os.join_path(root, 'vshould-compile-all')
	built := run_should_compile_cache_command(@VEXE, ['-o', tool,
		os.join_path(@VEXEROOT, 'cmd', 'tools', 'vshould-compile-all.v')], environment)
	assert built.exit_code == 0, built.output
	// Use make's native warmup, independently of the tool's os/log imports and
	// their different C flags and interface set.
	warmup := os.join_path(@VEXEROOT, 'cmd', 'tools', 'cache_basic_modules.v')
	warmed := run_should_compile_cache_command(@VEXE, ['-new-compiler', 'run', warmup], environment)
	assert warmed.exit_code == 0, warmed.output
	builtin_objects := os.walk_ext(cache, '.o').filter(os.file_name(it).starts_with('builtin_'))
	assert builtin_objects.len > 0, warmed.output
	// The GUI script appends the compiler already selected by the workflow.
	environment['VFLAGS'] = '-cc cc -cc cc -showcc'
	result := run_should_compile_cache_command(tool, ['should-compile-all', examples], environment)
	assert result.exit_code == 0, result.output
	assert builtin_objects.any(result.output.contains(it)), result.output
	assert !result.output.contains('V3 module cache miss: module=builtin'), result.output
	assert !result.output.contains('V3 module cache object miss: module=builtin'), result.output
	for name in ['first', 'second'] {
		ran := os.exec([os.join_path(examples, name)])
		assert ran.exit_code == 0, ran.output
	}
	// An inherited backend must not change ordinary examples into JS builds.
	environment['VFLAGS'] = '-cc cc -b js'
	overridden := run_should_compile_cache_command(tool, ['should-compile-all', examples],
		environment)
	assert overridden.exit_code == 0, overridden.output
	assert overridden.output.contains(' -b c '), overridden.output
}

fn run_should_compile_cache_command(executable string, args []string, environment map[string]string) os.Result {
	mut process := os.new_process(executable)
	process.set_args(args)
	process.set_environment(environment)
	process.set_redirect_stdio_merged()
	process.run()
	output := process.stdout_slurp()
	process.wait()
	result := os.Result{
		exit_code: process.code
		output:    output
	}
	process.close()
	return result
}
