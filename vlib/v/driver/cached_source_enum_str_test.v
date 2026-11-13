module driver

import os

fn test_cached_body_keeps_builtin_enum_stringifier_after_source_refresh() ! {
	cc := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'cached_source_enum_str_${os.getpid()}')
	app := os.join_path(root, 'app')
	modules := os.join_path(root, 'modules')
	os.mkdir_all(app)!
	os.mkdir_all(os.join_path(modules, 'cached'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(app, 'v.mod'), "Module { name: 'cached_source_enum_str' }")!
	os.write_file(os.join_path(modules, 'cached', 'cached.v'), 'module cached

pub fn describe(kind AttributeKind) string {
	return "\${kind}"
}

pub fn touch() {}
')!
	source := os.join_path(app, 'main.v')
	executable := os.join_path(root, 'probe' + $if windows { '.exe' } $else { '' })
	cache := os.join_path(root, 'cache')
	mut environment := os.environ()
	for name in ['VFLAGS', 'VEXE', 'V3_CACHE_FORCE_SOURCE', 'V3_INTERNAL_CACHE_FORCE_SOURCE'] {
		environment.delete(name)
	}
	environment['V3CACHE'] = cache
	environment['VMODULES'] = modules
	environment['V_MACOS_V3_NO_FALLBACK'] = '1'
	for warm in [false, true] {
		body := if warm { 'cached.touch()' } else { 'println(cached.describe(.string))' }
		os.write_file(source, 'import cached\nfn main() { ${body} }\n')!
		if warm {
			// A refreshed dependency is parsed from .v while the cached caller's
			// .vh omits the body that names its generated enum stringifier.
			environment['V3_INTERNAL_CACHE_FORCE_SOURCE'] = 'builtin'
		}
		mut process := os.new_process(@VEXE)
		process.set_environment(environment)
		process.set_args(['-cc', cc, '-gc', 'none', '-path', '${modules}|@vlib', '-o', executable,
			source])
		process.set_redirect_stdio()
		process.wait()
		output := process.stdout_slurp() + process.stderr_slurp()
		code := process.code
		process.close()
		assert code == 0, 'warm=${warm}: ${output}'
		run := os.exec([executable])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == if warm { '' } else { 'string' }
		assert os.walk_ext(cache, '.o').any(os.file_name(it).starts_with('cached_'))
	}
}
