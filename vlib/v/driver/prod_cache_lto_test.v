module driver

import os

fn test_production_module_cache_contains_native_objects() ! {
	$if linux {
		cc := os.find_abs_path_of_executable('cc') or { return }
		root := os.join_path(os.vtmp_dir(), 'prod_cache_lto_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		source := os.join_path(root, 'hello.v')
		executable := os.join_path(root, 'hello')
		cache := os.join_path(root, 'cache')
		os.write_file(source, 'import os\nfn main() { println(os.args.len) }\n')!
		mut environment := os.environ()
		environment.delete('VFLAGS')
		environment['V3CACHE'] = cache
		environment['V_MACOS_V3_NO_FALLBACK'] = '1'
		mut process := os.new_process(@VEXE)
		process.set_environment(environment)
		process.set_args(['-showcc', '-prod', '-cc', cc, '-gc', 'none', '-o', executable, source])
		process.set_redirect_stdio()
		process.wait()
		output := process.stdout_slurp() + process.stderr_slurp()
		code := process.code
		process.close()
		assert code == 0, output
		assert output.contains('-flto'), output
		objects := os.walk_ext(cache, '.o')
		assert objects.len > 0
		for object in objects {
			// GCC embeds this section prefix in every LTO object. Such objects
			// cannot be replaced safely while another linker uses the cache.
			assert !os.read_file(object)!.contains('.gnu.lto_'), object
		}
		result := os.exec([executable])
		assert result.exit_code == 0, result.output
		assert result.output.trim_space() == '1'
	}
}
