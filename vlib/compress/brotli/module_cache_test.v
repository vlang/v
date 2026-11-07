module brotli

import os
import v.cmdexec

fn test_web_imports_retain_the_module_cache() ! {
	$if !linux && !macos {
		return
	}
	compiler := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'brotli_web_cache_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for module_name, type_name in {
		'net.http':      'http.Request'
		'net.websocket': 'websocket.Client'
		'veb':           'veb.Context'
	} {
		name := module_name.replace('.', '_')
		source := os.join_path(root, '${name}.v')
		program := os.join_path(root, name)
		cache := os.join_path(root, '${name}_cache')
		os.write_file(source, 'import ${module_name}\nfn main() { println(sizeof(${type_name}) > 0) }\n')!
		environment := {
			'VFLAGS':                 ''
			'VTMP':                   os.join_path(root, 'tmp')
			'V3CACHE':                cache
			'V3_CACHE_TRACE':         '1'
			'V_MACOS_V3_NO_FALLBACK': '1'
		}
		mut count := 0
		for build in 0 .. 2 {
			result := cmdexec.run_in_merged_with_env(@VEXE, ['-cc', compiler, '-o', program, source], @VEXEROOT, environment)
			assert result.exit_code == 0, '${module_name}: ${result.output}'
			assert !result.output.contains('module cache fallback'), result.output
			objects := os.walk_ext(cache, '.o')
			assert objects.len > 1, '${module_name}: no module objects were cached: ${result.output}'
			if build == 0 {
				count = objects.len
			} else {
				assert objects.len == count
			}
			run := cmdexec.run(program, [])
			assert run.exit_code == 0, run.output
			assert run.output == 'true\n', run.output
		}
	}
}
