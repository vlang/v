module brotli

import os
import v.cmdexec

// Keep Clang as the normal regression compiler; constrained CI can select
// the bundled TinyCC to exercise the same module cache contract.
fn module_cache_test_compiler() ?string {
	selected := os.getenv('V_BROTLI_MODULE_CACHE_TEST_CC')
	if selected != '' {
		return selected
	}
	return os.find_abs_path_of_executable('clang') or { return none }
}

fn module_cache_test_objects(cache string) []string {
	mut objects := []string{}
	for path in os.walk_ext(cache, '.o') {
		name := os.file_name(path)
		// macOS also caches generated program units, which can differ between
		// cold and warm builds without invalidating any imported module.
		if !name.starts_with('program_prefix_') && !name.starts_with('program_main_') {
			objects << path
		}
	}
	objects.sort()
	return objects
}

fn test_module_cache_objects_ignore_program_units() ! {
	root := os.join_path(os.vtmp_dir(), 'brotli_cache_objects_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	first := os.join_path(root, 'module_first.o')
	second := os.join_path(root, 'module_second.o')
	os.write_file(first, '')!
	os.write_file(os.join_path(root, 'program_main_cold.o'), '')!
	objects := module_cache_test_objects(root)
	assert objects == [first]
	os.write_file(os.join_path(root, 'program_main_warm.o'), '')!
	os.write_file(os.join_path(root, 'program_prefix_warm.o'), '')!
	assert module_cache_test_objects(root) == objects
	// A different module object must still fail the reuse check, even when
	// the number of module objects stays the same.
	os.rm(first)!
	os.write_file(second, '')!
	assert module_cache_test_objects(root) != objects
}

fn test_web_imports_retain_the_module_cache() ! {
	$if !linux && !macos {
		return
	}
	compiler := module_cache_test_compiler() or { return }
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
		mut cached_objects := []string{}
		for build in 0 .. 2 {
			result := cmdexec.run_in_merged_with_env(@VEXE, ['-cc', compiler, '-o', program, source], @VEXEROOT, environment)
			assert result.exit_code == 0, '${module_name}: ${result.output}'
			assert !result.output.contains('module cache fallback'), result.output
			objects := module_cache_test_objects(cache)
			assert objects.len > 1, '${module_name}: no module objects were cached: ${result.output}'
			if build == 0 {
				cached_objects = objects.clone()
			} else {
				assert objects == cached_objects, '${module_name}: cached module objects changed: ${result.output}'
			}
			run := cmdexec.run(program, [])
			assert run.exit_code == 0, run.output
			assert run.output == 'true\n', run.output
		}
	}
}

fn test_brotli_unpoison_helper_survives_module_cache() ! {
	$if !linux && !macos {
		return
	}
	compiler := module_cache_test_compiler() or { return }
	root := os.join_path(os.vtmp_dir(), 'brotli_noop_cache_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	program := os.join_path(root, 'main')
	cache := os.join_path(root, 'cache')
	program_source := 'import compress.brotli\n' +
		'fn main() {\n' +
		'\tencoded := brotli.compress([]u8{}, brotli.CompressParams{}) or { panic(err) }\n' +
		'\t_ := brotli.decompress(encoded, brotli.DecompressParams{}) or { panic(err) }\n' +
		'}\n'
	os.write_file(source, program_source)!
	environment := {
		'VFLAGS':                 ''
		'VTMP':                   os.join_path(root, 'tmp')
		'V3CACHE':                cache
		'V3_CACHE_TRACE':         '1'
		'V_MACOS_V3_NO_FALLBACK': '1'
	}
	mut cached_objects := []string{}
	for build in 0 .. 2 {
		result := cmdexec.run_in_merged_with_env(@VEXE, ['-cc', compiler, '-o', program, source], @VEXEROOT, environment)
		assert result.exit_code == 0, result.output
		assert !result.output.contains('module cache fallback'), result.output
		objects := module_cache_test_objects(cache)
		assert objects.len > 0, '${build}: no module objects were cached: ${result.output}'
		if build == 0 {
			cached_objects = objects.clone()
		} else {
			assert objects == cached_objects, 'cached module objects changed: ${result.output}'
		}
	}
}
