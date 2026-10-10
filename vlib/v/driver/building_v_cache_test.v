module driver

import os

fn test_building_v_discovers_generic_calls_in_cold_cached_library_bodies() {
	// The module cache requires the platform cc, rather than another C compiler.
	os.find_abs_path_of_executable('cc') or {
		eprintln('SKIP: the compiler-build module cache regression requires cc')
		return
	}
	keys := ['VFLAGS', 'VOSARGS', 'V3_CACHE_FORCE_SOURCE']
	mut previous_environment := map[string]string{}
	for key in keys {
		if value := os.getenv_opt(key) { previous_environment[key] = value }
		os.unsetenv(key)
	}
	defer {
		for key in keys {
			if value := previous_environment[key] {
				os.setenv(key, value, true)
			} else {
				os.unsetenv(key)
			}
		}
	}
	root := os.join_path(os.vtmp_dir(), 'building_v_cache_generics_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	cache := os.join_path(root, 'cache')
	previous_cache := os.getenv_opt('V3CACHE')
	os.setenv('V3CACHE', cache, true)
	defer {
		if value := previous_cache {
			os.setenv('V3CACHE', value, true)
		} else {
			os.unsetenv('V3CACHE')
		}
	}
	data := os.join_path(root, 'data.txt')
	source := os.join_path(root, 'main.v')
	payload := 'compiler bytes\n'.repeat(200)
	os.write_file(data, payload)!
	os.write_file(source, 'fn main() {
	file := \$embed_file("data.txt")
	text := file.to_string()
	bytes := file.to_bytes()
	println("\${text.len}:\${bytes.len}")
}')!
	for index in 0 .. 2 {
		output := os.join_path(root, 'probe_${index}' + $if windows { '.exe' } $else { '' })
		result := os.exec([@VEXE, '-cc', 'cc', '-building-v', '-o', output, source])
		assert result.exit_code == 0, result.output
		run := os.exec([output])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == '${payload.len}:${payload.len}'
		objects := os.walk_ext(cache, '.o')
		assert objects.any(os.file_name(it).starts_with('builtin_')), objects.str()
	}
}
