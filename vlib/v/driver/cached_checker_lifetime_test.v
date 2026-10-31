module driver

import os

fn test_cached_build_discovers_generics_and_retains_checker_metadata_in_parallel() {
	// Embedded text and bytes exercise generic calls in cached builtin bodies.
	// Cold/warm builds with default and four workers also cover metadata ownership
	// through cache planning, after scoped transformation and C generation finish.
	// Module objects are cached for the default system C compiler.
	cc := 'cc'
	if _ := os.find_abs_path_of_executable(cc) {
	} else {
		eprintln('skipping cached checker lifetime test: ${cc} is unavailable')
		return
	}
	probe := os.exec([cc, '--version'])
	if probe.exit_code != 0 {
		eprintln('skipping cached checker lifetime test: ${cc} cannot run')
		return
	}
	variables := ['VFLAGS', 'VOSARGS', 'VJOBS', 'V3CACHE', 'V3_CACHE_FORCE_SOURCE']
	mut previous := map[string]string{}
	for name in variables {
		if value := os.getenv_opt(name) {
			previous[name] = value
		}
		os.unsetenv(name)
	}
	defer {
		for name in variables {
			if value := previous[name] {
				os.setenv(name, value, true)
			} else {
				os.unsetenv(name)
			}
		}
	}
	root := os.join_path(os.vtmp_dir(), 'cached_checker_lifetime_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	payload := 'compiler bytes\n'.repeat(200)
	os.write_file(os.join_path(root, 'data.txt'), payload)!
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() {
	file := \$embed_file("data.txt")
	text := file.to_string()
	bytes := file.to_bytes()
	println("\${text.len}:\${bytes.len}")
}')!
	for jobs in ['', '4'] {
		if jobs.len == 0 {
			os.unsetenv('VJOBS')
		} else {
			os.setenv('VJOBS', jobs, true)
		}
		cache := os.join_path(root, 'cache_' + if jobs.len == 0 { 'default' } else { jobs })
		os.setenv('V3CACHE', cache, true)
		for index in 0 .. 2 {
			output := os.join_path(root, 'probe' + $if windows { '.exe' } $else { '' })
			build := os.exec([@VEXE, '-cc', cc, '-building-v', '-o', output, source])
			assert build.exit_code == 0, 'VJOBS=${jobs}, build ${index}: ${build.output}'
			run := os.exec([output])
			assert run.exit_code == 0, run.output
			assert run.output.trim_space() == '${payload.len}:${payload.len}'
			objects := os.walk_ext(cache, '.o')
			assert objects.any(os.file_name(it).starts_with('builtin_')), objects.str()
		}
	}
}
