import os
import v.cmdexec

// Modules shipped with V that compile a C source or the implementation section of a
// single-header library into the program have no owner object in the V caches. Their
// cold and warm `-cc cc` builds must not give every cached module object a copy of
// those definitions (duplicate symbols) or drop them (undeclared functions).

const native_cache_vexe = os.join_path(@VMODROOT, 'v' + $if windows { '.exe' } $else { '' })

fn build_native_cache_program_twice(name string, source string, expected_output string) {
	os.find_abs_path_of_executable('cc') or {
		eprintln('skipping native module cache test: cc is unavailable')
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_native_module_cache_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	source_path := os.join_path(root, '${name}.v')
	os.write_file(source_path, source) or { panic(err) }
	saved := os.environ()
	env_names := ['V3CACHE', 'V3_CACHE_TRACE', 'V3_CACHE_FORCE_SOURCE', 'VFLAGS', 'CFLAGS', 'LDFLAGS']
	defer {
		for env_name in env_names {
			if env_name in saved {
				os.setenv(env_name, saved[env_name], true)
			} else {
				os.unsetenv(env_name)
			}
		}
	}
	os.setenv('V3CACHE', os.join_path(root, 'cache'), true)
	os.setenv('V3_CACHE_TRACE', '1', true)
	for env_name in env_names[2..] {
		os.unsetenv(env_name)
	}
	for attempt in ['cold', 'warm'] {
		output := os.join_path(root, '${name}_${attempt}')
		build := cmdexec.run_with_timeout(native_cache_vexe, ['-new-compiler', '-no-retry-compilation',
			'-cc', 'cc', '-o', output, source_path], 600_000)
		assert build.exit_code == 0, '${attempt} ${name} build:\n${build.output}'
		if expected_output.len == 0 {
			continue
		}
		run := cmdexec.run_with_timeout(output, [], 30_000)
		assert run.exit_code == 0, '${attempt} ${name} run:\n${run.output}'
		assert run.output.trim_space() == expected_output, '${attempt} ${name} run:\n${run.output}'
	}
}

fn test_szip_program_builds_cold_and_warm_with_the_module_cache() {
	build_native_cache_program_twice('szip', "import compress.szip
import os

fn main() {
	path := os.join_path(os.dir(os.executable()), 'cache_test.zip')
	mut zip := szip.open(path, .best_speed, .write) or { panic(err) }
	zip.open_entry('entry.txt') or { panic(err) }
	zip.write_entry('zip ok'.bytes()) or { panic(err) }
	zip.close_entry()
	zip.close()
	println(os.file_size(path) > 0)
}
",
		'true')
}

fn test_zstd_program_builds_cold_and_warm_with_the_module_cache() {
	build_native_cache_program_twice('zstd', "import compress.zstd

fn main() {
	packed := zstd.compress('zstd ok'.bytes()) or { panic(err) }
	println(zstd.decompress(packed) or { panic(err) }.bytestr())
}
",
		'zstd ok')
}

fn test_gg_program_builds_cold_and_warm_with_the_module_cache() {
	// Linking gg needs the platform graphics libraries but no display; the program
	// itself is not run.
	$if linux {
		if !os.exists('/usr/include/X11/Xlib.h') || !os.exists('/usr/include/GL/gl.h') {
			eprintln('skipping gg module cache test: X11 or OpenGL headers are unavailable')
			return
		}
	} $else $if !macos {
		return
	}
	build_native_cache_program_twice('gg', "import gg

fn main() {
	mut context := gg.new_context(width: 100, height: 100, window_title: 'native module cache')
	context.run()
}
",
		'')
}
