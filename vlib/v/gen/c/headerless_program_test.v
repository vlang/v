module c

import os
import v.cmdexec

fn test_brotli_binding_retains_its_native_headers() {
	$if !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'brotli_native_headers_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	c_file := os.join_path(root, 'main.c')
	os.write_file(source, 'import compress.brotli
fn main() {
	encoded := brotli.compress("hello".bytes(), brotli.CompressParams{}) or { panic(err) }
	decoded := brotli.decompress(encoded, brotli.DecompressParams{}) or { panic(err) }
	assert decoded.bytestr() == "hello"
}
')!
	generated := cmdexec.run(@VEXE, ['-nocache', '-o', c_file, source])
	assert generated.exit_code == 0, generated.output
	code := os.read_file(c_file)!
	assert code.contains('#include <dlfcn.h>'), 'Brotli must retain its native loader header'
}

fn test_headerless_test_statistics_without_importing_time() {
	$if windows || freestanding || vinix {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'headerless_test_statistics_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'statistics_test.v')
	os.write_file(source, 'fn test_statistics() { assert 6 * 7 == 42 }\n')!
	run := cmdexec.run(@VEXE, ['-nocache', '-cc', 'clang', '-stats', source])
	assert run.exit_code == 0, run.output
	assert run.output.contains('1 passed'), run.output
}

fn test_posix_hello_world_c_has_no_includes() {
	$if windows || freestanding || vinix {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'headerless_hello_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'hello_world.v')
	c_file := os.join_path(root, 'hello_world.c')
	os.write_file(source, "println('Hello, World!')\n")!
	for gc in ['none', 'boehm', 'boehm_leak'] {
		generated := cmdexec.run(@VEXE, ['-nocache', '-gc', gc, '-o', c_file, source])
		assert generated.exit_code == 0, generated.output
		code := os.read_file(c_file)!
		assert !code.contains('#include'), gc
		assert code.len < 256, '${gc}: ${code.len} bytes'
		assert code.contains('int puts(const char*'), gc
		assert !code.contains('malloc') && !code.contains('GC_') && !code.contains('gc_'), gc
		run := cmdexec.run(@VEXE, ['-nocache', '-no-retry-compilation', '-gc', gc, 'run', source])
		assert run.exit_code == 0, run.output
		assert run.output == 'Hello, World!\n', run.output
	}
	for target in ['linux', 'macos', 'freebsd', 'openbsd', 'netbsd', 'dragonfly', 'solaris', 'haiku',
		'qnx', 'serenity', 'termux', 'wasm32_emscripten', 'windows'] {
		generated := cmdexec.run(@VEXE, ['-nocache', '-os', target, '-o', c_file, source])
		assert generated.exit_code == 0, generated.output
		assert !os.read_file(c_file)!.contains('#include'), target
	}
}

fn test_bodyless_libc_bindings_compile_without_headers() {
	$if windows || freestanding || vinix {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'headerless_libc_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, 'fn C.strlen(const_s &char) uint
fn C.puts(const_s &char) i32

fn main() {
	assert C.strlen(c"hello") == 5
	mut values := [3, 1, 2]
	values.sort()
	assert values == [1, 2, 3]
	C.puts(c"OK")
}
')!
	run := cmdexec.run(@VEXE, ['-nocache', '-gc', 'none', 'run', source])
	assert run.exit_code == 0, run.output
	assert run.output == 'OK\n', run.output
}

fn test_bsd_executable_uses_native_sysctl_ownership() {
	$if windows || freestanding || vinix {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'bsd_executable_headers_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.c.v')
	c_file := os.join_path(root, 'main.c')
	program := 'import os\nfn main() {\n executable := os.executable()\n assert os.is_file(executable)\n assert os.real_path(executable) == os.real_path(os.args[0])\n}\n'
	for with_explicit_header in [false, true] {
		os.write_file(source, if with_explicit_header {
			'#include <time.h>\n' + program
		} else {
			program
		})!
		for target in ['freebsd', 'openbsd'] {
			generated := cmdexec.run(@VEXE, ['-nocache', '-gc', 'none', '-os', target, '-o', c_file,
				source])
			assert generated.exit_code == 0, generated.output
			code := os.read_file(c_file)!
			assert code.contains('sysctl('), target
			if with_explicit_header {
				assert code.contains('#include <sys/sysctl.h>'), '${target}: native headers own sysctl declarations'
			} else {
				assert !code.contains('#include <sys/sysctl.h>'), '${target}: standalone builds provide the core ABI'
				assert code.contains('i32 sysctl('), '${target}: standalone builds need the sysctl prototype'
				assert code.contains('#define CTL_KERN 1'), target
				assert code.contains('#define KERN_PROC_PATHNAME 12'), target
				assert code.contains('#define KERN_PROC_ARGS 55'), target
			}
		}
		$if freebsd || openbsd {
			mut compilers := ['cc']
			$if freebsd {
				compilers << 'tcc'
			}
			for cc in compilers {
				run := cmdexec.run(@VEXE, ['-nocache', '-gc', 'none', '-cc', cc, '-no-retry-compilation',
					'run', source])
				assert run.exit_code == 0, run.output
			}
		}
	}
}

fn test_headerless_boehm_registers_spawned_threads() {
	$if windows || freestanding || vinix {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'headerless_gc_threads_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn churn() {
	for i in 0 .. 8 {
		values := []int{len: 1024, init: i}
		gc_collect()
		assert values[100] == i
	}
}

fn main() {
	mut threads := []thread{}
	for _ in 0 .. 4 {
		threads << spawn churn()
	}
	threads.wait()
	println("OK")
}
')!
	run := cmdexec.run(@VEXE, ['-nocache', '-gc', 'boehm', 'run', source])
	assert run.exit_code == 0, run.output
	assert run.output == 'OK\n', run.output
}

fn test_headerless_runtime_prototypes_survive_warm_module_cache() {
	$if windows || freestanding || vinix {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'headerless_cached_runtime_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	// Different bodies reuse builtin interfaces while rebuilding the program prefix.
	for message in ['first', 'second'] {
		os.write_file(source, 'fn main() { println("${message}") }\nfn cleanup() { println("cleanup") }\n')!
		run := cmdexec.run(@VEXE, ['-cc', 'clang', '-gc', 'none', '-no-retry-compilation', 'run',
			source])
		assert run.exit_code == 0, run.output
		assert run.output == '${message}\ncleanup\n', run.output
	}
}

fn test_boehm_dlopen_uses_platform_gc_redirects() {
	$if windows || freestanding || vinix {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'headerless_gc_dlopen_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, '#include <dlfcn.h>
fn C.dlopen(filename &char, flags i32) voidptr
fn C.dlsym(handle voidptr, symbol &char) voidptr
fn C.dlclose(handle voidptr) i32
fn main() {
 handle := C.dlopen(unsafe { nil }, 2)
 assert handle != unsafe { nil }
 assert C.dlsym(handle, c"printf") != unsafe { nil }
 assert C.dlclose(handle) == 0
 println("OK")
}
')!
	run := cmdexec.run(@VEXE, ['-nocache', '-gc', 'boehm', '-no-retry-compilation', 'run', source])
	assert run.exit_code == 0, run.output
	assert run.output == 'OK\n', run.output
}

fn test_self_c_has_no_include_directives() {
	$if !(macos || linux || windows || freebsd || openbsd || dragonfly || android || termux || ios) || freestanding {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'headerless_self_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	c_file := os.join_path(root, 'v.c')
	for mode in [['-gc', 'none', '-prealloc', '-compile-backend', 'fastc'], ['-cross'],
		['-os', 'cross']] {
		mut args := ['-nocache']
		args << mode
		args << ['-o', c_file, os.join_path(@VEXEROOT, 'cmd', 'v')]
		generated := cmdexec.run(@VEXE, args)
		assert generated.exit_code == 0, generated.output
		code := os.read_file(c_file)!
		$if !windows {
			assert code.split_into_lines().any(it.starts_with('DIR* fdopendir(')), 'the CLI needs the pointer-returning POSIX fdopendir binding'
			assert code.split_into_lines().any(it.starts_with('DIR* opendir(')), 'the CLI needs the pointer-returning POSIX opendir binding'
		}
		mut headerless := code
		if '-cross' in mode || 'cross' in mode {
			// Portable snapshots also carry a header-based variant for 32-bit hosts.
			headerless = libc_variant_source(code, true)
			headers := libc_variant_source(code, false)
			assert headerless.contains('#define CUSTOM_DEFINE_v3_headerless_bootstrap'), mode.join(' ')
			assert !headers.contains('#define CUSTOM_DEFINE_v3_headerless_bootstrap'), mode.join(' ')
			assert headers.contains('\n#include <stdint.h>\n'), mode.join(' ')
		}
		for line in headerless.split_into_lines() {
			// Compiler string literals still contain text used to generate other C files.
			assert !line.trim_space().starts_with('#include'), line
		}
	}
}

// vc/v.c is generated on Linux (gen_vc_ci.yml), and every non-Windows `make` compiles
// it. On 64-bit targets it includes no headers, so clang can check it for each target
// without a sysroot. 32-bit targets use its header-based variant, which needs the
// target's headers; forcing V's own declarations there still checks that every use
// of `int` agrees with the 32-bit `__v_int`.
fn test_linux_generated_self_c_compiles_for_bootstrap_targets() {
	$if !(macos || linux) || freestanding {
		return
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'headerless_self_targets_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	c_file := os.join_path(root, 'v.c')
	generated := cmdexec.run(@VEXE, ['-nocache', '-os', 'linux', '-cross', '-o', c_file,
		os.join_path(@VEXEROOT, 'cmd', 'v')])
	assert generated.exit_code == 0, generated.output
	for target in ['x86_64-unknown-linux-gnu', 'aarch64-unknown-linux-gnu', 'aarch64-linux-android24',
		'arm64-apple-macos13', 'x86_64-apple-macos12', 'x86_64-unknown-freebsd14',
		'x86_64-unknown-openbsd7', 'x86_64-unknown-netbsd10', 'x86_64-unknown-dragonfly6'] {
		checked := os.exec([clang, '-target', target, '-std=c99', '-fsyntax-only', '-Wno-everything',
			'-Werror=implicit-function-declaration', '-DCUSTOM_DEFINE_v1_fallback', c_file])
		assert checked.exit_code == 0, '${target}: ${checked.output}'
	}
	for target in ['armv7-unknown-linux-gnueabihf', 'i686-unknown-linux-gnu', 'x86_64-unknown-linux-gnu'] {
		checked := os.exec([clang, '-target', target, '-std=c99', '-fsyntax-only', '-Wno-everything',
			'-Werror=implicit-function-declaration', '-Werror=incompatible-pointer-types',
			'-Wno-error=incompatible-pointer-types-discards-qualifiers', '-Werror=int-conversion',
			'-D${headerless_libc_macro}=1', '-DCUSTOM_DEFINE_v1_fallback', c_file])
		assert checked.exit_code == 0, '${target}: ${checked.output}'
	}
}

fn test_compiler_source_checks_for_32_bit_hosts() {
	// A 32-bit host builds cmd/v with every backend, so an `int` literal that only
	// fits 64 bits stops its `make` at v1 -> v2.
	for os_name, arch in {
		'linux':   'arm32'
		'freebsd': 'x86'
	} {
		checked := cmdexec.run(@VEXE, ['-nocache', '-os', os_name, '-arch', arch, '-check',
			os.join_path(@VEXEROOT, 'cmd', 'v')])
		assert checked.exit_code == 0, '${os_name}/${arch}: ${checked.output}'
	}
}

fn test_headerless_standard_modules_and_native_helpers() {
	$if !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'headerless_modules_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import os\nimport time\nimport math\nimport runtime\nimport sync\nfn main() {\n assert os.exec(["printf", "OK"]).output == "OK"\n assert time.now().year > 1970\n assert math.sqrt(9) == 3\n assert runtime.free_memory()! > 0\n assert runtime.used_memory()! > 0\n mut wg := sync.new_waitgroup()\n wg.go(fn () {})\n wg.wait()\n println("OK")\n}\n')!
	c_file := os.join_path(root, 'main.c')
	generated := cmdexec.run(@VEXE, ['-nocache', '-o', c_file, source])
	assert generated.exit_code == 0, generated.output
	assert !os.read_file(c_file)!.contains('#include')
	for cc in ['tcc', 'clang'] {
		run := cmdexec.run(@VEXE, ['-nocache', '-cc', cc, '-thread-stack-size', '65536',
			'-no-retry-compilation', 'run', source])
		assert run.exit_code == 0, run.output
		assert run.output == 'OK\n', run.output
	}
}

fn test_headerless_cached_os_objects_link_closure_cleanup() {
	$if !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'headerless_cached_os_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	for message in ['first', 'second'] {
		os.write_file(source, 'import os\nfn main() { assert os.getpid() > 0; println("${message}") }\n')!
		run := cmdexec.run(@VEXE, ['-cc', 'clang', '-gc', 'boehm', '-no-retry-compilation', 'run',
			source])
		assert run.exit_code == 0, run.output
		assert run.output == '${message}\n', run.output
	}
}

fn test_runtime_bindings_survive_unrelated_system_headers() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'runtime_headers_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, '#include <time.h>\nimport os\nimport os.filelock\nimport v.bench\nimport v.gen.fastc\nfn main() {\n _ = fastc.fastc_compile_c_units\n mut b := bench.new()\n b.disable_memory_limit()\n b.start_memory_monitor()\n b.stop_memory_monitor()\n mut file_lock := filelock.new(os.join_path(os.temp_dir(), "runtime_headers_${os.getpid()}"))\n assert file_lock.try_acquire()\n assert file_lock.release()\n println("OK")\n}\n')!
	run := cmdexec.run(@VEXE, ['-nocache', '-gc', 'none', '-cc', 'clang', '-no-retry-compilation',
		'run', source])
	assert run.exit_code == 0, run.output
	assert run.output == 'OK\n', run.output
}

fn test_cached_member_name_does_not_register_a_program_cleanup_hook() {
	$if windows || freestanding || vinix {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'cached_cleanup_member_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	for gc in ['none', 'boehm'] {
		for message in ['first', 'second', 'second'] {
			os.write_file(source, '@[translated]\nmodule main\nstruct Holder { _vcleanup int }\nfn main() { value := Holder{_vcleanup: 42}; println(value._vcleanup); println("${message}") }\n')!
			run := cmdexec.run(@VEXE, ['-cc', 'clang', '-gc', gc, '-no-retry-compilation', 'run',
				source])
			assert run.exit_code == 0, run.output
			assert run.output == '42\n${message}\n', run.output
		}
	}
}
