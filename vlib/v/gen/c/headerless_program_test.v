module c

import os
import v.cmdexec

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
		assert code.contains('void* malloc(size_t'), gc
		run := cmdexec.run(@VEXE, ['-nocache', '-no-retry-compilation', '-gc', gc, 'run', source])
		assert run.exit_code == 0, run.output
		assert run.output == 'Hello, World!\n', run.output
	}
	for target in ['linux', 'macos', 'freebsd'] {
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
	os.write_file(source, 'fn C.strlen(const_s &char) usize
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

fn test_macos_self_c_has_no_include_directives() {
	$if !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'headerless_self_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	c_file := os.join_path(root, 'v.c')
	generated := cmdexec.run(@VEXE, ['-nocache', '-gc', 'none', '-prealloc', '-compile-backend',
		'fastc', '-o', c_file, os.join_path(@VEXEROOT, 'cmd', 'v')])
	assert generated.exit_code == 0, generated.output
	code := os.read_file(c_file)!
	assert code.contains('DIR* fdopendir('), 'the CLI needs the pointer-returning POSIX binding'
	for line in code.split_into_lines() {
		// Compiler string literals still contain text used to generate other C files.
		assert !line.trim_space().starts_with('#include'), line
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
