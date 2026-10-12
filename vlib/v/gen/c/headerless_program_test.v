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
