module c

import os
import v.cmdexec
import v.pref

fn test_macos_headerless_pthread_and_jump_buffer_match_sdk_abi() {
	$if !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'headerless_native_abi_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut g := FlatGen.new()
	g.set_target(pref.target_from('macos', $if arm64 { 'arm64' } $else { 'amd64' })!)
	g.headerless_libc_preamble()
	// Isolate the emitted storage declarations so the oracle needs no V runtime.
	preamble := g.sb.str()
	start := preamble.index('typedef int pid_t;')?
	end := preamble.index('#ifndef va_start')?
	storage := preamble[start..end]
	mut jump := FlatGen.new()
	jump.set_target(g.target)
	jump.jump_buffer_declarations()
	mut net_constants := FlatGen.new()
	net_constants.headerless_darwin_net_constants()
	mut posix_records := FlatGen.new()
	posix_records.headerless_statvfs_struct()
	posix_records.headerless_timeval_struct()
	headerless := storage + jump.sb.str() + net_constants.sb.str() +
		'typedef unsigned int u32;\ntypedef int i32;\n' + posix_records.sb.str()
	assert !headerless.contains('#include')
	mut measurements := 'int printf(const char*, ...);\nint main(void) {\n'
	for name in ['pthread_t', 'pthread_attr_t', 'pthread_mutex_t', 'pthread_cond_t', 'pthread_rwlock_t',
		'pthread_rwlockattr_t', 'pthread_condattr_t', 'pthread_once_t', 'pthread_key_t', 'sem_t',
		'sigset_t', 'jmp_buf', 'struct statvfs', 'struct timeval'] {
		measurements += 'printf("${name} %lu %lu\\n", (unsigned long)sizeof(${name}), (unsigned long)_Alignof(${name}));\n'
	}
	for name, fields in {
		'statvfs': ['f_bsize', 'f_frsize', 'f_blocks', 'f_bfree', 'f_bavail', 'f_files', 'f_ffree',
			'f_favail', 'f_fsid', 'f_flag', 'f_namemax']
		'timeval': ['tv_sec', 'tv_usec']
	} {
		for field in fields {
			measurements += 'printf("${name}.${field} %lu %lu\\n", (unsigned long)__builtin_offsetof(struct ${name}, ${field}), (unsigned long)sizeof(((struct ${name}*)0)->${field}));\n'
		}
	}
	// Once.count follows an embedded RwMutex; a size mismatch changes its offset.
	measurements += 'struct rwmutex { pthread_rwlock_t mutex; unsigned int inited; };\n'
	measurements += 'struct once { struct rwmutex m; unsigned long long count; };\n'
	measurements += 'printf("once.count %lu\\n", (unsigned long)__builtin_offsetof(struct once, count));\n'
	measurements += 'printf("shutdown %d %d %d\\n", SHUT_RD, SHUT_WR, SHUT_RDWR);\n'
	measurements += 'printf("nosignal %d\\n", MSG_NOSIGNAL);\nreturn 0;\n}\n'
	mut results := []string{}
	for role, declarations in [headerless,
		'#include <stdio.h>\n#include <pthread.h>\n#include <semaphore.h>\n#include <signal.h>\n#include <setjmp.h>\n#include <sys/socket.h>\n#include <sys/statvfs.h>\n#include <sys/time.h>\n'] {
		source := os.join_path(root, 'oracle_${role}.c')
		binary := os.join_path(root, 'oracle_${role}')
		os.write_file(source, declarations + measurements)!
		compiled := cmdexec.run('cc', ['-std=c11', '-o', binary, source])
		assert compiled.exit_code == 0, compiled.output
		run := cmdexec.run(binary, [])
		assert run.exit_code == 0, run.output
		results << run.output
	}
	assert results[0] == results[1], 'headerless:\n${results[0]}SDK:\n${results[1]}'
}

fn test_macos_recover_c_has_no_includes_and_runs() {
	$if !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'headerless_recover_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() {\n defer { if message := recover() { println(message) } }\n panic("recovered")\n}\n')!
	c_file := os.join_path(root, 'main.c')
	generated := cmdexec.run(@VEXE, ['-nocache', '-o', c_file, source])
	assert generated.exit_code == 0, generated.output
	assert !os.read_file(c_file)!.contains('#include')
	for flags in [['-cc', 'tcc'], ['-cc', 'clang'], ['-cc', 'clang', '-prod']] {
		mut args := ['-nocache', '-no-retry-compilation']
		args << flags
		args << ['run', source]
		run := cmdexec.run(@VEXE, args)
		assert run.exit_code == 0, run.output
		assert run.output == 'recovered\n', run.output
	}
}

fn test_macos_cached_test_recovery_declares_jump_buffer_before_frames() {
	$if !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'headerless_test_recover_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'recover_test.v')
	c_file := os.join_path(root, 'recover_test.c')
	for message in ['first', 'second'] {
		os.write_file(source, 'fn test_recover() {\n defer { if message := recover() { assert message == "${message}" } }\n panic("${message}")\n}\n')!
		generated := cmdexec.run(@VEXE, ['-cc', 'clang', '-o', c_file, source])
		assert generated.exit_code == 0, generated.output
		code := os.read_file(c_file)!
		assert !code.contains('#include')
		// Recovery frames precede the test runner's own jump-buffer declaration.
		assert code.index('typedef int jmp_buf')? < code.index('typedef struct v_unwind_frame')?
		run := cmdexec.run(@VEXE, ['-cc', 'clang', '-silent', '-no-retry-compilation', source])
		assert run.exit_code == 0, run.output
	}
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
