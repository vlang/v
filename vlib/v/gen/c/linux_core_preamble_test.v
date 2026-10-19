module c

import os
import v.cmdexec
import v.flat
import v.types

fn test_linux_open_flags_match_native_libc() {
	$if !linux || freestanding {
		return
	}
	mut g := FlatGen.new()
	g.headerless_linux_constants()
	constants := ['O_DIRECTORY', 'O_NOFOLLOW', 'O_RDONLY', 'O_WRONLY', 'O_RDWR', 'O_CREAT', 'O_EXCL',
		'O_NOCTTY', 'O_TRUNC', 'O_APPEND', 'O_NONBLOCK', 'O_SYNC', 'O_CLOEXEC', 'AT_FDCWD',
		'AT_REMOVEDIR', 'F_GETFD', 'F_SETFD', 'F_GETFL', 'F_SETFL', 'F_SETLK', 'F_SETLKW', 'FD_CLOEXEC',
		'MADV_DONTNEED']
	mut code := '#define _GNU_SOURCE 1\n#include <fcntl.h>\n#include <sys/mman.h>\n'
	for name in constants {
		code += 'enum { SDK_${name} = ${name} };\n'
	}
	for name in constants {
		code += '#undef ${name}\n'
	}
	for line in g.sb.str().split_into_lines() {
		if line.starts_with('#define ') {
			name := line.all_after('#define ').all_before(' ')
			if name in constants {
				code += '${line}\n'
			}
		} else if line.starts_with('#') {
			code += '${line}\n'
		}
	}
	for name in constants {
		code += '_Static_assert(${name} == SDK_${name}, "${name} value");\n'
	}
	dir := os.join_path(os.vtmp_dir(), 'linux_open_flags_sdk_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'sdk.c')
	os.write_file(source, code)!
	result := cmdexec.run('cc', ['-std=c11', '-Werror', '-fsyntax-only', source])
	assert result.exit_code == 0, result.output
}

fn test_linux_rusage_storage_matches_native_libc() {
	$if !linux || freestanding {
		return
	}
	mut g := FlatGen.new()
	g.headerless_rusage_struct()
	mut defines := ''
	$if musl ? {
		defines = '#define CUSTOM_DEFINE_musl 1\n'
	}
	fields := ['ru_utime', 'ru_stime', 'ru_maxrss', 'ru_ixrss', 'ru_idrss', 'ru_isrss', 'ru_minflt',
		'ru_majflt', 'ru_nswap', 'ru_inblock', 'ru_oublock', 'ru_msgsnd', 'ru_msgrcv', 'ru_nsignals',
		'ru_nvcsw', 'ru_nivcsw']
	mut code := defines + '#define rusage SDK_rusage\n#include <sys/resource.h>\n#include <stddef.h>\n#undef rusage\n' + g.sb.str()
	code += '_Static_assert(sizeof(struct rusage) == sizeof(struct SDK_rusage), "rusage size");\n'
	code += '_Static_assert(_Alignof(struct rusage) == _Alignof(struct SDK_rusage), "rusage alignment");\n'
	for field in fields {
		code += '_Static_assert(offsetof(struct rusage, ${field}) == offsetof(struct SDK_rusage, ${field}), "${field} offset");\n'
		code += '_Static_assert(sizeof(((struct rusage*)0)->${field}) == sizeof(((struct SDK_rusage*)0)->${field}), "${field} width");\n'
	}
	dir := os.join_path(os.vtmp_dir(), 'linux_rusage_sdk_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'sdk.c')
	os.write_file(source, code)!
	result := cmdexec.run('cc', ['-std=c11', '-Werror', '-fsyntax-only', source])
	assert result.exit_code == 0, result.output
}

fn test_linux_headerless_rwmutex_uses_native_libc_support() {
	$if !linux || freestanding {
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'linux_rwmutex_libc_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'main.v')
	os.write_file(source, 'import sync
fn main() {
	mut mutex := sync.new_rwmutex()
	mutex.rlock()
	mutex.runlock()
	mutex.lock()
	mutex.unlock()
	mutex.destroy()
	println("OK")
}
')!
	result := cmdexec.run(@VEXE, ['-nocache', '-cc', 'cc', '-no-retry-compilation', 'run', source])
	assert result.exit_code == 0, result.output
	assert result.output == 'OK\n', result.output
}

// rwlock_kind_decls returns the generated `#if` block that decides how
// pthread_rwlockattr_setkind_np is called.
fn rwlock_kind_decls(mut g FlatGen) string {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	g.a = &ast
	g.tc = &tc
	g.has_builtins = true
	g.builtin_abi_decls()
	lines := g.sb.str().split_into_lines()
	start := lines.index(lines.filter(it.starts_with('#if !defined(PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP)'))[0])
	mut end := start
	for end < lines.len && lines[end] != '#endif' {
		end++
	}
	return lines[start..end + 1].join('\n')
}

// Headerless C that does not know its Linux libc (vc/v.c, `-o file.c`) defines
// PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP itself. musl has no
// pthread_rwlockattr_setkind_np, so a plain call failed to link there.
fn test_unknown_linux_libc_binds_rwlock_kind_weakly() {
	mut g := FlatGen.new()
	decls := rwlock_kind_decls(mut g)
	assert decls.contains('#elif defined(__linux__) && !defined(__ANDROID__) && !defined(CUSTOM_DEFINE_glibc)'), decls
	assert decls.contains('int pthread_rwlockattr_setkind_np(void* attr, int kind) __attribute__((weak));'), decls
	$if !linux || freestanding {
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'linux_rwlock_kind_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'main.c')
	exe := os.join_path(dir, 'main')
	// Like the headerless output, declare the pthread API without <pthread.h>,
	// whose glibc version has its own prototypes for both functions.
	os.write_file(source, '#include <stdio.h>
typedef union { unsigned char _opaque[64]; long long _align; } pthread_rwlockattr_t;
int pthread_rwlockattr_init(void* attr);
int pthread_rwlockattr_destroy(void* attr);
#define PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP 2
${decls}
int pthread_rwlockattr_getkind_np(const void* attr, int* kind) __attribute__((weak));
int main(void) {
	pthread_rwlockattr_t attr;
	if (pthread_rwlockattr_init(&attr) != 0) return 1;
	if (pthread_rwlockattr_setkind_np(&attr, PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP) != 0) return 2;
	int (*getkind)(const void*, int*) = pthread_rwlockattr_getkind_np;
	int kind = -1;
	if (getkind == 0) {
		puts("no kind");
	} else if (getkind(&attr, &kind) == 0 && kind == PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP) {
		puts("writer");
	}
	return pthread_rwlockattr_destroy(&attr);
}
')!
	compiled := cmdexec.run('cc', ['-std=c99', '-o', exe, source, '-lpthread'])
	assert compiled.exit_code == 0, compiled.output
	run := cmdexec.run(exe, [])
	assert run.exit_code == 0, run.output
	$if musl ? {
		assert run.output == 'no kind\n', run.output
	} $else {
		assert run.output == 'writer\n', run.output
	}
}

fn test_linux_rwmutex_c_output_links_without_a_libc_define() {
	$if !linux || freestanding {
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'linux_rwmutex_c_output_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'main.v')
	c_file := os.join_path(dir, 'main.c')
	exe := os.join_path(dir, 'main')
	os.write_file(source, 'import sync
fn main() {
	mut mutex := sync.new_rwmutex()
	mutex.rlock()
	mutex.runlock()
	mutex.lock()
	mutex.unlock()
	mutex.destroy()
	println("OK")
}
')!
	// `-o file.c` leaves the libc unknown. The segfault handler lives in a separate
	// object that a plain `cc main.c` does not link.
	generated := cmdexec.run(@VEXE, ['-nocache', '-gc', 'none', '-d', 'no_segfault_handler', '-o',
		c_file, source])
	assert generated.exit_code == 0, generated.output
	code := os.read_file(c_file)!
	assert !code.contains('#include'), 'expected headerless C'
	assert !code.contains('#define CUSTOM_DEFINE_glibc') && !code.contains('#define CUSTOM_DEFINE_musl')
	compiled := cmdexec.run('cc', ['-std=c99', '-w', '-o', exe, c_file, '-lm', '-lpthread'])
	assert compiled.exit_code == 0, compiled.output
	run := cmdexec.run(exe, [])
	assert run.exit_code == 0, run.output
	assert run.output == 'OK\n', run.output
}
