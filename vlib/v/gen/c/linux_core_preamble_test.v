module c

import os
import v.cmdexec

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
