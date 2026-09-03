module fastc

import os

// The header-free prelude is checked against the host's own headers: every
// struct layout, type size, macro value and function prototype the table
// declares must agree with what the system headers say, on the machine
// running the tests (macOS and glibc Linux tables).
fn test_c_abi_prelude_matches_host_headers() {
	host_os := os.user_os()
	host_arch := $if arm64 { 'arm64' } $else $if amd64 { 'amd64' } $else { '' }
	if !fastc_c_abi_supported(host_os, host_arch, fastc_host_uses_glibc()) {
		return
	}
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(tcc) {
		return
	}
	prelude := fastc_c_abi_prelude(host_os, host_arch, 'v_abi_')
	assert !prelude.contains('#include')
	source := fastc_c_abi_check_source(host_os, prelude)
	test_dir := os.join_path(os.temp_dir(), 'fastc_c_abi_${os.getpid()}')
	os.mkdir_all(test_dir) or { panic(err) }
	defer {
		os.rmdir_all(test_dir) or {}
	}
	source_path := os.join_path_single(test_dir, 'abi_check.c')
	exe_path := os.join_path_single(test_dir, 'abi_check')
	os.write_file(source_path, source) or { panic(err) }
	tcc_lib := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'lib')
	mut args := ['-std=gnu11', '-B${tcc_lib}', '-I${os.join_path_single(tcc_lib, 'include')}',
		'-L${tcc_lib}', '-I/usr/local/include', '-L/usr/local/lib']
	if host_os == 'macos' {
		mut sdk_root := os.getenv('SDKROOT')
		if !os.is_dir(sdk_root) {
			result := os.execute('xcrun --show-sdk-path')
			if result.exit_code == 0 {
				sdk_root = result.output.trim_space()
			}
		}
		if os.is_dir(sdk_root) {
			args << '-I${os.join_path(sdk_root, 'usr', 'include')}'
			args << '-L${os.join_path(sdk_root, 'usr', 'lib')}'
		}
	}
	args << ['-w', '-o', exe_path, source_path, '-lpthread', '-lm']
	compile := os.execute('${tcc} ${args.join(' ')}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(exe_path)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}

fn test_c_abi_prelude_requires_glibc_on_linux() {
	for arch in ['amd64', 'arm64'] {
		assert fastc_c_abi_supported('linux', arch, true)
		assert !fastc_c_abi_supported('linux', arch, false)
	}
	assert fastc_c_abi_supported('macos', 'amd64', false)
	assert !fastc_c_abi_supported('linux', 'x86', true)
}

// fastc_c_abi_check_source renders the C program that compares the prefixed
// prelude with the real headers: static assertions for layouts, values and
// prototypes, and runtime checks for the initializers and fd_set macros.
fn fastc_c_abi_check_source(host_os string, prelude string) string {
	macos := host_os == 'macos'
	mut out := []string{}
	out << '#include <stdint.h>'
	out << '#include <stdio.h>'
	out << '#include <stdlib.h>'
	out << '#include <string.h>'
	out << '#include <strings.h>'
	out << '#include <stddef.h>'
	out << '#include <pthread.h>'
	out << '#include <sys/stat.h>'
	out << '#include <sys/types.h>'
	out << '#include <sys/select.h>'
	out << '#include <sys/ioctl.h>'
	out << '#include <sys/mman.h>'
	out << '#include <sys/wait.h>'
	out << '#include <errno.h>'
	out << '#include <dirent.h>'
	if macos {
		out << '#include <CommonCrypto/CommonDigest.h>'
	}
	out << '#include <unistd.h>'
	out << '#include <fcntl.h>'
	out << '#include <time.h>'
	if macos {
		out << '#include <mach/mach_time.h>'
		out << '#include <mach-o/dyld.h>'
	}
	// Some libcs spell these as macros; the prototypes are compared as functions.
	for name in ['feof', 'ferror', 'fileno', 'getchar', 'fgetc', 'fputc', 'stat', 'lstat', 'fstat'] {
		out << '#undef ${name}'
	}
	// unistd.h does not declare environ on macOS; the prelude declares its own.
	out << 'extern char **environ;'
	out << prelude
	out << '#define CHECK_EQ(a, b) _Static_assert((a) == (b), #a " == " #b)'
	out << '#define CHECK_SIZE(t) _Static_assert(sizeof(v_abi_##t) == sizeof(t), "sizeof " #t)'
	out << '#define CHECK_STRUCT_SIZE(t) _Static_assert(sizeof(struct v_abi_##t) == sizeof(struct t), "sizeof struct " #t)'
	out << '#define CHECK_FIELD(t, f) _Static_assert(offsetof(struct v_abi_##t, f) == offsetof(struct t, f), "offset " #t "." #f); _Static_assert(sizeof(((struct v_abi_##t *)0)->f) == sizeof(((struct t *)0)->f), "size " #t "." #f)'
	out << '#define CHECK_FN(f) _Static_assert(__builtin_types_compatible_p(__typeof__(v_abi_##f), __typeof__(f)), "prototype " #f)'
	mut macros := ['O_RDONLY', 'O_WRONLY', 'O_RDWR', 'O_APPEND', 'O_CREAT', 'O_TRUNC', 'O_EXCL',
		'O_CLOEXEC', 'S_IRUSR', 'S_IWUSR', 'S_IXUSR', 'S_IRGRP', 'S_IWGRP', 'S_IXGRP', 'S_IROTH',
		'O_SYNC', 'S_IRUSR', 'S_IWUSR', 'S_IXUSR', 'S_IRGRP', 'S_IWGRP', 'S_IXGRP', 'S_IROTH',
		'O_NONBLOCK', 'S_IRUSR', 'S_IWUSR', 'S_IXUSR', 'S_IRGRP', 'S_IWGRP', 'S_IXGRP', 'S_IROTH',
		'O_NOCTTY', 'S_IRUSR', 'S_IWUSR', 'S_IXUSR', 'S_IRGRP', 'S_IWGRP', 'S_IXGRP', 'S_IROTH',
		'S_IWOTH', 'S_IXOTH', 'S_IFMT', 'S_IFDIR', 'S_IFREG', 'S_IFLNK', 'S_IFSOCK', 'S_IFIFO',
		'S_IFCHR', 'S_IFBLK', 'F_GETFD', 'F_SETFD', 'FD_CLOEXEC', 'EINTR', 'EAGAIN', 'ENOENT',
		'EEXIST', 'SEEK_SET', 'SEEK_CUR', 'SEEK_END', 'EOF', '_IOFBF', '_IOLBF', '_IONBF', 'BUFSIZ',
		'PROT_READ', 'PROT_WRITE', 'MAP_PRIVATE', 'MAP_ANONYMOUS', 'CLOCK_REALTIME', 'CLOCK_MONOTONIC',
		'WNOHANG', '_SC_NPROCESSORS_ONLN', 'STDIN_FILENO', 'STDOUT_FILENO', 'STDERR_FILENO',
		'FIONREAD', 'FD_SETSIZE', 'PTHREAD_CREATE_DETACHED']
	for name in macros {
		out << 'CHECK_EQ(v_abi_${name}, ${name});'
	}
	out << 'CHECK_EQ((long)v_abi_MAP_FAILED, (long)MAP_FAILED);'
	for name in ['int8_t', 'int16_t', 'int32_t', 'int64_t', 'uint8_t', 'uint16_t', 'uint32_t',
		'uint64_t', 'intptr_t', 'uintptr_t', 'size_t', 'ssize_t', 'off_t', 'pid_t', 'uid_t', 'gid_t',
		'time_t', 'mode_t', 'dev_t', 'nlink_t', 'ino_t', 'clockid_t', 'suseconds_t', 'pthread_t',
		'pthread_attr_t', 'pthread_mutex_t', 'pthread_mutexattr_t', 'pthread_once_t', 'pthread_key_t',
		'fd_set'] {
		out << 'CHECK_SIZE(${name});'
	}
	if macos {
		out << 'CHECK_SIZE(mach_timebase_info_data_t);'
		out << 'CHECK_EQ(offsetof(v_abi_mach_timebase_info_data_t, denom), offsetof(mach_timebase_info_data_t, denom));'
	}
	for name in ['stat', 'dirent', 'timespec', 'timeval', 'tm'] {
		out << 'CHECK_STRUCT_SIZE(${name});'
	}
	for field in ['st_dev', 'st_mode', 'st_nlink', 'st_ino', 'st_uid', 'st_gid', 'st_rdev', 'st_size',
		'st_blocks', 'st_blksize'] {
		out << 'CHECK_FIELD(stat, ${field});'
	}
	for field in ['st_atime', 'st_mtime', 'st_ctime'] {
		out << 'CHECK_EQ(offsetof(struct v_abi_stat, v_abi_${field}), offsetof(struct stat, ${field}));'
	}
	for field in ['d_ino', 'd_reclen', 'd_type', 'd_name'] {
		out << 'CHECK_FIELD(dirent, ${field});'
	}
	for field in ['tv_sec', 'tv_nsec'] {
		out << 'CHECK_FIELD(timespec, ${field});'
	}
	for field in ['tv_sec', 'tv_usec'] {
		out << 'CHECK_FIELD(timeval, ${field});'
	}
	for field in ['tm_sec', 'tm_min', 'tm_hour', 'tm_mday', 'tm_mon', 'tm_year', 'tm_wday', 'tm_yday',
		'tm_isdst', 'tm_gmtoff', 'tm_zone'] {
		out << 'CHECK_FIELD(tm, ${field});'
	}
	for f in fastc_c_abi_functions(host_os, if macos { 'arm64' } else { 'amd64' }) {
		out << 'CHECK_FN(${f.name});'
	}
	out << 'int main(void) {'
	out << '\tv_abi_pthread_once_t mine = v_abi_PTHREAD_ONCE_INIT;'
	out << '\tpthread_once_t real = PTHREAD_ONCE_INIT;'
	out << '\tif (memcmp(&mine, &real, sizeof(real)) != 0) { puts("PTHREAD_ONCE_INIT differs"); return 1; }'
	out << '\tv_abi_fd_set my_set; fd_set real_set;'
	out << '\tv_abi_FD_ZERO(&my_set); FD_ZERO(&real_set);'
	out << '\tv_abi_FD_SET(3, &my_set); FD_SET(3, &real_set);'
	out << '\tv_abi_FD_SET(63, &my_set); FD_SET(63, &real_set);'
	out << '\tv_abi_FD_SET(64, &my_set); FD_SET(64, &real_set);'
	out << '\tv_abi_FD_SET(1000, &my_set); FD_SET(1000, &real_set);'
	out << '\tif (memcmp(&my_set, &real_set, sizeof(real_set)) != 0) { puts("FD_SET differs"); return 1; }'
	out << '\tif (!v_abi_FD_ISSET(64, &my_set) || v_abi_FD_ISSET(65, &my_set)) { puts("FD_ISSET differs"); return 1; }'
	out << '\tif (v_abi_errno != errno) { puts("errno differs"); return 1; }'
	out << '\tif (v_abi_stdout != stdout || v_abi_stderr != stderr || v_abi_stdin != stdin) { puts("stdio streams differ"); return 1; }'
	out << '\tif (v_abi_environ != environ) { puts("environ differs"); return 1; }'
	out << '\tputs("ok");'
	out << '\treturn 0;'
	out << '}'
	return out.join('\n') + '\n'
}
