module fastc

import os
import strings

// The self-host C is emitted without a single `#include`: everything it takes
// from the C library is declared here, per target, from a table kept in V.
// The prelude replaces the header block of the preamble, so the declarations
// below are exactly what TinyCC otherwise parsed out of the system headers
// (about 60K preprocessed lines per build), and every C function the emitted
// code may call gets a prototype with its real C types, which keeps argument
// conversions identical to a header-based build. `-Werror=implicit-function-
// declaration` is passed along, so a call to a C function missing from the
// table fails the build instead of being compiled with an implicit `int`
// signature. c_abi_test.v checks the table against the host headers.

// fastc_host_uses_glibc reports whether this Linux process has positively
// identifiable glibc objects loaded. A static process or an unavailable procfs
// deliberately returns false: retaining the system headers is the safe fallback.
fn fastc_host_uses_glibc() bool {
	if os.user_os() != 'linux' {
		return false
	}
	maps := os.read_file('/proc/self/maps') or { return false }
	for line in maps.split_into_lines() {
		base := os.file_name(line.all_after_last(' '))
		if base == 'libc.so.6' || base.starts_with('ld-linux-') {
			return true
		}
		if base.starts_with('libc-') && base.ends_with('.so') && base.len > 'libc-.so'.len
			&& base['libc-'.len].is_digit() {
			return true
		}
	}
	return false
}

// fastc_c_abi_supported reports whether the header-free prelude is available
// for a target (the tables cover 64-bit macOS and glibc Linux).
fn fastc_c_abi_supported(target_os string, target_arch string, host_uses_glibc bool) bool {
	if target_os == 'macos' {
		return target_arch in ['arm64', 'amd64']
	}
	if target_os == 'linux' {
		return host_uses_glibc && target_arch in ['arm64', 'amd64']
	}
	return false
}

// FastcCAbiFunction is one C library function: its C prototype, with `@`
// standing for the (possibly prefixed) function name, and the asm label the
// symbol needs on the target ('' for the plain name).
struct FastcCAbiFunction {
	name      string
	prototype string
	asm_label string
}

fn fastc_c_abi_fn(name string, prototype string, asm_label string) FastcCAbiFunction {
	return FastcCAbiFunction{
		name: name
		prototype: prototype
		asm_label: asm_label
	}
}

// fastc_c_abi_functions lists the C functions with their prototypes for a
// target.
fn fastc_c_abi_functions(target_os string, target_arch string) []FastcCAbiFunction {
	macos := target_os == 'macos'
	inode64 := macos && target_arch == 'amd64'
	realpath_label := if macos { '_realpath\$DARWIN_EXTSN' } else { '' }
	stat_label := if inode64 { '_stat\$INODE64' } else { '' }
	lstat_label := if inode64 { '_lstat\$INODE64' } else { '' }
	fstat_label := if inode64 { '_fstat\$INODE64' } else { '' }
	opendir_label := if inode64 { '_opendir\$INODE64' } else { '' }
	readdir_label := if inode64 { '_readdir\$INODE64' } else { '' }
	mut fns := [
		fastc_c_abi_fn('malloc', 'void *@(size_t);', ''),
		fastc_c_abi_fn('calloc', 'void *@(size_t, size_t);', ''),
		fastc_c_abi_fn('realloc', 'void *@(void *, size_t);', ''),
		fastc_c_abi_fn('free', 'void @(void *);', ''),
		fastc_c_abi_fn('aligned_alloc', 'void *@(size_t, size_t);', ''),
		fastc_c_abi_fn('memcpy', 'void *@(void *, const void *, size_t);', ''),
		fastc_c_abi_fn('memmove', 'void *@(void *, const void *, size_t);', ''),
		fastc_c_abi_fn('memset', 'void *@(void *, int, size_t);', ''),
		fastc_c_abi_fn('memcmp', 'int @(const void *, const void *, size_t);', ''),
		fastc_c_abi_fn('memchr', 'void *@(const void *, int, size_t);', ''),
		fastc_c_abi_fn('strlen', 'size_t @(const char *);', ''),
		fastc_c_abi_fn('strcmp', 'int @(const char *, const char *);', ''),
		fastc_c_abi_fn('strerror', 'char *@(int);', ''),
		fastc_c_abi_fn('bzero', 'void @(void *, size_t);', ''),
		fastc_c_abi_fn('fopen', 'FILE *@(const char *, const char *);', ''),
		fastc_c_abi_fn('fdopen', 'FILE *@(int, const char *);', ''),
		fastc_c_abi_fn('fclose', 'int @(FILE *);', ''),
		fastc_c_abi_fn('fread', 'size_t @(void *, size_t, size_t, FILE *);', ''),
		fastc_c_abi_fn('fwrite', 'size_t @(const void *, size_t, size_t, FILE *);', ''),
		fastc_c_abi_fn('fseek', 'int @(FILE *, long, int);', ''),
		fastc_c_abi_fn('ftell', 'long @(FILE *);', ''),
		fastc_c_abi_fn('rewind', 'void @(FILE *);', ''),
		fastc_c_abi_fn('fflush', 'int @(FILE *);', ''),
		fastc_c_abi_fn('feof', 'int @(FILE *);', ''),
		fastc_c_abi_fn('ferror', 'int @(FILE *);', ''),
		fastc_c_abi_fn('fileno', 'int @(FILE *);', ''),
		fastc_c_abi_fn('fgets', 'char *@(char *, int, FILE *);', ''),
		fastc_c_abi_fn('fgetc', 'int @(FILE *);', ''),
		fastc_c_abi_fn('fputs', 'int @(const char *, FILE *);', ''),
		fastc_c_abi_fn('fputc', 'int @(int, FILE *);', ''),
		fastc_c_abi_fn('getchar', 'int @(void);', ''),
		fastc_c_abi_fn('fprintf', 'int @(FILE *, const char *, ...);', ''),
		fastc_c_abi_fn('printf', 'int @(const char *, ...);', ''),
		fastc_c_abi_fn('snprintf', 'int @(char *, size_t, const char *, ...);', ''),
		fastc_c_abi_fn('sscanf', 'int @(const char *, const char *, ...);', ''),
		fastc_c_abi_fn('setvbuf', 'int @(FILE *, char *, int, size_t);', ''),
		fastc_c_abi_fn('getline', 'ssize_t @(char **, size_t *, FILE *);', ''),
		fastc_c_abi_fn('popen', 'FILE *@(const char *, const char *);', ''),
		fastc_c_abi_fn('pclose', 'int @(FILE *);', ''),
		fastc_c_abi_fn('open', 'int @(const char *, int, ...);', ''),
		fastc_c_abi_fn('close', 'int @(int);', ''),
		fastc_c_abi_fn('read', 'ssize_t @(int, void *, size_t);', ''),
		fastc_c_abi_fn('write', 'ssize_t @(int, const void *, size_t);', ''),
		fastc_c_abi_fn('lseek', 'off_t @(int, off_t, int);', ''),
		fastc_c_abi_fn('ftruncate', 'int @(int, off_t);', ''),
		fastc_c_abi_fn('symlink', 'int @(const char *, const char *);', ''),
		fastc_c_abi_fn('link', 'int @(const char *, const char *);', ''),
		fastc_c_abi_fn('fcntl', 'int @(int, int, ...);', ''),
		fastc_c_abi_fn('flock', 'int @(int, int);', ''),
		fastc_c_abi_fn('ioctl', 'int @(int, unsigned long, ...);', ''),
		fastc_c_abi_fn('isatty', 'int @(int);', ''),
		fastc_c_abi_fn('dup2', 'int @(int, int);', ''),
		fastc_c_abi_fn('pipe', 'int @(int *);', ''),
		fastc_c_abi_fn('access', 'int @(const char *, int);', ''),
		fastc_c_abi_fn('chdir', 'int @(const char *);', ''),
		fastc_c_abi_fn('getcwd', 'char *@(char *, size_t);', ''),
		fastc_c_abi_fn('mkdir', 'int @(const char *, mode_t);', ''),
		fastc_c_abi_fn('mkstemp', 'int @(char *);', ''),
		fastc_c_abi_fn('rmdir', 'int @(const char *);', ''),
		fastc_c_abi_fn('unlink', 'int @(const char *);', ''),
		fastc_c_abi_fn('remove', 'int @(const char *);', ''),
		fastc_c_abi_fn('rename', 'int @(const char *, const char *);', ''),
		fastc_c_abi_fn('chmod', 'int @(const char *, mode_t);', ''),
		fastc_c_abi_fn('utime', 'int @(const char *, const struct utimbuf *);', ''),
		fastc_c_abi_fn('uname', 'int @(struct utsname *);', ''),
		fastc_c_abi_fn('readlink', 'ssize_t @(const char *, char *, size_t);', ''),
		fastc_c_abi_fn('realpath', 'char *@(const char *, char *);', realpath_label),
		fastc_c_abi_fn('stat', 'int @(const char *, struct stat *);', stat_label),
		fastc_c_abi_fn('lstat', 'int @(const char *, struct stat *);', lstat_label),
		fastc_c_abi_fn('fstat', 'int @(int, struct stat *);', fstat_label),
		fastc_c_abi_fn('opendir', 'DIR *@(const char *);', opendir_label),
		fastc_c_abi_fn('readdir', 'struct dirent *@(DIR *);', readdir_label),
		fastc_c_abi_fn('closedir', 'int @(DIR *);', ''),
		fastc_c_abi_fn('getenv', 'char *@(const char *);', ''),
		fastc_c_abi_fn('setenv', 'int @(const char *, const char *, int);', ''),
		fastc_c_abi_fn('unsetenv', 'int @(const char *);', ''),
		fastc_c_abi_fn('exit', 'void @(int);', ''),
		fastc_c_abi_fn('atexit', 'int @(void (*)(void));', ''),
		fastc_c_abi_fn('abort', 'void @(void);', ''),
		fastc_c_abi_fn('system', 'int @(const char *);', ''),
		fastc_c_abi_fn('signal', 'void (*@(int, void (*)(int)))(int);', ''),
		fastc_c_abi_fn('getpid', 'pid_t @(void);', ''),
		fastc_c_abi_fn('getuid', 'uid_t @(void);', ''),
		fastc_c_abi_fn('geteuid', 'uid_t @(void);', ''),
		fastc_c_abi_fn('fork', 'pid_t @(void);', ''),
		fastc_c_abi_fn('execve', 'int @(const char *, char *const *, char *const *);', ''),
		fastc_c_abi_fn('execvp', 'int @(const char *, char *const *);', ''),
		fastc_c_abi_fn('wait', 'pid_t @(int *);', ''),
		fastc_c_abi_fn('waitpid', 'pid_t @(pid_t, int *, int);', ''),
		fastc_c_abi_fn('setpgid', 'int @(pid_t, pid_t);', ''),
		fastc_c_abi_fn('sysconf', 'long @(int);', ''),
		fastc_c_abi_fn('pthread_create', 'int @(pthread_t *, const pthread_attr_t *, void *(*)(void *), void *);', ''),
		fastc_c_abi_fn('pthread_join', 'int @(pthread_t, void **);', ''),
		fastc_c_abi_fn('pthread_detach', 'int @(pthread_t);', ''),
		fastc_c_abi_fn('pthread_self', 'pthread_t @(void);', ''),
		fastc_c_abi_fn('pthread_attr_init', 'int @(pthread_attr_t *);', ''),
		fastc_c_abi_fn('pthread_attr_destroy', 'int @(pthread_attr_t *);', ''),
		fastc_c_abi_fn('pthread_attr_setstacksize', 'int @(pthread_attr_t *, size_t);', ''),
		fastc_c_abi_fn('pthread_attr_setdetachstate', 'int @(pthread_attr_t *, int);', ''),
		fastc_c_abi_fn('pthread_key_create', 'int @(pthread_key_t *, void (*)(void *));', ''),
		fastc_c_abi_fn('pthread_getspecific', 'void *@(pthread_key_t);', ''),
		fastc_c_abi_fn('pthread_setspecific', 'int @(pthread_key_t, const void *);', ''),
		fastc_c_abi_fn('pthread_once', 'int @(pthread_once_t *, void (*)(void));', ''),
		fastc_c_abi_fn('pthread_mutex_init', 'int @(pthread_mutex_t *, const pthread_mutexattr_t *);', ''),
		fastc_c_abi_fn('pthread_mutex_lock', 'int @(pthread_mutex_t *);', ''),
		fastc_c_abi_fn('pthread_mutex_trylock', 'int @(pthread_mutex_t *);', ''),
		fastc_c_abi_fn('pthread_mutex_unlock', 'int @(pthread_mutex_t *);', ''),
		fastc_c_abi_fn('pthread_mutex_destroy', 'int @(pthread_mutex_t *);', ''),
		fastc_c_abi_fn('clock_gettime', 'int @(clockid_t, struct timespec *);', ''),
		fastc_c_abi_fn('gettimeofday', 'int @(struct timeval *, void *);', ''),
		fastc_c_abi_fn('nanosleep', 'int @(const struct timespec *, struct timespec *);', ''),
		fastc_c_abi_fn('localtime_r', 'struct tm *@(const time_t *, struct tm *);', ''),
		fastc_c_abi_fn('gmtime_r', 'struct tm *@(const time_t *, struct tm *);', ''),
		fastc_c_abi_fn('time', 'time_t @(time_t *);', ''),
		fastc_c_abi_fn('strftime', 'size_t @(char *, size_t, const char *, const struct tm *);', ''),
		fastc_c_abi_fn('mmap', 'void *@(void *, size_t, int, int, int, off_t);', ''),
		fastc_c_abi_fn('munmap', 'int @(void *, size_t);', ''),
		fastc_c_abi_fn('select', 'int @(int, fd_set *, fd_set *, fd_set *, struct timeval *);', ''),
		fastc_c_abi_fn('pow', 'double @(double, double);', ''),
	]
	if macos {
		fns << fastc_c_abi_fn('getrusage', 'int @(int, struct rusage *);', '')
		fns << fastc_c_abi_fn('task_info', 'int @(uint32_t, uint32_t, int *, uint32_t *);', '')
		fns << fastc_c_abi_fn('pthread_condattr_init', 'int @(pthread_condattr_t *);', '')
		fns << fastc_c_abi_fn('pthread_attr_set_qos_class_np', 'int @(pthread_attr_t *, uint32_t, int);', '')
		fns << fastc_c_abi_fn('pthread_condattr_setpshared', 'int @(pthread_condattr_t *, int);', '')
		fns << fastc_c_abi_fn('pthread_condattr_destroy', 'int @(pthread_condattr_t *);', '')
		fns << fastc_c_abi_fn('pthread_cond_init', 'int @(pthread_cond_t *, const pthread_condattr_t *);', '')
		fns << fastc_c_abi_fn('pthread_cond_signal', 'int @(pthread_cond_t *);', '')
		fns << fastc_c_abi_fn('pthread_cond_wait', 'int @(pthread_cond_t *, pthread_mutex_t *);', '')
		fns << fastc_c_abi_fn('pthread_cond_timedwait', 'int @(pthread_cond_t *, pthread_mutex_t *, const struct timespec *);', '')
		fns << fastc_c_abi_fn('pthread_cond_destroy', 'int @(pthread_cond_t *);', '')
		fns << fastc_c_abi_fn('pthread_rwlockattr_init', 'int @(pthread_rwlockattr_t *);', '')
		fns << fastc_c_abi_fn('pthread_rwlock_init', 'int @(pthread_rwlock_t *, const pthread_rwlockattr_t *);', '')
		fns << fastc_c_abi_fn('pthread_rwlock_rdlock', 'int @(pthread_rwlock_t *);', '')
		fns << fastc_c_abi_fn('pthread_rwlock_tryrdlock', 'int @(pthread_rwlock_t *);', '')
		fns << fastc_c_abi_fn('pthread_rwlock_wrlock', 'int @(pthread_rwlock_t *);', '')
		fns << fastc_c_abi_fn('pthread_rwlock_trywrlock', 'int @(pthread_rwlock_t *);', '')
		fns << fastc_c_abi_fn('pthread_rwlock_unlock', 'int @(pthread_rwlock_t *);', '')
		fns << fastc_c_abi_fn('pthread_rwlock_destroy', 'int @(pthread_rwlock_t *);', '')
		fns << fastc_c_abi_fn('__error', 'int *@(void);', '')
		fns << fastc_c_abi_fn('CC_SHA256', 'unsigned char *@(const void *, unsigned int, unsigned char *);', '')
		fns << fastc_c_abi_fn('mach_absolute_time', 'uint64_t @(void);', '')
		fns << fastc_c_abi_fn('mach_timebase_info', 'int @(mach_timebase_info_data_t *);', '')
		fns << fastc_c_abi_fn('clock_gettime_nsec_np', 'uint64_t @(clockid_t);', '')
		fns << fastc_c_abi_fn('_dyld_get_image_header', 'const struct mach_header *@(uint32_t);', '')
		fns << fastc_c_abi_fn('_dyld_get_image_name', 'const char *@(uint32_t);', '')
	} else {
		fns << fastc_c_abi_fn('__errno_location', 'int *@(void);', '')
	}
	return fns
}

// fastc_c_abi_prelude renders the header-free prelude for a target. `p`
// prefixes every name the prelude introduces (types, struct tags, macros,
// functions, globals); it is '' for generation and lets c_abi_test.v compile
// the prelude next to the real headers.
fn fastc_c_abi_prelude(target_os string, target_arch string, p string) string {
	macos := target_os == 'macos'
	arm := target_arch == 'arm64'
	// C symbols carry a leading underscore in Mach-O.
	symbol_prefix := if macos { '_' } else { '' }
	mut b := strings.new_builder(8192)
	b.writeln('/* C library declarations for ${target_os}/${target_arch}, from vlib/v3/gen/fastc/c_abi.v (no headers). */')
	// V's own C helpers (inlined below the preamble) skip their system
	// includes under this macro.
	b.writeln('#define V_FASTC_NO_HEADERS 1')
	b.writeln('typedef signed char ${p}int8_t; typedef short ${p}int16_t; typedef int ${p}int32_t; typedef long long ${p}int64_t;')
	b.writeln('typedef unsigned char ${p}uint8_t; typedef unsigned short ${p}uint16_t; typedef unsigned int ${p}uint32_t; typedef unsigned long long ${p}uint64_t;')
	b.writeln('typedef long ${p}intptr_t; typedef unsigned long ${p}uintptr_t; typedef unsigned long ${p}size_t; typedef long ${p}ssize_t;')
	b.writeln('typedef long ${p}off_t; typedef int ${p}pid_t; typedef unsigned int ${p}uid_t; typedef unsigned int ${p}gid_t; typedef long ${p}time_t;')
	b.writeln('#define ${p}NULL ((void *)0)')
	if macos {
		b.writeln('typedef unsigned short ${p}mode_t; typedef int ${p}dev_t; typedef unsigned short ${p}nlink_t; typedef unsigned long long ${p}ino_t;')
		b.writeln('typedef unsigned int ${p}clockid_t; typedef int ${p}suseconds_t;')
		b.writeln('typedef struct __sFILE ${p}FILE;')
		b.writeln('typedef struct ${p}v_dir ${p}DIR;')
		b.writeln('struct ${p}timespec { long tv_sec; long tv_nsec; };')
		b.writeln('struct ${p}timeval { long tv_sec; ${p}suseconds_t tv_usec; };')
		b.writeln('struct ${p}utimbuf { ${p}time_t actime; ${p}time_t modtime; };')
		b.writeln('struct ${p}utsname { char sysname[256]; char nodename[256]; char release[256]; char version[256]; char machine[256]; };')
		b.writeln('struct ${p}rusage { struct ${p}timeval ru_utime; struct ${p}timeval ru_stime; long ru_maxrss; long ru_ixrss; long ru_idrss; long ru_isrss; long ru_minflt; long ru_majflt; long ru_nswap; long ru_inblock; long ru_oublock; long ru_msgsnd; long ru_msgrcv; long ru_nsignals; long ru_nvcsw; long ru_nivcsw; };')
		b.writeln('struct __attribute__((packed, aligned(4))) ${p}task_basic_info { int suspend_count; ${p}uint64_t virtual_size; ${p}uint64_t resident_size; struct { int seconds; int microseconds; } user_time; struct { int seconds; int microseconds; } system_time; int policy; };')
		b.writeln('struct ${p}flock { ${p}off_t l_start; ${p}off_t l_len; ${p}pid_t l_pid; short l_type; short l_whence; };')
		b.writeln('struct ${p}stat { ${p}dev_t st_dev; ${p}mode_t st_mode; ${p}nlink_t st_nlink; ${p}ino_t st_ino; ${p}uid_t st_uid; ${p}gid_t st_gid; ${p}dev_t st_rdev; struct ${p}timespec st_atimespec; struct ${p}timespec st_mtimespec; struct ${p}timespec st_ctimespec; struct ${p}timespec st_birthtimespec; ${p}off_t st_size; long long st_blocks; int st_blksize; unsigned int st_flags; unsigned int st_gen; int st_lspare; long long st_qspare[2]; };')
		b.writeln('#define ${p}st_atime st_atimespec.tv_sec')
		b.writeln('#define ${p}st_mtime st_mtimespec.tv_sec')
		b.writeln('#define ${p}st_ctime st_ctimespec.tv_sec')
		b.writeln('struct ${p}dirent { unsigned long long d_ino; unsigned long long d_seekoff; unsigned short d_reclen; unsigned short d_namlen; unsigned char d_type; char d_name[1024]; };')
		b.writeln('typedef struct ${p}v_opaque_pthread *${p}pthread_t;')
		b.writeln('typedef struct { long __sig; char __opaque[56]; } ${p}pthread_attr_t;')
		b.writeln('typedef struct { long __sig; char __opaque[56]; } ${p}pthread_mutex_t;')
		b.writeln('typedef struct { long __sig; char __opaque[8]; } ${p}pthread_mutexattr_t;')
		b.writeln('typedef struct { long __sig; char __opaque[8]; } ${p}pthread_once_t;')
		b.writeln('typedef struct { long __sig; char __opaque[40]; } ${p}pthread_cond_t;')
		b.writeln('typedef struct { long __sig; char __opaque[8]; } ${p}pthread_condattr_t;')
		b.writeln('typedef struct { long __sig; char __opaque[192]; } ${p}pthread_rwlock_t;')
		b.writeln('typedef struct { long __sig; char __opaque[16]; } ${p}pthread_rwlockattr_t;')
		b.writeln('#define ${p}PTHREAD_ONCE_INIT {0x30B1BCBA, {0}}')
		b.writeln('typedef unsigned long ${p}pthread_key_t;')
		b.writeln('typedef struct { ${p}uint32_t numer; ${p}uint32_t denom; } ${p}mach_timebase_info_data_t;')
		b.writeln('struct mach_header;')
		b.writeln('#define ${p}PTHREAD_CREATE_DETACHED 2')
		b.writeln('#define ${p}PTHREAD_PROCESS_PRIVATE 2')
		b.writeln('typedef struct { ${p}int32_t fds_bits[32]; } ${p}fd_set;')
		b.writeln('#define ${p}FD_SETSIZE 1024')
		b.writeln('#define ${p}FD_ZERO(set) ${p}memset((set), 0, sizeof(*(set)))')
		b.writeln('#define ${p}FD_SET(fd, set) ((set)->fds_bits[(fd) / 32] |= (${p}int32_t)(1U << ((fd) % 32)))')
		b.writeln('#define ${p}FD_ISSET(fd, set) (((set)->fds_bits[(fd) / 32] & (${p}int32_t)(1U << ((fd) % 32))) != 0)')
		b.writeln('extern ${p}FILE *__stdinp; extern ${p}FILE *__stdoutp; extern ${p}FILE *__stderrp;')
		b.writeln('#define ${p}stdin __stdinp')
		b.writeln('#define ${p}stdout __stdoutp')
		b.writeln('#define ${p}stderr __stderrp')
		b.writeln('#define ${p}errno (*${p}__error())')
		b.writeln('#define ${p}O_RDONLY 0')
		b.writeln('#define ${p}O_WRONLY 1')
		b.writeln('#define ${p}O_RDWR 2')
		b.writeln('#define ${p}O_APPEND 0x8')
		b.writeln('#define ${p}O_CREAT 0x200')
		b.writeln('#define ${p}O_TRUNC 0x400')
		b.writeln('#define ${p}O_EXCL 0x800')
		b.writeln('#define ${p}O_CLOEXEC 0x1000000')
		b.writeln('#define ${p}O_SYNC 0x80')
		b.writeln('#define ${p}O_NONBLOCK 0x4')
		b.writeln('#define ${p}O_NOCTTY 0x20000')
		b.writeln('#define ${p}F_SETLK 8')
		b.writeln('#define ${p}F_SETLKW 9')
		b.writeln('#define ${p}F_RDLCK 1')
		b.writeln('#define ${p}F_UNLCK 2')
		b.writeln('#define ${p}F_WRLCK 3')
		b.writeln('#define ${p}LOCK_SH 0x01')
		b.writeln('#define ${p}LOCK_EX 0x02')
		b.writeln('#define ${p}LOCK_NB 0x04')
		b.writeln('#define ${p}LOCK_UN 0x08')
		b.writeln('#define ${p}EAGAIN 35')
		b.writeln('#define ${p}EINVAL 22')
		b.writeln('#define ${p}ETIMEDOUT 60')
		b.writeln('#define ${p}SIGHUP 1')
		b.writeln('#define ${p}SIGINT 2')
		b.writeln('#define ${p}SIGQUIT 3')
		b.writeln('#define ${p}SIGILL 4')
		b.writeln('#define ${p}SIGABRT 6')
		b.writeln('#define ${p}SIGFPE 8')
		b.writeln('#define ${p}SIGKILL 9')
		b.writeln('#define ${p}SIGSEGV 11')
		b.writeln('#define ${p}SIGPIPE 13')
		b.writeln('#define ${p}SIGALRM 14')
		b.writeln('#define ${p}SIGTERM 15')
		b.writeln('#define ${p}SIG_DFL ((void (*)(int))0)')
		b.writeln('#define ${p}SIG_IGN ((void (*)(int))1)')
		b.writeln('#define ${p}SIG_ERR ((void (*)(int))-1)')
		b.writeln('#define ${p}MACH_TASK_BASIC_INFO_COUNT 12')
		b.writeln('#define ${p}TASK_BASIC_INFO ${if arm { 18 } else { 5 }}')
		b.writeln('#define ${p}KERN_SUCCESS 0')
		b.writeln('#define ${p}QOS_CLASS_USER_INITIATED 0x19')
		b.writeln('#define ${p}RUSAGE_SELF 0')
		b.writeln('#define ${p}BUFSIZ 1024')
		b.writeln('#define ${p}MAP_ANONYMOUS 0x1000')
		b.writeln('#define ${p}MAP_ANON ${p}MAP_ANONYMOUS')
		b.writeln('#define ${p}CLOCK_MONOTONIC 6')
		b.writeln('#define ${p}_SC_NPROCESSORS_ONLN 58')
		b.writeln('#define ${p}FIONREAD 0x4004667fUL')
	} else {
		b.writeln('typedef unsigned int ${p}mode_t; typedef unsigned long ${p}dev_t; typedef unsigned long ${p}ino_t;')
		if arm {
			b.writeln('typedef unsigned int ${p}nlink_t;')
		} else {
			b.writeln('typedef unsigned long ${p}nlink_t;')
		}
		b.writeln('typedef int ${p}clockid_t; typedef long ${p}suseconds_t;')
		b.writeln('typedef struct _IO_FILE ${p}FILE;')
		b.writeln('typedef struct __dirstream ${p}DIR;')
		b.writeln('struct ${p}timespec { long tv_sec; long tv_nsec; };')
		b.writeln('struct ${p}timeval { long tv_sec; ${p}suseconds_t tv_usec; };')
		if arm {
			b.writeln('struct ${p}stat { ${p}dev_t st_dev; ${p}ino_t st_ino; ${p}mode_t st_mode; ${p}nlink_t st_nlink; ${p}uid_t st_uid; ${p}gid_t st_gid; ${p}dev_t st_rdev; unsigned long __pad1; ${p}off_t st_size; int st_blksize; int __pad2; long st_blocks; struct ${p}timespec st_atim; struct ${p}timespec st_mtim; struct ${p}timespec st_ctim; unsigned int __unused[2]; };')
		} else {
			b.writeln('struct ${p}stat { ${p}dev_t st_dev; ${p}ino_t st_ino; ${p}nlink_t st_nlink; ${p}mode_t st_mode; ${p}uid_t st_uid; ${p}gid_t st_gid; int __pad0; ${p}dev_t st_rdev; ${p}off_t st_size; long st_blksize; long st_blocks; struct ${p}timespec st_atim; struct ${p}timespec st_mtim; struct ${p}timespec st_ctim; long __unused[3]; };')
		}
		b.writeln('#define ${p}st_atime st_atim.tv_sec')
		b.writeln('#define ${p}st_mtime st_mtim.tv_sec')
		b.writeln('#define ${p}st_ctime st_ctim.tv_sec')
		b.writeln('struct ${p}dirent { unsigned long d_ino; long d_off; unsigned short d_reclen; unsigned char d_type; char d_name[256]; };')
		b.writeln('typedef unsigned long ${p}pthread_t;')
		if arm {
			b.writeln('typedef union { char __size[64]; long __align; } ${p}pthread_attr_t;')
			b.writeln('typedef union { char __size[48]; long __align; } ${p}pthread_mutex_t;')
		} else {
			b.writeln('typedef union { char __size[56]; long __align; } ${p}pthread_attr_t;')
			b.writeln('typedef union { char __size[40]; long __align; } ${p}pthread_mutex_t;')
		}
		b.writeln('typedef union { char __size[4]; int __align; } ${p}pthread_mutexattr_t;')
		b.writeln('typedef int ${p}pthread_once_t;')
		b.writeln('#define ${p}PTHREAD_ONCE_INIT 0')
		b.writeln('typedef unsigned int ${p}pthread_key_t;')
		b.writeln('#define ${p}PTHREAD_CREATE_DETACHED 1')
		b.writeln('typedef struct { long fds_bits[16]; } ${p}fd_set;')
		b.writeln('#define ${p}FD_SETSIZE 1024')
		b.writeln('#define ${p}FD_ZERO(set) ${p}memset((set), 0, sizeof(*(set)))')
		b.writeln('#define ${p}FD_SET(fd, set) ((set)->fds_bits[(fd) / 64] |= (1UL << ((fd) % 64)))')
		b.writeln('#define ${p}FD_ISSET(fd, set) (((set)->fds_bits[(fd) / 64] & (1UL << ((fd) % 64))) != 0)')
		b.writeln('extern ${p}FILE *stdin; extern ${p}FILE *stdout; extern ${p}FILE *stderr;')
		if p != '' {
			b.writeln('#define ${p}stdin stdin')
			b.writeln('#define ${p}stdout stdout')
			b.writeln('#define ${p}stderr stderr')
		}
		b.writeln('#define ${p}errno (*${p}__errno_location())')
		b.writeln('#define ${p}O_RDONLY 0')
		b.writeln('#define ${p}O_WRONLY 1')
		b.writeln('#define ${p}O_RDWR 2')
		b.writeln('#define ${p}O_APPEND 02000')
		b.writeln('#define ${p}O_CREAT 0100')
		b.writeln('#define ${p}O_TRUNC 01000')
		b.writeln('#define ${p}O_EXCL 0200')
		b.writeln('#define ${p}O_CLOEXEC 02000000')
		b.writeln('#define ${p}O_SYNC 04010000')
		b.writeln('#define ${p}O_NONBLOCK 04000')
		b.writeln('#define ${p}O_NOCTTY 0400')
		b.writeln('#define ${p}EAGAIN 11')
		b.writeln('#define ${p}BUFSIZ 8192')
		b.writeln('#define ${p}MAP_ANONYMOUS 0x20')
		b.writeln('#define ${p}CLOCK_MONOTONIC 1')
		b.writeln('#define ${p}_SC_NPROCESSORS_ONLN 84')
		b.writeln('#define ${p}FIONREAD 0x541B')
	}
	b.writeln('struct ${p}tm { int tm_sec; int tm_min; int tm_hour; int tm_mday; int tm_mon; int tm_year; int tm_wday; int tm_yday; int tm_isdst; long tm_gmtoff; char *tm_zone; };')
	if p == '' {
		b.writeln('extern char **environ;')
		if macos {
			b.writeln('extern ${p}uint32_t mach_task_self_;')
			b.writeln('#define mach_task_self() mach_task_self_')
		}
	} else {
		b.writeln('extern char **${p}environ __asm__("${symbol_prefix}environ");')
		if macos {
			b.writeln('extern ${p}uint32_t ${p}mach_task_self_ __asm__("${symbol_prefix}mach_task_self_");')
			b.writeln('#define ${p}mach_task_self() ${p}mach_task_self_')
		}
	}
	b.writeln('#define ${p}S_IRUSR 0400')
	b.writeln('#define ${p}S_IWUSR 0200')
	b.writeln('#define ${p}S_IXUSR 0100')
	b.writeln('#define ${p}S_IRGRP 040')
	b.writeln('#define ${p}S_IWGRP 020')
	b.writeln('#define ${p}S_IXGRP 010')
	b.writeln('#define ${p}S_IROTH 04')
	b.writeln('#define ${p}S_IWOTH 02')
	b.writeln('#define ${p}S_IXOTH 01')
	b.writeln('#define ${p}S_IFMT 0170000')
	b.writeln('#define ${p}S_IFDIR 0040000')
	b.writeln('#define ${p}S_IFREG 0100000')
	b.writeln('#define ${p}S_IFLNK 0120000')
	b.writeln('#define ${p}S_IFSOCK 0140000')
	b.writeln('#define ${p}S_IFIFO 0010000')
	b.writeln('#define ${p}S_IFCHR 0020000')
	b.writeln('#define ${p}S_IFBLK 0060000')
	b.writeln('#define ${p}F_GETFD 1')
	b.writeln('#define ${p}F_SETFD 2')
	b.writeln('#define ${p}FD_CLOEXEC 1')
	b.writeln('#define ${p}EINTR 4')
	b.writeln('#define ${p}ENOENT 2')
	b.writeln('#define ${p}EEXIST 17')
	b.writeln('#define ${p}SEEK_SET 0')
	b.writeln('#define ${p}SEEK_CUR 1')
	b.writeln('#define ${p}SEEK_END 2')
	b.writeln('#define ${p}EOF (-1)')
	b.writeln('#define ${p}_IOFBF 0')
	b.writeln('#define ${p}_IOLBF 1')
	b.writeln('#define ${p}_IONBF 2')
	b.writeln('#define ${p}PROT_READ 1')
	b.writeln('#define ${p}PROT_WRITE 2')
	b.writeln('#define ${p}MAP_PRIVATE 2')
	b.writeln('#define ${p}MAP_SHARED 1')
	b.writeln('#define ${p}MAP_FAILED ((void *)-1)')
	b.writeln('#define ${p}CLOCK_REALTIME 0')
	b.writeln('#define ${p}WNOHANG 1')
	b.writeln('#define ${p}STDIN_FILENO 0')
	b.writeln('#define ${p}STDOUT_FILENO 1')
	b.writeln('#define ${p}STDERR_FILENO 2')
	for f in fastc_c_abi_functions(target_os, target_arch) {
		// Only the function name takes the prefix: the test compiles these
		// prototypes against the real headers' types and compares them.
		mut line := f.prototype.replace('@', p + f.name)
		mut asm_label := f.asm_label
		if p != '' && asm_label == '' {
			// The prefixed prototypes of the test still bind to the real
			// symbols, so its runtime checks call the C library.
			asm_label = symbol_prefix + f.name
		}
		if asm_label != '' {
			line = line.trim_right(';') + ' __asm__("${asm_label}");'
		}
		b.writeln(line)
	}
	b.writeln('')
	return b.str()
}

// fastc_c_directive_quoted_include_path returns the path of an
// `#include "path"` directive line, or none for a system include.
fn fastc_c_directive_quoted_include_path(source string, start int, end int) ?string {
	line := source[start..end]
	open_quote := line.index('"') or { return none }
	close_quote := line.index_after_('"', open_quote + 1)
	if close_quote < 0 {
		return none
	}
	return line[open_quote + 1..close_quote]
}

// fastc_c_directive_is_include reports whether the directive line at
// source[start..end] is an `#include`.
@[direct_array_access]
fn fastc_c_directive_is_include(source string, start int, end int) bool {
	mut i := start
	if i >= end || source[i] != `#` {
		return false
	}
	i++
	for i < end && source[i] in [` `, `\t`] {
		i++
	}
	return end - i >= 7 && source[i] == `i` && source[i + 1] == `n` && source[i + 2] == `c`
		&& source[i + 3] == `l` && source[i + 4] == `u` && source[i + 5] == `d` && source[i + 6] == `e`
}
