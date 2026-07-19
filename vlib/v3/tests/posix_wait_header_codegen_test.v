import os

const wait_header_vexe = @VEXE
const wait_header_tests_dir = os.dir(@FILE)
const wait_header_v3_dir = os.dir(wait_header_tests_dir)
const wait_header_vlib_dir = os.dir(wait_header_v3_dir)
const wait_header_v3_src = os.join_path(wait_header_v3_dir, 'v3.v')

struct WaitHeaderProgram {
	c_code string
	out    string
}

fn wait_header_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_wait_header_test_${pid}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${wait_header_vexe} -gc none -path "${wait_header_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${wait_header_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn wait_header_compile(v3_bin string, name string, source string) WaitHeaderProgram {
	pid := os.getpid()
	src := os.join_path(os.temp_dir(), 'v3_wait_header_${name}_${pid}.v')
	out := os.join_path(os.temp_dir(), 'v3_wait_header_${name}_${pid}')
	os.write_file(src, source) or { panic(err) }
	os.rm(out) or {}
	os.rm(out + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	return WaitHeaderProgram{
		c_code: os.read_file(out + '.c') or { panic(err) }
		out:    out
	}
}

fn wait_header_gen_c(v3_bin string, name string, source string) string {
	pid := os.getpid()
	src := os.join_path(os.temp_dir(), 'v3_wait_header_${name}_${pid}.v')
	c_path := os.join_path(os.temp_dir(), 'v3_wait_header_${name}_${pid}.c')
	os.write_file(src, source) or { panic(err) }
	os.rm(c_path) or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${c_path}')
	assert compile.exit_code == 0, compile.output
	return os.read_file(c_path) or { panic(err) }
}

fn wait_header_has_include_directive(c_code string) bool {
	for line in c_code.split_into_lines() {
		trimmed := line.trim_space()
		if trimmed.starts_with('#include') {
			return true
		}
	}
	return false
}

fn test_os_import_uses_waitpid_without_headers() {
	$if windows {
		return
	}
	v3_bin := wait_header_build_v3()
	with_os := wait_header_compile(v3_bin, 'with_os_execute', "module main

import os

fn main() {
	result := os.execute('true')
	assert result.exit_code == 0
	stat_info := os.stat(@FILE) or { panic(err) }
	assert stat_info.size > 0
	uname_info := os.uname()
	assert uname_info.sysname.len > 0
	entries := os.ls(os.dir(@FILE)) or { panic(err) }
	usage := os.disk_usage(os.dir(@FILE)) or { panic(err) }
	os.signal_ignore(.pipe)
	signals_ok := C.SIGSTOP > 0 && C.SIGCONT > 0 && C.SIGTERM == 15 && C.SIGKILL == 9
	println('waitpid-ok')
	println((entries.len > 0 && usage.total > 0 && signals_ok).str())
}
")
	assert !wait_header_has_include_directive(with_os.c_code), with_os.c_code
	assert with_os.c_code.contains('waitpid('), with_os.c_code
	assert with_os.c_code.contains('int close(int fd);'), with_os.c_code
	assert with_os.c_code.contains('typedef _Bool bool;'), with_os.c_code
	assert with_os.c_code.contains('typedef unsigned char bool;'), with_os.c_code
	assert with_os.c_code.contains('#define __bool_true_false_are_defined 1'), with_os.c_code
	assert with_os.c_code.contains('#if !defined(__FILE_defined) && !defined(_FILE_DEFINED) && !defined(_FILEDEFED) && !defined(__DEFINED_FILE) && !defined(_FILE_DECLARED) && !defined(__FILE_DECLARED)'), with_os.c_code
	assert with_os.c_code.contains('typedef intptr_t ssize_t;'), with_os.c_code
	assert with_os.c_code.contains('int setenv(const char* name, const char* value, int overwrite);'), with_os.c_code
	assert with_os.c_code.contains('void abort(void);'), with_os.c_code
	assert with_os.c_code.contains('void* memset(void* s, int c, size_t n);'), with_os.c_code
	assert with_os.c_code.contains('void* memcpy(void* dest, const void* src, size_t n);'), with_os.c_code
	assert with_os.c_code.contains('void* memmove(void* dest, const void* src, size_t n);'), with_os.c_code
	assert with_os.c_code.contains('int memcmp(const void* s1, const void* s2, size_t n);'), with_os.c_code
	assert with_os.c_code.contains('size_t strlen(const char* s);'), with_os.c_code
	assert !with_os.c_code.contains('i32 strlen(char* s);'), with_os.c_code
	assert with_os.c_code.contains('int strcmp(const char* s1, const char* s2);'), with_os.c_code
	assert with_os.c_code.contains('int strncmp(const char* s1, const char* s2, size_t n);'), with_os.c_code
	assert with_os.c_code.contains('char* strncpy(char* dest, const char* src, size_t n);'), with_os.c_code
	assert with_os.c_code.contains('double floor(double x);'), with_os.c_code
	assert with_os.c_code.contains('double ceil(double x);'), with_os.c_code
	assert with_os.c_code.contains('float floorf(float x);'), with_os.c_code
	assert with_os.c_code.contains('float ceilf(float x);'), with_os.c_code
	assert with_os.c_code.contains('double sqrt(double x);'), with_os.c_code
	assert with_os.c_code.contains('double pow(double x, double y);'), with_os.c_code
	assert with_os.c_code.contains('double ldexp(double x, int exp);'), with_os.c_code
	assert with_os.c_code.contains('double fmod(double x, double y);'), with_os.c_code
	assert with_os.c_code.contains('double cos(double x);'), with_os.c_code
	assert with_os.c_code.contains('double acos(double x);'), with_os.c_code
	assert with_os.c_code.contains('double fabs(double x);'), with_os.c_code
	assert with_os.c_code.contains('int open(const char* path, int flags, ...);'), with_os.c_code
	assert with_os.c_code.contains('ssize_t read(int fd, void* buf, size_t count);'), with_os.c_code
	assert with_os.c_code.contains('int fork(void);'), with_os.c_code
	assert with_os.c_code.contains('int dup2(int oldfd, int newfd);'), with_os.c_code
	assert with_os.c_code.contains('int execlp(const char* file, const char* arg, ...);'), with_os.c_code
	assert with_os.c_code.contains('int execvp(const char* file, char* const argv[]);'), with_os.c_code
	assert with_os.c_code.contains('void _exit(int status);'), with_os.c_code
	ssize_typedef_idx := with_os.c_code.index('typedef intptr_t ssize_t;') or { -1 }
	read_proto_idx := with_os.c_code.index('ssize_t read(int fd, void* buf, size_t count);') or {
		-1
	}
	assert ssize_typedef_idx >= 0 && ssize_typedef_idx < read_proto_idx, with_os.c_code
	assert with_os.c_code.contains('int access(const char* path, int mode);'), with_os.c_code
	assert with_os.c_code.contains('char* realpath(const char* path, char* resolved_path);'), with_os.c_code
	assert with_os.c_code.contains('char* strrchr(const char* s, int c);'), with_os.c_code
	assert with_os.c_code.contains('char* strstr(const char* haystack, const char* needle);'), with_os.c_code
	assert with_os.c_code.contains('int snprintf(char* str, size_t size, const char* format, ...);'), with_os.c_code
	assert with_os.c_code.contains('#elif defined(__ANDROID__)'), with_os.c_code
	assert with_os.c_code.contains('int* __errno(void);'), with_os.c_code
	assert with_os.c_code.contains('#define errno (*__errno())'), with_os.c_code
	assert with_os.c_code.contains('int* __errno_location(void);'), with_os.c_code
	assert with_os.c_code.contains('#define errno (*__errno_location())'), with_os.c_code
	assert with_os.c_code.contains('#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__DragonFly__)'), with_os.c_code
	assert with_os.c_code.contains('#define stdout __stdoutp'), with_os.c_code
	assert with_os.c_code.contains('#elif defined(__OpenBSD__)'), with_os.c_code
	assert with_os.c_code.contains('extern struct __sFstub __stdout[];'), with_os.c_code
	assert with_os.c_code.contains('#define stdout ((FILE*)__stdout)'), with_os.c_code
	assert with_os.c_code.contains('#elif defined(__NetBSD__)'), with_os.c_code
	assert with_os.c_code.contains('struct __netbsd_FILE_stub { unsigned char _opaque[152]; };'), with_os.c_code
	assert with_os.c_code.contains('#define stdout ((FILE*)&__sF[1])'), with_os.c_code
	assert with_os.c_code.contains('typedef union { unsigned char _opaque[128]; long long _align; } pthread_mutex_t;'), with_os.c_code
	assert with_os.c_code.contains('typedef union { unsigned char _opaque[256]; long long _align; } pthread_rwlock_t;'), with_os.c_code
	assert with_os.c_code.contains('#define PTHREAD_MUTEX_INITIALIZER'), with_os.c_code
	assert with_os.c_code.contains('int pthread_mutex_lock(void* mutex);'), with_os.c_code
	assert with_os.c_code.contains('int pthread_mutex_unlock(void* mutex);'), with_os.c_code
	assert with_os.c_code.contains('typedef union { unsigned char _opaque[128]; long long _align; } sigset_t;'), with_os.c_code
	assert with_os.c_code.contains('struct winsize { unsigned short ws_row; unsigned short ws_col; unsigned short ws_xpixel; unsigned short ws_ypixel; };'), with_os.c_code
	assert with_os.c_code.contains('typedef union epoll_data { void* ptr; int fd; u32 u32; u64 u64; } epoll_data_t;'), with_os.c_code
	assert with_os.c_code.contains('struct epoll_event { u32 events; epoll_data_t data; } __attribute__((packed));'), with_os.c_code
	assert with_os.c_code.contains('#elif defined(__linux__) && (defined(__i386__) || defined(__arm__))'), with_os.c_code
	assert with_os.c_code.contains('struct dirent { unsigned long d_ino; long d_off; unsigned short d_reclen; unsigned char d_type; char d_name[256]; };'), with_os.c_code
	assert with_os.c_code.contains('struct dirent { u64 d_ino; i64 d_off; unsigned short d_reclen; unsigned char d_type; char d_name[256]; };'), with_os.c_code
	assert with_os.c_code.contains('struct dirent { u64 d_ino; u64 d_seekoff; u16 d_reclen; u16 d_namlen; u8 d_type; char d_name[1024]; };'), with_os.c_code
	assert with_os.c_code.contains('struct dirent { u64 d_ino; i64 d_seekoff; u16 d_reclen; u8 d_type; u8 __pad0; u16 d_namlen; u16 __pad1; char d_name[256]; };'), with_os.c_code
	assert with_os.c_code.contains('struct statvfs { unsigned long f_flag; unsigned long f_bsize; unsigned long f_frsize; unsigned long f_iosize; u64 f_blocks;'), with_os.c_code
	assert with_os.c_code.contains('char f_mntfromlabel[1024];'), with_os.c_code
	assert with_os.c_code.contains('struct statvfs { unsigned long f_bsize; unsigned long f_frsize; unsigned long f_blocks; unsigned long f_bfree; unsigned long f_bavail;'), with_os.c_code
	assert with_os.c_code.contains('#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)'), with_os.c_code
	assert with_os.c_code.contains('struct utsname { char sysname[256]; char nodename[256]; char release[256]; char version[256]; char machine[256]; };'), with_os.c_code
	assert with_os.c_code.contains('struct utsname { char sysname[65]; char nodename[65]; char release[65]; char version[65]; char machine[65]; char domainname[65]; };'), with_os.c_code
	assert with_os.c_code.contains('#if defined(__x86_64__) && !defined(__ILP32__)'), with_os.c_code
	assert with_os.c_code.contains('struct stat { u64 st_dev; u64 st_ino; u64 st_nlink; u32 st_mode; u32 st_uid; u32 st_gid; int __pad0; u64 st_rdev; i64 st_size; i64 st_blksize; i64 st_blocks; i64 st_atime; i64 st_atimensec; i64 st_mtime; i64 st_mtimensec; i64 st_ctime; i64 st_ctimensec; i64 __glibc_reserved[3]; };'), with_os.c_code
	assert with_os.c_code.contains('#elif defined(__aarch64__) || (defined(__riscv) && __riscv_xlen == 64) || defined(__loongarch_lp64)'), with_os.c_code
	assert with_os.c_code.contains('#elif defined(__i386__) || defined(__arm__)'), with_os.c_code
	assert with_os.c_code.contains('struct stat { u64 st_dev; unsigned short __pad1; unsigned long st_ino; u32 st_mode; unsigned long st_nlink;'), with_os.c_code
	assert with_os.c_code.contains('#error unsupported Linux struct stat layout for this architecture'), with_os.c_code
	assert with_os.c_code.contains('i32 st_atim_ext; i64 st_atime; long st_atimensec;'), with_os.c_code
	assert with_os.c_code.contains('struct stat { u64 st_dev; u64 st_ino; u64 st_nlink; u16 st_mode; i16 st_bsdflags;'), with_os.c_code
	assert with_os.c_code.contains('struct stat { u32 st_mode; i32 st_dev; u64 st_ino; u32 st_nlink;'), with_os.c_code
	assert with_os.c_code.contains('struct stat { u64 st_dev; u32 st_mode; u64 st_ino; u32 st_nlink;'), with_os.c_code
	assert with_os.c_code.contains('struct stat { u64 st_ino; u32 st_nlink; u32 st_dev; u16 st_mode;'), with_os.c_code
	assert with_os.c_code.contains('#elif defined(__sun)'), with_os.c_code
	assert with_os.c_code.contains('struct stat { u64 st_dev; u64 st_ino; u32 st_mode; u32 st_nlink; u32 st_uid; u32 st_gid;'), with_os.c_code
	assert with_os.c_code.contains('char st_fstype[16];'), with_os.c_code
	assert with_os.c_code.contains('#elif defined(__QNX__) || defined(__QNXNTO__)'), with_os.c_code
	assert with_os.c_code.contains('#if _FILE_OFFSET_BITS - 0 == 64'), with_os.c_code
	assert with_os.c_code.contains('struct stat { u64 st_ino; i64 st_size; u64 st_dev; u64 st_rdev;'), with_os.c_code
	assert with_os.c_code.contains('#elif defined(__BIGENDIAN__)'), with_os.c_code
	assert with_os.c_code.contains('#error unsupported headerless Unix struct stat layout for this platform'), with_os.c_code
	assert with_os.c_code.contains('int stat(const char* path, struct stat* buf);'), with_os.c_code
	assert !with_os.c_code.contains('i32 stat(char*, void*);'), with_os.c_code
	assert with_os.c_code.contains('i64 st_birthtime;'), with_os.c_code
	assert with_os.c_code.contains('struct rusage { struct timeval ru_utime; struct timeval ru_stime; long ru_maxrss; long ru_ixrss; long ru_idrss;'), with_os.c_code
	assert with_os.c_code.contains('#if !defined(_STRUCT_TIMESPEC) && !defined(_TIMESPEC_DEFINED) && !defined(_TIMESPEC_DECLARED) && !defined(__timespec_defined)'), with_os.c_code
	assert with_os.c_code.contains('typedef struct timespec timespec;'), with_os.c_code
	assert !with_os.c_code.contains('typedef struct stat stat;'), with_os.c_code
	assert !with_os.c_code.contains('typedef struct sigset_t sigset_t;'), with_os.c_code
	assert !with_os.c_code.contains('struct winsize {\n\tws_row'), with_os.c_code
	assert !with_os.c_code.contains('struct rusage {\n\tru_maxrss int'), with_os.c_code
	assert !with_os.c_code.contains('typedef union epoll_data_t epoll_data_t;'), with_os.c_code
	assert !with_os.c_code.contains('u64 d_seekoff;\n\tu64 d_reclen;'), with_os.c_code
	assert with_os.c_code.contains('#define SIGSTOP'), with_os.c_code
	assert with_os.c_code.contains('#define SIGCONT'), with_os.c_code
	assert with_os.c_code.contains('#define SIGTERM 15'), with_os.c_code
	assert with_os.c_code.contains('#define SIGKILL 9'), with_os.c_code
	assert with_os.c_code.contains('#define SIGPIPE 13'), with_os.c_code
	assert with_os.c_code.contains('#define SIG_IGN ((void*)1)'), with_os.c_code
	assert with_os.c_code.contains('#define SIG_BLOCK'), with_os.c_code
	assert with_os.c_code.contains('#define PTRACE_ATTACH 16'), with_os.c_code
	assert with_os.c_code.contains('#define PTRACE_DETACH 17'), with_os.c_code
	assert with_os.c_code.contains('#define PT_TRACE_ME 0'), with_os.c_code
	assert with_os.c_code.contains('#define PT_ATTACH 10'), with_os.c_code
	assert with_os.c_code.contains('#define PT_DETACH 11'), with_os.c_code
	assert with_os.c_code.contains('#define WNOHANG 1'), with_os.c_code
	assert with_os.c_code.contains('#define ENOENT 2'), with_os.c_code
	assert with_os.c_code.contains('#define RUSAGE_SELF 0'), with_os.c_code
	assert with_os.c_code.contains('#define EACCES 13'), with_os.c_code
	assert with_os.c_code.contains('#define EIO 5'), with_os.c_code
	assert with_os.c_code.contains('#define EBADF 9'), with_os.c_code
	assert with_os.c_code.contains('#define ENOMEM 12'), with_os.c_code
	assert with_os.c_code.contains('#define EFAULT 14'), with_os.c_code
	assert with_os.c_code.contains('#define ESPIPE 29'), with_os.c_code
	assert with_os.c_code.contains('#define EOVERFLOW 75'), with_os.c_code
	assert with_os.c_code.contains('#define _SC_PAGESIZE 0x0027'), with_os.c_code
	assert with_os.c_code.contains('#define _SC_NPROCESSORS_ONLN 0x0061'), with_os.c_code
	assert with_os.c_code.contains('#define _SC_PHYS_PAGES 0x0062'), with_os.c_code
	assert with_os.c_code.contains('#define _SC_AVPHYS_PAGES 0x0063'), with_os.c_code
	assert with_os.c_code.contains('#define _SC_PAGESIZE 30'), with_os.c_code
	assert with_os.c_code.contains('#define _SC_NPROCESSORS_ONLN 84'), with_os.c_code
	assert with_os.c_code.contains('#define _SC_PHYS_PAGES 85'), with_os.c_code
	assert with_os.c_code.contains('#define _SC_AVPHYS_PAGES 86'), with_os.c_code
	assert with_os.c_code.contains('#define PROT_READ 0x1'), with_os.c_code
	assert with_os.c_code.contains('#define PROT_WRITE 0x2'), with_os.c_code
	assert with_os.c_code.contains('#define PROT_EXEC 0x4'), with_os.c_code
	assert with_os.c_code.contains('#define MAP_PRIVATE 0x0002'), with_os.c_code
	assert with_os.c_code.contains('#define MAP_ANONYMOUS 0x20'), with_os.c_code
	assert with_os.c_code.contains('#define MAP_ANONYMOUS 0x1000'), with_os.c_code
	assert with_os.c_code.contains('#define MAP_FAILED ((void*)-1)'), with_os.c_code
	assert with_os.c_code.contains('#define SYS_getrandom 318'), with_os.c_code
	assert with_os.c_code.contains('#define SYS_getrandom 355'), with_os.c_code
	assert with_os.c_code.contains('#define SYS_getrandom 384'), with_os.c_code
	assert with_os.c_code.contains('#define SYS_getrandom 278'), with_os.c_code
	assert with_os.c_code.contains('#define CTL_KERN 1'), with_os.c_code
	assert with_os.c_code.contains('#define CTL_VM 2'), with_os.c_code
	assert with_os.c_code.contains('#define KERN_PROC_PATHNAME 12'), with_os.c_code
	assert with_os.c_code.contains('#define KERN_PROC_INC_THREAD 0x10'), with_os.c_code
	assert with_os.c_code.contains('#define KERN_PROC_ARGS 55'), with_os.c_code
	assert with_os.c_code.contains('#define KERN_PROC_ARGV 1'), with_os.c_code
	assert with_os.c_code.contains('#define VM_UVMEXP 4'), with_os.c_code
	assert with_os.c_code.contains('__atomic_fetch_add((uintptr_t*)ptr, (uintptr_t)0, 5)'), with_os.c_code
	assert with_os.c_code.contains('__atomic_load_n((void**)ptr, 5)'), with_os.c_code
	assert !with_os.c_code.contains('*(void* volatile*)ptr'), with_os.c_code
	assert with_os.c_code.contains('#define SOCK_NONBLOCK 04000'), with_os.c_code
	assert with_os.c_code.contains('#define SOCK_NONBLOCK 0x20000000'), with_os.c_code
	assert with_os.c_code.contains('#define SO_REUSEPORT 15'), with_os.c_code
	assert with_os.c_code.contains('#define TCP_QUICKACK 12'), with_os.c_code
	assert with_os.c_code.contains('#define TCP_DEFER_ACCEPT 9'), with_os.c_code
	assert with_os.c_code.contains('#define TCP_FASTOPEN 23'), with_os.c_code
	assert with_os.c_code.contains('#define SOMAXCONN 4096'), with_os.c_code
	assert with_os.c_code.contains('#define MEM_COMMIT 0x00001000U'), with_os.c_code
	assert with_os.c_code.contains('#define MEM_RESERVE 0x00002000U'), with_os.c_code
	assert with_os.c_code.contains('#define PAGE_READWRITE 0x04U'), with_os.c_code
	assert with_os.c_code.contains('#define PAGE_EXECUTE_READ 0x20U'), with_os.c_code
	assert with_os.c_code.contains('#define TLS_OUT_OF_INDEXES 0xffffffffU'), with_os.c_code
	assert with_os.c_code.contains('#define SOCKET_ERROR (-1)'), with_os.c_code
	assert with_os.c_code.contains('#define WSAEWOULDBLOCK 10035'), with_os.c_code
	run := os.execute(with_os.out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'waitpid-ok\ntrue', run.output

	hello := wait_header_compile(v3_bin, 'hello', "module main

fn main() {
	println('hello')
}
")
	assert !wait_header_has_include_directive(hello.c_code), hello.c_code
}

fn test_user_c_decl_emits_extern_prototype_without_headers() {
	$if windows {
		return
	}
	v3_bin := wait_header_build_v3()
	program := wait_header_compile(v3_bin, 'user_c_getppid', 'module main

#flag -Werror=implicit-function-declaration

fn C.getppid() int

fn main() {
	println(C.getppid().str())
}
')
	assert !wait_header_has_include_directive(program.c_code), program.c_code
	assert program.c_code.contains('int getppid(void);'), program.c_code
	run := os.execute(program.out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().int() > 0, run.output
}

fn test_windows_crt_underscore_c_decls_emit_extern_prototypes() {
	v3_bin := wait_header_build_v3()
	c_code := wait_header_gen_c(v3_bin, 'windows_crt_underscore_decls', 'module main

fn C._wfopen(&u16, &u16) voidptr
fn C._wsystem(&u16) int
fn C._wgetenv(&u16) voidptr
fn C._waccess(&u16, int) int
fn C._wchdir(&u16) int
fn C._chsize_s(voidptr, u64) int

fn main() {
	p := &u16(unsafe { nil })
	h := voidptr(0)
	_ = C._wfopen(p, p)
	_ = C._wsystem(p)
	_ = C._wgetenv(p)
	_ = C._waccess(p, 0)
	_ = C._wchdir(p)
	_ = C._chsize_s(h, u64(0))
}
')
	assert !wait_header_has_include_directive(c_code), c_code
	assert c_code.contains('#ifdef _MSC_VER'), c_code
	assert c_code.contains('typedef unsigned __int64 size_t;'), c_code
	assert c_code.contains('typedef __int64 ptrdiff_t;'), c_code
	assert c_code.contains('typedef unsigned __int64 uintptr_t;'), c_code
	assert c_code.contains('typedef __int64 intptr_t;'), c_code
	assert c_code.contains('#elif defined(_WIN32)'), c_code
	assert c_code.contains('int* _errno(void);'), c_code
	assert c_code.contains('#define errno (*_errno())'), c_code
	assert c_code.contains('void* _wfopen(u16*, u16*);'), c_code
	assert c_code.contains('int _wsystem(u16*);'), c_code
	assert c_code.contains('void* _wgetenv(u16*);'), c_code
	assert c_code.contains('int _waccess(u16*, int);'), c_code
	assert c_code.contains('int _wchdir(u16*);'), c_code
	assert c_code.contains('int _chsize_s(void*, u64);'), c_code
}

fn test_windows_sdk_types_are_emitted_before_extern_prototypes() {
	v3_bin := wait_header_build_v3()
	c_code := wait_header_gen_c(v3_bin, 'windows_sdk_type_order', 'module main

@[typedef]
struct C.SECURITY_ATTRIBUTES {}

fn C.CreateHardLinkW(&u16, &u16, &C.SECURITY_ATTRIBUTES) int

fn main() {
	path := &u16(unsafe { nil })
	attrs := &C.SECURITY_ATTRIBUTES(unsafe { nil })
	_ = C.CreateHardLinkW(path, path, attrs)
}
')
	security_typedef := 'typedef struct SECURITY_ATTRIBUTES { DWORD nLength; void* lpSecurityDescriptor; BOOL bInheritHandle; } SECURITY_ATTRIBUTES;'
	security_typedef_idx := c_code.index(security_typedef) or { -1 }
	prototype_idx := c_code.index('int WINAPI CreateHardLinkW(u16*, u16*, SECURITY_ATTRIBUTES*);') or {
		-1
	}
	assert security_typedef_idx >= 0, c_code
	assert prototype_idx >= 0, c_code
	assert security_typedef_idx < prototype_idx, c_code
	assert !c_code.contains('typedef struct SECURITY_ATTRIBUTES SECURITY_ATTRIBUTES;'), c_code
}

fn test_winapi_extern_prototypes_use_calling_convention() {
	v3_bin := wait_header_build_v3()
	c_code := wait_header_gen_c(v3_bin, 'winapi_calling_convention', 'module main

fn C.GetStdHandle(u32) voidptr
fn C.CreateFileW(&u16, u32, u32, voidptr, u32, u32, voidptr) voidptr

fn main() {
	path := &u16(unsafe { nil })
	_ = C.GetStdHandle(u32(0))
	_ = C.CreateFileW(path, 0, 0, voidptr(0), 0, 0, voidptr(0))
}
')
	assert c_code.contains('#ifndef WINAPI'), c_code
	assert c_code.contains('#define WINAPI __stdcall'), c_code
	assert c_code.contains('void* WINAPI GetStdHandle(u32);'), c_code
	assert c_code.contains('void* WINAPI CreateFileW(u16*, u32, u32, void*, u32, u32, void*);'), c_code
	assert !c_code.contains('void* GetStdHandle(u32);'), c_code
	assert !c_code.contains('void* CreateFileW(u16*, u32, u32, void*, u32, u32, void*);'), c_code
}

fn test_unsuffixed_winapi_decls_use_wide_exports() {
	v3_bin := wait_header_build_v3()
	c_code := wait_header_gen_c(v3_bin, 'winapi_unsuffixed_wide_exports', 'module main

fn C.GetModuleFileName(voidptr, &u16, u32) u32
fn C.CreateFile(&u16, u32, u32, voidptr, u32, u32, voidptr) voidptr
fn C.LoadLibrary(&u16) voidptr
fn C.DefWindowProc(voidptr, u32, usize, isize) isize

fn main() {
	path := &u16(unsafe { nil })
	_ = C.GetModuleFileName(voidptr(0), path, 0)
	_ = C.CreateFile(path, 0, 0, voidptr(0), 0, 0, voidptr(0))
	_ = C.LoadLibrary(path)
	_ = voidptr(&C.DefWindowProc)
}
')
	assert c_code.contains('u32 WINAPI GetModuleFileNameW(void*, u16*, u32);'), c_code
	assert c_code.contains('void* WINAPI CreateFileW(u16*, u32, u32, void*, u32, u32, void*);'), c_code
	assert c_code.contains('void* WINAPI LoadLibraryW(u16*);'), c_code
	assert c_code.contains('ptrdiff_t WINAPI DefWindowProcW(void*, u32, size_t, ptrdiff_t);'), c_code
	assert c_code.contains('GetModuleFileNameW('), c_code
	assert c_code.contains('CreateFileW('), c_code
	assert c_code.contains('LoadLibraryW('), c_code
	assert c_code.contains('&DefWindowProcW'), c_code
	assert !c_code.contains('GetModuleFileName('), c_code
	assert !c_code.contains('CreateFile('), c_code
	assert !c_code.contains('LoadLibrary('), c_code
	assert !c_code.contains('&DefWindowProc)'), c_code
}

fn test_synthesized_c_extern_prototypes_preserve_const_pointer_params() {
	v3_bin := wait_header_build_v3()
	c_code := wait_header_gen_c(v3_bin, 'c_extern_const_pointer_params', 'module main

fn C.SSL_CTX_load_verify_locations(ctx voidptr, const_file &char, const_ca_path &char) int

fn main() {
	path := &char(unsafe { nil })
	_ = C.SSL_CTX_load_verify_locations(voidptr(0), path, path)
}
')
	assert c_code.contains('int SSL_CTX_load_verify_locations(void* ctx, const char* const_file, const char* const_ca_path);'), c_code
	assert !c_code.contains('int SSL_CTX_load_verify_locations(void* ctx, char* const_file'), c_code
}

fn test_windows_sync_structs_use_headerless_preamble_storage() {
	v3_bin := wait_header_build_v3()
	c_code := wait_header_gen_c(v3_bin, 'windows_sync_struct_storage', 'module main

@[typedef]
struct C.SRWLOCK {}

@[typedef]
struct C.CONDITION_VARIABLE {}

struct SyncHolder {
	rw C.SRWLOCK
	cv C.CONDITION_VARIABLE
}

fn main() {
	_ := SyncHolder{}
}
')
	assert !wait_header_has_include_directive(c_code), c_code
	assert c_code.contains('typedef struct SRWLOCK { void* Ptr; } SRWLOCK;'), c_code
	assert c_code.contains('typedef struct CONDITION_VARIABLE { void* Ptr; } CONDITION_VARIABLE;'), c_code
	assert c_code.contains('SRWLOCK rw;'), c_code
	assert c_code.contains('CONDITION_VARIABLE cv;'), c_code
	assert !c_code.contains('typedef struct SRWLOCK SRWLOCK;'), c_code
	assert !c_code.contains('typedef struct CONDITION_VARIABLE CONDITION_VARIABLE;'), c_code
}

fn test_epoll_data_tag_uses_headerless_preamble_definition() {
	v3_bin := wait_header_build_v3()
	c_code := wait_header_gen_c(v3_bin, 'epoll_data_tag_storage', 'module main

@[typedef]
union C.epoll_data {
mut:
	ptr voidptr
	fd  int
	u32 u32
	u64 u64
}

@[packed]
struct C.epoll_event {
	events u32
	data   C.epoll_data
}

fn main() {
	_ := C.epoll_event{}
}
')
	assert c_code.contains('typedef union epoll_data { void* ptr; int fd; u32 u32; u64 u64; } epoll_data_t;'), c_code
	assert !c_code.contains('union epoll_data {\n'), c_code
}

fn test_windows_console_records_use_headerless_preamble_definitions() {
	v3_bin := wait_header_build_v3()
	c_code := wait_header_gen_c(v3_bin, 'windows_console_records', 'module main

pub union C.Event {
	KeyEvent              C.KEY_EVENT_RECORD
	MouseEvent            C.MOUSE_EVENT_RECORD
	WindowBufferSizeEvent C.WINDOW_BUFFER_SIZE_RECORD
	MenuEvent             C.MENU_EVENT_RECORD
	FocusEvent            C.FOCUS_EVENT_RECORD
}

@[typedef]
pub struct C.INPUT_RECORD {
	EventType u16
	Event     C.Event
}

pub union C.uChar {
mut:
	UnicodeChar rune
	AsciiChar   u8
}

@[typedef]
pub struct C.KEY_EVENT_RECORD {
	bKeyDown          int
	wRepeatCount      u16
	wVirtualKeyCode   u16
	wVirtualScanCode  u16
	uChar             C.uChar
	dwControlKeyState u32
}

@[typedef]
pub struct C.MOUSE_EVENT_RECORD {
	dwMousePosition   C.COORD
	dwButtonState     u32
	dwControlKeyState u32
	dwEventFlags      u32
}

@[typedef]
pub struct C.WINDOW_BUFFER_SIZE_RECORD {
	dwSize C.COORD
}

@[typedef]
pub struct C.MENU_EVENT_RECORD {
	dwCommandId u32
}

@[typedef]
pub struct C.FOCUS_EVENT_RECORD {
	bSetFocus int
}

@[typedef]
pub struct C.COORD {
	X i16
	Y i16
}

@[typedef]
pub struct C.SMALL_RECT {
	Left   u16
	Top    u16
	Right  u16
	Bottom u16
}

@[typedef]
pub struct C.CONSOLE_SCREEN_BUFFER_INFO {
	dwSize              C.COORD
	dwCursorPosition    C.COORD
	wAttributes         u16
	srWindow            C.SMALL_RECT
	dwMaximumWindowSize C.COORD
}

@[typedef]
pub struct C.CHAR_INFO {
	Char       C.uChar
	Attributes u16
}

fn main() {
	_ := C.INPUT_RECORD{}
	_ := C.CHAR_INFO{}
}
')
	assert c_code.contains('typedef union uChar { u16 UnicodeChar; u8 AsciiChar; } uChar;'), c_code
	assert c_code.contains('typedef struct KEY_EVENT_RECORD { int bKeyDown; u16 wRepeatCount; u16 wVirtualKeyCode; u16 wVirtualScanCode; uChar uChar; u32 dwControlKeyState; } KEY_EVENT_RECORD;'), c_code
	assert c_code.contains('typedef struct INPUT_RECORD { u16 EventType; Event Event; } INPUT_RECORD;'), c_code
	assert !c_code.contains('union uChar {\n'), c_code
	assert !c_code.contains('struct uChar'), c_code
	assert !c_code.contains('struct KEY_EVENT_RECORD {\n'), c_code
}

fn test_time_import_uses_platform_tm_layout_without_headers() {
	$if windows {
		return
	}
	v3_bin := wait_header_build_v3()
	program := wait_header_compile(v3_bin, 'time_tm_layout', 'module main

import time

fn main() {
	now := time.now()
	println((now.year > 0).str())
}
')
	assert !wait_header_has_include_directive(program.c_code), program.c_code
	assert program.c_code.contains('struct tm { int tm_sec; int tm_min; int tm_hour; int tm_mday; int tm_mon; int tm_year; int tm_wday; int tm_yday; int tm_isdst; long tm_gmtoff; const char* tm_zone; };'), program.c_code
	assert program.c_code.contains('typedef struct tm tm;'), program.c_code
	assert !program.c_code.contains('int tm_gmtoff;'), program.c_code
	run := os.execute(program.out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'true', run.output
}

fn test_term_import_uses_platform_termios_layout_without_headers() {
	$if windows {
		return
	}
	v3_bin := wait_header_build_v3()
	program := wait_header_compile(v3_bin, 'term_termios', 'module main

import term
import term.termios

fn main() {
	mut t := termios.Termios{}
	t.disable_echo()
	width, height := term.get_terminal_size()
	println((width > 0 && height > 0).str())
}
')
	assert !wait_header_has_include_directive(program.c_code), program.c_code
	assert program.c_code.contains('struct termios { size_t c_iflag; size_t c_oflag; size_t c_cflag; size_t c_lflag; u8 c_cc[20]; size_t c_ispeed; size_t c_ospeed; };'), program.c_code
	assert program.c_code.contains('struct termios { int c_iflag; int c_oflag; int c_cflag; int c_lflag; u8 c_line; u8 c_cc[32]; int c_ispeed; int c_ospeed; };'), program.c_code
	assert program.c_code.contains('struct termios { int c_iflag; int c_oflag; int c_cflag; int c_lflag; u8 c_cc[20]; };'), program.c_code
	assert program.c_code.contains('struct termios { int c_iflag; int c_oflag; int c_cflag; int c_lflag; u8 c_cc[20]; u32 reserved[3]; int c_ispeed; int c_ospeed; };'), program.c_code
	assert program.c_code.contains('struct termios { int c_iflag; int c_oflag; int c_cflag; int c_lflag; u8 c_cc[20]; int c_ispeed; int c_ospeed; };'), program.c_code
	assert program.c_code.contains('#define VMIN'), program.c_code
	assert program.c_code.contains('#define TIOCGWINSZ'), program.c_code
	run := os.execute(program.out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'true', run.output
}

fn test_filelock_uses_headerless_fcntl_helpers() {
	$if windows {
		return
	}
	v3_bin := wait_header_build_v3()
	program := wait_header_compile(v3_bin, 'filelock_helpers', "module main

import os

fn C.open(&char, i32, i32) i32
fn C.close(i32) i32
fn C.v_filelock_lock(i32, i32, i32, u64, u64) i32
fn C.v_filelock_unlock(i32, u64, u64) i32

fn main() {
	path := os.join_path(os.temp_dir(), 'v3_headerless_filelock_target')
	os.write_file(path, 'abcdef') or { panic(err) }
	fd := C.open(&char(path.str), C.O_RDWR, 0)
	println(fd)
	lock_result := C.v_filelock_lock(fd, 1, 1, u64(0), u64(3))
	unlock_result := C.v_filelock_unlock(fd, u64(0), u64(3))
	println(lock_result)
	println(unlock_result)
	C.close(fd)
	os.rm(path) or {}
}
")
	assert !wait_header_has_include_directive(program.c_code), program.c_code
	assert program.c_code.contains('#define LOCK_EX 2'), program.c_code
	assert program.c_code.contains('int open(const char* path, int flags, ...);'), program.c_code
	assert !program.c_code.contains('int open(char*'), program.c_code
	assert program.c_code.contains('#elif defined(_WIN32)'), program.c_code
	assert program.c_code.contains('#define GENERIC_READ 0x80000000U'), program.c_code
	assert program.c_code.contains('#define OPEN_ALWAYS 4'), program.c_code
	assert program.c_code.contains('static inline int v_filelock_lock(int fd, int exclusive, int immediate, u64 start, u64 len) { struct flock fl;'), program.c_code
	assert program.c_code.contains('return fcntl(fd, immediate ? F_SETLK : F_SETLKW, &fl);'), program.c_code
	assert program.c_code.contains('typedef struct SECURITY_ATTRIBUTES { DWORD nLength; void* lpSecurityDescriptor; BOOL bInheritHandle; } SECURITY_ATTRIBUTES;'), program.c_code
	assert program.c_code.contains('BOOL LockFileEx(HANDLE handle, DWORD flags, DWORD reserved, DWORD low, DWORD high, OVERLAPPED* overlap);'), program.c_code
	assert program.c_code.contains('return LockFileEx(handle, flags, 0, low, high, &overlap) ? 0 : -1;'), program.c_code
	assert program.c_code.contains('return UnlockFileEx(handle, 0, low, high, &overlap) ? 0 : -1;'), program.c_code
	run := os.execute(program.out)
	assert run.exit_code == 0, run.output
	lines := run.output.trim_space().split_into_lines()
	assert lines.len == 3, run.output
	assert lines[0].int() != -1, run.output
	assert lines[1] == '0', run.output
	assert lines[2] == '0', run.output
}

fn test_net_import_uses_headerless_socket_constants() {
	$if windows {
		return
	}
	v3_bin := wait_header_build_v3()
	program := wait_header_compile(v3_bin, 'net_socket_constants', 'module main

import net

fn C.getaddrinfo(&char, &char, &C.addrinfo, &&C.addrinfo) int
fn C.freeaddrinfo(&C.addrinfo)

fn main() {
	println((int(net.SocketType.tcp) > 0).str())
	println((int(net.AddrFamily.ip) > 0).str())
	host := "127.0.0.1"
	service := "80"
	mut hints := C.addrinfo{}
	unsafe { vmemset(&hints, 0, int(sizeof(hints))) }
	hints.ai_family = C.AF_INET
	hints.ai_socktype = C.SOCK_STREAM
	hints.ai_flags = C.AI_PASSIVE
	mut results := &C.addrinfo(unsafe { nil })
	code := C.getaddrinfo(&char(host.str), &char(service.str), &hints, &results)
	if code != 0 {
		println("false")
		return
	}
	ok := !isnil(results) && results.ai_family == C.AF_INET
	C.freeaddrinfo(results)
	println(ok.str())
}
')
	assert !wait_header_has_include_directive(program.c_code), program.c_code
	assert program.c_code.contains('typedef unsigned short wchar_t;'), program.c_code
	assert program.c_code.contains('typedef unsigned int wchar_t;'), program.c_code
	assert program.c_code.contains('typedef uintptr_t SOCKET;'), program.c_code
	assert program.c_code.contains('struct fd_set { unsigned int fd_count; SOCKET fd_array[FD_SETSIZE]; };'), program.c_code
	assert program.c_code.contains('static inline void v_fd_set(SOCKET fd, fd_set* set)'), program.c_code
	assert program.c_code.contains('#elif defined(__APPLE__)'), program.c_code
	assert program.c_code.contains('#define __V_FD_BITS 32'), program.c_code
	assert program.c_code.contains('struct fd_set { unsigned int fds_bits[FD_SETSIZE / __V_FD_BITS]; };'), program.c_code
	assert program.c_code.contains('struct fd_set { unsigned long fds_bits[FD_SETSIZE / __V_FD_BITS]; };'), program.c_code
	assert program.c_code.contains('struct kevent { uintptr_t ident; u32 filter; u32 flags; u32 fflags; i64 data; void* udata; u64 ext[4]; };'), program.c_code
	assert program.c_code.contains('struct kevent { uintptr_t ident; i16 filter; u16 flags; u32 fflags; intptr_t data; void* udata; };'), program.c_code
	assert program.c_code.contains('struct winsize { unsigned short ws_row; unsigned short ws_col; unsigned short ws_xpixel; unsigned short ws_ypixel; };'), program.c_code
	assert program.c_code.contains('struct addrinfo { int ai_flags; int ai_family; int ai_socktype; int ai_protocol; size_t ai_addrlen; char* ai_canonname; void* ai_addr; struct addrinfo* ai_next; };'), program.c_code
	assert program.c_code.contains('struct addrinfo { int ai_flags; int ai_family; int ai_socktype; int ai_protocol; unsigned int ai_addrlen; char* ai_canonname; void* ai_addr; struct addrinfo* ai_next; };'), program.c_code
	assert program.c_code.contains('struct addrinfo { int ai_flags; int ai_family; int ai_socktype; int ai_protocol; unsigned int ai_addrlen; void* ai_addr; char* ai_canonname; struct addrinfo* ai_next; };'), program.c_code
	assert program.c_code.contains('typedef struct addrinfo addrinfo;'), program.c_code
	assert !program.c_code.contains('int ai_family; int ai_socktype; int ai_flags;'), program.c_code
	assert program.c_code.contains('struct sockaddr { u8 sa_len; u8 sa_family; char sa_data[14]; };'), program.c_code
	assert program.c_code.contains('struct sockaddr_in { u8 sin_len; u8 sin_family; u16 sin_port; u32 sin_addr; char sin_zero[8]; };'), program.c_code
	assert program.c_code.contains('struct sockaddr_in6 { u8 sin6_len; u8 sin6_family; u16 sin6_port; u32 sin6_flowinfo; u8 sin6_addr[16]; u32 sin6_scope_id; };'), program.c_code
	assert program.c_code.contains('struct sockaddr { u16 sa_family; char sa_data[14]; };'), program.c_code
	assert program.c_code.contains('struct sockaddr_in { u16 sin_family; u16 sin_port; u32 sin_addr; char sin_zero[8]; };'), program.c_code
	assert program.c_code.contains('struct sockaddr_in6 { u16 sin6_family; u16 sin6_port; u32 sin6_flowinfo; u8 sin6_addr[16]; u32 sin6_scope_id; };'), program.c_code
	assert !program.c_code.contains('struct sockaddr_in6 {\n\tu16 sin6_family;\n\tu16 sin6_port;\n\tu32 sin6_addr[4];'), program.c_code
	assert program.c_code.contains('#define FLT_EPSILON 1.19209290e-7F'), program.c_code
	assert program.c_code.contains('#define DBL_EPSILON 2.2204460492503131e-16'), program.c_code
	assert program.c_code.contains('#define FLT_MAX __FLT_MAX__'), program.c_code
	assert program.c_code.contains('#define DBL_MAX __DBL_MAX__'), program.c_code
	assert program.c_code.contains('#define KEY_EVENT 0x0001'), program.c_code
	assert program.c_code.contains('#define MOUSE_MOVED 0x0001'), program.c_code
	assert program.c_code.contains('#define DOUBLE_CLICK 0x0002'), program.c_code
	assert program.c_code.contains('#define MOUSE_WHEELED 0x0004'), program.c_code
	assert program.c_code.contains('#define VK_BACK 0x08'), program.c_code
	assert program.c_code.contains('#define VK_RETURN 0x0d'), program.c_code
	assert program.c_code.contains('struct timeval { long tv_sec; long tv_usec; };'), program.c_code
	assert program.c_code.contains('typedef struct timeval timeval;'), program.c_code
	assert program.c_code.contains('struct rusage { struct timeval ru_utime; struct timeval ru_stime; long ru_maxrss; long ru_ixrss; long ru_idrss;'), program.c_code
	assert !program.c_code.contains('u64 tv_sec;'), program.c_code
	assert program.c_code.contains('struct timespec { i64 tv_sec; long tv_nsec; };'), program.c_code
	assert program.c_code.contains('struct timespec { long tv_sec; long tv_nsec; };'), program.c_code
	assert program.c_code.contains('typedef struct timespec timespec;'), program.c_code
	assert !program.c_code.contains('struct timespec {\n\ttv_sec'), program.c_code
	assert program.c_code.contains('#define F_GETFL 3'), program.c_code
	assert program.c_code.contains('#define F_SETFL 4'), program.c_code
	assert program.c_code.contains('#define SOCK_STREAM 1'), program.c_code
	assert program.c_code.contains('#define AF_INET 2'), program.c_code
	assert program.c_code.contains('#define SOL_SOCKET'), program.c_code
	assert program.c_code.contains('#define EV_SET(kevp, a, b, c, d, e, f) do'), program.c_code
	assert program.c_code.contains('#define EVFILT_READ (-1)'), program.c_code
	assert program.c_code.contains('#define EVFILT_MACHPORT (-8)'), program.c_code
	assert program.c_code.contains('#define EV_ADD 0x0001'), program.c_code
	assert program.c_code.contains('#define EV_CLEAR 0x0020'), program.c_code
	assert program.c_code.contains('#define EV_ERROR 0x4000'), program.c_code
	assert program.c_code.contains('#elif defined(__FreeBSD__)'), program.c_code
	assert program.c_code.contains('#define O_CLOEXEC 0x00100000'), program.c_code
	assert program.c_code.contains('#define F_SETLK 12'), program.c_code
	assert program.c_code.contains('#define AF_INET6 28'), program.c_code
	assert program.c_code.contains('#elif defined(__OpenBSD__)'), program.c_code
	assert program.c_code.contains('#define O_CLOEXEC 0x10000'), program.c_code
	assert program.c_code.contains('#define AF_INET6 24'), program.c_code
	assert program.c_code.contains('#elif defined(__sun)'), program.c_code
	assert program.c_code.contains('#define O_NONBLOCK 0x80'), program.c_code
	assert program.c_code.contains('#define SOCK_STREAM 2'), program.c_code
	assert program.c_code.contains('#define AI_PASSIVE 0x0008'), program.c_code
	assert program.c_code.contains('#elif defined(__QNX__) || defined(__QNXNTO__)'), program.c_code

	assert program.c_code.contains('#define O_NONBLOCK 000200'), program.c_code
	assert program.c_code.contains('#define EINPROGRESS 236'), program.c_code
	assert program.c_code.contains('#elif defined(__linux__) || defined(__ANDROID__)'), program.c_code
	assert program.c_code.contains('#define EPOLLIN 0x001'), program.c_code
	assert program.c_code.contains('#define EPOLLET (1U << 31)'), program.c_code
	assert program.c_code.contains('#define EPOLL_CTL_ADD 1'), program.c_code

	assert program.c_code.contains('#error unsupported headerless C platform constants'), program.c_code

	assert program.c_code.contains('#define TCP_NODELAY 1'), program.c_code
	run := os.execute(program.out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'true\ntrue\ntrue', run.output
}

// The same system header may appear under different guards. v3 drops system
// includes in both contexts while preserving the surrounding C directives.
fn test_same_system_include_under_different_guards_is_dropped() {
	$if windows {
		return
	}
	v3_bin := wait_header_build_v3()
	c_code := wait_header_gen_c(v3_bin, 'dup_guarded_include', "module main

#ifdef __linux__
#include <sys/v3dup_guard.h>
#endif
#ifdef __APPLE__
#include <sys/v3dup_guard.h>
#endif

fn main() {
	println('ok')
}
")
	assert !wait_header_has_include_directive(c_code), c_code
	assert c_code.count('#include <sys/v3dup_guard.h>') == 0, c_code
	assert c_code.contains('#ifdef __linux__\n#endif'), c_code
	assert c_code.contains('#ifdef __APPLE__\n#endif'), c_code
}
