module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#include <sys/syscall.h>

	fn bare_native_result(result u64) i64 {
		value := i64(result)
		if value < 0 && value >= -4095 {
			bf_native_errno_cell = i32(-value)
			return -1
		}
		return value
	}

	@[export: 'v_native_setpgid'; markused]
	fn bare_native_setpgid(pid i32, group i32) i32 {
		return i32(bare_native_result(sys_call2(u64(C.SYS_setpgid), u64(pid), u64(group))))
	}

	@[export: 'v_native_getpgid'; markused]
	fn bare_native_getpgid(pid i32) i32 {
		return i32(bare_native_result(sys_call1(u64(C.SYS_getpgid), u64(pid))))
	}

	@[export: 'v_native_getsid'; markused]
	fn bare_native_getsid(pid i32) i32 {
		return i32(bare_native_result(sys_call1(u64(C.SYS_getsid), u64(pid))))
	}

	@[export: 'v_native_eaccess'; markused]
	fn bare_native_eaccess(path uint, mode i32) i32 {
		return bare_native_faccessat(-100, path, mode, 512)
	}

	@[export: 'v_native_usleep'; markused]
	fn bare_native_usleep(microseconds u32) i32 {
		request := [i64(microseconds / 1000000), i64(microseconds % 1000000) * 1000]!
		return i32(bare_native_result(u64(bare_cancel_syscall(i64(C.SYS_nanosleep), uint(&request[0]), 0, 0, 0, 0, 0))))
	}

	@[export: 'v_native_dup'; markused]
	fn bare_native_dup(fd i32) i32 {
		return i32(bare_native_result(sys_call1(u64(C.SYS_dup), u64(fd))))
	}

	@[export: 'v_native_dup3'; markused]
	fn bare_native_dup3(oldfd i32, newfd i32, flags i32) i32 {
		return i32(bare_native_result(sys_call3(u64(C.SYS_dup3), u64(oldfd), u64(newfd), u64(flags))))
	}

	@[export: 'v_native_accept'; markused]
	fn bare_native_accept(fd i32, address uint, size uint) i32 {
		return i32(bare_native_result(u64(bare_cancel_syscall(i64(C.SYS_accept4), uint(fd), address, size, 0, 0, 0))))
	}

	@[export: 'v_native_getpeername'; markused]
	fn bare_native_getpeername(fd i32, address uint, size uint) i32 {
		return i32(bare_native_result(sys_call3(u64(C.SYS_getpeername), u64(fd), u64(address), u64(size))))
	}

	@[export: 'v_native_chdir'; markused]
	fn bare_native_chdir(path uint) i32 {
		return i32(bare_native_result(sys_call1(u64(C.SYS_chdir), u64(path))))
	}

	@[export: 'v_native_fchdir'; markused]
	fn bare_native_fchdir(fd i32) i32 {
		return i32(bare_native_result(sys_call1(u64(C.SYS_fchdir), u64(fd))))
	}

	@[export: 'v_native_fchmod'; markused]
	fn bare_native_fchmod(fd i32, mode u32) i32 {
		return i32(bare_native_result(sys_call2(u64(C.SYS_fchmod), u64(fd), u64(mode))))
	}

	@[export: 'v_native_fchown'; markused]
	fn bare_native_fchown(fd i32, user u32, group u32) i32 {
		return i32(bare_native_result(sys_call3(u64(C.SYS_fchown), u64(fd), u64(user), u64(group))))
	}

	@[export: 'v_native_chown'; markused]
	fn bare_native_chown(path uint, user u32, group u32) i32 {
		return i32(bare_native_result(sys_call5(u64(C.SYS_fchownat), u64(i64(-100)), u64(path), u64(user), u64(group), u64(0))))
	}

	@[export: 'v_native_lchown'; markused]
	fn bare_native_lchown(path uint, user u32, group u32) i32 {
		return i32(bare_native_result(sys_call5(u64(C.SYS_fchownat), u64(i64(-100)), u64(path), u64(user), u64(group), u64(256))))
	}

	@[export: 'v_native_fsync'; markused]
	fn bare_native_fsync(fd i32) i32 {
		return i32(bare_native_result(u64(bare_cancel_syscall(i64(C.SYS_fsync), uint(fd), 0, 0, 0, 0, 0))))
	}

	@[export: 'v_native_fdatasync'; markused]
	fn bare_native_fdatasync(fd i32) i32 {
		return i32(bare_native_result(u64(bare_cancel_syscall(i64(C.SYS_fdatasync), uint(fd), 0, 0, 0, 0, 0))))
	}

	@[export: 'v_native_ftruncate'; markused]
	fn bare_native_ftruncate(fd i32, length i64) i32 {
		return i32(bare_native_result(sys_call2(u64(C.SYS_ftruncate), u64(fd), u64(length))))
	}

	@[export: 'v_native_pread'; markused]
	fn bare_native_pread(fd i32, output uint, count uint, offset i64) i64 {
		return bare_native_result(u64(bare_cancel_syscall(i64(C.SYS_pread64), uint(fd), output, count, uint(offset), 0, 0)))
	}

	@[export: 'v_native_umask'; markused]
	fn bare_native_umask(mode u32) u32 {
		return u32(bare_native_result(sys_call1(u64(C.SYS_umask), u64(mode))))
	}

	@[export: 'v_native_unlinkat'; markused]
	fn bare_native_unlinkat(fd i32, path uint, flags i32) i32 {
		return i32(bare_native_result(sys_call3(u64(C.SYS_unlinkat), u64(fd), u64(path), u64(flags))))
	}

	@[export: 'v_native_linkat'; markused]
	fn bare_native_linkat(oldfd i32, oldpath uint, newfd i32, newpath uint, flags i32) i32 {
		return i32(bare_native_result(sys_call5(u64(C.SYS_linkat), u64(oldfd), u64(oldpath), u64(newfd), u64(newpath), u64(flags))))
	}

	@[export: 'v_native_readlinkat'; markused]
	fn bare_native_readlinkat(fd i32, path uint, buffer uint, size uint) i64 {
		return i64(bare_native_result(sys_call4(u64(C.SYS_readlinkat), u64(fd), u64(path), u64(buffer), u64(size))))
	}

	@[export: 'v_native_mknodat'; markused]
	fn bare_native_mknodat(fd i32, path uint, mode u32, device u64) i32 {
		return i32(bare_native_result(sys_call4(u64(C.SYS_mknodat), u64(fd), u64(path), u64(mode), u64(device))))
	}

	@[export: 'v_native_mknod'; markused]
	fn bare_native_mknod(path uint, mode u32, device u64) i32 {
		return i32(bare_native_result(sys_call4(u64(C.SYS_mknodat), u64(i64(-100)), u64(path), u64(mode), u64(device))))
	}

	@[export: 'v_native_mkfifo'; markused]
	fn bare_native_mkfifo(path uint, mode u32) i32 {
		return i32(bare_native_result(sys_call4(u64(C.SYS_mknodat), u64(i64(-100)), u64(path), u64((mode & 4095) | 4096), u64(0))))
	}

	@[export: 'v_native_flock'; markused]
	fn bare_native_flock(fd i32, operation i32) i32 {
		return i32(bare_native_result(sys_call2(u64(C.SYS_flock), u64(fd), u64(operation))))
	}

	@[export: 'v_native_utimensat'; markused]
	fn bare_native_utimensat(fd i32, path uint, times uint, flags i32) i32 {
		return i32(bare_native_result(sys_call4(u64(C.SYS_utimensat), u64(fd), u64(path), u64(times), u64(flags))))
	}

	@[export: 'v_native_futimens'; markused]
	fn bare_native_futimens(fd i32, times uint) i32 {
		return i32(bare_native_result(sys_call4(u64(C.SYS_utimensat), u64(fd), u64(0), u64(times), u64(0))))
	}

	@[export: 'v_native_madvise'; markused]
	fn bare_native_madvise(address uint, size uint, advice i32) i32 {
		return i32(bare_native_result(sys_call3(u64(C.SYS_madvise), u64(address), u64(size), u64(advice))))
	}

	@[export: 'v_native_mlock'; markused]
	fn bare_native_mlock(address uint, size uint) i32 {
		return i32(bare_native_result(sys_call2(u64(C.SYS_mlock), u64(address), u64(size))))
	}

	@[export: 'v_native_munlock'; markused]
	fn bare_native_munlock(address uint, size uint) i32 {
		return i32(bare_native_result(sys_call2(u64(C.SYS_munlock), u64(address), u64(size))))
	}

	@[export: 'v_native_getrlimit'; markused]
	fn bare_native_getrlimit(resource i32, limit uint) i32 {
		return i32(bare_native_result(sys_call4(u64(C.SYS_prlimit64), u64(0), u64(resource), u64(0), u64(limit))))
	}

	@[export: 'v_native_setrlimit'; markused]
	fn bare_native_setrlimit(resource i32, limit uint) i32 {
		return i32(bare_native_result(sys_call4(u64(C.SYS_prlimit64), u64(0), u64(resource), u64(limit), u64(0))))
	}

	@[export: 'v_native_gettid'; markused]
	fn bare_native_gettid() i32 {
		return i32(bare_native_result(sys_call0(u64(C.SYS_gettid))))
	}

	@[export: 'v_native_sched_yield'; markused]
	fn bare_native_sched_yield() i32 {
		return i32(bare_native_result(sys_call0(u64(C.SYS_sched_yield))))
	}

	@[export: 'v_native_gettimeofday'; markused]
	fn bare_native_gettimeofday(stamp uint, zone uint) i32 {
		return i32(bare_native_result(sys_call2(u64(C.SYS_gettimeofday), u64(stamp), u64(zone))))
	}

	@[export: 'v_native_waitpid'; markused]
	fn bare_native_waitpid(pid i32, status uint, options i32) i32 {
		return bare_native_wait4(pid, status, options, 0)
	}
	@[export: 'v_native_wait4'; markused]
	fn bare_native_wait4(pid i32, status uint, options i32, usage uint) i32 {
		return i32(bare_native_result(u64(bare_cancel_syscall(i64(C.SYS_wait4), uint(pid), status, uint(options), usage, 0, 0))))
	}
	@[export: 'v_native_waitid'; markused]
	fn bare_native_waitid(kind i32, id u32, info uint, options i32) i32 {
		// Linux amd64/arm64 use the native 128-byte siginfo_t layout. The fifth
		// syscall argument is an optional rusage, absent from the POSIX API.
		return i32(bare_native_result(u64(bare_cancel_syscall(i64(C.SYS_waitid), uint(kind), id, info, uint(options), 0, 0))))
	}

	@[export: 'v_native_getxattr'; markused]
	fn bare_native_getxattr(path uint, name uint, value uint, size uint) i64 {
		return i64(bare_native_result(sys_call4(u64(C.SYS_getxattr), u64(path), u64(name), u64(value), u64(size))))
	}

	@[export: 'v_native_lgetxattr'; markused]
	fn bare_native_lgetxattr(path uint, name uint, value uint, size uint) i64 {
		return i64(bare_native_result(sys_call4(u64(C.SYS_lgetxattr), u64(path), u64(name), u64(value), u64(size))))
	}

	@[export: 'v_native_fgetxattr'; markused]
	fn bare_native_fgetxattr(fd i32, name uint, value uint, size uint) i64 {
		return i64(bare_native_result(sys_call4(u64(C.SYS_fgetxattr), u64(fd), u64(name), u64(value), u64(size))))
	}

	@[export: 'v_native_setxattr'; markused]
	fn bare_native_setxattr(path uint, name uint, value uint, size uint, flags i32) i32 {
		return i32(bare_native_result(sys_call5(u64(C.SYS_setxattr), u64(path), u64(name), u64(value), u64(size), u64(flags))))
	}

	@[export: 'v_native_lsetxattr'; markused]
	fn bare_native_lsetxattr(path uint, name uint, value uint, size uint, flags i32) i32 {
		return i32(bare_native_result(sys_call5(u64(C.SYS_lsetxattr), u64(path), u64(name), u64(value), u64(size), u64(flags))))
	}

	@[export: 'v_native_fsetxattr'; markused]
	fn bare_native_fsetxattr(fd i32, name uint, value uint, size uint, flags i32) i32 {
		return i32(bare_native_result(sys_call5(u64(C.SYS_fsetxattr), u64(fd), u64(name), u64(value), u64(size), u64(flags))))
	}

	@[export: 'v_native_listxattr'; markused]
	fn bare_native_listxattr(path uint, list uint, size uint) i64 {
		return i64(bare_native_result(sys_call3(u64(C.SYS_listxattr), u64(path), u64(list), u64(size))))
	}

	@[export: 'v_native_llistxattr'; markused]
	fn bare_native_llistxattr(path uint, list uint, size uint) i64 {
		return i64(bare_native_result(sys_call3(u64(C.SYS_llistxattr), u64(path), u64(list), u64(size))))
	}

	@[export: 'v_native_flistxattr'; markused]
	fn bare_native_flistxattr(fd i32, list uint, size uint) i64 {
		return i64(bare_native_result(sys_call3(u64(C.SYS_flistxattr), u64(fd), u64(list), u64(size))))
	}

	@[export: 'v_native_removexattr'; markused]
	fn bare_native_removexattr(path uint, name uint) i32 {
		return i32(bare_native_result(sys_call2(u64(C.SYS_removexattr), u64(path), u64(name))))
	}

	@[export: 'v_native_lremovexattr'; markused]
	fn bare_native_lremovexattr(path uint, name uint) i32 {
		return i32(bare_native_result(sys_call2(u64(C.SYS_lremovexattr), u64(path), u64(name))))
	}

	@[export: 'v_native_fremovexattr'; markused]
	fn bare_native_fremovexattr(fd i32, name uint) i32 {
		return i32(bare_native_result(sys_call2(u64(C.SYS_fremovexattr), u64(fd), u64(name))))
	}

	@[export: 'v_native_recvmsg'; markused]
	fn bare_native_recvmsg(fd i32, message uint, flags i32) i64 {
		return i64(bare_native_result(sys_call3(u64(C.SYS_recvmsg), u64(fd), u64(message), u64(flags))))
	}

	@[export: 'v_native_sendmsg'; markused]
	fn bare_native_sendmsg(fd i32, message uint, flags i32) i64 {
		return i64(bare_native_result(sys_call3(u64(C.SYS_sendmsg), u64(fd), u64(message), u64(flags))))
	}

	@[export: 'v_native_recvmmsg'; markused]
	fn bare_native_recvmmsg(fd i32, messages uint, count u32, flags i32, timeout uint) i32 {
		return i32(bare_native_result(sys_call5(u64(C.SYS_recvmmsg), u64(fd), u64(messages), u64(count), u64(flags), u64(timeout))))
	}

	@[export: 'v_native_sendmmsg'; markused]
	fn bare_native_sendmmsg(fd i32, messages uint, count u32, flags i32) i32 {
		return i32(bare_native_result(sys_call4(u64(C.SYS_sendmmsg), u64(fd), u64(messages), u64(count), u64(flags))))
	}

	@[export: 'v_time_clock_getres'; markused]
	fn bare_native_clock_getres(clock i32, output uint) i32 {
		return i32(bare_native_result(sys_call2(u64(C.SYS_clock_getres), u64(clock), u64(output))))
	}
}
