module builtin

// OpenBSD bare runtime: kernel syscall layer for `-freestanding`.
// Numbers verified against OpenBSD sys/sys/syscall.h (MI table, shared by
// amd64/arm64). Error convention: carry flag set on error, errno in rax/x0.

$if !i386 {
	pub enum VlibcErrno {
		enoerror        = 0
		eperm           = 1
		enoent          = 2
		esrch           = 3
		eintr           = 4
		eio             = 5
		enxio           = 6
		e2big           = 7
		enoexec         = 8
		ebadf           = 9
		echild          = 10
		edeadlk         = 11
		enomem          = 12
		eacces          = 13
		efault          = 14
		enotblk         = 15
		ebusy           = 16
		eexist          = 17
		exdev           = 18
		enodev          = 19
		enotdir         = 20
		eisdir          = 21
		einval          = 22
		enfile          = 23
		emfile          = 24
		enotty          = 25
		etxtbsy         = 26
		efbig           = 27
		enospc          = 28
		espipe          = 29
		erofs           = 30
		emlink          = 31
		epipe           = 32
		edom            = 33
		erange          = 34
		eagain          = 35
		einprogress     = 36
		ealready        = 37
		enotsock        = 38
		edestaddrreq    = 39
		emsgsize        = 40
		eprototype      = 41
		enoprotoopt     = 42
		eprotonosupport = 43
		esocktnosupport = 44
		eopnotsupp      = 45
		epfnosupport    = 46
		eafnosupport    = 47
		eaddrinuse      = 48
		eaddrnotavail   = 49
		enetdown        = 50
		enetunreach     = 51
		enetreset       = 52
		econnaborted    = 53
		econnreset      = 54
		enobufs         = 55
		eisconn         = 56
		enotconn        = 57
		eshutdown       = 58
		etoomanyrefs    = 59
		etimedout       = 60
		econnrefused    = 61
		eloop           = 62
		enametoolong    = 63
		ehostdown       = 64
		ehostunreach    = 65
		enotempty       = 66
		eproclim        = 67
		eusers          = 68
		edquot          = 69
		estale          = 70
		eremote         = 71
		enolck          = 77
		enosys          = 78
		eilseq          = 84
		eoverflow       = 87
		ecanceled       = 88
		eidrm           = 89
		enomsg          = 90
		ebadmsg         = 92
		enotrecoverable = 93
		eownerdead      = 94
		eproto          = 95
	}

	const ob_nr_exit = u64(1)
	const ob_nr_fork = u64(2)
	const ob_nr_read = u64(3)
	const ob_nr_write = u64(4)
	const ob_nr_open = u64(5)
	const ob_nr_close = u64(6)
	const ob_nr_tfork = u64(8)
	const ob_nr_unlink = u64(10)
	const ob_nr_wait4 = u64(11)
	const ob_nr_chdir = u64(12)
	const ob_nr_chmod = u64(15)
	const ob_nr_getpid = u64(20)
	const ob_nr_getuid = u64(24)
	const ob_nr_geteuid = u64(25)
	const ob_nr_recvmsg = u64(27)
	const ob_nr_sendmsg = u64(28)
	const ob_nr_recvfrom = u64(29)
	const ob_nr_accept = u64(30)
	const ob_nr_getpeername = u64(31)
	const ob_nr_getsockname = u64(32)
	const ob_nr_stat = u64(38)
	const ob_nr_getppid = u64(39)
	const ob_nr_lstat = u64(40)
	const ob_nr_dup = u64(41)
	const ob_nr_fstatat = u64(42)
	const ob_nr_sigaction = u64(46)
	const ob_nr_getgid = u64(47)
	const ob_nr_sigprocmask = u64(48)
	const ob_nr_mmap = u64(49)
	const ob_nr_fstat = u64(53)
	const ob_nr_ioctl = u64(54)
	const ob_nr_symlink = u64(57)
	const ob_nr_readlink = u64(58)
	const ob_nr_execve = u64(59)
	const ob_nr_umask = u64(60)
	const ob_nr_vfork = u64(66)
	const ob_nr_gettimeofday = u64(67)
	const ob_nr_kevent = u64(72)
	const ob_nr_munmap = u64(73)
	const ob_nr_mprotect = u64(74)
	const ob_nr_madvise = u64(75)
	const ob_nr_setpgid = u64(82)
	const ob_nr_futex = u64(83)
	const ob_nr_clock_gettime = u64(87)
	const ob_nr_dup2 = u64(90)
	const ob_nr_nanosleep = u64(91)
	const ob_nr_fcntl = u64(92)
	const ob_nr_accept4 = u64(93)
	const ob_nr_fsync = u64(95)
	const ob_nr_socket = u64(97)
	const ob_nr_connect = u64(98)
	const ob_nr_getdents = u64(99)
	const ob_nr_pipe2 = u64(101)
	const ob_nr_dup3 = u64(102)
	const ob_nr_bind = u64(104)
	const ob_nr_setsockopt = u64(105)
	const ob_nr_listen = u64(106)
	const ob_nr_getsockopt = u64(118)
	const ob_nr_kill = u64(122)
	const ob_nr_rename = u64(128)
	const ob_nr_flock = u64(131)
	const ob_nr_sendto = u64(133)
	const ob_nr_shutdown = u64(134)
	const ob_nr_socketpair = u64(135)
	const ob_nr_mkdir = u64(136)
	const ob_nr_rmdir = u64(137)
	const ob_nr_waitid = u64(160)
	const ob_nr_lseek = u64(166)
	const ob_nr_truncate = u64(167)
	const ob_nr_ftruncate = u64(168)
	const ob_nr_getrlimit = u64(194)
	const ob_nr_setrlimit = u64(195)
	const ob_nr_sysctl = u64(202)
	const ob_nr_getcwd = u64(304)
	const ob_nr_sched_yield = u64(298)
	const ob_nr_sigaltstack = u64(288)
	const ob_nr_openat = u64(321)
	const ob_nr_readlinkat = u64(322)
	const ob_nr_renameat = u64(323)
	const ob_nr_unlinkat = u64(325)
	const ob_nr_mkdirat = u64(318)
	const ob_nr_faccessat = u64(313)
	const ob_nr_fchmodat = u64(314)
	const ob_nr_threxit = u64(302)

	// futex ops (same values as Linux).
	const ob_futex_wait = u64(0)
	const ob_futex_wake = u64(1)

	// struct __tfork for __tfork (sys/sys/unistd.h): tcb, tid, stack.
	struct ObTforkParam {
		tcb   voidptr
		tid   &i32
		stack voidptr
	}

	fn bsd_split(rc_in u64, er_in u64) (i64, VlibcErrno) {
		if er_in != 0 {
			return i64(-1), unsafe { VlibcErrno(er_in) }
		}
		return i64(rc_in), VlibcErrno.enoerror
	}

	fn sys_call0(scn u64) (u64, u64) {
		mut res := u64(0)
		mut err := u64(0)
		$if arm64 {
			asm volatile arm64 {
				mov x8, scn
				svc 0
				mov res, x0
				mov err, xzr
				b.cc '1f'
				mov err, x0
				1:
				; =r (res)
				  =r (err)
				; r (scn)
				; x0
				  x1
				  x8
				  cc
				  memory
			}
		} $else {
			asm volatile amd64 {
				mov rax, scn
				syscall
				mov res, rax
				mov err, 0
				jnc '1f'
				mov err, rax
				1:
				; =r (res)
				  =r (err)
				; r (scn)
				; rax
				  rdx
				  rcx
				  r11
				  cc
				  memory
			}
		}
		return res, err
	}

	fn sys_call1(scn u64, arg1 u64) (u64, u64) {
		mut res := u64(0)
		mut err := u64(0)
		$if arm64 {
			asm volatile arm64 {
				mov x8, scn
				mov x0, arg1
				svc 0
				mov res, x0
				mov err, xzr
				b.cc '1f'
				mov err, x0
				1:
				; =r (res)
				  =r (err)
				; r (scn)
				  r (arg1)
				; x0
				  x1
				  x8
				  cc
				  memory
			}
		} $else {
			asm volatile amd64 {
				mov rax, scn
				syscall
				mov res, rax
				mov err, 0
				jnc '1f'
				mov err, rax
				1:
				; =r (res)
				  =r (err)
				; r (scn)
				  D (arg1)
				; rax
				  rdx
				  rcx
				  r11
				  cc
				  memory
			}
		}
		return res, err
	}

	fn sys_call2(scn u64, arg1 u64, arg2 u64) (u64, u64) {
		mut res := u64(0)
		mut err := u64(0)
		$if arm64 {
			asm volatile arm64 {
				mov x8, scn
				mov x0, arg1
				mov x1, arg2
				svc 0
				mov res, x0
				mov err, xzr
				b.cc '1f'
				mov err, x0
				1:
				; =r (res)
				  =r (err)
				; r (scn)
				  r (arg1)
				  r (arg2)
				; x0
				  x1
				  x8
				  cc
				  memory
			}
		} $else {
			asm volatile amd64 {
				mov rax, scn
				syscall
				mov res, rax
				mov err, 0
				jnc '1f'
				mov err, rax
				1:
				; =r (res)
				  =r (err)
				; r (scn)
				  D (arg1)
				  S (arg2)
				; rax
				  rdx
				  rcx
				  r11
				  cc
				  memory
			}
		}
		return res, err
	}

	fn sys_call3(scn u64, arg1 u64, arg2 u64, arg3 u64) (u64, u64) {
		mut res := u64(0)
		mut err := u64(0)
		$if arm64 {
			asm volatile arm64 {
				mov x8, scn
				mov x0, arg1
				mov x1, arg2
				mov x2, arg3
				svc 0
				mov res, x0
				mov err, xzr
				b.cc '1f'
				mov err, x0
				1:
				; =r (res)
				  =r (err)
				; r (scn)
				  r (arg1)
				  r (arg2)
				  r (arg3)
				; x0
				  x1
				  x2
				  x8
				  cc
				  memory
			}
		} $else {
			asm volatile amd64 {
				mov rax, scn
				mov rdx, arg3
				syscall
				mov res, rax
				mov err, 0
				jnc '1f'
				mov err, rax
				1:
				; =r (res)
				  =r (err)
				; r (scn)
				  D (arg1)
				  S (arg2)
				  g (arg3)
				; rax
				  rdx
				  rcx
				  r11
				  cc
				  memory
			}
		}
		return res, err
	}

	fn sys_call4(scn u64, arg1 u64, arg2 u64, arg3 u64, arg4 u64) (u64, u64) {
		mut res := u64(0)
		mut err := u64(0)
		$if arm64 {
			asm volatile arm64 {
				mov x8, scn
				mov x0, arg1
				mov x1, arg2
				mov x2, arg3
				mov x3, arg4
				svc 0
				mov res, x0
				mov err, xzr
				b.cc '1f'
				mov err, x0
				1:
				; =r (res)
				  =r (err)
				; r (scn)
				  r (arg1)
				  r (arg2)
				  r (arg3)
				  r (arg4)
				; x0
				  x1
				  x2
				  x3
				  x8
				  cc
				  memory
			}
		} $else {
			asm volatile amd64 {
				mov rax, scn
				mov r10, arg4
				mov rdx, arg3
				syscall
				mov res, rax
				mov err, 0
				jnc '1f'
				mov err, rax
				1:
				; =r (res)
				  =r (err)
				; r (scn)
				  D (arg1)
				  S (arg2)
				  g (arg3)
				  g (arg4)
				; rax
				  rdx
				  rcx
				  r10
				  r11
				  cc
				  memory
			}
		}
		return res, err
	}

	fn sys_call5(scn u64, arg1 u64, arg2 u64, arg3 u64, arg4 u64, arg5 u64) (u64, u64) {
		mut res := u64(0)
		mut err := u64(0)
		$if arm64 {
			asm volatile arm64 {
				mov x8, scn
				mov x0, arg1
				mov x1, arg2
				mov x2, arg3
				mov x3, arg4
				mov x4, arg5
				svc 0
				mov res, x0
				mov err, xzr
				b.cc '1f'
				mov err, x0
				1:
				; =r (res)
				  =r (err)
				; r (scn)
				  r (arg1)
				  r (arg2)
				  r (arg3)
				  r (arg4)
				  r (arg5)
				; x0
				  x1
				  x2
				  x3
				  x4
				  x8
				  cc
				  memory
			}
		} $else {
			asm volatile amd64 {
				mov rax, scn
				mov r10, arg4
				mov r8, arg5
				mov rdx, arg3
				syscall
				mov res, rax
				mov err, 0
				jnc '1f'
				mov err, rax
				1:
				; =r (res)
				  =r (err)
				; r (scn)
				  D (arg1)
				  S (arg2)
				  g (arg3)
				  g (arg4)
				  g (arg5)
				; rax
				  rdx
				  rcx
				  r10
				  r11
				  r8
				  cc
				  memory
			}
		}
		return res, err
	}

	fn sys_call6(scn u64, arg1 u64, arg2 u64, arg3 u64, arg4 u64, arg5 u64, arg6 u64) (u64, u64) {
		mut res := u64(0)
		mut err := u64(0)
		$if arm64 {
			asm volatile arm64 {
				mov x8, scn
				mov x0, arg1
				mov x1, arg2
				mov x2, arg3
				mov x3, arg4
				mov x4, arg5
				mov x5, arg6
				svc 0
				mov res, x0
				mov err, xzr
				b.cc '1f'
				mov err, x0
				1:
				; =r (res)
				  =r (err)
				; r (scn)
				  r (arg1)
				  r (arg2)
				  r (arg3)
				  r (arg4)
				  r (arg5)
				  r (arg6)
				; x0
				  x1
				  x2
				  x3
				  x4
				  x5
				  x8
				  cc
				  memory
			}
		} $else {
			asm volatile amd64 {
				mov rax, scn
				mov r10, arg4
				mov r8, arg5
				mov r9, arg6
				mov rdx, arg3
				syscall
				mov res, rax
				mov err, 0
				jnc '1f'
				mov err, rax
				1:
				; =r (res)
				  =r (err)
				; r (scn)
				  D (arg1)
				  S (arg2)
				  g (arg3)
				  g (arg4)
				  g (arg5)
				  g (arg6)
				; rax
				  rdx
				  rcx
				  r10
				  r11
				  r8
				  r9
				  cc
				  memory
			}
		}
		return res, err
	}

	$if freestanding {
		$if amd64 {
			asm amd64 {
				.pushsection .text
				.globl _start
				_start:
				xor rbp, rbp
				mov rdi, [rsp]
				lea rsi, [rsp + 8]
				call main
				mov rdi, rax
				mov rax, 1
				syscall
				.popsection
			}
		} $else $if arm64 {
			asm arm64 {
				.pushsection .text
				.globl _start
				_start:
				mov x29, 0
				mov x30, 0
				ldr x0, [sp]
				add x1, sp, 8
				bl main
				mov x8, 1
				svc 0
				.popsection
			}
		}
	}
}
