module builtin

// NetBSD bare runtime: kernel syscall layer for `-freestanding`.
// Numbers verified against NetBSD sys/sys/syscall.h (single master table,
// shared by amd64/aarch64). Error convention: carry flag set on error,
// errno in rax/x0.

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
		eidrm           = 82
		enomsg          = 83
		eoverflow       = 84
		eilseq          = 85
		ecanceled       = 87
		ebadmsg         = 88
		emultihop       = 94
		enolink         = 95
	}

	const nb_nr_exit = u64(1)
	const nb_nr_fork = u64(2)
	const nb_nr_read = u64(3)
	const nb_nr_write = u64(4)
	const nb_nr_open = u64(5)
	const nb_nr_close = u64(6)
	const nb_nr_unlink = u64(10)
	const nb_nr_chdir = u64(12)
	const nb_nr_chmod = u64(15)
	const nb_nr_getpid = u64(20)
	const nb_nr_getuid = u64(24)
	const nb_nr_geteuid = u64(25)
	const nb_nr_recvmsg = u64(27)
	const nb_nr_sendmsg = u64(28)
	const nb_nr_recvfrom = u64(29)
	const nb_nr_accept = u64(30)
	const nb_nr_getpeername = u64(31)
	const nb_nr_getsockname = u64(32)
	const nb_nr_kill = u64(37)
	const nb_nr_getppid = u64(39)
	const nb_nr_dup = u64(41)
	const nb_nr_pipe = u64(42)
	const nb_nr_getgid = u64(47)
	const nb_nr_ioctl = u64(54)
	const nb_nr_symlink = u64(57)
	const nb_nr_readlink = u64(58)
	const nb_nr_execve = u64(59)
	const nb_nr_umask = u64(60)
	const nb_nr_vfork = u64(66)
	const nb_nr_munmap = u64(73)
	const nb_nr_mprotect = u64(74)
	const nb_nr_madvise = u64(75)
	const nb_nr_setpgid = u64(82)
	const nb_nr_dup2 = u64(90)
	const nb_nr_fcntl = u64(92)
	const nb_nr_fsync = u64(95)
	const nb_nr_connect = u64(98)
	const nb_nr_bind = u64(104)
	const nb_nr_setsockopt = u64(105)
	const nb_nr_listen = u64(106)
	const nb_nr_getsockopt = u64(118)
	const nb_nr_rename = u64(270)
	const nb_nr_flock = u64(131)
	const nb_nr_sendto = u64(133)
	const nb_nr_shutdown = u64(134)
	const nb_nr_socketpair = u64(135)
	const nb_nr_mkdir = u64(136)
	const nb_nr_rmdir = u64(137)
	const nb_nr_getrlimit = u64(194)
	const nb_nr_setrlimit = u64(195)
	const nb_nr_mmap = u64(197)
	const nb_nr_lseek = u64(199)
	const nb_nr_truncate = u64(200)
	const nb_nr_ftruncate = u64(201)
	const nb_nr_sysctl = u64(202)
	const nb_nr_fdatasync = u64(241)
	const nb_nr_clone = u64(287)
	const nb_nr_getcwd = u64(296)
	const nb_nr_lwp_exit = u64(310)
	const nb_nr_lwp_self = u64(311)
	const nb_nr_lwp_detach = u64(319)
	const nb_nr_lwp_kill = u64(318)
	const nb_nr_sched_yield = u64(350)
	const nb_nr_mremap = u64(411)
	const nb_nr_clock_gettime = u64(427)
	const nb_nr_nanosleep = u64(430)
	const nb_nr_stat = u64(439)
	const nb_nr_fstat = u64(440)
	const nb_nr_lstat = u64(441)
	const nb_nr_pipe2 = u64(453)
	const nb_nr_renameat = u64(458)
	const nb_nr_mkdirat = u64(461)
	const nb_nr_faccessat = u64(462)
	const nb_nr_fchmodat = u64(463)
	const nb_nr_fstatat = u64(466)
	const nb_nr_openat = u64(468)
	const nb_nr_readlinkat = u64(469)
	const nb_nr_unlinkat = u64(471)
	const nb_nr_wait4 = u64(449)
	const nb_nr_wait6 = u64(481)
	const nb_nr_getdents = u64(390)
	const nb_nr_socket = u64(394)

	// __clone flags (sys/sys/sched.h).
	const nb_clone_vm = u64(0x100)
	const nb_clone_fs = u64(0x200)
	const nb_clone_files = u64(0x400)
	const nb_clone_sighand = u64(0x1700)

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
				mov x17, scn
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
				  x17
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
				mov x17, scn
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
				  x17
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
				mov x17, scn
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
				  x17
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
				mov x17, scn
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
				  x17
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
				mov x17, scn
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
				  x17
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
				mov x17, scn
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
				  x17
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
				mov x17, scn
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
				  x17
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

	// sys_call7 includes the padded mmap offset required by this kernel ABI.
	fn sys_call7(scn u64, arg1 u64, arg2 u64, arg3 u64, arg4 u64, arg5 u64, arg6 u64, arg7 u64) (u64, u64) {
		mut res := u64(0)
		mut err := u64(0)
		$if arm64 {
			asm volatile arm64 {
				mov x17, scn
				mov x0, arg1
				mov x1, arg2
				mov x2, arg3
				mov x3, arg4
				mov x4, arg5
				mov x5, arg6
				mov x6, arg7
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
				  r (arg7)
				; x0
				  x1
				  x2
				  x3
				  x4
				  x5
				  x6
				  x17
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
				mov r11, arg7
				// The kernel reads argument seven at rsp+8. Keep this below the C red zone.
				sub rsp, 144
				mov [rsp + 8], r11
				syscall
				mov res, rax
				mov err, 0
				jnc '1f'
				mov err, rax
				1:
				add rsp, 144
				; =r (res)
				  =r (err)
				; r (scn)
				  D (arg1)
				  S (arg2)
				  g (arg3)
				  g (arg4)
				  g (arg5)
				  g (arg6)
				  r (arg7)
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
			mov x17, 1
			svc 0
			.popsection
		}
	}
}
