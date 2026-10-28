module builtin

// DragonFly BSD bare runtime: kernel syscall layer for `-freestanding`.
// Numbers verified against DragonFlyBSD sys/sys/syscall.h (x86_64 only).
// Error convention: carry flag set on error, errno in rax.

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
		ecanceled       = 85
		eilseq          = 86
		ebadmsg         = 89
		emultihop       = 90
		enolink         = 91
		eproto          = 92
		enotrecoverable = 94
		eownerdead      = 95
	}

	const df_nr_exit = u64(1)
	const df_nr_fork = u64(2)
	const df_nr_read = u64(3)
	const df_nr_write = u64(4)
	const df_nr_open = u64(5)
	const df_nr_close = u64(6)
	const df_nr_wait4 = u64(7)
	const df_nr_unlink = u64(10)
	const df_nr_chdir = u64(12)
	const df_nr_mknod = u64(14)
	const df_nr_chmod = u64(15)
	const df_nr_getpid = u64(20)
	const df_nr_getuid = u64(24)
	const df_nr_geteuid = u64(25)
	const df_nr_recvmsg = u64(27)
	const df_nr_sendmsg = u64(28)
	const df_nr_recvfrom = u64(29)
	const df_nr_accept = u64(30)
	const df_nr_getpeername = u64(31)
	const df_nr_getsockname = u64(32)
	const df_nr_kill = u64(37)
	const df_nr_getppid = u64(39)
	const df_nr_dup = u64(41)
	const df_nr_pipe = u64(42)
	const df_nr_getgid = u64(47)
	const df_nr_sigaltstack = u64(53)
	const df_nr_ioctl = u64(54)
	const df_nr_symlink = u64(57)
	const df_nr_readlink = u64(58)
	const df_nr_execve = u64(59)
	const df_nr_umask = u64(60)
	const df_nr_vfork = u64(66)
	const df_nr_munmap = u64(73)
	const df_nr_mprotect = u64(74)
	const df_nr_madvise = u64(75)
	const df_nr_setpgid = u64(82)
	const df_nr_dup2 = u64(90)
	const df_nr_fcntl = u64(92)
	const df_nr_fsync = u64(95)
	const df_nr_socket = u64(97)
	const df_nr_connect = u64(98)
	const df_nr_bind = u64(104)
	const df_nr_setsockopt = u64(105)
	const df_nr_listen = u64(106)
	const df_nr_gettimeofday = u64(116)
	const df_nr_getsockopt = u64(118)
	const df_nr_rename = u64(128)
	const df_nr_flock = u64(131)
	const df_nr_sendto = u64(133)
	const df_nr_shutdown = u64(134)
	const df_nr_socketpair = u64(135)
	const df_nr_mkdir = u64(136)
	const df_nr_rmdir = u64(137)
	const df_nr_getrlimit = u64(194)
	const df_nr_setrlimit = u64(195)
	const df_nr_mmap = u64(197)
	const df_nr_lseek = u64(199)
	const df_nr_truncate = u64(200)
	const df_nr_ftruncate = u64(201)
	const df_nr_sysctl = u64(202)
	const df_nr_clock_gettime = u64(232)
	const df_nr_nanosleep = u64(240)
	const df_nr_sched_yield = u64(331)
	const df_nr_sigprocmask = u64(340)
	const df_nr_sigaction = u64(342)
	const df_nr_getcwd = u64(326)
	const df_nr_stat = u64(475)
	const df_nr_fstat = u64(476)
	const df_nr_lstat = u64(477)
	const df_nr_getdirentries = u64(479)
	const df_nr_getdents = u64(480)
	const df_nr_extexit = u64(494)
	const df_nr_lwp_create = u64(495)
	const df_nr_lwp_gettid = u64(496)
	const df_nr_openat = u64(504)
	const df_nr_fstatat = u64(505)
	const df_nr_fchmodat = u64(506)
	const df_nr_unlinkat = u64(508)
	const df_nr_faccessat = u64(509)
	const df_nr_renameat = u64(523)
	const df_nr_mkdirat = u64(524)
	const df_nr_readlinkat = u64(527)
	const df_nr_pipe2 = u64(538)
	const df_nr_accept4 = u64(541)
	// SYS_getrandom (sys/sys/syscall.h).
	const df_nr_getrandom = u64(550)

	// extexit modes (sys/sys/unistd.h): EXTEXIT_LWP | EXTEXIT_SIMPLE.
	const df_extexit_lwp = u64(0x10000)

	// struct lwp_params for lwp_create (sys/sys/lwp.h).
	struct DfLwpParams {
		lwp_func  voidptr
		lwp_arg   voidptr
		lwp_stack voidptr
		lwp_tid1  &i64
		lwp_tid2  &i64
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
		asm volatile amd64 {
			syscall
			mov res, rax
			xor err, err
			jnc bsd0_done
			mov err, rax
			bsd0_done:
			; =r (res)
			  =r (err)
			; 0 (scn)
			; rcx
			  r11
			  memory
		}
		return res, err
	}

	fn sys_call1(scn u64, arg1 u64) (u64, u64) {
		mut res := u64(0)
		mut err := u64(0)
		asm volatile amd64 {
			syscall
			mov res, rax
			xor err, err
			jnc bsd1_done
			mov err, rax
			bsd1_done:
			; =r (res)
			  =r (err)
			; 0 (scn)
			  D (arg1)
			; rcx
			  r11
			  memory
		}
		return res, err
	}

	fn sys_call2(scn u64, arg1 u64, arg2 u64) (u64, u64) {
		mut res := u64(0)
		mut err := u64(0)
		asm volatile amd64 {
			syscall
			mov res, rax
			xor err, err
			jnc bsd2_done
			mov err, rax
			bsd2_done:
			; =r (res)
			  =r (err)
			; 0 (scn)
			  D (arg1)
			  S (arg2)
			; rcx
			  r11
			  memory
		}
		return res, err
	}

	fn sys_call3(scn u64, arg1 u64, arg2 u64, arg3 u64) (u64, u64) {
		mut res := u64(0)
		mut err := u64(0)
		asm volatile amd64 {
			syscall
			mov res, rax
			xor err, err
			jnc bsd3_done
			mov err, rax
			bsd3_done:
			; =r (res)
			  =r (err)
			; 0 (scn)
			  D (arg1)
			  S (arg2)
			  d (arg3)
			; rcx
			  r11
			  memory
		}
		return res, err
	}

	fn sys_call4(scn u64, arg1 u64, arg2 u64, arg3 u64, arg4 u64) (u64, u64) {
		mut res := u64(0)
		mut err := u64(0)
		asm volatile amd64 {
			mov r10, arg4
			syscall
			mov res, rax
			xor err, err
			jnc bsd4_done
			mov err, rax
			bsd4_done:
			; =r (res)
			  =r (err)
			; 0 (scn)
			  D (arg1)
			  S (arg2)
			  d (arg3)
			  g (arg4)
			; rcx
			  r10
			  r11
			  memory
		}
		return res, err
	}

	fn sys_call5(scn u64, arg1 u64, arg2 u64, arg3 u64, arg4 u64, arg5 u64) (u64, u64) {
		mut res := u64(0)
		mut err := u64(0)
		asm volatile amd64 {
			mov r10, arg4
			mov r8, arg5
			syscall
			mov res, rax
			xor err, err
			jnc bsd5_done
			mov err, rax
			bsd5_done:
			; =r (res)
			  =r (err)
			; 0 (scn)
			  D (arg1)
			  S (arg2)
			  d (arg3)
			  g (arg4)
			  g (arg5)
			; rcx
			  r10
			  r11
			  r8
			  memory
		}
		return res, err
	}

	fn sys_call6(scn u64, arg1 u64, arg2 u64, arg3 u64, arg4 u64, arg5 u64, arg6 u64) (u64, u64) {
		mut res := u64(0)
		mut err := u64(0)
		asm volatile amd64 {
			mov r10, arg4
			mov r8, arg5
			mov r9, arg6
			syscall
			mov res, rax
			xor err, err
			jnc bsd6_done
			mov err, rax
			bsd6_done:
			; =r (res)
			  =r (err)
			; 0 (scn)
			  D (arg1)
			  S (arg2)
			  d (arg3)
			  g (arg4)
			  g (arg5)
			  g (arg6)
			; rcx
			  r10
			  r11
			  r8
			  r9
			  memory
		}
		return res, err
	}

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
}
