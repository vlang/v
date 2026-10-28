module builtin

// Solaris/Illumos bare runtime: kernel syscall layer for `-freestanding`.
// Numbers verified against illumos-gate usr/src/uts/common/sys/syscall.h.
// Error convention: carry is set and rax contains errno on failure.

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
		edeadlk         = 45
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
		eagain          = 11
		einprogress     = 150
		ealready        = 149
		enotsock        = 95
		edestaddrreq    = 96
		emsgsize        = 97
		eprototype      = 98
		enoprotoopt     = 99
		eprotonosupport = 120
		esocktnosupport = 121
		eopnotsupp      = 122
		epfnosupport    = 123
		eafnosupport    = 124
		eaddrinuse      = 125
		eaddrnotavail   = 126
		enetdown        = 127
		enetunreach     = 128
		enetreset       = 129
		econnaborted    = 130
		econnreset      = 131
		enobufs         = 132
		eisconn         = 133
		enotconn        = 134
		eshutdown       = 143
		etoomanyrefs    = 144
		etimedout       = 145
		econnrefused    = 146
		eloop           = 90
		enametoolong    = 78
		ehostdown       = 147
		ehostunreach    = 148
		enotempty       = 93

		eusers          = 94
		edquot          = 49
		estale          = 151
		eremote         = 66
		enolck          = 46
		enosys          = 89
		eidrm           = 36
		enomsg          = 35
		eoverflow       = 79
		ecanceled       = 47
		eilseq          = 88
		ebadmsg         = 77
		emultihop       = 74
		enolink         = 67
		eproto          = 71
		enotrecoverable = 59
		eownerdead      = 58
	}

	const sol_nr_exit = u64(1)
	const sol_nr_fork = u64(2)
	const sol_nr_read = u64(3)
	const sol_nr_write = u64(4)
	const sol_nr_open = u64(5)
	const sol_nr_close = u64(6)
	const sol_nr_unlink = u64(10)
	const sol_nr_chdir = u64(12)
	const sol_nr_chmod = u64(15)
	const sol_nr_stat = u64(18)
	const sol_nr_lseek = u64(19)
	const sol_nr_getpid = u64(20)
	const sol_nr_getuid = u64(24)
	const sol_nr_kill = u64(37)
	const sol_nr_fstat = u64(28)
	const sol_nr_statfs = u64(35)
	const sol_nr_fstatfs = u64(38)
	const sol_nr_pipe = u64(42)
	const sol_nr_faccessat = u64(45)
	const sol_nr_getgid = u64(47)
	const sol_nr_ioctl = u64(54)
	const sol_nr_umask = u64(60)
	const sol_nr_fcntl = u64(62)
	const sol_nr_renameat = u64(64)
	const sol_nr_unlinkat = u64(65)
	const sol_nr_fstatat = u64(66)
	const sol_nr_fstatat64 = u64(67)
	const sol_nr_openat = u64(68)
	const sol_nr_openat64 = u64(69)
	const sol_nr_rmdir = u64(79)
	const sol_nr_mkdir = u64(80)
	const sol_nr_getdents = u64(81)
	const sol_nr_symlink = u64(89)
	const sol_nr_readlink = u64(90)
	const sol_nr_sigprocmask = u64(95)
	const sol_nr_sigaltstack = u64(97)
	const sol_nr_sigaction = u64(98)
	const sol_nr_fchmodat = u64(101)
	const sol_nr_mkdirat = u64(102)
	const sol_nr_statvfs = u64(103)
	const sol_nr_fstatvfs = u64(104)
	const sol_nr_waitid = u64(107)
	const sol_nr_mincore = u64(114)
	const sol_nr_mmap = u64(115)
	const sol_nr_mprotect = u64(116)
	const sol_nr_munmap = u64(117)
	const sol_nr_setrlimit = u64(128)
	const sol_nr_getrlimit = u64(129)
	const sol_nr_rename = u64(134)
	const sol_nr_forksys = u64(142)
	const sol_nr_sched_yield = u64(146)
	const sol_nr_gettimeofday = u64(156)
	const sol_nr_pollsys = u64(183)
	const sol_nr_clock_gettime = u64(191)
	const sol_nr_nanosleep = u64(199)
	const sol_nr_getdents64 = u64(213)
	const sol_nr_mmap64 = u64(214)
	const sol_nr_stat64 = u64(215)
	const sol_nr_lstat64 = u64(216)
	const sol_nr_fstat64 = u64(217)
	const sol_nr_statvfs64 = u64(218)
	const sol_nr_fstatvfs64 = u64(219)
	const sol_nr_setrlimit64 = u64(220)
	const sol_nr_getrlimit64 = u64(221)
	const sol_nr_open64 = u64(225)
	const sol_nr_getcwd = u64(229)
	const sol_nr_bind = u64(232)
	const sol_nr_listen = u64(233)
	const sol_nr_accept = u64(234)
	const sol_nr_connect = u64(235)
	const sol_nr_shutdown = u64(236)
	const sol_nr_recvfrom = u64(238)
	const sol_nr_sendto = u64(242)
	const sol_nr_getpeername = u64(243)
	const sol_nr_getsockname = u64(244)
	const sol_nr_getsockopt = u64(245)
	const sol_nr_setsockopt = u64(246)
	const sol_nr_socket = u64(247)
	const sol_nr_socketpair = u64(248)
	const sol_nr_lwp_park = u64(77)
	const sol_nr_lwp_create = u64(159)
	const sol_nr_lwp_exit = u64(160)
	const sol_nr_lwp_self = u64(164)
	const sol_nr_lwp_kill = u64(163)
	const sol_nr_lwp_wait = u64(167)
	const sol_nr_lwp_suspend = u64(161)
	const sol_nr_lwp_continue = u64(162)

	// O_ flags (sys/fcntl.h).
	const sol_o_rdonly = u64(0)
	const sol_o_wronly = u64(1)
	const sol_o_rdwr = u64(2)
	const sol_o_append = u64(0x08)
	const sol_o_sync = u64(0x10)
	const sol_o_nonblock = u64(0x80)
	const sol_o_creat = u64(0x100)
	const sol_o_trunc = u64(0x200)
	const sol_o_excl = u64(0x400)
	const sol_o_noctty = u64(0x800)
	const sol_o_nofollow = u64(0x20000)
	const sol_o_cloexec = u64(0x800000)
	const sol_o_directory = u64(0x1000000)

	// MAP/PROT flags (sys/mman.h).
	const sol_map_shared = u64(1)
	const sol_map_private = u64(2)
	const sol_map_anon = u64(0x100)
	const sol_prot_read = u64(1)
	const sol_prot_write = u64(2)
	const sol_prot_exec = u64(4)

	// AT_FDCWD magic value (sys/fcntl.h).
	const sol_at_fdcwd = u64(0xffd19553)
	const sol_at_removedir = u64(1)

	// lwp_create flags (sys/lwp.h).
	const sol_lwp_detached = u64(0x00000040)

	// Solaris amd64 uses syscall, with carry indicating an errno in rax.

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
		return res, err
	}

	// getpid/getuid/getgid return the parent/effective ID in the second register.
	fn sol_sys_call0_secondary(scn u64) (u64, u64) {
		_, second, er := sol_sys_call0_pair(scn)
		return second, er
	}

	fn sol_sys_call0_pair(scn u64) (u64, u64, u64) {
		mut first := u64(0)
		mut second := u64(0)
		mut err := u64(0)
		asm volatile amd64 {
			mov rax, scn
			xor rdi, rdi
			xor rsi, rsi
			syscall
			mov first, rax
			mov second, rdx
			mov err, 0
			jnc '1f'
			mov err, rax
			1:
			; =r (first)
			  =r (second)
			  =r (err)
			; r (scn)
			; rax
			  rdi
			  rsi
			  rdx
			  rcx
			  r11
			  cc
			  memory
		}
		return first, second, err
	}

	fn sys_call1(scn u64, arg1 u64) (u64, u64) {
		mut res := u64(0)
		mut err := u64(0)
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
		return res, err
	}

	fn sys_call2(scn u64, arg1 u64, arg2 u64) (u64, u64) {
		mut res := u64(0)
		mut err := u64(0)
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
		return res, err
	}

	fn sys_call3(scn u64, arg1 u64, arg2 u64, arg3 u64) (u64, u64) {
		mut res := u64(0)
		mut err := u64(0)
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
		return res, err
	}

	fn sys_call4(scn u64, arg1 u64, arg2 u64, arg3 u64, arg4 u64) (u64, u64) {
		mut res := u64(0)
		mut err := u64(0)
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
		return res, err
	}

	fn sys_call5(scn u64, arg1 u64, arg2 u64, arg3 u64, arg4 u64, arg5 u64) (u64, u64) {
		mut res := u64(0)
		mut err := u64(0)
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
		return res, err
	}

	fn sys_call6(scn u64, arg1 u64, arg2 u64, arg3 u64, arg4 u64, arg5 u64, arg6 u64) (u64, u64) {
		mut res := u64(0)
		mut err := u64(0)
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
		return res, err
	}

	// Solaris ucontext for lwp_create (sys/ucontext.h).
	struct SolUcontext {
		flags       u64
		link        voidptr
		sigmask     u64
		stack_sp    voidptr
		stack_size  u64
		stack_flags u64
		// mcontext: gregs[28] (amd64) + fpregs (not needed for thread entry).
		gregs [28]i64
	}

	const sol_reg_rip = 17
	const sol_reg_rsp = 20
	const sol_reg_rdi = 8

	fn sys_lwp_create(ucp voidptr, flags u64, new_lwp &i64) i64 {
		rc, er := sys_call3(sol_nr_lwp_create, u64(ucp), flags, u64(new_lwp))
		if er != 0 {
			return -i64(er)
		}
		return i64(rc)
	}

	fn sys_lwp_exit() {
		sys_call0(sol_nr_lwp_exit)
		for {}
	}

	fn sys_lwp_self() i64 {
		rc, _ := sys_call0(sol_nr_lwp_self)
		return i64(rc)
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
