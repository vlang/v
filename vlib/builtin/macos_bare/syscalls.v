module builtin

// macOS bare runtime: kernel syscall layer for `-freestanding`.
// Numbers verified against Go's zsysnum_darwin_amd64.go (generated from
// /usr/include/sys/syscall.h), with the BSD syscall class 0x2000000 ORed in.
// Error convention: carry flag set on error, errno in rax/x0.
// macOS arm64 uses x16 + `svc 0x80`; amd64 uses rax + `syscall`.

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
	}

	// The x86_64 syscall class is stripped by the arm64 syscall helpers.
	const mac_nr_exit = u64(0x2000001)
	const mac_nr_fork = u64(0x2000002)
	const mac_nr_read = u64(0x2000003)
	const mac_nr_write = u64(0x2000004)
	const mac_nr_open = u64(0x2000005)
	const mac_nr_close = u64(0x2000006)
	const mac_nr_wait4 = u64(0x2000007)
	const mac_nr_unlink = u64(0x200000a)
	const mac_nr_chdir = u64(0x200000c)
	const mac_nr_chmod = u64(0x200000f)
	const mac_nr_getpid = u64(0x2000014)
	const mac_nr_getuid = u64(0x2000018)
	const mac_nr_geteuid = u64(0x2000019)
	const mac_nr_recvmsg = u64(0x200001b)
	const mac_nr_sendmsg = u64(0x200001c)
	const mac_nr_recvfrom = u64(0x200001d)
	const mac_nr_accept = u64(0x200001e)
	const mac_nr_getpeername = u64(0x200001f)
	const mac_nr_getsockname = u64(0x2000020)
	const mac_nr_kill = u64(0x2000025)
	const mac_nr_getppid = u64(0x2000027)
	const mac_nr_dup = u64(0x2000029)
	const mac_nr_pipe = u64(0x200002a)
	const mac_nr_getegid = u64(0x200002b)
	const mac_nr_sigaction = u64(0x200002e)
	const mac_nr_getgid = u64(0x200002f)
	const mac_nr_sigprocmask = u64(0x2000030)
	const mac_nr_ioctl = u64(0x2000036)
	const mac_nr_symlink = u64(0x2000039)
	const mac_nr_readlink = u64(0x200003a)
	const mac_nr_execve = u64(0x200003b)
	const mac_nr_umask = u64(0x200003c)
	const mac_nr_vfork = u64(0x2000042)
	const mac_nr_munmap = u64(0x2000049)
	const mac_nr_mprotect = u64(0x200004a)
	const mac_nr_madvise = u64(0x200004b)
	const mac_nr_setpgid = u64(0x2000052)
	const mac_nr_dup2 = u64(0x200005a)
	const mac_nr_fcntl = u64(0x200005c)
	const mac_nr_select = u64(0x200005d)
	const mac_nr_fsync = u64(0x200005f)
	const mac_nr_socket = u64(0x2000061)
	const mac_nr_connect = u64(0x2000062)
	const mac_nr_bind = u64(0x2000068)
	const mac_nr_setsockopt = u64(0x2000069)
	const mac_nr_listen = u64(0x200006a)
	const mac_nr_gettimeofday = u64(0x2000074)
	const mac_nr_getsockopt = u64(0x2000076)
	const mac_nr_rename = u64(0x2000080)
	const mac_nr_flock = u64(0x2000083)
	const mac_nr_sendto = u64(0x2000085)
	const mac_nr_shutdown = u64(0x2000086)
	const mac_nr_socketpair = u64(0x2000087)
	const mac_nr_mkdir = u64(0x2000088)
	const mac_nr_rmdir = u64(0x2000089)
	const mac_nr_setsid = u64(0x2000093)
	const mac_nr_waitid = u64(0x20000ad)
	const mac_nr_fstat64 = u64(0x2000153)
	const mac_nr_stat64 = u64(0x2000152)
	const mac_nr_getdirentries64 = u64(0x2000158)
	const mac_nr_mmap = u64(0x20000c5)
	const mac_nr_lseek = u64(0x20000c7)
	const mac_nr_truncate = u64(0x20000c8)
	const mac_nr_ftruncate = u64(0x20000c9)
	const mac_nr_sysctl = u64(0x20000ca)
	const mac_nr_kqueue = u64(0x200016a)
	const mac_nr_kevent = u64(0x200016b)

	const mac_f_getpath = u64(50)

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
				mov x16, scn
				and x16, x16, #0xffffff
				svc 0x80
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
				  x16
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

	fn mac_sys_call0_pair(scn u64) (u64, u64, u64) {
		mut first := u64(0)
		mut second := u64(0)
		mut err := u64(0)
		$if arm64 {
			asm volatile arm64 {
				mov x16, scn
				and x16, x16, #0xffffff
				svc 0x80
				mov first, x0
				mov second, x1
				mov err, xzr
				b.cc '1f'
				mov err, x0
				1:
				; =r (first)
				  =r (second)
				  =r (err)
				; r (scn)
				; x0
				  x1
				  x16
				  cc
				  memory
			}
		} $else {
			asm volatile amd64 {
				mov rax, scn
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
				  rdx
				  rcx
				  r11
				  cc
				  memory
			}
		}
		return first, second, err
	}

	fn sys_call1(scn u64, arg1 u64) (u64, u64) {
		mut res := u64(0)
		mut err := u64(0)
		$if arm64 {
			asm volatile arm64 {
				mov x16, scn
				and x16, x16, #0xffffff
				mov x0, arg1
				svc 0x80
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
				  x16
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
				mov x16, scn
				and x16, x16, #0xffffff
				mov x0, arg1
				mov x1, arg2
				svc 0x80
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
				  x16
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
				mov x16, scn
				and x16, x16, #0xffffff
				mov x0, arg1
				mov x1, arg2
				mov x2, arg3
				svc 0x80
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
				  x16
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
				mov x16, scn
				and x16, x16, #0xffffff
				mov x0, arg1
				mov x1, arg2
				mov x2, arg3
				mov x3, arg4
				svc 0x80
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
				  x16
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
				mov x16, scn
				and x16, x16, #0xffffff
				mov x0, arg1
				mov x1, arg2
				mov x2, arg3
				mov x3, arg4
				mov x4, arg5
				svc 0x80
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
				  x16
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
				mov x16, scn
				and x16, x16, #0xffffff
				mov x0, arg1
				mov x1, arg2
				mov x2, arg3
				mov x3, arg4
				mov x4, arg5
				mov x5, arg6
				svc 0x80
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
				  x16
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
}
