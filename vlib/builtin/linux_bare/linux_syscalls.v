module builtin

enum SigIndex {
	si_signo  = 0x00
	si_code   = 0x02
	si_pid    = 0x04
	si_uid    = 0x05
	si_status = 0x06
	si_size   = 0x80
}

enum Signo {
	sighup    = 1 // Hangup.
	sigint    = 2 // Interactive attention signal.
	sigquit   = 3 // Quit.
	sigill    = 4 // Illegal instruction.
	sigtrap   = 5 // Trace/breakpoint trap.
	sigabrt   = 6 // Abnormal termination.
	sigbus    = 7
	sigfpe    = 8 // Erroneous arithmetic operation.
	sigkill   = 9 // Killed.
	sigusr1   = 10
	sigsegv   = 11 // Invalid access to memory.
	sigusr2   = 12
	sigpipe   = 13 // Broken pipe.
	sigalrm   = 14 // Alarm clock.
	sigterm   = 15 // Termination request.
	sigstkflt = 16
	sigchld   = 17
	sigcont   = 18
	sigstop   = 19
	sigtstp   = 20
	sigttin   = 21 // Background read from control terminal.
	sigttou   = 22 // Background write to control terminal.
	sigurg    = 23
	sigxcpu   = 24 // CPU time limit exceeded.
	sigxfsz   = 25 // File size limit exceeded.
	sigvtalrm = 26 // Virtual timer expired.
	sigprof   = 27 // Profiling timer expired.
	sigwinch  = 28
	sigpoll   = 29
	sigsys    = 31
}

// List of all the errors returned by syscalls
enum Errno {
	enoerror = 0x00000000
	eperm    = 0x00000001
	enoent   = 0x00000002
	esrch    = 0x00000003
	eintr    = 0x00000004
	eio      = 0x00000005
	enxio    = 0x00000006
	e2big    = 0x00000007
	enoexec  = 0x00000008
	ebadf    = 0x00000009
	echild   = 0x0000000a
	eagain   = 0x0000000b
	enomem   = 0x0000000c
	eacces   = 0x0000000d
	efault   = 0x0000000e
	enotblk  = 0x0000000f
	ebusy    = 0x00000010
	eexist   = 0x00000011
	exdev    = 0x00000012
	enodev   = 0x00000013
	enotdir  = 0x00000014
	eisdir   = 0x00000015
	einval   = 0x00000016
	enfile   = 0x00000017
	emfile   = 0x00000018
	enotty   = 0x00000019
	etxtbsy  = 0x0000001a
	efbig    = 0x0000001b
	enospc   = 0x0000001c
	espipe   = 0x0000001d
	erofs    = 0x0000001e
	emlink   = 0x0000001f
	epipe    = 0x00000020
	edom     = 0x00000021
	erange   = 0x00000022
}

enum MemProt {
	prot_read      = 0x1
	prot_write     = 0x2
	prot_exec      = 0x4
	prot_none      = 0x0
	prot_growsdown = 0x01000000
	prot_growsup   = 0x02000000
}

enum MapFlags {
	map_shared          = 0x01
	map_private         = 0x02
	map_shared_validate = 0x03
	map_type            = 0x0f
	map_fixed           = 0x10
	map_file            = 0x00
	map_anonymous       = 0x20
	map_huge_shift      = 26
	map_huge_mask       = 0x3f
}

//   const (
// 	fcntlf_dupfd         = 0x00000000
// 	fcntlf_exlck         = 0x00000004
// 	fcntlf_getfd         = 0x00000001
// 	fcntlf_getfl         = 0x00000003
// 	fcntlf_getlk         = 0x00000005
// 	fcntlf_getlk64       = 0x0000000c
// 	fcntlf_getown        = 0x00000009
// 	fcntlf_getowner_uids = 0x00000011
// 	fcntlf_getown_ex     = 0x00000010
// 	fcntlf_getsig        = 0x0000000b
// 	fcntlf_ofd_getlk     = 0x00000024
// 	fcntlf_ofd_setlk     = 0x00000025
// 	fcntlf_ofd_setlkw    = 0x00000026
// 	fcntlf_owner_pgrp    = 0x00000002
// 	fcntlf_owner_pid     = 0x00000001
// 	fcntlf_owner_tid     = 0x00000000
// 	fcntlf_rdlck         = 0x00000000
// 	fcntlf_setfd         = 0x00000002
// 	fcntlf_setfl         = 0x00000004
// 	fcntlf_setlk         = 0x00000006
// 	fcntlf_setlk64       = 0x0000000d
// 	fcntlf_setlkw        = 0x00000007
// 	fcntlf_setlkw64      = 0x0000000e
// 	fcntlf_setown        = 0x00000008
// 	fcntlf_setown_ex     = 0x0000000f
// 	fcntlf_setsig        = 0x0000000a
// 	fcntlf_shlck         = 0x00000008
// 	fcntlf_unlck         = 0x00000002
// 	fcntlf_wrlck         = 0x00000001
// 	fcntllock_ex         = 0x00000002
// 	fcntllock_mand       = 0x00000020
// 	fcntllock_nb         = 0x00000004
// 	fcntllock_read       = 0x00000040
// 	fcntllock_rw         = 0x000000c0
// 	fcntllock_sh         = 0x00000001
// 	fcntllock_un         = 0x00000008
// 	fcntllock_write      = 0x00000080
// 	fcntlo_accmode       = 0x00000003
// 	fcntlo_append        = 0x00000400
// 	fcntlo_cloexec       = 0x00080000
// 	fcntlo_creat         = 0x00000040
// 	fcntlo_direct        = 0x00004000
// 	fcntlo_directory     = 0x00010000
// 	fcntlo_dsync         = 0x00001000
// 	fcntlo_excl          = 0x00000080
// 	fcntlo_largefile     = 0x00008000
// 	fcntlo_ndelay        = 0x00000800
// 	fcntlo_noatime       = 0x00040000
// 	fcntlo_noctty        = 0x00000100
// 	fcntlo_nofollow      = 0x00020000
// 	fcntlo_nonblock      = 0x00000800
// 	fcntlo_path          = 0x00200000
// 	fcntlo_rdonly        = 0x00000000
// 	fcntlo_rdwr          = 0x00000002
// 	fcntlo_trunc         = 0x00000200
// 	fcntlo_wronly        = 0x00000001
// )

/*
Paraphrased from "man 2 waitid" on Linux

	Upon successful return, waitid() fills in the
	following fields of the siginfo_t structure
	pointed to by infop:

	si_pid, offset 0x10, int index 0x04:
		The process ID of the child.

	si_uid: offset 0x14, int index 0x05
		The real user ID of the child.

	si_signo: offset 0x00, int index 0x00
		Always set to SIGCHLD.

	si_status: ofset 0x18, int index 0x06
		1 the exit status of the child, as given to _exit(2)
			(or exit(3)) (sc_sys.cld_exited)
		2 the signal that caused the child to terminate, stop,
			or continue.
		3 The si_code field can be used to determine how to
			interpret this field.

	si_code, set to one of (enum Wi_si_code), offset 0x08, int index 0x02:
		CLD_EXITED (child called _exit(2));
		CLD_KILLED (child killed by signal);
		CLD_DUMPED (child  killed by signal, and dumped core);
		CLD_STOPPED (child stopped by signal);
		CLD_TRAPPED (traced child has trapped);
		CLD_CONTINUED (child continued by SIGCONT).
*/

const wp_sys_wnohang = u64(0x00000001)
const wp_sys_wuntraced = u64(0x00000002)
const wp_sys_wstopped = u64(0x00000002)
const wp_sys_wexited = u64(0x00000004)
const wp_sys_wcontinued = u64(0x00000008)
const wp_sys_wnowait = u64(0x01000000) // don't reap, just poll status.

const wp_sys___wnothread = u64(0x20000000) // don't wait on children of other threads in this group

const wp_sys___wall = u64(0x40000000) // wait on all children, regardless of type

const wp_sys___wclone = u64(0x80000000)

// First argument to waitid:
enum WiWhich {
	p_all  = 0
	p_pid  = 1
	p_pgid = 2
}

enum WiSiCode {
	cld_exited    = 1 // child has exited
	cld_killed    = 2 // child was killed
	cld_dumped    = 3 // child terminated abnormally
	cld_trapped   = 4 // traced child has trapped
	cld_stopped   = 5 // child has stopped
	cld_continued = 6 // stopped child has continued
}

// Syscall numbers. arm64 uses the asm-generic table, which has no open, pipe,
// fork, vfork or dup2: the wrappers below use openat, pipe2, clone and dup3 there.
$if arm64 {
	const sys_nr_read = u64(63)
	const sys_nr_write = u64(64)
	const sys_nr_openat = u64(56)
	const sys_nr_close = u64(57)
	const sys_nr_mmap = u64(222)
	const sys_nr_munmap = u64(215)
	const sys_nr_mremap = u64(216)
	const sys_nr_pipe2 = u64(59)
	const sys_nr_sched_yield = u64(124)
	const sys_nr_madvise = u64(233)
	const sys_nr_getpid = u64(172)
	const sys_nr_clone = u64(220)
	const sys_nr_dup3 = u64(24)
	const sys_nr_execve = u64(221)
	const sys_nr_exit = u64(93)
	const sys_nr_getuid = u64(174)
	const sys_nr_waitid = u64(95)
} $else {
	const sys_nr_read = u64(0)
	const sys_nr_write = u64(1)
	const sys_nr_open = u64(2)
	const sys_nr_close = u64(3)
	const sys_nr_mmap = u64(9)
	const sys_nr_munmap = u64(11)
	const sys_nr_mremap = u64(25)
	const sys_nr_pipe = u64(22)
	const sys_nr_sched_yield = u64(24)
	const sys_nr_madvise = u64(28)
	const sys_nr_getpid = u64(39)
	const sys_nr_fork = u64(57)
	const sys_nr_vfork = u64(58)
	const sys_nr_dup2 = u64(33)
	const sys_nr_execve = u64(59)
	const sys_nr_exit = u64(60)
	const sys_nr_getuid = u64(102)
	const sys_nr_waitid = u64(247)
}

$if !i386 {
	fn split_int_errno(rc_in u64) (i64, Errno) {
		rc := i64(rc_in)
		if rc < 0 {
			return i64(-1), unsafe { Errno(-rc) }
		}
		return rc, Errno.enoerror
	}

	fn sys_read(fd i64, buf &u8, count u64) (i64, Errno) {
		return split_int_errno(sys_call3(sys_nr_read, u64(fd), u64(buf), count))
	}

	pub fn sys_write(fd i64, buf &u8, count u64) (i64, Errno) {
		return split_int_errno(sys_call3(sys_nr_write, u64(fd), u64(buf), count))
	}

	fn sys_open(filename &u8, flags i64, mode int) (i64, Errno) {
		$if arm64 {
			at_fdcwd := u64(-i64(100))
			return split_int_errno(sys_call4(sys_nr_openat, at_fdcwd, u64(filename), u64(flags),
				u64(mode)))
		} $else {
			return split_int_errno(sys_call3(sys_nr_open, u64(filename), u64(flags), u64(mode)))
		}
	}

	fn sys_close(fd i64) Errno {
		return unsafe { Errno(-i64(sys_call1(sys_nr_close, u64(fd)))) }
	}

	fn sys_mmap(addr &u8, len u64, prot MemProt, flags MapFlags, fildes i64, off u64) (&u8, Errno) {
		rc := sys_call6(sys_nr_mmap, u64(addr), len, u64(prot), u64(flags), fildes, off)
		a, e := split_int_errno(rc)
		return unsafe { &u8(a) }, e
	}

	fn sys_munmap(addr voidptr, len u64) Errno {
		return unsafe { Errno(-sys_call2(sys_nr_munmap, u64(addr), len)) }
	}

	fn sys_mremap(old_addr voidptr, old_len u64, new_len u64, flags u64) (&u8, Errno) {
		rc := sys_call4(sys_nr_mremap, u64(old_addr), old_len, new_len, flags)
		a, e := split_int_errno(rc)
		return unsafe { &u8(a) }, e
	}

	fn sys_pipe(filedes &int) Errno {
		$if arm64 {
			return unsafe { Errno(sys_call2(sys_nr_pipe2, u64(filedes), 0)) }
		} $else {
			return unsafe { Errno(sys_call1(sys_nr_pipe, u64(filedes))) }
		}
	}

	fn sys_sched_yield() Errno {
		return unsafe { Errno(sys_call0(sys_nr_sched_yield)) }
	}

	fn sys_madvise(addr voidptr, len u64, advice int) Errno {
		return unsafe { Errno(sys_call3(sys_nr_madvise, u64(addr), len, u64(advice))) }
	}

	fn sys_getpid() int {
		return int(sys_call0(sys_nr_getpid))
	}

	fn sys_fork() int {
		$if arm64 {
			sigchld := u64(Signo.sigchld)
			return int(sys_call5(sys_nr_clone, sigchld, 0, 0, 0, 0))
		} $else {
			return int(sys_call0(sys_nr_fork))
		}
	}

	fn sys_vfork() int {
		$if arm64 {
			clone_vm_vfork := u64(0x4100) // CLONE_VM | CLONE_VFORK
			return int(sys_call5(sys_nr_clone, clone_vm_vfork | u64(Signo.sigchld), 0, 0, 0, 0))
		} $else {
			return int(sys_call0(sys_nr_vfork))
		}
	}

	fn sys_dup2(oldfd int, newfd int) (i64, Errno) {
		$if arm64 {
			// dup3 rejects equal descriptors, which dup2 returns unchanged.
			if oldfd == newfd {
				return i64(newfd), Errno.enoerror
			}
			return split_int_errno(sys_call3(sys_nr_dup3, u64(oldfd), u64(newfd), 0))
		} $else {
			return split_int_errno(sys_call2(sys_nr_dup2, u64(oldfd), u64(newfd)))
		}
	}

	fn sys_execve(filename &u8, argv []&u8, envp []&u8) int {
		return int(sys_call3(sys_nr_execve, u64(filename), u64(argv.data), u64(envp.data)))
	}

	@[noreturn]
	fn sys_exit(ec int) {
		sys_call1(sys_nr_exit, u64(ec))
		for {}
	}

	fn sys_getuid() int {
		return int(sys_call0(sys_nr_getuid))
	}

	fn sys_waitid(which WiWhich, pid int, infop &int, options int, ru voidptr) Errno {
		return unsafe {
			Errno(sys_call5(sys_nr_waitid, u64(which), u64(pid), u64(infop), u64(options),
				u64(ru)))
		}
	}

	// The syscall primitives stay defined for every non-i386 arch so the wrappers
	// above remain resolvable; only the inline asm is gated to amd64 and arm64. On
	// another target each returns -ENOSYS (38) -- a failed syscall rather than a
	// fake success -- so e.g. mm_alloc sees the mmap error instead of
	// dereferencing 0. Such a target must supply its own syscall + startup layer.
	//
	// The kernel clobbers rcx and r11 on amd64. Every syscall may read or write
	// memory through its arguments, hence `memory` and `volatile`. On arm64 the
	// arguments go through x0-x5 and the number through x8; inputs cannot be
	// allocated to clobbered registers, so moving them there in order is safe.
	fn sys_call0(scn u64) u64 {
		mut res := u64(-i64(38)) // -ENOSYS, overwritten by the syscall on amd64 and arm64
		$if amd64 {
			asm volatile amd64 {
				syscall
				; =a (res)
				; 0 (scn)
				; rcx
				  r11
				  memory
			}
		} $else $if arm64 {
			asm volatile arm64 {
				mov x8, scn
				svc 0
				mov res, x0
				; =r (res)
				; r (scn)
				; x0
				  x8
				  memory
			}
		}
		return res
	}

	fn sys_call1(scn u64, arg1 u64) u64 {
		mut res := u64(-i64(38)) // -ENOSYS, overwritten by the syscall on amd64 and arm64
		$if amd64 {
			asm volatile amd64 {
				syscall
				; =a (res)
				; 0 (scn)
				  D (arg1)
				; rcx
				  r11
				  memory
			}
		} $else $if arm64 {
			asm volatile arm64 {
				mov x8, scn
				mov x0, arg1
				svc 0
				mov res, x0
				; =r (res)
				; r (scn)
				  r (arg1)
				; x0
				  x8
				  memory
			}
		}
		return res
	}

	fn sys_call2(scn u64, arg1 u64, arg2 u64) u64 {
		mut res := u64(-i64(38)) // -ENOSYS, overwritten by the syscall on amd64 and arm64
		$if amd64 {
			asm volatile amd64 {
				syscall
				; =a (res)
				; 0 (scn)
				  D (arg1)
				  S (arg2)
				; rcx
				  r11
				  memory
			}
		} $else $if arm64 {
			asm volatile arm64 {
				mov x8, scn
				mov x0, arg1
				mov x1, arg2
				svc 0
				mov res, x0
				; =r (res)
				; r (scn)
				  r (arg1)
				  r (arg2)
				; x0
				  x1
				  x8
				  memory
			}
		}
		return res
	}

	fn sys_call3(scn u64, arg1 u64, arg2 u64, arg3 u64) u64 {
		mut res := u64(-i64(38)) // -ENOSYS, overwritten by the syscall on amd64 and arm64
		$if amd64 {
			asm volatile amd64 {
				syscall
				; =a (res)
				; 0 (scn)
				  D (arg1)
				  S (arg2)
				  d (arg3)
				; rcx
				  r11
				  memory
			}
		} $else $if arm64 {
			asm volatile arm64 {
				mov x8, scn
				mov x0, arg1
				mov x1, arg2
				mov x2, arg3
				svc 0
				mov res, x0
				; =r (res)
				; r (scn)
				  r (arg1)
				  r (arg2)
				  r (arg3)
				; x0
				  x1
				  x2
				  x8
				  memory
			}
		}
		return res
	}

	fn sys_call4(scn u64, arg1 u64, arg2 u64, arg3 u64, arg4 u64) u64 {
		mut res := u64(-i64(38)) // -ENOSYS, overwritten by the syscall on amd64 and arm64
		$if amd64 {
			asm volatile amd64 {
				mov r10, arg4
				syscall
				; =a (res)
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
		} $else $if arm64 {
			asm volatile arm64 {
				mov x8, scn
				mov x0, arg1
				mov x1, arg2
				mov x2, arg3
				mov x3, arg4
				svc 0
				mov res, x0
				; =r (res)
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
				  memory
			}
		}
		return res
	}

	fn sys_call5(scn u64, arg1 u64, arg2 u64, arg3 u64, arg4 u64, arg5 u64) u64 {
		mut res := u64(-i64(38)) // -ENOSYS, overwritten by the syscall on amd64 and arm64
		$if amd64 {
			asm volatile amd64 {
				mov r10, arg4
				mov r8, arg5
				syscall
				; =a (res)
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
		} $else $if arm64 {
			asm volatile arm64 {
				mov x8, scn
				mov x0, arg1
				mov x1, arg2
				mov x2, arg3
				mov x3, arg4
				mov x4, arg5
				svc 0
				mov res, x0
				; =r (res)
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
				  memory
			}
		}
		return res
	}

	fn sys_call6(scn u64, arg1 u64, arg2 u64, arg3 u64, arg4 u64, arg5 i64, arg6 u64) u64 {
		mut res := u64(-i64(38)) // -ENOSYS, overwritten by the syscall on amd64 and arm64
		$if amd64 {
			asm volatile amd64 {
				mov r10, arg4
				mov r8, arg5
				mov r9, arg6
				syscall
				; =a (res)
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
		} $else $if arm64 {
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
				; =r (res)
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
				  memory
			}
		}
		return res
	}

	// _start is the process entry point: the kernel leaves argc at the stack
	// pointer, followed by the argv array. It calls `main(argc, argv)` and exits
	// with its result. A top-level asm block lands in whatever section the C
	// compiler is emitting at that point (gcc: the string literals' `.data`), so
	// it switches to `.text` itself.
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
			mov rax, 60
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
			mov x8, 93
			svc 0
			.popsection
		}
	}
}
}
