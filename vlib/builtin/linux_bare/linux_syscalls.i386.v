module builtin

struct LinuxMmapArgs {
	addr   usize
	len    usize
	prot   usize
	flags  usize
	fd     usize
	offset usize
}

fn split_int_errno(rc_in usize) (i64, Errno) {
	rc := isize(rc_in)
	if rc < 0 {
		return i64(-1), unsafe { Errno(-rc) }
	}
	return i64(rc), Errno.enoerror
}

// 3 sys_read
fn sys_read(fd i64, buf &u8, count u64) (i64, Errno) {
	return split_int_errno(sys_call3(3, usize(fd), usize(buf), usize(count)))
}

// 4 sys_write
pub fn sys_write(fd i64, buf &u8, count u64) (i64, Errno) {
	return split_int_errno(sys_call3(4, usize(fd), usize(buf), usize(count)))
}

// 5 sys_open
fn sys_open(filename &u8, flags i64, mode int) (i64, Errno) {
	return split_int_errno(sys_call3(5, usize(filename), usize(flags), usize(mode)))
}

// 6 sys_close
fn sys_close(fd i64) Errno {
	return unsafe { Errno(-isize(sys_call1(6, usize(fd)))) }
}

// 90 sys_mmap
fn sys_mmap(addr &u8, len u64, prot MemProt, flags MapFlags, fildes i64, off u64) (&u8, Errno) {
	args := LinuxMmapArgs{
		addr:   usize(addr)
		len:    usize(len)
		prot:   usize(prot)
		flags:  usize(flags)
		fd:     usize(fildes)
		offset: usize(off)
	}
	rc := sys_call1(90, usize(&args))
	a, e := split_int_errno(rc)
	return unsafe { &u8(usize(a)) }, e
}

// 91 sys_munmap
fn sys_munmap(addr voidptr, len u64) Errno {
	return unsafe { Errno(-isize(sys_call2(91, usize(addr), usize(len)))) }
}

// 163 sys_mremap
fn sys_mremap(old_addr voidptr, old_len u64, new_len u64, flags u64) (&u8, Errno) {
	rc := sys_call4(163, usize(old_addr), usize(old_len), usize(new_len), usize(flags))
	a, e := split_int_errno(rc)
	return unsafe { &u8(usize(a)) }, e
}

// 42 sys_pipe
fn sys_pipe(filedes &int) Errno {
	return unsafe { Errno(isize(sys_call1(42, usize(filedes)))) }
}

// 158 sys_sched_yield
fn sys_sched_yield() Errno {
	return unsafe { Errno(isize(sys_call0(158))) }
}

// 219 sys_madvise
fn sys_madvise(addr voidptr, len u64, advice int) Errno {
	return unsafe { Errno(isize(sys_call3(219, usize(addr), usize(len), usize(advice)))) }
}

// 20 sys_getpid
fn sys_getpid() int {
	return int(sys_call0(20))
}

// 2 sys_fork
fn sys_fork() int {
	return int(sys_call0(2))
}

// 190 sys_vfork
fn sys_vfork() int {
	return int(sys_call0(190))
}

// 63 sys_dup2
fn sys_dup2(oldfd int, newfd int) (i64, Errno) {
	return split_int_errno(sys_call2(63, usize(oldfd), usize(newfd)))
}

// 11 sys_execve
fn sys_execve(filename &u8, argv []&u8, envp []&u8) int {
	return int(sys_call3(11, usize(filename), usize(argv.data), usize(envp.data)))
}

// 1 sys_exit
@[noreturn]
fn sys_exit(ec int) {
	sys_call1(1, usize(ec))
	for {}
}

// 199 sys_getuid32
fn sys_getuid() int {
	return int(sys_call0(199))
}

// 284 sys_waitid
fn sys_waitid(which WiWhich, pid int, infop &int, options int, ru voidptr) Errno {
	return unsafe {
		Errno(isize(sys_call5(284, usize(which), usize(pid), usize(infop), usize(options),
			usize(ru))))
	}
}

fn sys_call0(scn usize) usize {
	mut res := usize(0)
	asm volatile i386 {
		mov eax, scn
		int 0x80
		mov res, eax
		; =r (res)
		; r (scn)
		; eax
		  memory
	}
	return res
}

fn sys_call1(scn usize, arg1 usize) usize {
	mut res := usize(0)
	asm volatile i386 {
		mov eax, scn
		mov ebx, arg1
		int 0x80
		mov res, eax
		; =r (res)
		; r (scn)
		  r (arg1)
		; eax
		  ebx
		  memory
	}
	return res
}

fn sys_call2(scn usize, arg1 usize, arg2 usize) usize {
	mut res := usize(0)
	asm volatile i386 {
		mov eax, scn
		mov ebx, arg1
		mov ecx, arg2
		int 0x80
		mov res, eax
		; =r (res)
		; r (scn)
		  r (arg1)
		  r (arg2)
		; eax
		  ebx
		  ecx
		  memory
	}
	return res
}

fn sys_call3(scn usize, arg1 usize, arg2 usize, arg3 usize) usize {
	mut res := usize(0)
	asm volatile i386 {
		mov eax, scn
		mov ebx, arg1
		mov ecx, arg2
		mov edx, arg3
		int 0x80
		mov res, eax
		; =r (res)
		; r (scn)
		  r (arg1)
		  r (arg2)
		  r (arg3)
		; eax
		  ebx
		  ecx
		  edx
		  memory
	}
	return res
}

fn sys_call4(scn usize, arg1 usize, arg2 usize, arg3 usize, arg4 usize) usize {
	mut res := usize(0)
	asm volatile i386 {
		mov eax, scn
		mov ebx, arg1
		mov ecx, arg2
		mov edx, arg3
		mov esi, arg4
		int 0x80
		mov res, eax
		; =r (res)
		; r (scn)
		  r (arg1)
		  r (arg2)
		  r (arg3)
		  r (arg4)
		; eax
		  ebx
		  ecx
		  edx
		  esi
		  memory
	}
	return res
}

fn sys_call5(scn usize, arg1 usize, arg2 usize, arg3 usize, arg4 usize, arg5 usize) usize {
	mut res := usize(0)
	asm volatile i386 {
		mov eax, scn
		mov ebx, arg1
		mov ecx, arg2
		mov edx, arg3
		mov esi, arg4
		mov edi, arg5
		int 0x80
		mov res, eax
		; =r (res)
		; r (scn)
		  r (arg1)
		  r (arg2)
		  r (arg3)
		  r (arg4)
		  r (arg5)
		; eax
		  ebx
		  ecx
		  edx
		  esi
		  edi
		  memory
	}
	return res
}

asm i386 {
	.globl _start
	_start:
	call main
	mov eax, 1
	xor ebx, ebx
	int 0x80
	ret
}
