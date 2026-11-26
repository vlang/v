module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#flag @DIR/libc_spawn_abi.c
	#include <spawn.h>
	#include <sys/resource.h>
	#include <sys/syscall.h>

	struct BareSpawnAttributes {
		defaults u64
		mask     u64
		flags    i32
		group    i32
		policy   i32
		priority i32
	}
	struct BareSpawnActions {
	mut:
		first &BareSpawnAction = unsafe { nil }
		last  &BareSpawnAction = unsafe { nil }
	}
	@[heap]
	struct BareSpawnAction {
	mut:
		next   &BareSpawnAction = unsafe { nil }
		kind   i32
		fd     i32
		target i32
		path   uint
		flags  i32
		mode   u32
	}
	@[export: 'v_spawn_action_add'; markused]
	fn bare_spawn_action_add(input uint, kind i32, fd i32, target i32, path uint, flags i32, mode u32) i32 {
		if fd < 0 || target < 0 { return i32(C.EBADF) }
		saved := bf_native_errno_cell
		defer { bf_native_errno_cell = saved }
		path_size := if path == 0 { uint(0) } else { bare_string_chr_nul(path, 0) - path + 1 }
		mut node := unsafe { &BareSpawnAction(__calloc(1, sizeof(BareSpawnAction) + path_size)) }
		if node == unsafe { nil } { return i32(C.ENOMEM) }
		node.kind = kind
		node.fd = fd
		node.target = target
		node.flags = flags
		node.mode = mode
		if path_size > 0 {
			node.path = uint(node) + sizeof(BareSpawnAction)
			unsafe { vmemcpy(voidptr(node.path), voidptr(path), int(path_size)) }
		}
		mut list := unsafe { &BareSpawnActions(input) }
		if list.last == unsafe { nil } { list.first = node } else { list.last.next = node }
		list.last = node
		return 0
	}
	@[export: 'v_spawn_actions_free'; markused]
	fn bare_spawn_actions_free(input uint) {
		mut list := unsafe { &BareSpawnActions(input) }
		mut node := list.first
		for node != unsafe { nil } {
			next := node.next
			unsafe { __free(node) }
			node = next
		}
		list.first = unsafe { nil }
		list.last = unsafe { nil }
	}
	fn bare_spawn_action(node &BareSpawnAction) i64 {
		fd := u64(node.fd)
		match node.kind {
			1 {
				closed := i64(sys_call1(u64(C.SYS_close), fd))
				if closed == -i64(C.EBADF) {
					mut limits := [2]u64{}
					if i64(sys_call4(u64(C.SYS_prlimit64), 0, u64(C.RLIMIT_NOFILE), 0, u64(&limits[0]))) == 0
						&& fd < limits[0] {
						return 0
					}
				}
				return closed
			}
			2 {
				if node.fd == node.target {
					return i64(sys_call3(u64(C.SYS_fcntl), fd, u64(C.F_SETFD), 0))
				}
				return i64(sys_call3(u64(C.SYS_dup3), fd, u64(node.target), 0))
			}
			3 {
				sys_call1(u64(C.SYS_close), fd)
				opened := i64(sys_call4(bf_nr_openat, u64(i64(-100)), u64(node.path), u64(node.flags), u64(node.mode)))
				if opened < 0 || opened == node.fd { return opened }
				result := i64(sys_call3(u64(C.SYS_dup3), u64(opened), fd, 0))
				sys_call1(u64(C.SYS_close), u64(opened))
				return result
			}
			4 { return i64(sys_call1(u64(C.SYS_chdir), u64(node.path))) }
			5 { return i64(sys_call1(u64(C.SYS_fchdir), fd)) }
			6 {
				bare_native_closefrom(node.fd)
				return 0
			}
			else { return -i64(C.EINVAL) }
		}
	}

	fn bare_spawn_attributes(attr &BareSpawnAttributes) i64 {
		if attr.flags & int(C.POSIX_SPAWN_SETSCHEDULER) != 0 {
			result := i64(sys_call3(u64(C.SYS_sched_setscheduler), 0, u64(attr.policy), u64(&attr.priority)))
			if result < 0 { return result }
		} else if attr.flags & int(C.POSIX_SPAWN_SETSCHEDPARAM) != 0 {
			result := i64(sys_call2(u64(C.SYS_sched_setparam), 0, u64(&attr.priority)))
			if result < 0 { return result }
		}
		if attr.flags & int(C.POSIX_SPAWN_SETSID) != 0 {
			result := i64(sys_call0(u64(C.SYS_setsid)))
			if result < 0 { return result }
		}
		if attr.flags & int(C.POSIX_SPAWN_SETPGROUP) != 0 {
			result := i64(sys_call2(u64(C.SYS_setpgid), 0, u64(attr.group)))
			if result < 0 { return result }
		}
		if attr.flags & int(C.POSIX_SPAWN_RESETIDS) != 0 {
			gid := i64(sys_call3(u64(C.SYS_setresgid), ~u64(0), sys_call0(u64(C.SYS_getgid)), ~u64(0)))
			if gid < 0 { return gid }
			return i64(sys_call3(u64(C.SYS_setresuid), ~u64(0), sys_call0(u64(C.SYS_getuid)), ~u64(0)))
		}
		return 0
	}

	fn bare_spawn_exec(file uint, search uint, args uint, env uint) i64 {
		if search == 0 { return i64(sys_call3(bf_nr_execve, u64(file), u64(args), u64(env))) }
		file_len := bare_string_chr_nul(file, 0) - file
		if file_len == 0 { return -i64(C.ENOENT) }
		mut candidate := [4096]u8{}
		mut start := search
		mut result := -i64(C.ENOENT)
		for {
			end := bare_string_chr_nul(start, `:`)
			dir_len := end - start
			slash := uint(dir_len != 0)
			if dir_len + slash + file_len < candidate.len {
				unsafe {
					vmemcpy(&candidate[0], voidptr(start), int(dir_len))
					if slash != 0 { candidate[dir_len] = `/` }
					vmemcpy(&candidate[dir_len + slash], voidptr(file), int(file_len + 1))
				}
				status := i64(sys_call3(bf_nr_execve, u64(&candidate[0]), u64(args), u64(env)))
				if status == -i64(C.EACCES) {
					result = status
				} else if status != -i64(C.ENOENT) && status != -i64(C.ENOTDIR) {
					return status
				}
			}
			if unsafe { *(&u8(end)) } == 0 { break }
			start = end + 1
		}
		return result
	}

	fn bare_spawn_child(file uint, search uint, actions uint, attr &BareSpawnAttributes, args uint, env uint, mask u64) i64 {
		// Reset caught dispositions while all signals are still blocked. Preserve
		// ignored dispositions unless the caller explicitly requests SIG_DFL.
		default_action := BareNativeSignalAction{}
		for sig in 1 .. 65 {
			if sig == C.SIGKILL || sig == C.SIGSTOP { continue }
			mut previous := BareNativeSignalAction{}
			sys_call4(u64(C.SYS_rt_sigaction), u64(sig), 0, u64(&previous), 8)
			if previous.handler > 1 || (attr.flags & int(C.POSIX_SPAWN_SETSIGDEF) != 0 && attr.defaults & (u64(1) << (sig - 1)) != 0) {
				sys_call4(u64(C.SYS_rt_sigaction), u64(sig), u64(&default_action), 0, 8)
			}
		}
		result := bare_spawn_attributes(attr)
		if result < 0 { return result }
		if actions != 0 {
			list := unsafe { &BareSpawnActions(actions) }
			for node := list.first; node != unsafe { nil }; node = node.next {
				code := bare_spawn_action(node)
				if code < 0 { return code }
			}
		}
		child_mask := if attr.flags & int(C.POSIX_SPAWN_SETSIGMASK) != 0 { attr.mask } else { mask }
		sys_call4(u64(C.SYS_rt_sigprocmask), 2, u64(&child_mask), 0, 8)
		return bare_spawn_exec(file, search, args, env)
	}

	@[export: 'v_spawn_run'; markused]
	fn bare_spawn_run(output uint, file uint, actions uint, attributes uint, args uint, env uint, path_search i32) i32 {
		saved := bf_native_errno_cell
		defer { bf_native_errno_cell = saved }
		mut search := uint(0)
		if path_search != 0 && unsafe { *(&u8(bare_string_chr_nul(file, `/`))) } == 0 {
			path := C.getenv(c'PATH')
			search = if path == unsafe { nil } { uint(c'/bin:/usr/bin') } else { uint(path) }
		}
		// The shared error word survives exec/exit without consuming an fd that
		// user file actions could close. CLONE_VFORK waits for that handoff;
		// omitting CLONE_VM gives the child its own stack and address space.
		status := i64(sys_call6(u64(C.SYS_mmap), 0, 4096, 3, 0x21, -1, 0))
		if status < 0 { return i32(-status) }
		defer { sys_call2(u64(C.SYS_munmap), u64(status), 4096) }
		all := ~u64(0)
		mut mask := u64(0)
		blocked := i64(sys_call4(u64(C.SYS_rt_sigprocmask), 2, u64(&all), u64(&mask), 8))
		if blocked < 0 { return i32(-blocked) }
		pid := i64(sys_call5(u64(C.SYS_clone), 0x4000 | u64(C.SIGCHLD), 0, 0, 0, 0))
		if pid == 0 {
			err := bare_spawn_child(file, search, actions, unsafe { &BareSpawnAttributes(attributes) }, args, env, mask)
			bare_store_u32(unsafe { &u32(status) }, u32(-err))
			sys_call1(u64(C.SYS_exit_group), 127)
			for {}
		}
		sys_call4(u64(C.SYS_rt_sigprocmask), 2, u64(&mask), 0, 8)
		if pid < 0 { return i32(-pid) }
		code := i32(bare_load_u32(unsafe { &u32(status) }))
		if code != 0 {
			for i64(sys_call4(u64(C.SYS_wait4), u64(pid), 0, 0, 0)) == -i64(C.EINTR) {
			}
		} else if output != 0 {
			unsafe { *(&i32(output)) = i32(pid) }
		}
		return code
	}
}
