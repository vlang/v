@[has_globals]
module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#include "@DIR/libc_native_abi.h"
	#include "@DIR/libc_runtime.h"
	fn C.v_native_environment() uint
	fn C.v_native_termios_get(i32, uint) i32
	fn C.v_native_termios_set(i32, i32, uint) i32
	fn C.getenv(&char) &char
	fn C.sysconf(i32) i64

	@[export: 'v_native_openat'; markused]
	fn bare_native_openat(fd i32, path uint, flags i32, mode u32) i32 {
		return i32(bare_native_result(sys_call4(bf_nr_openat, u64(fd), u64(path), u64(flags), u64(mode))))
	}
	@[export: 'v_native_execv'; markused]
	fn bare_native_execv(path uint, arguments uint) i32 {
		return i32(bare_native_result(sys_call3(bf_nr_execve, u64(path), u64(arguments), u64(C.v_native_environment()))))
	}
	fn bare_native_exec_shell(path uint, arguments uint) i32 {
		bare_native_execv(path, arguments)
		if bf_native_errno_cell != i32(Errno.enoexec) { return -1 }
		mut count := uint(0)
		if arguments != 0 {
			values := unsafe { &uint(arguments) }
			for unsafe { values[count] } != 0 {
				if count >= uint(max_int) / sizeof(uint) - 3 {
					bf_native_errno_cell = i32(Errno.e2big)
					return -1
				}
				count++
			}
		}
		capacity := if count > 0 { count + 2 } else { uint(3) }
		mut shell := unsafe { &uint(__calloc(capacity, sizeof(uint))) }
		if shell == unsafe { nil } { return -1 }
		unsafe {
			shell[0] = uint(c'/bin/sh')
			shell[1] = path
		}
		for i in 1 .. int(count) {
			unsafe { shell[i + 1] = (&uint(arguments))[i] }
		}
		bare_native_execv(uint(c'/bin/sh'), uint(shell))
		error := bf_native_errno_cell
		unsafe { __free(shell) }
		bf_native_errno_cell = error
		return -1
	}
	@[export: 'v_native_execvp'; markused]
	fn bare_native_execvp(file uint, arguments uint) i32 {
		filename := unsafe { tos2(&u8(file)) }
		if filename.len == 0 {
			bf_native_errno_cell = i32(Errno.enoent)
			return -1
		}
		if filename.contains('/') { return bare_native_exec_shell(file, arguments) }
		value := C.getenv(c'PATH')
		search := if value == unsafe { nil } {
			'/bin:/usr/bin'
		} else {
			unsafe { tos2(&u8(value)) }
		}
		if search.len > max_int - filename.len - 2 {
			bf_native_errno_cell = i32(C.ENAMETOOLONG)
			return -1
		}
		mut path := unsafe { &u8(__malloc(uint(search.len + filename.len + 2))) }
		if path == unsafe { nil } { return -1 }
		mut denied := false
		mut position := 0
		mut error := i32(Errno.enoent)
		for {
			mut end := position
			for end < search.len && search[end] != `:` { end++ }
			mut length := end - position
			for i in 0 .. length {
				unsafe { path[i] = search[position + i] }
			}
			if length > 0 {
				unsafe { path[length] = `/` }
				length++
			}
			for i in 0 .. filename.len {
				unsafe { path[length + i] = filename[i] }
			}
			unsafe { path[length + filename.len] = 0 }
			bare_native_exec_shell(uint(path), arguments)
			error = bf_native_errno_cell
			if error == i32(Errno.eacces) {
				denied = true
			} else if error != i32(Errno.enoent) && error != i32(Errno.enotdir) {
				break
			}
			if end == search.len {
				error = if denied { i32(Errno.eacces) } else { i32(Errno.enoent) }
				break
			}
			position = end + 1
		}
		unsafe { __free(path) }
		bf_native_errno_cell = error
		return -1
	}
	@[heap]
	struct BareAtfork {
	mut:
		prepare uint
		parent  uint
		child   uint
		older   &BareAtfork = unsafe { nil }
		newer   &BareAtfork = unsafe { nil }
	}
	__global bare_atfork_lock u32
	__global bare_atfork_head &BareAtfork
	@[export: 'v_pthread_atfork'; markused]
	fn bare_pthread_atfork(prepare uint, parent uint, child uint) i32 {
		saved := bf_native_errno_cell
		defer { bf_native_errno_cell = saved }
		mut node := unsafe { &BareAtfork(__calloc(1, sizeof(BareAtfork))) }
		if node == unsafe { nil } { return i32(C.ENOMEM) }
		node.prepare = prepare
		node.parent = parent
		node.child = child
		bare_compat_lock(&bare_atfork_lock)
		node.older = bare_atfork_head
		if bare_atfork_head != unsafe { nil } { bare_atfork_head.newer = node }
		bare_atfork_head = node
		bare_compat_unlock(&bare_atfork_lock)
		return 0
	}
	@[export: 'v_native_fork'; markused]
	fn bare_native_fork() i32 {
		// Capture the newest registration. Its older chain stays valid while
		// callbacks run, even if another thread registers a new callback.
		bare_compat_lock(&bare_atfork_lock)
		callbacks := bare_atfork_head
		bare_compat_unlock(&bare_atfork_lock)
		mut last := callbacks
		for node := callbacks; node != unsafe { nil }; node = node.older {
			if node.prepare != 0 { C.v_pthread_once_call(node.prepare) }
			last = node
		}
		bare_compat_lock(&bare_atfork_lock)
		bare_compat_lock(&bare_credential_mutex)
		bare_heap_lock()
		mut result := i32(0)
		$if amd64 {
			result = i32(bare_native_result(sys_call0(u64(C.SYS_fork))))
		} $else {
			result = i32(bare_native_result(sys_call5(u64(C.SYS_clone), 17, 0, 0, 0, 0)))
		}
		if result == 0 {
			bare_credential_threads = 0
			mut thread := bare_pthread_current_get()
			if thread != unsafe { nil } {
				thread.metadata_lock = 0
				bare_store_u32(&thread.tid, u32(sys_call0(u64(C.SYS_gettid))))
			}
		}
		fork_errno := bf_native_errno_cell
		bare_heap_unlock()
		bare_compat_unlock(&bare_credential_mutex)
		bare_compat_unlock(&bare_atfork_lock)
		for node := last; node != unsafe { nil }; node = node.newer {
			callback := if result == 0 { node.child } else { node.parent }
			if callback != 0 { C.v_pthread_once_call(callback) }
			if node == callbacks { break }
		}
		if result < 0 { bf_native_errno_cell = fork_errno }
		return result
	}

	@[export: 'v_native_fchmodat'; markused]
	fn bare_native_fchmodat(fd i32, path uint, mode u32, flags i32) i32 {
		if flags == 0 {
			return i32(bare_native_result(sys_call3(bf_nr_fchmodat, u64(fd), u64(path), u64(mode))))
		}
		// fchmodat2 is syscall 452 in both supported Linux ABIs.
		return i32(bare_native_result(sys_call4(452, u64(fd), u64(path), u64(mode), u64(flags))))
	}
	@[export: 'v_native_futimesat'; markused]
	fn bare_native_futimesat(fd i32, path uint, times uint, flags i32) i32 {
		mut converted := [4]i64{}
		if times != 0 {
			input := unsafe { &i64(times) }
			for i in 0 .. 2 {
				seconds := unsafe { input[i * 2] }
				micros := unsafe { input[i * 2 + 1] }
				if micros < 0 || micros >= 1_000_000 {
					bf_native_errno_cell = i32(C.EINVAL)
					return -1
				}
				converted[i * 2] = seconds
				converted[i * 2 + 1] = micros * 1000
			}
		}
		pointer := if times == 0 { uint(0) } else { uint(&converted[0]) }
		return i32(bare_native_result(sys_call4(u64(C.SYS_utimensat), u64(fd), u64(path), u64(pointer), u64(flags))))
	}
	@[export: 'v_native_truncate'; markused]
	fn bare_native_truncate(path uint, size i64) i32 {
		return i32(bare_native_result(sys_call2(u64(C.SYS_truncate), u64(path), u64(size))))
	}
	@[export: 'v_native_tcgetattr'; markused]
	fn bare_native_tcgetattr(fd i32, settings uint) i32 {
		if settings == 0 {
			bf_native_errno_cell = i32(C.EFAULT)
			return -1
		}
		return C.v_native_termios_get(fd, settings)
	}
	@[export: 'v_native_tcsetattr'; markused]
	fn bare_native_tcsetattr(fd i32, action i32, settings uint) i32 {
		if action < 0 || action > 2 {
			bf_native_errno_cell = i32(C.EINVAL)
			return -1
		}
		if settings == 0 {
			bf_native_errno_cell = i32(C.EFAULT)
			return -1
		}
		return C.v_native_termios_set(fd, action, settings)
	}
	@[export: 'v_native_getauxval'; markused]
	fn bare_native_getauxval(kind uint) uint {
		pointer := C.v_freestanding_program_auxiliary()
		if pointer != 0 {
			values := unsafe { &uint(pointer) }
			mut offset := 0
			for unsafe { values[offset] } != 0 {
				if unsafe { values[offset] } == kind { return unsafe { values[offset + 1] } }
				offset += 2
			}
		}
		bf_native_errno_cell = i32(Errno.enoent)
		return 0
	}
	@[export: 'v_native_secure_getenv'; markused]
	fn bare_native_secure_getenv(name uint) uint {
		saved_errno := bf_native_errno_cell
		secure := bare_native_getauxval(23) != 0 || sys_getuid() != sys_call0(u64(C.SYS_geteuid))
			|| sys_call0(u64(C.SYS_getgid)) != sys_call0(u64(C.SYS_getegid))
		bf_native_errno_cell = saved_errno
		if secure { return 0 }
		return uint(C.getenv(unsafe { &char(name) }))
	}
	@[export: 'v_native_prctl'; markused]
	fn bare_native_prctl(option i32, a uint, b uint, c uint, d uint) i32 {
		return i32(bare_native_result(sys_call5(u64(C.SYS_prctl), u64(option), u64(a), u64(b), u64(c), u64(d))))
	}
	@[export: 'v_native_sync'; markused]
	fn bare_native_sync() { sys_call0(u64(C.SYS_sync)) }
	@[export: 'v_native_processors'; markused]
	fn bare_native_processors(configured i32) i32 {
		return i32(C.sysconf(if configured != 0 {
			i32(C._SC_NPROCESSORS_CONF)
		} else {
			i32(C._SC_NPROCESSORS_ONLN)
		}))
	}
	@[export: 'v_native_pagesize'; markused]
	fn bare_native_pagesize() i32 { return i32(C.sysconf(i32(C._SC_PAGESIZE))) }
}
