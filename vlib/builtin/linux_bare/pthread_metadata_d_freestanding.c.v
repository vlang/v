module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#include <sys/prctl.h>
	fn C.v_pthread_sched_read(uint) i32
	fn C.v_pthread_sched_write(uint, i32)

	fn bare_pthread_metadata_leave(mut thread BarePthread) {
		// Queries hold this lock through their kernel operation. Publish zero
		// before kernel exit so a retained joinable handle cannot select a reused TID.
		bare_compat_lock(&thread.metadata_lock)
		bare_store_u32(&thread.tid, 0)
		bare_compat_unlock(&thread.metadata_lock)
	}
	// The kernel grows the main stack downwards up to RLIMIT_STACK or the
	// preceding mapping. Other implicit threads report their current mapping.
	fn bare_pthread_stack_mapping(sample uint, main_thread bool, mut attributes BarePthreadAttributes) i32 {
		fd := bare_native_openat(-100, uint(c'/proc/self/maps'), i32(C.O_CLOEXEC), 0)
		if fd < 0 { return bf_native_errno_cell }
		defer { sys_call1(u64(C.SYS_close), u64(fd)) }
		mut buffer := [4096]u8{}
		mut line := [96]u8{}
		mut used := 0
		mut preceding := uint(0)
		mut preceding_guard := uint(0)
		for {
			count := i64(sys_call3(u64(C.SYS_read), u64(fd), u64(&buffer[0]), u64(buffer.len)))
			if count == -i64(C.EINTR) { continue }
			if count < 0 { return i32(-count) }
			if count == 0 { return i32(C.ENOENT) }
			for i in 0 .. int(count) {
				if buffer[i] != 10 {
					if used < line.len {
						line[used] = buffer[i]
						used++
					}
					continue
				}
				start, separator := bare_fortify_hex(&line, used, 0)
				end, last := bare_fortify_hex(&line, used, separator + 1)
				no_access := last + 3 < used && line[last + 1] == `-` && line[last + 2] == `-`
					&& line[last + 3] == `-`
				used = 0
				if start <= sample && sample < end {
					mut base := start
					if main_thread {
						mut limits := [2]u64{}
						status := i64(sys_call4(u64(C.SYS_prlimit64), 0, 3, 0, u64(&limits[0])))
						if status < 0 { return i32(-status) }
						base = if limits[0] < end - preceding {
							end - uint(limits[0])
						} else {
							preceding
						}
					}
					// An implicit V worker uses the runtime's default one-page guard.
					// The initial kernel stack has no separately allocated guard.
					attributes.guard_size = if !main_thread && preceding == start
						&& preceding_guard >= attributes.guard_size {
						attributes.guard_size
					} else {
						uint(0)
					}
					attributes.stack_base = base
					attributes.stack_size = end - base
					return 0
				}
				preceding_guard = if no_access { end - start } else { uint(0) }
				preceding = end
			}
		}
		return i32(C.ENOENT)
	}

	@[export: 'v_pthread_getattr'; markused]
	fn bare_pthread_getattr(handle uint, output uint) i32 {
		saved := bf_native_errno_cell
		defer { bf_native_errno_cell = saved }
		mut thread := unsafe { &BarePthread(handle) }
		bare_compat_lock(&thread.metadata_lock)
		defer { bare_compat_unlock(&thread.metadata_lock) }
		if bare_load_u32(&thread.tid) == 0 { return i32(C.ESRCH) }
		bare_pthread_attr_init(output)
		mut attributes := unsafe { &BarePthreadAttributes(output) }
		if bare_load_u32(&thread.state) == 2 {
			attributes.detached = i32(C.PTHREAD_CREATE_DETACHED)
		}
		if thread.implicit != 0 {
			return bare_pthread_stack_mapping(thread.stack_sample, thread.implicit == 1, mut attributes)
		}
		mapping := unsafe { (&BareThreadContext(thread.backend)).mapping }
		attributes.stack_base = uint(mapping.stack)
		attributes.stack_size = uint(mapping.stack_size)
		attributes.guard_size = uint(mapping.bare_stack_header().guard_size)
		return 0
	}
	@[export: 'v_pthread_stack_bounds'; markused]
	fn bare_pthread_stack_bounds(input uint, base uint, size uint) i32 {
		attributes := unsafe { &BarePthreadAttributes(input) }
		unsafe { *(&uint(base)) = attributes.stack_base }
		return bare_pthread_attr_getstack(input, size)
	}

	fn bare_pthread_comm_path(tid u32, output uint) {
		mut used := bare_string_strlcpy(output, uint(c'/proc/self/task/'), 64)
		used += bare_name_decimal(output + used, tid)
		bare_string_strlcpy(output + used, uint(c'/comm'), 16)
	}
	@[export: 'v_pthread_getname'; markused]
	fn bare_pthread_getname(handle uint, output uint, size uint) i32 {
		if size < 16 { return i32(C.ERANGE) }
		saved := bf_native_errno_cell
		defer { bf_native_errno_cell = saved }
		mut thread := unsafe { &BarePthread(handle) }
		bare_compat_lock(&thread.metadata_lock)
		defer { bare_compat_unlock(&thread.metadata_lock) }
		tid := bare_load_u32(&thread.tid)
		if tid == 0 { return i32(C.ESRCH) }
		if handle == uint(bare_cancel_current()) {
			result := i64(sys_call5(u64(C.SYS_prctl), u64(C.PR_GET_NAME), u64(output), 0, 0, 0))
			return if result < 0 { i32(-result) } else { i32(0) }
		}
		mut path := [64]u8{}
		bare_pthread_comm_path(tid, uint(&path[0]))
		fd := bare_native_openat(-100, uint(&path[0]), i32(C.O_RDONLY) | i32(C.O_CLOEXEC), 0)
		if fd < 0 { return bf_native_errno_cell }
		defer { sys_call1(u64(C.SYS_close), u64(fd)) }
		mut text := [32]u8{}
		mut used := uint(0)
		for used < 32 {
			count := i64(sys_call3(u64(C.SYS_read), u64(fd), u64(&text[0]) + u64(used), u64(32 - used)))
			if count == -i64(C.EINTR) { continue }
			if count < 0 { return i32(-count) }
			if count == 0 { break }
			used += uint(count)
		}
		if used == 0 || text[used - 1] != `\n` { return i32(C.EIO) }
		used--
		if used >= 16 { return i32(C.ERANGE) }
		return i32(bare_name_copy(output, size, unsafe { tos(&text[0], int(used)) }))
	}
	@[export: 'v_pthread_setname'; markused]
	fn bare_pthread_setname(handle uint, name uint) i32 {
		mut length := uint(0)
		for length < 16 && unsafe { (&u8(name))[length] } != 0 { length++ }
		if length == 16 { return i32(C.ERANGE) }
		saved := bf_native_errno_cell
		defer { bf_native_errno_cell = saved }
		mut thread := unsafe { &BarePthread(handle) }
		bare_compat_lock(&thread.metadata_lock)
		defer { bare_compat_unlock(&thread.metadata_lock) }
		tid := bare_load_u32(&thread.tid)
		if tid == 0 { return i32(C.ESRCH) }
		if handle == uint(bare_cancel_current()) {
			result := i64(sys_call5(u64(C.SYS_prctl), u64(C.PR_SET_NAME), u64(name), 0, 0, 0))
			return if result < 0 { i32(-result) } else { i32(0) }
		}
		mut path := [64]u8{}
		bare_pthread_comm_path(tid, uint(&path[0]))
		fd := bare_native_openat(-100, uint(&path[0]), i32(C.O_WRONLY) | i32(C.O_CLOEXEC), 0)
		if fd < 0 { return bf_native_errno_cell }
		defer { sys_call1(u64(C.SYS_close), u64(fd)) }
		// A zero-length name still needs a procfs write to clear the old name.
		count := i64(sys_call3(u64(C.SYS_write), u64(fd), u64(name), u64(length + 1)))
		return if count < 0 {
			i32(-count)
		} else if uint(count) != length + 1 {
			i32(C.EIO)
		} else {
			i32(0)
		}
	}
	@[export: 'v_pthread_getsched'; markused]
	fn bare_pthread_getsched(handle uint, policy uint, parameters uint) i32 {
		mut thread := unsafe { &BarePthread(handle) }
		bare_compat_lock(&thread.metadata_lock)
		defer { bare_compat_unlock(&thread.metadata_lock) }
		tid := bare_load_u32(&thread.tid)
		if tid == 0 { return i32(C.ESRCH) }
		kind := i64(sys_call1(u64(C.SYS_sched_getscheduler), u64(tid)))
		if kind < 0 { return i32(-kind) }
		mut priority := i32(0)
		result := i64(sys_call2(u64(C.SYS_sched_getparam), u64(tid), u64(&priority)))
		if result < 0 { return i32(-result) }
		unsafe { *(&i32(policy)) = i32(kind) }
		C.v_pthread_sched_write(parameters, priority)
		return 0
	}
	@[export: 'v_pthread_setsched'; markused]
	fn bare_pthread_setsched(handle uint, policy i32, parameters uint) i32 {
		priority := C.v_pthread_sched_read(parameters)
		mut thread := unsafe { &BarePthread(handle) }
		bare_compat_lock(&thread.metadata_lock)
		defer { bare_compat_unlock(&thread.metadata_lock) }
		tid := bare_load_u32(&thread.tid)
		if tid == 0 { return i32(C.ESRCH) }
		result := i64(sys_call3(u64(C.SYS_sched_setscheduler), u64(tid), u64(policy), u64(&priority)))
		return if result < 0 { i32(-result) } else { i32(0) }
	}
	@[export: 'v_pthread_setprio'; markused]
	fn bare_pthread_setprio(handle uint, priority i32) i32 {
		mut thread := unsafe { &BarePthread(handle) }
		bare_compat_lock(&thread.metadata_lock)
		defer { bare_compat_unlock(&thread.metadata_lock) }
		tid := bare_load_u32(&thread.tid)
		if tid == 0 { return i32(C.ESRCH) }
		result := i64(sys_call2(u64(C.SYS_sched_setparam), u64(tid), u64(&priority)))
		return if result < 0 { i32(-result) } else { i32(0) }
	}
	@[export: 'v_pthread_affinity'; markused]
	fn bare_pthread_affinity(handle uint, size uint, mask uint, setting i32) i32 {
		mut thread := unsafe { &BarePthread(handle) }
		bare_compat_lock(&thread.metadata_lock)
		defer { bare_compat_unlock(&thread.metadata_lock) }
		tid := bare_load_u32(&thread.tid)
		if tid == 0 { return i32(C.ESRCH) }
		result := bare_scheduler_affinity_raw(i32(tid), size, mask, setting != 0)
		if result < 0 { return i32(-result) }
		return 0
	}
}
