module builtin

$if (freestanding || syscalls) && !native && !i386 {

	// bf_mach_task_self acquires a send right for the current task.
	// Release the reference with bf_mach_port_deallocate. Zero means acquisition failed.
	pub fn bf_mach_task_self() u32 {
		return mac_kernel_task_self()
	}

	// bf_mach_thread_self acquires a send right for the calling kernel thread.
	// Release the reference with bf_mach_port_deallocate. Zero means acquisition failed.
	pub fn bf_mach_thread_self() u32 {
		return mac_kernel_thread_self()
	}

	// bf_mach_reply_port creates a receive right, or returns zero on failure.
	// Release it with bf_mach_port_destroy_receive, rather than port_deallocate.
	pub fn bf_mach_reply_port() u32 {
		return mac_kernel_reply_port()
	}

	// bf_mach_port_deallocate releases one send/dead-name right reference.
	// It returns a native Mach status and does not update either errno cell.
	pub fn bf_mach_port_deallocate(port u32) i32 {
		return mac_kernel_port_deallocate(port)
	}

	// bf_mach_port_destroy_receive destroys a receive right and queued messages.
	// It returns a native Mach status and does not update either errno cell.
	pub fn bf_mach_port_destroy_receive(port u32) i32 {
		return mac_kernel_port_destroy_receive(port)
	}

	// bf_mach_port_type reads a port's right-kind bits without acquiring another reference.
	// It returns the type and native Mach status; an absent name gives KERN_INVALID_NAME.
	pub fn bf_mach_port_type(port u32) (u32, i32) {
		mut kind := u32(0)
		code := mac_kernel_port_type(port, &kind)
		return kind, code
	}

	// bf_mach_thread_create_running creates a kernel thread from native register state.
	// The caller owns its stack/TLS, which must stay live until the thread is terminated.
	// The returned send right belongs to the caller; the second result is a Mach status.
	pub fn bf_mach_thread_create_running(flavor i32, state &u32, count u32) (u32, i32) {
		mut thread := u32(0)
		code := mac_kernel_thread_create_running(flavor, state, count, &thread)
		return thread, code
	}

	// bf_mach_thread_terminate terminates another kernel thread without releasing its send right.
	// It returns a native Mach status and waits until the target cannot run user code again.
	// The calling thread is rejected with KERN_INVALID_ARGUMENT; use bf_mach_thread_exit for it.
	pub fn bf_mach_thread_terminate(thread u32) i32 {
		return mac_kernel_thread_terminate(thread)
	}

	// bf_mach_thread_exit terminates only the calling thread through raw BSD361.
	// It creates no temporary ports and needs no pthread registration. Success does not return.
	// A returned value is a normalized positive BF error; neither errno cell is changed.
	pub fn bf_mach_thread_exit() i32 {
		code := mac_kernel_thread_exit()
		return i32(bf_errno_code(unsafe { VlibcErrno(code) }))
	}

	// bf_thread_exit_mapped terminates a worker and releases its stack/TLS mapping in the kernel.
	// The kernel unmaps address/size, marks the thread terminated, signals semaphore when nonzero,
	// and releases one thread send-right reference when nonzero. The caller transfers that reference.
	// No user code runs after unmapping. Success never returns; failure returns a positive BF error.
	// Use process exit for the initial main thread, which has special kernel stack handling.
	pub fn bf_thread_exit_mapped(address u64, size u64, thread u32, semaphore u32) int {
		code := mac_kernel_thread_exit_mapped(address, size, thread, semaphore)
		return bf_errno_code(unsafe { VlibcErrno(code) })
	}

	// bf_mach_semaphore_create creates a kernel semaphore and returns its send right and Mach status.
	pub fn bf_mach_semaphore_create(policy i32, value i32) (u32, i32) {
		mut semaphore := u32(0)
		code := mac_kernel_semaphore_create(policy, value, &semaphore)
		return semaphore, code
	}

	// bf_mach_semaphore_destroy destroys a kernel semaphore and returns a native Mach status.
	// Release the resulting dead-name reference separately with bf_mach_port_deallocate.
	pub fn bf_mach_semaphore_destroy(semaphore u32) i32 {
		return mac_kernel_semaphore_destroy(semaphore)
	}

	// bf_mach_semaphore_wait waits for a signal and returns a native Mach status.
	// KERN_ABORTED is an interruption; the caller decides whether to retry.
	pub fn bf_mach_semaphore_wait(semaphore u32) i32 {
		return mac_kernel_semaphore_wait(semaphore)
	}

	// bf_mach_semaphore_signal posts one signal and returns a native Mach status.
	pub fn bf_mach_semaphore_signal(semaphore u32) i32 {
		return mac_kernel_semaphore_signal(semaphore)
	}

	// bf_mach_vm_allocate allocates zero-filled memory in the current task.
	// It returns the selected address and native Mach status; flags are VM_FLAGS_*.
	pub fn bf_mach_vm_allocate(address u64, size u64, flags i32) (u64, i32) {
		mut result := address
		code := mac_kernel_vm_allocate(&result, size, flags)
		return result, code
	}

	// bf_mach_vm_deallocate releases a current-task VM range and returns a native Mach status.
	pub fn bf_mach_vm_deallocate(address u64, size u64) i32 {
		return mac_kernel_vm_deallocate(address, size)
	}

	// bf_mach_vm_remap aliases or copies a current-task range with mask zero and shared inheritance.
	// It returns address, current protection, maximum protection, and native Mach status.
	pub fn bf_mach_vm_remap(source u64, size u64, target u64, flags i32, copy bool) (u64, i32, i32, i32) {
		mut result := target
		mut current := i32(0)
		mut maximum := i32(0)
		code := mac_kernel_vm_remap(source, size, &result, flags, i32(copy), &current,
			&maximum)
		return result, current, maximum, code
	}

	// bf_ulock_wait waits while an aligned live word equals value; zero timeout means indefinite.
	// timeout is in microseconds. Read the predicate before waiting so its page is resident.
	// It returns a normalized positive BF error without changing either errno cell.
	// ULF_NO_ERRNO is rejected with EINVAL; wakeups can be spurious or interrupted.
	pub fn bf_ulock_wait(operation u32, address voidptr, value u64, timeout u32) int {
		code := mac_kernel_ulock_wait(operation, address, value, timeout)
		return bf_errno_code(unsafe { VlibcErrno(code) })
	}

	// bf_ulock_wake wakes waiters using native ULock operation flags.
	// It returns a normalized positive BF error without changing either errno cell.
	// ULF_NO_ERRNO is rejected with EINVAL; ENOENT means no matching waiters exist.
	pub fn bf_ulock_wake(operation u32, address voidptr, value u64) int {
		code := mac_kernel_ulock_wake(operation, address, value)
		return bf_errno_code(unsafe { VlibcErrno(code) })
	}

	// bf_wait_u32 waits while an aligned live atomic word equals expected.
	// timeout_ns is relative nanoseconds; negative means indefinite. The caller must
	// reload its predicate after every return. Errors use positive BF numbers and
	// neither errno cell is changed. A zero timeout requests an immediate expiry.
	pub fn bf_wait_u32(address &u32, expected u32, timeout_ns i64) int {
		// ULock reserves zero for an indefinite wait, so use the smallest finite
		// interval for a zero deadline. The kernel still checks the word first.
		timeout := if timeout_ns < 0 {
			u64(0)
		} else if timeout_ns == 0 {
			u64(1)
		} else {
			u64(timeout_ns)
		}
		code := mac_kernel_ulock_wait2(1, address, u64(expected), timeout)
		return bf_errno_code(unsafe { VlibcErrno(code) })
	}

	// bf_wake_u32 wakes one waiter, or all waiters when all is true.
	// It returns a positive BF error, treating no waiters as success. It does not
	// report a woken count and does not update either errno cell.
	pub fn bf_wake_u32(address &u32, all bool) int {
		operation := if all { u32(0x101) } else { u32(1) }
		code := mac_kernel_ulock_wake(operation, address, 0)
		return if code == 2 { 0 } else { bf_errno_code(unsafe { VlibcErrno(code) }) }
	}

	// bf_page_size queries the native VM page size through raw sysctl.
	// It returns zero and a positive BF error on failure, without changing errno cells.
	pub fn bf_page_size() (u64, int) {
		mut size := u64(0)
		code := mac_kernel_page_size(&size)
		return size, bf_errno_code(unsafe { VlibcErrno(code) })
	}
}
