module builtin

$if (freestanding || syscalls) && !native && !i386 {
	@[export: 'v_macos_kernel_mach_trap6'; markused; no_profile; no_trace]
	fn mac_kernel_mach_trap6(index u32, a1 u64, a2 u64, a3 u64, a4 u64, a5 u64, a6 u64) u64 {
		mut result := u64(0)
		$if amd64 {
			number := u64(0x1000000) | u64(index)
			asm volatile amd64 {
				mov rax, number
				mov r10, a4
				mov r8, a5
				mov r9, a6
				mov rdx, a3
				syscall
				mov result, rax
				; =r (result)
				; r (number)
				  D (a1)
				  S (a2)
				  g (a3)
				  g (a4)
				  g (a5)
				  g (a6)
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
		} $else {
			number := -i64(index)
			asm volatile arm64 {
				mov x0, a1
				mov x1, a2
				mov x2, a3
				mov x3, a4
				mov x4, a5
				mov x5, a6
				mov x16, number
				svc #0x80
				mov result, x0
				; =r (result)
				; r (number)
				  r (a1)
				  r (a2)
				  r (a3)
				  r (a4)
				  r (a5)
				  r (a6)
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
		}
		return result
	}

	// Carry is decoded independently of the primary result. No errno cell is
	// accessed here, including before a newly created worker activates TLS.
	@[export: 'v_macos_kernel_bsd_call6'; markused; no_profile; no_trace]
	fn mac_kernel_bsd_call6(number u32, a1 u64, a2 u64, a3 u64, a4 u64, a5 u64, a6 u64, value &u64) i32 {
		result, error := sys_call6(u64(0x2000000) | u64(number), a1, a2, a3, a4, a5, a6)
		if value != unsafe { nil } {
			unsafe { *value = if error == 0 { result } else { u64(0) } }
		}
		return i32(error)
	}

	@[no_profile; no_trace]
	fn mac_kernel_bsd6(number u32, a1 u64, a2 u64, a3 u64, a4 u64, a5 u64, a6 u64) i32 {
		return mac_kernel_bsd_call6(number, a1, a2, a3, a4, a5, a6, unsafe { nil })
	}

	@[export: 'v_macos_kernel_task_self'; markused; no_profile; no_trace]
	fn mac_kernel_task_self() u32 {
		return u32(mac_kernel_mach_trap6(28, 0, 0, 0, 0, 0, 0))
	}

	@[export: 'v_macos_kernel_thread_self'; markused; no_profile; no_trace]
	fn mac_kernel_thread_self() u32 {
		return u32(mac_kernel_mach_trap6(27, 0, 0, 0, 0, 0, 0))
	}

	@[export: 'v_macos_kernel_reply_port'; markused; no_profile; no_trace]
	fn mac_kernel_reply_port() u32 {
		return u32(mac_kernel_mach_trap6(26, 0, 0, 0, 0, 0, 0))
	}

	@[no_profile; no_trace]
	fn mac_kernel_port_release(task u32, port u32) i32 {
		return i32(mac_kernel_mach_trap6(18, task, port, 0, 0, 0, 0))
	}

	@[export: 'v_macos_kernel_port_deallocate'; markused; no_profile; no_trace]
	fn mac_kernel_port_deallocate(port u32) i32 {
		task := mac_kernel_task_self()
		if task == 0 { return i32(C.KERN_RESOURCE_SHORTAGE) }
		code := mac_kernel_port_release(task, port)
		mac_kernel_port_release(task, task)
		return code
	}

	@[export: 'v_macos_kernel_port_destroy_receive'; markused; no_profile; no_trace]
	fn mac_kernel_port_destroy_receive(port u32) i32 {
		task := mac_kernel_task_self()
		if task == 0 { return i32(C.KERN_RESOURCE_SHORTAGE) }
		code := i32(mac_kernel_mach_trap6(19, task, port, u64(C.MACH_PORT_RIGHT_RECEIVE), ~u64(0), 0, 0))
		mac_kernel_port_release(task, task)
		return code
	}

	@[no_profile; no_trace]
	fn mac_kernel_pair(low u32, high u32) u64 {
		return u64(low) | (u64(high) << 32)
	}

	// A fresh receive right isolates each operation. Retrying an interrupted
	// receive never resends an already accepted create/remap request.
	@[no_profile; no_trace]
	fn mac_kernel_rpc(message &C.mach_msg_header_t, capacity u32, task u32, destination u32, id i32, descriptors u32) i32 {
		reply := mac_kernel_reply_port()
		if reply == 0 { return i32(C.KERN_RESOURCE_SHORTAGE) }
		unsafe {
			message.msgh_bits = u32(C.MACH_MSG_TYPE_COPY_SEND) | (u32(C.MACH_MSG_TYPE_MAKE_SEND_ONCE) << 8)
			if descriptors != 0 { message.msgh_bits |= u32(C.MACH_MSGH_BITS_COMPLEX) }
			message.msgh_remote_port = destination
			message.msgh_local_port = reply
			message.msgh_voucher_port = 0
			message.msgh_id = id
		}
		options := u64(0x200000000) | u64(C.MACH_SEND_MSG) | u64(C.MACH_RCV_MSG)
		bits_size := mac_kernel_pair(message.msgh_bits, message.msgh_size)
		ports := mac_kernel_pair(destination, reply)
		voucher_id := mac_kernel_pair(0, u32(id))
		descriptors_reply := mac_kernel_pair(descriptors, reply)
		mut code := mac_kernel_msg2(message, options, bits_size, ports, voucher_id, descriptors_reply, capacity, 0)
		for code == i32(C.MACH_SEND_INTERRUPTED) {
			code = mac_kernel_msg2(message, options, bits_size, ports, voucher_id, descriptors_reply, capacity, 0)
		}
		for code == i32(C.MACH_RCV_INTERRUPTED) {
			code = mac_kernel_msg2(message, options & ~u64(C.MACH_SEND_MSG), bits_size & u64(0xffffffff), ports, voucher_id, descriptors_reply, capacity, 0)
		}
		mac_kernel_mach_trap6(19, task, reply, u64(C.MACH_PORT_RIGHT_RECEIVE), ~u64(0), 0, 0)
		if code != i32(C.MACH_MSG_SUCCESS) { return code }
		if message.msgh_id != id + 100 { return i32(C.MIG_REPLY_MISMATCH) }
		return 0
	}

	@[no_profile; no_trace]
	fn mac_kernel_simple_reply(message &C.mach_msg_header_t, expected u32) i32 {
		if message.msgh_bits & u32(C.MACH_MSGH_BITS_COMPLEX) != 0
			|| message.msgh_size < u32(sizeof(C.mig_reply_error_t)) {
			return i32(C.MIG_TYPE_ERROR)
		}
		error := unsafe { &C.mig_reply_error_t(message) }
		if error.RetCode != 0 { return error.RetCode }
		return if message.msgh_size == expected { i32(0) } else { i32(C.MIG_TYPE_ERROR) }
	}

	@[export: 'v_macos_kernel_port_type'; markused; no_profile; no_trace]
	fn mac_kernel_port_type(port u32, kind &u32) i32 {
		mut message := MacKernelMessage{ room: [512]u64{} }
		unsafe {
			*kind = 0
			message.port_request.Head.msgh_size = u32(sizeof(C.__Request__mach_port_type_t))
			message.port_request.NDR = C.NDR_record_t{ int_rep: 1 }
			message.port_request.name = port
		}
		task := mac_kernel_task_self()
		if task == 0 { return i32(C.KERN_RESOURCE_SHORTAGE) }
		mut code := mac_kernel_rpc(unsafe { &message.header }, u32(sizeof(MacKernelMessage)), task, task, 3201, 0)
		mac_kernel_port_release(task, task)
		if code != 0 { return code }
		code = mac_kernel_simple_reply(unsafe { &message.header }, u32(sizeof(C.__Reply__mach_port_type_t)))
		if code == 0 { unsafe { *kind = message.port_reply.ptype } }
		return code
	}

	@[export: 'v_macos_kernel_thread_create_running'; markused; no_profile; no_trace]
	fn mac_kernel_thread_create_running(flavor i32, state &u32, count u32, thread &u32) i32 {
		unsafe { *thread = 0 }
		offset := u32(__offsetof(C.__Request__thread_create_running_t, new_state))
		capacity := (u32(sizeof(C.__Request__thread_create_running_t)) - offset) / u32(sizeof(u32))
		if state == unsafe { nil } || count > capacity { return i32(C.KERN_INVALID_ARGUMENT) }
		mut message := MacKernelMessage{ room: [512]u64{} }
		unsafe {
			message.thread_request.Head.msgh_size = offset + count * u32(sizeof(u32))
			message.thread_request.NDR = C.NDR_record_t{ int_rep: 1 }
			message.thread_request.flavor = flavor
			message.thread_request.new_stateCnt = count
			output := &u32(&message.thread_request.new_state[0])
			for i in 0 .. count { output[i] = state[i] }
		}
		task := mac_kernel_task_self()
		if task == 0 { return i32(C.KERN_RESOURCE_SHORTAGE) }
		code := mac_kernel_rpc(unsafe { &message.header }, u32(sizeof(MacKernelMessage)), task, task, 3412, 0)
		mac_kernel_port_release(task, task)
		if code != 0 { return code }
		unsafe {
			if message.header.msgh_bits & u32(C.MACH_MSGH_BITS_COMPLEX) == 0 {
				status := mac_kernel_simple_reply(&message.header, u32(sizeof(C.__Reply__thread_create_running_t)))
				return if status == 0 { i32(C.MIG_TYPE_ERROR) } else { status }
			}
			port := message.thread_reply.child_act.name
			if message.header.msgh_size != u32(sizeof(C.__Reply__thread_create_running_t))
				|| message.thread_reply.msgh_body.msgh_descriptor_count != 1
				|| message.thread_reply.child_act.type != u8(C.MACH_MSG_PORT_DESCRIPTOR)
				|| message.thread_reply.child_act.disposition != u8(C.MACH_MSG_TYPE_MOVE_SEND)
				|| port == 0 || port == ~u32(0) {
				return i32(C.MIG_TYPE_ERROR)
			}
			*thread = port
		}
		return 0
	}

	@[export: 'v_macos_kernel_thread_terminate'; markused; no_profile; no_trace]
	fn mac_kernel_thread_terminate(thread u32) i32 {
		mut message := MacKernelMessage{ room: [512]u64{} }
		unsafe { message.terminate_request.Head.msgh_size = u32(sizeof(C.__Request__thread_terminate_t)) }
		task := mac_kernel_task_self()
		if task == 0 { return i32(C.KERN_RESOURCE_SHORTAGE) }
		self := mac_kernel_thread_self()
		if self != 0 { mac_kernel_port_release(task, self) }
		if thread == self && self != 0 {
			mac_kernel_port_release(task, task)
			return i32(C.KERN_INVALID_ARGUMENT)
		}
		code := mac_kernel_rpc(unsafe { &message.header }, u32(sizeof(MacKernelMessage)), task, thread, 3600, 0)
		mac_kernel_port_release(task, task)
		if code != 0 { return code }
		return mac_kernel_simple_reply(unsafe { &message.header }, u32(sizeof(C.__Reply__thread_terminate_t)))
	}

	@[export: 'v_macos_kernel_semaphore_create'; markused; no_profile; no_trace]
	fn mac_kernel_semaphore_create(policy i32, value i32, semaphore &u32) i32 {
		mut message := MacKernelMessage{ room: [512]u64{} }
		unsafe {
			*semaphore = 0
			message.semaphore_request.Head.msgh_size = u32(sizeof(C.__Request__semaphore_create_t))
			message.semaphore_request.NDR = C.NDR_record_t{ int_rep: 1 }
			message.semaphore_request.policy = policy
			message.semaphore_request.value = value
		}
		task := mac_kernel_task_self()
		if task == 0 { return i32(C.KERN_RESOURCE_SHORTAGE) }
		code := mac_kernel_rpc(unsafe { &message.header }, u32(sizeof(MacKernelMessage)), task, task, 3418, 0)
		mac_kernel_port_release(task, task)
		if code != 0 { return code }
		unsafe {
			if message.header.msgh_bits & u32(C.MACH_MSGH_BITS_COMPLEX) == 0 {
				status := mac_kernel_simple_reply(&message.header, u32(sizeof(C.__Reply__semaphore_create_t)))
				return if status == 0 { i32(C.MIG_TYPE_ERROR) } else { status }
			}
			port := message.semaphore_reply.semaphore.name
			if message.header.msgh_size != u32(sizeof(C.__Reply__semaphore_create_t))
				|| message.semaphore_reply.msgh_body.msgh_descriptor_count != 1
				|| message.semaphore_reply.semaphore.type != u8(C.MACH_MSG_PORT_DESCRIPTOR)
				|| message.semaphore_reply.semaphore.disposition != u8(C.MACH_MSG_TYPE_MOVE_SEND)
				|| port == 0 || port == ~u32(0) {
				return i32(C.MIG_TYPE_ERROR)
			}
			*semaphore = port
		}
		return 0
	}

	@[export: 'v_macos_kernel_semaphore_destroy'; markused; no_profile; no_trace]
	fn mac_kernel_semaphore_destroy(semaphore u32) i32 {
		mut message := MacKernelMessage{ room: [512]u64{} }
		unsafe {
			message.destroy_request.Head.msgh_size = u32(sizeof(C.__Request__semaphore_destroy_t))
			message.destroy_request.msgh_body.msgh_descriptor_count = 1
			message.destroy_request.semaphore.name = semaphore
			message.destroy_request.semaphore.disposition = u8(C.MACH_MSG_TYPE_COPY_SEND)
			message.destroy_request.semaphore.type = u8(C.MACH_MSG_PORT_DESCRIPTOR)
		}
		task := mac_kernel_task_self()
		if task == 0 { return i32(C.KERN_RESOURCE_SHORTAGE) }
		code := mac_kernel_rpc(unsafe { &message.header }, u32(sizeof(MacKernelMessage)), task, task, 3419, 1)
		mac_kernel_port_release(task, task)
		if code != 0 { return code }
		return mac_kernel_simple_reply(unsafe { &message.header }, u32(sizeof(C.__Reply__semaphore_destroy_t)))
	}

	@[export: 'v_macos_kernel_semaphore_wait'; markused; no_profile; no_trace]
	fn mac_kernel_semaphore_wait(semaphore u32) i32 {
		return i32(mac_kernel_mach_trap6(36, semaphore, 0, 0, 0, 0, 0))
	}

	@[export: 'v_macos_kernel_semaphore_signal'; markused; no_profile; no_trace]
	fn mac_kernel_semaphore_signal(semaphore u32) i32 {
		return i32(mac_kernel_mach_trap6(33, semaphore, 0, 0, 0, 0, 0))
	}

	@[export: 'v_macos_kernel_vm_allocate'; markused; no_profile; no_trace]
	fn mac_kernel_vm_allocate(address &u64, size u64, flags i32) i32 {
		task := mac_kernel_task_self()
		if task == 0 { return i32(C.KERN_RESOURCE_SHORTAGE) }
		code := i32(mac_kernel_mach_trap6(10, task, u64(address), size, u64(u32(flags)), 0, 0))
		mac_kernel_port_release(task, task)
		return code
	}

	@[export: 'v_macos_kernel_vm_deallocate'; markused; no_profile; no_trace]
	fn mac_kernel_vm_deallocate(address u64, size u64) i32 {
		task := mac_kernel_task_self()
		if task == 0 { return i32(C.KERN_RESOURCE_SHORTAGE) }
		code := i32(mac_kernel_mach_trap6(12, task, address, size, 0, 0, 0))
		mac_kernel_port_release(task, task)
		return code
	}

	@[export: 'v_macos_kernel_vm_remap'; markused; no_profile; no_trace]
	fn mac_kernel_vm_remap(source u64, size u64, target &u64, flags i32, copy i32, current &i32, maximum &i32) i32 {
		mut message := MacKernelMessage{ room: [512]u64{} }
		unsafe {
			*current = 0
			*maximum = 0
		}
		task := mac_kernel_task_self()
		if task == 0 { return i32(C.KERN_RESOURCE_SHORTAGE) }
		unsafe {
			message.remap_request.Head.msgh_size = u32(sizeof(C.__Request__mach_vm_remap_t))
			message.remap_request.msgh_body.msgh_descriptor_count = 1
			message.remap_request.src_task.name = task
			message.remap_request.src_task.disposition = u8(C.MACH_MSG_TYPE_COPY_SEND)
			message.remap_request.src_task.type = u8(C.MACH_MSG_PORT_DESCRIPTOR)
			message.remap_request.NDR = C.NDR_record_t{ int_rep: 1 }
			message.remap_request.target_address = *target
			message.remap_request.size = size
			message.remap_request.mask = 0
			message.remap_request.flags = flags
			message.remap_request.src_address = source
			message.remap_request.copy = copy
			message.remap_request.inheritance = i32(C.VM_INHERIT_SHARE)
		}
		mut code := mac_kernel_rpc(unsafe { &message.header }, u32(sizeof(MacKernelMessage)), task, task, 4813, 1)
		mac_kernel_port_release(task, task)
		if code != 0 { return code }
		code = mac_kernel_simple_reply(unsafe { &message.header }, u32(sizeof(C.__Reply__mach_vm_remap_t)))
		if code == 0 {
			unsafe {
				*target = message.remap_reply.target_address
				*current = message.remap_reply.cur_protection
				*maximum = message.remap_reply.max_protection
			}
		}
		return code
	}

	@[export: 'v_macos_kernel_ulock_wait'; markused; no_profile; no_trace]
	fn mac_kernel_ulock_wait(operation u32, address voidptr, value u64, timeout u32) i32 {
		if operation & u32(0x1000000) != 0 { return 22 }
		return mac_kernel_bsd6(515, operation, u64(address), value, timeout, 0, 0)
	}

	@[export: 'v_macos_kernel_ulock_wait2'; markused; no_profile; no_trace]
	fn mac_kernel_ulock_wait2(operation u32, address voidptr, value u64, timeout u64) i32 {
		if operation & u32(0x1000000) != 0 { return 22 }
		return mac_kernel_bsd6(544, operation, u64(address), value, timeout, 0, 0)
	}

	@[export: 'v_macos_kernel_ulock_wake'; markused; no_profile; no_trace]
	fn mac_kernel_ulock_wake(operation u32, address voidptr, value u64) i32 {
		if operation & u32(0x1000000) != 0 { return 22 }
		return mac_kernel_bsd6(516, operation, u64(address), value, 0, 0, 0)
	}

	@[export: 'v_macos_kernel_thread_exit'; markused; no_profile; no_trace]
	fn mac_kernel_thread_exit() i32 {
		return mac_kernel_thread_exit_mapped(0, 0, 0, 0)
	}

	@[export: 'v_macos_kernel_thread_exit_mapped'; markused; no_profile; no_trace]
	fn mac_kernel_thread_exit_mapped(address u64, size u64, thread u32, semaphore u32) i32 {
		return mac_kernel_bsd6(361, address, size, thread, semaphore, 0, 0)
	}

	@[export: 'v_macos_kernel_page_size'; markused; no_profile; no_trace]
	fn mac_kernel_page_size(size &u64) i32 {
		mut name := [i32(6), i32(7)]!
		mut value := i32(0)
		mut length := u64(sizeof(i32))
		code := mac_kernel_bsd6(202, u64(&name[0]), 2, u64(&value), u64(&length), 0, 0)
		unsafe { *size = 0 }
		if code != 0 { return code }
		if length != sizeof(i32) || value <= 0 || value & (value - 1) != 0 { return 5 }
		unsafe { *size = u64(value) }
		return 0
	}
}
