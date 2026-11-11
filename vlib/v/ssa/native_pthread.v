module ssa

// register_native_pthread_helpers supplies the small OS and pthread bridges
// normally linked from the standard library's C helper objects.
fn (mut b Builder) register_native_pthread_helpers() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	b.register_extern('pthread_attr_setdetachstate', b.i32_type, [ptr_i8, b.i32_type])
	detached_id := b.register_synthetic_c_function('v_sync_thread_create_detached_with_stack',
		b.i32_type, [ptr_i8, ptr_i8, b.u64_type])
	b.generate_native_pthread_create_body(detached_id, true)
	default_id := b.register_synthetic_c_function('v_sync_thread_create_detached', b.i32_type,
		[ptr_i8, ptr_i8])
	entry := b.m.add_block(default_id, 'detached_default_entry')
	start := b.func_add_argument(default_id, ptr_i8, 'start')
	argument := b.func_add_argument(default_id, ptr_i8, 'argument')
	stack_size := b.m.get_or_add_const(b.u64_type, b.thread_stack_size.str())
	callee := b.m.add_value(.func_ref, b.i32_type, 'v_sync_thread_create_detached_with_stack', detached_id)
	status := b.block_instr4(.call, entry, b.i32_type, callee, start, argument, stack_size)
	b.block_instr1(.ret, entry, b.void_type, status)
	b.register_native_tls_helpers()
	if b.m.target.is_linux() {
		ptr_i32 := b.m.type_store.get_ptr(b.i32_type)
		b.register_extern('pipe2', b.i32_type, [ptr_i32, b.i32_type])
		pipe_id := b.register_synthetic_c_function('v_os_process_pipe_cloexec', b.i32_type,
			[ptr_i32])
		pipe_entry := b.m.add_block(pipe_id, 'pipe_cloexec_entry')
		fds := b.func_add_argument(pipe_id, ptr_i32, 'fds')
		cloexec := b.m.get_or_add_const(b.i32_type, '524288')
		result := b.native_pthread_call(pipe_entry, 'pipe2', b.i32_type, [fds, cloexec])
		b.block_instr1(.ret, pipe_entry, b.void_type, result)
	}
}

// generate_native_pthread_create_body preserves initialization failures, stack
// size validation, detachment, and attribute cleanup from the C bridges.
fn (mut b Builder) generate_native_pthread_create_body(func_id int, detached bool) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_i32 := b.m.type_store.get_ptr(b.i32_type)
	ptr_u64 := b.m.type_store.get_ptr(b.u64_type)
	b.register_extern('abort', b.void_type, [])
	entry := b.m.add_block(func_id, 'pthread_create_entry')
	mut thread_ptr := ValueID(0)
	mut stack_size := ValueID(0)
	mut start := ValueID(0)
	mut argument := ValueID(0)
	if detached {
		start = b.func_add_argument(func_id, ptr_i8, 'start')
		argument = b.func_add_argument(func_id, ptr_i8, 'argument')
		stack_size = b.func_add_argument(func_id, b.u64_type, 'stack_size')
		thread_ptr = b.block_instr0(.alloca, entry, ptr_u64)
	} else {
		thread_ptr = b.func_add_argument(func_id, ptr_i8, 'thread')
		stack_size = b.func_add_argument(func_id, b.u64_type, 'stack_size')
		start = b.func_add_argument(func_id, ptr_i8, 'start')
		argument = b.func_add_argument(func_id, ptr_i8, 'argument')
	}
	// Both supported 64-bit libc ABIs fit pthread_attr_t in this aligned area.
	attr_type := b.m.type_store.get_array(b.u64_type, 16)
	storage := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(attr_type))
	attr := b.block_instr1(.bitcast, entry, ptr_i8, storage)
	result_slot := b.block_instr0(.alloca, entry, ptr_i32)
	zero := b.m.get_or_add_const(b.i32_type, '0')
	initialized := b.m.add_block(func_id, 'pthread_initialized')
	set_stack := b.m.add_block(func_id, 'pthread_set_stack')
	configure := b.m.add_block(func_id, 'pthread_configure')
	create := b.m.add_block(func_id, 'pthread_start')
	destroy := b.m.add_block(func_id, 'pthread_destroy_attributes')
	failed_destroy := b.m.add_block(func_id, 'pthread_destroy_failed')
	done := b.m.add_block(func_id, 'pthread_create_done')
	initial_status := b.native_pthread_call(entry, 'pthread_attr_init', b.i32_type, [attr])
	b.block_instr2(.store, entry, b.void_type, initial_status, result_slot)
	init_ok := b.block_instr2(.eq, entry, b.i1_type, initial_status, zero)
	b.block_instr3(.br, entry, b.void_type, init_ok, ValueID(initialized), ValueID(done))
	if detached {
		zero_size := b.m.get_or_add_const(b.u64_type, '0')
		has_stack_size := b.block_instr2(.ne, initialized, b.i1_type, stack_size, zero_size)
		b.block_instr3(.br, initialized, b.void_type, has_stack_size, ValueID(set_stack),
			ValueID(configure))
	} else {
		if !b.m.target.is_linux() {
			b.register_extern('pthread_attr_set_qos_class_np', b.i32_type,
				[ptr_i8, b.u32_type, b.i32_type])
			qos := b.m.get_or_add_const(b.u32_type, '25')
			b.native_pthread_call(initialized, 'pthread_attr_set_qos_class_np', b.i32_type,
				[attr, qos, zero])
		}
		b.block_instr1(.jmp, initialized, b.void_type, ValueID(set_stack))
	}
	stack_status := b.native_pthread_call(set_stack, 'pthread_attr_setstacksize', b.i32_type,
		[attr, stack_size])
	b.block_instr2(.store, set_stack, b.void_type, stack_status, result_slot)
	stack_ok := b.block_instr2(.eq, set_stack, b.i1_type, stack_status, zero)
	b.block_instr3(.br, set_stack, b.void_type, stack_ok, ValueID(configure), ValueID(destroy))
	if detached {
		detach_state := native_platform_constant('PTHREAD_CREATE_DETACHED', b.m.target) or { 1 }
		detached_value := b.m.get_or_add_const(b.i32_type, detach_state.str())
		detach_status := b.native_pthread_call(configure, 'pthread_attr_setdetachstate',
			b.i32_type, [attr, detached_value])
		b.block_instr2(.store, configure, b.void_type, detach_status, result_slot)
		detach_ok := b.block_instr2(.eq, configure, b.i1_type, detach_status, zero)
		b.block_instr3(.br, configure, b.void_type, detach_ok, ValueID(create), ValueID(destroy))
	} else {
		b.block_instr1(.jmp, configure, b.void_type, ValueID(create))
	}
	create_status := b.native_pthread_call(create, 'pthread_create', b.i32_type,
		[thread_ptr, attr, start, argument])
	b.block_instr2(.store, create, b.void_type, create_status, result_slot)
	b.block_instr1(.jmp, create, b.void_type, ValueID(destroy))
	destroy_status := b.native_pthread_call(destroy, 'pthread_attr_destroy', b.i32_type, [attr])
	destroy_ok := b.block_instr2(.eq, destroy, b.i1_type, destroy_status, zero)
	b.block_instr3(.br, destroy, b.void_type, destroy_ok, ValueID(done), ValueID(failed_destroy))
	b.native_pthread_call(failed_destroy, 'abort', b.void_type, [])
	b.block_instr0(.unreachable, failed_destroy, b.void_type)
	result := b.block_instr1(.load, done, b.i32_type, result_slot)
	b.block_instr1(.ret, done, b.void_type, result)
}

fn (mut b Builder) register_native_tls_helpers() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_u64 := b.m.type_store.get_ptr(b.u64_type)
	key_type := if b.m.target.is_linux() { b.u32_type } else { b.u64_type }
	ptr_key := b.m.type_store.get_ptr(key_type)
	b.register_extern('pthread_key_create', b.i32_type, [ptr_key, ptr_i8])
	b.register_extern('pthread_key_delete', b.i32_type, [key_type])
	b.register_extern('pthread_setspecific', b.i32_type, [key_type, ptr_i8])
	b.register_extern('pthread_getspecific', ptr_i8, [key_type])
	create_id := b.register_synthetic_c_function('v_sync_tls_create', b.i32_type, [ptr_i8])
	entry := b.m.add_block(create_id, 'tls_create_entry')
	storage := b.func_add_argument(create_id, ptr_i8, 'storage')
	key := b.block_instr0(.alloca, entry, ptr_key)
	nil_ptr := b.m.get_or_add_const(ptr_i8, '0')
	status := b.native_pthread_call(entry, 'pthread_key_create', b.i32_type, [key, nil_ptr])
	zero := b.m.get_or_add_const(b.i32_type, '0')
	ok := b.block_instr2(.eq, entry, b.i1_type, status, zero)
	store := b.m.add_block(create_id, 'tls_create_store')
	done := b.m.add_block(create_id, 'tls_create_done')
	b.block_instr3(.br, entry, b.void_type, ok, ValueID(store), ValueID(done))
	value := b.block_instr1(.load, store, key_type, key)
	wide := if key_type == b.u32_type {
		b.block_instr1(.zext, store, b.u64_type, value)
	} else {
		value
	}
	output := b.block_instr1(.bitcast, store, ptr_u64, storage)
	b.block_instr2(.store, store, b.void_type, wide, output)
	b.block_instr1(.jmp, store, b.void_type, ValueID(done))
	b.block_instr1(.ret, done, b.void_type, status)
	for operation in ['delete', 'set', 'get'] {
		ret_type := if operation == 'get' { ptr_i8 } else { b.i32_type }
		mut params := [ptr_i8]
		if operation == 'set' {
			params << ptr_i8
		}
		id := b.register_synthetic_c_function('v_sync_tls_${operation}', ret_type, params)
		block := b.m.add_block(id, 'tls_${operation}_entry')
		argument := b.func_add_argument(id, ptr_i8, 'storage')
		pointer := b.block_instr1(.bitcast, block, ptr_u64, argument)
		wide_key := b.block_instr1(.load, block, b.u64_type, pointer)
		native_key := if key_type == b.u32_type {
			b.block_instr1(.trunc, block, key_type, wide_key)
		} else {
			wide_key
		}
		mut args := [native_key]
		if operation == 'set' {
			args << b.func_add_argument(id, ptr_i8, 'value')
		}
		name := match operation {
			'delete' { 'pthread_key_delete' }
			'set' { 'pthread_setspecific' }
			else { 'pthread_getspecific' }
		}
		result := b.native_pthread_call(block, name, ret_type, args)
		b.block_instr1(.ret, block, b.void_type, result)
	}
}

fn (mut b Builder) native_pthread_call(block BlockID, name string, ret TypeID, args []ValueID) ValueID {
	callee := b.m.add_value(.func_ref, ret, name, b.fn_ids[name])
	return b.m.add_instr(.call, block, ret, [callee, ...args])
}
