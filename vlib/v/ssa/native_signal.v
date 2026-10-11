module ssa

fn (mut b Builder) register_native_signal_helpers() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	b.register_extern('sigaltstack', b.i32_type, [ptr_i8, ptr_i8])
	b.register_extern('sigaction', b.i32_type, [b.i32_type, ptr_i8, ptr_i8])
	b.register_extern('raise', b.i32_type, [b.i32_type])
	action_type := b.m.type_store.register(Type{
		kind:        .struct_t
		fields:      [ptr_i8, b.u32_type, b.i32_type]
		field_names: ['handler', 'mask', 'flags']
	})
	stack_type := b.m.type_store.register(Type{
		kind:        .struct_t
		fields:      [ptr_i8, b.u64_type, b.i32_type]
		field_names: ['pointer', 'size', 'flags']
	})
	fallback_addr := b.m.add_global('__ssa_signal_fallback', ptr_i8)
	handler_id := b.register_synthetic_function('__ssa_signal_handler', b.void_type,
		[b.i32_type, ptr_i8, ptr_i8])
	b.generate_native_signal_handler(handler_id, fallback_addr, action_type)
	install_one_id := b.register_synthetic_function('__ssa_install_signal', b.void_type,
		[b.i32_type, ptr_i8])
	b.generate_native_signal_install_one(install_one_id, action_type)
	install_id := b.register_synthetic_c_function('v_install_segfault_handler', b.void_type,
		[ptr_i8, ptr_i8])
	b.generate_native_signal_install(install_id, fallback_addr, handler_id, install_one_id,
		stack_type)
}

fn (mut b Builder) generate_native_signal_install(func_id int, fallback_addr ValueID,
	handler_id int, install_one_id int, stack_type TypeID) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	entry := b.m.add_block(func_id, 'signal_install_entry')
	fallback := b.func_add_argument(func_id, ptr_i8, 'fallback')
	_ := b.func_add_argument(func_id, ptr_i8, 'main_argv')
	b.block_instr2(.store, entry, b.void_type, fallback, fallback_addr)
	nil_ptr := b.m.get_or_add_const(ptr_i8, '0')
	zero := b.m.get_or_add_const(b.i32_type, '0')
	old_stack := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(stack_type))
	old_stack_raw := b.block_instr1(.bitcast, entry, ptr_i8, old_stack)
	query_ref := b.m.add_value(.func_ref, b.void_type, 'sigaltstack', b.fn_ids['sigaltstack'])
	queried := b.block_instr3(.call, entry, b.i32_type, query_ref, nil_ptr, old_stack_raw)
	query_ok := b.block_instr2(.eq, entry, b.i1_type, queried, zero)
	check_stack := b.m.add_block(func_id, 'signal_check_stack')
	allocate_stack := b.m.add_block(func_id, 'signal_allocate_stack')
	install := b.m.add_block(func_id, 'signal_install_handlers')
	b.block_instr3(.br, entry, b.void_type, query_ok, ValueID(check_stack), ValueID(install))
	flags_ptr := b.block_struct_field_ptr(check_stack, old_stack, stack_type, 2)
	flags := b.block_instr1(.load, check_stack, b.i32_type, flags_ptr)
	disabled_mask := b.m.get_or_add_const(b.i32_type, '4')
	disabled_bits := b.block_instr2(.and_, check_stack, b.i32_type, flags, disabled_mask)
	disabled := b.block_instr2(.ne, check_stack, b.i1_type, disabled_bits, zero)
	b.block_instr3(.br, check_stack, b.void_type, disabled, ValueID(allocate_stack), ValueID(install))
	stack_size := b.m.get_or_add_const(b.u64_type, '65536')
	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.fn_ids['malloc'])
	stack_memory := b.block_instr2(.call, allocate_stack, ptr_i8, malloc_ref, stack_size)
	allocated := b.block_instr2(.ne, allocate_stack, b.i1_type, stack_memory, nil_ptr)
	register_stack := b.m.add_block(func_id, 'signal_register_stack')
	b.block_instr3(.br, allocate_stack, b.void_type, allocated, ValueID(register_stack), ValueID(install))
	new_stack := b.block_instr0(.alloca, register_stack, b.m.type_store.get_ptr(stack_type))
	new_stack_raw := b.block_instr1(.bitcast, register_stack, ptr_i8, new_stack)
	for i, value in [stack_memory, stack_size, zero] {
		field := b.block_struct_field_ptr(register_stack, new_stack, stack_type, i)
		b.block_instr2(.store, register_stack, b.void_type, value, field)
	}
	registered := b.block_instr3(.call, register_stack, b.i32_type, query_ref, new_stack_raw, nil_ptr)
	registered_ok := b.block_instr2(.eq, register_stack, b.i1_type, registered, zero)
	free_stack := b.m.add_block(func_id, 'signal_free_unused_stack')
	b.block_instr3(.br, register_stack, b.void_type, registered_ok, ValueID(install), ValueID(free_stack))
	free_ref := b.m.add_value(.func_ref, b.void_type, 'free', b.fn_ids['free'])
	b.block_instr2(.call, free_stack, b.void_type, free_ref, stack_memory)
	b.block_instr1(.jmp, free_stack, b.void_type, ValueID(install))
	handler := b.m.add_value(.func_ref, ptr_i8, '__ssa_signal_handler', handler_id)
	install_ref := b.m.add_value(.func_ref, b.void_type, '__ssa_install_signal', install_one_id)
	for signal_number in [11, 10] {
		signal := b.m.get_or_add_const(b.i32_type, signal_number.str())
		b.block_instr3(.call, install, b.void_type, install_ref, signal, handler)
	}
	b.block_instr0(.ret, install, b.void_type)
}

fn (mut b Builder) generate_native_signal_install_one(func_id int, action_type TypeID) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	entry := b.m.add_block(func_id, 'signal_action_entry')
	signal := b.func_add_argument(func_id, b.i32_type, 'signal')
	handler := b.func_add_argument(func_id, ptr_i8, 'handler')
	nil_ptr := b.m.get_or_add_const(ptr_i8, '0')
	zero := b.m.get_or_add_const(b.i32_type, '0')
	previous := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(action_type))
	previous_raw := b.block_instr1(.bitcast, entry, ptr_i8, previous)
	action_ref := b.m.add_value(.func_ref, b.void_type, 'sigaction', b.fn_ids['sigaction'])
	status := b.block_instr4(.call, entry, b.i32_type, action_ref, signal, nil_ptr, previous_raw)
	ok := b.block_instr2(.eq, entry, b.i1_type, status, zero)
	check_default := b.m.add_block(func_id, 'signal_check_default_action')
	install := b.m.add_block(func_id, 'signal_set_action')
	done := b.m.add_block(func_id, 'signal_action_done')
	b.block_instr3(.br, entry, b.void_type, ok, ValueID(check_default), ValueID(done))
	previous_handler_ptr := b.block_struct_field_ptr(check_default, previous, action_type, 0)
	previous_handler := b.block_instr1(.load, check_default, ptr_i8, previous_handler_ptr)
	is_default := b.block_instr2(.eq, check_default, b.i1_type, previous_handler, nil_ptr)
	// Darwin keeps custom and ignored actions under kernel control, including one-shot handlers.
	b.block_instr3(.br, check_default, b.void_type, is_default, ValueID(install), ValueID(done))
	previous_mask_ptr := b.block_struct_field_ptr(install, previous, action_type, 1)
	previous_mask := b.block_instr1(.load, install, b.u32_type, previous_mask_ptr)
	previous_flags_ptr := b.block_struct_field_ptr(install, previous, action_type, 2)
	previous_flags := b.block_instr1(.load, install, b.i32_type, previous_flags_ptr)
	preserve_flags := b.m.get_or_add_const(b.i32_type, '18')
	preserved := b.block_instr2(.and_, install, b.i32_type, previous_flags, preserve_flags)
	// SA_SIGINFO | SA_ONSTACK supplies the three-argument handler on the alternate stack.
	new_flags := b.block_instr2(.or_, install, b.i32_type, preserved,
		b.m.get_or_add_const(b.i32_type, '65'))
	new_action := b.block_instr0(.alloca, install, b.m.type_store.get_ptr(action_type))
	for i, value in [handler, previous_mask, new_flags] {
		field := b.block_struct_field_ptr(install, new_action, action_type, i)
		b.block_instr2(.store, install, b.void_type, value, field)
	}
	new_action_raw := b.block_instr1(.bitcast, install, ptr_i8, new_action)
	b.block_instr4(.call, install, b.i32_type, action_ref, signal, new_action_raw, nil_ptr)
	b.block_instr1(.jmp, install, b.void_type, ValueID(done))
	b.block_instr0(.ret, done, b.void_type)
}

fn (mut b Builder) generate_native_signal_handler(func_id int, fallback_addr ValueID,
	action_type TypeID) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	entry := b.m.add_block(func_id, 'signal_handler_entry')
	signal := b.func_add_argument(func_id, b.i32_type, 'signal')
	_ := b.func_add_argument(func_id, ptr_i8, 'info')
	_ := b.func_add_argument(func_id, ptr_i8, 'context')
	fallback := b.block_instr1(.load, entry, ptr_i8, fallback_addr)
	nil_ptr := b.m.get_or_add_const(ptr_i8, '0')
	has_fallback := b.block_instr2(.ne, entry, b.i1_type, fallback, nil_ptr)
	is_segv := b.block_instr2(.eq, entry, b.i1_type, signal, b.m.get_or_add_const(b.i32_type, '11'))
	call_fallback := b.block_instr2(.and_, entry, b.i1_type, has_fallback, is_segv)
	report := b.m.add_block(func_id, 'signal_report')
	restore_default := b.m.add_block(func_id, 'signal_restore_default')
	b.block_instr3(.br, entry, b.void_type, call_fallback, ValueID(report), ValueID(restore_default))
	b.block_instr2(.call_indirect, report, b.void_type, fallback, signal)
	b.block_instr1(.jmp, report, b.void_type, ValueID(restore_default))
	default_action := b.block_instr0(.alloca, restore_default, b.m.type_store.get_ptr(action_type))
	zero := b.m.get_or_add_const(b.i32_type, '0')
	for i, value in [nil_ptr, zero, zero] {
		field := b.block_struct_field_ptr(restore_default, default_action, action_type, i)
		b.block_instr2(.store, restore_default, b.void_type, value, field)
	}
	action_raw := b.block_instr1(.bitcast, restore_default, ptr_i8, default_action)
	action_ref := b.m.add_value(.func_ref, b.void_type, 'sigaction', b.fn_ids['sigaction'])
	b.block_instr4(.call, restore_default, b.i32_type, action_ref, signal, action_raw, nil_ptr)
	raise_ref := b.m.add_value(.func_ref, b.void_type, 'raise', b.fn_ids['raise'])
	b.block_instr2(.call, restore_default, b.i32_type, raise_ref, signal)
	b.block_instr0(.ret, restore_default, b.void_type)
}
