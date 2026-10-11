module ssa

fn (mut b Builder) register_native_process_externs() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_i32 := b.m.type_store.get_ptr(b.i32_type)
	ptr_argv := b.m.type_store.get_ptr(ptr_i8)
	b.register_extern('pipe', b.i32_type, [ptr_i32])
	b.register_extern('fcntl', b.i32_type, [b.i32_type, b.i32_type, b.i32_type])
	mut fcntl_func := b.m.funcs[b.fn_ids['fcntl']]
	fcntl_func.is_variadic = true
	fcntl_func.variadic_start = 2
	b.m.funcs[fcntl_func.id] = fcntl_func
	b.register_extern('posix_spawn_file_actions_init', b.i32_type, [ptr_i8])
	b.register_extern('posix_spawn_file_actions_destroy', b.i32_type, [ptr_i8])
	b.register_extern('posix_spawn_file_actions_adddup2', b.i32_type,
		[ptr_i8, b.i32_type, b.i32_type])
	b.register_extern('posix_spawn_file_actions_addclose', b.i32_type, [ptr_i8, b.i32_type])
	for name in ['posix_spawn', 'posix_spawnp'] {
		b.register_extern(name, b.i32_type, [ptr_i32, ptr_i8, ptr_i8, ptr_i8, ptr_argv, ptr_argv])
	}
}

// These helpers mirror execute_capture_nix.h, including close-on-exec pipes,
// merged stdout/stderr, and descriptor cleanup when spawning fails.
fn (mut b Builder) generate_native_process_capture_body(func_id int, shell bool) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_i32 := b.m.type_store.get_ptr(b.i32_type)
	ptr_argv := b.m.type_store.get_ptr(ptr_i8)
	entry := b.m.add_block(func_id, 'capture_entry')
	input_type := if shell { ptr_i8 } else { ptr_argv }
	input := b.func_add_argument(func_id, input_type, 'input')
	child_pid := b.func_add_argument(func_id, b.m.type_store.get_ptr(b.i64_type), 'child_pid')
	read_fd := b.func_add_argument(func_id, b.m.type_store.get_ptr(b.i64_type), 'read_fd')
	zero32 := b.m.get_or_add_const(b.i32_type, '0')
	one32 := b.m.get_or_add_const(b.i32_type, '1')
	two32 := b.m.get_or_add_const(b.i32_type, '2')
	nil_ptr := b.m.get_or_add_const(ptr_i8, '0')
	failed := b.m.add_block(func_id, 'capture_failed')
	pipe_block := b.m.add_block(func_id, 'capture_pipe')
	mut argv := input
	mut executable := input
	if shell {
		argv_type := b.m.type_store.get_array(ptr_i8, 4)
		argv_storage := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(argv_type))
		argv = b.block_instr1(.bitcast, entry, ptr_argv, argv_storage)
		shell_literal := b.m.add_value(.string_literal, b.str_type, '/bin/sh', 0)
		executable = b.block_instr1(.bitcast, entry, ptr_i8, shell_literal)
		flag_literal := b.m.add_value(.string_literal, b.str_type, '-c', 0)
		flag := b.block_instr1(.bitcast, entry, ptr_i8, flag_literal)
		for i, value in [executable, flag, input, nil_ptr] {
			offset := b.m.get_or_add_const(b.i64_type, '${i * 8}')
			slot := b.block_instr2(.get_element_ptr, entry, ptr_argv, argv, offset)
			b.block_instr2(.store, entry, b.void_type, value, slot)
		}
		b.block_instr1(.jmp, entry, b.void_type, ValueID(pipe_block))
	} else {
		nil_argv := b.m.get_or_add_const(ptr_argv, '0')
		valid_argv := b.block_instr2(.ne, entry, b.i1_type, input, nil_argv)
		check_executable := b.m.add_block(func_id, 'capture_check_executable')
		b.block_instr3(.br, entry, b.void_type, valid_argv, ValueID(check_executable),
			ValueID(failed))
		executable = b.block_instr1(.load, check_executable, ptr_i8, input)
		valid_executable := b.block_instr2(.ne, check_executable, b.i1_type, executable, nil_ptr)
		b.block_instr3(.br, check_executable, b.void_type, valid_executable, ValueID(pipe_block),
			ValueID(failed))
	}
	pipe_type := b.m.type_store.get_array(b.i32_type, 2)
	pipe_storage := b.block_instr0(.alloca, pipe_block, b.m.type_store.get_ptr(pipe_type))
	pipe_ptr := b.block_instr1(.bitcast, pipe_block, ptr_i32, pipe_storage)
	pipe_ref := b.m.add_value(.func_ref, b.i32_type, 'pipe', b.fn_ids['pipe'])
	pipe_rc := b.block_instr2(.call, pipe_block, b.i32_type, pipe_ref, pipe_ptr)
	pipe_ok := b.block_instr2(.eq, pipe_block, b.i1_type, pipe_rc, zero32)
	setup := b.m.add_block(func_id, 'capture_setup')
	b.block_instr3(.br, pipe_block, b.void_type, pipe_ok, ValueID(setup), ValueID(failed))
	write_offset := b.m.get_or_add_const(b.i64_type, '4')
	write_ptr := b.block_instr2(.get_element_ptr, setup, ptr_i32, pipe_ptr, write_offset)
	reader := b.block_instr1(.load, setup, b.i32_type, pipe_ptr)
	writer := b.block_instr1(.load, setup, b.i32_type, write_ptr)
	fcntl_ref := b.m.add_value(.func_ref, b.i32_type, 'fcntl', b.fn_ids['fcntl'])
	// F_SETFD and FD_CLOEXEC are both 2 and 1, respectively, on Darwin.
	for fd in [reader, writer] {
		b.block_instr4(.call, setup, b.i32_type, fcntl_ref, fd, two32, one32)
	}
	actions_type := b.m.type_store.get_array(b.u8_type, 128)
	actions_storage := b.block_instr0(.alloca, setup, b.m.type_store.get_ptr(actions_type))
	actions := b.block_instr1(.bitcast, setup, ptr_i8, actions_storage)
	close_pipes := b.m.add_block(func_id, 'capture_close_pipes')
	cleanup := b.m.add_block(func_id, 'capture_cleanup_actions')
	redirect_stdout := b.m.add_block(func_id, 'capture_redirect_stdout')
	redirect_stderr := b.m.add_block(func_id, 'capture_redirect_stderr')
	close_reader := b.m.add_block(func_id, 'capture_child_close_reader')
	close_writer := b.m.add_block(func_id, 'capture_child_close_writer')
	spawn_block := b.m.add_block(func_id, 'capture_spawn')
	success := b.m.add_block(func_id, 'capture_success')
	b.native_process_checked_call(setup, 'posix_spawn_file_actions_init', [actions],
		redirect_stdout, close_pipes)
	b.native_process_checked_call(redirect_stdout, 'posix_spawn_file_actions_adddup2',
		[actions, writer, one32], redirect_stderr, cleanup)
	b.native_process_checked_call(redirect_stderr, 'posix_spawn_file_actions_adddup2',
		[actions, writer, two32], close_reader, cleanup)
	b.native_process_checked_call(close_reader, 'posix_spawn_file_actions_addclose',
		[actions, reader], close_writer, cleanup)
	b.native_process_checked_call(close_writer, 'posix_spawn_file_actions_addclose',
		[actions, writer], spawn_block, cleanup)
	pid_storage := b.block_instr0(.alloca, spawn_block, ptr_i32)
	environ_addr := b.m.add_external_global('environ', ptr_argv)
	environ := b.block_instr1(.load, spawn_block, ptr_argv, environ_addr)
	spawn_name := if shell { 'posix_spawn' } else { 'posix_spawnp' }
	spawn_ref := b.m.add_value(.func_ref, b.i32_type, spawn_name, b.fn_ids[spawn_name])
	spawn_rc := b.m.add_instr(.call, spawn_block, b.i32_type, [spawn_ref, pid_storage, executable,
		actions, nil_ptr, argv, environ])
	destroy_ref := b.m.add_value(.func_ref, b.i32_type, 'posix_spawn_file_actions_destroy',
		b.fn_ids['posix_spawn_file_actions_destroy'])
	b.block_instr2(.call, spawn_block, b.i32_type, destroy_ref, actions)
	spawn_ok := b.block_instr2(.eq, spawn_block, b.i1_type, spawn_rc, zero32)
	b.block_instr3(.br, spawn_block, b.void_type, spawn_ok, ValueID(success), ValueID(close_pipes))
	close_ref := b.m.add_value(.func_ref, b.i64_type, 'close', b.fn_ids['close'])
	b.block_instr2(.call, cleanup, b.i32_type, destroy_ref, actions)
	b.block_instr1(.jmp, cleanup, b.void_type, ValueID(close_pipes))
	b.block_instr2(.call, close_pipes, b.i64_type, close_ref, reader)
	b.block_instr2(.call, close_pipes, b.i64_type, close_ref, writer)
	b.block_instr1(.jmp, close_pipes, b.void_type, ValueID(failed))
	b.block_instr2(.call, success, b.i64_type, close_ref, writer)
	pid32 := b.block_instr1(.load, success, b.i32_type, pid_storage)
	// The os wrappers infer their local integer slots from i64 SSA literals.
	// Widen the libc results, including the descriptor initially stored as -1.
	pid := b.block_instr1(.sext, success, b.i64_type, pid32)
	fd := b.block_instr1(.sext, success, b.i64_type, reader)
	b.block_instr2(.store, success, b.void_type, pid, child_pid)
	b.block_instr2(.store, success, b.void_type, fd, read_fd)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	minus_one := b.m.get_or_add_const(b.i64_type, '-1')
	b.block_instr1(.ret, success, b.void_type, zero)
	b.block_instr1(.ret, failed, b.void_type, minus_one)
}

fn (mut b Builder) native_process_checked_call(block BlockID, name string, args []ValueID, success BlockID, failure BlockID) {
	callee := b.m.add_value(.func_ref, b.i32_type, name, b.fn_ids[name])
	result := b.m.add_instr(.call, block, b.i32_type, [callee, ...args])
	zero := b.m.get_or_add_const(b.i32_type, '0')
	ok := b.block_instr2(.eq, block, b.i1_type, result, zero)
	b.block_instr3(.br, block, b.void_type, ok, ValueID(success), ValueID(failure))
}
