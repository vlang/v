module ssa

// register_native_cmdexec_helpers supplies the header-only nonblocking pipe poll helper.
fn (mut b Builder) register_native_cmdexec_helpers() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	i16_type := b.m.type_store.get_int(16)
	nfds_type := if b.m.target.is_linux() { b.u64_type } else { b.u32_type }
	b.register_extern('poll', b.i32_type, [ptr_i8, nfds_type, b.i32_type])
	func_id := b.register_synthetic_c_function('v_cmdexec_poll_revents', i16_type,
		[b.i32_type, i16_type])
	entry := b.m.add_block(func_id, 'cmdexec_poll_entry')
	fd := b.func_add_argument(func_id, b.i32_type, 'fd')
	events := b.func_add_argument(func_id, i16_type, 'events')
	// Linux and Darwin pollfd use int fd, short events, short revents.
	descriptor_type := b.m.type_store.register(Type{
		kind:        .struct_t
		fields:      [b.i32_type, i16_type, i16_type]
		field_names: ['fd', 'events', 'revents']
	})
	descriptor := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(descriptor_type))
	zero16 := b.m.get_or_add_const(i16_type, '0')
	for index, value in [fd, events, zero16] {
		field := b.block_struct_field_ptr(entry, descriptor, descriptor_type, index)
		b.block_instr2(.store, entry, b.void_type, value, field)
	}
	buffer := b.block_instr1(.bitcast, entry, ptr_i8, descriptor)
	one := b.m.get_or_add_const(nfds_type, '1')
	zero32 := b.m.get_or_add_const(b.i32_type, '0')
	callee := b.m.add_value(.func_ref, b.i32_type, 'poll', b.fn_ids['poll'])
	status := b.block_instr4(.call, entry, b.i32_type, callee, buffer, one, zero32)
	ready := b.block_instr2(.gt, entry, b.i1_type, status, zero32)
	read := b.m.add_block(func_id, 'cmdexec_poll_ready')
	empty := b.m.add_block(func_id, 'cmdexec_poll_empty')
	b.block_instr3(.br, entry, b.void_type, ready, ValueID(read), ValueID(empty))
	revents_ptr := b.block_struct_field_ptr(read, descriptor, descriptor_type, 2)
	revents := b.block_instr1(.load, read, i16_type, revents_ptr)
	b.block_instr1(.ret, read, b.void_type, revents)
	b.block_instr1(.ret, empty, b.void_type, zero16)
}
