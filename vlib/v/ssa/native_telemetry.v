module ssa

// register_native_telemetry_helpers replaces the header-only peak RSS helper.
fn (mut b Builder) register_native_telemetry_helpers() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	b.register_extern('getrusage', b.i32_type, [b.i32_type, ptr_i8])
	id := b.register_synthetic_c_function('v3_bench_peak_rss_kb', b.i64_type, [])
	entry := b.m.add_block(id, 'peak_rss_entry')
	// Darwin rusage contains two timevals followed by fourteen 64-bit longs.
	usage_type := b.m.type_store.get_array(b.u64_type, 18)
	usage := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(usage_type))
	buffer := b.block_instr1(.bitcast, entry, ptr_i8, usage)
	self := b.m.get_or_add_const(b.i32_type, '0')
	callee := b.m.add_value(.func_ref, b.i32_type, 'getrusage', b.fn_ids['getrusage'])
	status := b.block_instr3(.call, entry, b.i32_type, callee, self, buffer)
	ok := b.block_instr2(.eq, entry, b.i1_type, status, self)
	read := b.m.add_block(id, 'peak_rss_read')
	failed := b.m.add_block(id, 'peak_rss_failed')
	b.block_instr3(.br, entry, b.void_type, ok, ValueID(read), ValueID(failed))
	offset := b.m.get_or_add_const(b.i64_type, '32')
	maxrss_ptr := b.block_instr2(.add, read, b.m.type_store.get_ptr(b.i64_type), buffer, offset)
	maxrss := b.block_instr1(.load, read, b.i64_type, maxrss_ptr)
	bytes_per_kb := b.m.get_or_add_const(b.i64_type, '1024')
	result := b.block_instr2(.sdiv, read, b.i64_type, maxrss, bytes_per_kb)
	b.block_instr1(.ret, read, b.void_type, result)
	failure := b.m.get_or_add_const(b.i64_type, '-1')
	b.block_instr1(.ret, failed, b.void_type, failure)
}
