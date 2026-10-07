module ssa

fn (mut b Builder) register_native_map_format_helpers() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	for name in ['v3_map_signed', 'v3_map_unsigned'] {
		id := b.register_synthetic_function(name, b.i64_type, [ptr_i8, b.i32_type])
		b.generate_native_map_integer_body(id, name == 'v3_map_signed')
	}
	array_id := b.register_synthetic_function('_native_float_array_str', b.str_type,
		[ptr_i8, b.i32_type, b.i1_type])
	b.generate_native_float_array_string_body(array_id)
	piece_id := b.register_synthetic_function('v3_map_str_piece', b.str_type,
		[ptr_i8, b.i32_type, b.i32_type, b.i32_type])
	b.generate_native_map_string_piece_body(piece_id)
	map_id := b.register_synthetic_function('v3_map_str', b.str_type,
		[b.map_type, b.i32_type, b.i32_type, b.i32_type])
	b.generate_native_map_string_body(map_id)
}

fn (mut b Builder) generate_native_map_integer_body(func_id int, signed bool) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	entry := b.m.add_block(func_id, 'map_integer_entry')
	p := b.func_add_argument(func_id, ptr_i8, 'p')
	bytes := b.func_add_argument(func_id, b.i32_type, 'bytes')
	mut check := entry
	for nbytes, value_type in {
		1: b.i8_type
		2: b.u16_type
		8: b.i64_type
		4: b.i32_type
	} {
		load := b.m.add_block(func_id, 'map_integer_${nbytes}')
		next := b.m.add_block(func_id, 'map_integer_check_${nbytes}')
		size := b.m.get_or_add_const(b.i32_type, '${nbytes}')
		matches := b.block_instr2(.eq, check, b.i1_type, bytes, size)
		b.block_instr3(.br, check, b.void_type, matches, ValueID(load), ValueID(next))
		ptr := b.block_instr1(.bitcast, load, b.m.type_store.get_ptr(value_type), p)
		value := b.block_instr1(.load, load, value_type, ptr)
		wide := if value_type == b.i64_type {
			value
		} else {
			b.block_instr1(if signed { .sext } else { .zext }, load, b.i64_type, value)
		}
		b.block_instr1(.ret, load, b.void_type, wide)
		check = next
	}
	ptr := b.block_instr1(.bitcast, check, b.m.type_store.get_ptr(b.i32_type), p)
	value := b.block_instr1(.load, check, b.i32_type, ptr)
	wide := b.block_instr1(if signed { .sext } else { .zext }, check, b.i64_type, value)
	b.block_instr1(.ret, check, b.void_type, wide)
}

fn (mut b Builder) native_format_string_plus(block BlockID, left ValueID, right ValueID) ValueID {
	ref := b.m.add_value(.func_ref, b.str_type, 'string__plus', b.fn_ids['string__plus'])
	return b.block_instr3(.call, block, b.str_type, ref, left, right)
}

fn (mut b Builder) native_format_quoted_string(block BlockID, text ValueID, quote string) ValueID {
	q := b.m.add_value(.string_literal, b.str_type, quote, 0)
	prefix := b.native_format_string_plus(block, q, text)
	return b.native_format_string_plus(block, prefix, q)
}

fn (mut b Builder) generate_native_float_array_string_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	entry := b.m.add_block(func_id, 'float_array_entry')
	data := b.func_add_argument(func_id, ptr_i8, 'data')
	count := b.func_add_argument(func_id, b.i32_type, 'count')
	is_f32 := b.func_add_argument(func_id, b.i1_type, 'is_f32')
	i_slot := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(b.i32_type))
	out_slot := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(b.str_type))
	value_slot := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(b.f64_type))
	zero := b.m.get_or_add_const(b.i32_type, '0')
	one := b.m.get_or_add_const(b.i32_type, '1')
	start := b.m.add_value(.string_literal, b.str_type, '[', 0)
	b.block_instr2(.store, entry, b.void_type, zero, i_slot)
	b.block_instr2(.store, entry, b.void_type, start, out_slot)
	loop := b.m.add_block(func_id, 'float_array_loop')
	body := b.m.add_block(func_id, 'float_array_body')
	separator := b.m.add_block(func_id, 'float_array_separator')
	choose := b.m.add_block(func_id, 'float_array_choose')
	load32 := b.m.add_block(func_id, 'float_array_load32')
	load64 := b.m.add_block(func_id, 'float_array_load64')
	append := b.m.add_block(func_id, 'float_array_append')
	done := b.m.add_block(func_id, 'float_array_done')
	b.block_instr1(.jmp, entry, b.void_type, ValueID(loop))
	i := b.block_instr1(.load, loop, b.i32_type, i_slot)
	more := b.block_instr2(.lt, loop, b.i1_type, i, count)
	b.block_instr3(.br, loop, b.void_type, more, ValueID(body), ValueID(done))
	has_previous := b.block_instr2(.gt, body, b.i1_type, i, zero)
	b.block_instr3(.br, body, b.void_type, has_previous, ValueID(separator), ValueID(choose))
	out := b.block_instr1(.load, separator, b.str_type, out_slot)
	comma := b.m.add_value(.string_literal, b.str_type, ', ', 0)
	separated := b.native_format_string_plus(separator, out, comma)
	b.block_instr2(.store, separator, b.void_type, separated, out_slot)
	b.block_instr1(.jmp, separator, b.void_type, ValueID(choose))
	b.block_instr3(.br, choose, b.void_type, is_f32, ValueID(load32), ValueID(load64))
	for block, value_type in {
		load32: b.f32_type
		load64: b.f64_type
	} {
		wide_i := b.block_instr1(.zext, block, b.i64_type, i)
		stride := b.m.get_or_add_const(b.i64_type, if value_type == b.f32_type { '4' } else { '8' })
		offset := b.block_instr2(.mul, block, b.i64_type, wide_i, stride)
		ptr := b.block_instr2(.add, block, ptr_i8, data, offset)
		typed_ptr := b.block_instr1(.bitcast, block, b.m.type_store.get_ptr(value_type), ptr)
		value := b.block_instr1(.load, block, value_type, typed_ptr)
		wide := if value_type == b.f32_type {
			b.block_instr1(.bitcast, block, b.f64_type, value)
		} else {
			value
		}
		b.block_instr2(.store, block, b.void_type, wide, value_slot)
		b.block_instr1(.jmp, block, b.void_type, ValueID(append))
	}
	value := b.block_instr1(.load, append, b.f64_type, value_slot)
	float_ref := b.m.add_value(.func_ref, b.str_type, 'strconv__f64_to_str_l',
		b.fn_ids['strconv__f64_to_str_l'])
	text := b.block_instr2(.call, append, b.str_type, float_ref, value)
	base := b.block_instr1(.load, append, b.str_type, out_slot)
	added := b.native_format_string_plus(append, base, text)
	b.block_instr2(.store, append, b.void_type, added, out_slot)
	next_i := b.block_instr2(.add, append, b.i32_type, i, one)
	b.block_instr2(.store, append, b.void_type, next_i, i_slot)
	b.block_instr1(.jmp, append, b.void_type, ValueID(loop))
	final_base := b.block_instr1(.load, done, b.str_type, out_slot)
	end := b.m.add_value(.string_literal, b.str_type, ']', 0)
	result := b.native_format_string_plus(done, final_base, end)
	b.block_instr1(.ret, done, b.void_type, result)
}

fn (mut b Builder) generate_native_map_string_piece_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	entry := b.m.add_block(func_id, 'map_piece_entry')
	p := b.func_add_argument(func_id, ptr_i8, 'p')
	kind := b.func_add_argument(func_id, b.i32_type, 'kind')
	bytes := b.func_add_argument(func_id, b.i32_type, 'bytes')
	fixed_len := b.func_add_argument(func_id, b.i32_type, 'fixed_len')
	mut check := entry
	for k in 1 .. 10 {
		format := b.m.add_block(func_id, 'map_piece_kind_${k}')
		next := b.m.add_block(func_id, 'map_piece_check_${k}')
		kind_const := b.m.get_or_add_const(b.i32_type, '${k}')
		matches := b.block_instr2(.eq, check, b.i1_type, kind, kind_const)
		b.block_instr3(.br, check, b.void_type, matches, ValueID(format), ValueID(next))
		match k {
			1 {
				ptr := b.block_instr1(.bitcast, format, b.m.type_store.get_ptr(b.str_type), p)
				text := b.block_instr1(.load, format, b.str_type, ptr)
				result := b.native_format_quoted_string(format, text, "'")
				b.block_instr1(.ret, format, b.void_type, result)
			}
			2, 3, 4 {
				name := if k == 2 { 'v3_map_signed' } else { 'v3_map_unsigned' }
				ref := b.m.add_value(.func_ref, b.i64_type, name, b.fn_ids[name])
				value := b.block_instr3(.call, format, b.i64_type, ref, p, bytes)
				mut result := ValueID(0)
				if k == 2 {
					int_ref := b.m.add_value(.func_ref, b.str_type, 'int_str', b.fn_ids['int_str'])
					result = b.block_instr2(.call, format, b.str_type, int_ref, value)
				} else if k == 3 {
					uint_ref := b.m.add_value(.func_ref, b.str_type, 'strconv__format_uint',
						b.fn_ids['strconv__format_uint'])
					base := b.m.get_or_add_const(b.i64_type, '10')
					result = b.block_instr3(.call, format, b.str_type, uint_ref, value, base)
				} else {
					rune32 := b.block_instr1(.trunc, format, b.i32_type, value)
					char_ref := b.m.add_value(.func_ref, b.str_type, 'v3_char_string', b.fn_ids['v3_char_string'])
					text := b.block_instr2(.call, format, b.str_type, char_ref, rune32)
					result = b.native_format_quoted_string(format, text, '`')
				}
				b.block_instr1(.ret, format, b.void_type, result)
			}
			5, 8 { b.generate_native_map_float_piece(format, p, bytes, k == 8) }
			6, 9 { b.generate_native_map_float_array_piece(format, p, bytes, fixed_len, k == 9) }
			7 {
				byte := b.block_instr1(.load, format, b.i8_type, p)
				zero := b.m.get_or_add_const(b.i8_type, '0')
				value := b.block_instr2(.ne, format, b.i1_type, byte, zero)
				ref := b.m.add_value(.func_ref, b.str_type, 'bool_str', b.fn_ids['bool_str'])
				result := b.block_instr2(.call, format, b.str_type, ref, value)
				b.block_instr1(.ret, format, b.void_type, result)
			}
			else {}
		}
		check = next
	}
	unknown := b.m.add_value(.string_literal, b.str_type, '<map value>', 0)
	b.block_instr1(.ret, check, b.void_type, unknown)
}

fn (mut b Builder) generate_native_map_float_piece(block BlockID, p ValueID, bytes ValueID, force_f32 bool) {
	func_id := b.m.blocks[block].parent
	load32 := b.m.add_block(func_id, 'map_piece_float32')
	load64 := b.m.add_block(func_id, 'map_piece_float64')
	if force_f32 {
		b.block_instr1(.jmp, block, b.void_type, ValueID(load32))
	} else {
		four := b.m.get_or_add_const(b.i32_type, '4')
		is_f32 := b.block_instr2(.eq, block, b.i1_type, bytes, four)
		b.block_instr3(.br, block, b.void_type, is_f32, ValueID(load32), ValueID(load64))
	}
	for load, value_type in {
		load32: b.f32_type
		load64: b.f64_type
	} {
		ptr := b.block_instr1(.bitcast, load, b.m.type_store.get_ptr(value_type), p)
		value := b.block_instr1(.load, load, value_type, ptr)
		wide := if value_type == b.f32_type {
			b.block_instr1(.bitcast, load, b.f64_type, value)
		} else {
			value
		}
		ref := b.m.add_value(.func_ref, b.str_type, 'strconv__f64_to_str_l',
			b.fn_ids['strconv__f64_to_str_l'])
		result := b.block_instr2(.call, load, b.str_type, ref, wide)
		b.block_instr1(.ret, load, b.void_type, result)
	}
}

fn (mut b Builder) generate_native_map_float_array_piece(block BlockID, p ValueID, bytes ValueID, fixed_len ValueID, force_f32 bool) {
	func_id := b.m.blocks[block].parent
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	data_slot := b.block_instr0(.alloca, block, b.m.type_store.get_ptr(ptr_i8))
	count_slot := b.block_instr0(.alloca, block, b.m.type_store.get_ptr(b.i32_type))
	f32_slot := b.block_instr0(.alloca, block, b.m.type_store.get_ptr(b.i1_type))
	zero := b.m.get_or_add_const(b.i32_type, '0')
	four := b.m.get_or_add_const(b.i32_type, '4')
	stride := b.m.get_or_add_const(b.i32_type, if force_f32 { '4' } else { '8' })
	flag := b.m.get_or_add_const(b.i1_type, if force_f32 { '1' } else { '0' })
	default_count := b.block_instr2(.sdiv, block, b.i32_type, bytes, stride)
	b.block_instr2(.store, block, b.void_type, p, data_slot)
	b.block_instr2(.store, block, b.void_type, default_count, count_slot)
	b.block_instr2(.store, block, b.void_type, flag, f32_slot)
	fixed := b.m.add_block(func_id, 'map_piece_array_fixed')
	dynamic_check := b.m.add_block(func_id, 'map_piece_array_dynamic_check')
	dynamic := b.m.add_block(func_id, 'map_piece_array_dynamic')
	done := b.m.add_block(func_id, 'map_piece_array_done')
	is_fixed := b.block_instr2(.gt, block, b.i1_type, fixed_len, zero)
	b.block_instr3(.br, block, b.void_type, is_fixed, ValueID(fixed), ValueID(dynamic_check))
	b.block_instr2(.store, fixed, b.void_type, fixed_len, count_slot)
	if !force_f32 {
		float_bytes := b.block_instr2(.mul, fixed, b.i32_type, fixed_len, four)
		is_float32 := b.block_instr2(.eq, fixed, b.i1_type, bytes, float_bytes)
		b.block_instr2(.store, fixed, b.void_type, is_float32, f32_slot)
	}
	b.block_instr1(.jmp, fixed, b.void_type, ValueID(done))
	if force_f32 {
		b.block_instr1(.jmp, dynamic_check, b.void_type, ValueID(done))
	} else {
		array_size := b.m.get_or_add_const(b.i32_type, '${b.m.type_size(b.array_type)}')
		is_array := b.block_instr2(.eq, dynamic_check, b.i1_type, bytes, array_size)
		b.block_instr3(.br, dynamic_check, b.void_type, is_array, ValueID(dynamic), ValueID(done))
	}
	array := b.block_instr1(.bitcast, dynamic, b.m.type_store.get_ptr(b.array_type), p)
	data_ptr := b.block_struct_field_ptr(dynamic, array, b.array_type, 0)
	len_ptr := b.block_struct_field_ptr(dynamic, array, b.array_type, 2)
	size_ptr := b.block_struct_field_ptr(dynamic, array, b.array_type, 5)
	data := b.block_instr1(.load, dynamic, ptr_i8, data_ptr)
	len := b.block_instr1(.load, dynamic, b.i32_type, len_ptr)
	elem_size := b.block_instr1(.load, dynamic, b.i32_type, size_ptr)
	is_float32 := b.block_instr2(.eq, dynamic, b.i1_type, elem_size, four)
	b.block_instr2(.store, dynamic, b.void_type, data, data_slot)
	b.block_instr2(.store, dynamic, b.void_type, len, count_slot)
	b.block_instr2(.store, dynamic, b.void_type, is_float32, f32_slot)
	b.block_instr1(.jmp, dynamic, b.void_type, ValueID(done))
	final_data := b.block_instr1(.load, done, ptr_i8, data_slot)
	final_count := b.block_instr1(.load, done, b.i32_type, count_slot)
	final_flag := b.block_instr1(.load, done, b.i1_type, f32_slot)
	ref := b.m.add_value(.func_ref, b.str_type, '_native_float_array_str',
		b.fn_ids['_native_float_array_str'])
	result := b.block_instr4(.call, done, b.str_type, ref, final_data, final_count, final_flag)
	b.block_instr1(.ret, done, b.void_type, result)
}

fn (mut b Builder) generate_native_map_string_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	entry := b.m.add_block(func_id, 'map_string_entry')
	m := b.func_add_argument(func_id, b.map_type, 'm')
	key_kind := b.func_add_argument(func_id, b.i32_type, 'key_kind')
	val_kind := b.func_add_argument(func_id, b.i32_type, 'val_kind')
	fixed_len := b.func_add_argument(func_id, b.i32_type, 'fixed_len')
	m_slot := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(b.map_type))
	i_slot := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(b.i64_type))
	out_slot := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(b.str_type))
	zero := b.m.get_or_add_const(b.i64_type, '0')
	one := b.m.get_or_add_const(b.i64_type, '1')
	zero32 := b.m.get_or_add_const(b.i32_type, '0')
	start := b.m.add_value(.string_literal, b.str_type, '{', 0)
	b.block_instr2(.store, entry, b.void_type, m, m_slot)
	b.block_instr2(.store, entry, b.void_type, zero, i_slot)
	b.block_instr2(.store, entry, b.void_type, start, out_slot)
	state := b.map_state_ptr(entry, m_slot)
	nil_state := b.m.get_or_add_const(b.m.type_store.get_ptr(b.map_state_type), '0')
	has_state := b.block_instr2(.ne, entry, b.i1_type, state, nil_state)
	prepare := b.m.add_block(func_id, 'map_string_prepare')
	loop := b.m.add_block(func_id, 'map_string_loop')
	body := b.m.add_block(func_id, 'map_string_body')
	separator := b.m.add_block(func_id, 'map_string_separator')
	append := b.m.add_block(func_id, 'map_string_append')
	done := b.m.add_block(func_id, 'map_string_done')
	b.block_instr3(.br, entry, b.void_type, has_state, ValueID(prepare), ValueID(done))
	keys_ptr := b.map_state_field_ptr(prepare, state, 0)
	vals_ptr := b.map_state_field_ptr(prepare, state, 1)
	len_ptr := b.map_state_field_ptr(prepare, state, 3)
	key_size_ptr := b.map_state_field_ptr(prepare, state, 4)
	val_size_ptr := b.map_state_field_ptr(prepare, state, 5)
	keys := b.block_instr1(.load, prepare, ptr_i8, keys_ptr)
	vals := b.block_instr1(.load, prepare, ptr_i8, vals_ptr)
	len := b.block_instr1(.load, prepare, b.i64_type, len_ptr)
	key_size := b.block_instr1(.load, prepare, b.i64_type, key_size_ptr)
	val_size := b.block_instr1(.load, prepare, b.i64_type, val_size_ptr)
	key_size32 := b.block_instr1(.trunc, prepare, b.i32_type, key_size)
	val_size32 := b.block_instr1(.trunc, prepare, b.i32_type, val_size)
	b.block_instr1(.jmp, prepare, b.void_type, ValueID(loop))
	i := b.block_instr1(.load, loop, b.i64_type, i_slot)
	more := b.block_instr2(.lt, loop, b.i1_type, i, len)
	b.block_instr3(.br, loop, b.void_type, more, ValueID(body), ValueID(done))
	has_previous := b.block_instr2(.gt, body, b.i1_type, i, zero)
	b.block_instr3(.br, body, b.void_type, has_previous, ValueID(separator), ValueID(append))
	base := b.block_instr1(.load, separator, b.str_type, out_slot)
	comma := b.m.add_value(.string_literal, b.str_type, ', ', 0)
	separated := b.native_format_string_plus(separator, base, comma)
	b.block_instr2(.store, separator, b.void_type, separated, out_slot)
	b.block_instr1(.jmp, separator, b.void_type, ValueID(append))
	key_offset := b.block_instr2(.mul, append, b.i64_type, i, key_size)
	val_offset := b.block_instr2(.mul, append, b.i64_type, i, val_size)
	key_ptr := b.block_instr2(.add, append, ptr_i8, keys, key_offset)
	val_ptr := b.block_instr2(.add, append, ptr_i8, vals, val_offset)
	piece_ref := b.m.add_value(.func_ref, b.str_type, 'v3_map_str_piece', b.fn_ids['v3_map_str_piece'])
	key_text := b.m.add_instr(.call, append, b.str_type,
		[piece_ref, key_ptr, key_kind, key_size32, zero32])
	val_text := b.m.add_instr(.call, append, b.str_type,
		[piece_ref, val_ptr, val_kind, val_size32, fixed_len])
	out := b.block_instr1(.load, append, b.str_type, out_slot)
	with_key := b.native_format_string_plus(append, out, key_text)
	colon := b.m.add_value(.string_literal, b.str_type, ': ', 0)
	with_colon := b.native_format_string_plus(append, with_key, colon)
	with_value := b.native_format_string_plus(append, with_colon, val_text)
	b.block_instr2(.store, append, b.void_type, with_value, out_slot)
	next_i := b.block_instr2(.add, append, b.i64_type, i, one)
	b.block_instr2(.store, append, b.void_type, next_i, i_slot)
	b.block_instr1(.jmp, append, b.void_type, ValueID(loop))
	final_base := b.block_instr1(.load, done, b.str_type, out_slot)
	end := b.m.add_value(.string_literal, b.str_type, '}', 0)
	result := b.native_format_string_plus(done, final_base, end)
	b.block_instr1(.ret, done, b.void_type, result)
}
