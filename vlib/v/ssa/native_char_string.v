module ssa

// generate_native_char_string_body encodes a codepoint as one to four UTF-8 bytes.
fn (mut b Builder) generate_native_char_string_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	entry := b.m.add_block(func_id, 'char_entry')
	code := b.func_add_argument(func_id, b.i32_type, 'code')
	max_code := b.m.get_or_add_const(b.i32_type, '1114111')
	valid := b.block_instr2(.ule, entry, b.i1_type, code, max_code)
	allocate := b.m.add_block(func_id, 'char_allocate')
	invalid := b.m.add_block(func_id, 'char_invalid')
	b.block_instr3(.br, entry, b.void_type, valid, ValueID(allocate), ValueID(invalid))
	empty := b.m.add_value(.string_literal, b.str_type, '', 0)
	b.block_instr1(.ret, invalid, b.void_type, empty)
	size := b.m.get_or_add_const(b.i64_type, '5')
	malloc_ref := b.m.add_value(.func_ref, ptr_i8, 'malloc', b.fn_ids['malloc'])
	out := b.block_instr2(.call, allocate, ptr_i8, malloc_ref, size)
	mut check := allocate
	for nbytes in 1 .. 5 {
		encode := b.m.add_block(func_id, 'char_encode_${nbytes}')
		if nbytes < 4 {
			limit := b.m.get_or_add_const(b.i32_type, match nbytes {
				1 { '127' }
				2 { '2047' }
				else { '65535' }
			})
			next := b.m.add_block(func_id, 'char_check_${nbytes + 1}')
			fits := b.block_instr2(.ule, check, b.i1_type, code, limit)
			b.block_instr3(.br, check, b.void_type, fits, ValueID(encode), ValueID(next))
			check = next
		} else {
			b.block_instr1(.jmp, check, b.void_type, ValueID(encode))
		}
		for i in 0 .. nbytes {
			shift := b.m.get_or_add_const(b.i32_type, '${(nbytes - i - 1) * 6}')
			mut byte := b.block_instr2(.lshr, encode, b.i32_type, code, shift)
			if nbytes > 1 {
				mask := b.m.get_or_add_const(b.i32_type, if i == 0 {
					match nbytes {
						2 { '31' }
						3 { '15' }
						else { '7' }
					}
				} else {
					'63'
				})
				byte = b.block_instr2(.and_, encode, b.i32_type, byte, mask)
				prefix := b.m.get_or_add_const(b.i32_type, if i == 0 {
					match nbytes {
						2 { '192' }
						3 { '224' }
						else { '240' }
					}
				} else {
					'128'
				})
				byte = b.block_instr2(.or_, encode, b.i32_type, byte, prefix)
			}
			byte8 := b.block_instr1(.trunc, encode, b.i8_type, byte)
			offset := b.m.get_or_add_const(b.i64_type, '${i}')
			byte_ptr := b.block_instr2(.add, encode, ptr_i8, out, offset)
			b.block_instr2(.store, encode, b.void_type, byte8, byte_ptr)
		}
		len := b.m.get_or_add_const(b.i64_type, '${nbytes}')
		nul := b.m.get_or_add_const(b.i8_type, '0')
		term := b.block_instr2(.add, encode, ptr_i8, out, len)
		b.block_instr2(.store, encode, b.void_type, nul, term)
		result := b.emit_make_string(encode, out, len, 0)
		b.block_instr1(.ret, encode, b.void_type, result)
	}
}
