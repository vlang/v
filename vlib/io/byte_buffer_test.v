module io

fn test_byte_buffer_roundtrip() {
	mut b := new_byte_buffer(16)
	b.put_u8(0xAB)
	b.put_u16_be(0x1234)
	b.put_u16_le(0x5678)
	b.put_u32_be(0xDEADBEEF)
	b.put_u32_le(0xCAFEBABE)
	b.put_u64_be(0x0123456789ABCDEF)
	b.put_u64_le(0xFEDCBA9876543210)
	b.put_bytes('hi'.bytes())
	assert b.remaining() == 1 + 2 + 2 + 4 + 4 + 8 + 8 + 2
	b.flip()
	assert b.get_u8()! == 0xAB
	assert b.get_u16_be()! == 0x1234
	assert b.get_u16_le()! == 0x5678
	assert b.get_u32_be()! == 0xDEADBEEF
	assert b.get_u32_le()! == 0xCAFEBABE
	assert b.get_u64_be()! == 0x0123456789ABCDEF
	assert b.get_u64_le()! == 0xFEDCBA9876543210
	assert b.get_bytes(2)! == 'hi'.bytes()
	assert b.remaining() == 0
}

fn test_byte_buffer_compact_and_rewind() {
	mut b := new_byte_buffer(0)
	b.put_u32_be(1)
	b.put_u32_be(2)
	b.put_u32_be(3)
	b.flip()
	assert b.get_u32_be()! == 1
	b.compact()
	assert b.remaining() == 8, 'consumed bytes must be discarded'
	assert b.get_u32_be()! == 2
	b.rewind()
	assert b.remaining() == 8, 'rewind must restore the cursor'
	assert b.get_u32_be()! == 2
	assert b.get_u32_be()! == 3
	b.clear()
	assert b.remaining() == 0 && b.len() == 0
}

fn test_byte_buffer_underflow() {
	mut b := new_byte_buffer(0)
	b.put_u8(9)
	b.flip()
	assert b.get_u8()! == 9
	b.get_u8() or {
		assert err.msg() == 'io: ByteBuffer underflow'
		return
	}
	assert false, 'over-read must error'
	b.get_u32_be() or {
		assert err.msg() == 'io: ByteBuffer underflow'
		return
	}
	assert false, 'wide over-read must error'
	b.get_bytes(3) or {
		return
	}
	assert false, 'bulk over-read must error'
}
