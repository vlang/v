// Copyright (c) 2026 blackshirt.
//
// Wire codec tests: primitives and vectors must read back exactly what was
// written, and truncated reads must fail instead of indexing past the buffer.
module wire

fn test_wire_roundtrip_primitives() ! {
	mut w := new_writer(64)
	w.write_u8(0x42)
	w.write_u16(0x1234)
	w.write_u24(0x010203)
	w.write_u32(0xaabbccdd)
	w.write_u64(0x0102030405060708)

	bytes := w.bytes()
	assert bytes.len == 1 + 2 + 3 + 4 + 8

	mut r := new_reader(bytes)
	assert r.read_u8()! == 0x42
	assert r.read_u16()! == 0x1234
	assert r.read_u24()! == 0x010203
	assert r.read_u32()! == 0xaabbccdd
	assert r.read_u64()! == 0x0102030405060708
	assert r.remaining() == 0
}

fn test_wire_vectors() ! {
	mut w := new_writer(64)
	payload := [u8(10), 20, 30, 40]
	w.write_vector_u8(payload)!
	w.write_vector_u16(payload)!
	w.write_vector_u24(payload)!

	mut r := new_reader(w.bytes())
	assert r.read_vector_u8()! == payload
	assert r.read_vector_u16()! == payload
	assert r.read_vector_u24()! == payload
	assert r.remaining() == 0
}

fn test_wire_reader_bounds_check() {
	mut r := new_reader([u8(1), 2])
	assert r.read_u8() or { 0 } == 1
	assert r.read_u8() or { 0 } == 2
	r.read_u8() or {
		assert err.msg().contains('unexpected EOF')
		return
	}
	assert false
}

// test_wire_writer_u8_vector_boundary exercises new_writer's capacity<=0 fallback
// and proves a full 255-byte payload round-trips intact.
fn test_wire_writer_u8_vector_boundary() ! {
	data := []u8{len: 255, init: 0x7f}
	mut w := new_writer(0) // cap<=0 → default capacity; growth must still fit 256 bytes
	w.write_vector_u8(data)!

	mut r := new_reader(w.bytes())
	got := r.read_vector_u8()!
	assert got.len == 255
	assert got[0] == 0x7f
	assert got[254] == 0x7f
}

// test_wire_writer_rejects_oversized_u8_vector: a u8-prefixed vector cannot
// encode more than 255 bytes, so it must be rejected rather than truncated.
fn test_wire_writer_rejects_oversized_u8_vector() ! {
	over := []u8{len: 256}
	mut w := new_writer(64)
	w.write_vector_u8(over) or {
		assert err.msg().contains('exceeds u8 max')
		return
	}
	assert false
}

// test_wire_reader_read_bytes_negative: a negative count must be rejected as a
// framing error rather than slicing the buffer invalidly.
fn test_wire_reader_read_bytes_negative() ! {
	mut r := new_reader([u8(1), 2, 3])
	assert r.remaining() == 3
	r.read_bytes(-1) or {
		assert err.msg().contains('unexpected EOF')
		return
	}
	assert false
}

// test_wire_reader_truncated_vector: the 1-byte prefix declares 5 payload bytes
// but only 2 are present, so the read must fail the bounds check.
fn test_wire_reader_truncated_vector() ! {
	mut r := new_reader([u8(5), 1, 2])
	r.read_vector_u8() or {
		assert err.msg().contains('unexpected EOF')
		return
	}
	assert false
}

// test_wire_reader_pos tracks the walked distance as reads succeed: it must
// equal bytes consumed from the front of the buffer, not the remainder.
fn test_wire_reader_pos() ! {
	mut r := new_reader([u8(1), 2, 3, 4, 5, 6, 7, 8])
	assert r.pos() == 0
	r.read_u16()!
	assert r.pos() == 2
	r.read_u64() or { 0 } // only 6 bytes remain: bounds error must not advance
	assert r.pos() == 2
	r.read_bytes(6)!
	assert r.pos() == 8
	assert r.remaining() == 0
}
