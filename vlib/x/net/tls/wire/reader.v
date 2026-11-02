// Copyright (c) 2026 blackshirt.
//
// wire is the bottom layer of the TLS stack: a bounds-checked cursor (Reader)
// and a big-endian appender (Writer) doing framing arithmetic only; every
// out-of-bounds or oversized access is an error.
module wire

// Reader is a bounds-checked cursor over TLS wire data.
// @[noinit]: construct via new_reader; every read length-checks first (RFC 8446 §4).
@[noinit]
pub struct Reader {
	data []u8 // buffer being parsed; never mutated, only walked
mut:
	cursor int // next unread byte; advanced only after a successful read
}

// new_reader initializes a Reader over the given byte slice.
pub fn new_reader(data []u8) Reader {
	return Reader{
		data:   data
		cursor: 0
	}
}

// remaining returns the number of unread bytes.
pub fn (r &Reader) remaining() int {
	rem := r.data.len - r.cursor
	return if rem > 0 { rem } else { 0 }
}

// pos returns the offset of the next unread byte from the start of the buffer.
// Callers slice regions relative to the walked distance, not the remainder.
pub fn (r &Reader) pos() int {
	return r.cursor
}

// ensure checks that at least `n` bytes remain, failing with a framing error
// instead of indexing out of bounds; `what` names the item.
fn (mut r Reader) ensure(n int, what string) ! {
	if n < 0 || n > r.data.len - r.cursor {
		return error('wire.Reader: unexpected EOF reading ${what}')
	}
}

// read_u8 reads a single 8-bit unsigned integer.
pub fn (mut r Reader) read_u8() !u8 {
	r.ensure(1, 'u8')!
	b := r.data[r.cursor]
	r.cursor++
	return b
}

// read_u16 reads a 16-bit big-endian unsigned integer (RFC 8446 §2).
pub fn (mut r Reader) read_u16() !u16 {
	r.ensure(2, 'u16')!
	v := (u16(r.data[r.cursor]) << 8) | u16(r.data[r.cursor + 1])
	r.cursor += 2
	return v
}

// read_u24 reads a 24-bit big-endian unsigned integer as u32.
// TLS has no native u24, but 24-bit lengths are pervasive (§4, §4.4.2).
pub fn (mut r Reader) read_u24() !u32 {
	r.ensure(3, 'u24')!
	v := (u32(r.data[r.cursor]) << 16) | (u32(r.data[r.cursor + 1]) << 8) | u32(r.data[r.cursor + 2])
	r.cursor += 3
	return v
}

// read_u32 reads a 32-bit big-endian unsigned integer (RFC 8446 §2).
pub fn (mut r Reader) read_u32() !u32 {
	r.ensure(4, 'u32')!
	v := (u32(r.data[r.cursor]) << 24) | (u32(r.data[r.cursor + 1]) << 16) | (u32(r.data[r.cursor + 2]) << 8) | u32(r.data[r.cursor + 3])
	r.cursor += 4
	return v
}

// read_u64 reads a 64-bit big-endian unsigned integer.
pub fn (mut r Reader) read_u64() !u64 {
	r.ensure(8, 'u64')!
	mut v := u64(0)
	for i := 0; i < 8; i++ {
		v = (v << 8) | u64(r.data[r.cursor + i])
	}
	r.cursor += 8
	return v
}

// read_bytes reads exactly `n` bytes into a fresh (cloned) slice.
// Callers outlive this reader's buffer, so aliasing is avoided; short reads fail.
pub fn (mut r Reader) read_bytes(n int) ![]u8 {
	if n < 0 || n > r.data.len - r.cursor {
		return error('wire.Reader: unexpected EOF reading ${n} bytes (only ${r.remaining()} available)')
	}
	slice := r.data[r.cursor..r.cursor + n].clone()
	r.cursor += n
	return slice
}

// These decode the `opaque T<V>` syntax: length prefix + bytes (RFC 8446 §3).

// read_vector_u8 reads a vector prefixed with a 1-byte length.
pub fn (mut r Reader) read_vector_u8() ![]u8 {
	len := int(r.read_u8()!)
	return r.read_bytes(len)
}

// read_vector_u16 reads a vector prefixed with a 2-byte big-endian length.
pub fn (mut r Reader) read_vector_u16() ![]u8 {
	len := int(r.read_u16()!)
	return r.read_bytes(len)
}

// read_vector_u24 reads a vector prefixed with a 3-byte big-endian length.
// Used for certificate_list chains that can exceed a u16 prefix (§4.4.2).
pub fn (mut r Reader) read_vector_u24() ![]u8 {
	len := int(r.read_u24()!)
	return r.read_bytes(len)
}
