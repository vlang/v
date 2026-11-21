// Copyright (c) 2026 blackshirt.
//
// writer.v appends TLS wire data to an in-memory buffer, emitting big-endian
// integers and length-prefixed vectors for the handshake and record encoders.
module wire

// Writer appends TLS wire data to an in-memory buffer, in network byte order
// (RFC 8446 §2); @[noinit] forces construction through new_writer.
@[noinit]
pub struct Writer {
mut:
	buf []u8 // bytes accumulated for the frame currently being built
}

// default_writer_cap is the buffer capacity used when new_writer is given a
// non-positive size; it comfortably fits a typical handshake message so the
// common case avoids grow-and-copy without forcing every caller to pick a size.
const default_writer_cap = 128

// new_writer creates a Writer with an optional preallocated capacity.
// Preallocating the frame size avoids grow-and-copy; capacity <= 0 uses a default.
pub fn new_writer(capacity int) Writer {
	cap_size := if capacity > 0 { capacity } else { default_writer_cap }
	return Writer{
		buf: []u8{cap: cap_size}
	}
}

// len returns the current number of bytes written.
pub fn (w &Writer) len() int {
	return w.buf.len
}

// bytes returns a copy of the written bytes.
pub fn (w &Writer) bytes() []u8 {
	return w.buf.clone()
}

// write_u8 appends a single 8-bit unsigned integer.
pub fn (mut w Writer) write_u8(v u8) {
	w.buf << v
}

// write_u16 appends a 16-bit big-endian unsigned integer (RFC 8446 §2).
pub fn (mut w Writer) write_u16(v u16) {
	w.buf << u8((v >> 8) & 0xff)
	w.buf << u8(v & 0xff)
}

// write_u24 appends a 24-bit big-endian unsigned integer (lower 3 bytes of v).
// The upper byte is dropped: callers pass pre-validated lengths <= 0xffffff.
pub fn (mut w Writer) write_u24(v u32) {
	w.buf << u8((v >> 16) & 0xff)
	w.buf << u8((v >> 8) & 0xff)
	w.buf << u8(v & 0xff)
}

// write_u32 appends a 32-bit big-endian unsigned integer (RFC 8446 §2).
pub fn (mut w Writer) write_u32(v u32) {
	w.buf << u8((v >> 24) & 0xff)
	w.buf << u8((v >> 16) & 0xff)
	w.buf << u8((v >> 8) & 0xff)
	w.buf << u8(v & 0xff)
}

// write_u64 appends a 64-bit big-endian unsigned integer.
pub fn (mut w Writer) write_u64(v u64) {
	w.buf << u8((v >> 56) & 0xff)
	w.buf << u8((v >> 48) & 0xff)
	w.buf << u8((v >> 40) & 0xff)
	w.buf << u8((v >> 32) & 0xff)
	w.buf << u8((v >> 24) & 0xff)
	w.buf << u8((v >> 16) & 0xff)
	w.buf << u8((v >> 8) & 0xff)
	w.buf << u8(v & 0xff)
}

// write_bytes appends raw bytes to the buffer.
pub fn (mut w Writer) write_bytes(data []u8) {
	w.buf << data
}

// These emit the `opaque T<V>` framing of RFC 8446 §3 (length prefix + bytes).

// write_vector_u8 appends a byte slice prefixed with its 1-byte length.
pub fn (mut w Writer) write_vector_u8(data []u8) ! {
	if data.len > 0xff {
		return error('wire.Writer: data length ${data.len} exceeds u8 max (255)')
	}
	w.write_u8(u8(data.len))
	w.write_bytes(data)
}

// write_vector_u16 appends a byte slice prefixed with its 2-byte big-endian length.
pub fn (mut w Writer) write_vector_u16(data []u8) ! {
	if data.len > 0xffff {
		return error('wire.Writer: data length ${data.len} exceeds u16 max (65535)')
	}
	w.write_u16(u16(data.len))
	w.write_bytes(data)
}

// write_vector_u24 appends a byte slice prefixed with its 3-byte big-endian length.
pub fn (mut w Writer) write_vector_u24(data []u8) ! {
	if data.len > 0xffffff {
		return error('wire.Writer: data length ${data.len} exceeds u24 max (16777215)')
	}
	w.write_u24(u32(data.len))
	w.write_bytes(data)
}
