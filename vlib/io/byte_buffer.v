module io

// ByteBuffer is a cursor-based byte buffer for protocol parsing: producers
// append with put_*, flip switches to read mode, consumers take with get_*,
// and compact discards consumed bytes. It is the V-idiomatic equivalent of
// Java's java.nio.ByteBuffer, minus the limit register: the readable region
// is always pos..len, and puts always append at the end. Integer codecs are
// hand-rolled shifts (rather than encoding.binary) to keep io dependency-free.
pub struct ByteBuffer {
mut:
	buf []u8
	pos int // read cursor; the write end is buf.len
}

// new_byte_buffer creates an empty ByteBuffer with the given initial capacity.
pub fn new_byte_buffer(cap int) &ByteBuffer {
	return &ByteBuffer{
		buf: []u8{cap: if cap > 0 { cap } else { 0 }}
	}
}

// remaining returns the number of unread bytes.
pub fn (b &ByteBuffer) remaining() int {
	return b.buf.len - b.pos
}

// len returns the total buffered bytes (read plus unread).
pub fn (b &ByteBuffer) len() int {
	return b.buf.len
}

// flip switches from write mode to read mode: the cursor returns to the
// start so the appended bytes can be consumed with get_*.
pub fn (mut b ByteBuffer) flip() {
	b.pos = 0
}

// rewind returns the cursor to the start without discarding anything.
pub fn (mut b ByteBuffer) rewind() {
	b.pos = 0
}

// compact discards consumed bytes, keeping the unread remainder.
pub fn (mut b ByteBuffer) compact() {
	if b.pos == 0 {
		return
	}
	keep := b.buf.len - b.pos
	for i in 0 .. keep {
		b.buf[i] = b.buf[b.pos + i]
	}
	b.buf.trim(keep)
	b.pos = 0
}

// clear discards everything, resetting cursor and length.
pub fn (mut b ByteBuffer) clear() {
	b.buf.trim(0)
	b.pos = 0
}

// put_u8 appends one byte.
pub fn (mut b ByteBuffer) put_u8(v u8) {
	b.buf << v
}

// put_u16_be appends v big-endian.
pub fn (mut b ByteBuffer) put_u16_be(v u16) {
	b.buf << u8(v >> 8)
	b.buf << u8(v)
}

// put_u16_le appends v little-endian.
pub fn (mut b ByteBuffer) put_u16_le(v u16) {
	b.buf << u8(v)
	b.buf << u8(v >> 8)
}

// put_u32_be appends v big-endian.
pub fn (mut b ByteBuffer) put_u32_be(v u32) {
	b.buf << u8(v >> 24)
	b.buf << u8(v >> 16)
	b.buf << u8(v >> 8)
	b.buf << u8(v)
}

// put_u32_le appends v little-endian.
pub fn (mut b ByteBuffer) put_u32_le(v u32) {
	b.buf << u8(v)
	b.buf << u8(v >> 8)
	b.buf << u8(v >> 16)
	b.buf << u8(v >> 24)
}

// put_u64_be appends v big-endian.
pub fn (mut b ByteBuffer) put_u64_be(v u64) {
	b.buf << u8(v >> 56)
	b.buf << u8(v >> 48)
	b.buf << u8(v >> 40)
	b.buf << u8(v >> 32)
	b.buf << u8(v >> 24)
	b.buf << u8(v >> 16)
	b.buf << u8(v >> 8)
	b.buf << u8(v)
}

// put_u64_le appends v little-endian.
pub fn (mut b ByteBuffer) put_u64_le(v u64) {
	b.buf << u8(v)
	b.buf << u8(v >> 8)
	b.buf << u8(v >> 16)
	b.buf << u8(v >> 24)
	b.buf << u8(v >> 32)
	b.buf << u8(v >> 40)
	b.buf << u8(v >> 48)
	b.buf << u8(v >> 56)
}

// put_bytes appends raw bytes.
pub fn (mut b ByteBuffer) put_bytes(data []u8) {
	b.buf << data
}

// peek reserves n unread bytes, erroring on underflow.
fn (b &ByteBuffer) peek(n int) ! {
	if n < 0 || n > b.remaining() {
		return error('io: ByteBuffer underflow')
	}
}

// get_u8 consumes one byte.
pub fn (mut b ByteBuffer) get_u8() !u8 {
	b.peek(1)!
	v := b.buf[b.pos]
	b.pos++
	return v
}

// get_u16_be consumes a big-endian u16.
pub fn (mut b ByteBuffer) get_u16_be() !u16 {
	b.peek(2)!
	v := u16(b.buf[b.pos]) << 8 | u16(b.buf[b.pos + 1])
	b.pos += 2
	return v
}

// get_u16_le consumes a little-endian u16.
pub fn (mut b ByteBuffer) get_u16_le() !u16 {
	b.peek(2)!
	v := u16(b.buf[b.pos]) | u16(b.buf[b.pos + 1]) << 8
	b.pos += 2
	return v
}

// get_u32_be consumes a big-endian u32.
pub fn (mut b ByteBuffer) get_u32_be() !u32 {
	b.peek(4)!
	v := u32(b.buf[b.pos]) << 24 | u32(b.buf[b.pos + 1]) << 16 | u32(b.buf[b.pos + 2]) << 8 | u32(b.buf[b.pos + 3])
	b.pos += 4
	return v
}

// get_u32_le consumes a little-endian u32.
pub fn (mut b ByteBuffer) get_u32_le() !u32 {
	b.peek(4)!
	v := u32(b.buf[b.pos]) | u32(b.buf[b.pos + 1]) << 8 | u32(b.buf[b.pos + 2]) << 16 | u32(b.buf[b.pos + 3]) << 24
	b.pos += 4
	return v
}

// get_u64_be consumes a big-endian u64.
pub fn (mut b ByteBuffer) get_u64_be() !u64 {
	b.peek(8)!
	v := u64(b.buf[b.pos]) << 56 | u64(b.buf[b.pos + 1]) << 48 | u64(b.buf[b.pos + 2]) << 40 | u64(b.buf[b.pos + 3]) << 32 | u64(b.buf[b.pos + 4]) << 24 | u64(b.buf[b.pos + 5]) << 16 | u64(b.buf[b.pos + 6]) << 8 | u64(b.buf[b.pos + 7])
	b.pos += 8
	return v
}

// get_u64_le consumes a little-endian u64.
pub fn (mut b ByteBuffer) get_u64_le() !u64 {
	b.peek(8)!
	v := u64(b.buf[b.pos]) | u64(b.buf[b.pos + 1]) << 8 | u64(b.buf[b.pos + 2]) << 16 | u64(b.buf[b.pos + 3]) << 24 | u64(b.buf[b.pos + 4]) << 32 | u64(b.buf[b.pos + 5]) << 40 | u64(b.buf[b.pos + 6]) << 48 | u64(b.buf[b.pos + 7]) << 56
	b.pos += 8
	return v
}

// get_bytes consumes n raw bytes.
pub fn (mut b ByteBuffer) get_bytes(n int) ![]u8 {
	b.peek(n)!
	out := b.buf[b.pos..b.pos + n].clone()
	b.pos += n
	return out
}
