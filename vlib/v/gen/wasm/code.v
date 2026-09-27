module wasm

// code.v emits the WASM instruction stream for a single function body. It is a
// thin layer over a []u8 buffer with one method per opcode used by the backend,
// analogous to the arm64 backend's asm.v instruction encoders.

pub struct Code {
pub mut:
	bytes []u8
}

@[inline]
pub fn (mut c Code) raw(b u8) {
	c.bytes << b
}

// ---- constants ----

pub fn (mut c Code) i32_const(v i64) {
	c.bytes << 0x41
	// Wrap to a signed 32-bit value so the LEB128 stays in i32 range; e.g.
	// u32(4000000000) must encode as the 32-bit pattern, not a 5-byte i64.
	leb_i(mut c.bytes, i64(i32(v)))
}

pub fn (mut c Code) i64_const(v i64) {
	c.bytes << 0x42
	leb_i(mut c.bytes, v)
}

pub fn (mut c Code) f32_const(v f32) {
	c.bytes << 0x43
	bits := u32(f32_bits(v))
	for i in 0 .. 4 {
		c.bytes << u8((bits >> (8 * i)) & 0xff)
	}
}

pub fn (mut c Code) f64_const(v f64) {
	c.bytes << 0x44
	bits := f64_bits(v)
	for i in 0 .. 8 {
		c.bytes << u8((bits >> (8 * i)) & 0xff)
	}
}

fn f32_bits(v f32) u32 {
	return unsafe { *(&u32(&v)) }
}

fn f64_bits(v f64) u64 {
	return unsafe { *(&u64(&v)) }
}

// ---- locals / globals ----

pub fn (mut c Code) local_get(idx int) {
	c.bytes << 0x20
	leb_u(mut c.bytes, u64(idx))
}

pub fn (mut c Code) local_set(idx int) {
	c.bytes << 0x21
	leb_u(mut c.bytes, u64(idx))
}

pub fn (mut c Code) local_tee(idx int) {
	c.bytes << 0x22
	leb_u(mut c.bytes, u64(idx))
}

pub fn (mut c Code) global_get(idx int) {
	c.bytes << 0x23
	leb_u(mut c.bytes, u64(idx))
}

pub fn (mut c Code) global_set(idx int) {
	c.bytes << 0x24
	leb_u(mut c.bytes, u64(idx))
}

// ---- memory ----

pub fn (mut c Code) load(op u8, align int, offset int) {
	c.bytes << op
	leb_u(mut c.bytes, u64(align))
	leb_u(mut c.bytes, u64(offset))
}

pub fn (mut c Code) store(op u8, align int, offset int) {
	c.bytes << op
	leb_u(mut c.bytes, u64(align))
	leb_u(mut c.bytes, u64(offset))
}

// i32_store / i32_load with natural alignment for a full i32.
pub fn (mut c Code) i32_store(offset int) {
	c.store(0x36, 2, offset)
}

pub fn (mut c Code) i32_store8(offset int) {
	c.store(0x3a, 0, offset)
}

pub fn (mut c Code) i32_load(offset int) {
	c.load(0x28, 2, offset)
}

// ---- control flow ----

pub fn (mut c Code) block_void() {
	c.bytes << 0x02
	c.bytes << 0x40
}

pub fn (mut c Code) loop_void() {
	c.bytes << 0x03
	c.bytes << 0x40
}

pub fn (mut c Code) if_void() {
	c.bytes << 0x04
	c.bytes << 0x40
}

pub fn (mut c Code) else_() {
	c.bytes << 0x05
}

pub fn (mut c Code) end() {
	c.bytes << 0x0b
}

pub fn (mut c Code) br(depth int) {
	c.bytes << 0x0c
	leb_u(mut c.bytes, u64(depth))
}

pub fn (mut c Code) br_if(depth int) {
	c.bytes << 0x0d
	leb_u(mut c.bytes, u64(depth))
}

pub fn (mut c Code) ret() {
	c.bytes << 0x0f
}

pub fn (mut c Code) call(idx int) {
	c.bytes << 0x10
	leb_u(mut c.bytes, u64(idx))
}

pub fn (mut c Code) drop() {
	c.bytes << 0x1a
}
