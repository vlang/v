// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module netcode

import gg
import math

// Packet is a little-endian byte stream used for compact network messages.
pub struct Packet {
pub mut:
	data []u8
	pos  int
}

pub fn new_packet() Packet {
	return Packet{}
}

pub fn from_bytes(bytes []u8) Packet {
	return Packet{ data: bytes.clone() }
}

fn ensure(mut p Packet, extra int) {
	for i64(p.data.len) < i64(p.pos) + i64(extra) {
		p.data << 0
	}
}

// can_read uses i64 arithmetic so a corrupted public `pos` cannot wrap past
// the bounds check and cause an out-of-range slice access.
fn (p &Packet) can_read(n int) bool {
	return p.pos >= 0 && i64(p.pos) + i64(n) <= i64(p.data.len)
}

pub fn (mut p Packet) write_u8(v u8) {
	ensure(mut p, 1)
	p.data[p.pos] = v
	p.pos++
}

pub fn (mut p Packet) read_u8() !u8 {
	if !p.can_read(1) {
		return error('netcode: packet truncated')
	}
	v := p.data[p.pos]
	p.pos++
	return v
}

pub fn (mut p Packet) write_u16(v u16) {
	ensure(mut p, 2)
	p.data[p.pos] = u8(v)
	p.data[p.pos + 1] = u8(v >> 8)
	p.pos += 2
}

pub fn (mut p Packet) read_u16() !u16 {
	if !p.can_read(2) {
		return error('netcode: packet truncated')
	}
	v := u16(p.data[p.pos]) | u16(p.data[p.pos + 1]) << 8
	p.pos += 2
	return v
}

pub fn (mut p Packet) write_u32(v u32) {
	ensure(mut p, 4)
	for i in 0 .. 4 {
		p.data[p.pos + i] = u8(v >> u32(i * 8))
	}
	p.pos += 4
}

pub fn (mut p Packet) read_u32() !u32 {
	if !p.can_read(4) {
		return error('netcode: packet truncated')
	}
	mut v := u32(0)
	for i in 0 .. 4 {
		v |= u32(p.data[p.pos + i]) << u32(i * 8)
	}
	p.pos += 4
	return v
}

pub fn (mut p Packet) write_f32(v f32) {
	p.write_u32(math.f32_bits(v))
}

pub fn (mut p Packet) read_f32() !f32 {
	return math.f32_from_bits(p.read_u32()!)
}

// write_string stores a u16 byte length, so longer strings are rejected
// instead of silently truncating the value read back by the peer.
pub fn (mut p Packet) write_string(s string) ! {
	if s.len > 65535 {
		return error('netcode: string too long for packet')
	}
	p.write_u16(u16(s.len))
	for i in 0 .. s.len {
		p.write_u8(u8(s[i]))
	}
}

pub fn (mut p Packet) read_string() !string {
	len := p.read_u16()!
	if !p.can_read(int(len)) {
		return error('netcode: packet truncated')
	}
	// Build from a preallocated buffer: `s += ...` is quadratic for long
	// strings because every append copies the whole prefix.
	mut bytes := []u8{len: int(len)}
	for i in 0 .. int(len) {
		bytes[i] = p.read_u8()!
	}
	return bytes.bytestr()
}

pub fn (mut p Packet) write_vec3(v gg.Vec3) {
	p.write_f32(v.x)
	p.write_f32(v.y)
	p.write_f32(v.z)
}

pub fn (mut p Packet) read_vec3() !gg.Vec3 {
	return gg.Vec3{p.read_f32()!, p.read_f32()!, p.read_f32()!}
}

pub fn (p &Packet) bytes() []u8 {
	return p.data[..p.pos].clone()
}

pub fn (p &Packet) remaining() int {
	return p.data.len - p.pos
}
