// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// this is an implementation of wyhash v4
// from https://github.com/wangyi-fudan/wyhash
//
// TODO: use u128 once implemented
// currently the C version performs slightly better
// because it uses 128 bit int when available and
// branch prediction hints. the C version will be
// removed once the perfomance is matched.
// you can test performance by running:
// `v run cmd/tools/bench/wyhash.v`
// try running with and without the `-prod` flag
module hash

const (
	wyp0 = u64(0xa0761d6478bd642f)
	wyp1 = u64(0xe7037ed1a0b428db)
	wyp2 = u64(0x8ebc6af09c88c6e3)
	wyp3 = u64(0x589965cc75374cc3)
	wyp4 = u64(0x1d8e4e27c47d124f)
)

[inline]
fn wyrotr(v u64, k u32) u64 {
	return (v >> k) | (v << (64 - k))
}

[inline]
pub fn wymum(a u64, b u64) u64 {
	/*
	mut r := u128(a)
	r = r*b
	return (r>>64)^r
	*/
	mask32 := u32(4294967295)
	x0 := a & mask32
	x1 := a >> 32
	y0 := b & mask32
	y1 := b >> 32
	w0 := x0 * y0
	t := x1 * y0 + (w0 >> 32)
	mut w1 := t & mask32
	w2 := t >> 32
	w1 += x0 * y1
	hi := x1 * y1 + w2 + (w1 >> 32)
	lo := a * b
	return hi ^ lo
}

[inline]
fn wyr3(p &u8, k u64) u64 {
	unsafe {
		return (u64(p[0]) << 16) | (u64(p[k >> 1]) << 8) | u64(p[k - 1])
	}
}

[inline]
fn wyr4(p &u8) u64 {
	unsafe {
		return u32(p[0]) | (u32(p[1]) << u32(8)) | (u32(p[2]) << u32(16)) | (u32(p[3]) << u32(24))
	}
}

[inline]
fn wyr8(p &u8) u64 {
	unsafe {
		return u64(p[0]) | (u64(p[1]) << 8) | (u64(p[2]) << 16) | (u64(p[3]) << 24) | (u64(p[4]) << 32) | (u64(p[5]) << 40) | (u64(p[6]) << 48) | (u64(p[7]) << 56)
	}
}
