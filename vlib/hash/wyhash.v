// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
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
// removed once the performance is matched.
// you can test performance by running:
// `v run cmd/tools/bench/wyhash.v`
// try running with and without the `-prod` flag
module hash

const wyp0 = u64(0x2d358dccaa6c78a5)
const wyp1 = u64(0x8bb84b93962eacc9)
const wyp2 = u64(0x4b33a62ed433d4a3)
const wyp3 = u64(0x4d5a2da51de1aa47)

// wymum returns a hash by performing multiply and mix on `a` and `b`.
@[ignore_overflow; inline]
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
