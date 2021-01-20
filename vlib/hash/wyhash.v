// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
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
pub fn sum64_string(key string, seed u64) u64 {
	return wyhash64(key.str, u64(key.len), seed)
}

[inline]
pub fn sum64(key []byte, seed u64) u64 {
	return wyhash64(byteptr(key.data), u64(key.len), seed)
}

[inline]
fn wyhash64(key byteptr, len u64, seed_ u64) u64 {
	if len == 0 {
		return 0
	}
	mut p := key
	mut seed := seed_
	mut i := len & 63
	seed = unsafe{match i {
		0...3 {
			wymum(wyr3(p, i) ^ seed ^ wyp0, seed ^ wyp1)
		}
		4...8 {
			wymum(wyr4(p) ^ seed ^ wyp0, wyr4(p + i - 4) ^ seed ^ wyp1)
		}
		9...16 {
			wymum(wyr8(p) ^ seed ^ wyp0, wyr8(p + i - 8) ^ seed ^ wyp1)
		}
		17...24 {
			wymum(wyr8(p) ^ seed ^ wyp0, wyr8(p + 8) ^ seed ^ wyp1) ^ wymum(wyr8(p + i - 8) ^ seed ^ wyp2, seed ^ wyp3)
		}
		25...32 {
			wymum(wyr8(p) ^ seed ^ wyp0, wyr8(p + 8) ^ seed ^ wyp1) ^ wymum(wyr8(p + 16) ^ seed ^ wyp2, wyr8(p + i - 8) ^ seed ^ wyp3)
		}
		else {
			wymum(wyr8(p) ^ seed ^ wyp0, wyr8(p + 8) ^ seed ^ wyp1) ^ wymum(wyr8(p + 16) ^ seed ^ wyp2, wyr8(p + 24) ^ seed ^ wyp3) ^ wymum(wyr8(p + i - 32) ^ seed ^ wyp1, wyr8(p + i - 24) ^ seed ^ wyp2) ^ wymum(wyr8(p + i - 16) ^ seed ^ wyp3, wyr8(p + i - 8) ^ seed ^ wyp0)
		}
	}}
	if i == len {
		return wymum(seed, len ^ wyp4)
	}
	mut see1 := seed
	mut see2 := seed
	mut see3 := seed
	unsafe {
		p = p + i
		for i = len - i; i >= 64; i -= 64 {
			seed = wymum(wyr8(p) ^ seed ^ wyp0, wyr8(p + 8) ^ seed ^ wyp1)
			see1 = wymum(wyr8(p + 16) ^ see1 ^ wyp2, wyr8(p + 24) ^ see1 ^ wyp3)
			see2 = wymum(wyr8(p + 32) ^ see2 ^ wyp1, wyr8(p + 40) ^ see2 ^ wyp2)
			see3 = wymum(wyr8(p + 48) ^ see3 ^ wyp3, wyr8(p + 56) ^ see3 ^ wyp0)
			p = p + 64
		}
	}
	return wymum(seed ^ see1 ^ see2, see3 ^ len ^ wyp4)
}

[inline]
fn wyrotr(v u64, k u32) u64 {
	return (v>>k) | (v<<(64 - k))
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
	x1 := a>>32
	y0 := b & mask32
	y1 := b>>32
	w0 := x0 * y0
	t := x1 * y0 + (w0>>32)
	mut w1 := t & mask32
	w2 := t>>32
	w1 += x0 * y1
	hi := x1 * y1 + w2 + (w1>>32)
	lo := a * b
	return hi ^ lo
}

[inline]
fn wyr3(p byteptr, k u64) u64 {
	unsafe {
		return (u64(p[0])<<16) | (u64(p[k>>1])<<8) | u64(p[k - 1])
	}
}

[inline]
fn wyr4(p byteptr) u64 {
	unsafe {
		return u32(p[0]) | (u32(p[1])<<u32(8)) | (u32(p[2])<<u32(16)) | (u32(p[3])<<u32(24))
	}
}

[inline]
fn wyr8(p byteptr) u64 {
	unsafe {
		return u64(p[0]) | (u64(p[1])<<8) | (u64(p[2])<<16) | (u64(p[3])<<24) | (u64(p[4])<<32) | (u64(p[5])<<40) | (u64(p[6])<<48) | (u64(p[7])<<56)
	}
}
