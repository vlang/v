// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// This is an implementation of wyhash v4
// from https://github.com/wangyi-fudan/wyhash
// TODO: try pointers instead of slices
module wyhash

import (
	math.bits
)

const (
	wyp0 = 0xa0761d6478bd642f
	wyp1 = 0xe7037ed1a0b428db
	wyp2 = 0x8ebc6af09c88c6e3
	wyp3 = 0x589965cc75374cc3
	wyp4 = 0x1d8e4e27c47d124f
)

[inline]
pub fn sum64(key []byte seed u64, ) u64 {
	return wyhash64(key, u64(key.len), seed)
}

[inline]
fn wyhash64(key []byte, len, seed_ u64) u64 {
	if len == 0 {
		return 0
	}
	mut p := &key.data[0]
	mut seed := seed_
	mut i := len & 63
	if i < 4 {
		seed = wymum(wyr3(p, i) ^ seed ^ wyp0, seed ^ wyp1)
	}
	else if i <= 8 {
		seed = wymum(wyr4(p) ^ seed ^ wyp0, wyr4(p+i-4) ^ seed ^ wyp1)
	}
	else if i <= 16 {
		seed = wymum(wyr8(p) ^ seed ^ wyp0, wyr8(p+i-8) ^ seed ^ wyp1)
	}
	else if i <= 24 {
		seed = wymum(wyr8(p) ^ seed ^ wyp0, wyr8(p+8) ^ seed ^ wyp1) ^ wymum(wyr8(p+i-8) ^ seed ^ wyp2, seed ^ wyp3)
	}
	else if i <= 32 {
		seed = wymum(wyr8(p) ^ seed ^ wyp0, wyr8(p+8) ^ seed ^ wyp1) ^ wymum(wyr8(p+16) ^ seed ^ wyp2, wyr8(p+i-8) ^ seed ^ wyp3)
	}
	else {
		seed = wymum(wyr8(p) ^ seed ^ wyp0, wyr8(p+8) ^ seed ^ wyp1) ^ wymum(wyr8(p+16) ^ seed ^ wyp2, wyr8(p+24) ^ seed ^ wyp3) ^ wymum(wyr8(p+i-32) ^ seed ^ wyp1, wyr8(p+i-24) ^ seed ^ wyp2) ^ wymum(wyr8(p+i-16) ^ seed ^ wyp3, wyr8(p+i-8) ^ seed ^ wyp0)
	}
	if i == len {
		return wymum(seed, len ^ wyp4)
	}
	mut see1 := seed
	mut see2 := seed
	mut see3 := seed
	p = p+=i
	for i = len - i; i >= 64; i -= 64 {
		seed = wymum(wyr8(p) ^ seed ^ wyp0, wyr8(p+8) ^ seed ^ wyp1)
		see1 = wymum(wyr8(p+16) ^ see1 ^ wyp2, wyr8(p+24) ^ see1 ^ wyp3)
		see2 = wymum(wyr8(p+32) ^ see2 ^ wyp1, wyr8(p+40) ^ see2 ^ wyp2)
		see3 = wymum(wyr8(p+48) ^ see3 ^ wyp3, wyr8(p+56) ^ see3 ^ wyp0)
		p = p+64
	}
	return wymum(seed ^ see1 ^ see2, see3 ^ len ^ wyp4)
}

[inline]
fn wymum(a, b u64) u64 {
	hi,lo := bits.mul64(a, b)
	return hi ^ lo
}

[inline]
fn wyr3(p byteptr, k u64) u64 {
	return (u64(p[0])<<16) | (u64(p[k>>1])<<8) | u64(p[k - 1])
}

[inline]
fn wyr4(p byteptr) u64 {
	//mut v := u64(0)
	//memcpy(&v,  p,  4)
	//return v
	return u32(p[0]) | (u32(p[1])<<u32(8)) | (u32(p[2])<<u32(16)) | (u32(p[3])<<u32(24))
}

[inline]
fn wyr8(p byteptr) u64 {
	//mut v := u64(0)
	//memcpy(&v,  p,  8)
	//return v
	return u64(p[0]) | (u64(p[1])<<u64(8)) | (u64(p[2])<<u64(16)) | (u64(p[3])<<u64(24)) | (u64(p[4])<<u64(32)) | (u64(p[5])<<u64(40)) | (u64(p[6])<<u64(48)) | (u64(p[7])<<u64(56))
}
