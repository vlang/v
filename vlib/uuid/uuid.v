// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Based off:   https://github.com/golang/go/tree/master/src/uuid
// Last commit: https://github.com/golang/go/commit/0cd3d3e1391ae989cc551c6bae356e6a521a55a1

// Package uuid generates and parses UUIDs as defined in RFC 9562.
// The random components of new UUIDs come from the cryptographically
// secure random number generator of the operating system (`crypto.rand`).
@[has_globals]
module uuid

import crypto.rand
import sync
import time

// UUID is a Universally Unique Identifier as specified in RFC 9562.
// UUIDs can be compared with `==` and used as map keys.
pub type UUID = [16]u8

// nil_uuid is the Nil UUID `00000000-0000-0000-0000-000000000000`, defined in
// section 5.9 of RFC 9562.
pub const nil_uuid = UUID([16]u8{})

// max_uuid is the Max UUID `ffffffff-ffff-ffff-ffff-ffffffffffff`, defined in
// section 5.10 of RFC 9562.
pub const max_uuid = UUID([16]u8{init: 0xff})

const hex_digits = '0123456789abcdef'

__global (
	v7_mutex          &sync.Mutex
	v7_last_secs      u64
	v7_last_timestamp u64
)

fn init() {
	v7_mutex = sync.new_mutex()
}

// parse returns the UUID represented by `s`, which can be in any of the forms
// `f81d4fae-7dec-11d0-a765-00a0c91e6bf6`, `{f81d4fae-7dec-11d0-a765-00a0c91e6bf6}`,
// `urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6` or `f81d4fae7dec11d0a76500a0c91e6bf6`.
// The hexadecimal digits may be in any case.
@[direct_array_access]
pub fn parse(s string) !UUID {
	mut u := UUID{}
	if s.len == 32 {
		for i in 0 .. 16 {
			hi := hex_value(s[2 * i])
			lo := hex_value(s[2 * i + 1])
			if hi | lo > 0x0f {
				return error('invalid uuid')
			}
			u[i] = (hi << 4) | lo
		}
		return u
	}
	mut text := s
	if s.len == 45 && s.starts_with('urn:uuid:') {
		text = s[9..]
	} else if s.len == 38 && s[0] == `{` && s[37] == `}` {
		text = s[1..37]
	}
	if text.len != 36 || text[8] != `-` || text[13] != `-` || text[18] != `-` || text[23] != `-` {
		return error('invalid uuid')
	}
	mut j := 0
	for i in 0 .. 16 {
		if j == 8 || j == 13 || j == 18 || j == 23 {
			j++
		}
		hi := hex_value(text[j])
		lo := hex_value(text[j + 1])
		if hi | lo > 0x0f {
			return error('invalid uuid')
		}
		u[i] = (hi << 4) | lo
		j += 2
	}
	return u
}

// hex_value is the value of the hexadecimal digit `c`, or 0xff if `c` is not one.
@[inline]
fn hex_value(c u8) u8 {
	return match c {
		`0`...`9` { c - `0` }
		`a`...`f` { c - `a` + 10 }
		`A`...`F` { c - `A` + 10 }
		else { 0xff }
	}
}

// str returns the lowercase hex-and-dash form of `u` defined in RFC 9562,
// like `f81d4fae-7dec-11d0-a765-00a0c91e6bf6`.
@[direct_array_access]
pub fn (u UUID) str() string {
	mut buf := unsafe { malloc_noscan(37) }
	mut j := 0
	for i in 0 .. 16 {
		if i == 4 || i == 6 || i == 8 || i == 10 {
			unsafe {
				buf[j] = `-`
			}
			j++
		}
		unsafe {
			buf[j] = hex_digits[u[i] >> 4]
			buf[j + 1] = hex_digits[u[i] & 0x0f]
		}
		j += 2
	}
	unsafe {
		buf[36] = 0
		return buf.vstring_with_len(36)
	}
}

// compare returns -1 if `u` sorts before `v`, 1 if it sorts after `v`, and 0
// if they are the same, in the big-endian byte order of section 6.11 of RFC 9562.
@[direct_array_access]
pub fn (u UUID) compare(v UUID) int {
	for i in 0 .. 16 {
		if u[i] != v[i] {
			return if u[i] < v[i] { -1 } else { 1 }
		}
	}
	return 0
}

// new returns a new UUID, generated with an algorithm suitable for most purposes.
// At this time it is the same as `new_v4`.
pub fn new() UUID {
	return new_v4()
}

// new_v4 returns a new version 4 UUID, which holds 122 random bits.
pub fn new_v4() UUID {
	mut u := UUID{}
	fill_random(mut u, 0)
	u.set_version(4)
	u.set_variant(0b10)
	return u
}

// new_v7 returns a new version 7 UUID: the Unix time in milliseconds in its 48
// most significant bits, then a 12-bit fraction of the millisecond and 62
// random bits. The UUIDs it returns always sort in increasing order, except
// when the system clock moves backwards.
pub fn new_v7() UUID {
	return new_v7_from(utc_now)
}

// utc_now is the Unix time as seconds and nanoseconds within the second.
fn utc_now() (u64, u64) {
	now := time.utc()
	return u64(now.unix()), u64(now.nanosecond)
}

// new_v7_from is `new_v7` with the clock `now`. It reads the clock under the
// lock, so that the times it gets increase in the order of the UUIDs.
fn new_v7_from(now fn () (u64, u64)) UUID {
	// The 60-bit timestamp is 48 bits of milliseconds and 12 bits of
	// 1/4096 milliseconds, which RFC 9562 allows in the `rand_a` field.
	v7_mutex.lock()
	secs, nanos := now()
	msecs := nanos / 1_000_000
	frac := nanos - 1_000_000 * msecs
	mut timestamp := (1000 * secs + msecs) << 12
	timestamp += (frac * 4096) / 1_000_000
	if v7_last_secs > secs {
		// The clock moved backwards: the previous UUIDs do not count.
	} else if timestamp <= v7_last_timestamp {
		// Keep the order of the UUIDs that share a timestamp.
		timestamp = v7_last_timestamp + 1
	}
	v7_last_secs = secs
	v7_last_timestamp = timestamp
	v7_mutex.unlock()

	// Leave a gap for the 4 bits of the version.
	hibits := ((timestamp << 4) & 0xffff_ffff_ffff_0000) | (timestamp & 0x0fff)
	mut u := UUID{}
	for i in 0 .. 8 {
		u[i] = u8(hibits >> (56 - 8 * i))
	}
	fill_random(mut u, 8)
	u.set_version(7)
	u.set_variant(0b10)
	return u
}

// fill_random sets the bytes of `u` from `from` on to random values.
fn fill_random(mut u UUID, from int) {
	bytes := rand.bytes(16 - from) or { panic('uuid: ${err}') }
	for i, b in bytes {
		u[from + i] = b
	}
}

fn (mut u UUID) set_version(version u8) {
	u[6] = (u[6] & 0b0000_1111) | (version << 4)
}

fn (mut u UUID) set_variant(variant u8) {
	u[8] = (u[8] & 0b0011_1111) | (variant << 6)
}
