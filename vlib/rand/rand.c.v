module rand

import time
import crypto.rand as crypto_rand

// csprng_u64_pair reads 16 bytes from the OS CSPRNG and packs them into two
// big-endian u64 values, suitable for feeding `internal_uuid`.
fn csprng_u64_pair() (u64, u64) {
	buf := crypto_rand.read(16) or { panic('rand: OS CSPRNG unavailable: ${err}') }
	mut a := u64(0)
	mut b := u64(0)
	for i in 0 .. 8 {
		a = (a << 8) | u64(buf[i])
		b = (b << 8) | u64(buf[8 + i])
	}
	return a, b
}

// uuid_v4 generates a random (v4) UUID, sourcing its 122 random bits from the
// OS CSPRNG (see `crypto.rand.read`).
// See https://en.wikipedia.org/wiki/Universally_unique_identifier#Version_4_(random)
// See https://datatracker.ietf.org/doc/html/rfc9562#name-uuid-version-4
pub fn uuid_v4() string {
	rand_1, rand_2 := csprng_u64_pair()
	return internal_uuid(4, rand_1, rand_2)
}

@[direct_array_access; inline]
fn internal_uuid(version u8, rand_1 u64, rand_2 u64) string {
	// 0                   1                   2                   3
	// 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
	// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
	// |                            rand_1                             |
	// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
	// |           rand_1              |  ver  |        rand_1         |
	// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
	// |var|                        rand_2                             |
	// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
	// |                            rand_2                             |
	// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

	mut parts := [8]u16{}
	parts[0] = u16(rand_1 >> 48)
	parts[1] = u16(rand_1 >> 32)
	parts[2] = u16(rand_1 >> 16)
	parts[3] = u16(rand_1)
	parts[4] = u16(rand_2 >> 48)
	parts[5] = u16(rand_2 >> 32)
	parts[6] = u16(rand_2 >> 16)
	parts[7] = u16(rand_2)

	parts[3] = (parts[3] & 0x0FFF) | (u16(version) << 12) // set version
	parts[4] = (parts[4] & 0x3FFF) | 0x8000 // set variant = 0b10

	mut buf := unsafe { malloc_noscan(37) }
	mut start := 0
	unsafe {
		for i in 0 .. 8 {
			val := parts[i]
			buf[start] = hex_chars[(val >> 12) & 0xF]
			buf[start + 1] = hex_chars[(val >> 8) & 0xF]
			buf[start + 2] = hex_chars[(val >> 4) & 0xF]
			buf[start + 3] = hex_chars[val & 0xF]
			start += 4
			// insert `-` at specified locations
			if start in [8, 13, 18, 23]! {
				buf[start] = `-`
				start++
			}
		}
		buf[36] = 0
		return buf.vstring_with_len(36)
	}
}

// uuid_v7 generates a time-ordered (v7) UUID. The 48-bit Unix millisecond
// timestamp is concatenated with random bits drawn from the OS CSPRNG.
// See https://datatracker.ietf.org/doc/html/rfc9562#name-uuid-version-7
pub fn uuid_v7() string {
	timestamp_48 := u64(time.now().unix_milli()) << 16
	r1, r2 := csprng_u64_pair()
	rand_1 := timestamp_48 | (r1 & 0xFFFF)
	rand_2 := r2
	return internal_uuid(7, rand_1, rand_2)
}

pub struct UUIDSession {
mut:
	counter u8 // 6 bits session counter
}

// new_uuid_v7_session create a new session for generating uuid_v7.
// The 12 bits `rand_a` in the RFC 9652, is replaced by 6 bits
// sub-millisecond timestamp + 6 bits session counter.
// See https://git.postgresql.org/gitweb/?p=postgresql.git;a=commitdiff;h=78c5e141e9c139fc2ff36a220334e4aa25e1b0eb
pub fn new_uuid_v7_session() UUIDSession {
	return UUIDSession{}
}

// next get a new uuid_v7 from current session.
@[ignore_overflow]
pub fn (mut u UUIDSession) next() string {
	timestamp := u64(time.now().unix_nano())
	// make place for holding 4 bits `version`
	timestamp_shift_4bits := (timestamp & 0xFFFF_FFFF_FFFF_0000) | ((timestamp & 0x0000_0000_0000_FFFF) >> 4)
	rand_1 := (timestamp_shift_4bits & 0xFFFF_FFFF_FFFF_FFC0) | u64(u.counter & 0x3F) // 6 bits session counter
	_, rand_2 := csprng_u64_pair()

	u.counter++

	return internal_uuid(7, rand_1, rand_2)
}

const ulid_encoding = '0123456789ABCDEFGHJKMNPQRSTVWXYZ'

// internal_ulid_format encodes a 48-bit Unix millisecond timestamp and 128
// bits of randomness (supplied as two u64s, of which only the low 45 + 35
// bits are used) into a 26-character Crockford Base32 ULID.
@[direct_array_access]
fn internal_ulid_format(unix_time_milli u64, rand_a u64, rand_b u64) string {
	buflen := 26
	mut buf := unsafe { malloc_noscan(27) }
	mut t := unix_time_milli
	mut i := 9
	for i >= 0 {
		unsafe {
			buf[i] = ulid_encoding[int(t & 0x1F)]
		}
		t = t >> 5
		i--
	}
	mut x := rand_a
	i = 10
	for i < 19 {
		unsafe {
			buf[i] = ulid_encoding[int(x & 0x1F)]
		}
		x = x >> 5
		i++
	}
	x = rand_b
	for i < 26 {
		unsafe {
			buf[i] = ulid_encoding[int(x & 0x1F)]
		}
		x = x >> 5
		i++
	}
	unsafe {
		buf[26] = 0
		return buf.vstring_with_len(buflen)
	}
}

@[direct_array_access]
fn internal_ulid_at_millisecond(mut rng PRNG, unix_time_milli u64) string {
	return internal_ulid_format(unix_time_milli, rng.u64(), rng.u64())
}

// ulid generates a unique lexicographically sortable identifier whose
// 80 random bits are sourced from the OS CSPRNG (see `crypto.rand.read`).
// See https://github.com/ulid/spec .
// Note: ULIDs can leak timing information, if you make them public, because
// you can infer the rate at which some resource is being created, like
// users or business transactions.
// (https://news.ycombinator.com/item?id=14526173)
pub fn ulid() string {
	return ulid_at_millisecond(u64(time.utc().unix_milli()))
}

// ulid_at_millisecond does the same as `ulid` but takes a custom Unix millisecond timestamp via `unix_milli`.
pub fn ulid_at_millisecond(unix_time_milli u64) string {
	rand_a, rand_b := csprng_u64_pair()
	return internal_ulid_format(unix_time_milli, rand_a, rand_b)
}

@[direct_array_access]
fn internal_string_from_set(mut rng PRNG, charset string, len int) string {
	if len == 0 {
		return ''
	}
	mut buf := unsafe { malloc_noscan(len + 1) }
	for i in 0 .. len {
		unsafe {
			buf[i] = charset[rng.u32() % u32(charset.len)]
		}
	}
	unsafe {
		buf[len] = 0
	}
	return unsafe { buf.vstring_with_len(len) }
}

@[direct_array_access]
fn internal_fill_buffer_from_set(mut rng PRNG, charset string, mut buf []u8) {
	if buf.len == 0 {
		return
	}
	blen := buf.len
	for i in 0 .. blen {
		unsafe {
			buf[i] = charset[rng.u32() % u32(charset.len)]
		}
	}
}

fn deinit() {
	unsafe {
		default_rng.free() // free the implementation
		free(default_rng) // free the interface wrapper itself
	}
}

// init initializes the default RNG.
fn init() {
	default_rng = new_default()
	at_exit(deinit) or {}
}

@[direct_array_access]
fn read_32(mut rng PRNG, mut buf []u8) {
	p32 := unsafe { &u32(buf.data) }
	u32s := buf.len / 4
	for i in 0 .. u32s {
		unsafe {
			*(p32 + i) = rng.u32()
		}
	}
	for i in u32s * 4 .. buf.len {
		buf[i] = rng.u8()
	}
}

@[direct_array_access]
fn read_64(mut rng PRNG, mut buf []u8) {
	p64 := unsafe { &u64(buf.data) }
	if u64(p64) & 0xF != 0 {
		for i in 0 .. buf.len {
			buf[i] = rng.u8()
		}
		return
	}
	u64s := buf.len / 8
	for i in 0 .. u64s {
		unsafe {
			*(p64 + i) = rng.u64()
		}
	}
	for i in u64s * 8 .. buf.len {
		buf[i] = rng.u8()
	}
}

@[direct_array_access]
fn read_internal(mut rng PRNG, mut buf []u8) {
	match rng.block_size() {
		32 {
			read_32(mut rng, mut buf)
		}
		64 {
			read_64(mut rng, mut buf)
		}
		else {
			for i in 0 .. buf.len {
				buf[i] = rng.u8()
			}
		}
	}
}
