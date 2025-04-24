module rand

import time

const clock_seq_hi_and_reserved_valid_values = [`8`, `9`, `a`, `b`]!

// uuid_v4 generates a random (v4) UUID
// See https://en.wikipedia.org/wiki/Universally_unique_identifier#Version_4_(random)
pub fn uuid_v4() string {
	return internal_uuid_v4(mut default_rng)
}

@[direct_array_access]
fn internal_uuid_v4(mut rng PRNG) string {
	buflen := 36
	mut buf := unsafe { malloc_noscan(37) }
	mut i_buf := 0
	mut x := u64(0)
	mut d := u8(0)
	for i_buf < buflen {
		mut c := 0
		x = rng.u64()
		// do most of the bit manipulation at once:
		x &= 0x0F0F0F0F0F0F0F0F
		x += 0x3030303030303030
		// write the ASCII codes to the buffer:
		for c < 8 && i_buf < buflen {
			d = u8(x)
			unsafe {
				buf[i_buf] = if d > 0x39 { d + 0x27 } else { d }
			}
			i_buf++
			c++
			x = x >> 8
		}
	}
	// there are still some random bits in x:
	x = x >> 8
	d = u8(x)
	unsafe {
		// From https://www.ietf.org/rfc/rfc4122.txt :
		// >> Set the two most significant bits (bits 6 and 7) of the clock_seq_hi_and_reserved
		// >> to zero and one, respectively.
		// all nibbles starting with 10 are: 1000, 1001, 1010, 1011 -> hex digits `8`, `9`, `a`, `b`
		// these are stored in clock_seq_hi_and_reserved_valid_values, choose one of them at random:
		buf[19] = clock_seq_hi_and_reserved_valid_values[d & 0x03]
		// >> Set the four most significant bits (bits 12 through 15) of the
		// >> time_hi_and_version field to the 4-bit version number from Section 4.1.3.
		buf[14] = `4`
		buf[8] = `-`
		buf[13] = `-`
		buf[18] = `-`
		buf[23] = `-`
		buf[buflen] = 0 // ensure the string will be 0 terminated, just in case
		// for i in 0..37 { println('i: ${i:2} | ${buf[i].ascii_str()} | ${buf[i].hex()} | ${buf[i]:08b}') }
		return buf.vstring_with_len(buflen)
	}
}

// uuid_v7 generates a time-ordered (v7) UUID
// See https://datatracker.ietf.org/doc/html/rfc9562#name-uuid-version-7
pub fn uuid_v7() string {
	return internal_uuid_v7(mut default_rng)
}

@[direct_array_access]
fn internal_uuid_v7(mut rng PRNG) string {
	// 0                   1                   2                   3
	// 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
	// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
	// |                           unix_ts_ms                          |
	// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
	// |          unix_ts_ms           |  ver  |       rand_a          |
	// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
	// |var|                        rand_b                             |
	// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
	// |                            rand_b                             |
	// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

	// 1. Construct timestamp field (48 bits)
	now := time.now()
	timestamp := u64(now.unix_milli())

	// 2. Generate random field (74 bits)
	rand_a := rng.u16() // 16 bits (only use low 12 bits)
	rand_b := rng.u64() // 64 bits (only use low 62 bits)

	// 3. Construct fields with little-endian handling
	mut parts := [5]u16{}
	// 48-bit big-endian timestamp
	parts[0] = u16(timestamp >> 32)
	parts[1] = u16(timestamp >> 16)
	parts[2] = u16(timestamp)

	// 16 bits (version + rand_a)
	parts[3] = (rand_a & 0x0FFF) | 0x7000 // version(7)

	// 16 bits (variant bits + rand_b)
	parts[4] = u16(rand_b >> 48) & 0x3FFF // 16 bits
	parts[4] |= 0x8000 // Set variant bits 0b10

	// 4. Format output buffer (8-4-4-4-12)
	buflen := 36
	mut buf := unsafe { malloc_noscan(37) }

	// Timestamp section (first 12 chars)
	format_part(mut buf, parts[0], 0)
	format_part(mut buf, parts[1], 4)
	unsafe {
		buf[8] = `-`
	}
	format_part(mut buf, parts[2], 9)
	unsafe {
		buf[13] = `-`
	}
	// Random section with version bits
	format_part(mut buf, parts[3], 14)
	unsafe {
		buf[18] = `-`
	}
	// Random section with variant bits
	format_part(mut buf, parts[4], 19)
	unsafe {
		buf[23] = `-`
	}
	// Fill remaining random bytes (12 chars)
	mut offset := 28
	for i in 0 .. 6 {
		byte_val := u8((rand_b >> (40 - i * 8)) & 0xFF)
		unsafe {
			buf[offset + i * 2] = hex_chars[(byte_val >> 4) & 0x0F]
			buf[offset + i * 2 + 1] = hex_chars[byte_val & 0x0F]
		}
	}

	unsafe {
		buf[buflen] = 0 // Null terminator
		return buf.vstring_with_len(buflen)
	}
}

// Formats 16-bit value into buffer segment
@[direct_array_access; inline]
fn format_part(mut buf &u8, val u16, start int) {
	unsafe {
		buf[start] = hex_chars[(val >> 12) & 0xF]
		buf[start + 1] = hex_chars[(val >> 8) & 0xF]
		buf[start + 2] = hex_chars[(val >> 4) & 0xF]
		buf[start + 3] = hex_chars[val & 0xF]
	}
}

const ulid_encoding = '0123456789ABCDEFGHJKMNPQRSTVWXYZ'

@[direct_array_access]
fn internal_ulid_at_millisecond(mut rng PRNG, unix_time_milli u64) string {
	buflen := 26
	mut buf := unsafe { malloc_noscan(27) }
	mut t := unix_time_milli
	mut i := 9
	for i >= 0 {
		unsafe {
			buf[i] = ulid_encoding[t & 0x1F]
		}
		t = t >> 5
		i--
	}
	// first rand set
	mut x := rng.u64()
	i = 10
	for i < 19 {
		unsafe {
			buf[i] = ulid_encoding[x & 0x1F]
		}
		x = x >> 5
		i++
	}
	// second rand set
	x = rng.u64()
	for i < 26 {
		unsafe {
			buf[i] = ulid_encoding[x & 0x1F]
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
