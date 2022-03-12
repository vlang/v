module rand

const clock_seq_hi_and_reserved_valid_values = [`8`, `9`, `a`, `b`]

// uuid_v4 generates a random (v4) UUID
// See https://en.wikipedia.org/wiki/Universally_unique_identifier#Version_4_(random)
pub fn uuid_v4() string {
	return internal_uuid_v4(mut default_rng)
}

fn internal_uuid_v4(mut rng PRNG) string {
	buflen := 36
	mut buf := unsafe { malloc_noscan(37) }
	mut i_buf := 0
	mut x := u64(0)
	mut d := byte(0)
	for i_buf < buflen {
		mut c := 0
		x = rng.u64()
		// do most of the bit manipulation at once:
		x &= 0x0F0F0F0F0F0F0F0F
		x += 0x3030303030303030
		// write the ASCII codes to the buffer:
		for c < 8 && i_buf < buflen {
			d = byte(x)
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
	d = byte(x)
	unsafe {
		// From https://www.ietf.org/rfc/rfc4122.txt :
		// >> Set the two most significant bits (bits 6 and 7) of the clock_seq_hi_and_reserved
		// >> to zero and one, respectively.
		// all nibbles starting with 10 are: 1000, 1001, 1010, 1011 -> hex digits `8`, `9`, `a`, `b`
		// these are stored in clock_seq_hi_and_reserved_valid_values, choose one of them at random:
		buf[19] = rand.clock_seq_hi_and_reserved_valid_values[d & 0x03]
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

const ulid_encoding = '0123456789ABCDEFGHJKMNPQRSTVWXYZ'

fn internal_ulid_at_millisecond(mut rng PRNG, unix_time_milli u64) string {
	buflen := 26
	mut buf := unsafe { malloc_noscan(27) }
	mut t := unix_time_milli
	mut i := 9
	for i >= 0 {
		unsafe {
			buf[i] = rand.ulid_encoding[t & 0x1F]
		}
		t = t >> 5
		i--
	}
	// first rand set
	mut x := rng.u64()
	i = 10
	for i < 19 {
		unsafe {
			buf[i] = rand.ulid_encoding[x & 0x1F]
		}
		x = x >> 5
		i++
	}
	// second rand set
	x = rng.u64()
	for i < 26 {
		unsafe {
			buf[i] = rand.ulid_encoding[x & 0x1F]
		}
		x = x >> 5
		i++
	}
	unsafe {
		buf[26] = 0
		return buf.vstring_with_len(buflen)
	}
}

fn internal_string_from_set(mut rng PRNG, charset string, len int) string {
	if len == 0 {
		return ''
	}
	mut buf := unsafe { malloc_noscan(len + 1) }
	for i in 0 .. len {
		unsafe {
			buf[i] = charset[intn(charset.len) or { 0 }]
		}
	}
	unsafe {
		buf[len] = 0
	}
	return unsafe { buf.vstring_with_len(len) }
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
	C.atexit(deinit)
}

fn read_32(mut rng PRNG, mut buf []byte) {
	p32 := unsafe { &u32(buf.data) }
	u32s := buf.len / 4
	for i in 0 .. u32s {
		unsafe {
			*(p32 + i) = rng.u32()
		}
	}
	for i in u32s * 4 .. buf.len {
		buf[i] = rng.byte()
	}
}

fn read_64(mut rng PRNG, mut buf []byte) {
	p64 := unsafe { &u64(buf.data) }
	u64s := buf.len / 8
	for i in 0 .. u64s {
		unsafe {
			*(p64 + i) = rng.u64()
		}
	}
	for i in u64s * 8 .. buf.len {
		buf[i] = rng.byte()
	}
}

fn read_internal(mut rng PRNG, mut buf []byte) {
	match rng.block_size() {
		32 {
			read_32(mut rng, mut buf)
		}
		64 {
			read_64(mut rng, mut buf)
		}
		else {
			for i in 0 .. buf.len {
				buf[i] = rng.byte()
			}
		}
	}
}
