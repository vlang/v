module sha512

import crypto

fn test_checksum_into_with_large_cumulative_length() {
	// Seed the cumulative counter to exercise large streams without hashing gigabytes.
	for base in [(u64(1) << 31) - u64(chunk), u64(1) << 31, u64(1) << 32, u64(1) << 40] {
		for remainder in 0 .. chunk {
			data := []u8{len: remainder, init: u8(index * 7 + 1)}
			for variant in [crypto.Hash.sha512, .sha384, .sha512_224, .sha512_256] {
				mut digest := new_digest(variant)
				digest.write(data)!
				digest.len = base + u64(remainder)
				message_bits := digest.len << 3

				// Build reference padding one byte at a time, independently of pad().
				mut reference := new_digest(variant)
				reference.write(data)!
				reference.write([u8(0x80)])!
				for reference.nx != chunk - 16 {
					reference.write([u8(0)])!
				}
				mut length_bytes := []u8{len: 16}
				for i in 0 .. 8 {
					length_bytes[8 + i] = u8(message_bits >> (56 - 8 * i))
				}
				reference.write(length_bytes)!

				mut out := []u8{len: digest.size() + 3, init: 0xaa}
				digest.checksum_into(mut out)
				assert digest.h == reference.h, 'base=${base} remainder=${remainder} variant=${variant}'
				assert digest.nx == 0
				assert out[digest.size()..] == [u8(0xaa), 0xaa, 0xaa]
			}
		}
	}
}
