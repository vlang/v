module sha512

import crypto
import encoding.binary

fn test_checksum_into_with_large_cumulative_length() {
	input := []u8{len: chunk, init: u8(index * 7 + 1)}
	// Seed the cumulative counter to exercise large streams without hashing gigabytes.
	for base in [(u64(1) << 31) - u64(chunk), u64(1) << 31, u64(1) << 32, u64(1) << 40] {
		for remainder in 0 .. chunk {
			data := input[..remainder]
			message_len := base + u64(remainder)

			// Build the reference message without using the digest's padding code or state.
			padded_len := if remainder + 1 + 16 <= chunk { chunk } else { 2 * chunk }
			mut padded := []u8{len: padded_len}
			copy(mut padded, data)
			padded[remainder] = 0x80
			binary.big_endian_put_u64_end(mut padded, message_len << 3)

			for variant in [crypto.Hash.sha512, .sha384, .sha512_224, .sha512_256] {
				mut digest := new_digest(variant)
				digest.write(data)!
				digest.len = message_len

				mut reference := new_digest(variant)
				reference.write(padded)!

				mut out := []u8{len: digest.size() + 3, init: 0xaa}
				digest.checksum_into(mut out)
				assert digest.h == reference.h, 'base=${base} remainder=${remainder} variant=${variant}'
				assert digest.nx == 0
				assert out[digest.size()..] == [u8(0xaa), 0xaa, 0xaa]
			}
		}
	}
}
