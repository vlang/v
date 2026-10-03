module sha256

fn test_checksum_into_with_large_cumulative_length() {
	// Seed the cumulative counter to exercise large streams without hashing gigabytes.
	for base in [(u64(1) << 31) - u64(chunk), u64(1) << 31, u64(1) << 32, u64(1) << 40] {
		for remainder in 0 .. chunk {
			data := []u8{len: remainder, init: u8(index * 7 + 1)}
			for is224 in [false, true] {
				mut digest := if is224 { new224() } else { new() }
				digest.write(data)!
				digest.len = base + u64(remainder)
				message_bits := digest.len << 3

				// Build reference padding one byte at a time, independently of pad().
				mut reference := if is224 { new224() } else { new() }
				reference.write(data)!
				reference.write([u8(0x80)])!
				for reference.nx != chunk - 8 {
					reference.write([u8(0)])!
				}
				mut length_bytes := []u8{len: 8}
				for i in 0 .. 8 {
					length_bytes[i] = u8(message_bits >> (56 - 8 * i))
				}
				reference.write(length_bytes)!

				mut out := []u8{len: digest.size() + 3, init: 0xaa}
				digest.checksum_into(mut out)
				assert digest.h == reference.h, 'base=${base} remainder=${remainder} is224=${is224}'
				assert digest.nx == 0
				assert out[digest.size()..] == [u8(0xaa), 0xaa, 0xaa]
			}
		}
	}
}
