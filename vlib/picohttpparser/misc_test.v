module picohttpparser

// Test various values under 10,000
pub fn test_u64toa_small_values() {
	for v in [u64(0), 1, 10, 99, 100, 999, 1000, 9999] {
		mut buf := [10]u8{}
		len := unsafe { u64toa(&buf[0], v) or { 0 } }

		assert len == expected_len(v)

		// Check the actual string for accuracy
		assert buf[0..len] == v.str().bytes()
	}
}

// Test various values above 10,000 and error handling
pub fn test_u64toa_large_values() {
	for i, v in [u64(10000), 12345, 99999, 100000, 999999, 12345678, 99_999_999, 100_000_000] {
		mut buf := [20]u8{}

		len := unsafe {
			u64toa(&buf[0], v) or {
				assert err.msg() == 'Maximum size of 100MB exceeded!'
				0
			}
		}

		if v < 100_000_000 {
			assert len == expected_len(v)

			assert buf[0..len] == v.str().bytes()
		} else {
			assert len == 0
		}
	}
}

// Test edge cases
pub fn test_u64toa_edge_cases() {
	mut buf := [10]u8{}

	// Test zero value
	len := unsafe {
		u64toa(&buf[0], 0) or {
			assert false
			0
		}
	}

	assert len == 1
	assert buf[0] == `0`
}

// Helper functions for expected values
fn expected_len(v u64) int {
	if v == 0 {
		return 1
	}

	// return int(math.ceil(math.log10(f64(v + 1))))

	mut count := 0
	mut temp := v

	for temp > 0 {
		temp /= 10
		count++
	}

	return count
}
