module picohttpparser

// A wider sweep for `u64toa` than misc_test.v: every decimal-boundary value and
// a deterministic pseudo-random sample, each compared against `u64.str()`.

const u64toa_boundaries = [
	u64(0),
	1,
	2,
	9,
	10,
	11,
	99,
	100,
	101,
	989,
	990,
	999,
	1000,
	1001,
	9989,
	9990,
	9999,
	10000,
	10001,
	98999,
	99999,
	100000,
	100001,
	998999,
	999999,
	1000000,
	1000001,
	9989999,
	9999999,
	10000000,
	10000001,
	98999999,
	99999998,
	99999999,
]

fn u64toa_to_string(value u64) !string {
	mut buf := [20]u8{}
	len := unsafe { u64toa(&buf[0], value)! }
	return unsafe { tos(&buf[0], len) }
}

pub fn test_u64toa_matches_str_on_decimal_boundaries() {
	for v in u64toa_boundaries {
		got := u64toa_to_string(v) or {
			assert false, 'u64toa(${v}) failed: ${err}'
			''
		}
		assert got == v.str(), 'u64toa(${v}) = "${got}", want "${v.str()}"'
	}
}

pub fn test_u64toa_matches_str_on_a_deterministic_sample() {
	mut state := u64(12345)
	for _ in 0 .. 4096 {
		state = state * 6364136223846793005 + 1442695040888963407
		v := (state >> 33) % 100_000_000
		got := u64toa_to_string(v) or {
			assert false, 'u64toa(${v}) failed: ${err}'
			''
		}
		assert got == v.str(), 'u64toa(${v}) = "${got}", want "${v.str()}"'
	}
}

pub fn test_u64toa_rejects_values_from_100mb() {
	for v in [u64(100_000_000), 100_000_001, 4_294_967_295, 18_446_744_073_709_551_615] {
		mut buf := [20]u8{}
		mut msg := ''
		unsafe {
			u64toa(&buf[0], v) or { msg = err.msg() }
		}
		assert msg == 'Maximum size of 100MB exceeded!', 'u64toa(${v}) gave "${msg}"'
	}
}

pub fn test_u64toa_writes_nothing_past_the_returned_length() {
	mut buf := [20]u8{}
	for i in 0 .. 20 {
		buf[i] = 0xAA
	}
	len := unsafe { u64toa(&buf[0], 4242) or { 0 } }
	assert len == 4, 'len ${len}'
	for i in len .. 20 {
		assert buf[i] == 0xAA, 'byte ${i} was modified: ${buf[i]}'
	}
}
