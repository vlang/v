module main

struct ClockVector {
	seconds  u64
	nanos    u64
	ratio    u64
	limit    u64
	expected u64
}

fn check_clock_vectors() {
	vectors := [
		ClockVector{ seconds: u64(0), nanos: u64(0), ratio: u64(4294967297), limit: u64(18446744073709551615), expected: u64(0) },
		ClockVector{ seconds: u64(0), nanos: u64(1), ratio: u64(4294967297), limit: u64(18446744073709551615), expected: u64(1) },
		ClockVector{ seconds: u64(0), nanos: u64(999999999), ratio: u64(4294967297), limit: u64(18446744073709551615), expected: u64(999999999) },
		ClockVector{ seconds: u64(1), nanos: u64(1), ratio: u64(4294967297), limit: u64(18446744073709551615), expected: u64(1000000001) },
		ClockVector{ seconds: u64(0), nanos: u64(999999999), ratio: u64(4294967297), limit: u64(18446744073709551615), expected: u64(999999999) },
		ClockVector{ seconds: u64(9223372036854775807), nanos: u64(999999999), ratio: u64(4294967297), limit: u64(18446744073709551615), expected: u64(18446744073709551615) },
		ClockVector{ seconds: u64(123456789), nanos: u64(987654321), ratio: u64(4294967297), limit: u64(9000000000), expected: u64(9000000000) },
		ClockVector{ seconds: u64(0), nanos: u64(1), ratio: u64(4294967297), limit: u64(0), expected: u64(0) },
		ClockVector{ seconds: u64(10), nanos: u64(5), ratio: u64(4294967297), limit: u64(12345), expected: u64(12345) },
		ClockVector{ seconds: u64(0), nanos: u64(0), ratio: u64(8589934595), limit: u64(18446744073709551615), expected: u64(0) },
		ClockVector{ seconds: u64(0), nanos: u64(1), ratio: u64(8589934595), limit: u64(18446744073709551615), expected: u64(1) },
		ClockVector{ seconds: u64(0), nanos: u64(999999999), ratio: u64(8589934595), limit: u64(18446744073709551615), expected: u64(666666666) },
		ClockVector{ seconds: u64(1), nanos: u64(1), ratio: u64(8589934595), limit: u64(18446744073709551615), expected: u64(666666668) },
		ClockVector{ seconds: u64(2), nanos: u64(999999999), ratio: u64(8589934595), limit: u64(18446744073709551615), expected: u64(2000000000) },
		ClockVector{ seconds: u64(9223372036854775807), nanos: u64(999999999), ratio: u64(8589934595), limit: u64(18446744073709551615), expected: u64(18446744073709551615) },
		ClockVector{ seconds: u64(123456789), nanos: u64(987654321), ratio: u64(8589934595), limit: u64(9000000000), expected: u64(9000000000) },
		ClockVector{ seconds: u64(0), nanos: u64(1), ratio: u64(8589934595), limit: u64(0), expected: u64(0) },
		ClockVector{ seconds: u64(10), nanos: u64(5), ratio: u64(8589934595), limit: u64(12345), expected: u64(12345) },
		ClockVector{ seconds: u64(0), nanos: u64(0), ratio: u64(12884902013), limit: u64(18446744073709551615), expected: u64(0) },
		ClockVector{ seconds: u64(0), nanos: u64(1), ratio: u64(12884902013), limit: u64(18446744073709551615), expected: u64(1) },
		ClockVector{ seconds: u64(0), nanos: u64(999999999), ratio: u64(12884902013), limit: u64(18446744073709551615), expected: u64(24000000) },
		ClockVector{ seconds: u64(1), nanos: u64(1), ratio: u64(12884902013), limit: u64(18446744073709551615), expected: u64(24000001) },
		ClockVector{ seconds: u64(124), nanos: u64(999999999), ratio: u64(12884902013), limit: u64(18446744073709551615), expected: u64(3000000000) },
		ClockVector{ seconds: u64(9223372036854775807), nanos: u64(999999999), ratio: u64(12884902013), limit: u64(18446744073709551615), expected: u64(18446744073709551615) },
		ClockVector{ seconds: u64(123456789), nanos: u64(987654321), ratio: u64(12884902013), limit: u64(9000000000), expected: u64(9000000000) },
		ClockVector{ seconds: u64(0), nanos: u64(1), ratio: u64(12884902013), limit: u64(0), expected: u64(0) },
		ClockVector{ seconds: u64(10), nanos: u64(5), ratio: u64(12884902013), limit: u64(12345), expected: u64(12345) },
		ClockVector{ seconds: u64(0), nanos: u64(0), ratio: u64(18446744069414584319), limit: u64(18446744073709551615), expected: u64(0) },
		ClockVector{ seconds: u64(0), nanos: u64(1), ratio: u64(18446744069414584319), limit: u64(18446744073709551615), expected: u64(1) },
		ClockVector{ seconds: u64(0), nanos: u64(999999999), ratio: u64(18446744069414584319), limit: u64(18446744073709551615), expected: u64(999999999) },
		ClockVector{ seconds: u64(1), nanos: u64(1), ratio: u64(18446744069414584319), limit: u64(18446744073709551615), expected: u64(1000000001) },
		ClockVector{ seconds: u64(4294967294), nanos: u64(999999999), ratio: u64(18446744069414584319), limit: u64(18446744073709551615), expected: u64(4294967294000000000) },
		ClockVector{ seconds: u64(9223372036854775807), nanos: u64(999999999), ratio: u64(18446744069414584319), limit: u64(18446744073709551615), expected: u64(18446744073709551615) },
		ClockVector{ seconds: u64(123456789), nanos: u64(987654321), ratio: u64(18446744069414584319), limit: u64(9000000000), expected: u64(9000000000) },
		ClockVector{ seconds: u64(0), nanos: u64(1), ratio: u64(18446744069414584319), limit: u64(0), expected: u64(0) },
		ClockVector{ seconds: u64(10), nanos: u64(5), ratio: u64(18446744069414584319), limit: u64(12345), expected: u64(12345) },
		ClockVector{ seconds: u64(0), nanos: u64(0), ratio: u64(18446744069414584321), limit: u64(18446744073709551615), expected: u64(0) },
		ClockVector{ seconds: u64(0), nanos: u64(1), ratio: u64(18446744069414584321), limit: u64(18446744073709551615), expected: u64(4294967295) },
		ClockVector{ seconds: u64(0), nanos: u64(999999999), ratio: u64(18446744069414584321), limit: u64(18446744073709551615), expected: u64(4294967290705032705) },
		ClockVector{ seconds: u64(1), nanos: u64(1), ratio: u64(18446744069414584321), limit: u64(18446744073709551615), expected: u64(4294967299294967295) },
		ClockVector{ seconds: u64(0), nanos: u64(999999999), ratio: u64(18446744069414584321), limit: u64(18446744073709551615), expected: u64(4294967290705032705) },
		ClockVector{ seconds: u64(9223372036854775807), nanos: u64(999999999), ratio: u64(18446744069414584321), limit: u64(18446744073709551615), expected: u64(18446744073709551615) },
		ClockVector{ seconds: u64(123456789), nanos: u64(987654321), ratio: u64(18446744069414584321), limit: u64(9000000000), expected: u64(9000000000) },
		ClockVector{ seconds: u64(0), nanos: u64(1), ratio: u64(18446744069414584321), limit: u64(0), expected: u64(0) },
		ClockVector{ seconds: u64(10), nanos: u64(5), ratio: u64(18446744069414584321), limit: u64(12345), expected: u64(12345) },
		ClockVector{ seconds: u64(0), nanos: u64(0), ratio: u64(8589934591), limit: u64(18446744073709551615), expected: u64(0) },
		ClockVector{ seconds: u64(0), nanos: u64(1), ratio: u64(8589934591), limit: u64(18446744073709551615), expected: u64(1) },
		ClockVector{ seconds: u64(0), nanos: u64(999999999), ratio: u64(8589934591), limit: u64(18446744073709551615), expected: u64(1) },
		ClockVector{ seconds: u64(1), nanos: u64(1), ratio: u64(8589934591), limit: u64(18446744073709551615), expected: u64(1) },
		ClockVector{ seconds: u64(4294967294), nanos: u64(999999999), ratio: u64(8589934591), limit: u64(18446744073709551615), expected: u64(1000000000) },
		ClockVector{ seconds: u64(9223372036854775807), nanos: u64(999999999), ratio: u64(8589934591), limit: u64(18446744073709551615), expected: u64(2147483648500000001) },
		ClockVector{ seconds: u64(123456789), nanos: u64(987654321), ratio: u64(8589934591), limit: u64(9000000000), expected: u64(28744524) },
		ClockVector{ seconds: u64(0), nanos: u64(1), ratio: u64(8589934591), limit: u64(0), expected: u64(0) },
		ClockVector{ seconds: u64(10), nanos: u64(5), ratio: u64(8589934591), limit: u64(12345), expected: u64(3) },
	]
	for value in vectors {
		assert mac_clock_duration_ticks(value.seconds, value.nanos, value.ratio, value.limit) == value.expected, value.str()
	}
	for ticks in [u64(0), 1, 2, 3, 100, 1000000] {
		nanos, code := mac_clock_scale(ticks, u64(2) << 32 | u64(3))
		assert code == 0 && nanos == ticks * 3 / 2
	}
	_, overflow := mac_clock_scale(~u64(0), u64(1) << 32 | u64(2))
	assert overflow == 84
}
