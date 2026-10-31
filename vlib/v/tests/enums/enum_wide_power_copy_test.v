enum WidePower as u64 {
	big = u64(2) ** u64(40)
}

enum CopiedWidePower as u64 {
	big = u64(WidePower.big)
}

@[comptime]
fn wide_enum_power(base u64, exponent u64) u64 {
	return base ** exponent
}

enum HelperWidePower as u64 {
	big = wide_enum_power(2, 40)
}

enum CopiedHelperWidePower as u64 {
	big = u64(HelperWidePower.big)
}

enum SignedPower as i64 {
	negative = i64(-2) ** i64(33)
}

enum CopiedSignedPower as i64 {
	negative = i64(SignedPower.negative)
}

fn test_copied_enum_power_keeps_all_64_bits() {
	assert u64(WidePower.big) == u64(1) << 40
	assert u64(CopiedWidePower.big) == u64(1) << 40
	assert u64(CopiedHelperWidePower.big) == u64(1) << 40
	assert i64(CopiedSignedPower.negative) == -8589934592
	mut values := []u64{}
	$for member in CopiedWidePower.values {
		values << u64(member.value)
		$if member.value != 1099511627776 {
			$compile_error('copied wide power lost bits in checker metadata')
		}
	}
	assert values == [u64(1) << 40]
}
