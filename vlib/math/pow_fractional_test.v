module math

struct FractionalPowCase {
	x    f64
	y    f64
	want f64
}

fn test_pow_fractional_binary_powers() {
	// Avoid the +/-0.5 shortcuts and exercise both signs of the frexp exponent.
	cases := [
		FractionalPowCase{16.0, 1.25, 32.0},
		FractionalPowCase{16.0, -1.25, 0.03125},
		FractionalPowCase{16.0, 1.75, 128.0},
		FractionalPowCase{16.0, -1.75, 0.0078125},
		FractionalPowCase{0.0625, 1.25, 0.03125},
		FractionalPowCase{0.0625, -1.25, 32.0},
		FractionalPowCase{0.0625, 1.75, 0.0078125},
		FractionalPowCase{0.0625, -1.75, 128.0},
		FractionalPowCase{16.0, 4.25, 131072.0},
		FractionalPowCase{0.0625, -4.25, 131072.0},
	]
	for tc in cases {
		got := pow(tc.x, tc.y)
		assert abs(got - tc.want) <= 1e-14 * abs(tc.want),
			'pow(${tc.x}, ${tc.y}): got ${got}, want ${tc.want}; lower bound ${int(u32(u32(-1) << 12))}, integer limit ${f64(u64(1) << 63)}'
	}
}
