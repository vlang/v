module math

fn approx(a f64, b f64) bool {
	d := a - b
	if d < 0 {
		return -d < 1e-12 * (if b < 0 { -b } else { b }) || -d < 1e-12
	}
	return d < 1e-12 * b || d < 1e-12
}

fn test_lgamma_known_values() {
	assert approx(lgamma(1.0), 0.0)
	assert approx(lgamma(5.0), 3.1780538303479458) // ln(24)
	assert approx(lgamma(0.5), 0.5723649429247001) // ln(sqrt(pi))
	assert approx(lgamma(10.0), 12.801827480081469) // ln(362880)
	assert lgamma(0.0) == inf(1)
	assert lgamma(-2.0) == inf(1)
	assert lgamma(nan()) != lgamma(nan())
	assert lgamma(inf(1)) == inf(1)
}

fn test_remainder_ieee() {
	assert remainder(5.0, 3.0) == -1.0 // 5/3 rounds to 2 (ties not involved)
	assert remainder(4.0, 3.0) == 1.0
	assert remainder(2.5, 2.0) == 0.5 // 1.25 -> 1
	assert remainder(3.5, 2.0) == -0.5 // 1.75 -> 2
	assert remainder(-5.0, 3.0) == 1.0
	assert remainder(1.0, 0.0) != remainder(1.0, 0.0)
	assert remainder(inf(1), 2.0) != remainder(inf(1), 2.0)
	assert remainder(2.0, inf(1)) == 2.0
}

fn test_fma_and_floor_mod() {
	assert fma(2.0, 3.0, 4.0) == 10.0
	assert fma(0.0, 1.0, 5.0) == 5.0
	assert floor_mod(-1.0, 3.0) == 2.0 // Python semantics
	assert floor_mod(5.0, 3.0) == 2.0
	assert floor_mod(-5.0, 3.0) == 1.0
	assert floor_mod(5.0, -3.0) == -1.0
	assert floor_mod(6.0, 3.0) == 0.0
	assert floor_mod(1.0, 0.0) != floor_mod(1.0, 0.0)
}

fn test_angle_conversion() {
	assert approx(to_degrees(pi), 180.0)
	assert approx(to_radians(180.0), pi)
	assert approx(to_degrees(pi / 2), 90.0)
	assert to_degrees(0.0) == 0.0
}

fn test_powers_of_two() {
	assert next_power_of_two(1) == 1
	assert next_power_of_two(2) == 2
	assert next_power_of_two(3) == 4
	assert next_power_of_two(1000) == 1024
	assert next_power_of_two(1024) == 1024
	assert next_power_of_two(0) == 1
	assert next_power_of_two(u64(1) << 62) == u64(1) << 62
	assert next_power_of_two((u64(1) << 63) + 1) == 0, '2^64 overflows u64'
	assert is_power_of_two(1)
	assert is_power_of_two(1024)
	assert !is_power_of_two(0)
	assert !is_power_of_two(1000)
	assert is_power_of_two(u64(1) << 63), '2^63 is a power of two in u64'
	assert next_power_of_two(u64(1) << 63) == u64(1) << 63
}

fn test_classify() {
	assert classify(0.0) == .zero
	assert classify(-0.0) == .zero
	assert classify(1.0) == .normal
	assert classify(-1.5) == .normal
	assert classify(nan()) == .nan
	assert classify(inf(1)) == .infinite
	assert classify(inf(-1)) == .infinite
	assert classify(1e-320) == .subnormal // below 2.2250738585072014e-308
	assert classify(2.2250738585072014e-308) == .normal
}
