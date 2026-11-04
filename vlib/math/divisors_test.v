module math

fn test_divisors() {
	d := divisors(12)
	assert d.len == 6
}
