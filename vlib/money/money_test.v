module money

import math.decimal

fn test_basic_sum() {
	a := from_string('19.99', usd)!
	b := from_string('1.50', usd)!
	total := a + b
	assert total.str_code() == 'USD 21.49'
}

fn test_minor_units() {
	m := from_minor_units(1999, usd)!
	assert m.minor_units()! == 1999
	assert m.str() == '$19.99'
}

fn test_allocate() {
	m := from_string('10.00', usd)!
	parts := m.allocate(3)!
	assert parts.len == 3
	mut sum := zero(usd)
	for p in parts {
		sum = sum + p
	}
	assert sum == m
}

fn test_mul() {
	m := from_string('10.00', usd)!
	taxed := m.mul(decimal.from_string('1.21')!)
	assert taxed.str_code() == 'USD 12.10'
}
