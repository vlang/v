module big

const decimal_power_digits = 144

struct DecimalPower {
	value Integer
	width int
}

// decimal_powers returns 10^144, 10^288, 10^576 and so on, each the square of the one before,
// ending at the first 10^width with 2 * width >= size.
fn decimal_powers(size int) []DecimalPower {
	// Reuse successive squares throughout one conversion, without shared mutable state.
	mut power := DecimalPower{
		value: integer_from_int(10).pow(decimal_power_digits)
		width: decimal_power_digits
	}
	mut powers := [power]
	for power.width * 2 < size {
		power = DecimalPower{
			value: power.value * power.value
			width: power.width * 2
		}
		powers << power
	}
	return powers
}
