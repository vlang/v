type Decimal = f64
type Counter = int

fn test_empty_init_of_numeric_aliases() {
	decimal_zero := Decimal{}
	counter_zero := Counter{}

	assert typeof(decimal_zero).name == 'Decimal'
	assert typeof(counter_zero).name == 'Counter'
	assert decimal_zero == Decimal(0.0)
	assert counter_zero == Counter(0)
}
