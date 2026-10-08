type ByteOptions = []u8

type NestedByteOptions = ByteOptions

struct ByteDefinition {
	options ByteOptions
	nested  NestedByteOptions
}

fn spread_total(offset int, values ...u8) int {
	mut total := offset
	for value in values {
		total += int(value)
	}
	return total
}

fn alias_array_argument_count(values ...ByteOptions) int {
	return values.len
}

fn test_alias_array_field_spread_retains_all_elements() {
	definition := ByteDefinition{
		options: ByteOptions([u8(1), 2, 3])
		nested:  NestedByteOptions([u8(4), 5])
	}
	assert spread_total(10, ...definition.options) == 16
	assert spread_total(10, ...definition.nested) == 19
	options := definition.options
	assert spread_total(10, ...options) == 16
	callback := spread_total
	assert callback(10, ...definition.options) == 16
	assert spread_total(10, ...ByteDefinition{}.options) == 10
	assert spread_total(10, u8(1), 2, 3) == 16
	assert spread_total(10, ...[]u8(definition.options)) == 16
	assert alias_array_argument_count(definition.options) == 1
}
