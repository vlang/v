import json

struct Wrap[T] {
	val T
}

fn encode[T](value T) string {
	return json.encode(Wrap[T]{
		val: value
	})
}

fn test_json_encode_generic_fixed_array_regression() {
	assert encode('x') == '{"val":"x"}'
	assert encode([1, 2]!) == '{"val":[1,2]}'
}

fn test_json_encode_generic_fixed_array_regression_reversed() {
	assert encode([1, 2]!) == '{"val":[1,2]}'
	assert encode('x') == '{"val":"x"}'
}
