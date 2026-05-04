import x.json2

struct ScientificI64Value {
	size i64
}

fn test_decode_i64_from_scientific_notation_number() {
	decoded := json2.decode[ScientificI64Value]('{"size":1.7596215426069998e+12}')!
	assert decoded.size == i64(1.7596215426069998e+12)
}

fn test_decode_i64_from_scientific_notation_string() {
	decoded := json2.decode[ScientificI64Value]('{"size":"1.7596215426069998e+12"}')!
	assert decoded.size == i64(1.7596215426069998e+12)
}

fn test_decode_i64_from_plain_fraction_still_fails() {
	if _ := json2.decode[ScientificI64Value]('{"size":123.45}') {
		assert false
	} else {
		assert true
	}
}
