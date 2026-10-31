import yaml

struct Numbers {
	small    int
	port     u16
	big      u64
	smallest i64
	ratio    f64
	tenths   f32
	nested   []int
	by_name  map[string]int
}

// Integers must not be written as floats. `encode` used to route the JSON text
// through `json2.Any`, which decodes every number as an `f64`.
fn test_encode_keeps_integers_integral() ! {
	encoded := yaml.encode[Numbers](Numbers{5, 8080, 0, 0, 0.0, 0.0, [1, 2], {
		'a': 3
	}})
	doc := yaml.parse_text(encoded)!
	assert doc.value('small').type_name() != 'f64'
	assert doc.value('port').type_name() != 'f64'
	assert doc.value('nested[0]').type_name() != 'f64'
	assert doc.value('by_name.a').type_name() != 'f64'
	assert doc.value('small').int() == 5
	assert doc.value('port').int() == 8080
	assert !encoded.contains('5.0')
	assert !encoded.contains('8080.0')
}

// Values above 2^53 cannot survive a detour through `f64`.
fn test_encode_keeps_64_bit_integers_exact() ! {
	encoded := yaml.encode[Numbers](Numbers{0, 0, 18446744073709551615, -9223372036854775808, 0.0, 0.0, [], {}})
	assert encoded.contains('18446744073709551615')
	assert encoded.contains('-9223372036854775808')
	back := yaml.decode[Numbers](encoded)!
	assert back.big == u64(18446744073709551615)
	assert back.smallest == i64(-9223372036854775808)
}

// Floats are still written as floats.
fn test_encode_keeps_floats() ! {
	encoded := yaml.encode[Numbers](Numbers{0, 0, 0, 0, 1.5, 0.25, [], {}})
	doc := yaml.parse_text(encoded)!
	assert doc.value('ratio').type_name() == 'f64'
	assert doc.value('tenths').type_name() == 'f64'
	assert doc.value('ratio').f64() == 1.5
	assert doc.value('tenths').f64() == 0.25
	assert encoded.contains('1.5')
	assert encoded.contains('0.25')
}

// An empty collection used to be written on its own line, which the parser then
// rejected with "expected a mapping entry".
fn test_encode_keeps_empty_collections_readable() ! {
	encoded := yaml.encode[Numbers](Numbers{1, 2, 3, 4, 1.5, 2.5, [], {}})
	assert encoded.contains('[]')
	assert encoded.contains('{}')
	doc := yaml.parse_text(encoded)!
	assert doc.value('nested').array().len == 0
	assert doc.value('by_name').as_map().len == 0
}

fn test_to_yaml_of_empty_collections_is_reparseable() ! {
	// The plain `Any` path, which `encode` does not go through.
	doc := yaml.parse_text('a: []\nb: {}\nc: [[]]\n')!
	printed := doc.to_yaml()
	assert printed.contains('[]')
	assert printed.contains('{}')
	again := yaml.parse_text(printed)!
	assert again.value('a').array().len == 0
	assert again.value('b').as_map().len == 0
	assert again.value('c').array()[0].array().len == 0
}

fn test_encode_decode_round_trip_keeps_types() ! {
	original := Numbers{5, 8080, 18446744073709551615, -9223372036854775808, 1.5, 0.25, [
		1,
		2,
		3,
	], {
		'a': 4
	}}
	back := yaml.decode[Numbers](yaml.encode[Numbers](original))!
	assert back == original
}

fn test_encode_decode_round_trip_with_empty_collections() ! {
	original := Numbers{1, 2, 3, 4, 1.5, 2.5, [], {}}
	back := yaml.decode[Numbers](yaml.encode[Numbers](original))!
	assert back == original
}
