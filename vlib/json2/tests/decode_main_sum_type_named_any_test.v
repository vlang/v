import json2

// `Any` here is a main sum type, distinct from `json2.Any`.
type Any = string | f32 | bool

fn test_main_sum_type_str_does_not_use_the_imported_homonym() {
	values := [Any('hi'), Any(true)]
	assert values.str() == "[Any('hi'), Any(true)]"
	assert '${Any(true)}' == 'Any(true)'
	assert json2.Any('x').str() == 'x'
}

fn test_decode_into_main_sum_type_named_any() {
	assert json2.decode[Any]('"hi"')! == Any('hi')
	assert json2.decode[Any]('true')! == Any(true)
	assert json2.decode[Any]('1.5')! == Any(f32(1.5))
}

fn test_decode_arrays_into_main_sum_type_named_any() {
	assert json2.decode[[]Any]('["hi",true,1.5]')! == [Any('hi'), Any(true), Any(f32(1.5))]
	assert json2.decode[[][]Any]('[["hi"],[true]]')! == [[Any('hi')], [Any(true)]]
	assert json2.decode[[]json2.Any]('["hi",true]')! == [json2.Any('hi'), json2.Any(true)]
}

fn test_decode_nested_fixed_arrays_into_main_sum_type_named_any() {
	decoded := json2.decode[[][2]Any]('[["hi",true],[1.5,"bye"]]')!
	assert decoded.len == 2
	first := decoded[0]
	second := decoded[1]
	assert first[0] == Any('hi')
	assert first[1] == Any(true)
	assert second[0] == Any(f32(1.5))
	assert second[1] == Any('bye')
}

fn test_decode_nested_maps_into_main_sum_type_named_any() {
	decoded := json2.decode[[]map[string]Any]('[{"a":"hi"},{"b":true}]')!
	a := decoded[0]['a'] or { panic('missing a') }
	b := decoded[1]['b'] or { panic('missing b') }
	assert a == Any('hi')
	assert b == Any(true)
	assert json2.decode[map[string][2]Any]('{"a":["hi",true]}')!['a'][1] == Any(true)
}
