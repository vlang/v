module json2

fn test_decode_scratch_storage_spills_and_releases_on_errors() {
	for count in [0, 1, 7, 8, 9, 31, 32, 33, 1024] {
		values := []string{len: count, init: '"entry"'}
		source := '[' + values.join(',') + ']'
		decoded := decode[[]string](source)!
		assert decoded == []string{len: count, init: 'entry'}
		invalid := source[..source.len - 1] + ',]'
		if _ := decode[[]string](invalid) {
			assert false, invalid
		}
		assert decode[string]('"after error"')! == 'after error'
	}
}

fn test_flat_metadata_survives_growth_and_tracks_nested_spans() {
	document := '{"name":"entry","items":[true,false,null]}'
	documents := []string{len: 2048, init: document}
	source := '[' + documents.join(',') + ']'
	mut decoder := Decoder{ json: source }
	decoder.check_json_format()!
	assert decoder.values_info.len == 1 + 8 * documents.len
	assert decoder.values_info[0].length == source.len
	assert decoder.values_info[1].length == document.len
	assert decoder.next_value(1) == 9
	assert decoder.next_value(5) == 9
	assert decoder.next_value(0) == decoder.values_info.len
	expected := [
		ValueKind.array,
		.object,
		.string,
		.string,
		.string,
		.array,
		.boolean,
		.boolean,
		.null,
	]
	for index, kind in expected {
		assert decoder.values_info[index].value_kind == kind
	}
	for value in decoder.values_info {
		assert value.length > 0
		assert value.position + value.length <= source.len
	}
	decoded := decode[Any](source)!
	assert encode(decoded) == source
}

fn test_flat_metadata_discriminator_skips_nested_values() {
	source := '{"nested":{"_type":"Wrong"},"_type":"Right"}'
	mut decoder := Decoder{ json: source }
	decoder.check_json_format()!
	index := decoder.get_sumtype_type_field_idx(0)
	assert index < decoder.values_info.len
	assert decoder.decode_string_value(decoder.values_info[index])! == 'Right'
	decoder.skip_current_value()
	assert decoder.current_idx == decoder.values_info.len
}

fn test_decoded_values_own_borrowed_input_fragments() {
	mut source := '{"key":42,"escaped\\tkey":"a\\tb","plain":"text"}'.clone()
	decoded := decode[map[string]Any](source)!
	unsafe { source.free() }
	assert (decoded['key'] or { panic('missing key') }) == Any(f64(42))
	assert (decoded['escaped\tkey'] or { panic('missing key') }) == Any('a\tb')
	assert (decoded['plain'] or { panic('missing key') }) == Any('text')
}

fn test_encoded_output_survives_builder_release_and_reuse() {
	item := 'text\n世界'.repeat(1024)
	values := [item, item, item]
	first := encode(values)
	second := encode(['other'.repeat(8192)])
	assert decode[[]string](first)! == values
	assert decode[[]string](second)! == ['other'.repeat(8192)]
}

fn test_integer_encoding_preserves_signed_and_unsigned_limits() {
	signed := [i64(-9223372036854775807) - 1, -1, 0, 9223372036854775807]
	unsigned := [u64(0), 9223372036854775808, 18446744073709551615]
	expected_signed := '[-9223372036854775808,-1,0,9223372036854775807]'
	expected_unsigned := '[0,9223372036854775808,18446744073709551615]'
	assert encode(signed) == expected_signed
	assert encode(unsigned) == expected_unsigned
	assert decode[[]i64](expected_signed)! == signed
	assert decode[[]u64](expected_unsigned)! == unsigned
}

fn test_decoded_array_supports_slices_after_growth() {
	source := '[' + []string{len: 4096, init: '42'}.join(',') + ']'
	mut decoded := decode[[]Any](source)!
	prefix := unsafe { decoded[..2] }
	assert prefix.data == decoded.data
	for _ in 0 .. 8192 {
		decoded << Any('later')
	}
	assert prefix == [Any(f64(42)), Any(f64(42))]
	assert decoded.len == 12288
	assert decoded.last() == Any('later')
}

struct ArrayDefault {
mut:
	value int            = 42
	name  string         = 'default'
	items []int          = [1]
	table map[string]int = {
		'count': 1
	}
}

fn test_array_storage_preserves_element_defaults() {
	mut values := decode[[]ArrayDefault]('[{}, {"value":7}]')!
	assert values == [ArrayDefault{}, ArrayDefault{ value: 7 }]
	values[0].items << 2
	values[0].table['count'] = 2
	assert values[1].items == [1]
	assert values[1].table['count'] == 1
	assert decode[[][]int]('[[],[1,2],[],[3]]')! == [[], [1, 2], [], [3]]
	assert decode[[]?int]('[null,7,null]')! == [?int(none), ?int(7), ?int(none)]
	assert decode[[]Any]('[]')!.len == 0
}

fn test_container_counts_and_skips_match_nested_boundaries() {
	source := '[{},[],{"a":[1,{"b":[]}]},true,null,"[}"]'
	mut decoder := Decoder{ json: source }
	decoder.check_json_format()!
	expected := [1, 2, 3, 10, 11, 12]
	mut next := 1
	for index in expected {
		assert next == index
		next = decoder.next_value(next)
	}
	assert next == decoder.values_info.len
	assert decoder.container_len(0) == 6
	assert decoder.container_len(1) == 0
	assert decoder.container_len(2) == 0
	assert decoder.container_len(3) == 1
	assert decoder.container_len(5) == 2
	assert decoder.next_value(0) == decoder.values_info.len
}

fn test_reserved_maps_keep_duplicates_and_allow_later_mutation() {
	mut values := decode[map[string]Any]('{"a":1,"a":2,"b":[3]}')!
	assert values.len == 2
	assert (values['a'] or { panic('missing key') }) == Any(f64(2))
	for key in ['c', 'd', 'e', 'f', 'g', 'h', 'i'] {
		values[key] = key
	}
	values.delete('a')
	values['j'] = 'j'
	assert values.len == 9
	assert (values['j'] or { panic('missing key') }) == Any('j')
	assert (values['b'] or { panic('missing key') }) == Any([Any(f64(3))])
}

type StorageIntegerKey = int
type StorageStringKey = string

fn test_decoded_map_keys_follow_the_declared_type() {
	integers := decode[map[int]string]('{"42":"answer","-7":"negative"}')!
	assert integers[42] == 'answer'
	assert integers[-7] == 'negative'
	unsigned := decode[map[u64]int]('{"18446744073709551615":7}')!
	assert unsigned[u64(18446744073709551615)] == 7
	aliases := decode[map[StorageIntegerKey]int]('{"3":5}')!
	assert aliases[StorageIntegerKey(3)] == 5
	strings := decode[map[StorageStringKey]int]('{"name":9}')!
	assert strings[StorageStringKey('name')] == 9
	runes := decode[map[rune]int]('{"a":11,"世":13,"🙂":17,"1":19}')!
	assert runes[`a`] == 11
	assert runes[`世`] == 13
	assert runes[`🙂`] == 17
	assert runes[`1`] == 19
	assert decode[map[rune]int](encode(runes))! == runes
	values := decode[map[int]?int]('{"2":null,"3":4}')!
	assert values[2] == none
	assert values[3] == ?int(4)
}

fn test_decoded_map_keys_reject_out_of_range_and_multiple_runes() {
	if _ := decode[map[u8]int]('{"256":1}') {
		assert false
	}
	if _ := decode[map[rune]int]('{"ab":1}') {
		assert false
	}
}
