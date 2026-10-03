module json2

// checked_decoder returns a decoder for `json` after its check pass, which fills
// values_info. `room` is the number of values that values_info is allocated for.
fn checked_decoder(json string, room int) !Decoder {
	mut decoder := Decoder{
		json:        json
		values_info: []ValueInfo{len: room}
	}
	decoder.check_json_format()!
	decoder.values_info.trim(decoder.values_len)
	return decoder
}

// value_texts returns the JSON text of every value in values_info, in order.
fn value_texts(decoder Decoder) []string {
	mut texts := []string{}
	for value_info in decoder.values_info {
		texts << decoder.json[value_info.position..value_info.position + value_info.length]
	}
	return texts
}

fn test_values_are_stored_in_one_array_in_document_order() {
	json := '{"a": [1, {"b": null}], "c": "d", "e": true}'
	decoder := checked_decoder(json, value_count_bound(json))!
	assert decoder.values_info.len == decoder.values_len
	assert value_texts(decoder) == [json, '"a"', '[1, {"b": null}]', '1', '{"b": null}', '"b"',
		'null', '"c"', '"d"', '"e"', 'true']
	assert decoder.values_info.map(it.value_kind) == [ValueKind.object, .string, .array, .number,
		.object, .string, .null, .string, .string, .string, .boolean]
}

fn test_value_count_bound_is_exact_without_empty_containers() {
	for json in ['1', '"text"', 'null', '[1]', '[1, 2, 3]', '{"a": 1}', '{"a": [1, 2], "b": {"c": "d"}}',
		'[[[1]]]', ' [ 1 , 2 ] '] {
		decoder := checked_decoder(json, value_count_bound(json))!
		assert decoder.values_len == value_count_bound(json), json
	}
}

fn test_value_count_bound_counts_separators_in_strings_of_short_json() {
	for json in ['"a,b:c[d{e"', '{"k,:[{": ",:[{"}', r'["\"", "\\", "\u005b"]',
		r'{"a\",\":": "b\\", "c": ",\\\",:"}', r'["\\\\", 1, "{\"a\":[1,2]}"]'] {
		assert json.len < skip_strings_min_json_len
		decoder := checked_decoder(json, value_count_bound(json))!
		assert decoder.values_len <= value_count_bound(json), json
	}
}

fn test_value_count_bound_skips_strings_of_long_json() {
	items := ['"a,b:c[d{e"', '{"k,:[{": ",:[{"}', r'["\"", "\\", "\u005b"]',
		r'{"a\",\":": "b\\", "c": ",\\\",:"}', r'["\\\\", 1, "{\"a\":[1,2]}"]']
	mut elements := []string{}
	mut len := 0
	for len < skip_strings_min_json_len {
		elements << items
		len += items.join(',').len
	}
	json := '[' + elements.join(',') + ']'
	assert json.len >= skip_strings_min_json_len
	decoder := checked_decoder(json, value_count_bound(json))!
	assert decoder.values_len == value_count_bound(json)
	// the root array, and the 1, 3, 4, 5 and 4 values of every repetition of `items`
	assert decoder.values_len == 1 + (elements.len / items.len) * 17
}

// naive_json_string_end is json_string_end, one byte at a time.
fn naive_json_string_end(json string, start int) int {
	mut i := start
	for i < json.len && json[i] != `"` {
		if json[i] == `\\` {
			i++
		}
		i++
	}
	return if i < json.len { i } else { json.len }
}

fn test_json_string_end() {
	assert json_string_end('"abc"', 1) == 4
	assert json_string_end('""', 1) == 1
	assert json_string_end(r'"a\"b"', 1) == 5
	assert json_string_end(r'"a\\"', 1) == 4
	assert json_string_end('"abc', 1) == 4
	assert json_string_end(r'"abc\', 1) == 5
	assert json_string_end('"0123456789abcdef0123456789"x', 1) == 27
	// A `"`, an escape sequence and the end of the JSON, at every position of the 8
	// bytes that are skipped at a time.
	for len in 0 .. 40 {
		plain := 'x'.repeat(len)
		closed := '"' + plain + '", 1'
		assert json_string_end(closed, 1) == len + 1, closed
		assert json_string_end(closed[..len + 1], 1) == len + 1, 'not closed, len: ${len}'
		for pos in 0 .. len {
			for special in [r'\"', r'\\', r'\n', r'\u0022', r'\\\"', '"'] {
				json := '"' + plain[..pos] + special + plain[pos..] + '", "next"'
				assert json_string_end(json, 1) == naive_json_string_end(json, 1), json
				truncated := json[..1 + pos + special.len]
				assert json_string_end(truncated, 1) == naive_json_string_end(truncated, 1), truncated
			}
		}
	}
}

fn test_word_has_byte() {
	for pos in 0 .. 8 {
		mut bytes := [u8(`a`), `b`, `c`, `d`, `e`, `f`, `g`, `h`]
		mut word := u64(0)
		unsafe { vmemcpy(&word, bytes.data, 8) }
		assert !word_has_byte(word, `"`)
		assert word_has_byte(word, `a` + u8(pos))
		bytes[pos] = `"`
		unsafe { vmemcpy(&word, bytes.data, 8) }
		assert word_has_byte(word, `"`), 'pos: ${pos}'
		assert !word_has_byte(word, `\\`), 'pos: ${pos}'
		// `#` is the byte after `"`, and `!` the one before it.
		assert !word_has_byte(word, `#`), 'pos: ${pos}'
		assert !word_has_byte(word, `!`), 'pos: ${pos}'
	}
	assert word_has_byte(0, 0)
	assert !word_has_byte(0x0101010101010101, 0)
	assert !word_has_byte(0x8080808080808080, 0)
	assert word_has_byte(0xffffffffffffffff, 0xff)
	assert !word_has_byte(0xffffffffffffffff, 0x7f)
}

fn test_value_count_bound_counts_a_value_too_many_for_an_empty_container() {
	// An empty array or object is the only JSON for which the bound is higher than the
	// number of values.
	for json in ['[]', '{}', '[[], {}]', '{"a": {}}'] {
		decoder := checked_decoder(json, value_count_bound(json))!
		assert decoder.values_len < value_count_bound(json), json
		assert decoder.values_len > 0, json
	}
}

fn test_invalid_json_is_an_error() {
	// The checker puts a value in values_info before it knows that the JSON is invalid.
	// It can then see more values than value_count_bound() made room for, as in `["a"`.
	for json in ['["a"', '[', '[[[[', '{"a"', '{"a":', '[1,', '[1 2', '{"a" 1}', '1 2', '[1,,2]',
		'{,}', '[}', '{]', '"open', 'tru', '-', '["open, [1, 2]', r'["a\", 1, 2]', r'["\q", 1, 2]',
		r'["\u12", 1, 2]', '[1 "a", 2, 3]', r'[1, \"a", 2, 3]', '["a"x, 1, 2]'] {
		if _ := decode[Any](json) {
			assert false, 'invalid JSON was decoded: `${json}`'
		}
	}
}

fn test_checker_grows_values_info_when_it_has_no_room() {
	json := '{"a": [1, {"b": null}], "c": "d", "e": true}'
	expected := checked_decoder(json, value_count_bound(json))!
	for room in [0, 1, 2, 5] {
		decoder := checked_decoder(json, room)!
		assert decoder.values_len == expected.values_len, 'room: ${room}'
		assert value_texts(decoder) == value_texts(expected), 'room: ${room}'
	}
}

fn test_skip_current_value_skips_the_values_nested_in_it() {
	json := '[{"a": [1, 2, {"b": 3}]}, "next", 5]'
	mut decoder := checked_decoder(json, value_count_bound(json))!
	// the first element of the root array
	decoder.current_idx = 1
	assert decoder.current_value().value_kind == .object
	decoder.skip_current_value()
	assert decoder.json[decoder.current_value().position..decoder.current_value().position +
		decoder.current_value().length] == '"next"'
	decoder.skip_current_value()
	assert decoder.current_value().value_kind == .number
	decoder.skip_current_value()
	// After the last value there is nothing left, and skipping stays there.
	assert decoder.current_idx == decoder.values_info.len
	assert !decoder.has_value(decoder.current_idx)
	decoder.skip_current_value()
	assert decoder.current_idx == decoder.values_info.len
	// Skipping the root skips everything.
	decoder.current_idx = 0
	decoder.skip_current_value()
	assert decoder.current_idx == decoder.values_info.len
}

fn test_has_value() {
	decoder := checked_decoder('[1, 2]', 3)!
	assert decoder.has_value(0)
	assert decoder.has_value(2)
	assert !decoder.has_value(3)
	assert !decoder.has_value(no_value_idx)
}

fn test_get_sumtype_type_field_idx() {
	json := '[{"a": {"_type": "Inner"}, "_type": "Outer"}, {"a": 1}, 5]'
	mut decoder := checked_decoder(json, value_count_bound(json))!
	// The object's own `_type` is found, not the one of the object nested in it.
	type_field_idx := decoder.get_sumtype_type_field_idx(1)
	assert decoder.has_value(type_field_idx)
	assert decoder.sumtype_type_field_matches(type_field_idx, 'Outer')
	assert !decoder.sumtype_type_field_matches(type_field_idx, 'Inner')
	inner_idx := decoder.get_sumtype_type_field_idx(3)
	assert decoder.sumtype_type_field_matches(inner_idx, 'Inner')
	// An object without a `_type`, a value that is not an object, and no value at all.
	assert decoder.values_info[8].value_kind == .object
	assert decoder.get_sumtype_type_field_idx(8) == no_value_idx
	assert decoder.get_sumtype_type_field_idx(decoder.values_info.len - 1) == no_value_idx
	assert decoder.get_sumtype_type_field_idx(decoder.values_info.len) == no_value_idx
	assert decoder.get_sumtype_type_field_idx(no_value_idx) == no_value_idx
	assert !decoder.sumtype_type_field_matches(no_value_idx, 'Outer')
}

struct BigItem {
	id   int
	name string
	tags []string
}

fn test_decode_big_array_with_skipped_values() {
	n := 20000
	mut parts := []string{cap: n}
	for i in 0 .. n {
		parts << '{"skipped": {"deep": [1, [2, {"x": null}]]}, "id": ${i}, "name": "item ${i}", "tags": ["a", "b"]}'
	}
	json := '[' + parts.join(',') + ']'
	items := decode[[]BigItem](json)!
	assert items.len == n
	for i in [0, 1, n / 2, n - 1] {
		assert items[i].id == i
		assert items[i].name == 'item ${i}'
		assert items[i].tags == ['a', 'b']
	}
}
