module json2

type SortedStringMap = map[string]int
type SortedIntegerKey = int
type SortedStringKey = string

enum SortedEnumKey {
	zebra
	apple
}

struct SortedMapHolder {
	values map[string]int
}

fn test_encode_map_sorts_string_keys() {
	value := {
		'b': 1
		'a': 2
	}
	expected := '{"a":2,"b":1}'
	assert encode(value) == expected
	assert encode({
		'a': 2
		'b': 1
	}) == expected
	assert encode(SortedStringMap(value)) == expected
	assert encode(SortedMapHolder{value}) == '{"values":${expected}}'
	assert encode([value]) == '[${expected}]'
	assert encode(&value) == expected
	mut output := 'prefix:'.bytes()
	encode_append(value, mut output)
	assert output.bytestr() == 'prefix:' + expected
	mut encoder := Encoder{}
	encoder.encode_map(value)
	assert encoder.output.bytestr() == expected
	assert encode(map[string]int{}) == '{}'
}

fn test_encode_map_sorts_converted_keys_lexically() {
	assert encode({
		2:  'two'
		10: 'ten'
		-1: 'minus'
	}) == '{"-1":"minus","10":"ten","2":"two"}'
	assert encode({
		u64(2):  2
		u64(10): 10
	}) == '{"10":10,"2":2}'
	assert encode({
		SortedIntegerKey(2):  2
		SortedIntegerKey(10): 10
	}) == '{"10":10,"2":2}'
	assert encode({
		SortedStringKey('z'): 1
		SortedStringKey('a'): 2
	}) == '{"a":2,"z":1}'
	assert encode({
		SortedEnumKey.zebra: 1
		SortedEnumKey.apple: 2
	}) == '{"apple":2,"zebra":1}'
}

fn test_encode_any_and_nested_maps_sort_keys() {
	value := Any({
		'b': Any(1)
		'a': Any(2)
	})
	assert encode(value) == '{"a":2,"b":1}'
	assert value.json_str() == '{"a":2,"b":1}'
	assert encode({
		'z': {
			'b': 1
			'a': 2
		}
		'a': {
			'd': 3
			'c': 4
		}
	}) == '{"a":{"c":4,"d":3},"z":{"a":2,"b":1}}'
	assert encode(Any({
		'z': value
		'a': Any({
			'd': Any(3)
			'c': Any(4)
		})
	})) == '{"a":{"c":4,"d":3},"z":{"a":2,"b":1}}'
}

fn test_encode_map_sorts_before_escaping_keys() {
	value := {
		'a':  1
		'\n': 2
		'"':  3
		'é':  4
	}
	assert encode(value) == r'{"\n":2,"\"":3,"a":1,"é":4}'
	assert encode(value, escape_unicode: true) == r'{"\n":2,"\"":3,"a":1,"\u00e9":4}'
	assert encode({
		'b': 1
		'a': 2
	}, prettify: true, indent_string: '  ') == '{\n  "a": 2,\n  "b": 1\n}'
	assert encode({
		'b': 1
		'a': 2
	}, prettify: true, legacy_layout: true) == '{\n\t"a":\t2,\n\t"b":\t1\n}'
}

fn test_encode_map_preserves_owned_keys_after_reuse() {
	first := 'a'.repeat(64) + '1'
	second := 'b'.repeat(64) + '2'
	third := 'c'.repeat(64) + '3'
	mut values := map[string]int{}
	values[first] = 0
	values[second] = 20
	for i in 0 .. 32 {
		assert encode(values) == '{"${first}":${i},"${second}":20}'
		assert values[first] == i
		assert values[second] == 20
		values[first] = i + 1
	}
	values.delete(second)
	values[third] = 30
	assert encode(values) == '{"${first}":32,"${third}":30}'
	assert values[first] == 32
	assert values[third] == 30
	values.clear()
	values[second] = 99
	assert encode(values) == '{"${second}":99}'
	assert values[second] == 99
}
