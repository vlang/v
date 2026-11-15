import json2 as json

struct StructType[T] {
mut:
	val T
}

fn test_array_of_strings() {
	assert json.decode[[]int]('[1, 2, 3]')! == [1, 2, 3]

	assert json.decode[[]string]('["a", "b", "c"]')! == ['a', 'b', 'c']

	assert json.decode[[]bool]('[true, false, true]')! == [true, false, true]

	assert json.decode[[]f64]('[1.1, 2.2, 3.3]')! == [1.1, 2.2, 3.3]

	// nested arrays
	assert json.decode[[][]int]('[[1, 22], [333, 4444]]')! == [
		[1, 22],
		[333, 4444],
	]

	assert json.decode[[][]string]('[["a", "b"], ["c", "d"]]')! == [
		['a', 'b'],
		['c', 'd'],
	]

	assert json.decode[[][]bool]('[[true, false], [false, true]]')! == [
		[true, false],
		[false, true],
	]
}

fn test_array_of_struct() {
	assert json.decode[[]StructType[int]]('[{"val": 1}, {"val": 2}, {"val": 3}, {"val": 4}]')! == [
		StructType{
			val: 1
		},
		StructType{
			val: 2
		},
		StructType{
			val: 3
		},
		StructType{
			val: 4
		},
	]
}

struct Data {
	name string
	data [][]f64
}

fn test_nested_float_arrays_in_struct() {
	json_data := '{"name":"test","data":[[1,2,3],[4,5,6]]}'
	info := json.decode[Data](json_data)!
	assert info.name == 'test'
	assert info.data == [[1.0, 2, 3], [4.0, 5, 6]]
}

struct Dataset {
	values [][]string
}

struct Dataset2 {
	values [][]u8
}

fn test_nested_string_array_round_trip() {
	d := Dataset{
		values: [
			['a', 'b', 'c'],
			['d', 'e', 'f'],
		]
	}
	s := json.encode(d)
	d2 := json.decode[Dataset](s)!
	assert d2.values == d.values
}

fn test_nested_u8_array_round_trip() {
	d := Dataset2{
		values: [
			[u8(1), 2, 3, 4, 5],
			[u8(2), 3, 4, 5, 6],
		]
	}
	s := json.encode(d)
	d2 := json.decode[Dataset2](s)!
	assert d2.values == d.values
}

struct FixedElem {
	value int = 7
}

struct FixedFields {
mut:
	nums  [3]int = [1, 2, 3]!
	elems [2]FixedElem
	ptrs  [2]&FixedElem
	grid  [2][2]int
}

fn test_fixed_array() {
	mut expected := [3]int{}
	expected[0] = 1
	expected[1] = 2
	expected[2] = 3
	assert json.decode[[3]int]('[1, 2, 3]')! == expected
}

fn test_fixed_array_to_few() {
	json.decode[[4]int]('[1, 2, 3]', strict: true) or {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 8
			assert err.message == 'Data: Fixed size array expected 4 elements but got 3 elements'
		}

		return
	}
	assert false
}

fn test_fixed_array_to_many() {
	json.decode[[2]int]('[1, 2, 3]', strict: true) or {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 8
			assert err.message == 'Data: Fixed size array expected 2 elements but got 3 elements'
		}

		return
	}
	assert false
}

fn test_fixed_array_null_in_strict_mode() {
	json.decode[FixedFields]('{"nums":null}', strict: true) or {
		assert err.msg().contains('Expected array, but got null')
		return
	}
	assert false
}

fn test_fixed_array_lenient_lengths() {
	assert json.decode[[4]int]('[1, 2, 3]')! == [1, 2, 3, 0]!
	assert json.decode[[2]int]('[1, 2, 3, {"skipped": [4]}]')! == [1, 2]!

	short := json.decode[FixedFields]('{"nums":[9],"elems":[{"value":4}],"ptrs":[{"value":5}],"grid":[[1],[2,3,4]]}')!
	assert short.nums == [9, 2, 3]!
	assert short.elems[0].value == 4
	assert short.elems[1].value == 7
	assert short.ptrs[0].value == 5
	assert short.ptrs[1] == unsafe { nil }
	assert short.grid == [[1, 0]!, [2, 3]!]!
}

fn test_fixed_array_lenient_null() {
	decoded := json.decode[FixedFields]('{"nums":null,"elems":null,"ptrs":[null,{"value":1}],"grid":[null,[3,4]]}')!
	assert decoded.nums == [1, 2, 3]!
	assert decoded.elems[0].value == 7
	assert decoded.ptrs[0] == unsafe { nil }
	assert decoded.ptrs[1].value == 1
	assert decoded.grid == [[0, 0]!, [3, 4]!]!
}

fn test_fixed_array_rejects_other_values() {
	json.decode[FixedFields]('{"nums":"123"}') or {
		assert err.msg().contains('Expected array, but got string')
		return
	}
	assert false
}
