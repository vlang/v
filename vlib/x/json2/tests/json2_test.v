import x.json2 as json

enum JobTitle {
	manager
	executive
	worker
}

struct Employee {
pub mut:
	name   string
	age    int
	salary f32
	title  JobTitle
}

fn test_fast_raw_decode() {
	s := '{"name":"Peter","age":28,"salary":95000.5,"title":2}'
	o := json.decode[json.Any](s) or {
		assert false
		json.Any('')
	}
	str := o.str()
	assert str == '{"name":"Peter","age":28,"salary":95000.5,"title":2}'
}

fn test_character_unescape() {
	message := r'{
	"newline": "new\nline",
	"tab": "\ttab",
	"backslash": "back\\slash",
	"quotes": "\"quotes\"",
	"slash":"\/dev\/null"
}'
	mut obj := json.raw_decode(message) or {
		println(err)
		assert false
		return
	}
	lines := obj.as_map()
	assert lines['newline'] or { 0 }.str() == 'new\nline'
	assert lines['tab'] or { 0 }.str() == '\ttab'
	assert lines['backslash'] or { 0 }.str() == 'back\\slash'
	assert lines['quotes'] or { 0 }.str() == '"quotes"'
	assert lines['slash'] or { 0 }.str() == '/dev/null'
}

struct StructType[T] {
mut:
	val T
}

fn test_struct_with_bool_to_map() {
	array_of_struct := [StructType[bool]{
		val: true
	}, StructType[bool]{
		val: false
	}]

	mut array_of_map := []json.Any{}

	for variable in array_of_struct {
		array_of_map << json.map_from(variable)
	}

	assert array_of_map.str() == '[{"val":true},{"val":false}]'
}

fn test_struct_with_string_to_map() {
	array_of_struct := [StructType[string]{
		val: 'true'
	}, StructType[string]{
		val: 'false'
	}]

	mut array_of_map := []json.Any{}

	for variable in array_of_struct {
		array_of_map << json.map_from(variable)
	}

	assert array_of_map.str() == '[{"val":"true"},{"val":"false"}]'
}

fn test_struct_with_array_to_map() {
	array_of_struct := [StructType[[]bool]{
		val: [false, true]
	}, StructType[[]bool]{
		val: [true, false]
	}]

	mut array_of_map := []json.Any{}

	for variable in array_of_struct {
		array_of_map << json.map_from(variable)
	}

	assert array_of_map.str() == '[{"val":[false,true]},{"val":[true,false]}]'
}

fn test_struct_with_array_of_arrays_to_map() {
	array_of_struct := [
		StructType[[][]bool]{
			val: [[true, false], [true, false]]
		},
		StructType[[][]bool]{
			val: [[false, true], [false, true]]
		},
	]
	mut array_of_map := []json.Any{}
	for variable in array_of_struct {
		array_of_map << json.map_from(variable)
	}
	assert array_of_map.str() == '[{"val":[[true,false],[true,false]]},{"val":[[false,true],[false,true]]}]'

	array_of_struct_int := [
		StructType[[][]int]{
			val: [[1, 0], [1, 0]]
		},
		StructType[[][]int]{
			val: [[0, 1], [0, 1]]
		},
	]
	mut array_of_map_int := []json.Any{}
	for variable in array_of_struct_int {
		array_of_map_int << json.map_from(variable)
	}
	assert array_of_map_int.str() == '[{"val":[[1,0],[1,0]]},{"val":[[0,1],[0,1]]}]'
}

fn test_struct_with_number_to_map() {
	assert json.map_from(StructType[string]{'3'}).str() == '{"val":"3"}'
	assert json.map_from(StructType[bool]{true}).str() == '{"val":true}'
	assert json.map_from(StructType[i8]{3}).str() == '{"val":3}'
	assert json.map_from(StructType[i16]{3}).str() == '{"val":3}'
	assert json.map_from(StructType[int]{3}).str() == '{"val":3}'
	assert json.map_from(StructType[i64]{3}).str() == '{"val":3}'
	assert json.map_from(StructType[i8]{-3}).str() == '{"val":-3}'
	assert json.map_from(StructType[i16]{i16(-3)}).str() == '{"val":-3}'
	assert json.map_from(StructType[int]{-3}).str() == '{"val":-3}'
	assert json.map_from(StructType[i64]{-3}).str() == '{"val":-3}'
	assert json.map_from(StructType[f32]{3.0}).str() == '{"val":3}'
	assert json.map_from(StructType[f64]{3.0}).str() == '{"val":3}'
	assert json.map_from(StructType[u8]{3}).str() == '{"val":3}'
	assert json.map_from(StructType[u16]{3}).str() == '{"val":3}'
	assert json.map_from(StructType[u32]{3}).str() == '{"val":3}'
	assert json.map_from(StructType[u64]{3}).str() == '{"val":3}'
}

fn test_struct_with_struct_to_map() {
	assert json.map_from(StructType[StructType[string]]{StructType[string]{'3'}}).str() == '{"val":{"val":"3"}}'
	assert json.map_from(StructType[StructType[int]]{StructType[int]{3}}).str() == '{"val":{"val":3}}'
}

fn test_maps() {
	assert json.decode[map[string]string]('{"test":"abc"}')! == {
		'test': 'abc'
	}

	// assert json.decode[map[string]StructType[bool]]('{"test":{"val":true}}') or {
	// 	dump(err)
	// 	assert false
	// } == {"test":StructType[bool]{true}}
}
