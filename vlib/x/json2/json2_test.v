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
	o := json.fast_raw_decode(s) or {
		assert false
		json.Any(json.null)
	}
	str := o.str()
	assert str == '{"name":"Peter","age":"28","salary":"95000.5","title":"2"}'
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

fn test_struct_to_map() {
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

// // waiting for  https://github.com/vlang/v/pull/16796
// fn test_struct_to_map2() {
// 	array_of_struct := [StructType[[]bool]{
// 		val: [false, true]
// 	}, StructType[[]bool]{
// 		val: [true, false]
// 	}]

// 	mut array_of_map := []json.Any{}

// 	for variable in array_of_struct {
// 		array_of_map << json.map_from(variable)
// 	}

// 	assert array_of_map.str() == '[{"val":[false,true]},{"val":[true,false]}]'
// }
