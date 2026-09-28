// vtest vflags: -w
import json2

pub struct MyStruct {
pub mut:
	valuea int
}

pub struct MyStruct2 {
pub mut:
	valuea int
	valueb ?MyStruct
}

struct Node {
	location NodeLocation @[json: 'loc']
}

struct NodeLocation {
	source_file ?SourceFile @[json: 'includedFrom']
}

struct SourceFile {
	path string @[json: 'file']
}

fn test_encode_decode() {
	assert json2.encode(MyStruct2{ valuea: 1 }, escape_unicode: true) == '{"valuea":1}'

	assert json2.decode[MyStruct2]('{"valuea": 1}')! == MyStruct2{
		valuea: 1
		valueb: none
	}
}

fn test_encode_decode2() {
	assert json2.encode(MyStruct2{ valuea: 1, valueb: none }, escape_unicode: true) == '{"valuea":1}'

	assert json2.decode[MyStruct2]('{"valuea": 1}')! == MyStruct2{
		valuea: 1
		valueb: none
	}
}

fn test_encode_decode3() {
	assert json2.encode(MyStruct2{
		valuea: 1
		valueb: MyStruct{
			valuea: 123
		}
	}, escape_unicode: true) == '{"valuea":1,"valueb":{"valuea":123}}'

	assert json2.decode[MyStruct2]('{"valuea": 1}')! == MyStruct2{
		valuea: 1
		valueb: none
	}
}

fn test_main() {
	node := json2.decode[Node]('{"loc": { "includedFrom": { "file": "/bin/foo" } } }')!

	source_file := node.location.source_file or {
		SourceFile{
			path: '-'
		}
	}

	assert source_file.path == '/bin/foo'
}
