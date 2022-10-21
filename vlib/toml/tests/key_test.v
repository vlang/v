import os
import toml
import toml.to

fn path_by_extension(ext string) string {
	return os.join_path(os.dir(@VEXE), 'vlib/toml/tests/testdata/key_test.$ext')
}

fn test_keys() {
	toml_doc := toml.parse_file(path_by_extension('toml'))!

	mut value := toml_doc.value('34-11')
	assert value.int() == 23

	value = toml_doc.value('1.2')
	assert value.int() == 3

	value = toml_doc.value('34-12.2')
	assert value.int() == 42

	toml_json := to.json(toml_doc)
	out_file_json := os.read_file(path_by_extension('out'))!
	println(toml_json)
	assert toml_json == out_file_json
	//
	if x := toml_doc.value_opt('unknown key') {
		assert false
	} else {
		assert err.msg() == 'no value for key'
	}
	if x := toml_doc.value_opt("'a") {
		assert false
	} else {
		assert err.msg() == 'invalid dotted key'
	}
}

fn test_parse_dotted_key() {
	assert toml.parse_dotted_key('')! == []
	assert toml.parse_dotted_key('abc')! == ['abc']
	assert toml.parse_dotted_key('tube.test."test.test".h."i.j."."k"')! == ['tube', 'test',
		'test.test', 'h', 'i.j.', 'k']
	if x := toml.parse_dotted_key("'some unclosed string") {
		assert false
	} else {
		assert err.msg().starts_with('parse_dotted_key: could not parse key, missing closing string delimiter')
	}
}
