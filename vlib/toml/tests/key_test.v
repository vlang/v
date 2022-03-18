import os
import toml
import toml.to

fn test_keys() {
	toml_file :=
		os.real_path(os.join_path(os.dir(@FILE), 'testdata', os.file_name(@FILE).all_before_last('.'))) +
		'.toml'
	toml_doc := toml.parse_file(toml_file) or { panic(err) }

	mut value := toml_doc.value('34-11')
	assert value.int() == 23

	value = toml_doc.value('1.2')
	assert value.int() == 3

	value = toml_doc.value('34-12.2')
	assert value.int() == 42

	toml_json := to.json(toml_doc)
	out_file :=
		os.real_path(os.join_path(os.dir(@FILE), 'testdata', os.file_name(@FILE).all_before_last('.'))) +
		'.out'
	out_file_json := os.read_file(out_file) or { panic(err) }
	println(toml_json)
	assert toml_json == out_file_json
}
