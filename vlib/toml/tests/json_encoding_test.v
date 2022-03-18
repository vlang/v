import os
import toml
import toml.to

const fprefix = os.join_path(os.dir(@FILE), 'testdata', os.file_name(@FILE).all_before_last('.'))

fn test_parse() ? {
	toml_doc := toml.parse_file(os.real_path(fprefix + '.toml')) ?

	toml_json := to.json(toml_doc)
	println(toml_json)

	out_file_json := os.read_file(os.real_path(fprefix + '.out')) ?
	assert toml_json == out_file_json
}
