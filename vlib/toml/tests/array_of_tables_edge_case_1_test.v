import os
import toml
import toml.to

fn test_array_of_tables_edge_case_file() {
	toml_file :=
		os.real_path(os.join_path(os.dir(@FILE), 'testdata', os.file_name(@FILE).all_before_last('.'))) +
		'.toml'
	toml_doc := toml.parse_file(toml_file) or { panic(err) }

	toml_json := to.json(toml_doc)
	out_file :=
		os.real_path(os.join_path(os.dir(@FILE), 'testdata', os.file_name(@FILE).all_before_last('.'))) +
		'.out'
	out_file_json := os.read_file(out_file) or { panic(err) }
	println(toml_json)
	assert toml_json == out_file_json
}
