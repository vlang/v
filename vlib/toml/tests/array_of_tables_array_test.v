import os
import toml
import toml.to

const (
	toml_text = '[[a]]
    [[a.b]]
        [a.b.c]
            d = "val0"
    [[a.b]]
        [a.b.c]
            d = "val1"
'
)

fn test_nested_array_of_tables() {
	mut toml_doc := toml.parse_text(toml_text) or { panic(err) }

	toml_json := to.json(toml_doc)

	eprintln(toml_json)
	assert toml_json == os.read_file(
		os.real_path(os.join_path(os.dir(@FILE), 'testdata', os.file_name(@FILE).all_before_last('.'))) +
		'.out') or { panic(err) }
}
