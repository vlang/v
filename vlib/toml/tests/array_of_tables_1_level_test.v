import os
import toml
import toml.to

const (
	toml_table_text = '
[[products]]
name = "Hammer"
sku = 738594937

[[products]]  # empty table within the array

[[products]]
name = "Nail"
sku = 284758393

color = "gray"'
)

fn test_tables() {
	mut toml_doc := toml.parse_text(toml_table_text) or { panic(err) }

	toml_json := to.json(toml_doc)

	assert toml_json == os.read_file(
		os.real_path(os.join_path(os.dir(@FILE), 'testdata', os.file_name(@FILE).all_before_last('.'))) +
		'.out') or { panic(err) }
}
