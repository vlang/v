import os
import x.toml

// TODO whole text from the example in the README.md:
// https://github.com/toml-lang/toml/blob/3b11f6921da7b6f5db37af039aa021fee450c091/README.md#Example
const toml_text = '# Test TOML file

title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00#-08:00 # First class dates

[database]
server = "192.168.1.1"
ports = [ 8000, 8001, 8002 ]
connection_max = 5000
enabled = true'

fn test_parse_file() {
	out_path := os.join_path(os.temp_dir(), 'v_toml_tests')
	test_file := os.join_path(out_path, 'toml_parse_file_test_1.toml')
	os.mkdir_all(out_path) or { assert false }
	os.write_file(test_file, toml_text) or { assert false }
	toml_doc := toml.parse_file(test_file)

	assert toml_doc.value('title') as string == 'TOML Example'

	// dump(toml_doc.ast)
	// assert false
}

fn test_parse_text() {
	toml_doc := toml.parse_text(toml_text)
	assert toml_doc.value('title') as string == 'TOML Example'
}

fn test_i64() {
	toml_txt := 'integer = 120'
	toml_doc := toml.parse_text(toml_txt)
	assert toml_doc.value('integer') as i64 == 120
}
