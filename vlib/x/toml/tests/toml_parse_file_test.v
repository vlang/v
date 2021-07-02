import os
import x.toml

fn test_parse_file() {
	out_path := os.join_path(os.temp_dir(), 'v_toml_tests')
	test_file := os.join_path(out_path, 'toml_parse_file_test_1.toml')
	// Text from the eaxmple in the README.md:
	// https://github.com/toml-lang/toml/blob/3b11f6921da7b6f5db37af039aa021fee450c091/README.md#Example
	toml_str := '# Test TOML file

title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27 #TODO T07:32:00-08:00 # First class dates

[database]
server = "192.168.1.1"
#TODO ports = [ 8000, 8001, 8002 ]
connection_max = 5000
enabled = true'
	os.mkdir_all(out_path) or { assert false }
	os.write_file(test_file, toml_str) or { assert false }
	ast_root := toml.parse_file(test_file)
	// dump(ast_root)
	// assert false
}
