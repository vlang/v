import os
import x.toml

fn test_parse_file() {
	out_path := os.join_path(os.temp_dir(), 'v_toml_tests')
	test_file := os.join_path(out_path, 'toml_parse_file_test_1.toml')
	toml_str := '# Test TOML file

title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
#dob = 1979-05-27T07:32:00-08:00 # First class dates
'
	os.mkdir_all(out_path) or { assert false }
	os.write_file(test_file, toml_str) or { assert false }
	ast_root := toml.parse_file(test_file)
	//dump(ast_root)
	//assert false
}
