import os
import toml

// Instructions for developers:
// The large (1MB) TOML file can be obtained by doing:
// `cd vlib/toml/tests/testdata`
// `wget https://gist.githubusercontent.com/Larpon/89b0e3d94c6903851ff15559e5df7a05/raw/62a1f87a4e37bf157f2e0bfb32d85d840c98e422/large_toml_file_test.toml`

// See also the CI toml tests

// test_large_file parser 'testdata/large_toml_file_test.toml' if found.
fn test_large_file() {
	toml_file :=
		os.real_path(os.join_path(os.dir(@FILE), 'testdata', os.file_name(@FILE).all_before_last('.'))) +
		'.toml'
	if os.exists(toml_file) {
		println('Testing parsing of large (${os.file_size(toml_file)} bytes) "$toml_file"...')
		toml_doc := toml.parse_file(toml_file) or { panic(err) }
		println('OK   [1/1] "$toml_file"...') // So it can be checked with `v -stats test ...`
	}
}
