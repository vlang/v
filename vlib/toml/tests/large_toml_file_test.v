import os
import toml

// Instructions for developers:
// The large (1MB) TOML file can be obtained by doing:
// `wget https://gist.githubusercontent.com/Larpon/89b0e3d94c6903851ff15559e5df7a05/raw/62a1f87a4e37bf157f2e0bfb32d85d840c98e422/large_toml_file_test.toml`
// `mv large_toml_file_test.toml vlib/toml/tests/testdata/`

const toml_file = os.join_path_single(@VEXEROOT, 'vlib/toml/tests/testdata/large_toml_file_test.toml')

fn test_large_file() {
	if !os.exists(toml_file) {
		eprintln('skipping ${@FILE} since ${toml_file} is missing.')
		return
	}
	println('Testing parsing of large (${os.file_size(toml_file)} bytes) "${toml_file}"...')
	doc := toml.parse_file(toml_file) or { panic(err) }
	assert true
}
