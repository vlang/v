import os
import x.toml

// TODO Goal: make parsing AND value retrieval of all of https://github.com/BurntSushi/toml-test/test pass
const (
	valid_exceptions   = [
		'float/exponent.toml',
		'float/inf-and-nan.toml',
		'table/array-table-array.toml', // <- TODO This one is a real turd-fest, not sure if we should even support it
	]
	// valid_exceptions = ['']
	invalid_exceptions = ['']
)

// Run though 'testdata/toml-test/tests' if found.
// The actual tests and data can be obtained by doing:
// `cd testdata`
// `git clone git@github.com:BurntSushi/toml-test.git burntsushi/toml-test`
fn test_burnt_sushi_tomltest() {
	this_file := @FILE
	test_root := os.join_path(os.dir(this_file), 'testdata', 'burntsushi', 'toml-test', 'tests')
	if os.is_dir(test_root) {
		valid_test_files := os.walk_ext(os.join_path(test_root, 'valid'), '.toml')
		println('Testing $valid_test_files.len valid TOML files...')
		mut valid := 0
		mut e := 0
		for i, valid_test_file in valid_test_files {
			relative := valid_test_file.all_after(os.join_path('toml-test', 'tests', 'valid')).trim_left(os.path_separator)
			if relative !in valid_exceptions {
				println('OK   [$i/$valid_test_files.len] "$valid_test_file"...')
				toml_doc := toml.parse_file(valid_test_file)

				//parsed_json := toml_doc.to_json().replace(' ','')
				//mut test_suite_json := os.read_file(valid_test_file.all_before_last('.')+'.json') or { panic(err) }
				//test_suite_json = test_suite_json.replace('\n ','').replace(' ','')
				//println(test_suite_json.replace('\n ','').replace(' ',''))
				//assert parsed_json == test_suite_json
				valid++
			} else {
				e++
				println('SKIP [$i/$valid_test_files.len] "$valid_test_file" EXCEPTION [$e/$valid_exceptions.len]...')
			}
		}
		println('$valid/$valid_test_files.len TOML files was parsed correctly')
		// TODO
		println('TODO Skipped parsing of $valid_exceptions.len valid TOML files...')

		// NOTE uncomment to see list of skipped files
		// assert false

		/*
		// TODO test cases where the parser should fail
		invalid_test_files := os.walk_ext(os.join_path(test_root,'invalid'), '.toml')
		println('Testing $invalid_test_files.len invalid TOML files...')
		for i, invalid_test_file in invalid_test_files {
			relative := invalid_test_file.all_after(os.join_path('toml-test','tests','valid')).trim_left(os.path_separator)
			if relative !in invalid_exceptions {
				println('Parsing $i/$invalid_test_files.len "$invalid_test_file"...')
				toml_doc := toml.parse_file(invalid_test_file)
			}
		}
		println('TODO Skipped $invalid_exceptions.len valid files...')
		*/
	} else {
		println('No test data directory found in "$test_root"')
		assert true
	}
}
