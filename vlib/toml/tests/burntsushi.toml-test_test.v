import os
import toml

// Instructions for developers:
// The actual tests and data can be obtained by doing:
// `cd vlib/toml/tests/testdata`
// `git clone --depth 1 https://github.com/BurntSushi/toml-test.git burntsushi/toml-test`
// See also the CI toml tests
// TODO Goal: make parsing AND value retrieval of all of https://github.com/BurntSushi/toml-test/test/ pass
const (
	valid_exceptions   = [
		'float/inf-and-nan.toml',
		'table/array-table-array.toml',
	]
	invalid_exceptions = [
		// String
		'string/basic-multiline-out-of-range-unicode-escape-1.toml',
		'string/basic-byte-escapes.toml',
		'string/multiline-escape-space.toml',
		'string/bad-codepoint.toml',
		'string/basic-multiline-out-of-range-unicode-escape-2.toml',
		'string/bad-slash-escape.toml',
		'string/basic-out-of-range-unicode-escape-1.toml',
		'string/basic-out-of-range-unicode-escape-2.toml',
		'string/bad-uni-esc.toml',
		'string/bad-escape.toml',
		'string/basic-multiline-unknown-escape.toml',
		'string/missing-quotes.toml',
		'string/bad-byte-escape.toml',
		'string/basic-unknown-escape.toml',
		// Integer
		'integer/capital-bin.toml',
		'integer/invalid-bin.toml',
		'integer/invalid-oct.toml',
		// Encoding
		'encoding/bad-utf8-in-comment.toml',
		'encoding/bad-utf8-in-string.toml',
		// Float
		'float/exp-double-us.toml',
		'float/exp-leading-us.toml',
		'float/nan_underscore.toml',
		'float/nan-incomplete-1.toml',
		'invalid/float/exp-point-1.toml',
		'float/trailing-us.toml',
		'float/us-after-point.toml',
		'float/exp-double-e-1.toml',
		'float/inf-incomplete-1.toml',
		'float/inf_underscore.toml',
		// Table
		'table/rrbrace.toml',
		'table/duplicate-table-array2.toml',
		'table/duplicate.toml',
		'table/array-implicit.toml',
		'table/injection-2.toml',
		'table/llbrace.toml',
		'table/injection-1.toml',
		'table/duplicate-table-array.toml',
		// Array
		'array/tables-1.toml',
		'array/no-close-2.toml',
		'array/missing-separator.toml',
		'array/text-after-array-entries.toml',
		'array/no-close.toml',
		'array/text-before-array-separator.toml',
		// Date / Time
		'datetime/impossible-date.toml',
		'datetime/no-leads-with-milli.toml',
		'datetime/no-leads.toml',
		// Control
		'control/string-us.toml',
		'control/comment-lf.toml',
		'control/multi-us.toml',
		'control/rawstring-del.toml',
		'control/rawmulti-del.toml',
		'control/rawstring-us.toml',
		'control/string-bs.toml',
		'control/multi-null.toml',
		'control/rawstring-lf.toml',
		'control/rawmulti-null.toml',
		'control/comment-null.toml',
		'control/multi-lf.toml',
		'control/comment-del.toml',
		'control/rawstring-null.toml',
		'control/rawmulti-lf.toml',
		'control/multi-del.toml',
		'control/string-del.toml',
		'control/rawmulti-us.toml',
		'control/comment-us.toml',
		'control/string-lf.toml',
		'control/string-null.toml',
		'inline-table/empty.toml',
		'inline-table/double-comma.toml',
		'inline-table/trailing-comma.toml',
		'inline-table/linebreak-4.toml',
		'inline-table/linebreak-3.toml',
		'inline-table/linebreak-1.toml',
		'inline-table/linebreak-2.toml',
		'inline-table/no-comma.toml',
		// Key
		'key/duplicate.toml',
		'key/after-table.toml',
		'key/duplicate-keys.toml',
		'key/after-value.toml',
		'key/newline.toml',
		'key/without-value-2.toml',
		'key/no-eol.toml',
		'key/after-array.toml',
		'key/multiline.toml',
	]
)

// test_burnt_sushi_tomltest run though 'testdata/burntsushi/toml-test/*' if found.
fn test_burnt_sushi_tomltest() {
	this_file := @FILE
	test_root := os.join_path(os.dir(this_file), 'testdata', 'burntsushi', 'toml-test',
		'tests')
	if os.is_dir(test_root) {
		valid_test_files := os.walk_ext(os.join_path(test_root, 'valid'), '.toml')
		println('Testing $valid_test_files.len valid TOML files...')
		mut valid := 0
		mut e := 0
		for i, valid_test_file in valid_test_files {
			relative := valid_test_file.all_after(os.join_path('toml-test', 'tests', 'valid')).trim_left(os.path_separator)
			if relative !in valid_exceptions {
				println('OK   [$i/$valid_test_files.len] "$valid_test_file"...')
				toml_doc := toml.parse_file(valid_test_file) or { panic(err) }

				// parsed_json := toml_doc.to_json().replace(' ','')
				// mut test_suite_json := os.read_file(valid_test_file.all_before_last('.')+'.json') or { panic(err) }
				// test_suite_json = test_suite_json.replace('\n ','').replace(' ','')
				// println(test_suite_json.replace('\n ','').replace(' ',''))
				// assert parsed_json == test_suite_json
				valid++
			} else {
				e++
				println('SKIP [$i/$valid_test_files.len] "$valid_test_file" EXCEPTION [$e/$valid_exceptions.len]...')
			}
		}
		println('$valid/$valid_test_files.len TOML files was parsed correctly')
		if valid_exceptions.len > 0 {
			println('TODO Skipped parsing of $valid_exceptions.len valid TOML files...')
		}

		// NOTE uncomment to see list of skipped files
		// assert false

		// TODO test cases where the parser should fail
		invalid_test_files := os.walk_ext(os.join_path(test_root, 'invalid'), '.toml')
		println('Testing $invalid_test_files.len invalid TOML files...')
		mut invalid := 0
		e = 0
		for i, invalid_test_file in invalid_test_files {
			relative := invalid_test_file.all_after(os.join_path('toml-test', 'tests',
				'invalid')).trim_left(os.path_separator)
			if relative !in invalid_exceptions {
				println('OK   [$i/$invalid_test_files.len] "$invalid_test_file"...')
				if toml_doc := toml.parse_file(invalid_test_file) {
					assert false
				} else {
					println('     $err.msg')
					assert true // err.msg == 'your error'
				}
				invalid++
			} else {
				e++
				println('SKIP [$i/$invalid_test_files.len] "$invalid_test_file" EXCEPTION [$e/$invalid_exceptions.len]...')
			}
		}
		println('$invalid/$invalid_test_files.len TOML files was parsed correctly')
		if invalid_exceptions.len > 0 {
			println('TODO Skipped parsing of $invalid_exceptions.len invalid TOML files...')
		}

		// NOTE uncomment to see list of skipped files
		// assert false
	} else {
		println('No test data directory found in "$test_root"')
		assert true
	}
}
