import os
import toml
import toml.ast
import x.json2

// Instructions for developers:
// The actual tests and data can be obtained by doing:
// `cd vlib/toml/tests/testdata`
// `git clone --depth 1 https://github.com/iarna/toml-spec-tests.git iarna/toml-test`
// See also the CI toml tests
const (
	// Can be set to `true` to skip tests that stress test the parser
	// by having large data amounts - these pass - but slow down the test run
	skip_large_files       = false

	// Kept for easier handling of future updates to the tests
	valid_exceptions       = [
		'values/spec-key-value-pair-8.toml',
		'values/spec-float-3.toml',
		'values/spec-float-10.toml',
		'values/spec-float-11.toml',
		'values/spec-float-12.toml',
		'values/spec-float-13.toml',
		'values/spec-float-14.toml',
		'values/spec-float-15.toml',
		'values/spec-key-value-pair-6.toml',
	]
	invalid_exceptions     = [
		'errors/table-3.toml',
		'errors/table-4.toml',
		'errors/table-invalid-4.toml',
		'errors/inline-table-imutable-1.toml',
	]

	valid_value_exceptions = [
		'values/spec-date-local-1.toml',
		'values/spec-date-time-local-1.toml',
		'values/spec-date-time-local-2.toml',
		'values/spec-time-1.toml',
		'values/spec-time-2.toml',
	]

	jq                     = os.find_abs_path_of_executable('jq') or { '' }
	compare_work_dir_root  = os.join_path(os.temp_dir(), 'v', 'toml', 'iarna')
	// From: https://stackoverflow.com/a/38266731/1904615
	jq_normalize           = r'# Apply f to composite entities recursively using keys[], and to atoms
def sorted_walk(f):
  . as $in
  | if type == "object" then
      reduce keys[] as $key
        ( {}; . + { ($key):  ($in[$key] | sorted_walk(f)) } ) | f
  elif type == "array" then map( sorted_walk(f) ) | f
  else f
  end;

def normalize: sorted_walk(if type == "array" then sort else . end);

normalize'
)

fn run(args []string) ?string {
	res := os.execute(args.join(' '))
	if res.exit_code != 0 {
		return error('${args[0]} failed with return code ${res.exit_code}.\n$res.output')
	}
	return res.output
}

// test_iarna_toml_spec_tests run though 'testdata/iarna/toml-test/*' if found.
fn test_iarna_toml_spec_tests() {
	this_file := @FILE
	test_root := os.join_path(os.dir(this_file), 'testdata', 'iarna', 'toml-test')
	if os.is_dir(test_root) {
		valid_test_files := os.walk_ext(os.join_path(test_root, 'values'), '.toml')
		println('Testing $valid_test_files.len valid TOML files...')
		mut valid := 0
		mut e := 0
		for i, valid_test_file in valid_test_files {
			mut relative := valid_test_file.all_after('toml-test').trim_left(os.path_separator)
			$if windows {
				relative = relative.replace('/', '\\')
			}

			if skip_large_files && valid_test_file.contains('qa-') {
				e++
				println('SKIP [${i + 1}/$valid_test_files.len] "$valid_test_file" EXCEPTION [$e/$valid_exceptions.len]...')
				continue
			}

			if relative !in valid_exceptions {
				println('OK   [${i + 1}/$valid_test_files.len] "$valid_test_file"...')
				toml_doc := toml.parse_file(valid_test_file) or { panic(err) }
				valid++
			} else {
				e++
				println('SKIP [${i + 1}/$valid_test_files.len] "$valid_test_file" EXCEPTION [$e/$valid_exceptions.len]...')
			}
		}
		println('$valid/$valid_test_files.len TOML files was parsed correctly')
		if valid_exceptions.len > 0 {
			println('TODO Skipped parsing of $e valid TOML files...')
		}

		// If the command-line tool `jq` is installed, value tests can be run as well.
		if jq != '' {
			println('Testing value output of $valid_test_files.len valid TOML files using "$jq"...')

			if os.exists(compare_work_dir_root) {
				os.rmdir_all(compare_work_dir_root) or { panic(err) }
			}
			os.mkdir_all(compare_work_dir_root) or { panic(err) }

			jq_normalize_path := os.join_path(compare_work_dir_root, 'normalize.jq')
			os.write_file(jq_normalize_path, jq_normalize) or { panic(err) }

			valid = 0
			e = 0
			for i, valid_test_file in valid_test_files {
				mut relative := valid_test_file.all_after('toml-test').trim_left(os.path_separator)
				$if windows {
					relative = relative.replace('/', '\\')
				}
				if !os.exists(valid_test_file.all_before_last('.') + '.json') {
					println('N/A  [${i + 1}/$valid_test_files.len] "$valid_test_file"...')
					continue
				}
				// Skip the file if we know it can't be parsed or we know that the value retrieval needs work.
				if relative !in valid_exceptions && relative !in valid_value_exceptions {
					println('OK   [${i + 1}/$valid_test_files.len] "$valid_test_file"...')
					toml_doc := toml.parse_file(valid_test_file) or { panic(err) }

					v_toml_json_path := os.join_path(compare_work_dir_root,
						os.file_name(valid_test_file).all_before_last('.') + '.v.json')
					iarna_toml_json_path := os.join_path(compare_work_dir_root,
						os.file_name(valid_test_file).all_before_last('.') + '.json')

					os.write_file(v_toml_json_path, to_iarna(toml_doc.ast.table)) or { panic(err) }

					iarna_json := os.read_file(valid_test_file.all_before_last('.') + '.json') or {
						panic(err)
					}
					os.write_file(iarna_toml_json_path, iarna_json) or { panic(err) }

					v_normalized_json := run([jq, '-S', '-f "$jq_normalize_path"', v_toml_json_path]) or {
						contents := os.read_file(v_toml_json_path) or { panic(err) }
						panic(err.msg + '\n$contents')
					}
					iarna_normalized_json := run([jq, '-S', '-f "$jq_normalize_path"',
						iarna_toml_json_path]) or {
						contents := os.read_file(v_toml_json_path) or { panic(err) }
						panic(err.msg + '\n$contents')
					}

					assert iarna_normalized_json == v_normalized_json

					valid++
				} else {
					e++
					println('SKIP [${i + 1}/$valid_test_files.len] "$valid_test_file" EXCEPTION [$e/$valid_value_exceptions.len]...')
				}
			}
			println('$valid/$valid_test_files.len TOML files was parsed correctly and value checked')
			if valid_value_exceptions.len > 0 {
				println('TODO Skipped value checks of $e valid TOML files...')
			}
		}

		invalid_test_files := os.walk_ext(os.join_path(test_root, 'errors'), '.toml')
		println('Testing $invalid_test_files.len invalid TOML files...')
		mut invalid := 0
		e = 0
		for i, invalid_test_file in invalid_test_files {
			mut relative := invalid_test_file.all_after('toml-test').trim_left(os.path_separator)
			$if windows {
				relative = relative.replace('/', '\\')
			}
			if relative !in invalid_exceptions {
				println('OK   [${i + 1}/$invalid_test_files.len] "$invalid_test_file"...')
				if toml_doc := toml.parse_file(invalid_test_file) {
					content_that_should_have_failed := os.read_file(invalid_test_file) or {
						panic(err)
					}
					println('     This TOML should have failed:\n${'-'.repeat(40)}\n$content_that_should_have_failed\n${'-'.repeat(40)}')
					assert false
				} else {
					println('     $err.msg')
					assert true
				}
				invalid++
			} else {
				e++
				println('SKIP [${i + 1}/$invalid_test_files.len] "$invalid_test_file" EXCEPTION [$e/$invalid_exceptions.len]...')
			}
		}
		println('$invalid/$invalid_test_files.len TOML files was parsed correctly')
		if invalid_exceptions.len > 0 {
			println('TODO Skipped parsing of $invalid_exceptions.len invalid TOML files...')
		}
	} else {
		println('No test data directory found in "$test_root"')
		assert true
	}
}

// to_iarna returns a iarna compatible json string converted from the `value` ast.Value.
fn to_iarna(value ast.Value) string {
	match value {
		ast.Quoted {
			json_text := json2.Any(value.text).json_str()
			return '{ "type": "string", "value": "$json_text" }'
		}
		ast.DateTime {
			// Normalization for json
			json_text := json2.Any(value.text).json_str().to_upper().replace(' ', 'T')
			typ := if json_text.ends_with('Z') || json_text.all_after('T').contains('-')
				|| json_text.all_after('T').contains('+') {
				'datetime'
			} else {
				'datetime-local'
			}
			return '{ "type": "$typ", "value": "$json_text" }'
		}
		ast.Date {
			json_text := json2.Any(value.text).json_str()
			return '{ "type": "date-local", "value": "$json_text" }'
		}
		ast.Time {
			json_text := json2.Any(value.text).json_str()
			return '{ "type": "time-local", "value": "$json_text" }'
		}
		ast.Bool {
			json_text := json2.Any(value.text.bool()).json_str()
			return '{ "type": "bool", "value": "$json_text" }'
		}
		ast.Null {
			json_text := json2.Any(value.text).json_str()
			return '{ "type": "null", "value": "$json_text" }'
		}
		ast.Number {
			if value.text.contains('inf') || value.text.contains('nan') {
				return '{ "type": "float", "value": "$value.text" }'
			}
			if !value.text.starts_with('0x')
				&& (value.text.contains('.') || value.text.to_lower().contains('e')) {
				mut val := '$value.f64()'.replace('.e+', '.0e') // json notation
				if !val.contains('.') && val != '0' { // json notation
					val += '.0'
				}
				return '{ "type": "float", "value": "$val" }'
			}
			v := value.i64()
			// TODO workaround https://github.com/vlang/v/issues/9507
			if v == i64(-9223372036854775807 - 1) {
				return '{ "type": "integer", "value": "-9223372036854775808" }'
			}
			return '{ "type": "integer", "value": "$v" }'
		}
		map[string]ast.Value {
			mut str := '{ '
			for key, val in value {
				json_key := json2.Any(key).json_str()
				str += ' "$json_key": ${to_iarna(val)},'
			}
			str = str.trim_right(',')
			str += ' }'
			return str
		}
		[]ast.Value {
			mut str := '[ '
			for val in value {
				str += ' ${to_iarna(val)},'
			}
			str = str.trim_right(',')
			str += ' ]\n'
			return str
		}
	}
	return '<error>'
}
