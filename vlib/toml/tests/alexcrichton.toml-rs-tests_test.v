import os
import toml
import toml.ast
import x.json2

const hide_oks = os.getenv('VTEST_HIDE_OK') == '1'

// Instructions for developers:
// The actual tests and data can be obtained by doing:
// `git clone --depth 1 https://github.com/alexcrichton/toml-rs.git vlib/toml/tests/testdata/alexcrichton/toml-test`
// See also the CI toml tests
const (
	// Kept for easier handling of future updates to the tests
	valid_exceptions       = [
		'valid/example-v0.3.0.toml',
		'valid/example-v0.4.0.toml',
		'valid/datetime-truncate.toml', // Not considered valid since RFC 3339 doesn't permit > 6 ms digits ??
	]
	invalid_exceptions     = []string{}

	valid_value_exceptions = [
		// These have correct values, and should've passed, but the format of arrays is *mixed* in the JSON ??
		'valid/datetime-truncate.toml',
		'valid/example2.toml',
		'valid/example-v0.4.0.toml',
		'valid/example-v0.3.0.toml',
	]

	// These have correct values, and should've passed as-is, but the format of arrays changes in the JSON ??
	// We account for that here
	use_type_2_arrays      = [
		'valid/table-array-implicit.toml',
		'valid/table-array-many.toml',
		'valid/table-array-one.toml',
		'valid/table-array-nest.toml',
		'valid/table-array-nest-no-keys.toml',
	]
	tests_folder           = os.join_path('test-suite', 'tests')
	jq                     = os.find_abs_path_of_executable('jq') or { '' }
	compare_work_dir_root  = os.join_path(os.vtmp_dir(), 'v', 'toml', 'alexcrichton')
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

fn run(args []string) !string {
	res := os.execute(args.join(' '))
	if res.exit_code != 0 {
		return error('${args[0]} failed with return code ${res.exit_code}.\n$res.output')
	}
	return res.output
}

// test_alexcrichton_toml_rs run though 'testdata/alexcrichton/toml-test/test-suite/tests/*' if found.
fn test_alexcrichton_toml_rs() {
	this_file := @FILE
	test_root := os.join_path(os.dir(this_file), 'testdata', 'alexcrichton', 'toml-test')
	if os.is_dir(test_root) {
		valid_test_files := os.walk_ext(os.join_path(test_root, 'test-suite', 'tests',
			'valid'), '.toml')
		println('Testing $valid_test_files.len valid TOML files...')
		mut valid := 0
		mut e := 0
		for i, valid_test_file in valid_test_files {
			mut relative := valid_test_file.all_after(tests_folder).trim_left(os.path_separator)
			$if windows {
				relative = relative.replace('/', '\\')
			}

			if relative in valid_exceptions {
				e++
				idx := valid_exceptions.index(relative) + 1
				println('SKIP [${i + 1}/$valid_test_files.len] "$valid_test_file" VALID EXCEPTION [$idx/$valid_exceptions.len]...')
				continue
			}
			if !hide_oks {
				println('OK   [${i + 1}/$valid_test_files.len] "$valid_test_file"...')
			}
			toml_doc := toml.parse_file(valid_test_file)!
			valid++
		}
		println('$valid/$valid_test_files.len TOML files were parsed correctly')
		if valid_exceptions.len > 0 {
			println('TODO Skipped parsing of $e valid TOML files...')
		}

		// If the command-line tool `jq` is installed, value tests can be run as well.
		if jq != '' {
			println('Testing value output of $valid_test_files.len valid TOML files using "$jq"...')

			if os.exists(compare_work_dir_root) {
				os.rmdir_all(compare_work_dir_root)!
			}
			os.mkdir_all(compare_work_dir_root)!

			jq_normalize_path := os.join_path(compare_work_dir_root, 'normalize.jq')
			os.write_file(jq_normalize_path, jq_normalize)!

			valid = 0
			e = 0
			for i, valid_test_file in valid_test_files {
				mut relative := valid_test_file.all_after(tests_folder).trim_left(os.path_separator)
				$if windows {
					relative = relative.replace('/', '\\')
				}
				if !os.exists(valid_test_file.all_before_last('.') + '.json') {
					println('N/A  [${i + 1}/$valid_test_files.len] "$valid_test_file"...')
					continue
				}
				// Skip the file if we know it can't be parsed or we know that the value retrieval needs work.
				if relative in valid_exceptions {
					e++
					idx := valid_exceptions.index(relative) + 1
					println('SKIP [${i + 1}/$valid_test_files.len] "$valid_test_file" VALID EXCEPTION [$idx/$valid_exceptions.len]...')
					continue
				}
				if relative in valid_value_exceptions {
					e++
					idx := valid_value_exceptions.index(relative) + 1
					println('SKIP [${i + 1}/$valid_test_files.len] "$valid_test_file" VALID VALUE EXCEPTION [$idx/$valid_value_exceptions.len]...')
					continue
				}

				if !hide_oks {
					println('OK   [${i + 1}/$valid_test_files.len] "$valid_test_file"...')
				}
				toml_doc := toml.parse_file(valid_test_file)?

				v_toml_json_path := os.join_path(compare_work_dir_root,
					os.file_name(valid_test_file).all_before_last('.') + '.v.json')
				alexcrichton_toml_json_path := os.join_path(compare_work_dir_root,
					os.file_name(valid_test_file).all_before_last('.') + '.json')

				mut array_type := 1
				if relative in use_type_2_arrays {
					array_type = 2
				}

				os.write_file(v_toml_json_path, to_alexcrichton(toml_doc.ast.table, array_type))!

				alexcrichton_json := os.read_file(valid_test_file.all_before_last('.') + '.json')!

				os.write_file(alexcrichton_toml_json_path, alexcrichton_json)!

				v_normalized_json := run([jq, '-S', '-f "$jq_normalize_path"', v_toml_json_path]) or {
					contents := os.read_file(v_toml_json_path)!
					panic(err.msg() + '\n$contents')
				}
				alexcrichton_normalized_json := run([jq, '-S', '-f "$jq_normalize_path"',
					alexcrichton_toml_json_path]) or {
					contents := os.read_file(v_toml_json_path)!
					panic(err.msg() + '\n$contents')
				}

				assert alexcrichton_normalized_json == v_normalized_json

				valid++
			}
			println('$valid/$valid_test_files.len TOML files were parsed correctly and value checked')
			if valid_value_exceptions.len > 0 {
				println('TODO Skipped value checks of $e valid TOML files...')
			}
		}

		invalid_test_files := os.walk_ext(os.join_path(test_root, 'test-suite', 'tests',
			'invalid'), '.toml')
		println('Testing $invalid_test_files.len invalid TOML files...')
		mut invalid := 0
		e = 0
		for i, invalid_test_file in invalid_test_files {
			mut relative := invalid_test_file.all_after(tests_folder).trim_left(os.path_separator)
			$if windows {
				relative = relative.replace('/', '\\')
			}
			if relative in invalid_exceptions {
				e++
				idx := invalid_exceptions.index(relative) + 1
				println('SKIP [${i + 1}/$invalid_test_files.len] "$invalid_test_file" INVALID EXCEPTION [$idx/$invalid_exceptions.len]...')
				continue
			}

			if !hide_oks {
				println('OK   [${i + 1}/$invalid_test_files.len] "$invalid_test_file"...')
			}
			if toml_doc := toml.parse_file(invalid_test_file) {
				content_that_should_have_failed := os.read_file(invalid_test_file)!
				println('     This TOML should have failed:\n${'-'.repeat(40)}\n$content_that_should_have_failed\n${'-'.repeat(40)}')
				assert false
			} else {
				if !hide_oks {
					println('     $err.msg()')
				}
				assert true
			}
			invalid++
		}
		println('$invalid/$invalid_test_files.len TOML files were parsed correctly')
		if invalid_exceptions.len > 0 {
			println('TODO Skipped parsing of $invalid_exceptions.len invalid TOML files...')
		}
	} else {
		println('No test data directory found in "$test_root"')
		assert true
	}
}

// to_alexcrichton_time
fn to_alexcrichton_time(time_str string) string {
	if time_str.contains('.') {
		date_and_time := time_str.all_before('.')
		mut ms := time_str.all_after('.')
		z := if ms.contains('Z') { 'Z' } else { '' }
		ms = ms.replace('Z', '')
		if ms.len > 3 {
			ms = ms[..3]
		}
		return date_and_time + '.' + ms + z
	} else {
		return time_str
	}
}

// to_alexcrichton returns an alexcrichton compatible json string converted from the `value` ast.Value.
fn to_alexcrichton(value ast.Value, array_type int) string {
	match value {
		ast.Quoted {
			json_text := json2.Any(value.text).json_str()
			return '{ "type": "string", "value": $json_text }'
		}
		ast.DateTime {
			// Normalization for json
			mut json_text := json2.Any(value.text).json_str().to_upper().replace(' ',
				'T')
			typ := if json_text.ends_with('Z"') || json_text.all_after('T').contains('-')
				|| json_text.all_after('T').contains('+') {
				'datetime'
			} else {
				'datetime-local'
			}
			// NOTE test suite inconsistency.
			// It seems it's implementation specific how time and
			// date-time values are represented in detail. For now we follow the BurntSushi format
			// that expands to 6 digits which is also a valid RFC 3339 representation.
			json_text = to_alexcrichton_time(json_text[1..json_text.len - 1])
			return '{ "type": "$typ", "value": "$json_text" }'
		}
		ast.Date {
			json_text := json2.Any(value.text).json_str()
			return '{ "type": "date", "value": $json_text }'
		}
		ast.Time {
			mut json_text := json2.Any(value.text).json_str()
			json_text = to_alexcrichton_time(json_text[1..json_text.len - 1])
			return '{ "type": "time", "value": "$json_text" }'
		}
		ast.Bool {
			json_text := json2.Any(value.text.bool()).json_str()
			return '{ "type": "bool", "value": "$json_text" }'
		}
		ast.Null {
			json_text := json2.Any(value.text).json_str()
			return '{ "type": "null", "value": $json_text }'
		}
		ast.Number {
			text := value.text
			if text.contains('inf') || text.contains('nan') {
				return '{ "type": "float", "value": $value.text }'
			}
			if !text.starts_with('0x') && (text.contains('.') || text.to_lower().contains('e')) {
				mut val := ''
				if text.to_lower().contains('e') && !text.contains('-') {
					val = '${value.f64():.1f}'
				} else {
					val = '$value.f64()'
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
				str += ' $json_key: ${to_alexcrichton(val, array_type)},'
			}
			str = str.trim_right(',')
			str += ' }'
			return str
		}
		[]ast.Value {
			mut str := ''
			if array_type == 1 {
				str = '{ "type": "array", "value": [ '
			} else {
				str = '[ '
			}
			for val in value {
				str += ' ${to_alexcrichton(val, array_type)},'
			}
			str = str.trim_right(',')
			if array_type == 1 {
				str += ' ] }\n'
			} else {
				str += ' ]\n'
			}
			return str
		}
	}
	return '<error>'
}
