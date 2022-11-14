import os
import toml
import toml.ast
import x.json2

const hide_oks = os.getenv('VTEST_HIDE_OK') == '1'

// Can be set to `true` to process tests that stress test the parser
// by having large data amounts - these pass - but slow down the test run
const do_large_files = os.getenv('VTEST_TOML_DO_LARGE_FILES') == '1'

// Can be set to `true` to process tests that triggers a slow conversion
// process that uses `python` to convert from YAML to JSON.
const do_yaml_conversion = os.getenv('VTEST_TOML_DO_YAML_CONVERSION') == '1'

// Instructions for developers:
// The actual tests and data can be obtained by doing:
// `git clone --depth 1 https://github.com/iarna/toml-spec-tests.git vlib/toml/tests/testdata/iarna/toml-test`
// See also the CI toml tests
const (
	// Kept for easier handling of future updates to the tests
	valid_exceptions       = []string{}
	invalid_exceptions     = []string{}

	valid_value_exceptions = [
		'values/spec-date-time-3.toml',
		'values/spec-date-time-4.toml',
		'values/spec-readme-example.toml',
		'values/spec-date-time-6.toml',
		'values/spec-date-time-5.toml',
		'values/spec-date-time-1.toml',
		'values/spec-date-time-2.toml',
		'values/qa-table-inline-nested-1000.toml',
		'values/qa-array-inline-nested-1000.toml',
	]

	yaml_value_exceptions  = [
		'values/spec-float-5.toml', // YAML: "1e6", V: 1000000
		'values/spec-float-9.toml', // YAML: "-0e0", V: 0
		'values/spec-float-6.toml', // YAML: "-2E-2", V: -0.02
		'values/spec-float-4.toml', // YAML: "5e+22", V: 50000000000000004000000
	]

	jq                     = os.find_abs_path_of_executable('jq') or { '' }
	python                 = os.find_abs_path_of_executable('python') or { '' }
	compare_work_dir_root  = os.join_path(os.vtmp_dir(), 'v', 'toml', 'iarna')
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
		return error('${args[0]} failed with return code ${res.exit_code}.\n${res.output}')
	}
	return res.output
}

// test_iarna_toml_spec_tests run though 'testdata/iarna/toml-test/*' if found.
fn test_iarna_toml_spec_tests() {
	this_file := @FILE
	test_root := os.join_path(os.dir(this_file), 'testdata', 'iarna', 'toml-test')
	if os.is_dir(test_root) {
		valid_test_files := os.walk_ext(os.join_path(test_root, 'values'), '.toml')
		println('Testing ${valid_test_files.len} valid TOML files...')
		mut valid := 0
		mut e := 0
		for i, valid_test_file in valid_test_files {
			mut relative := valid_test_file.all_after('toml-test').trim_left(os.path_separator)
			$if windows {
				relative = relative.replace('/', '\\')
			}

			if !do_large_files && valid_test_file.contains('qa-') {
				e++
				println('SKIP [${i + 1}/${valid_test_files.len}] "${valid_test_file}" LARGE FILE...')
				continue
			}

			if relative in valid_exceptions {
				e++
				idx := valid_exceptions.index(relative) + 1
				println('SKIP [${i + 1}/${valid_test_files.len}] "${valid_test_file}" VALID EXCEPTION [${idx}/${valid_exceptions.len}]...')
				continue
			}

			if !hide_oks {
				println('OK   [${i + 1}/${valid_test_files.len}] "${valid_test_file}"...')
			}
			toml_doc := toml.parse_file(valid_test_file)?
			valid++
		}
		println('${valid}/${valid_test_files.len} TOML files were parsed correctly')
		if valid_exceptions.len > 0 {
			println('TODO Skipped parsing of ${e} valid TOML files...')
		}

		// If the command-line tool `jq` is installed, value tests can be run as well.
		if jq != '' {
			println('Testing value output of ${valid_test_files.len} valid TOML files using "${jq}"...')

			if os.exists(compare_work_dir_root) {
				os.rmdir_all(compare_work_dir_root)?
			}
			os.mkdir_all(compare_work_dir_root)?

			jq_normalize_path := os.join_path(compare_work_dir_root, 'normalize.jq')
			os.write_file(jq_normalize_path, jq_normalize)?

			valid = 0
			e = 0
			for i, valid_test_file in valid_test_files {
				mut relative := valid_test_file.all_after('toml-test').trim_left(os.path_separator)
				$if windows {
					relative = relative.replace('/', '\\')
				}

				// Skip the file if we know it can't be parsed or we know that the value retrieval needs work.
				if relative in valid_exceptions {
					e++
					idx := valid_exceptions.index(relative) + 1
					println('SKIP [${i + 1}/${valid_test_files.len}] "${valid_test_file}" VALID EXCEPTION [${idx}/${valid_exceptions.len}]...')
					continue
				}

				if relative in valid_value_exceptions {
					e++
					idx := valid_value_exceptions.index(relative) + 1
					println('SKIP [${i + 1}/${valid_test_files.len}] "${valid_test_file}" VALID VALUE EXCEPTION [${idx}/${valid_value_exceptions.len}]...')
					continue
				}

				valid_test_file_name := os.file_name(valid_test_file).all_before_last('.')
				uses_json_format := os.exists(valid_test_file.all_before_last('.') + '.json')

				// Use python to convert the YAML files to json - it yields some inconsistencies
				// so we skip some of them
				mut converted_from_yaml := false
				mut converted_json_path := ''
				if !uses_json_format {
					$if windows {
						println('N/A  [${i + 1}/${valid_test_files.len}] "${valid_test_file}"...')
						continue
					}
					if python == '' {
						println('N/A  [${i + 1}/${valid_test_files.len}] "${valid_test_file}"...')
						continue
					}
					if !do_yaml_conversion || relative in yaml_value_exceptions {
						e++
						idx := yaml_value_exceptions.index(relative) + 1
						println('SKIP [${i + 1}/${valid_test_files.len}] "${valid_test_file}" YAML VALUE EXCEPTION [${idx}/${valid_value_exceptions.len}]...')
						continue
					}

					if !do_large_files && valid_test_file.contains('qa-') {
						e++
						println('SKIP [${i + 1}/${valid_test_files.len}] "${valid_test_file}" LARGE FILE...')
						continue
					}

					iarna_yaml_path := valid_test_file.all_before_last('.') + '.yaml'
					if os.exists(iarna_yaml_path) {
						converted_json_path = os.join_path(compare_work_dir_root, '${valid_test_file_name}.yaml.json')
						run([python, '-c',
							"'import sys, yaml, json; json.dump(yaml.load(sys.stdin, Loader=yaml.FullLoader), sys.stdout, indent=4)'",
							'<', iarna_yaml_path, '>', converted_json_path]) or {
							contents := os.read_file(iarna_yaml_path)?
							// NOTE there's known errors with the python convertion method.
							// For now we just ignore them as it's a broken tool - not a wrong test-case.
							// Uncomment this print to see/check them.
							// eprintln(err.msg() + '\n$contents')
							e++
							println('ERR  [${i + 1}/${valid_test_files.len}] "${valid_test_file}" EXCEPTION [${e}/${valid_value_exceptions.len}]...')
							continue
						}
						converted_from_yaml = true
					}
				}

				if !hide_oks {
					println('OK   [${i + 1}/${valid_test_files.len}] "${valid_test_file}"...')
				}
				toml_doc := toml.parse_file(valid_test_file)?

				v_toml_json_path := os.join_path(compare_work_dir_root, '${valid_test_file_name}.v.json')
				iarna_toml_json_path := os.join_path(compare_work_dir_root, '${valid_test_file_name}.json')

				os.write_file(v_toml_json_path, to_iarna(toml_doc.ast.table, converted_from_yaml))?

				if converted_json_path == '' {
					converted_json_path = valid_test_file.all_before_last('.') + '.json'
				}
				iarna_json := os.read_file(converted_json_path)?
				os.write_file(iarna_toml_json_path, iarna_json)?

				v_normalized_json := run([jq, '-S', '-f "${jq_normalize_path}"', v_toml_json_path]) or {
					contents := os.read_file(v_toml_json_path)?
					panic(err.msg() + '\n${contents}')
				}
				cmd := [jq, '-S', '-f "${jq_normalize_path}"', iarna_toml_json_path]
				iarna_normalized_json := run(cmd) or {
					contents := os.read_file(v_toml_json_path)?
					panic(err.msg() + '\n${contents}\n\ncmd: ${cmd.join(' ')}')
				}

				assert iarna_normalized_json == v_normalized_json

				valid++
			}
			println('${valid}/${valid_test_files.len} TOML files were parsed correctly and value checked')
			if valid_value_exceptions.len > 0 {
				println('TODO Skipped value checks of ${e} valid TOML files...')
			}
		}

		invalid_test_files := os.walk_ext(os.join_path(test_root, 'errors'), '.toml')
		println('Testing ${invalid_test_files.len} invalid TOML files...')
		mut invalid := 0
		e = 0
		for i, invalid_test_file in invalid_test_files {
			mut relative := invalid_test_file.all_after('toml-test').trim_left(os.path_separator)
			$if windows {
				relative = relative.replace('/', '\\')
			}
			if relative in invalid_exceptions {
				e++
				idx := invalid_exceptions.index(relative) + 1
				println('SKIP [${i + 1}/${invalid_test_files.len}] "${invalid_test_file}" INVALID EXCEPTION [${idx}/${invalid_exceptions.len}]...')
				continue
			}
			if !hide_oks {
				println('OK   [${i + 1}/${invalid_test_files.len}] "${invalid_test_file}"...')
			}
			if toml_doc := toml.parse_file(invalid_test_file) {
				content_that_should_have_failed := os.read_file(invalid_test_file)?
				println('     This TOML should have failed:\n${'-'.repeat(40)}\n${content_that_should_have_failed}\n${'-'.repeat(40)}')
				assert false
			} else {
				if !hide_oks {
					println('     ${err.msg()}')
				}
				assert true
			}
			invalid++
		}
		println('${invalid}/${invalid_test_files.len} TOML files were parsed correctly')
		if invalid_exceptions.len > 0 {
			println('TODO Skipped parsing of ${invalid_exceptions.len} invalid TOML files...')
		}
	} else {
		println('No test data directory found in "${test_root}"')
		assert true
	}
}

// to_iarna_time
fn to_iarna_time(time_str string) string {
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
		return time_str + '.000'
	}
}

// to_iarna returns a iarna compatible json string converted from the `value` ast.Value.
fn to_iarna(value ast.Value, skip_value_map bool) string {
	match value {
		ast.Quoted {
			json_text := json2.Any(value.text).json_str()
			if skip_value_map {
				return json_text
			}
			return '{ "type": "string", "value": ${json_text} }'
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
			json_text = to_iarna_time(json_text[1..json_text.len - 1])
			if skip_value_map {
				return json_text
			}
			return '{ "type": "${typ}", "value": "${json_text}" }'
		}
		ast.Date {
			json_text := json2.Any(value.text).json_str()
			if skip_value_map {
				return json_text
			}
			return '{ "type": "date", "value": ${json_text} }'
		}
		ast.Time {
			mut json_text := json2.Any(value.text).json_str()
			// Note: Removes the quotes of the encoded JSON string - Ned
			json_text = to_iarna_time(json_text[1..json_text.len - 1])
			if skip_value_map {
				return json_text
			}
			return '{ "type": "time", "value": "${json_text}" }'
		}
		ast.Bool {
			json_text := json2.Any(value.text.bool()).json_str()
			if skip_value_map {
				return json_text
			}
			return '{ "type": "bool", "value": "${json_text}" }'
		}
		ast.Null {
			json_text := json2.Any(value.text).json_str()
			if skip_value_map {
				return json_text
			}
			return '{ "type": "null", "value": ${json_text} }'
		}
		ast.Number {
			if value.text.contains('inf') {
				mut json_text := value.text.replace('inf', '1.7976931348623157e+308') // Inconsistency ???
				if skip_value_map {
					return '${json_text}'
				}
				return '{ "type": "float", "value": "${json_text}" }'
			}
			if value.text.contains('nan') {
				mut json_text := 'null'
				if skip_value_map {
					return '${json_text}'
				}
				return '{ "type": "float", "value": "${json_text}" }'
			}
			if !value.text.starts_with('0x')
				&& (value.text.contains('.') || value.text.to_lower().contains('e')) {
				mut val := '${value.f64()}'.replace('.e+', '.0e') // json notation
				if !val.contains('.') && val != '0' { // json notation
					val += '.0'
				}
				if skip_value_map {
					return '${val}'
				}
				return '{ "type": "float", "value": "${val}" }'
			}
			v := value.i64()
			// TODO workaround https://github.com/vlang/v/issues/9507
			if v == i64(-9223372036854775807 - 1) {
				if skip_value_map {
					return '-9223372036854775808'
				}
				return '{ "type": "integer", "value": "-9223372036854775808" }'
			}
			if skip_value_map {
				return '${v}'
			}
			return '{ "type": "integer", "value": "${v}" }'
		}
		map[string]ast.Value {
			mut str := '{ '
			for key, val in value {
				json_key := json2.Any(key).json_str()
				str += ' ${json_key}: ${to_iarna(val, skip_value_map)},'
			}
			str = str.trim_right(',')
			str += ' }'
			return str
		}
		[]ast.Value {
			mut str := '[ '
			for val in value {
				str += ' ${to_iarna(val, skip_value_map)},'
			}
			str = str.trim_right(',')
			str += ' ]\n'
			return str
		}
	}
	return '<error>'
}
