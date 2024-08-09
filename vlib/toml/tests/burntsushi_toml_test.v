// vtest flaky: true
// vtest retry: 3

// Instructions for developers:
// The actual tests and data can be obtained by doing:
// `git clone -n https://github.com/toml-lang/toml-test.git vlib/toml/tests/testdata/burntsushi`
// `git -C vlib/toml/tests/testdata/burntsushi reset --hard f30c716
// See also the CI toml tests
import os
import toml
import toml.ast
import x.json2

const hide_oks = os.getenv('VTEST_HIDE_OK') == '1'
const no_jq = os.getenv('VNO_JQ') == '1'

// Kept for easier handling of future updates to the tests
const valid_exceptions = [
	'array/open-parent-table.toml',
	'comment/after-literal-no-ws.toml',
	'comment/everywhere.toml',
	'datetime/no-seconds.toml',
	'inline-table/newline.toml',
	'key/space.toml',
	'key/start.toml',
	'key/unicode.toml',
	'string/escapes.toml',
	'string/escape-esc.toml',
	'string/hex-escape.toml',
	'string/multiline-escaped-crlf.toml',
	'string/multiline-quotes.toml',
	'table/array-implicit-and-explicit-after.toml',
	'table/array-within-dotted.toml',
]
const jq_not_equal = [
	'do_not_remove',
	'datetime/milliseconds.toml',
	'float/zero.toml',
	'inline-table/spaces.toml',
	'spec/array-of-tables-1.toml',
	'spec/float-0.toml',
	'spec/float-2.toml',
]
const invalid_exceptions = [
	'array/only-comma-1.toml',
	'control/bare-cr.toml',
	'datetime/hour-over.toml',
	'datetime/mday-under.toml',
	'datetime/minute-over.toml',
	'datetime/month-under.toml',
	'datetime/second-over.toml',
	'datetime/feb-29.toml',
	'datetime/feb-30.toml',
	'inline-table/duplicate-key-2.toml',
	'inline-table/duplicate-key-3.toml',
	'inline-table/overwrite-2.toml',
	'inline-table/overwrite-4.toml',
	'inline-table/overwrite-5.toml',
	'inline-table/overwrite-8.toml',
	'local-date/feb-29.toml',
	'local-date/feb-30.toml',
	'local-date/mday-under.toml',
	'local-date/month-under.toml',
	'local-datetime/feb-29.toml',
	'local-datetime/feb-30.toml',
	'local-datetime/hour-over.toml',
	'local-datetime/mday-under.toml',
	'local-datetime/minute-over.toml',
	'local-datetime/month-under.toml',
	'local-datetime/second-over.toml',
	'local-time/hour-over.toml',
	'local-time/minute-over.toml',
	'local-time/second-over.toml',
	'string/multiline-escape-space-2.toml',
	'table/duplicate-key-dotted-array.toml',
	'table/redefine-2.toml',
]
const valid_value_exceptions = [
	'do_not_remove',
]
// BUG with string interpolation of '${i64(-9223372036854775808)}') see below for workaround
//'integer/long.toml', // TODO: https://github.com/vlang/v/issues/9507

const jq = os.find_abs_path_of_executable('jq') or { '' }
const compare_work_dir_root = os.join_path(os.vtmp_dir(), 'toml_burntsushi')
// From: https://stackoverflow.com/a/38266731/1904615
const jq_normalize = r'# Apply f to composite entities recursively using keys[], and to atoms
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

fn run(args []string) !string {
	res := os.execute(args.join(' '))
	if res.exit_code != 0 {
		return error('${args[0]} failed with return code ${res.exit_code}.\n${res.output}')
	}
	return res.output
}

// test_burnt_sushi_tomltest run though 'testdata/burntsushi/toml-test/*' if found.
fn test_burnt_sushi_tomltest() {
	eprintln('> running ${@LOCATION}')
	test_root := '${@VROOT}/vlib/toml/tests/testdata/burntsushi/tests'
	if !os.is_dir(test_root) {
		println('No test data directory found in "${test_root}"')
		assert true
		return
	}
	valid_folder := 'valid'
	valid_test_files := os.walk_ext('${test_root}/valid', '.toml').map(it.replace('\\',
		'/')).sorted()
	invalid_folder := 'invalid'
	invalid_test_files := os.walk_ext('${test_root}/invalid', '.toml').map(it.replace('\\',
		'/')).sorted()

	println('\nTesting ${valid_test_files.len} valid TOML files...')
	mut valid := 0
	mut e := 0
	for i, valid_test_file in valid_test_files {
		relative := valid_test_file.all_after(valid_folder).trim_left('/')
		if relative in valid_exceptions {
			e++
			idx := valid_exceptions.index(relative) + 1
			println('SKIP [${i + 1}/${valid_test_files.len}] "${valid_test_file}" VALID EXCEPTION [${idx}/${valid_exceptions.len}]...')
			continue
		}
		// eprintln('>>> trying to parse: ${valid_test_file} | relative: $relative')
		toml_doc := toml.parse_file(valid_test_file) or {
			eprintln('>>> error while parsing: ${valid_test_file}')
			panic(err)
		}
		valid++
		if !hide_oks {
			println('OK   [${i + 1}/${valid_test_files.len}] "${valid_test_file}"...')
		}
	}
	println('${valid}/${valid_test_files.len} TOML files were parsed correctly')
	if valid_exceptions.len > 0 {
		println('TODO Skipped parsing of ${valid_exceptions.len} valid TOML files...')
	}

	// If the command-line tool `jq` is installed, value tests can be run as well.
	if jq != '' && !no_jq {
		println('\nTesting value output of ${valid_test_files.len} valid TOML files using "${jq}"...')

		if os.exists(compare_work_dir_root) {
			os.rmdir_all(compare_work_dir_root)!
		}
		os.mkdir_all(compare_work_dir_root)!

		jq_normalize_path := os.join_path(compare_work_dir_root, 'normalize.jq')
		os.write_file(jq_normalize_path, jq_normalize)!

		valid = 0
		e = 0
		for i, valid_test_file in valid_test_files {
			relative := valid_test_file.all_after(valid_folder).trim_left('/')
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

			if !hide_oks {
				println('OK   [${i + 1}/${valid_test_files.len}] "${valid_test_file}"...')
			}
			toml_doc := toml.parse_file(valid_test_file)!
			// eprintln('                relative: $relative parsed')

			v_toml_json_path := os.join_path(compare_work_dir_root,
				os.file_name(valid_test_file).all_before_last('.') + '.v.json')
			bs_toml_json_path := os.join_path(compare_work_dir_root,
				os.file_name(valid_test_file).all_before_last('.') + '.json')

			os.write_file(v_toml_json_path, to_burntsushi(toml_doc.ast.table))!

			bs_json := os.read_file(valid_test_file.all_before_last('.') + '.json')!

			os.write_file(bs_toml_json_path, bs_json)!

			v_normalized_json := run([jq, '-S', '-f "${jq_normalize_path}"', v_toml_json_path]) or {
				contents := os.read_file(v_toml_json_path)!
				panic(err.msg() + '\n${contents}')
			}
			bs_normalized_json := run([jq, '-S', '-f "${jq_normalize_path}"', bs_toml_json_path]) or {
				contents := os.read_file(v_toml_json_path)!
				panic(err.msg() + '\n${contents}')
			}

			if relative in jq_not_equal {
				e++
				eprintln('>>> skipped: relative: ${relative} in jq_not_equal, bs_normalized_json != bs_normalized_json')
				continue
			}

			if bs_normalized_json != v_normalized_json {
				e++
				eprintln('>>> error: relative: ${relative}, bs_normalized_json != bs_normalized_json')
				continue
			}

			assert bs_normalized_json == v_normalized_json
			valid++
		}
	} else {
		println('> Skipping json conversion tests, since jq: ${jq} | no_jq: ${no_jq}')
	}
	println('${valid}/${valid_test_files.len} TOML files were parsed correctly and value checked')
	if valid_value_exceptions.len > 0 {
		println('TODO Skipped value checks of ${valid_value_exceptions.len} valid TOML files...')
	}

	println('\nTesting ${invalid_test_files.len} invalid TOML files...')
	mut invalid := 0
	e = 0
	for i, invalid_test_file in invalid_test_files {
		relative := invalid_test_file.all_after(invalid_folder).trim_left('/')
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
			content_that_should_have_failed := os.read_file(invalid_test_file)!
			println('     This TOML from relative: ${relative}, invalid_test_file: ${invalid_test_file}, should have failed:\n${'-'.repeat(100)}\n${content_that_should_have_failed}\n${'-'.repeat(100)}')
			assert false
		} else {
			if !hide_oks {
				println('     ${err.msg()}')
			}
			assert true
		}
		invalid++
	}
	println('${invalid}/${invalid_test_files.len} invalid TOML files correctly had parsing errors')
	if invalid_exceptions.len > 0 {
		println('TODO Skipped parsing of ${invalid_exceptions.len} invalid TOML files...')
	}
}

// to_burntsushi returns a BurntSushi compatible json string converted from the `value` ast.Value.
fn to_burntsushi(value ast.Value) string {
	match value {
		ast.Quoted {
			json_text := json2.Any(value.text).json_str()
			return '{ "type": "string", "value": ${json_text} }'
		}
		ast.DateTime {
			// Normalization for json
			json_text := json2.Any(value.text).json_str().to_upper().replace(' ', 'T')

			// Note: Since encoding strings in JSON now automatically includes quotes,
			// I added a somewhat a workaround by adding an ending quote in order to
			// recognize properly the date time type. - Ned
			typ := if json_text.ends_with('Z"') || json_text.all_after('T').contains('-')
				|| json_text.all_after('T').contains('+') {
				'datetime'
			} else {
				'datetime-local'
			}
			return '{ "type": "${typ}", "value": ${json_text} }'
		}
		ast.Date {
			json_text := json2.Any(value.text).json_str()
			return '{ "type": "date-local", "value": ${json_text} }'
		}
		ast.Time {
			json_text := json2.Any(value.text).json_str()
			return '{ "type": "time-local", "value": ${json_text} }'
		}
		ast.Bool {
			json_text := json2.Any(value.text.bool()).json_str()
			return '{ "type": "bool", "value": "${json_text}" }'
		}
		ast.Null {
			json_text := json2.Any(value.text).json_str()
			return '{ "type": "null", "value": ${json_text} }'
		}
		ast.Number {
			if value.text.contains('inf') || value.text.contains('nan') {
				return '{ "type": "float", "value": "${value.text}" }'
			}
			if !value.text.starts_with('0x')
				&& (value.text.contains('.') || value.text.to_lower().contains('e')) {
				mut val := '${value.f64()}'.replace('.e+', '.0e') // JSON notation
				if !val.contains('.') && val != '0' { // JSON notation
					val += '.0'
				}
				// Since https://github.com/vlang/v/pull/16079 V's string conversion of a zero (0) will
				// output "0.0" for float types - the JSON test suite data, however, expects "0" for floats
				// The following is a correction for that inconsistency
				if val == '0.0' {
					val = '0'
				}
				return '{ "type": "float", "value": "${val}" }'
			}
			v := value.i64()
			// TODO: workaround https://github.com/vlang/v/issues/9507
			if v == i64(-9223372036854775807 - 1) {
				return '{ "type": "integer", "value": "-9223372036854775808" }'
			}
			return '{ "type": "integer", "value": "${v}" }'
		}
		map[string]ast.Value {
			mut str := '{ '
			for key, val in value {
				json_key := json2.Any(key).json_str()
				str += ' ${json_key}: ${to_burntsushi(val)},'
			}
			str = str.trim_right(',')
			str += ' }'
			return str
		}
		[]ast.Value {
			mut str := '[ '
			for val in value {
				str += ' ${to_burntsushi(val)},'
			}
			str = str.trim_right(',')
			str += ' ]\n'
			return str
		}
	}
	return '<error>'
}
