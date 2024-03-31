// TODO: delete either tt_toml_test.v or burntsushi_toml_test.v
import os
import toml
import toml.ast
import x.json2

// Instructions for developers:
// The actual tests and data can be obtained by doing:
// `git clone -n https://github.com/toml-lang/toml-test.git vlib/toml/tests/testdata/tt`
// `git -C vlib/toml/tests/testdata/tt reset --hard f30c716
// See also the CI toml tests
// Kept for easier handling of future updates to the tests

const hide_oks = os.getenv('VTEST_HIDE_OK') == '1'
const no_jq = os.getenv('VNO_JQ') == '1'

const valid_exceptions = [
	'do_not_remove',
	'valid/example-v0.3.0.toml',
	'valid/example-v0.4.0.toml',
	'valid/datetime-truncate.toml', // Not considered valid since RFC 3339 doesn't permit > 6 ms digits ??
	'valid/array/open-parent-table.toml',
	'valid/string/hex-escape.toml',
	'valid/string/escape-esc.toml',
	'valid/string/multiline-quotes.toml',
	'valid/string/multiline-escaped-crlf.toml',
	'valid/table/array-implicit-and-explicit-after.toml',
	'valid/table/array-within-dotted.toml',
	'valid/datetime/no-seconds.toml',
	'valid/key/unicode.toml',
	'valid/key/space.toml',
	'valid/key/start.toml',
	'valid/comment/after-literal-no-ws.toml',
	'valid/comment/everywhere.toml',
	'valid/inline-table/newline.toml',
]
const jq_errors = [
	'do_not_remove',
	'valid/spec/float-2.toml',
	'valid/float/inf-and-nan.toml',
]
const jq_not_equals = [
	'valid/array/array-subtables.toml',
	'valid/array/array.toml',
	'valid/array/bool.toml',
	'valid/array/empty.toml',
	'valid/array/hetergeneous.toml',
	'valid/array/mixed-int-array.toml',
	'valid/array/mixed-int-float.toml',
	'valid/array/mixed-int-string.toml',
	'valid/array/mixed-string-table.toml',
	'valid/array/nested-double.toml',
	'valid/array/nested-inline-table.toml',
	'valid/array/nested.toml',
	'valid/array/nospaces.toml',
	'valid/array/string-quote-comma-2.toml',
	'valid/array/string-quote-comma.toml',
	'valid/array/string-with-comma-2.toml',
	'valid/array/string-with-comma.toml',
	'valid/array/strings.toml',
	'valid/array/table-array-string-backslash.toml',
	'valid/array/trailing-comma.toml',
	'valid/comment/tricky.toml',
	'valid/datetime/edge.toml',
	'valid/datetime/leap-year.toml',
	'valid/datetime/local-date.toml',
	'valid/datetime/local-time.toml',
	'valid/datetime/milliseconds.toml',
	'valid/example.toml',
	'valid/float/underscore.toml',
	'valid/float/zero.toml',
	'valid/inline-table/array-values.toml',
	'valid/inline-table/array.toml',
	'valid/inline-table/empty.toml',
	'valid/inline-table/inline-table.toml',
	'valid/inline-table/key-dotted.toml',
	'valid/inline-table/nest.toml',
	'valid/inline-table/spaces.toml',
	'valid/key/dotted.toml',
	'valid/spec-example-1-compact.toml',
	'valid/spec-example-1.toml',
	'valid/spec/array-0.toml',
	'valid/spec/array-1.toml',
	'valid/spec/array-of-tables-0.toml',
	'valid/spec/array-of-tables-1.toml',
	'valid/spec/array-of-tables-2.toml',
	'valid/spec/float-0.toml',
	'valid/spec/local-date-0.toml',
	'valid/spec/local-date-time-0.toml',
	'valid/spec/local-time-0.toml',
	'valid/spec/offset-date-time-0.toml',
	'valid/spec/table-7.toml',
	'valid/table/array-implicit.toml',
	'valid/table/array-many.toml',
	'valid/table/array-nest.toml',
	'valid/table/array-one.toml',
	'valid/table/array-table-array.toml',
]

const invalid_exceptions = [
	'do_not_remove',
	'invalid/control/bare-cr.toml',
	'invalid/table/redefine-2.toml',
	'invalid/table/duplicate-key-dotted-array.toml',
	'invalid/array/only-comma-1.toml',
	'invalid/string/multiline-escape-space-2.toml',
	'invalid/local-datetime/hour-over.toml',
	'invalid/local-datetime/mday-under.toml',
	'invalid/local-datetime/minute-over.toml',
	'invalid/local-datetime/feb-29.toml',
	'invalid/local-datetime/second-over.toml',
	'invalid/local-datetime/feb-30.toml',
	'invalid/local-datetime/month-under.toml',
	'invalid/datetime/hour-over.toml',
	'invalid/datetime/mday-under.toml',
	'invalid/datetime/minute-over.toml',
	'invalid/datetime/feb-29.toml',
	'invalid/datetime/feb-30.toml',
	'invalid/datetime/second-over.toml',
	'invalid/datetime/month-under.toml',
	'invalid/local-time/hour-over.toml',
	'invalid/local-time/minute-over.toml',
	'invalid/local-time/second-over.toml',
	'invalid/local-date/mday-under.toml',
	'invalid/local-date/feb-29.toml',
	'invalid/local-date/feb-30.toml',
	'invalid/local-date/month-under.toml',
	'invalid/inline-table/duplicate-key-3.toml',
	'invalid/inline-table/duplicate-key-2.toml',
	'invalid/inline-table/overwrite-2.toml',
	'invalid/inline-table/overwrite-8.toml',
	'invalid/inline-table/overwrite-4.toml',
	'invalid/inline-table/overwrite-5.toml',
]

const valid_value_exceptions = [
	'do_not_remove',
	// These have correct values, and should've passed, but the format of arrays is *mixed* in the JSON ??
	'valid/datetime-truncate.toml',
	'valid/example2.toml',
	'valid/example-v0.4.0.toml',
	'valid/example-v0.3.0.toml',
]

// These have correct values, and should've passed as-is, but the format of arrays changes in the JSON ??
// We account for that here
const use_type_2_arrays = [
	'do_not_remove',
	'valid/table-array-implicit.toml',
	'valid/table-array-many.toml',
	'valid/table-array-one.toml',
	'valid/table-array-nest.toml',
	'valid/table-array-nest-no-keys.toml',
]
const tests_folder = 'testdata/tt/tests'
const jq = os.find_abs_path_of_executable('jq') or { '' }
const compare_work_dir_root = os.join_path(os.vtmp_dir(), 'toml_tt')
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

// test_tt_toml_rs run though 'vlib/toml/tests/testdata/tt/tests/*' if found.
fn test_tt_toml_rs() {
	eprintln('> running ${@LOCATION}')
	test_root := '${@VROOT}/vlib/toml/tests/testdata/tt/tests'
	if !os.is_dir(test_root) {
		println('No test data directory found in "${test_root}"')
		return
	}
	valid_test_files := os.walk_ext('${test_root}/valid', '.toml').map(it.replace('\\',
		'/')).sorted()
	invalid_test_files := os.walk_ext('${test_root}/invalid', '.toml').map(it.replace('\\',
		'/')).sorted()

	println('\nTesting ${valid_test_files.len} valid TOML files...')
	mut valid := 0
	mut e := 0
	for i, valid_test_file in valid_test_files {
		relative := valid_test_file.all_after(tests_folder).trim_left('/')
		// eprintln('> ${@LOCATION} | i: $i | valid_test_file: $valid_test_file | relative: $relative')
		if relative in valid_exceptions {
			e++
			idx := valid_exceptions.index(relative) + 1
			println('SKIP [${i + 1:3}/${valid_test_files.len}] "${valid_test_file}" VALID EXCEPTION [${idx}/${valid_exceptions.len}]...')
			continue
		}
		toml_doc := toml.parse_file(valid_test_file)!
		if !hide_oks {
			println('OK   [${i + 1:3}/${valid_test_files.len}] "${valid_test_file}"...')
		}
		valid++
	}
	println('${valid}/${valid_test_files.len} TOML files were parsed correctly')
	if valid_exceptions.len > 0 {
		println('TODO Skipped parsing of ${e} valid TOML files...')
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
			relative := valid_test_file.all_after(tests_folder).trim_left('/')
			// eprintln('> ${@LOCATION} | i: $i | relative: $relative')
			if !os.exists(valid_test_file.all_before_last('.') + '.json') {
				println('N/A  [${i + 1:3}/${valid_test_files.len}] "${valid_test_file}"...')
				continue
			}
			// Skip the file if we know it can't be parsed or we know that the value retrieval needs work.
			if relative in valid_exceptions {
				e++
				idx := valid_exceptions.index(relative) + 1
				println('SKIP [${i + 1:3}/${valid_test_files.len}] "${valid_test_file}" VALID EXCEPTION [${idx}/${valid_exceptions.len}]...')
				continue
			}
			if relative in valid_value_exceptions {
				e++
				idx := valid_value_exceptions.index(relative) + 1
				println('SKIP [${i + 1:3}/${valid_test_files.len}] "${valid_test_file}" VALID VALUE EXCEPTION [${idx}/${valid_value_exceptions.len}]...')
				continue
			}

			if !hide_oks {
				println('OK   [${i + 1:3}/${valid_test_files.len}] "${valid_test_file}"...')
			}
			toml_doc := toml.parse_file(valid_test_file)!

			v_toml_json_path := os.join_path(compare_work_dir_root,
				os.file_name(valid_test_file).all_before_last('.') + '.v.json')
			tt_toml_json_path := os.join_path(compare_work_dir_root,
				os.file_name(valid_test_file).all_before_last('.') + '.json')

			mut array_type := 1
			if relative in use_type_2_arrays {
				array_type = 2
			}

			os.write_file(v_toml_json_path, to_tt(toml_doc.ast.table, array_type))!
			os.cp(valid_test_file.all_before_last('.') + '.json', tt_toml_json_path)!

			if relative in jq_errors {
				e++
				idx := jq_errors.index(relative) + 1
				println('SKIP [${i + 1:3}/${valid_test_files.len}] running jq with ${valid_test_file}, since ${relative} is in jq_errors at idx: [${idx:3}/${jq_errors.len}]')
				continue
			}
			if relative in jq_not_equals {
				e++
				idx := jq_not_equals.index(relative) + 1
				println('SKIP [${i + 1:3}/${valid_test_files.len}] running jq with ${valid_test_file}, since ${relative} is in jq_not_equals at idx: [${idx:3}/${jq_not_equals.len}]')
				continue
			}
			v_normalized_json := run([jq, '-S', '-f "${jq_normalize_path}"', v_toml_json_path]) or {
				contents := os.read_file(v_toml_json_path)!
				panic(err.msg() + '\n${contents}')
			}
			tt_normalized_json := run([jq, '-S', '-f "${jq_normalize_path}"', tt_toml_json_path]) or {
				contents := os.read_file(v_toml_json_path)!
				panic(err.msg() + '\n${contents}')
			}

			assert tt_normalized_json == v_normalized_json
			valid++
		}
		println('${valid}/${valid_test_files.len} TOML files were parsed correctly and value checked')
		if valid_value_exceptions.len > 0 {
			println('TODO Skipped value checks of ${e} valid TOML files...')
		}
	} else {
		println('Skipping JQ tests, no_jq: ${no_jq}, jq: ${jq}')
	}

	println('\nTesting ${invalid_test_files.len} invalid TOML files...')
	mut invalid := 0
	e = 0
	for i, invalid_test_file in invalid_test_files {
		// eprintln('> ${@LOCATION} | i: $i | invalid_test_file: $invalid_test_file')
		mut relative := invalid_test_file.all_after(tests_folder).trim_left('/')
		if relative in invalid_exceptions {
			e++
			idx := invalid_exceptions.index(relative) + 1
			println('SKIP [${i + 1:3}/${invalid_test_files.len}] "${invalid_test_file}" INVALID EXCEPTION [${idx}/${invalid_exceptions.len}]...')
			continue
		}

		if !hide_oks {
			println('OK   [${i + 1:3}/${invalid_test_files.len}] "${invalid_test_file}"...')
		}
		if toml_doc := toml.parse_file(invalid_test_file) {
			content_that_should_have_failed := os.read_file(invalid_test_file)!
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
}

// to_tt_time
fn to_tt_time(time_str string) string {
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

// to_tt returns an tt compatible json string converted from the `value` ast.Value.
fn to_tt(value ast.Value, array_type int) string {
	match value {
		ast.Quoted {
			json_text := json2.Any(value.text).json_str()
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
			json_text = to_tt_time(json_text[1..json_text.len - 1])
			return '{ "type": "${typ}", "value": "${json_text}" }'
		}
		ast.Date {
			json_text := json2.Any(value.text).json_str()
			return '{ "type": "date", "value": ${json_text} }'
		}
		ast.Time {
			mut json_text := json2.Any(value.text).json_str()
			json_text = to_tt_time(json_text[1..json_text.len - 1])
			return '{ "type": "time", "value": "${json_text}" }'
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
			text := value.text
			if text.contains('inf') || text.contains('nan') {
				return '{ "type": "float", "value": ${value.text} }'
			}
			if !text.starts_with('0x') && (text.contains('.') || text.to_lower().contains('e')) {
				mut val := ''
				if text.to_lower().contains('e') && !text.contains('-') {
					val = '${value.f64():.1f}'
				} else {
					val = '${value.f64()}'
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
				str += ' ${json_key}: ${to_tt(val, array_type)},'
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
				str += ' ${to_tt(val, array_type)},'
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
