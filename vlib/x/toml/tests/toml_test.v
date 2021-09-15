import os
import x.toml

// TODO Goal: Complete text from the example in the README.md:
// https://github.com/toml-lang/toml/blob/3b11f6921da7b6f5db37af039aa021fee450c091/README.md#Example
const toml_text = '# Test TOML file

title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00#-08:00 # First class dates

[database]
server = "192.168.1.1"
ports = [ 8000, 8001, 8002 ]
connection_max = 5000
enabled = true'

fn test_parse_file() {
	out_path := os.join_path(os.temp_dir(), 'v_toml_tests')
	test_file := os.join_path(out_path, 'toml_parse_file_test_1.toml')
	os.mkdir_all(out_path) or { assert false }
	os.write_file(test_file, toml_text) or { assert false }
	toml_doc := toml.parse_file(test_file)

	title := toml_doc.value('title')
	assert title == toml.Any('TOML Example')
	assert title as string == 'TOML Example'

	// TODO make the following pass (by converting ast.Date* types)
	// owner := toml_doc.value('owner') as map[string]toml.Any

	database := toml_doc.value('database') as map[string]toml.Any
	assert database['server'] as string == '192.168.1.1'

	assert toml_doc.value('owner.name') as string == 'Tom Preston-Werner'

	assert toml_doc.value('database.server') as string == '192.168.1.1'

	database_ports := toml_doc.value('database.ports') as []toml.Any
	assert database_ports[0] as i64 == 8000
	assert database_ports[1] as i64 == 8001
	assert database_ports[2] as i64 == 8002

	assert toml_doc.value('database.connection_max') as i64 == 5000
	assert toml_doc.value('database.enabled') as bool == true

	// NOTE Kept for easier debugging:
	// dump(toml_doc.ast)
	// assert false
}

fn test_parse_text() {
	toml_doc := toml.parse_text(toml_text)
	value := toml_doc.value('title')
	assert value == toml.Any('TOML Example')
	assert value as string == 'TOML Example'
}

fn test_i64() {
	toml_txt := 'i64 = 120'
	toml_doc := toml.parse(toml_txt)

	value := toml_doc.value('i64')
	assert value == toml.Any(i64(120))
	assert value as i64 == 120
}

fn test_bool() {
	toml_txt := '
bool_true = true
bool_false = false'
	toml_doc := toml.parse(toml_txt)

	value_true := toml_doc.value('bool_true')
	assert value_true == toml.Any(true)
	assert value_true as bool == true
	assert value_true != toml.Any(false)
	assert value_true as bool != false

	value_false := toml_doc.value('bool_false')
	assert value_false == toml.Any(false)
	assert value_false as bool == false
	assert value_false != toml.Any(true)
	assert value_false as bool != true
}

fn test_bool_key_is_not_value() {
	toml_txt := 'true = true
false = false'
	toml_doc := toml.parse(toml_txt)

	value_true := toml_doc.value('true')
	assert value_true == toml.Any(true)
	assert value_true as bool == true
	assert value_true != toml.Any(false)
	assert value_true as bool != false

	value_false := toml_doc.value('false')
	assert value_false == toml.Any(false)
	assert value_false as bool == false
	assert value_false != toml.Any(true)
	assert value_false as bool != true
}

fn test_single_letter_key() {
	toml_txt := '[v]
open_sourced = "Jun 22 2019 20:20:28"'
	toml_doc := toml.parse(toml_txt)

	value := toml_doc.value('v.open_sourced').string()
	assert value == 'Jun 22 2019 20:20:28'
}
