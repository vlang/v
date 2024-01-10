import toml
import strconv

fn test_string() {
	str_value := 'test string'
	toml_txt := 'string = "test string"'
	toml_doc := toml.parse_text(toml_txt) or { panic(err) }

	value := toml_doc.value('string')
	assert value == toml.Any(str_value)
	assert value as string == str_value
	assert value.string() == str_value
}

fn test_i64() {
	toml_txt := 'i64 = 120'
	toml_doc := toml.parse_text(toml_txt) or { panic(err) }

	value := toml_doc.value('i64')
	assert value == toml.Any(i64(120))
	assert value as i64 == 120
	assert value.i64() == i64(120)
}

fn test_bool() {
	toml_txt := '
bool_true = true
bool_false = false'
	toml_doc := toml.parse_text(toml_txt) or { panic(err) }

	value_true := toml_doc.value('bool_true')
	assert value_true == toml.Any(true)
	assert value_true as bool == true
	assert value_true != toml.Any(false)
	assert value_true as bool != false
	assert value_true.bool() == true

	value_false := toml_doc.value('bool_false')
	assert value_false == toml.Any(false)
	assert value_false as bool == false
	assert value_false != toml.Any(true)
	assert value_false as bool != true
	assert value_false.bool() == false
}

fn test_bool_key_is_not_value() {
	toml_txt := 'true = true
false = false'
	toml_doc := toml.parse_text(toml_txt) or { panic(err) }

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
	toml_doc := toml.parse_text(toml_txt) or { panic(err) }

	value := toml_doc.value('v.open_sourced').string()
	assert value == 'Jun 22 2019 20:20:28'
}

fn test_hex_values() {
	// Regression test
	// '0xb' is carefully chosen to include the 'b' character that also denotes binary via 0b prefix.
	toml_txt := 'hex = 0xb'
	toml_doc := toml.parse_text(toml_txt) or { panic(err) }

	value := toml_doc.value('hex')
	assert value as i64 == 11
	assert value.i64() == 11
}

fn test_comment_as_last_value() {
	toml_txt := '
test = 42
# this line has comment as last thing'
	toml_doc := toml.parse_text(toml_txt) or { panic(err) }

	value := toml_doc.value('test')
	assert value as i64 == 42
	assert value.i64() == 42
}

fn test_nan_and_inf_values() {
	mut toml_doc := toml.parse_text('nan = nan') or { panic(err) }
	mut value := toml_doc.value('nan')
	assert value.string() == 'nan'

	toml_doc = toml.parse_text('nan = nan#comment') or { panic(err) }
	value = toml_doc.value('nan')
	assert value.string() == 'nan'

	toml_doc = toml.parse_text('nan = -nan') or { panic(err) }
	value = toml_doc.value('nan')
	assert value.string() == 'nan'

	toml_doc = toml.parse_text('nan = +nan') or { panic(err) }
	value = toml_doc.value('nan')
	assert value.string() == 'nan'

	toml_doc = toml.parse_text('inf = inf') or { panic(err) }
	value = toml_doc.value('inf')
	assert value.u64() == strconv.double_plus_infinity

	toml_doc = toml.parse_text('inf = +inf') or { panic(err) }
	value = toml_doc.value('inf')
	assert value.u64() == strconv.double_plus_infinity

	toml_doc = toml.parse_text('inf = -inf') or { panic(err) }
	value = toml_doc.value('inf')
	assert value.u64() == strconv.double_minus_infinity
}
