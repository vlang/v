import x.json2
import strings

fn test_json_string_characters() {
	text := json2.raw_decode(r'"\n\r\b\f\t\\\"\/"') or { '' }
	assert text.json_str() == '"\\n\\r\\b\\f\\t\\\\\\"\\/"'
}

fn test_json_escape_low_chars() {
	esc := '\u001b'
	assert esc.len == 1
	text := json2.Any(esc)
	assert text.json_str() == r'"\u001b"'
}

fn test_json_string() {
	text := json2.Any('te✔st')
	assert text.json_str() == r'"te\u2714st"'
	boolean := json2.Any(true)
	assert boolean.json_str() == 'true'
	integer := json2.Any(int(-5))
	assert integer.json_str() == '-5'
	u64integer := json2.Any(u64(5000))
	assert u64integer.json_str() == '5000'
	i64integer := json2.Any(i64(-17))
	assert i64integer.json_str() == '-17'
}

fn test_json_string_emoji() {
	text := json2.Any('🐈')
	assert text.json_str() == r'" "'
}

fn test_json_string_non_ascii() {
	text := json2.Any('ひらがな')
	assert text.json_str() == r'"\u3072\u3089\u304c\u306a"'
}

fn test_utf8_strings_are_not_modified() ? {
	original := '{"s":"Schilddrüsenerkrankungen"}'
	// dump(original)
	deresult := json2.raw_decode(original) ?
	// dump(deresult)
	assert deresult.str() == original
}

fn test_encoder_unescaped_utf32() ? {
	jap_text := json2.Any('ひらがな')
	enc := json2.Encoder{
		escape_unicode: false
	}

	mut sb := strings.new_builder(20)
	enc.encode_value(jap_text, mut sb) ?

	assert sb.str() == '"$jap_text"'
	sb.go_back_to(0)

	emoji_text := json2.Any('🐈')
	enc.encode_value(emoji_text, mut sb) ?
	assert sb.str() == '"$emoji_text"'
}

fn test_encoder_prettify() ? {
	obj := {
		'hello': json2.Any('world')
		'arr':   [json2.Any('im a string'), [json2.Any('3rd level')]]
		'obj':   {
			'map': json2.Any('map inside a map')
		}
	}
	enc := json2.Encoder{
		newline: `\n`
		newline_spaces_count: 2
	}
	mut sb := strings.new_builder(20)
	enc.encode_value(obj, mut sb) ?
	assert sb.str() == '{
  "hello": "world",
  "arr": [
    "im a string",
    [
      "3rd level"
    ]
  ],
  "obj": {
    "map": "map inside a map"
  }
}'
}
