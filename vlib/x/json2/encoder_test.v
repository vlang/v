import x.json2 as json
import strings

struct StructType[T] {
mut:
	val T
}

fn test_json_string_characters() {
	text := json.raw_decode(r'"\n\r\b\f\t\\\"\/"') or { '' }
	assert text.json_str() == '"\\n\\r\\b\\f\\t\\\\\\"\\/"'
}

fn test_json_escape_low_chars() {
	esc := '\u001b'
	assert esc.len == 1
	text := json.Any(esc)
	assert text.json_str() == r'"\u001b"'
}

fn test_json_string() {
	text := json.Any('te✔st')
	assert text.json_str() == r'"te\u2714st"'
	boolean := json.Any(true)
	assert boolean.json_str() == 'true'
	integer := json.Any(int(-5))
	assert integer.json_str() == '-5'
	u64integer := json.Any(u64(5000))
	assert u64integer.json_str() == '5000'
	i64integer := json.Any(i64(-17))
	assert i64integer.json_str() == '-17'
}

fn test_json_string_emoji() {
	text := json.Any('🐈')
	assert text.json_str() == r'" "'
}

fn test_json_string_non_ascii() {
	text := json.Any('ひらがな')
	assert text.json_str() == r'"\u3072\u3089\u304c\u306a"'
}

fn test_utf8_strings_are_not_modified() {
	original := '{"s":"Schilddrüsenerkrankungen"}'
	deresult := json.raw_decode(original)!
	assert deresult.str() == original
}

fn test_encoder_unescaped_utf32() ! {
	jap_text := json.Any('ひらがな')
	enc := json.Encoder{
		escape_unicode: false
	}

	mut sb := strings.new_builder(20)
	defer {
		unsafe { sb.free() }
	}
	enc.encode_value(jap_text, mut sb)!

	assert sb.str() == '"${jap_text}"'
	sb.go_back_to(0)

	emoji_text := json.Any('🐈')
	enc.encode_value(emoji_text, mut sb)!
	assert sb.str() == '"${emoji_text}"'
}

fn test_encoder_prettify() {
	obj := {
		'hello': json.Any('world')
		'arr':   [json.Any('im a string'), [json.Any('3rd level')]]
		'obj':   {
			'map': json.Any('map inside a map')
		}
	}
	enc := json.Encoder{
		newline: `\n`
		newline_spaces_count: 2
	}
	mut sb := strings.new_builder(20)
	defer {
		unsafe { sb.free() }
	}
	enc.encode_value(obj, mut sb)!
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

pub struct Test {
	val string
}

fn test_encode_struct() {
	enc := json.encode(Test{'hello!'})
	assert enc == '{"val":"hello!"}'
}

pub struct Uri {
	protocol string
	path     string
}

pub fn (u Uri) json_str() string {
	return '"${u.protocol}://${u.path}"'
}

fn test_encode_encodable() {
	assert json.encode(Uri{'file', 'path/to/file'}) == '"file://path/to/file"'
}

fn test_encode_array() {
	array_of_struct := [StructType[[]bool]{
		val: [false, true]
	}, StructType[[]bool]{
		val: [true, false]
	}]

	assert json.encode([1, 2, 3]) == '[1,2,3]'

	assert json.encode(array_of_struct) == '[{"val":[false,true]},{"val":[true,false]}]'
}

fn test_encode_simple() {
	assert json.encode('hello!') == '"hello!"'
	assert json.encode(1) == '1'
}
