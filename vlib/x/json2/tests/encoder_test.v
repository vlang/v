import x.json2 as json
import time

struct StructType[T] {
mut:
	val T
}

fn test_json_string_characters() {
	assert json.encode([u8(`/`)].bytestr()).bytes() == r'"/"'.bytes()
	assert json.encode([u8(`\\`)].bytestr()).bytes() == r'"\\"'.bytes()
	assert json.encode([u8(`"`)].bytestr()).bytes() == r'"\""'.bytes()
	assert json.encode([u8(`\n`)].bytestr()).bytes() == r'"\n"'.bytes()
	assert json.encode(r'\n\r') == r'"\\n\\r"'
	assert json.encode('\\n') == r'"\\n"'
	assert json.encode(r'\n\r\b') == r'"\\n\\r\\b"'
	assert json.encode(r'\"/').bytes() == r'"\\\"/"'.bytes()

	assert json.encode(r'\n\r\b\f\t\\\"\/') == r'"\\n\\r\\b\\f\\t\\\\\\\"\\/"'

	text := json.decode[json.Any](r'"\n\r\b\f\t\\\"\/"') or { '' }
	assert text.json_str() == '"\\n\\r\\b\\f\\t\\\\\\"/"'

	assert json.encode("fn main(){nprintln('Hello World! Helo \$a')\n}") == '"fn main(){nprintln(\'Hello World! Helo \$a\')\\n}"'
	assert json.encode(' And when "\'s are in the string, along with # "') == '" And when \\"\'s are in the string, along with # \\""'
	assert json.encode('a \\\nb') == r'"a \\\nb"'
	assert json.encode('Name\tJosé\nLocation\tSF.') == '"Name\\tJosé\\nLocation\\tSF."'
}

fn test_json_escape_low_chars() {
	esc := '\u001b'
	assert esc.len == 1
	text := json.Any(esc)
	assert text.json_str() == r'"\u001b"'

	assert json.encode('\u000f') == r'"\u000f"'
	assert json.encode('\u0020') == r'" "'
	assert json.encode('\u0000') == r'"\u0000"'
}

fn test_json_string() {
	text := json.Any('te✔st')

	assert text.json_str() == r'"te✔st"'
	assert json.encode('te✔st', escape_unicode: true) == r'"te\u2714st"'

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
	assert text.json_str() == r'"🐈"'
	assert json.Any('💀').json_str() == r'"💀"'

	assert json.encode('🐈') == r'"🐈"'
	assert json.encode('💀') == r'"💀"'
	// assert json.encode('🐈💀') == r'"🐈💀"'
}

fn test_json_string_non_ascii() {
	text := json.Any('ひらがな')
	assert text.json_str() == r'"ひらがな"'

	assert json.encode('ひらがな', escape_unicode: true) == r'"\u3072\u3089\u304c\u306a"'
}

fn test_utf8_strings_are_not_modified() {
	original := '{"s":"Schilddrüsenerkrankungen"}'
	deresult := json.decode[json.Any](original)!
	assert deresult.str() == original

	assert json.encode('ü') == '"ü"'
	assert json.encode('Schilddrüsenerkrankungen') == '"Schilddrüsenerkrankungen"'
}

fn test_encoder_unescaped_utf32() ! {
	jap_text := json.Any('ひらがな')

	assert json.encode(jap_text) == '"${jap_text}"'

	emoji_text := json.Any('🐈')
	assert json.encode(emoji_text) == '"${emoji_text}"'

	assert json.encode('ひらがな') == '"ひらがな"'
}

fn test_encoder_prettify() {
	obj := {
		'hello': json.Any('world')
		'arr':   [json.Any('im a string'), [json.Any('3rd level')]]
		'obj':   {
			'map': json.Any('map inside a map')
		}
	}
	assert json.encode(obj, prettify: true, indent_string: '  ') == '{
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

fn test_encode_value() {
	mut manifest := map[string]json.Any{}

	manifest['server_path'] = json.Any('new_path')
	manifest['last_updated'] = json.Any('timestamp.format_ss()')
	manifest['from_source'] = json.Any('from_source')

	assert json.encode(manifest, prettify: true, indent_string: '  ') == r'{
  "server_path": "new_path",
  "last_updated": "timestamp.format_ss()",
  "from_source": "from_source"
}'
}

fn test_encode_time() {
	assert json.encode({
		'bro': json.Any(time.Time{})
	}) == '{"bro":"0000-00-00T00:00:00.000Z"}'

	assert json.encode({
		'bro': time.Time{}
	}) == '{"bro":"0000-00-00T00:00:00.000Z"}'

	assert json.encode(time.Time{}) == '"0000-00-00T00:00:00.000Z"'
}
