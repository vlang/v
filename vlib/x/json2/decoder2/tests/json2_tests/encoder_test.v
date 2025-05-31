import x.json2.decoder2 as json
import x.json2
import strings
import time

struct StructType[T] {
mut:
	val T
}

fn test_json_string_characters() {
	assert json2.encode([u8(`/`)].bytestr()).bytes() == r'"\/"'.bytes()
	assert json2.encode([u8(`\\`)].bytestr()).bytes() == r'"\\"'.bytes()
	assert json2.encode([u8(`"`)].bytestr()).bytes() == r'"\""'.bytes()
	assert json2.encode([u8(`\n`)].bytestr()).bytes() == r'"\n"'.bytes()
	assert json2.encode(r'\n\r') == r'"\\n\\r"'
	assert json2.encode('\\n') == r'"\\n"'
	assert json2.encode(r'\n\r\b') == r'"\\n\\r\\b"'
	assert json2.encode(r'\"/').bytes() == r'"\\\"\/"'.bytes()

	assert json2.encode(r'\n\r\b\f\t\\\"\/') == r'"\\n\\r\\b\\f\\t\\\\\\\"\\\/"'

	assert json2.encode("fn main(){nprintln('Hello World! Helo \$a')\n}") == '"fn main(){nprintln(\'Hello World! Helo \$a\')\\n}"'
	assert json2.encode(' And when "\'s are in the string, along with # "') == '" And when \\"\'s are in the string, along with # \\""'
	assert json2.encode('a \\\nb') == r'"a \\\nb"'
	assert json2.encode('Name\tJosé\nLocation\tSF.') == '"Name\\tJosé\\nLocation\\tSF."'
}

fn test_json_escape_low_chars() {
	esc := '\u001b'
	assert esc.len == 1
	text := json2.Any(esc)
	assert text.json_str() == r'"\u001b"'

	assert json2.encode('\u000f') == r'"\u000f"'
	assert json2.encode('\u0020') == r'" "'
	assert json2.encode('\u0000') == r'"\u0000"'
}

fn test_json_string() {
	text := json2.Any('te✔st')

	assert text.json_str() == r'"te\u2714st"'
	assert json2.encode('te✔st') == r'"te\u2714st"'

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
	assert text.json_str() == r'"🐈"'
	assert json2.Any('💀').json_str() == r'"💀"'

	assert json2.encode('🐈') == r'"🐈"'
	assert json2.encode('💀') == r'"💀"'
	assert json2.encode('🐈💀') == r'"🐈💀"'
}

fn test_json_string_non_ascii() {
	text := json2.Any('ひらがな')
	assert text.json_str() == r'"\u3072\u3089\u304c\u306a"'

	assert json2.encode('ひらがな') == r'"\u3072\u3089\u304c\u306a"'
}

fn test_utf8_strings_are_not_modified() {
	original := '{"s":"Schilddrüsenerkrankungen"}'
	deresult := json.decode[json2.Any](original)!
	assert deresult.str() == original

	assert json2.encode('ü') == '"ü"'
	assert json2.encode('Schilddrüsenerkrankungen') == '"Schilddrüsenerkrankungen"'
}

fn test_encoder_unescaped_utf32() ! {
	jap_text := json2.Any('ひらがな')
	enc := json2.Encoder{
		escape_unicode: false
	}

	mut sb := strings.new_builder(20)
	defer {
		unsafe { sb.free() }
	}

	enc.encode_value(jap_text, mut sb)!

	assert sb.str() == '"${jap_text}"'
	sb.go_back_to(0)

	emoji_text := json2.Any('🐈')
	enc.encode_value(emoji_text, mut sb)!
	assert sb.str() == '"${emoji_text}"'

	mut buf := []u8{cap: 14}

	enc.encode_value('ひらがな', mut buf)!

	assert buf.len == 14
	assert buf.bytestr() == '"ひらがな"'
}

fn test_encoder_prettify() {
	obj := {
		'hello': json2.Any('world')
		'arr':   [json2.Any('im a string'), [json2.Any('3rd level')]]
		'obj':   {
			'map': json2.Any('map inside a map')
		}
	}
	enc := json2.Encoder{
		newline:              `\n`
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
	enc := json2.encode(Test{'hello!'})
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
	assert json2.encode(Uri{'file', 'path/to/file'}) == '"file://path/to/file"'
}

fn test_encode_array() {
	array_of_struct := [StructType[[]bool]{
		val: [false, true]
	}, StructType[[]bool]{
		val: [true, false]
	}]

	assert json2.encode([1, 2, 3]) == '[1,2,3]'

	assert json2.encode(array_of_struct) == '[{"val":[false,true]},{"val":[true,false]}]'
}

fn test_encode_simple() {
	assert json2.encode('hello!') == '"hello!"'
	assert json2.encode(1) == '1'
}

fn test_encode_value() {
	json_enc := json2.Encoder{
		newline:              `\n`
		newline_spaces_count: 2
		escape_unicode:       false
	}

	mut manifest := map[string]json2.Any{}

	manifest['server_path'] = json2.Any('new_path')
	manifest['last_updated'] = json2.Any('timestamp.format_ss()')
	manifest['from_source'] = json2.Any('from_source')

	mut sb := strings.new_builder(64)
	mut buffer := []u8{}
	json_enc.encode_value(manifest, mut buffer)!

	assert buffer.len > 0
	assert buffer == [u8(123), 10, 32, 32, 34, 115, 101, 114, 118, 101, 114, 95, 112, 97, 116,
		104, 34, 58, 32, 34, 110, 101, 119, 95, 112, 97, 116, 104, 34, 44, 10, 32, 32, 34, 108,
		97, 115, 116, 95, 117, 112, 100, 97, 116, 101, 100, 34, 58, 32, 34, 116, 105, 109, 101,
		115, 116, 97, 109, 112, 46, 102, 111, 114, 109, 97, 116, 95, 115, 115, 40, 41, 34, 44,
		10, 32, 32, 34, 102, 114, 111, 109, 95, 115, 111, 117, 114, 99, 101, 34, 58, 32, 34, 102,
		114, 111, 109, 95, 115, 111, 117, 114, 99, 101, 34, 10, 125]

	sb.write(buffer)!

	unsafe { buffer.free() }

	assert sb.str() == r'{
  "server_path": "new_path",
  "last_updated": "timestamp.format_ss()",
  "from_source": "from_source"
}'
}

fn test_encode_time() {
	assert json2.encode({
		'bro': json2.Any(time.Time{})
	}) == '{"bro":"0000-00-00T00:00:00.000Z"}'

	assert json2.encode({
		'bro': time.Time{}
	}) == '{"bro":"0000-00-00T00:00:00.000Z"}'

	assert json2.encode(time.Time{}) == '"0000-00-00T00:00:00.000Z"'
}
