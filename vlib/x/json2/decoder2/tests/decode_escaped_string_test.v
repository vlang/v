import x.json2
import x.json2.decoder2

fn test_decode_escaped_string() {
	escaped_strings := ['test', 'test\\sd', 'test\nsd', '\ntest', 'test\\"', 'test\\', 'test\u1234ps',
		'test\u1234', '\u1234\\\t"', '']

	json_string := json2.encode[[]string](escaped_strings)
	decoded_strings := decoder2.decode[[]string](json_string)!

	assert escaped_strings == decoded_strings
}
