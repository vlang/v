module json2

import time

enum AppendKind {
	first
	second
}

struct AppendRecord {
	text     string @[json: 'body']
	number   u64
	items    []int
	optional ?string
	kind     AppendKind
	created  time.Time
	raw      string @[raw]
}

fn append_matches[T](value T, options EncoderOptions) {
	expected := encode(value, options)
	mut output := 'prefix:'.bytes()
	encode_append(value, mut output, options)
	assert output.bytestr() == 'prefix:' + expected
	retained := output.bytestr()
	output.clear()
	encode_append(value, mut output, options)
	assert output.bytestr() == expected
	assert retained == 'prefix:' + expected
}

fn test_encode_append_preserves_options_and_values() {
	for text in ['', 'plain ASCII', '"\\\n\r\t\b\f\x00', 'مرحبا é 😀', 'x'.repeat(8193)] {
		value := AppendRecord{
			text:     text
			number:   u64(18446744073709551615)
			items:    [1, -2, 3]
			optional: 'present'
			kind:     .second
			created:  time.unix(1700000000)
			raw:      '{"nested":[true,null]}'
		}
		append_matches(value, EncoderOptions{})
		append_matches(value, EncoderOptions{ escape_unicode: true, time_as_unix: true })
		append_matches(value, EncoderOptions{ prettify: true, indent_string: '--', newline_string: '\r\n' })
		append_matches(value, EncoderOptions{ prettify: true, legacy_layout: true, enum_as_int: true })
	}
	append_matches([1, 2, 3]!, EncoderOptions{})
	append_matches({
		'empty':   []string{}
		'unicode': ['é', '😀']
	}, EncoderOptions{ escape_unicode: true })
	append_matches(null, EncoderOptions{})
}

fn test_encode_append_reuses_capacity_and_grows_without_corrupting_prefix() {
	mut output := []u8{cap: 32768}
	initial := output.data
	for i in 0 .. 100 {
		output.clear()
		encode_append('message ${i}', mut output)
		assert output.bytestr() == encode('message ${i}')
		assert output.data == initial
	}
	output.clear()
	output << 'kept:'.bytes()
	large := 'é😀"\\'.repeat(10000)
	encode_append(large, mut output, escape_unicode: true)
	assert output.bytestr() == 'kept:' + encode(large, escape_unicode: true)
	encode_append(42, mut output)
	assert output.bytestr() == 'kept:' + encode(large, escape_unicode: true) + '42'
}
