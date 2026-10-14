import json2
import math

struct DialectFields {
	name string @[json: 'displayName']
	port int = 8080
	omit ?int @[omitempty]
}

fn test_json5_syntax_is_opt_in() {
	for text in ['{a: 1}', r'{\u0061: 1}', "'value'", '[1,]', '// comment\n1', '0x10', '+1', '.5',
		'5.', 'Infinity', 'NaN'] {
		json2.decode[json2.Any](text) or {
			_ = json2.decode[json2.Any](text, json5: true)!
			continue
		}
		assert false, 'JSON accepted JSON5-only syntax: ${text}'
	}
}

fn test_json5_decode_unicode_escapes_in_unquoted_keys() {
	fields := json2.decode[DialectFields](r'{\u0064isplay\u004eame: "V", \u0070ort: 42}',
		json5: true
	)!
	assert fields.name == 'V'
	assert fields.port == 42
	values := json2.decode[map[string]int](r'{\u0061:1, a\u0030:2, \u00e9:3, \u0024:4,
		\u005f:5, \u0074rue:6, f\u0061lse:7, \u006eull:8, \u0049nfinity:9, N\u0061N:10,
		a\u0301:11, a\u200c:12, a\u200d:13}', json5: true)!
	assert values == {
		'a':        1
		'a0':       2
		'é':        3
		'$':        4
		'_':        5
		'true':     6
		'false':    7
		'null':     8
		'Infinity': 9
		'NaN':      10
		'a\u0301':  11
		'a\u200c':  12
		'a\u200d':  13
	}
	doc := json2.parse_text(r'{a: 1, \u0061: 2}', json5: true)!
	assert doc.value('a').int() == 2
}

fn test_json5_decode_rejects_escaped_value_keywords() {
	for text in [r'\u0074rue', r'f\u0061lse', r'\u006eull', r'Inf\u0069nity', r'+Inf\u0069nity',
		r'N\u0061N', r'-N\u0061N'] {
		json2.decode[json2.Any](text, json5: true) or {
			assert err is json2.JsonScanError
			continue
		}
		assert false, 'expected an escaped value keyword to fail: ${text}'
	}
}

fn test_json5_decode_rejects_numeric_and_signed_object_keys() {
	for key in ['0', '42', '0x10', '0XAB', '1.25', '.5', '5.', '1e2', '+42', '-42', '+0x10', '-0x10',
		'+.5', '-.5', '+Infinity', '-Infinity', '+NaN', '-NaN', '+Invalid', '-NaNother'] {
		for text in ['{' + key + ':1}', '{before:0,' + key + ':1}', '{nested:{' + key + ':1}}',
			'[{' + key + ':1}]'] {
			json2.decode[json2.Any](text, json5: true) or {
				assert err is json2.JsonScanError
				assert err.msg().contains('expected an object key')
				continue
			}
			assert false, 'expected invalid object key to fail: ${text}'
		}
		// Quoted numeric and signed keys remain ordinary strings.
		for quote in ['"', "'"] {
			values := json2.decode[map[string]int]('{' + quote + key + quote + ':1}', json5: true)!
			assert values[key] == 1
		}
		json2.decode[map[string]int]('{' + key + ':1}', json5: true) or { continue }
		assert false, 'typed decoding accepted an invalid object key: ${key}'
	}
}

fn test_json5_decode_rejects_leading_zero_decimals() {
	for sign in ['', '+', '-'] {
		for digits in ['00', '01', '007', '08', '09', '00.5', '01.5', '00e1', '01e+2'] {
			literal := sign + digits
			for text in [literal, '[' + literal + ']', '{a:' + literal + '}'] {
				json2.decode[json2.Any](text, json5: true) or {
					assert err is json2.JsonScanError
					assert err.msg().contains('leading zero')
					continue
				}
				assert false, 'expected leading-zero decimal to fail: ${text}'
			}
		}
	}
}

fn test_json5_decode_rejects_unescaped_newlines() {
	for quote in ['"', "'"] {
		for separator in ['\n', '\r', '\r\n'] {
			literal := quote + 'a' + separator + 'b' + quote
			for text in [literal, '{name:' + literal + '}', '{' + literal + ':1}'] {
				json2.decode[json2.Any](text, json5: true) or {
					assert err is json2.JsonScanError
					assert err.msg().contains('unescaped line terminator')
					continue
				}
				assert false, 'expected an unescaped line terminator error'
			}
		}
	}
}

fn test_json5_decode_accepts_unescaped_unicode_line_separators() {
	for quote in ['"', "'"] {
		for separator in ['\u2028', '\u2029'] {
			value := 'a' + separator + 'b'
			literal := quote + value + quote
			assert json2.decode[string](literal, json5: true)! == value
			fields := json2.decode[map[string]string]('{name:' + literal + '}', json5: true)!
			assert fields['name'] == value
			keys := json2.decode[map[string]int]('{' + literal + ':1}', json5: true)!
			assert keys[value] == 1
		}
	}
}

fn test_json5_formatting_is_opt_in() {
	value := DialectFields{ name: "it's V" }
	plain := json2.encode(value)
	assert json2.encode(value, single_quotes: true, unquoted_keys: true, trailing_commas: true) == plain
	assert json2.encode(value,
		json5:           true
		single_quotes:   true
		unquoted_keys:   true
		trailing_commas: true
	) == "{displayName:'it\\'s V',port:8080,}"
	// Encoding ordinary JSON after JSON5 reuses the same field metadata safely.
	assert json2.encode(value) == plain
}

fn test_json5_append_and_fixed_arrays() {
	mut bytes := 'prefix:'.bytes()
	value := [1, 2]!
	json2.encode_append(value, mut bytes, json5: true, trailing_commas: true)
	assert bytes.bytestr() == 'prefix:[1,2,]'
	assert json2.encode(value,
		json5:           true
		prettify:        true
		indent_string:   '  '
		trailing_commas: true
	) == '[\n  1,\n  2,\n]'
	assert json2.encode([]int{}, json5: true, trailing_commas: true) == '[]'
	assert json2.encode(map[string]int{}, json5: true, trailing_commas: true) == '{}'
	assert json2.decode[[2]int](bytes[7..].bytestr(), json5: true)! == value
}

fn test_json5_integer_precision_and_signs() {
	assert json2.decode[i64]('9007199254740993', json5: true)! == i64(9007199254740993)
	assert json2.decode[i64]('0x20000000000001', json5: true)! == i64(9007199254740993)
	assert json2.decode[u64]('0xFFFFFFFFFFFFFFFF', json5: true)! == u64(18446744073709551615)
	assert json2.decode[i64]('-0x8000000000000000', json5: true)! == i64(-9223372036854775807 - 1)
	assert json2.decode[int]('-0x10', json5: true)! == -16
	assert json2.decode[int]('+0XAB', json5: true)! == 171
	assert json2.decode[f64]('1.e2', json5: true)! == 100.0
	assert json2.decode[f64]('1e3', json5: true)! == 1000.0
	assert json2.decode[f64]('.5e+2', json5: true)! == 50.0
	assert json2.decode[f64]('-.5', json5: true)! == -0.5
	assert json2.decode[f64]('-0x10', json5: true)! == -16.0
	json2.decode[u64]('-0x1', json5: true) or { return }
	assert false, 'negative hexadecimal integer accepted as unsigned'
}

fn test_json5_nonfinite_values_and_json_defaults() {
	assert math.is_inf(json2.decode[f64]('+Infinity', json5: true)!, 1)
	assert math.is_inf(json2.decode[f32]('-Infinity', json5: true)!, -1)
	assert math.is_nan(json2.decode[f64]('-NaN', json5: true)!)
	assert json2.encode(math.inf(-1), json5: true) == '-Infinity'
	assert json2.encode(math.inf(1)) == 'null'
	assert json2.encode(math.nan()) == 'null'
	assert json2.encode([math.inf(1), math.nan()], json5: true) == '[Infinity,NaN]'
	json2.decode[int]('NaN', json5: true) or { return }
	assert false, 'NaN accepted as integer'
}

fn test_json5_reuse_between_dialects_and_errors() {
	mut buffer := json2.DecodeBuffer{}
	first := json2.decode_reuse[DialectFields]("{displayName: 'first',}", mut buffer,
		json5: true
	)!
	assert first.name == 'first'
	assert first.port == 8080
	json2.decode_reuse[DialectFields]('{displayName:', mut buffer, json5: true) or {}
	second := json2.decode_reuse[DialectFields]('{"displayName":"second"}', mut buffer)!
	assert second.name == 'second'
	third := json2.decode_reuse[DialectFields]("{displayName:'third',port:0x10}", mut buffer,
		json5: true
	)!
	assert third.port == 16
	assert first.name == 'first'
	assert second.name == 'second'
}

fn test_json5_strict_type_conversion() {
	assert json2.decode[int]('0x10', json5: true, strict: true)! == 16
	json2.decode[int]("'16'", json5: true, strict: true) or { return }
	assert false, 'strict decoding accepted a quoted number'
}

fn test_document_lookup_distinguishes_null_from_absent() {
	doc := json2.parse_text('{a:null,items:[{value:3}]}', json5: true)!
	assert doc.get('a') != none
	assert doc.get('missing') == none
	assert doc.get('items[2]') == none
	assert doc.get('items[0].value') or { panic('missing value') }.int() == 3
	assert json2.parse_text('{"a":1}')!.value('a').int() == 1
}

fn test_json5_single_quotes_escape_unicode() {
	text := json2.encode("V's 🎼\n", json5: true, single_quotes: true, escape_unicode: true)
	assert text == r"'V\'s \ud83c\udfbc\n'"
	assert json2.decode[string](text, json5: true)! == "V's 🎼\n"
}

fn test_document_keeps_json5_and_strict_options() {
	doc := json2.parse_text('Infinity', json5: true)!
	assert doc.str() == 'Infinity'
	assert math.is_inf(doc.decode[f64]()!, 1)
	assert doc.value_opt('')! is f64
	null_doc := json2.parse_text('null', json5: true)!
	assert null_doc.value_opt('')! is json2.Null
	strict_doc := json2.parse_text("'12'", json5: true, strict: true)!
	strict_doc.decode[int]() or { return }
	assert false, 'document lost strict decoding options'
}

fn test_document_reflect_keeps_default_for_incompatible_field() {
	doc := json2.parse_text("{displayName:'V',port:'invalid'}", json5: true)!
	value := doc.reflect[DialectFields]()
	assert value.name == 'V'
	assert value.port == 8080
}
