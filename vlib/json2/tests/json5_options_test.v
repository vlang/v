import json2
import math

struct DialectFields {
	name string @[json: 'displayName']
	port int = 8080
	omit ?int @[omitempty]
}

fn test_json5_syntax_is_opt_in() {
	for text in ['{a: 1}', "'value'", '[1,]', '// comment\n1', '0x10', '+1', '.5', '5.', 'Infinity',
		'NaN'] {
		json2.decode[json2.Any](text) or {
			_ = json2.decode[json2.Any](text, json5: true)!
			continue
		}
		assert false, 'JSON accepted JSON5-only syntax: ${text}'
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
