module jsonc

import os
import json2
import math

struct Server {
	host string
	port int = 8080
	tls  bool
}

struct Options {
	compiler struct {
		out_dir string
		strict  bool
	}
	include  []string
}

struct OverflowingNumbers {
	positive f64
	negative f64
	finite   f64
	values   []f64
}

fn test_is_valid_accepts_jsonc() {
	assert is_valid('{"a": 1}')
	assert is_valid('{ /* c */ "a": 1 // t\n}')
	assert is_valid('{"a": [1, {"b": null}]}')
}

fn test_is_valid_rejects_json5_only_syntax() {
	assert !is_valid("{'a': 1}")
	assert !is_valid('{a: 1}')
	assert !is_valid('{"a": 0x10}')
	assert !is_valid('{"a": Infinity}')
	assert !is_valid('{"a": "\\v"}')
	assert !is_valid('{"a": 1,}')
}

fn test_is_valid_rejects_malformed_json() {
	// Malformed input is not valid JSONC either, even though the reason comes
	// from the JSON5 parser rather than from a dialect rule.
	assert !is_valid('{"a":')
	assert !is_valid('')
	assert !is_valid('{"a" 1}')
	assert !is_valid('/* unterminated')
}

fn test_public_api_rejects_unescaped_newlines_as_syntax_errors() {
	for quote in ['"', "'"] {
		for separator in ['\n', '\r', '\r\n'] {
			text := '{"name":' + quote + 'a' + separator + 'b' + quote + '}'
			assert !is_valid(text)
			assert violation(text) == none
			parse_text(text) or {
				assert err is json2.JsonScanError
				assert err.msg().contains('unescaped line terminator')
				continue
			}
			assert false, 'expected a syntax error'
		}
	}
}

fn test_public_api_accepts_unescaped_unicode_line_separators_in_strings() {
	for separator in ['\u2028', '\u2029'] {
		text := '{"name":"a' + separator + 'b"}'
		assert is_valid(text)
		assert violation(text) == none
		assert parse_text(text)!.value('name').str() == 'a' + separator + 'b'
		bad := '{"name":"a' + separator + 'b",clé:1}'
		v := violation(bad) or { panic('expected an unquoted key violation') }
		assert v.pos.line == 2
		assert v.pos.col == 4
		assert bad[v.pos.offset..v.pos.end_offset] == 'clé'
	}
}

fn test_is_valid_opts_honours_trailing_comma() {
	opts := ParseOpts{
		allow_trailing_comma: true
	}
	assert is_valid_opts('{"a": 1,}', opts)
	assert !is_valid_opts('{"a": 1,}', ParseOpts{})
}

fn test_parse_text_returns_a_document() {
	doc := parse_text('{"name": "srv", "port": 1}') or { panic(err) }
	assert doc.str() == '{"name":"srv","port":1}'
	assert doc.value('name').str() == 'srv'
}

fn test_parse_text_rendering_round_trips_overflowing_numbers() {
	for literal in ['1e400', '-1e400', '1E+400', '-1E+400'] {
		sign := if literal.starts_with('-') { -1 } else { 1 }
		assert is_valid(literal)
		assert violation(literal) == none
		doc := parse_text(literal)!
		assert math.is_inf(doc.root.f64(), sign)
		rendered := doc.str()
		assert is_valid(rendered), rendered
		assert math.is_inf(parse(rendered)!.f64(), sign)
		shared := json2.Doc(doc)
		assert math.is_inf(shared.decode[f64]()!, sign)
	}
}

fn test_decode_any_preserves_overflowing_numbers() {
	for literal in ['1e400', '-1e400'] {
		sign := if literal.starts_with('-') { -1 } else { 1 }
		value := parse(literal)!
		assert math.is_inf(decode[f64](literal)!, sign)
		assert math.is_inf(decode_any[f64](value)!, sign)
		assert math.is_inf(decode_any[f32](value)!, sign)
		decode_any[i64](value) or { continue }
		assert false, 'expected integer decoding to reject infinity'
	}
}

fn test_nested_overflow_survives_rendering_and_typed_tree_decoding() {
	input := r'{"positive":1e400,"negative":-1e400,"finite":42.5,"values":[1e400,-1e400,1e-400],"Infinity":"Infinity \"NaN\" \\ -Infinity","NaN":"NaN","nested":{"value":1e400}}'
	doc := parse_text(input)!
	rendered := doc.str()
	assert is_valid(rendered), rendered
	again := parse_text(rendered)!
	assert math.is_inf(again.value('positive').f64(), 1)
	assert math.is_inf(again.value('negative').f64(), -1)
	assert math.is_inf(again.value('nested.value').f64(), 1)
	assert again.value('Infinity').str() == 'Infinity "NaN" \\ -Infinity'
	assert again.value('NaN').str() == 'NaN'
	numbers := decode_any[OverflowingNumbers](doc.to_any())!
	assert math.is_inf(numbers.positive, 1)
	assert math.is_inf(numbers.negative, -1)
	assert numbers.finite == 42.5
	assert math.is_inf(numbers.values[0], 1)
	assert math.is_inf(numbers.values[1], -1)
	assert numbers.values[2] == 0.0
	shared := json2.Doc(doc)
	assert math.is_inf(shared.decode[OverflowingNumbers]()!.positive, 1)
	assert math.is_inf(shared.reflect[OverflowingNumbers]().negative, -1)
}

fn test_overflow_rendering_with_trailing_comma_option() {
	doc := parse_text_opts('{"values":[1e400,-1e400,],}', ParseOpts{
		allow_trailing_comma: true
	})!
	assert doc.str() == '{"values":[1e999,-1e999]}'
	assert is_valid(doc.str())
	assert math.is_inf(parse_text(doc.str())!.value('values[1]').f64(), -1)
}

fn test_document_rendering_of_wrapped_nonfinite_values() {
	root := json2.Any([json2.Any(math.inf(1)), json2.Any(math.inf(-1)), json2.Any(math.nan()),
		json2.Any('Infinity NaN')])
	doc := Doc(json2.new_doc(root, json5: true))
	assert doc.str() == '[1e999,-1e999,null,"Infinity NaN"]'
	assert is_valid(doc.str())
	// JSON5 document rendering retains its own dialect.
	assert json2.Doc(doc).str() == '[Infinity,-Infinity,NaN,"Infinity NaN"]'
}

fn test_parse_text_supports_path_lookup() {
	doc := parse_text('{"a": {"b": [10, 20]}}') or { panic(err) }
	assert doc.get('a.b[1]') or { panic('no such path') }.int() == 20
}

fn test_parse_text_accepts_comments() {
	doc := parse_text('{
	// the port to bind
	"port": 8080, /* and the host */
	"host": "localhost"
}') or { panic(err) }
	assert doc.value('port').int() == 8080
	assert doc.value('host').str() == 'localhost'
}

fn test_parse_returns_the_root_value() {
	value := parse('{"a": [1, 2]}') or { panic(err) }
	assert value.as_map()['a'] or { panic('no a') }.as_array().len == 2
}

fn test_parse_opts_accepts_trailing_comma() {
	opts := ParseOpts{
		allow_trailing_comma: true
	}
	value := parse_opts('{"a": [1, 2,],}', opts) or { panic(err) }
	assert value.as_map()['a'] or { panic('no a') }.as_array().len == 2
}

fn test_decode_reads_a_struct_through_comments() {
	server := decode[Server]('{
	// where to listen
	"host": "0.0.0.0",
	/* the port */
	"port": 9090
}') or { panic(err) }
	assert server.host == '0.0.0.0'
	assert server.port == 9090
	// A key that is absent leaves the field at its default.
	assert server.tls == false
}

fn test_decode_flattens_an_embedded_struct() {
	opts := decode[Options]('{
	"compiler": {
		"out_dir": "bin",
		"strict": true
	},
	"include": ["a", "b"]
}') or { panic(err) }
	assert opts.compiler.out_dir == 'bin'
	assert opts.compiler.strict
	assert opts.include == ['a', 'b']
}

fn test_decode_enforces_the_dialect_first() {
	// An unquoted key is refused before the decoder is asked to do anything,
	// even though the struct field it names exists.
	server := decode[Server]('{host: "localhost"}') or { return }
	assert false, 'expected a rejection, got ${server.host}'
}

fn test_decode_any_converts_a_parsed_tree() {
	doc := parse_text('{"host": "h", "port": 1}') or { panic(err) }
	server := decode_any[Server](doc.to_any()) or { panic(err) }
	assert server.host == 'h'
	assert server.port == 1
}

fn test_errors_from_the_decoder_come_from_json2() {
	text := '{"port": "abc"}'
	mut expected := ''
	json2.decode[Server](text) or { expected = err.msg() }
	assert expected != ''
	decode[Server](text) or {
		assert err.msg() == expected
		return
	}
	assert false, 'expected a type error'
}

// ch returns `code` as a one-character string, so a test can name a code point
// without depending on how the source file encodes it.
fn ch(code int) string {
	return rune(code).str()
}

fn test_public_api_rejects_incomplete_numbers() {
	// Through the public entry points, not only the internal pass.
	for bad in ['1.e2', '-5.E+2', '01', '00.5', '1.', '1e', '00'] {
		assert !is_valid('{"a": ' + bad + '}'), 'expected `${bad}` to be refused'
	}
	// And the valid spellings stay valid.
	for good in ['0', '0.5', '1e2', '-0', '0e0', '10', '1.5e-3'] {
		assert is_valid('{"a": ' + good + '}'), 'expected `${good}` to be accepted'
	}
}

fn test_public_api_rejects_json5_only_whitespace() {
	for code in [0x0B, 0x0C, 0xA0, 0x202F, 0x205F] {
		assert !is_valid('{"a"' + ch(code) + ': 1}'), 'expected U+${code:04X} to be refused'
	}
	// The same characters are fine where they are not between tokens.
	assert is_valid(ch(0xFEFF) + '{"a": 1}')
	assert is_valid('{/*' + ch(0xA0) + '*/"a": 1}')
	assert is_valid('{"a": "' + ch(0xA0) + '"}')
}

fn test_trailing_comma_option_relaxes_nothing_else() {
	// Allowing a trailing comma must not open any of the other rules.
	opts := ParseOpts{
		allow_trailing_comma: true
	}
	for bad in ['{"a": 1.e2,}', '{"a": 01,}', '{a: 1,}', '{"a": 0x10,}', '{"a": NaN,}', '{"a": 1e,}'] {
		assert !is_valid_opts(bad, opts), 'expected `${bad}` to be refused'
	}
	// And a non-RFC space is still refused with the option on.
	assert !is_valid_opts('{"a"' + ch(0xA0) + ': 1,}', opts)
	// Only the comma itself is accepted.
	assert is_valid_opts('{"a": [1, 2,],}', opts)
}

fn test_public_api_byte_range_is_a_usable_slice() {
	// The advertised range has to be safe to slice the original text with, which
	// means it must not end inside a multibyte character.
	text := '{é:1}'
	if v := violation(text) {
		assert v.pos.offset == 1
		assert v.pos.end_offset == 3
		assert text[v.pos.offset..v.pos.end_offset] == 'é'
		return
	}
	assert false, 'expected an unquoted multibyte key to be reported'
}

fn test_public_api_token_positions_after_carriage_returns() {
	for separator in ['\n', '\r', '\r\n'] {
		for indent in ['', '  '] {
			text := '// heading' + separator + '{' + separator + indent + 'clé: 1}'
			start := text.index('clé') or { panic('missing key') }
			v := violation(text) or { panic('expected an unquoted key violation') }
			assert v.pos.line == 3
			assert v.pos.col == indent.len + 1
			assert v.pos.offset == start
			assert v.pos.end_offset == start + 'clé'.len
			assert v.pos.end_offset <= text.len
			assert text[v.pos.offset..v.pos.end_offset] == 'clé'
			parse_text(text) or {
				assert err is ParseError
				assert err.msg() == v.msg()
				continue
			}
			assert false, 'expected a JSONC dialect violation'
		}
	}
}

// slice_of returns the bytes the violation in `text` covers.
fn slice_of(text string) string {
	if v := violation(text) {
		assert v.pos.offset < v.pos.end_offset
		assert v.pos.end_offset <= text.len
		return text[v.pos.offset..v.pos.end_offset]
	}
	assert false, 'expected a violation in `${text}`'
	return ''
}

fn test_public_api_byte_range_covers_a_multibyte_character() {
	// The rune pass reports these, and each offending character is more than one
	// byte wide, so the range must cover the whole character.
	assert slice_of('{"a":' + ch(0xA0) + '1}') == ch(0xA0)
	assert slice_of('[1,' + ch(0x2028) + '2]') == ch(0x2028)
	assert slice_of('{"a": 1' + ch(0xFEFF) + '}') == ch(0xFEFF)
	// An escape is covered from its backslash to the end of the escaped character.
	assert slice_of('["\\é"]') == '\\é'
	assert slice_of('["\\v"]') == '\\v'
	assert slice_of('["\\u{1F600}"]') == '\\u'
	assert slice_of('["a\\' + ch(0x2028) + 'b"]') == '\\' + ch(0x2028)
	// A single-quoted string is covered whole.
	assert slice_of("{'é': 1}") == "'é'"
	if v := violation('["\\é"]') {
		assert v.pos.col == 3
		assert v.pos.offset == 2
		assert v.pos.end_offset == 5
		return
	}
	assert false, 'expected the escape to be reported'
}

fn test_violation_reports_the_first_in_the_document() {
	// The two passes each find their own kind of problem, and whichever comes
	// first in the text is the one reported, in either order.
	if v := violation('{"a": 0x10, "b": "\\v"}') {
		assert v.message.contains('hexadecimal')
		assert v.pos.col == 7
	} else {
		assert false, 'expected the hexadecimal literal to be reported'
	}
	if v := violation('{"b": "\\v", "a": 0x10}') {
		assert v.message.contains('escape')
		// The backslash, just inside the opening quote at column 7.
		assert v.pos.col == 8
	} else {
		assert false, 'expected the escape to be reported'
	}
	if v := violation("{a: 'b'}") {
		assert v.message.contains('not quoted')
		assert v.pos.col == 2
	} else {
		assert false, 'expected the unquoted key to be reported'
	}
	// parse_text reports the same one.
	parse_text("{a: 'b'}") or {
		assert err.msg() == 'jsonc: 1:2: the key `a` is not quoted, which JSON does not allow'
		return
	}
	assert false, 'expected parse_text to reject the same input'
}

fn test_parse_text_error_is_a_parse_error() {
	parse_text('{\n  "a": 1,\n  b: 2\n}') or {
		if err is ParseError {
			assert err.pos.line == 3
			assert err.pos.offset == 14
			assert err.pos.end_offset == 15
			return
		}
		assert false, 'expected a ParseError, got ${err.msg()}'
		return
	}
	assert false, 'expected an unquoted key to be rejected'
}

fn test_strip_comments_handles_invalid_utf8() {
	// A comment saved as Latin-1 carries bytes that are not UTF-8. Each one is a
	// single byte of comment, so the text after the comment must be untouched.
	mut b := '// caf'.bytes()
	b << u8(0xE9)
	b << '\n{"a": 1}'.bytes()
	head := b.bytestr()
	assert strip_comments(head) == ' '.repeat(7) + '\n{"a": 1}'
	// At the end of the text, it must not run past it.
	b = '{"a": 1} // caf'.bytes()
	b << u8(0xE9)
	tail := b.bytestr()
	assert strip_comments(tail) == '{"a": 1}' + ' '.repeat(8)
	// A real U+FFFD is three bytes, and is stripped as such.
	assert strip_comments('[1] //' + ch(0xFFFD)) == '[1]' + ' '.repeat(6)
}

fn test_violation_after_invalid_utf8_stays_in_bounds() {
	mut b := '["'.bytes()
	b << u8(0xFF)
	b << '", +1]'.bytes()
	text := b.bytestr()
	if v := violation(text) {
		assert v.message.contains('leading `+`')
		assert v.pos.offset == 6
		assert v.pos.end_offset == 8
		assert text[v.pos.offset..v.pos.end_offset] == '+1'
		return
	}
	assert false, 'expected the leading `+` to be reported'
}

fn test_violation_reports_a_valid_document_as_clean() {
	assert violation('{"a": 1 /* c */}') == none
	assert violation_opts('{"a": 1,}', ParseOpts{
		allow_trailing_comma: true
	}) == none
	// Malformed input belongs to the JSON5 parser, so there is no violation.
	assert violation('{"a":') == none
}

fn test_violation_exposes_the_position() {
	if v := violation('{a: 1}') {
		assert v.message.contains('not quoted')
		assert v.pos.line == 1
		assert v.pos.col == 2
		assert v.pos.offset == 1
		assert v.pos.end_offset == 2
		assert v.msg() == 'jsonc: 1:2: the key `a` is not quoted, which JSON does not allow'
		return
	}
	assert false, 'expected an unquoted key to be reported'
}

fn test_violation_agrees_with_the_error_from_parse_text() {
	// The two entry points must describe the same thing, because one is the
	// inspectable form of the other.
	text := '{\n  "a": 1,\n  b: 2\n}'
	if v := violation(text) {
		parse_text(text) or {
			assert err.msg() == v.msg()
			return
		}
		assert false, 'expected parse_text to reject the same input'
		return
	}
	assert false, 'expected an unquoted key to be reported'
}

fn test_strip_comments_output_parses() {
	text := '{
	// a line comment
	"host": "h" /* a block comment */
}'
	stripped := strip_comments(text)
	assert stripped.len == text.len
	value := parse(stripped) or { panic(err) }
	assert value.as_map()['host'] or { panic('no host') }.str() == 'h'
}

fn test_parse_file_and_decode_file() {
	path := os.join_path(os.temp_dir(), 'jsonc_test_${os.getpid()}.jsonc')
	mut file := os.create(path) or { panic(err) }
	file.write_string('{\n// generated\n"host": "from-disk",\n"port": 3\n}\n') or {
		panic(err)
	}
	file.close()
	defer {
		os.rm(path) or { panic(err) }
	}
	doc := parse_file(path) or { panic(err) }
	assert doc.value('host').str() == 'from-disk'
	server := decode_file[Server](path) or { panic(err) }
	assert server.port == 3
}

fn test_parse_file_reports_a_missing_file() {
	doc := parse_file('jsonc_no_such_file.jsonc') or {
		assert err.msg().contains('jsonc: could not read')
		return
	}
	assert false, 'expected a missing file to fail, got ${doc.str()}'
}

fn test_parse_error_reports_a_dialect_violation() {
	parse_text('{a: 1}') or {
		assert err.msg().contains('jsonc: 1:2:')
		assert err.msg().contains('not quoted')
		return
	}
	assert false, 'expected an unquoted key to be rejected'
}

fn test_decode_preserves_64_bit_integer_precision() {
	assert decode[u64]('18446744073709551615')! == u64(18446744073709551615)
	assert decode[i64]('9007199254740993')! == i64(9007199254740993)
}
