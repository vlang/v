module v

import os
import v3.flat
import v3.parser
import v3.pref

fn parse_literal_spelling_source(name string, source string) &flat.FlatAst {
	path := os.join_path(os.temp_dir(), 'v3_vfmt_spelling_${name}_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	mut prefs := pref.new_preferences()
	prefs.is_fmt = true
	prefs.preserve_comptime_conditionals = true
	prefs.supports_inline_asm = true
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, name
	return a
}

fn format_literal_spelling_source(name string, source string) string {
	return format(parse_literal_spelling_source(name, source))
}

fn assert_literal_spelling(name string, literal string) {
	source := 'fn main() {\n\ts := ${literal}\n\t_ = s\n}\n'
	out := format_literal_spelling_source(name, source)
	assert out == source, '${name}: ${out}'
	assert format_literal_spelling_source('${name}_twice', out) == out
}

fn test_formatter_preserves_each_nul_escape_spelling() {
	literals := [
		r"'x=\0'",
		r"'r=nonce,x=before\0after'",
		r"'x=\x00'",
		r"'r=nonce,x=before\x00after'",
		r"'x\x0041y'",
		r"'x\041y'",
		r"'x\00041y'",
	]
	for i, literal in literals {
		assert_literal_spelling('nul_${i}', literal)
	}
}

fn test_formatter_preserves_hex_unicode_and_backslash_spelling() {
	literals := [
		r"'A\x5cnB'",
		r"'A\u005cnB'",
		r"'A\U0000005CnB'",
		r"'\x41\u0042\U00000043'",
		r"'\x1B\x7f\xFF'",
		r"'\a\b\t\n\v\f\r'",
		r"'\$name and \\n'",
	]
	for i, literal in literals {
		assert_literal_spelling('escapes_${i}', literal)
	}
}

fn test_formatter_preserves_string_quote_delimiters() {
	literals := [
		r"''",
		r'""',
		r"'single quoted'",
		r'"double quoted"',
		r"'it\'s still single quoted'",
		r'"\"double\""',
	]
	for i, literal in literals {
		assert_literal_spelling('quotes_${i}', literal)
	}
}

fn test_formatter_preserves_prefixed_string_spelling() {
	literals := [
		r"r'\0\x00 $name'",
		r'r"\0\x00 $name"',
		r"js'\x41'",
		r'js"\x41"',
		r"c'\0'",
		r'c"\0"',
		r"c'A\x5cnB'",
	]
	for i, literal in literals {
		assert_literal_spelling('prefixes_${i}', literal)
	}
}

fn test_formatter_preserves_literals_while_formatting_surrounding_code() {
	source := "fn main(){\n  s:='x=\\0'\n  println( s )\n}\n"
	expected := "fn main() {\n\ts := 'x=\\0'\n\tprintln(s)\n}\n"
	out := format_literal_spelling_source('surrounding_code', source)
	assert out == expected, out
	assert format_literal_spelling_source('surrounding_code_twice', out) == out
}

fn test_formatter_aligns_map_keys_using_preserved_literal_spelling() {
	source := "fn main() {\n\t_ := {\n\t\t'\\x41': 1\n\t\t'B':    2\n\t}\n}\n"
	out := format_literal_spelling_source('map_keys', source)
	assert out == source, out
	assert format_literal_spelling_source('map_keys_twice', out) == out
}

fn test_formatter_keeps_c_string_selector_rewrite_idempotent() {
	source := 'fn main() {\n\ts := "\\x41".str\n}\n'
	expected := 'fn main() {\n\ts := c"\\x41"\n}\n'
	out := format_literal_spelling_source('c_string_selector_spelling', source)
	assert out == expected, out
	assert format_literal_spelling_source('c_string_selector_spelling_twice', out) == out
	js_out := format_with_options(parse_literal_spelling_source('js_string_selector', source),
		FormatOptions{
			backend: 'js'
		})
	assert js_out == source, js_out
}

fn test_formatter_preserves_multiline_string_contents() {
	source := "fn main() {\n\ts := 'first\n\t  second\nlast'\n\t_ = s\n}\n"
	out := format_literal_spelling_source('multiline', source)
	assert out == source, out
	assert format_literal_spelling_source('multiline_twice', out) == out
}

fn test_formatter_keeps_safe_escaping_without_literal_source() {
	g := Gen.new()
	// Without a source span, the fallback must not turn NUL + `41` into `\041`.
	nul := flat.Node{
		kind: .string_literal
		value: 'x\x0041y'
	}
	assert g.string_literal_text(&nul) == r"'x\x0041y'"
	// An ordinary string starting with `c:` is not a C-string node.
	ordinary := flat.Node{
		kind: .string_literal
		value: 'c:plain'
	}
	assert g.string_literal_text(&ordinary) == "'c:plain'"
	c_string := flat.Node{
		kind: .char_literal
		value: r'c:\0'
	}
	assert g.string_literal_text(&c_string) == r"c'\0'"
}
