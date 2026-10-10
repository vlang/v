// Coverage for the two `x/jsonc` entry points the sibling files never call:
// `parse_text_opts` and `parse_file_opts`. `jsonc_test.v`, `lex_test.v` and
// `validate_test.v` all reach the option-taking functions only through their
// zero-value wrappers, and `parse_file`/`decode_file` only through the
// default opts.
//
// Every input is a literal except the two the test writes itself into
// `os.vtmp_dir()`. The messages and positions below were read off the running
// library: the violation is reported at the offending token, and the offsets
// are recovered from the source text by the module's own `locate`.
module jsonc

import os

const trailing_comma = '{"a": 1,}'

const strict_doc = '{"a": 1}'

fn write_fixture(name string, text string) !string {
	path := os.join_path(os.vtmp_dir(), name)
	os.write_file(path, text)!
	return path
}

fn test_parse_text_opts_accepts_a_trailing_comma_when_allowed() {
	doc := parse_text_opts(trailing_comma, ParseOpts{
		allow_trailing_comma: true
	})!
	// Measured: the comma is not part of the parsed tree, only a spelling.
	assert doc.str() == '{"a":1}'
	assert doc.value('a').int() == 1
	if v := doc.get('a') {
		assert v.int() == 1
	} else {
		assert false, 'the key must still resolve'
	}
}

fn test_parse_text_opts_rejects_a_trailing_comma_by_default() {
	if _ := parse_text_opts(trailing_comma, ParseOpts{}) {
		assert false, 'a trailing comma must be refused without the option'
	} else {
		assert err is ParseError
		// The violation is reported at the comma, which is line 1 column 8 of
		// `{"a": 1,}`.
		assert err.msg() == 'jsonc: 1:8: a trailing comma is not valid JSONC'
	}
}

fn test_parse_text_opts_agrees_with_parse_text_for_a_clean_document() {
	from_opts := parse_text_opts(strict_doc, ParseOpts{})!
	direct := parse_text(strict_doc)!
	assert from_opts.str() == direct.str()
	assert from_opts.root.kind() == .object
	assert from_opts.value('a').int() == 1
	// The document decodes into a plain V type through the JSON5 decoder.
	assert decode[map[string]int](strict_doc)! == {
		'a': 1
	}
}

fn test_parse_text_opts_still_enforces_the_dialect_with_the_option_set() {
	// `allow_trailing_comma` relaxes exactly one rule: an unquoted key is
	// still refused, at the key rather than at the end of the object.
	if _ := parse_text_opts('{a: 1,}', ParseOpts{
		allow_trailing_comma: true
	}) {
		assert false, 'an unquoted key must be refused'
	} else {
		assert err.msg() == 'jsonc: 1:2: the key `a` is not quoted, which JSON does not allow'
	}
}

fn test_parse_text_opts_returns_the_json5_error_for_malformed_input() {
	// A document that is not well formed JSON never reaches a dialect rule.
	if _ := parse_text_opts('{"a":', ParseOpts{
		allow_trailing_comma: true
	}) {
		assert false, 'malformed input must be refused'
	} else {
		// Measured: the JSON5 parser owns this one, so the prefix is `json5:`
		// rather than `jsonc:`.
		assert err.msg().starts_with('json5:')
	}
}

fn test_parse_file_opts_reads_a_document_from_disk() {
	path := write_fixture('x_jsonc_opts_test_clean.jsonc', '{"a": 1,\n "b": 2}\n')!
	defer {
		os.rm(path) or {}
	}
	doc := parse_file_opts(path, ParseOpts{})!
	assert doc.str() == '{"a":1,"b":2}'
	assert doc.value('b').int() == 2
}

fn test_parse_file_opts_honours_the_option_on_a_file() {
	path := write_fixture('x_jsonc_opts_test_trailing.jsonc', trailing_comma)!
	defer {
		os.rm(path) or {}
	}
	// Same file, same caller: the option is the only difference.
	assert parse_file_opts(path, ParseOpts{ allow_trailing_comma: true })!.value('a').int() == 1
	if _ := parse_file_opts(path, ParseOpts{}) {
		assert false, 'the strict read must refuse the same file'
	} else {
		assert err.msg() == 'jsonc: 1:8: a trailing comma is not valid JSONC'
	}
}

fn test_parse_file_opts_names_the_file_in_a_read_error() {
	missing := os.join_path(os.vtmp_dir(), 'x_jsonc_opts_test_missing.jsonc')
	if _ := parse_file_opts(missing, ParseOpts{}) {
		assert false, 'a missing file must error'
	} else {
		assert err.msg().starts_with('jsonc: could not read')
		assert err.msg().contains(missing)
	}
}
