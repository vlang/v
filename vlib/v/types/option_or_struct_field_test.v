module types

import os
import v.parser
import v.pref

fn option_or_field_errors(source string) []TypeError {
	path := os.join_path(os.vtmp_dir(), 'option_or_field_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.diagnostic_files[path] = true
	tc.check_semantics()
	return tc.errors.clone()
}

fn test_optional_or_fallback_is_rejected_in_struct_field_value() {
	for expression in ['input.a or { fallback.a }', '(input.a or { fallback.a })',
		'if enabled { input.a or { fallback.a } } else { false }',
		'match enabled { true { input.a or { fallback.a } } false { false } }'] {
		errors := option_or_field_errors('struct Options { a ?bool }
fn merge(input Options, fallback Options, enabled bool) Options {
	return Options{a: ${expression}}
}
fn main() {}
')
		assert errors.any(it.msg == '`or` block must provide a value of type `bool`, not `?bool`'), errors.str()
	}
}

fn test_optional_string_or_fallback_is_rejected_in_struct_field_value() {
	errors := option_or_field_errors('struct Options { s ?string }
fn merge(input Options, fallback Options) Options {
	return Options{s: input.s or { fallback.s }}
}
fn main() {}
')
	assert errors.any(it.msg == '`or` block must provide a value of type `string`, not `?string`'), errors.str()
}

fn test_optional_struct_fields_accept_payload_fallbacks_and_option_values() {
	errors := option_or_field_errors('struct Options { a ?bool; s ?string }
fn merge(input Options) Options {
	return Options{a: input.a or { false }, s: input.s or { "default" }}
}
fn forward(input Options) Options { return Options{a: input.a, s: input.s} }
fn early_return(input Options) Options {
	return Options{a: input.a or { return Options{} }}
}
fn main() {}
')
	assert errors.len == 0, errors.str()
}
