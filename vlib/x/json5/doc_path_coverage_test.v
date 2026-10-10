// Coverage for the parts of `x/json5` that `parser_test.v`, `scanner_test.v`
// and the two files under `tests/` never reach: the `Doc` wrapper and its path
// syntax, the coercion helpers on `Any`, the `encode_any` options and the
// `decode_any` entry point.
//
// Everything here is deterministic: the inputs are literals, and the one input
// that comes from disk is written by the test itself into `os.vtmp_dir()`.
//
// The expected values were read off the running library rather than derived
// from the JSON5 specification, because several of them are decisions this
// module makes: an object coerced to an array yields its values, a scalar
// yields a one-element array, and a missing key resolves to `Any(Null)` while
// a malformed path is `none`.
module json5

import os

struct Sample {
	name  string
	count int
}

const sample_doc = '{a: {b: [10, 20, 30]}}'

const quoted_path = 'a."b.c"'

const quoted_doc = '{a: {"b.c": 5}}'

fn load_sample() !Doc {
	return parse_text(sample_doc)!
}

// ---------------------------------------------------------------------
// Any.array, Any.as_strings, Any.default_to, the kind predicates
// ---------------------------------------------------------------------

fn test_any_array_keeps_an_array_unchanged() {
	values := parse('[1, "two", true]')!
	assert values.array().len == 3
	assert values.array()[0].int() == 1
	assert values.array()[1].string() == 'two'
	assert values.array()[2].bool()
}

fn test_any_array_turns_an_object_into_its_values() {
	obj := parse('{a: 1, b: 2}')!
	// Measured: the members become the elements, in the map's own order.
	assert obj.array().len == 2
	assert obj.array()[0].int() == 1
	assert obj.array()[1].int() == 2
}

fn test_any_array_wraps_a_scalar_in_one_element() {
	scalar := parse('"x"')!
	assert scalar.array().len == 1
	assert scalar.array()[0].string() == 'x'
	assert parse('null')!.array().len == 1
}

fn test_as_strings_on_an_array_and_on_an_object() {
	values := parse('[1, "two", true]')!
	assert values.array().as_strings() == ['1', 'two', 'true']
	// An `Any` that is a number keeps its literal source text, so a hex
	// literal survives as the text it was written with.
	assert parse('[0x10]')!.array().as_strings() == ['0x10']

	obj := parse('{a: 1, b: 2}')!
	assert obj.as_map().as_strings() == {
		'a': '1'
		'b': '2'
	}
}

fn test_map_get_and_has_distinguish_present_from_absent() {
	m := parse('{a: 1}')!.as_map()
	if v := m.get('a') {
		assert v.int() == 1
	} else {
		assert false, 'an existing key must be present'
	}
	if v := m.get('b') {
		assert false, 'an absent key must be none, got `${v.str()}`'
	}
	assert m.has('a')
	assert !m.has('b')
}

fn test_is_number_and_is_string_follow_the_kind() {
	values := parse('[1, "two", true, null]')!.array()
	assert values[0].is_number()
	assert values[1].is_string()
	// Measured: a Boolean is its own kind, so `is_number` is false for `true`
	// even though `Any.int()` would coerce it to 1.
	assert !values[2].is_number()
	assert values[3].is_null()
	assert values[0].kind() == .number
	assert values[1].kind() == .string
	assert values[2].kind() == .bool
	assert values[3].kind() == .null
	assert parse('{}')!.kind() == .object
}

fn test_value_kind_str_names_every_kind() {
	// Measured: these strings are written for error messages, so `null` is
	// backquoted and every other kind is an English phrase.
	assert ValueKind.null.str() == '`null`'
	assert ValueKind.bool.str() == 'a boolean'
	assert ValueKind.number.str() == 'a number'
	assert ValueKind.string.str() == 'a string'
	assert ValueKind.array.str() == 'an array'
	assert ValueKind.object.str() == 'an object'
}

fn test_default_to_replaces_only_null() {
	fallback := parse('7')!
	assert parse('null')!.default_to(fallback).int() == 7
	assert parse('"x"')!.default_to(fallback).string() == 'x'
	// `false` and `0` are values, not absences, so they survive.
	assert parse('false')!.default_to(fallback) is bool
	assert parse('0')!.default_to(fallback).int() == 0
	// An object keeps all of its members rather than being replaced.
	obj := parse('{a: 1}')!
	assert obj.default_to(fallback).kind() == .object
	assert obj.default_to(fallback).as_map().has('a')
}

// ---------------------------------------------------------------------
// Doc: the wrapper and its path syntax
// ---------------------------------------------------------------------

fn test_new_doc_exposes_the_tree_it_wraps() {
	d := new_doc(parse('[1, 2]')!)
	// `to_any` re-encodes the root, and `str` renders it as JSON5.
	assert d.to_any().str() == '[1,2]'
	assert d.str() == '[1,2]'
	assert new_doc(parse('"x"')!).to_any().string() == 'x'
	// `Doc.str` delegates to the root's `str`, so a tree that came from
	// `parse` re-encodes to the same compact text.
	assert new_doc(parse('{a: {b: {}}}')!).str() == '{"a":{"b":{}}}'
}

fn test_doc_value_walks_keys_indices_and_quoted_keys() {
	d := load_sample()!
	assert d.value('a.b[0]').int() == 10
	assert d.value('a.b[2]').int() == 30
	assert d.value('a').array().len == 1
	// Measured: an out-of-range index and a missing key both resolve to
	// `null` rather than erroring.
	assert d.value('a.b[9]') is Null
	assert d.value('a.zz') is Null
	// A quoted key keeps the dot it contains.
	assert parse_text(quoted_doc)!.value(quoted_path).int() == 5
}

fn test_doc_value_is_null_when_a_step_meets_the_wrong_shape() {
	d := parse_text('{a: [], b: {}}')!
	assert d.value('a[0]') is Null
	assert d.value('b.zz') is Null
	// Walking a key into an array, and an index into an object, are both null.
	assert d.value('a.zz') is Null
	assert d.value('[0]') is Null
}

fn test_doc_value_opt_returns_the_value_when_the_path_resolves() {
	d := load_sample()!
	assert d.value_opt('a.b[1]')!.int() == 20
	// `a` is the inner object, so coercing it to an array yields one element,
	// which is that object's only member.
	assert d.value_opt('a')!.array().len == 1
	assert d.value_opt('a')!.as_map().has('b')
}

fn test_doc_value_opt_reports_a_missing_key_as_a_name_error() {
	d := load_sample()!
	if value := d.value_opt('a.zz') {
		assert false, 'a missing key must error, got `${value.str()}`'
	} else {
		assert err is NameError
		assert (err as NameError).key == 'a.zz'
		assert err.msg() == 'json5: no key `a.zz`'
	}
}

fn test_doc_get_separates_a_bad_path_from_a_missing_key() {
	d := load_sample()!
	// A path the parser cannot read is `none`; a readable path that names a
	// missing key still resolves, to `Any(Null)`.
	if v := d.get('a[x]') {
		assert false, 'a malformed path must be none, got `${v.str()}`'
	}
	if v := d.get('a.zz') {
		assert v is Null
	} else {
		assert false, 'a missing key must still resolve'
	}
	if v := d.get('a.b[0]') {
		assert v.int() == 10
	} else {
		assert false, 'a present key must resolve'
	}
}

fn test_doc_decode_reads_the_whole_document() {
	d := parse_text('{name: "bob", count: 3}')!
	s := d.decode[Sample]()!
	assert s.name == 'bob'
	assert s.count == 3
}

// ---------------------------------------------------------------------
// parse_path / resolve_path
// ---------------------------------------------------------------------

fn test_path_step_str_round_trips_each_form() {
	steps := parse_path('a.b[2].c')!
	assert steps.len == 4
	assert steps[0].str() == 'a'
	assert steps[1].str() == 'b'
	assert steps[2].str() == '[2]'
	assert steps[3].str() == 'c'
	// Measured: an index step carries an empty key and the parsed index,
	// which is what `is_index` reports.
	assert steps[2].key == ''
	assert steps[2].index == 2
	assert !steps[0].is_index()
	assert steps[2].is_index()
}

fn test_path_step_is_index_is_true_only_for_index_steps() {
	steps := parse_path('[0][7]')!
	assert steps.len == 2
	assert steps[0].is_index()
	assert steps[1].is_index()
	assert steps[0].index == 0
	assert steps[1].index == 7
	key_step := PathStep{
		key:   'a'
		index: -1
	}
	assert !key_step.is_index()
	// A key step is sentinel-marked with index -1, so `is_index` is a
	// comparison against that value rather than a separate flag.
	zero_key := PathStep{
		key:   'a'
		index: 0
	}
	assert zero_key.is_index()
}

fn test_parse_path_keeps_a_dot_inside_a_quoted_key() {
	steps := parse_path(quoted_path)!
	assert steps.len == 2
	assert steps[0].key == 'a'
	assert steps[1].key == 'b.c'
	assert steps[1].index == -1
	// Single quotes work the same way.
	assert parse_path("'a b'")![0].key == 'a b'
}

fn test_parse_path_of_an_empty_string_has_no_steps() {
	assert parse_path('')!.len == 0
	assert parse_path('')!.str() == '[]'
}

fn test_parse_path_rejects_a_bracket_that_is_not_an_index() {
	// Each of these is a path the walker cannot read; every one errors rather
	// than silently producing a wrong step.
	assert path_fails('a[x]')
	assert path_fails('a[]')
	assert path_fails('a[')
}

fn test_parse_path_rejects_an_unterminated_quote() {
	assert path_fails('a."b')
	assert path_fails("a.'b")
}

fn test_parse_path_rejects_text_after_a_quoted_key() {
	assert path_fails('a."b"c')
	assert path_fails("a.'b'c")
}

fn test_parse_path_rejects_a_quote_inside_a_bare_key() {
	assert path_fails('a"b')
}

fn test_resolve_path_matches_the_shape_of_each_step() {
	nested := parse(sample_doc)!
	step := parse_path('a.b[1]')!
	assert resolve_path(nested, step).int() == 20
	// A missing key, an out-of-range index, a key walked into an array and an
	// index walked into an object are all `null`.
	assert resolve_path(nested, parse_path('a.zz')!) is Null
	assert resolve_path(nested, parse_path('a.b[9]')!) is Null
	assert resolve_path(nested, parse_path('a.b.zz')!) is Null
	assert resolve_path(nested, parse_path('[0]')!) is Null
	assert resolve_path(nested, parse_path('a.b')!).array().len == 3
	assert resolve_path(nested, parse_path('a.b')!).array()[2].int() == 30
}

fn test_resolve_path_of_no_steps_returns_the_document_itself() {
	nested := parse(sample_doc)!
	whole := resolve_path(nested, [])
	assert whole is map[string]Any
	assert whole.as_map().has('a')
	// And a path with no steps applied to an array is the array itself.
	assert resolve_path(parse('[1]')!, []).array().len == 1
}

// ---------------------------------------------------------------------
// encode_any: the four options, applied to one tree
// ---------------------------------------------------------------------

fn test_encode_any_compact_is_also_valid_json() {
	assert encode_any(parse('{a: 1, b: 2}')!, EncodeOpts{}) == '{"a":1,"b":2}'
	assert encode_any(parse('[1, [2, 3]]')!, EncodeOpts{}) == '[1,[2,3]]'
}

fn test_encode_any_indent_unit_is_repeated_per_depth() {
	assert encode_any(parse('{a: {b: 1}}')!, EncodeOpts{
		indent: '  '
	}) == '{\n  "a": {\n    "b": 1\n  }\n}'
}

fn test_encode_any_single_quotes_only_affect_strings() {
	assert encode_any(parse('{a: "x"}')!, EncodeOpts{
		single_quotes: true
	}) == "{'a':'x'}"
}

fn test_encode_any_unquoted_keys_only_affects_identifier_keys() {
	assert encode_any(parse('{a: 1}')!, EncodeOpts{
		unquoted_keys: true
	}) == '{a:1}'
	// Measured: a key that is not a valid JSON5 identifier keeps its quotes,
	// so the `a-b` spelling is written the same way either way.
	assert encode_any(parse('{"a-b": 1}')!, EncodeOpts{
		unquoted_keys: true
	}) == '{"a-b":1}'
}

fn test_encode_any_trailing_commas_needs_an_indent_to_take_effect() {
	// NOTE: `trailing_commas` is read together with `indent` in both
	// `write_array` and `write_object` (`pretty := opts.indent != ''`), so on
	// its own it does nothing. Asserted as it behaves, not as it reads.
	assert encode_any(parse('[1]')!, EncodeOpts{
		trailing_commas: true
	}) == '[1]'
	assert encode_any(parse('{a: 1}')!, EncodeOpts{
		trailing_commas: true
	}) == '{"a":1}'
	assert encode_any(parse('[1, 2]')!, EncodeOpts{
		indent:          '  '
		trailing_commas: true
	}) == '[\n  1,\n  2,\n]'
	assert encode_any(parse('{a: 1}')!, EncodeOpts{
		indent:          '  '
		trailing_commas: true
	}) == '{\n  "a": 1,\n}'
}

fn test_encode_any_writes_a_raw_value_verbatim() {
	mut tree := parse('{a: 1}')!.as_map()
	tree['b'] = Raw{
		text: '/*c*/ 2'
	}
	// A `Raw` is not re-quoted, which is how a custom `to_json5()` method
	// controls quoting, comments and spacing.
	assert encode_any(tree, EncodeOpts{}) == '{"a":1,"b":/*c*/ 2}'
	assert encode_any(Raw{
		text: 'x'
	}, EncodeOpts{}) == 'x'
}

fn test_encode_any_round_trips_through_the_parser() {
	for text in ['{a: 1, b: [2, "x"], c: true}', '[1, 2, 3]', '"s"', 'null', '{a: {b: {}}}', '{}',
		'[]'] {
		value := parse(text)!
		encoded := encode_any(value, EncodeOpts{})
		// Re-parsing and re-encoding must give the same text back.
		assert parse(encoded)!.str() == value.str()
	}
}

// ---------------------------------------------------------------------
// parse_file / decode_any, against a file the test writes itself
// ---------------------------------------------------------------------

fn test_parse_file_reads_a_json5_document_from_disk() {
	path := os.join_path(os.vtmp_dir(), 'x_json5_doc_path_coverage_test.json5')
	os.write_file(path, '{name: "file", count: 12}\n')!
	defer {
		os.rm(path) or {}
	}
	d := parse_file(path)!
	if name := d.root.as_map().get('name') {
		assert name.string() == 'file'
	} else {
		assert false, 'the written file must carry a `name` member'
	}
	assert d.value('count').int() == 12
}

fn test_parse_file_names_the_file_in_a_read_error() {
	missing := os.join_path(os.vtmp_dir(), 'x_json5_doc_path_coverage_test_missing.json5')
	if _ := parse_file(missing) {
		assert false, 'a missing file must error'
	} else {
		assert err.msg().starts_with('json5: could not read')
		assert err.msg().contains(missing)
	}
}

fn test_decode_any_converts_an_already_parsed_tree() {
	s := decode_any[Sample](parse('{name: "bob", count: 3}')!)!
	assert s.name == 'bob'
	assert s.count == 3

	// A missing key leaves the field at its default, so a document that names
	// neither field decodes to the zero value rather than erroring.
	empty := decode_any[Sample](parse('{other: 1}')!)!
	assert empty.name == ''
	assert empty.count == 0
}

fn test_decode_any_errors_on_a_value_of_the_wrong_shape() {
	if _ := decode_any[int](parse('[1]')!) {
		assert false, 'an array must not decode into an int'
	} else {
		// The message is `TypeError.msg()`, so it carries the module prefix.
		assert err.msg() == 'json5: expected an integer, found an array'
	}
}

// ---------------------------------------------------------------------

fn path_fails(path string) bool {
	if steps := parse_path(path) {
		assert false, '`${path}` must not parse, got ${steps.len} steps'
	}
	return true
}
