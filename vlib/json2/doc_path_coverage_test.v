// JSON5 document and path coverage follows the API now provided by json2.
module json2

import os

struct DocCoverageSample {
	name  string
	count int
}

const sample_doc = '{a: {b: [10, 20, 30]}}'
const quoted_path = 'a."b.c"'
const quoted_doc = '{a: {"b.c": 5}}'

fn load_sample() !Doc {
	return parse_text(sample_doc, json5: true)!
}

fn test_doc_value_walks_keys_indices_and_quoted_keys() {
	d := load_sample()!
	assert d.value('a.b[0]').int() == 10
	assert d.value('a.b[2]').int() == 30
	assert d.value('a').as_array().len == 1
	// Measured: an out-of-range index and a missing key both resolve to
	// `null` rather than erroring.
	assert d.value('a.b[9]') is Null
	assert d.value('a.zz') is Null
	// A quoted key keeps the dot it contains.
	assert parse_text(quoted_doc, json5: true)!.value(quoted_path).int() == 5
}

fn test_doc_value_is_null_when_a_step_meets_the_wrong_shape() {
	d := parse_text('{a: [], b: {}}', json5: true)!
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
	assert d.value_opt('a')!.as_array().len == 1
	assert 'b' in d.value_opt('a')!.as_map()
}

fn test_doc_value_opt_reports_a_missing_key_as_a_name_error() {
	d := load_sample()!
	if value := d.value_opt('a.zz') {
		assert false, 'a missing key must error, got `${value.str()}`'
	} else {
		assert err.msg() == 'json2: no key `a.zz`'
	}
}

fn test_doc_get_separates_a_bad_path_from_a_missing_key() {
	d := load_sample()!
	// Missing and malformed paths are none; a present JSON null remains a value.
	if v := d.get('a[x]') {
		assert false, 'a malformed path must be none, got `${v.str()}`'
	}
	assert d.get('a.zz') == none
	null_doc := parse_text('{"a": null}')!
	if v := null_doc.get('a') {
		assert v is Null
	} else {
		assert false, 'a present null must resolve'
	}
	if v := d.get('a.b[0]') {
		assert v.int() == 10
	} else {
		assert false, 'a present key must resolve'
	}
}

fn test_doc_decode_reads_the_whole_document() {
	d := parse_text('{name: "bob", count: 3}', json5: true)!
	s := d.decode[DocCoverageSample]()!
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
	nested := parse(sample_doc, json5: true)!
	step := parse_path('a.b[1]')!
	assert resolve_path(nested, step).int() == 20
	// A missing key, an out-of-range index, a key walked into an array and an
	// index walked into an object are all `null`.
	assert resolve_path(nested, parse_path('a.zz')!) is Null
	assert resolve_path(nested, parse_path('a.b[9]')!) is Null
	assert resolve_path(nested, parse_path('a.b.zz')!) is Null
	assert resolve_path(nested, parse_path('[0]')!) is Null
	assert resolve_path(nested, parse_path('a.b')!).as_array().len == 3
	assert resolve_path(nested, parse_path('a.b')!).as_array()[2].int() == 30
}

fn test_resolve_path_of_no_steps_returns_the_document_itself() {
	nested := parse(sample_doc, json5: true)!
	whole := resolve_path(nested, [])
	assert whole is map[string]Any
	assert 'a' in whole.as_map()
	// And a path with no steps applied to an array is the array itself.
	assert resolve_path(parse('[1]')!, []).as_array().len == 1
}

fn test_parse_file_reads_a_json5_document_from_disk() {
	path := os.join_path(os.vtmp_dir(), 'json5_doc_path_coverage_${os.getpid()}.json5')
	os.write_file(path, '{name: "file", count: 12}\n')!
	defer { os.rm(path) or {} }
	d := parse_file(path, json5: true)!
	assert d.value('name').str() == 'file'
	assert d.value('count').int() == 12
}

fn test_decode_any_converts_an_already_parsed_tree() {
	value := parse('{name: "bob", count: 3}', json5: true)!
	s := decode_any[DocCoverageSample](value, json5: true)!
	assert s.name == 'bob'
	assert s.count == 3
}

fn path_fails(path string) bool {
	if steps := parse_path(path) {
		assert false, '`${path}` must not parse, got ${steps.len} steps'
	}
	return true
}
