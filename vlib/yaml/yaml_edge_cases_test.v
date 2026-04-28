module yaml

import os
import x.json2

// Edge-case coverage for parse_text + scalar parsing + serialization.
// Complements yaml_test.v which covers the happy path. Each fn targets one
// specific gap that a previous regression in the module would have silently
// passed.

fn test_parse_strips_utf8_bom() ! {
	src := '\xef\xbb\xbfname: app\nport: 8080\n'
	doc := parse_text(src)!
	assert doc.value('name').string() == 'app'
	assert doc.value('port').int() == 8080
}

fn test_parse_normalizes_crlf_and_cr() ! {
	a := parse_text('a: 1\r\nb: 2\r\n')!
	assert a.value('a').int() == 1
	assert a.value('b').int() == 2
	b := parse_text('a: 1\rb: 2\r')!
	assert b.value('a').int() == 1
	assert b.value('b').int() == 2
}

fn test_parse_empty_and_whitespace_only_documents() ! {
	// Per YAML 1.2, a document with no content is the null node.
	for src in ['', '   ', '\n\n', '   \n  \n'] {
		doc := parse_text(src)!
		assert doc.root is Null
	}
}

fn test_parse_null_variants() ! {
	doc := parse_text('
a: ~
b: null
c: Null
d: NULL
e:
')!
	for k in ['a', 'b', 'c', 'd', 'e'] {
		v := doc.value(k)
		assert v is Null, '${k} should be Null, got ${typeof(v).name}'
	}
}

fn test_parse_bool_yaml11_variants() ! {
	doc := parse_text('
t1: true
t2: True
t3: TRUE
t4: yes
t5: YES
t6: on
t7: On
f1: false
f2: False
f3: no
f4: NO
f5: off
f6: Off
')!
	for k in ['t1', 't2', 't3', 't4', 't5', 't6', 't7'] {
		assert doc.value(k).bool(), '${k} should be true'
	}
	for k in ['f1', 'f2', 'f3', 'f4', 'f5', 'f6'] {
		assert !doc.value(k).bool(), '${k} should be false'
	}
}

fn test_parse_numeric_underscores_and_signs() ! {
	doc := parse_text('
a: 1_000_000
b: -42
c: +17
d: 1.5e10
e: -1.0e-5
')!
	assert doc.value('a').i64() == 1_000_000
	assert doc.value('b').int() == -42
	assert doc.value('c').u64() == 17
	assert doc.value('d').f64() == 1.5e10
	assert doc.value('e').f64() == -1.0e-5
}

fn test_parse_quoted_string_escapes() ! {
	doc := parse_text('
a: "line1\\nline2"
b: "tab\\there"
c: "quote: \\""
d: "unicode: \\u00e9"
e: \'sing\'\'le\'
')!
	assert doc.value('a').string() == 'line1\nline2'
	assert doc.value('b').string() == 'tab\there'
	assert doc.value('c').string() == 'quote: "'
	assert doc.value('d').string() == 'unicode: é'
	assert doc.value('e').string() == "sing'le"
}

fn test_parse_comment_inside_quoted_string_is_preserved() ! {
	doc := parse_text('a: "value with # not a comment"\nb: real # comment trimmed\n')!
	assert doc.value('a').string() == 'value with # not a comment'
	assert doc.value('b').string() == 'real'
}

fn test_parse_nested_flow_style() ! {
	doc := parse_text('root: {a: [1, [2, 3], {b: c, d: [e, f]}], g: 4}\n')!
	assert doc.value('root.a[0]').int() == 1
	assert doc.value('root.a[1]').array().len == 2
	assert doc.value('root.a[2].b').string() == 'c'
	assert doc.value('root.a[2].d[1]').string() == 'f'
	assert doc.value('root.g').int() == 4
}

fn test_parse_block_scalar_literal_and_folded() ! {
	doc := parse_text('
literal: |
  line1
  line2

  line4
folded: >
  hello
  world


  next paragraph
')!
	assert doc.value('literal').string() == 'line1\nline2\n\nline4\n'
	assert doc.value('folded').string() == 'hello world\n\nnext paragraph\n'
}

fn test_parse_rejects_tabs_in_indentation() {
	if _ := parse_text('a:\n\tb: 1\n') {
		assert false, 'tabs in indentation should error'
	} else {
		msg := err.msg()
		assert msg.contains('tabs are not supported')
		// Error must point at the offending line so the caller can locate it.
		// The tab is on line 2 of the input.
		assert msg.contains('line 2'), 'error should report line number, got: ${msg}'
	}
}

fn test_parse_rejects_unexpected_indentation_in_mapping() {
	if _ := parse_text('a: 1\n  b: 2\n') {
		assert false, 'over-indented mapping entry should error'
	} else {
		assert err.msg().contains('unexpected indentation')
	}
}

fn test_parse_json_superset_path() ! {
	// JSON-shaped input takes the json2 fast path in parse_text.
	doc := parse_text('{"a": [1, 2, {"b": "c"}], "d": null}')!
	assert doc.value('a[0]').int() == 1
	assert doc.value('a[2].b').string() == 'c'
	assert doc.value('d') is Null
}

fn test_parse_empty_inline_collections() ! {
	doc := parse_text('a: []\nb: {}\nc: [[]]\nd: [{}]\n')!
	assert doc.value('a').array().len == 0
	assert doc.value('b').as_map().len == 0
	assert doc.value('c').array().len == 1
	assert doc.value('d').array().len == 1
}

fn test_parse_deeply_nested_structure() ! {
	mut src := 'root:\n'
	mut indent := '  '
	for i in 0 .. 30 {
		src += '${indent}level${i}:\n'
		indent += '  '
	}
	src += '${indent}leaf: 42\n'
	doc := parse_text(src)!
	mut node := doc.value('root')
	for i in 0 .. 30 {
		node = node.value('level${i}')
	}
	assert node.value('leaf').int() == 42
}

fn test_to_yaml_roundtrip_preserves_structure() ! {
	src := 'name: app
servers:
  - host: a
    port: 1
  - host: b
    port: 2
'
	doc := parse_text(src)!
	yaml_text := doc.to_yaml()
	doc2 := parse_text(yaml_text)!
	assert doc2.value('name').string() == 'app'
	assert doc2.value('servers[0].host').string() == 'a'
	assert doc2.value('servers[1].port').int() == 2
}

fn test_to_yaml_is_stable_across_many_calls() ! {
	// Anti-regression for a real crash that used to surface only after many
	// repeated `to_yaml` calls on the same Doc (sumtype recursion through the
	// json2.Any rebuild path under -prod -gc boehm). 1000 iterations are
	// enough to flush the original failure mode without bloating CI runtime.
	doc := parse_text('
name: my-app
version: 1.2.3
servers:
  - host: a
    port: 1
  - host: b
    port: 2
features:
  enable_cache: true
  enable_metrics: true
')!
	first := doc.to_yaml()
	for _ in 0 .. 1000 {
		assert doc.to_yaml() == first
	}
}

fn test_to_json_emits_valid_json_for_unicode() ! {
	doc := parse_text('a: "café"\nb: "中文"\n')!
	out := doc.to_json()
	// Re-parse the output instead of asserting on a substring: this catches
	// real corruption of the strings, while staying agnostic to whitespace
	// and key ordering choices in the emitter.
	parsed := json2.decode[json2.Any](out)!
	mapped := parsed as map[string]json2.Any
	a := mapped['a'] or { return error('missing key a in re-parsed output') }
	b := mapped['b'] or { return error('missing key b in re-parsed output') }
	assert a.str() == 'café'
	assert b.str() == '中文'
}

fn test_to_json_escapes_special_chars() ! {
	doc := parse_text('a: "tab\there"\nb: "quote: \\""\n')!
	out := doc.to_json()
	parsed := json2.decode[json2.Any](out)!
	mapped := parsed as map[string]json2.Any
	a := mapped['a'] or { return error('missing key a in re-parsed output') }
	b := mapped['b'] or { return error('missing key b in re-parsed output') }
	assert a.str() == 'tab\there'
	assert b.str() == 'quote: "'
}

fn test_to_yaml_quotes_keys_consistently() ! {
	doc := parse_text('plain: 1\n"a.b": 2\n')!
	out := doc.to_yaml()
	// Both keys go through yaml_quote_string -> json.encode, so both end up
	// quoted. This guards against a future change that would silently switch
	// to plain-style and break round-tripping for keys containing dots.
	assert out.contains('"plain":')
	assert out.contains('"a.b":')
}

fn test_value_returns_null_for_missing_path() ! {
	doc := parse_text('a: 1\nb:\n  c: 2\n')!
	assert doc.value('z') is Null
	assert doc.value('a.does.not.exist') is Null
	assert doc.value('b.c.d') is Null
}

fn test_value_opt_errors_on_missing() ! {
	doc := parse_text('a: 1\n')!
	if _ := doc.value_opt('z') {
		assert false, 'expected error for missing key'
	}
}

fn test_value_returns_null_on_array_out_of_bounds() ! {
	doc := parse_text('a: [1, 2, 3]\n')!
	assert doc.value('a[99]') is Null
}

fn test_parse_skips_yaml_directives() ! {
	// `%YAML`, `%TAG`, and any other `%`-prefixed directive line is consumed
	// without becoming part of the document.
	doc := parse_text('%YAML 1.2\n%TAG !e! tag:example.com,2000:app/\n---\nname: app\n')!
	assert doc.value('name').string() == 'app'
}

fn test_parse_anchor_and_alias_resolution() ! {
	// `&id` registers the value, `*id` returns the same value at use sites.
	doc := parse_text('a: &x hello\nb: *x\nlist:\n  - &y 42\n  - *y\n')!
	assert doc.value('a').string() == 'hello'
	assert doc.value('b').string() == 'hello'
	assert doc.value('list[0]').int() == 42
	assert doc.value('list[1]').int() == 42
}

fn test_parse_unknown_alias_returns_null() ! {
	doc := parse_text('a: *missing\n')!
	assert doc.value('a') is Null
}

fn test_parse_file_happy_path() ! {
	path := os.join_path(os.vtmp_dir(), 'yaml_pf_${os.getpid()}.yml')
	defer {
		os.rm(path) or {}
	}
	os.write_file(path, 'name: app\nport: 8080\n')!
	doc := parse_file(path)!
	assert doc.value('name').string() == 'app'
	assert doc.value('port').int() == 8080
}

fn test_parse_file_returns_error_on_missing_path() {
	missing := os.join_path(os.vtmp_dir(), 'yaml_does_not_exist_${os.getpid()}.yml')
	if _ := parse_file(missing) {
		assert false, 'parse_file on missing path should error'
	}
}

fn test_parse_flow_collection_spanning_multiple_lines() ! {
	// Flow `[ ]` and `{ }` may wrap across lines; the parser must accumulate
	// until brackets balance.
	doc := parse_text('arr: [\n  1,\n  2,\n  3\n]\n')!
	assert doc.value('arr').array().len == 3
	assert doc.value('arr[2]').int() == 3
	doc2 := parse_text('obj: {\n  a: 1,\n  b: 2\n}\n')!
	assert doc2.value('obj.a').int() == 1
	assert doc2.value('obj.b').int() == 2
}
