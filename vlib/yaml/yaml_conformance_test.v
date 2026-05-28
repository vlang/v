module yaml

// Conformance-style coverage. Each case mirrors a pattern from the YAML 1.2
// spec (or the public yaml-test-suite) that's within the subset V's parser
// implements: plain mappings, sequences, scalars, flow style, JSON-superset
// documents. Anchors, aliases, tags, merge keys, multi-document streams and
// the chomp/indent indicators on block scalars are intentionally NOT covered:
// V's parser does not support them today.
//
// Each case is `name | yaml_input | expected_json` and is verified by parsing
// `yaml_input`, serializing to JSON, and comparing against the expected JSON
// via `json_logically_eq` (see `test_helpers.v`).
//
// Scalar variants (booleans, nulls, numeric underscores), quoted-string
// escapes, block scalars, `[]`/`{}`, and the empty document are exercised by
// `yaml_edge_cases_test.v` with stricter, typed assertions; not duplicated here.

struct ConformanceCase {
	name     string
	src      string
	expected string
}

const cases = [
	ConformanceCase{
		name:     'single scalar mapping'
		src:      'key: value'
		expected: '{"key":"value"}'
	},
	ConformanceCase{
		name:     'integer values'
		src:      'a: 1\nb: -2\nc: 0\nd: 9223372036854775807'
		expected: '{"a":1,"b":-2,"c":0,"d":9223372036854775807}'
	},
	ConformanceCase{
		name:     'float values'
		src:      'a: 1.5\nb: -0.001\nc: 1.0e10\nd: -1.5e-3'
		expected: '{"a":1.5,"b":-0.001,"c":10000000000,"d":-0.0015}'
	},
	ConformanceCase{
		name:     'simple sequence'
		src:      'items:\n  - one\n  - two\n  - three'
		expected: '{"items":["one","two","three"]}'
	},
	ConformanceCase{
		name:     'nested mapping under sequence item'
		src:      'servers:\n  - host: a\n    port: 1\n  - host: b\n    port: 2'
		expected: '{"servers":[{"host":"a","port":1},{"host":"b","port":2}]}'
	},
	ConformanceCase{
		name:     'flow sequence inline'
		src:      'tags: [web, api, db]'
		expected: '{"tags":["web","api","db"]}'
	},
	ConformanceCase{
		name:     'flow mapping inline'
		src:      'config: {host: a, port: 8080}'
		expected: '{"config":{"host":"a","port":8080}}'
	},
	ConformanceCase{
		name:     'mixed flow and block'
		src:      'top:\n  inner:\n    - {k: v, n: 1}\n    - [a, b, c]'
		expected: '{"top":{"inner":[{"k":"v","n":1},["a","b","c"]]}}'
	},
	ConformanceCase{
		name:     'deeply nested mapping'
		src:      'a:\n  b:\n    c:\n      d:\n        e: end'
		expected: '{"a":{"b":{"c":{"d":{"e":"end"}}}}}'
	},
	ConformanceCase{
		name:     'sequence of sequences'
		src:      'matrix:\n  - [1, 2, 3]\n  - [4, 5, 6]\n  - [7, 8, 9]'
		expected: '{"matrix":[[1,2,3],[4,5,6],[7,8,9]]}'
	},
	ConformanceCase{
		name:     'unicode in string values'
		src:      'a: "café"\nb: "日本語"\nc: plain café'
		expected: '{"a":"café","b":"日本語","c":"plain café"}'
	},
	ConformanceCase{
		name:     'comment is stripped'
		src:      '# leading comment\nkey: value # trailing comment'
		expected: '{"key":"value"}'
	},
	ConformanceCase{
		name:     'document separator markers are tolerated'
		src:      '---\na: 1\n...'
		expected: '{"a":1}'
	},
	ConformanceCase{
		name:     'JSON-superset input (object)'
		src:      '{"a": 1, "b": [2, 3]}'
		expected: '{"a":1,"b":[2,3]}'
	},
	ConformanceCase{
		name:     'JSON-superset input (array root)'
		src:      '[1, 2, {"three": 3}]'
		expected: '[1,2,{"three":3}]'
	},
	ConformanceCase{
		name:     'sequence containing null'
		src:      'a:\n  - one\n  - ~\n  - three'
		expected: '{"a":["one",null,"three"]}'
	},
	ConformanceCase{
		name:     'mapping with quoted keys containing dots'
		src:      'plain: 1\n"a.b": 2\n"c.d.e": 3'
		expected: '{"plain":1,"a.b":2,"c.d.e":3}'
	},
	ConformanceCase{
		name:     'mixed types in flow sequence'
		src:      'mix: [1, "two", true, null, 3.14]'
		expected: '{"mix":[1,"two",true,null,3.14]}'
	},
	ConformanceCase{
		name:     'nested flow inside block sequence'
		src:      'items:\n  - {a: 1, b: [x, y]}\n  - {a: 2, b: [z]}'
		expected: '{"items":[{"a":1,"b":["x","y"]},{"a":2,"b":["z"]}]}'
	},
]!

fn test_yaml_conformance_cases() ! {
	mut failed := []string{}
	for c in cases {
		doc := parse_text(c.src) or {
			failed << '${c.name}: parse error: ${err}'
			continue
		}
		got := doc.to_json()
		matched := json_logically_eq(got, c.expected) or {
			failed << '${c.name}: invalid JSON produced: ${got} (vs expected ${c.expected}): ${err}'
			continue
		}
		if !matched {
			failed << '${c.name}: got ${got}, want ${c.expected}'
		}
	}
	if failed.len > 0 {
		eprintln('YAML conformance failures (${failed.len}/${cases.len}):')
		for f in failed {
			eprintln('  - ${f}')
		}
		assert false, '${failed.len} conformance case(s) failed'
	}
}
