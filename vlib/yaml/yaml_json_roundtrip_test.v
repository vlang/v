module yaml

// JSON round-trip: parse_text() should accept any JSON document (YAML 1.2 is
// a JSON superset), and to_json() should produce a JSON document that
// re-parses to the same logical tree. This guards against regressions in
// either the parser's JSON-superset fast path or the serializer's escaping.

const json_corpus = [
	// scalars
	'true',
	'false',
	'null',
	'0',
	'-1',
	'1234567890',
	'3.14159',
	'1.5e-10',
	'""',
	'"hello"',
	'"with\\"quote"',
	'"with\\nnewline"',
	'"unicode: \\u00e9 \\u4e2d"',
	'"verbatim utf8: café 中"',
	// arrays
	'[]',
	'[1]',
	'[1,2,3]',
	'["a","b","c"]',
	'[null,true,false,1,1.5,"s"]',
	'[[1,2],[3,4],[]]',
	'[{"k":"v"},{"k":"w"}]',
	// objects
	'{}',
	'{"a":1}',
	'{"a":1,"b":2,"c":3}',
	'{"nested":{"deep":{"deeper":42}}}',
	'{"mixed":[1,{"k":"v"},null,true]}',
	'{"empty_arr":[],"empty_obj":{}}',
	'{"unicode_key_café":"value","quoted.key":"v2"}',
	// edge sizes
	'[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]',
	'{"a":1,"b":2,"c":3,"d":4,"e":5,"f":6,"g":7,"h":8,"i":9,"j":10}',
]!

fn test_json_corpus_roundtrip() ! {
	mut failed := []string{}
	for input in json_corpus {
		doc := parse_text(input) or {
			failed << 'parse error on ${input}: ${err}'
			continue
		}
		out := doc.to_json()
		eq := json_logically_eq(input, out) or {
			failed << '${input} -> ${out}: invalid JSON produced (${err})'
			continue
		}
		if !eq {
			failed << '${input} -> ${out}: not logically equal'
		}
	}
	if failed.len > 0 {
		eprintln('JSON round-trip failures (${failed.len}/${json_corpus.len}):')
		for f in failed {
			eprintln('  - ${f}')
		}
		assert false, '${failed.len} round-trip case(s) failed'
	}
}

// Anti-regression for the "first call works, repeated call breaks" family
// that used to surface in the json2.Any rebuild path: parse + to_json must
// be byte-stable across iterations on the same input. 500 iterations are
// enough to flush the original failure mode without bloating CI runtime.
fn test_json_roundtrip_is_idempotent_under_repetition() ! {
	src := '{"users":[{"id":1,"name":"alice","tags":["x","y"]},{"id":2,"name":"bob","tags":[]}],"meta":{"count":2,"page":null,"flags":{"a":true,"b":false}}}'
	mut prev := parse_text(src)!.to_json()
	for _ in 0 .. 500 {
		curr := parse_text(prev)!.to_json()
		assert curr == prev, 'round-trip drift detected'
		prev = curr
	}
}
