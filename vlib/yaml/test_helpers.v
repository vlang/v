module yaml

import x.json2

// json_logically_eq compares two JSON strings by decoding and re-encoding
// both sides via json2, normalizing whitespace and treating an empty input
// as the literal "null" document.
//
// For tests only. Lives in a non-_test.v file because V compiles each
// `_test.v` file as its own binary, so a helper cannot otherwise be shared
// across the module's test files. Not exported (lowercase name) so it stays
// invisible to consumers of `yaml`.
fn json_logically_eq(a string, b string) !bool {
	a_norm := if a.trim_space() == '' { 'null' } else { a }
	b_norm := if b.trim_space() == '' { 'null' } else { b }
	pa := json2.decode[json2.Any](a_norm)!
	pb := json2.decode[json2.Any](b_norm)!
	return json2.encode(pa, json2.EncoderOptions{}) == json2.encode(pb, json2.EncoderOptions{})
}
