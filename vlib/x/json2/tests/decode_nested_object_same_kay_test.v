import x.json2

// Test for issue #26503: Decoder incorrectly reads nested object fields
// When a required field appears after unmatched fields with nested objects/arrays,
// the decoder was incorrectly picking up values from nested structures.

struct Foo {
	id    string @[required]
	title string @[required]
}

fn test_decode_with_nested_objects_field_order() {
	// Test case 1: required fields appear before the nested array
	s1 := '{"id":"sss","title":"ttt","thumb":[{ "url":"i1.jpg","id":"000"}]}'
	f1 := json2.decode[Foo](s1)!

	assert f1.id == 'sss', 'f1.id should be "sss" but got "${f1.id}"'
	assert f1.title == 'ttt', 'f1.title should be "ttt" but got "${f1.title}"'

	// Test case 2: required fields appear after the nested array
	s2 := '{"title":"ttt","thumb":[{ "url":"i1.jpg","id":"000"}],"id":"sss"}'
	f2 := json2.decode[Foo](s2)!

	assert f2.id == 'sss', 'f2.id should be "sss" but got "${f2.id}"'
	assert f2.title == 'ttt', 'f2.title should be "ttt" but got "${f2.title}"'

	// Test case 3: nested array appears between required fields
	s3 := '{"id":"sss","thumb":[{ "url":"i1.jpg","id":"000"}],"title":"ttt"}'
	f3 := json2.decode[Foo](s3)!

	assert f3.id == 'sss', 'f3.id should be "sss" but got "${f3.id}"'
	assert f3.title == 'ttt', 'f3.title should be "ttt" but got "${f3.title}"'
}

fn test_decode_with_deeply_nested_objects() {
	// Test with deeply nested structures to ensure complete skipping
	s := '{"id":"outer","data":{"nested":{"deep":{"id":"inner","title":"inner_title"}}},"title":"outer_title"}'
	f := json2.decode[Foo](s)!

	assert f.id == 'outer', 'id should be "outer" but got "${f.id}"'
	assert f.title == 'outer_title', 'title should be "outer_title" but got "${f.title}"'
}

fn test_decode_with_multiple_nested_arrays() {
	// Test with multiple nested arrays to ensure complete skipping
	s := '{"id":"correct","items":[{"id":"a"},{"id":"b"}],"more":[{"id":"c"}],"title":"correct_title"}'
	f := json2.decode[Foo](s)!

	assert f.id == 'correct', 'id should be "correct" but got "${f.id}"'
	assert f.title == 'correct_title', 'title should be "correct_title" but got "${f.title}"'
}

struct BarWithOptional {
	id    string @[required]
	title ?string
	extra string
}

fn test_decode_optional_fields_with_nested() {
	// Test optional fields work correctly with nested structures
	s := '{"id":"main","nested":{"title":"nested_title"},"extra":"extra_value"}'
	b := json2.decode[BarWithOptional](s)!

	assert b.id == 'main', 'id should be "main" but got "${b.id}"'
	assert b.extra == 'extra_value', 'extra should be "extra_value" but got "${b.extra}"'
	if title := b.title {
		assert false, 'title should be none but got "${title}"'
	}
}
