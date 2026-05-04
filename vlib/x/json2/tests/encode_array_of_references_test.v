import x.json2

struct RefArrayItem {
	a int
}

fn ref_array_item(value int, is_nil bool) &RefArrayItem {
	return if is_nil {
		unsafe { nil }
	} else {
		&RefArrayItem{
			a: value
		}
	}
}

fn test_json2_encode_array_of_references_with_nil() {
	value := [
		ref_array_item(1, false),
		ref_array_item(0, true),
		ref_array_item(2, false),
	]
	assert json2.encode(value) == '[{"a":1},null,{"a":2}]'
}
