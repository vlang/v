module main

type Dict = map[string]string

fn test_main() {
	mut x := Dict{}
	x['foo'] = 'bar'
	assert '${x}' == "Dict({'foo': 'bar'})"
	assert x.str() == "Dict({'foo': 'bar'})"
}

type Labels = map[string]int

struct LabelSet {
	values Labels
}

fn make_labels() Labels {
	return Labels({
		'key': 7
	})
}

fn test_map_alias_str_preserves_alias_name() {
	values := make_labels()
	expected := "Labels({'key': 7})"
	assert '${values}' == expected
	assert values.str() == expected
	assert make_labels().str() == expected
	assert Labels({
		'key': 7
	}).str() == expected
	set := LabelSet{values}
	assert set.values.str() == expected
}
