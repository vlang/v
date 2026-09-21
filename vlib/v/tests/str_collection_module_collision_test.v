import strcollectionalpha
import strcollectionbeta

type Any = f64 | string

fn (values []Any) str() string {
	return 'main array ${values.len}'
}

fn (values map[string]Any) str() string {
	return 'main map ${values.len}'
}

fn test_collection_str_does_not_select_another_modules_same_named_element_type() {
	assert strcollectionalpha.array_text() == "[Any(7), Any('one')]"
	assert strcollectionalpha.map_text() == "{'key': Any('one')}"
	values := [strcollectionalpha.Any(7), strcollectionalpha.Any('one')]
	fields := {
		'key': strcollectionalpha.Any('one')
	}
	assert '${values}' == "[Any(7), Any('one')]"
	assert '${fields}' == "{'key': Any('one')}"
	assert values.str() == "[Any(7), Any('one')]"
	assert fields.str() == "{'key': Any('one')}"
}

fn test_collection_str_preserves_custom_methods_for_their_defining_module() {
	assert strcollectionbeta.array_text() == 'custom array 2'
	assert strcollectionbeta.map_text() == 'custom map 1'
	values := [strcollectionbeta.Any(true), strcollectionbeta.Any('two')]
	fields := {
		'key': strcollectionbeta.Any('two')
	}
	assert '${values}' == 'custom array 2'
	assert '${fields}' == 'custom map 1'
	assert values.str() == 'custom array 2'
	assert fields.str() == 'custom map 1'
}

fn test_collection_str_preserves_custom_methods_in_main() {
	values := [Any(1.5), Any('three')]
	fields := {
		'key': Any('three')
	}
	assert '${values}' == 'main array 2'
	assert '${fields}' == 'main map 1'
	assert values.str() == 'main array 2'
	assert fields.str() == 'main map 1'
}
