struct OptionalCallMapHolder {
	items map[string]?int
}

fn optional_call_map_holder() &OptionalCallMapHolder {
	mut items := map[string]?int{}
	items['empty'] = none
	items['zero'] = 0
	items['value'] = 17
	return &OptionalCallMapHolder{ items: items }
}

fn optional_call_map_read(value ?int) int {
	if present := value {
		return present
	}
	return -1
}

fn test_call_result_map_field_preserves_stored_optional_values() {
	empty := optional_call_map_holder().items['empty']
	assert optional_call_map_read(empty) == -1
	missing := optional_call_map_holder().items['missing']
	assert optional_call_map_read(missing) == -1
	zero := optional_call_map_holder().items['zero']
	assert optional_call_map_read(zero) == 0
	value := optional_call_map_holder().items['value']
	assert optional_call_map_read(value) == 17
}
