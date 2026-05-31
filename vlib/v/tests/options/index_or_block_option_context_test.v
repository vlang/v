struct Integration {
	id int
}

fn accepts_option(value ?Integration) bool {
	return value == none
}

fn test_array_index_or_none_assigned_to_option() {
	integrations := []Integration{}
	mut current := ?Integration{}
	current = integrations[0] or { none }
	assert current == none
}

fn test_array_index_or_option_literal_assigned_to_option() {
	integrations := []Integration{}
	mut current := ?Integration{}
	current = integrations[0] or { ?Integration{} }
	assert current == none
}

fn test_chained_array_index_or_none_assigned_to_option() {
	integrations := [Integration{
		id: 1
	}, Integration{
		id: 2
	}]
	mut current := ?Integration{}
	current = integrations.filter(it.id == 1)[0] or { none }
	if value := current {
		assert value.id == 1
	} else {
		assert false
	}
}

fn test_chained_array_index_or_option_literal_assigned_to_option() {
	integrations := [Integration{
		id: 1
	}, Integration{
		id: 2
	}]
	mut current := ?Integration{}
	current = integrations.filter(it.id == 3)[0] or { ?Integration{} }
	assert current == none
}

fn test_array_index_or_none_keeps_present_value() {
	integrations := [Integration{
		id: 7
	}]
	mut current := ?Integration{}
	current = integrations[0] or { none }
	if value := current {
		assert value.id == 7
	} else {
		assert false
	}
}

fn test_map_index_or_none_assigned_to_option() {
	integrations := map[string]Integration{}
	mut current := ?Integration{}
	current = integrations['missing'] or { none }
	assert current == none
}

fn test_index_or_none_in_option_cast() {
	integrations := []Integration{}
	current := ?Integration(integrations[0] or { none })
	assert current == none
}

fn test_index_or_none_in_option_argument() {
	integrations := []Integration{}
	assert accepts_option(integrations[0] or { none })
}
