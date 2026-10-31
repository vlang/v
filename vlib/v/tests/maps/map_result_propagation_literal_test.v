fn result_map_literal() !map[string]string {
	return {
		'vlang': 'cool'
	}
}

fn test_result_map_call_and_index_propagation_remain_valid() {
	plain := {
		'vlang': 'cool'
	}
	assert plain['vlang'] == 'cool'
	returned := result_map_literal()!
	assert returned['vlang']! == 'cool'
	assert ({
		'vlang': 'cool'
	})['vlang']! == 'cool'
}
