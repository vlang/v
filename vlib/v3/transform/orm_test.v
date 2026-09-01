module transform

fn test_sql_clean_tokens_merges_selector_no_arg_call() {
	assert sql_clean_tokens(['time', '.', 'now', '(', ')']) == ['time.now()']
	assert sql_clean_tokens(['.', 'now', '(', ')']) == ['.now()']
}

fn test_sql_generic_type_suffix_matches_generic_receiver_specialization() {
	assert sql_generic_type_suffix('Row[int]') == 'Row_int'
	assert sql_generic_type_suffix('models.Row[[]int]') == 'models__Row_Array_int'
}
