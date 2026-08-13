module transform

fn test_sql_clean_tokens_merges_selector_no_arg_call() {
	assert sql_clean_tokens(['time', '.', 'now', '(', ')']) == ['time.now()']
	assert sql_clean_tokens(['.', 'now', '(', ')']) == ['.now()']
}
