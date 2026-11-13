fn test_array_equality_with_default_empty_strings() {
	defaults := []string{len: 2}
	other_defaults := []string{len: 2}
	literals := ['', '']
	assert defaults == other_defaults
	assert defaults == literals
	assert literals == defaults
	assert defaults != ['', 'value']
	assert ['value', ''] != defaults
	assert defaults != ['']
	assert [defaults] == [literals]
}
