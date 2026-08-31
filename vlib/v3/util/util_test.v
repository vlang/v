module util

fn test_escape_sequence_and_capital_helpers() {
	assert is_escape_sequence(`n`)
	assert is_escape_sequence(`\\`)
	assert !is_escape_sequence(`q`)
	assert contains_capital('lowerCase')
	assert !contains_capital('lower_case')
}

fn test_new_suggestion_bounds_candidate_storage() {
	mut possibilities := []string{cap: int(max_suggestions_limit) + 1}
	for i in 0 .. int(max_suggestions_limit) + 1 {
		possibilities << 'candidate_${i}'
	}
	suggestion := new_suggestion('missing_name', possibilities, SuggestionParams{})
	assert suggestion.known.len == max_suggestions_limit
	assert suggestion.known.cap == max_suggestions_limit
	assert suggestion.known.last().value.len > 0
}

fn test_githash_reads_repository_head() {
	hash := githash(@VMODROOT)!
	assert hash.len == 7
}
