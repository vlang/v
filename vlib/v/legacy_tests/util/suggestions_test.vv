module util

fn test_new_suggestion_preallocates_the_bounded_storage() {
	s := new_suggestion('missing_name', []string{}, SuggestionParams{})
	assert s.known.len == 0
	assert s.known.cap == max_suggestions_limit
}

fn test_new_suggestion_caps_the_number_of_known_possibilities() {
	mut possibilities := []string{cap: int(max_suggestions_limit) + 50}
	for i in 0 .. int(max_suggestions_limit) + 50 {
		possibilities << 'candidate_${i}'
	}
	s := new_suggestion('missing_name', possibilities, SuggestionParams{})
	assert s.known.len == max_suggestions_limit
	assert s.known.cap == max_suggestions_limit
	known_values := s.known.map(it.value)
	assert 'candidate_0' in known_values
	assert 'candidate_${max_suggestions_limit - 1}' in known_values
	assert 'candidate_${max_suggestions_limit}' !in known_values
}
