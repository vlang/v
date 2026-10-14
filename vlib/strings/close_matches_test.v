module strings

fn test_get_close_matches_ranking() {
	m := get_close_matches('appel', ['apple', 'pear', 'ape', 'banana'], 60)
	assert m == ['apple', 'ape'], 'matches above cutoff, best first (ties keep order)'
	assert 'banana' !in m, 'unrelated candidates must be filtered by cutoff'
}

fn test_get_close_matches_exact() {
	m := get_close_matches('kitten', ['sitting', 'kitten'], 60)
	assert m == ['kitten'], 'only the exact match clears a 60 cutoff here'
}

fn test_get_close_matches_cutoff_behaviour() {
	all := get_close_matches('abc', ['a', 'ab', 'abcd'], 0)
	assert all == ['abcd', 'ab', 'a'], 'cutoff 0 returns everything, best first'
	zeroresults := get_close_matches('abc', ['a', 'ab', 'abcd'], 99)
	assert zeroresults == [], 'no candidate reaches a 99 cutoff'
}

fn test_get_close_matches_empty() {
	assert get_close_matches('a', []string{}, 60) == []
}

fn test_get_close_matches_equal_scores_keep_order() {
	candidates := ['abd', 'abe', 'abf', 'abg', 'abh', 'abi', 'abj', 'abk', 'abl', 'abm', 'abn',
		'abo', 'abp', 'abq', 'abr', 'abs', 'abt', 'abu', 'abv', 'abw']
	assert get_close_matches('abc', candidates, 60) == candidates
	assert get_close_matches('', ['', 'a'], 100) == ['']
}
