import strings

fn test_levenshtein_distance() {
	assert strings.levenshtein_distance('', '') == 0
	assert strings.levenshtein_distance('one', 'one') == 0
	assert strings.levenshtein_distance('', 'two') == 3
	assert strings.levenshtein_distance('three', '') == 5
	assert strings.levenshtein_distance('bananna', '') == 7
	assert strings.levenshtein_distance('cats', 'hats') == 1
	assert strings.levenshtein_distance('hugs', 'shrugs') == 2
	assert strings.levenshtein_distance('broom', 'shroom') == 2
	assert strings.levenshtein_distance('flomax', 'volmax') == 3
}
