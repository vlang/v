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
	assert strings.levenshtein_distance('ab', 'cd') == 2
}

fn test_hamming_distance() {
	assert strings.hamming_distance('', '') == 0
	assert strings.hamming_distance('one', 'one') == 0
	assert strings.hamming_distance('', 'two') == 3
	assert strings.hamming_distance('three', '') == 5
	assert strings.hamming_distance('bananna', '') == 7
	assert strings.hamming_distance('cats', 'hats') == 1
	assert strings.hamming_distance('hugs', 'shrugs') == 6
	assert strings.hamming_distance('broom', 'shroom') == 5
	assert strings.hamming_distance('flomax', 'volmax') == 3
	assert strings.hamming_distance('ab', 'cd') == 2
}

fn test_jaro_similarity() {
	assert strings.jaro_similarity('MARTHA', 'MARHTA') == 0.9444444444444445
	assert strings.jaro_similarity('DIXON', 'DICKSONX') == 0.7666666666666666
	assert strings.jaro_similarity('JELLYFISH', 'SMELLYFISH') == 0.8962962962962964
}

fn test_jaro_winkler_similarity() {
	assert strings.jaro_winkler_similarity('accomodate', 'accommodate') == 0.018181818181818188
	assert strings.jaro_winkler_similarity('accomodate', 'accompanist') == 0.1327272727272727
	assert strings.jaro_winkler_similarity('untill', 'huntsville') == 0.1333333333333333
	assert strings.jaro_winkler_similarity('wich', 'wichita') == 0.08571428571428574
}
