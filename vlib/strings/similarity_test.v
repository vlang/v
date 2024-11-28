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

fn test_hamming_similarity() {
	assert strings.hamming_similarity('', '') == 1.0
	assert strings.hamming_similarity('one', 'one') == 1.0
	assert strings.hamming_similarity('', 'two') == 0
	assert strings.hamming_similarity('three', '') == 0
	assert strings.hamming_similarity('bananna', '') == 0
	assert strings.hamming_similarity('cats', 'hats') == 0.75
	assert strings.hamming_similarity('hugs', 'shrugs') == 0
	assert strings.hamming_similarity('broom', 'shroom') == 0.1666666865348816
	assert strings.hamming_similarity('flomax', 'volmax') == 0.5
	assert strings.hamming_similarity('ab', 'cd') == 0
}

fn test_jaro_similarity() {
	assert strings.jaro_similarity('', '') == 1
	assert strings.jaro_similarity('one', 'one') == 1
	assert strings.jaro_similarity('', 'two') == 0
	assert strings.jaro_similarity('three', '') == 0
	assert strings.jaro_similarity('bananna', '') == 0
	assert strings.jaro_similarity('MARTHA', 'MARHTA') == 0.9444444444444445
	assert strings.jaro_similarity('DIXON', 'DICKSONX') == 0.7666666666666666
	assert strings.jaro_similarity('JELLYFISH', 'SMELLYFISH') == 0.8962962962962964
}

fn test_jaro_winkler_similarity() {
	assert strings.jaro_winkler_similarity('', '') == 1
	assert strings.jaro_winkler_similarity('one', 'one') == 1
	assert strings.jaro_winkler_similarity('', 'two') == 0
	assert strings.jaro_winkler_similarity('three', '') == 0
	assert strings.jaro_winkler_similarity('bananna', '') == 0
	assert strings.jaro_winkler_similarity('accomodate', 'accommodate') == 0.9818181818181818
	assert strings.jaro_winkler_similarity('accomodate', 'accompanist') == 0.8672727272727273
	assert strings.jaro_winkler_similarity('untill', 'huntsville') == 0.8666666666666667
	assert strings.jaro_winkler_similarity('wich', 'wichita') == 0.9142857142857143
}
