import strings

// The similarity scores return f32, so the expected literals carry only the
// precision an f32 can represent.
fn test_levenshtein_distance_percentage_identical_strings_score_one() {
	assert strings.levenshtein_distance_percentage('one', 'one') == 100.0
	assert strings.levenshtein_distance_percentage('kitten', 'kitten') == 100.0
	assert strings.levenshtein_distance_percentage('a', 'a') == 100.0
}

// NOTE: two empty strings make the divisor zero, so the result is NaN rather
// than the 100.0 the doc comment implies. `x != x` is the portable NaN test.
fn test_levenshtein_distance_percentage_both_empty_is_nan() {
	both_empty := strings.levenshtein_distance_percentage('', '')
	assert both_empty != both_empty
}

fn test_levenshtein_distance_percentage_disjoint_strings_score_zero() {
	assert strings.levenshtein_distance_percentage('', 'two') == 0.0
	assert strings.levenshtein_distance_percentage('two', '') == 0.0
	assert strings.levenshtein_distance_percentage('ab', 'cd') == 0.0
}

fn test_levenshtein_distance_percentage_known_pairs() {
	assert strings.levenshtein_distance_percentage('cats', 'hats') == 75.0
	assert strings.levenshtein_distance_percentage('hugs', 'shrugs') == 66.666664
	assert strings.levenshtein_distance_percentage('broom', 'shroom') == 66.666664
	assert strings.levenshtein_distance_percentage('flomax', 'volmax') == 50.0
	assert strings.levenshtein_distance_percentage('kitten', 'sitting') == 57.142853
	assert strings.levenshtein_distance_percentage('one', 'two') == 0.0
}

fn test_levenshtein_distance_percentage_is_symmetric() {
	pairs := [['', 'two'], ['one', 'two'], ['cats', 'hats'], ['hugs', 'shrugs'], ['flomax', 'volmax'],
		['kitten', 'sitting'], ['abcd', 'dcba'], ['bananna', 'banana']]
	for pair in pairs {
		forward := strings.levenshtein_distance_percentage(pair[0], pair[1])
		backward := strings.levenshtein_distance_percentage(pair[1], pair[0])
		assert forward == backward, 'pair ${pair} gave ${forward} and ${backward}'
	}
}

fn test_levenshtein_distance_percentage_matches_the_distance_divisor() {
	pairs := [['one', 'two'], ['cats', 'hats'], ['broom', 'shroom'], ['flomax', 'volmax'],
		['kitten', 'sitting'], ['bus', 'bat'], ['alphabet', 'alphabeta']]
	for pair in pairs {
		distance := f64(strings.levenshtein_distance(pair[0], pair[1]))
		divisor := f64(longer_len(pair[0].len, pair[1].len))
		expected := (1.0 - distance / divisor) * 100.0
		got := f64(strings.levenshtein_distance_percentage(pair[0], pair[1]))
		assert abs_delta(got - expected) < 1e-4, 'pair ${pair}: got ${got}, want ${expected}'
	}
}

fn test_levenshtein_distance_percentage_stays_within_bounds() {
	words := ['', 'a', 'ab', 'kit', 'kitten', 'flomax', 'bananna', 'sitting']
	for left in words {
		for right in words {
			score := strings.levenshtein_distance_percentage(left, right)
			if left.len == 0 && right.len == 0 {
				continue
			}
			assert score >= 0.0 && score <= 100.0, '(${left}, ${right}) gave ${score}'
		}
	}
}

fn test_dice_coefficient_identical_strings_score_one() {
	assert strings.dice_coefficient('one', 'one') == 1.0
	assert strings.dice_coefficient('a', 'a') == 1.0
	assert strings.dice_coefficient('abcd', 'abcd') == 1.0
}

fn test_dice_coefficient_with_an_empty_operand_is_zero() {
	assert strings.dice_coefficient('', '') == 0.0
	assert strings.dice_coefficient('', 'two') == 0.0
	assert strings.dice_coefficient('two', '') == 0.0
	assert strings.dice_coefficient('bananna', '') == 0.0
}

fn test_dice_coefficient_known_pairs() {
	assert strings.dice_coefficient('one', 'two') == 0.0
	assert strings.dice_coefficient('ab', 'cd') == 0.0
	assert strings.dice_coefficient('cats', 'hats') == 0.6666667
	assert strings.dice_coefficient('hugs', 'shrugs') == 0.5
	assert strings.dice_coefficient('broom', 'shroom') == 0.6666667
	assert strings.dice_coefficient('flomax', 'volmax') == 0.4
	assert strings.dice_coefficient('night', 'nacht') == 0.25
	assert strings.dice_coefficient('aaaa', 'aa') == 0.5
	assert strings.dice_coefficient('aaab', 'aaba') == 0.6666667
}

// A single-character pair can never produce a bigram, so the only way to a
// non-zero score is the `s1 == s2` shortcut.
fn test_dice_coefficient_single_character_pairs() {
	assert strings.dice_coefficient('a', 'a') == 1.0
	assert strings.dice_coefficient('a', 'b') == 0.0
	assert strings.dice_coefficient('x', 'y') == 0.0
}

fn test_dice_coefficient_is_symmetric() {
	pairs := [['', 'two'], ['one', 'two'], ['cats', 'hats'], ['hugs', 'shrugs'], ['flomax', 'volmax'],
		['night', 'nacht'], ['aaaa', 'aa'], ['bananna', 'banana']]
	for pair in pairs {
		forward := strings.dice_coefficient(pair[0], pair[1])
		backward := strings.dice_coefficient(pair[1], pair[0])
		assert forward == backward, 'pair ${pair} gave ${forward} and ${backward}'
	}
}

fn test_dice_coefficient_stays_within_bounds() {
	words := ['', 'a', 'ab', 'abc', 'abcd', 'aaaa', 'night', 'nacht']
	for left in words {
		for right in words {
			score := strings.dice_coefficient(left, right)
			assert score >= 0.0 && score <= 1.0, '(${left}, ${right}) gave ${score}'
		}
	}
}

fn longer_len(a int, b int) int {
	return if a >= b { a } else { b }
}

fn abs_delta(x f64) f64 {
	return if x < 0 { -x } else { x }
}
