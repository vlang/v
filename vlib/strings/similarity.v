module strings

@[inline]
fn min(a int, b int, c int) int {
	mut m := a
	if b < m {
		m = b
	}
	if c < m {
		m = c
	}
	return m
}

@[inline]
fn max2(a int, b int) int {
	if a < b {
		return b
	}
	return a
}

@[inline]
fn min2(a int, b int) int {
	if a < b {
		return a
	}
	return b
}

@[inline]
fn abs2(a int, b int) int {
	if a < b {
		return b - a
	}
	return a - b
}

// levenshtein_distance uses the Levenshtein Distance algorithm to calculate
// the distance between between two strings `a` and `b` (lower is closer).
@[direct_array_access]
pub fn levenshtein_distance(a string, b string) int {
	if a.len == 0 {
		return b.len
	}
	if b.len == 0 {
		return a.len
	}
	if a == b {
		return 0
	}
	mut row := []int{len: a.len + 1, init: index}
	for i := 1; i < b.len + 1; i++ {
		mut prev := i
		for j := 1; j < a.len + 1; j++ {
			mut current := row[j - 1] // match
			if b[i - 1] != a[j - 1] {
				// insertion, substitution, deletion
				current = min(row[j - 1] + 1, prev + 1, row[j] + 1)
			}
			row[j - 1] = prev
			prev = current
		}
		row[a.len] = prev
	}
	return row[a.len]
}

// levenshtein_distance_percentage uses the Levenshtein Distance algorithm to calculate how similar two strings are as a percentage (higher is closer).
pub fn levenshtein_distance_percentage(a string, b string) f32 {
	d := levenshtein_distance(a, b)
	l := if a.len >= b.len { a.len } else { b.len }
	return (1.00 - f32(d) / f32(l)) * 100.00
}

// dice_coefficient implements the Sørensen–Dice coefficient.
// It finds the similarity between two strings, and returns a coefficient
// between 0.0 (not similar) and 1.0 (exact match).
pub fn dice_coefficient(s1 string, s2 string) f32 {
	if s1.len == 0 || s2.len == 0 {
		return 0.0
	}
	if s1 == s2 {
		return 1.0
	}
	if s1.len < 2 || s2.len < 2 {
		return 0.0
	}
	a := if s1.len > s2.len { s1 } else { s2 }
	b := if a == s1 { s2 } else { s1 }
	mut first_bigrams := map[string]int{}
	for i in 0 .. a.len - 1 {
		bigram := a[i..i + 2]
		q := if bigram in first_bigrams { first_bigrams[bigram] + 1 } else { 1 }
		first_bigrams[bigram] = q
	}
	mut intersection_size := 0
	for i in 0 .. b.len - 1 {
		bigram := b[i..i + 2]
		count := if bigram in first_bigrams { first_bigrams[bigram] } else { 0 }
		if count > 0 {
			first_bigrams[bigram] = count - 1
			intersection_size++
		}
	}
	return (2.0 * f32(intersection_size)) / (f32(a.len) + f32(b.len) - 2)
}

// hamming_distance uses the Hamming Distance algorithm to calculate
// the distance between two strings `a` and `b` (lower is closer).
@[direct_array_access]
pub fn hamming_distance(a string, b string) int {
	if a.len == 0 && b.len == 0 {
		return 0
	}
	mut match_len := min2(a.len, b.len)
	mut diff_count := abs2(a.len, b.len)
	for i in 0 .. match_len {
		if a[i] != b[i] {
			diff_count++
		}
	}
	return diff_count
}

// hamming_similarity uses the Hamming Distance algorithm to calculate the distance between two strings `a` and `b`.
// It returns a coefficient between 0.0 (not similar) and 1.0 (exact match).
pub fn hamming_similarity(a string, b string) f32 {
	l := max2(a.len, b.len)
	if l == 0 {
		// Both are empty strings, should return 1.0
		return 1.0
	}
	d := hamming_distance(a, b)
	return 1.00 - f32(d) / f32(l)
}

// jaro_similarity uses the Jaro Distance algorithm to calculate
// the distance between two strings `a` and `b`.
// It returns a coefficient between 0.0 (not similar) and 1.0 (exact match).
@[direct_array_access]
pub fn jaro_similarity(a string, b string) f64 {
	a_len := a.len
	b_len := b.len
	if a_len == 0 && b_len == 0 {
		// Both are empty strings, should return 1.0
		return 1.0
	}
	if a_len == 0 || b_len == 0 {
		return 0
	}

	// Maximum distance upto which matching is allowed
	match_distance := max2(a_len, b_len) / 2 - 1

	mut a_matches := []bool{len: a_len}
	mut b_matches := []bool{len: b_len}
	mut matches := 0
	mut transpositions := 0.0

	// Traverse through the first string
	for i in 0 .. a_len {
		start := max2(0, i - match_distance)
		end := min2(b_len, i + match_distance + 1)
		for k in start .. end {
			// If there is a match
			if b_matches[k] {
				continue
			}
			if a[i] != b[k] {
				continue
			}
			a_matches[i] = true
			b_matches[k] = true
			matches++
			break
		}
	}
	// If there is no match
	if matches == 0 {
		return 0
	}
	mut k := 0
	// Count number of occurrences where two characters match but
	// there is a third matched character in between the indices
	for i in 0 .. a_len {
		if !a_matches[i] {
			continue
		}
		// Find the next matched character in second string
		for !b_matches[k] {
			k++
		}
		if a[i] != b[k] {
			transpositions++
		}
		k++
	}
	transpositions /= 2
	return (matches / f64(a_len) + matches / f64(b_len) + (matches - transpositions) / matches) / 3
}

// jaro_winkler_similarity uses the Jaro Winkler Distance algorithm to calculate
// the distance between two strings `a` and `b`.
// It returns a coefficient between 0.0 (not similar) and 1.0 (exact match).
// The scaling factor(`p=0.1`) in Jaro-Winkler gives higher weight to prefix
// similarities, making it especially effective for cases where slight misspellings
// or prefixes are common.
@[direct_array_access]
pub fn jaro_winkler_similarity(a string, b string) f64 {
	// Maximum of 4 characters are allowed in prefix
	mut lmax := min2(4, min2(a.len, b.len))
	mut l := 0
	for i in 0 .. lmax {
		if a[i] == b[i] {
			l++
		}
	}
	js := jaro_similarity(a, b)
	// select a multiplier (Winkler suggested p=0.1) for the relative importance of the prefix for the word similarity
	p := 0.1
	ws := js + f64(l) * p * (1 - js)
	return ws
}
