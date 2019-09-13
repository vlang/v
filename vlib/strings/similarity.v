module strings

// use levenshtein distance algorithm to calculate
// the distance between between two strings (lower is closer)
pub fn levenshtein_distance(a, b string) int {
	mut f := [int(0); b.len+1]
	for ca in a {
		mut j := 1
		mut fj1 := f[0]
		f[0]++
		for cb in b {
			mut mn := if f[j]+1 <= f[j-1]+1 { f[j]+1 } else { f[j-1]+1 }
			if cb != ca {
				mn = if mn <= fj1+1 { mn } else { fj1+1 }
			} else {
				mn = if mn <= fj1 { mn } else { fj1 }
			}
			fj1 = f[j]
			f[j] = mn
			j++
		}
	}
	return f[f.len-1]
}

// use levenshtein distance algorithm to calculate
// how similar two strings are as a percentage (higher is closer)
pub fn levenshtein_distance_percentage(a, b string) f64 {
	d := levenshtein_distance(a, b)
	l := if a.len >= b.len { a.len } else { b.len }
	return (1.00 - f64(d)/f64(l)) * 100.00
}

// implementation of Sørensen–Dice coefficient.
// find the similarity between two strings.
// returns f64 between 0.0 (not similar) and 1.0 (exact match).
pub fn dice_coefficient(s1, s2 string) f64 {
	if s1.len == 0 || s2.len == 0 { return 0.0 }
	if s1 == s2 { return 1.0 }
    if s1.len < 2 || s2.len < 2 { return 0.0 }
    mut first_bigrams := map[string]int
	for i := 0; i < s1.len-1; i++ {
		a := s1[i]
		b := s1[i+1]
		bigram := (a+b).str()
		first_bigrams[bigram] = if bigram in first_bigrams { first_bigrams[bigram]+1 } else { 1 }
	}
	mut intersection_size := 0
	for i := 0; i < s2.len-1; i++ {
		a := s2[i]
		b := s2[i+1]
		bigram := (a+b).str()
		count := if bigram in first_bigrams { first_bigrams[bigram] } else { 0 }
		if count > 0 {
			first_bigrams[bigram] = count - 1
			intersection_size = intersection_size + 1
		}
	}
	return (2.0 * intersection_size) / (f64(s1.len) + f64(s2.len) - 2)
}
