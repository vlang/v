module strings

// #-js
// use levenshtein distance algorithm to calculate
// the distance between between two strings (lower is closer)
pub fn levenshtein_distance(a string, b string) int {
	mut f := [0].repeat(b.len + 1)
	for j in 0 .. f.len {
		f[j] = j
	}
	for ca in a {
		mut j := 1
		mut fj1 := f[0]
		f[0]++
		for cb in b {
			mut mn := if f[j] + 1 <= f[j - 1] + 1 { f[j] + 1 } else { f[j - 1] + 1 }
			if cb != ca {
				mn = if mn <= fj1 + 1 { mn } else { fj1 + 1 }
			} else {
				mn = if mn <= fj1 { mn } else { fj1 }
			}
			fj1 = f[j]
			f[j] = mn
			j++
		}
	}
	return f[f.len - 1]
}

// use levenshtein distance algorithm to calculate
// how similar two strings are as a percentage (higher is closer)
pub fn levenshtein_distance_percentage(a string, b string) f32 {
	d := levenshtein_distance(a, b)
	l := if a.len >= b.len { a.len } else { b.len }
	return (1.00 - f32(d) / f32(l)) * 100.00
}

// implementation of Sørensen–Dice coefficient.
// find the similarity between two strings.
// returns coefficient between 0.0 (not similar) and 1.0 (exact match).
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
