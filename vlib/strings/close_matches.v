module strings

// get_close_matches returns candidates above the required cutoff, most similar first.
// Scores use levenshtein_distance_percentage on the 0..100 scale. Equal scores
// retain candidate order. A cutoff of 60 is a typical fuzzy threshold; no default
// argument is supplied. Panics if cutoff is NaN or outside 0..100.
pub fn get_close_matches(word string, candidates []string, cutoff f32) []string {
	if !(cutoff >= 0 && cutoff <= 100) {
		panic('strings.get_close_matches: cutoff must be in 0..100, got ${cutoff}')
	}
	mut scored := []MatchScore{cap: candidates.len}
	for c in candidates {
		s := levenshtein_distance_percentage(word, c)
		if s >= cutoff {
			scored << MatchScore{c, s}
		}
	}
	scored.sort(a.score > b.score)
	mut out := []string{cap: scored.len}
	for m in scored {
		out << m.name
	}
	return out
}

// MatchScore is one candidate with its similarity score (0..100).
struct MatchScore {
	name  string
	score f32
}
