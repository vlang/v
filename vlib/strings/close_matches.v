module strings

// get_close_matches returns the candidates most similar to word, best first
// (Python's difflib.get_close_matches). Similarity is the existing
// `levenshtein_distance_percentage`, whose scale is 0..100, so `cutoff` uses
// the same scale — the default of 60 mirrors Python's `ratio`-based default
// of 0.6. Cutoffs outside 0..100 panic.
pub fn get_close_matches(word string, candidates []string, cutoff f32) []string {
	if cutoff < 0 || cutoff > 100 {
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
