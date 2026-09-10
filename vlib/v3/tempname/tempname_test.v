module tempname

fn test_unique_token_is_unique_across_sequential_calls() {
	mut seen := map[string]bool{}
	for _ in 0 .. 1000 {
		token := unique_token()
		assert token !in seen
		seen[token] = true
	}
}
