module params

fn needs_a_map(m map[string]int) int {
	return m.len
}

fn takes_plain(a int, b string, c bool) bool {
	return c && b.len == a
}
