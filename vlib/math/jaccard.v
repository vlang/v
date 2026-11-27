module math

// jaccard_similarity for sets represented as []f64? Use []int? Implement for []int.
pub fn jaccard_int(a []int, b []int) f64 {
	mut seta := map[int]bool{}
	mut setb := map[int]bool{}
	for x in a {
		seta[x] = true
	}
	for x in b {
		setb[x] = true
	}
	mut inter := 0
	for k, _ in seta {
		if setb[k] {
			inter++
		}
	}
	u := seta.len + setb.len - inter
	if u == 0 {
		return 0
	}
	return f64(inter) / f64(u)
}
