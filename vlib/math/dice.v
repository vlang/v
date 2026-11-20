module math

// dice_similarity for integer sets.
pub fn dice_int(a []int, b []int) f64 {
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
	if seta.len + setb.len == 0 {
		return 0
	}
	return 2.0 * f64(inter) / f64(seta.len + setb.len)
}
