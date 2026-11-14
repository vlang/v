module math

// overlap_coefficient for integer sets.
pub fn overlap_int(a []int, b []int) f64 {
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
	if seta.len == 0 || setb.len == 0 {
		return 0
	}
	mn := if seta.len < setb.len { seta.len } else { setb.len }
	return f64(inter) / f64(mn)
}
