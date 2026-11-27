module math

// bell returns the n-th Bell number.
pub fn bell(n int) i64 {
	if n < 0 {
		return 0
	}
	mut tri := []i64{len: n + 1}
	tri[0] = 1
	for i in 1 .. n + 1 {
		mut prev := tri[i - 1]
		for j in 0 .. i {
			curr := tri[j]
			tri[j] = prev
			prev = curr + prev
		}
		tri[i] = prev
	}
	return tri[0]
}
