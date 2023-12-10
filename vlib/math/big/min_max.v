module big

@[inline]
fn imax(a int, b int) int {
	return if a > b { a } else { b }
}

@[inline]
fn imin(a int, b int) int {
	return if a < b { a } else { b }
}

@[inline]
fn umax(a u32, b u32) u32 {
	return if a > b { a } else { b }
}

@[inline]
fn umin(a u32, b u32) u32 {
	return if a < b { a } else { b }
}

@[inline]
fn iabs(v int) int {
	return if v > 0 { v } else { -v }
}
