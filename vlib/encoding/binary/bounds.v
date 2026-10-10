module binary

// check_bounds panics unless b holds size bytes starting at offset o.
// The callers use @[direct_array_access] so a discarded `_ = b[i]` read would check nothing.
@[inline]
fn check_bounds(b []u8, o int, size int) {
	if o < 0 || o > b.len - size {
		panic('encoding.binary: index out of range (offset == ${o}, size == ${size}, len == ${b.len})')
	}
}
