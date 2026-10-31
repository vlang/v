struct NoallocSpan {
	base &u8 = unsafe { nil }
	len  int
}

@[noalloc: strict]
fn noalloc_in_range(x int) bool { return x > 3 && x < 9 }

@[noalloc: strict]
fn noalloc_out_of_range(x int) bool { return x < 3 || x > 9 }

@[noalloc: strict]
fn noalloc_span_of(buf []u8) NoallocSpan {
	return NoallocSpan{ base: unsafe { &u8(buf.data) }, len: buf.len }
}

@[noalloc: strict]
fn noalloc_view_len(buf []u8) int {
	s := unsafe { tos(&u8(buf.data), buf.len) }
	return s.len
}

@[noalloc: strict]
fn noalloc_prefix(s string, n int) string { return unsafe { tos(s.str, n) } }

fn test_noalloc_boolean_defaults_and_string_views() {
	buf := 'GET /users'.bytes()
	assert noalloc_in_range(5)
	assert !noalloc_in_range(2)
	assert noalloc_out_of_range(1)
	assert !noalloc_out_of_range(5)
	span := noalloc_span_of(buf)
	assert span.len == buf.len
	assert span.base == buf.data
	assert noalloc_view_len(buf) == buf.len
	assert noalloc_prefix('GET /users', 3) == 'GET'
}
