fn test_main() {
	mut arr := ?[]int(none)
	if arr == none {
		arr = []int{}
	} else {
		unwrap(arr) << 1
	}
	assert arr?.len == 0
}

fn t(a ?int) {}

@[inline]
pub fn unwrap[T](t ?T) T {
	return t or { panic('unexpected `none`') }
}
