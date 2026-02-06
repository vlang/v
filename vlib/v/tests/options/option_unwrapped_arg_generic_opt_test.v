fn test_main() {
	mut arr := ?[]int(none)
	if arr == none {
		arr = []int{}
	} else {
		ret := unwrap(arr)
		assert ret == arr
	}
	assert arr?.len == 0
}

fn t(a ?int) {}

@[inline]
pub fn unwrap[T](t ?T) T {
	return t or { panic('unexpected `none`') }
}
