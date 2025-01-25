fn grow[T](mut arr []T) {
	arr.grow_cap(10)
	unsafe { arr.grow_len(10) }
}

fn test_main() {
	mut arr := []int{}
	grow(mut arr)
}
