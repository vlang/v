fn test_comptime_generic() {
	mut a := [1, 2, 3]
	mut b := [4, 5, 6]!

	func(mut a)
	func(mut b)

	println(a)
	println(b)
}

fn func<T>(mut t T) {
	$if T is $Array {
		a := t.len

		println(a)
	}
}
