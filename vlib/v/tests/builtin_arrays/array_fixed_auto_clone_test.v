fn test_main() {
	mut buf := [100]u8{}
	name(mut buf)

	mut buf2 := []u8{}
	name(mut buf2)
}

fn name[T](mut buf4 T) {
	for idx in 0 .. 100 {
		$if T is $array_fixed {
			buf4[idx] = idx
		} $else $if T is $array {
			buf4 << idx
		}
	}
	mut bp := buf4[0..5]
	assert bp.len == 5
	println(bp)

	bp = buf4[..]
	assert bp.len == 100
	println(bp)

	bp = buf4[4..]
	assert bp.len == 96
	println(bp)

	bp = buf4[1..99]
	assert bp.len == 98
	println(bp)
}
