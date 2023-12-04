fn test_main() {
	mut buf4 := [100]u8{}
	name(mut buf4)
}

fn name[T](mut buf4 T) {
	mut bp := buf4[0..5]
	assert bp.len == 5
	println(bp)

	bp = buf4[..]
	assert bp.len == 100
	println(bp)

	bp = buf4[4..]
	assert bp.len == 96
	println(bp)
}
