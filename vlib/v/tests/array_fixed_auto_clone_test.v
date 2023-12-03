fn test_main() {
	mut buf4 := [100]u8{}
	name(mut buf4)
}

fn name[T](mut buf4 T) {
	for idx in 0 .. 100 {
		buf4[idx] = idx
	}

	mut bp := buf4[1..99]

	println(bp.bytestr())
	assert bp.len == 100
}
