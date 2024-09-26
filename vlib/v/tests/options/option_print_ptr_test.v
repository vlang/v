struct One {
	thing string = 'thing 1'
}

struct Two {
	maybe_one ?&One
}

fn test_dump_option_ptr() {
	a := Two{}
	println(a)
	println(Two{})
	dump(Two{})
}
