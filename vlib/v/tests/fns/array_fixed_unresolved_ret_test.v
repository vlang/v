enum Abc {
	a = 1
	b = 2
	c = 3
}

const abc_len = int(Abc.c)

fn build_arr() [abc_len]Abc {
	return [abc_len]Abc{}
}

fn test_main() {
	a := build_arr()
	println(a)
	assert a.len == 3
}
