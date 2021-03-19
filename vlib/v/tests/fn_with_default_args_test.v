module main

struct Info {
	age int
}

fn (inf Info) get_number(a int = 10) int {
	if a == 10 {
		return 10
	} else {
		return 0
	}
}

fn get_number(a int = 10) int {
	if a == 10 {
		return 10
	} else {
		return 0
	}
}

fn test_fn_with_default_args() {
	assert get_number() == 10
	assert get_number(2) == 0

	info := Info{}
	assert info.get_number() == 10
	assert info.get_number(2) == 0
}
