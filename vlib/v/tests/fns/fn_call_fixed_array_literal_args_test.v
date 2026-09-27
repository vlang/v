fn test_fn_call_fixed_array_literal_args() {
	ret := get_str([1]!)
	assert ret == '[1]'
}

fn get_str(t [1]int) string {
	println(t)
	return '${t}'
}

type Bools = [4]bool

fn accept_bools(_ Bools) {
}

fn get_bools(i int) Bools {
	return match i {
		0 {
			bools := Bools{}
			bools
		}
		else {
			Bools{}
		}
	}
}

fn test_fn_call_fixed_array_alias_init_arg() {
	accept_bools(Bools{})
	assert get_bools(1) == Bools{}
}
