enum Index {
	one
	two
}

fn direct(b u8) bool {
	return true
}

fn access(b u8) bool {
	return false
}

fn check_underscore(a []u8) {
	_ = a[1]
	direct(a[0])
}

fn check_fn(a []u8) {
	access(a[2])
	direct(a[2])
	direct(a[1])
}

fn check_if_access(a []u8) {
	access(a[3])
	if a[3] == `0` {
	}
}

fn check_if_fn_access(a []u8) {
	access(a[4])
	if direct(a[4]) {
		direct(a[4])
	}
}

fn check_if_test_in_branch(a []u8) {
	if a[6] == `0` {
		direct(a[5])
		access(a[7])
	}
}

fn check_if_fn_branch(a []u8) {
	if access(a[8]) {
		direct(a[7])
	}
}

fn check_for_branch(a []u8) {
	access(a[9])
	for _ in 0 .. 1 {
		access(a[10])
	}
	direct(a[9])
	access(a[10])
}

fn check_assert(a []u8) {
	assert a.len >= 19
	direct(a[18])

	assert 21 <= a.len
	_ = a[20]
}

fn check_range_access(a []u8) {
	access(a[23])
	range := a[20..25]
	direct(range[3])
	access(range[4])
	direct(range[4])
}

fn check_for(a []u8) {
	for access(a[30]) == false {
		direct(a[30])
		access(a[31])
		return
	}
}

fn check_for_c_init_0(a []u8) {
	for a[32] == 0 {
		direct(a[32])
		access(a[33])
		return
	}
}

fn check_for_c_init_1(a []u8) {
	for access_it := a[34]; a[34] == 0; {
		direct(a[34])
		access(a[35])
		// work around https://github.com/vlang/v/issues/12832
		println(access_it)
		return
	}
}

// fn check_for_c_init_2(a []u8) {
// 	mut run := true
// 	for x := a[36]; a[36] == 0 && run; x = a[38] {
// 		direct(a[36])
// 		access(a[37])
// 		run = false
// 	}
// 	direct(a[38])
// }

// fn skip_clone(a []u8) {
// 	access(a[30])
// 	fn_local := a.clone()
// 	direct(fn_local[30])
// }

// fn skip_mut_self_assign(a []u8) {
// 	mut fn_local := a.clone()
// 	fn_local[31] = fn_local[32]
// 	direct(fn_local[31])
// }

// fn skip_enum (a []u8) {
// 	access(a[33])
// 	direct(a[Index.two])
// }

fn main() {
	a := []u8{len: 50}
	direct(a[49])

	check_underscore(a)
	check_fn(a)
	check_if_access(a)
	check_if_fn_access(a)
	check_if_test_in_branch(a)
	check_if_fn_branch(a)
	check_for_branch(a)
	check_assert(a)
	check_range_access(a)
	check_for(a)
	check_for_c_init_0(a)
	check_for_c_init_1(a)
	// check_for_c_init_2(a)
	// skip_clone(a)
	// skip_mut_self_assign(a)
	// skip_enum (a)
}
