fn fixed() [4]int {
	return [1, 2, 3, 4]!
}

fn fixed_opt() ?[4]int {
	return [1, 2, 3, 4]!
}

fn multi_ret() ([4]int, bool) {
	return [1, 2, 3, 4]!, true
}

fn multi_ret_opt() (?[4]int, bool) {
	return [1, 2, 3, 4]!, true
}

fn multi_ret_opt_none() (?[4]int, bool) {
	return none, true
}

fn test_simple() {
	a := fixed()
	assert a.len == 4
}

fn test_simple_option() {
	b := fixed_opt()
	assert b?.len == 4
}

fn test_mr_fixed() {
	w, y := multi_ret()
	assert w.len == 4
	assert y == true
}

fn test_mr_fixed_opt() {
	w1, y1 := multi_ret_opt()
	assert w1?.len == 4
	assert y1 == true
}

fn test_mr_fixed_opt_none() {
	w2, y2 := multi_ret_opt_none()
	assert w2 == none
	assert y2 == true
}
