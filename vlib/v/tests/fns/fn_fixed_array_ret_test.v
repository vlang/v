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

fn four(a [4]int) [4]int {
	assert a.len == 4
	return a
}

fn test_passing_arg() {
	a := [1, 2, 3, 4]!
	b := four(a)
	assert b.len == 4
	c := four(b)
	assert c.len == 4
}

fn test_passing_opt_arg() {
	a := fixed_opt()
	four(a?)
}

fn test_assign() {
	mut a := [1, 2, 3, 4]!
	a = four(a)
	assert a.len == 4
	a = fixed_opt()?
	assert a.len == 4
	b := a
	assert b.len == 4
}

fn gn_fixed[T](a [4]T) [4]T {
	return a
}

fn test_generic() {
	b := [1, 2, 3, 4]!
	a := gn_fixed(b)
	c := gn_fixed(a)
}

fn test_inline() {
	mut a := [1, 2, 3, 4]!
	assert four(a)[1] == 2
}

fn f() [4]int {
	return [1, 2, 3, 4]!
}

fn test_simple_ret() {
	zzz := f()
	dump(zzz)
	dump(zzz[0])
}

fn g(a [4]int) {
}

fn test_arg_fixed() {
	zzz := f()
	g(zzz)
	g(f())
}

fn test_dump_ret() {
	zzz := f()
	a := dump(zzz)
	b := dump(zzz[0])
}
