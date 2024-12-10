fn f0(cb fn () int) int {
	return cb() * 10
}

fn f1(cb fn (a int) int) int {
	return cb(10)
}

fn f2(cb fn (a int, b int) int) int {
	return cb(10, 10)
}

fn f3(cb fn (a int, b int, c int) int) int {
	return cb(10, 10, 10)
}

enum MyEnum {
	no
	xyz   = 4
	other = 10
}

fn f3_different(cb fn (a int, b string, c MyEnum) string) string {
	return cb(10, 'abc', .xyz)
}

fn test_lambda_expr() {
	assert f0(|| 4) == 40
	assert f1(|x| x + 4) == 14
	assert f2(|xx, yy| xx + yy + 4) == 24
	assert f3(|xxx, yyy, zzz| xxx + yyy + zzz + 4) == 34
	assert f3_different(|xxx, yyy, zzz| yyy + ',${xxx}, ${yyy}, ${zzz}') == 'abc,10, abc, xyz'
}

fn doit(x int, y int, cb fn (a int, b int) string) string {
	dump(cb)
	dump(x)
	dump(y)
	return cb(x, y)
}

fn test_fn_with_callback_called_with_lambda_expression() {
	assert doit(10, 20, fn (aaa int, bbb int) string {
		return 'a: ${aaa}, b: ${bbb}'
	}) == 'a: 10, b: 20'
	assert doit(100, 200, |a, b| 'a: ${a}, b: ${b}') == 'a: 100, b: 200'
}

// for test params has blank_ident
fn f4(g fn (int) string) {
	assert g(0) == 'hello'
}

fn test_params_has_blank_ident() {
	f4(|_| 'hello')
}
