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

fn test_lambda_expr_can_omit_unused_callback_params() {
	assert f1(|| 4) == 4
	assert f2(|x| x + 4) == 14
}

struct LambdaData {
	value int
}

fn compare_lambda_data(compare fn (&LambdaData, &LambdaData) bool, a &LambdaData, b &LambdaData) bool {
	return compare(a, b)
}

fn test_inferred_lambda_reference_parameters_compare_values() {
	a := LambdaData{}
	b := LambdaData{}
	c := LambdaData{
		value: 1
	}
	assert compare_lambda_data(|x, y| x == y, a, b)
	assert !compare_lambda_data(|x, y| x != y, a, b)
	assert !compare_lambda_data(|x, y| x == y, a, c)
	assert compare_lambda_data(|x, y| x != y, a, c)
}

fn test_lambda_expr_with_if_and_match_body() {
	assert [1, 2, 3].map(|x| if x > 1 { x } else { 0 }) == [0, 2, 3]
	assert [1, 2, 3].map(|x| match x {
		1 { 10 }
		else { 20 }
	}) == [10, 20, 20]
	assert [1, 2, 3].filter(|x| if x > 1 { true } else { false }) == [2, 3]
	assert f1(|x| if x > 1 { x * 2 } else { 0 }) == 20
	k := 5
	assert [1, 2].map(|x| if x > 1 { x + k } else { k }) == [5, 7]
}
