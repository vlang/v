// helper
fn sqr(x int) int {
	return x * x
}

fn high_fn(f fn (int) int) {
	x := f(111)
	println('x == ${x}')
}

fn high_fn_no_ret(f fn (int)) {
	f(111)
}

fn high_fn_array(f fn (a []int) []int) {
}

fn high_fn_multi_return(a int, b fn (c []int, d []string) ([]int, []string)) {
}

fn high_fn_return_single_anon() fn (int) f32 {
	_ = 1
	correct := fn (n int) f32 {
		return f32(n * n)
	}
	return correct
}

fn high_fn_return_multi_anons() (fn (int) f32, fn (int) string) {
	// parsing trap
	_ = fn (n int) u8 {
		return 0x00
	}
	correct_second := fn (n int) string {
		return '${n}'
	}
	correct_first := fn (n int) f32 {
		return f32(n * n)
	}
	// parsing trap
	_ = fn (n int) []int {
		return [n]
	}
	return correct_first, correct_second
}

fn high_fn_return_named_fn() fn (int) int {
	return sqr
}

fn test_high_fn_ret_anons() {
	param := 13
	func_sqr1 := high_fn_return_single_anon()
	assert func_sqr1(param) == param * param

	func_sqr2, func_repr := high_fn_return_multi_anons()
	assert func_sqr2(param) == (param * param)
	assert func_repr(param) == '${param}'

	top_lvl_sqr := high_fn_return_named_fn()
	assert top_lvl_sqr(param) == param * param
}

fn high_fn_applier(arg int, func fn (a int) string) string {
	return func(arg)
}

fn test_high_fn_applier() {
	arg := 13
	expect := '${arg} ${arg}'
	func := fn (arg int) string {
		return '${arg} ${arg}'
	}
	assert expect == high_fn_applier(arg, func)
}

fn test_fns() {
	// no asserts for now, just test function declarations above
	high_fn(sqr)
}

fn test_anon_fn() {
	f1 := fn (a int) {
		println('hello from f1')
	}
	f1(1)

	f2 := fn (a int) int {
		println('hello from f2')
		return 10
	}
	f2res := f2(1)
	println('f2res == ${f2res}')
	// TODO/FIXME: assert bug? uncomment to see
	// assert f2res == 10

	high_fn(fn (x int) int {
		return x + 1
	})

	high_fn_no_ret(fn (x int) {
		println('hello ${x}')
	})
}

fn test_anon_fn_direct_call() {
	fn (name string) {
		println('hello ${name}')
	}('from anon')

	b := fn (n int) int {
		return 11 + n
	}(100)
	assert b == 111
}

//
// Test assigning functions (IdentFn)
//

fn simple_fn1() int {
	return 1
}

fn simple_fn2(n f32) (int, string) {
	return int(1 + n), 'fish'
}

fn test_assigning_fns() {
	func1 := simple_fn1
	assert func1() == 1

	func2 := simple_fn2
	res2_1, res2_2 := func2(13.0)
	assert res2_1 == 14.0
	assert res2_2 == 'fish'

	anon_func1 := fn (s string) int {
		return s.len
	}
	func3 := anon_func1
	res3 := func3('fish')
	assert res3 == 4
}

//
// End assigning functions (IdentFn)
//
