fn f_test(args ?[2]int) ? {
	println(args)
	assert args?.len == 2
}

fn f_arr(args ?[3]f64) ?[]f64 {
	mut ret := ?[]f64(none)
	ret = [-6.0]
	ret?.pop()
	ret? << args?[0]
	ret? << args?[1]
	ret? << args?[2]
	return ret
}

fn f_arr2(args ?[3]f64) ?[]f64 {
	arr := args?
	mut ret := []f64{}
	ret << arr[0]
	ret << arr[1]
	ret << arr[2]
	return ret
}

fn test_simple() {
	mut arr := ?[3]int(none)
	println(arr) // Option(none)
}

fn test_simple_assign() {
	mut arr := ?[3]int(none)
	assert arr == none

	arr = [1, 2, 3]!
	assert arr != none

	println(arr) // Option([1, 2, 3])
}

fn test_array_fixed_param() {
	f_test([1, 2]!)
}

fn test_assign() {
	mut a := ?[2]string(none)
	assert a == none
	a = ['a', 'b']!
	assert a != none
	a = none
	assert a == none
}

fn test_fn_call() {
	assert f_arr([0.0, 1.2, 2.3]!)?.len == 3
}

fn test_fn_unwrap_call() {
	assert f_arr2([0.0, 1.2, 2.3]!)?.len == 3
}
