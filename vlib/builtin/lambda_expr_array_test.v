const a = [4, 5, 1, 2, 5, 9]

fn test_map() {
	assert a.map(it) == a
	assert a.map(it * 10) == [40, 50, 10, 20, 50, 90]

	assert a.map(|x| x) == a
	assert a.map(|x| x * 10) == [40, 50, 10, 20, 50, 90]
	assert a.map(|x| 'x: ${x}') == ['x: 4', 'x: 5', 'x: 1', 'x: 2', 'x: 5', 'x: 9']
	assert a.map(|x| f64(x) * 10.0) == [40.0, 50.0, 10.0, 20.0, 50.0, 90.0]
}

fn test_filter() {
	assert a.filter(it > 4) == [5, 5, 9]
	assert a.filter(it < 4) == [1, 2]

	assert a.filter(|x| x > 4) == [5, 5, 9]
	assert a.filter(|x| x < 4) == [1, 2]
}

fn test_any() {
	assert a.any(it > 4)
	assert !a.any(it > 40)

	assert a.any(|x| x > 4)
	assert !a.any(|x| x > 40)
}

fn test_all() {
	assert !a.all(it > 4)
	assert a.all(it < 40)

	assert !a.all(|x| x > 4)
	assert a.all(|x| x < 40)
}

fn each(a []int, cb fn (x int)) {
	for x in a {
		cb(x)
	}
}

fn test_using_lambda_expr_that_does_not_return_as_cb() {
	each(a, fn (x int) {
		println(x)
	})
	each(a, |x| println(x))
	each(a, |x| dump(x))
}
