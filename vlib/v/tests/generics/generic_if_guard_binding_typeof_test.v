// The binding of an `if v := x` guard in a generic function is the payload of `x`, also
// for `typeof`, which the specialization resolves while it clones the body.
struct Point {
	x int
}

fn wrap_result[T](x T) !T {
	return x
}

fn wrap_option[T](x T) ?T {
	return x
}

fn option_param[T](a ?T) string {
	if v := a {
		return typeof(v).name
	}
	return 'none'
}

fn mut_option_param[T](mut a ?T) string {
	if mut v := a {
		return typeof(v).name
	}
	return 'none'
}

fn result_call[T](x T) string {
	if v := wrap_result(x) {
		return typeof(v).name
	}
	return 'error'
}

fn option_call[T](x T) string {
	if v := wrap_option(x) {
		w := v
		return '${typeof(v).name} ${typeof(w).name}'
	}
	return 'none'
}

fn option_element[T](a []?T) string {
	if v := a[0] {
		return typeof(v).name
	}
	return 'none'
}

fn test_guard_binding_typeof() {
	assert option_param(?Point(Point{1})) == 'Point'
	assert option_param(?int(3)) == 'int'
	assert option_param(?[]int([1])) == '[]int'
	assert option_param(?Point(none)) == 'none'
	mut p := Point{1}
	assert mut_option_param(mut p) == 'Point'
	mut s := ?string('s')
	assert mut_option_param(mut s) == 'string'
	assert result_call(Point{1}) == 'Point'
	assert result_call(1.5) == 'f64'
	assert option_call(Point{1}) == 'Point Point'
	assert option_call('x') == 'string string'
	assert option_element([?Point(Point{1})]) == 'Point'
}
