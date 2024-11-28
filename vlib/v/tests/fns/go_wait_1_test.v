fn f(x int, y f64) f64 {
	return x * y
}

fn test_go_return() {
	r := spawn f(3, 4.0)
	z := r.wait()
	assert typeof(z).name == 'f64'
	assert z == 12.0
}
