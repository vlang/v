fn my_func(a1 int, a2 int) {}

fn my_func2() (int, int) {
	return 0, 0
}

fn my_func3(a1 int, a2 int, a3 string) {}

fn my_func4(a0 string, a1 int, a2 int) {}

fn my_func5() (int, string) {
	return 0, ''
}

fn my_func6(a1 int, a2 string) {}

fn my_func7(a1 int, a2 string, a3 bool) {}

fn my_func8(a1 int, a2 int, a3 string, a4 bool) {}

fn my_func9() (string, bool) {
	return '', false
}

fn test_main() {
	my_func(my_func2())
	my_func3(my_func2(), 'foo')
	my_func4('foo', my_func2())
}

fn test_mixed() {
	my_func6(my_func5())
	my_func7(my_func5(), true)
	my_func8(my_func2(), my_func9())
}
