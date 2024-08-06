fn my_func(a1 int, a2 int) {}

fn my_func2() (int, int) {
	return 0, 0
}

fn test_main() {
	my_func(my_func2())
}
