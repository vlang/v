struct Task[T] {
	idx  int
	task T
}

fn check[T](input T) T {
	ch := chan Task[T]{}
	// do something with channel
	ch.close()
	return input
}

fn test_main() {
	out := check[int](5)
	assert out == 5
}
