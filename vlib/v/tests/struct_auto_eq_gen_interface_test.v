import io

type Foo = int

fn (f Foo) write(p []byte) ?int {
	return 0
}

fn new() io.Writer {
	return Foo(0)
}

fn test_iowriter() {
	w1 := new()
	w2 := new()

	assert w1 == w2
}
