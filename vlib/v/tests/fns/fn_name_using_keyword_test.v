struct Foo {}

fn (f Foo) lock() int {
	return 22
}

fn (f Foo) mut() bool {
	return true
}

fn select() bool {
	return true
}

fn lock[T](t T) T {
	return t
}

fn match() bool {
	return true
}

fn test_fn_name_using_keyword() {
	f := Foo{}
	assert select()
	assert match()

	assert lock[int](22) == 22
	assert lock(22) == 22
	assert lock[string]('hello') == 'hello'
	assert lock('hello') == 'hello'

	assert f.lock() == 22
	assert f.mut()
}
