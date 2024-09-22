type DataFn = fn (name string) string

fn which_lang(name string) string {
	return name
}

fn find_func(name string) ?DataFn {
	a := ?DataFn(which_lang)
	return a
}

fn test_main() {
	if a := find_func('foo') {
		assert a('bar') == 'bar'
	}
}
