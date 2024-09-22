type DataFn = fn (name string) string

fn which_lang(name string) string {
	return name
}

fn find_func(name string) ?DataFn {
	a := ?DataFn(which_lang)
	return a
}

fn find_func2(name string) ?DataFn {
	a := if name == 'vlang' { ?DataFn(which_lang) } else { none }
	return a
}

fn test_main() {
	if a := find_func('foo') {
		assert a('bar') == 'bar'
	}
	if b := find_func('foo') {
		assert b('bar') == 'bar'
	}
}
