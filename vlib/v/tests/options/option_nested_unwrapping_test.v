type Foo = string | int | f32

fn bar(log ?Foo) {
	if log != none {
		dump(log)
		assert typeof(log).name == 'Foo'
		if log is string {
			dump(log)
			assert true
			return
		} else {
			assert false
		}
	}
	assert false
}

fn test_main() {
	bar('foo')
}
