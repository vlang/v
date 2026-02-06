struct Foo {
	bar ?fn ()
}

fn test_main() {
	foo1 := Foo{
		bar: || println('foo1')
	}
	foo2 := Foo{
		bar: fn () {
			println('foo2')
		}
	}
	if bar_fn := foo1.bar {
		bar_fn()
		assert true
	} else {
		assert false
	}
	if bar_fn := foo2.bar {
		bar_fn()
		assert true
	} else {
		assert false
	}
}
