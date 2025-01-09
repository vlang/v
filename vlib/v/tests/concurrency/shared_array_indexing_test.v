struct Foo {
mut:
	bar shared [10]bool
}

fn test_main() {
	mut a := Foo{
		bar: [10]bool{}
	}
	lock a.bar {
		a.bar[0] = true
		a.bar[1] = false
	}
	rlock a.bar {
		assert a.bar[0] == true
		assert a.bar[1] == false
	}
}
