struct Foo {
	name ?&string
}

struct Foo2 {
mut:
	name ?&string
}

fn test_1() {
	_ := Foo{
		name: unsafe { nil }
	}
}

fn test_2() {
	mut foo := Foo2{}
	unsafe {
		foo.name = nil
	}
}
