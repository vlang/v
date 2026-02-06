struct Foo {
	data &int
}

struct FooHolder {
	foo ?&Foo
}

struct FooMain {
	foo_holder FooHolder
}

struct FooMain2 {
	foo_main FooMain
}

fn test_main() {
	a := FooMain{}
	assert a.foo_holder.foo == none

	b := FooMain2{}
	assert b.foo_main.foo_holder.foo == none
}
