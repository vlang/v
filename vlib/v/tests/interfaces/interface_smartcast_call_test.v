interface Foo {}

fn (f &Foo) func() {}

struct Bar {}

fn test_main() {
	mut f := Foo(Bar{})
	if mut f is Foo {
		f.func()
	}
}
