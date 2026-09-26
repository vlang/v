import bar
import baz
import qux as q

// Module `baz` declares no `Foo`, so `baz.Foo` must not resolve to the `Foo` of `main`.
struct Foo {
	data []bar.MyData
}

struct Holder {
	foo   baz.Foo
	alias q.Foo
}

fn take(foo baz.Foo) baz.Foo {
	return foo
}

fn main() {
	_ := baz.Foo{[bar.MyData{'hello'}]}
	_ := baz.Foo{
		data: [bar.MyData{'world'}]
	}
	println(Foo{})
}
