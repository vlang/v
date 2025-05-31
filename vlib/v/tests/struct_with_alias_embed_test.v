struct Foo1 {}

type Foo2 = Foo1

struct Bar {
	Foo2
}

fn test_main() {
	assert Bar{}.str() == 'Bar{
    Foo2:     Foo2(Foo1{})
}'
}
