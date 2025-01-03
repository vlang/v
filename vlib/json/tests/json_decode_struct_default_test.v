import json

struct Bar {
	b []int = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
}

struct Foo {
	Bar
	a []int = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
}

fn test_main() {
	str := json.encode(Foo{})
	assert json.decode(Foo, str)!.str() == 'Foo{
    Bar: Bar{
        b: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    }
    a: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
}'
}
