import x.json2 as json

struct Baz {
	c bool = true
}

struct Foo {
	Baz
	a int = 1
	b int = 2
}

struct Bar {
	Foo
	a string = '1'
	c string = '3'
}

fn test_embed() {
	assert json.encode(Bar{}) == '{"a":"1","c":"3","Foo.a":1,"b":2,"Foo.Baz.c":true}'
	assert json.encode(Bar{}, prettify: true) == '{
    "a": "1",
    "c": "3",
    "Foo.a": 1,
    "b": 2,
    "Foo.Baz.c": true
}'
}
