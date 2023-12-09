import json

struct Struct {
	a int
}

struct Test {
	a ?int
	b ?string
	c ?Struct
}

fn test_main() {
	a := json.decode(Test, '{"a": 1, "b": "foo"}')!
	dump(a)

	assert a.a != none
	assert a.b != none

	b := json.decode(Test, '{"a": 1}')!
	dump(b)
	assert b.a != none
	assert b.b == none

	c := json.decode(Test, '{"a": 1, "b": null}')!
	dump(b)
	assert c.a != none
	assert c.b == none

	d := json.decode(Test, '{"a": null, "b": null}')!
	dump(d)
	assert d.a == none
	assert d.b == none

	e := json.decode(Test, '{"a": null, "b": null, "c": null}')!
	dump(e)
	assert e.a == none
	assert e.b == none
	assert e.c == none

	f := json.decode(Test, '{"a": null, "b": null, "c": {"a":1}}')!
	dump(f)
	assert f.a == none
	assert f.b == none
	assert f.c != none
}
