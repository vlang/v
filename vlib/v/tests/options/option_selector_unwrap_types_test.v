type SumType = int | string

interface Interface {
	a int
}

struct Struct {
	a int
}

struct Foo {
	a ?int
	b ?string
	c ?SumType
	d ?Struct
	e ?Interface
}

fn test_main() {
	w := Foo{
		a: 123
		b: 'foo'
		c: SumType(123)
		d: Struct{
			a: 123
		}
		e: Struct{
			a: 456
		}
	}
	if w.a != none {
		dump(w.a)
		assert w.a == 123
	} else {
		assert false
	}
	if w.b != none {
		dump(w.b)
		assert w.b == 'foo'
	} else {
		assert false
	}
	if w.c != none {
		dump(w.c)
		assert w.c is int
	} else {
		assert false
	}
	if w.d != none {
		dump(w.d)
		assert w.d.a == 123
	} else {
		assert false
	}
	if w.e != none {
		dump(w.e)
		assert w.e.a == 456
	} else {
		assert false
	}
}
