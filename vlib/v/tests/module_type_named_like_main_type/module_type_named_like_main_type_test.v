import samename
import samename as sn

// https://github.com/vlang/v/issues/28952
// `main` declares types with the same short names as the types of `samename`.
// A `samename.` or `sn.` qualifier must still select the module's declarations.
struct Foo {
	b int
}

struct Box[T] {
	w T
	x int
}

enum Color {
	blue
}

type Num = u8

interface Speaker {
	shout() string
}

type Sum = Foo | string

struct Holder {
	f   samename.Foo
	arr []samename.Foo
	m   map[string]samename.Foo
	o   ?samename.Foo
	b   samename.Box[int]
	c   samename.Color
	n   samename.Num
	s   samename.Speaker = samename.Foo{'x'}
	sum samename.Sum     = 1
	al  sn.Foo
}

fn take(f samename.Foo) samename.Foo {
	return f
}

fn name_of[T]() string {
	return T.name
}

fn test_module_qualified_types_named_like_main_types() {
	f := samename.Foo{'hi'}
	assert take(f).a == 'hi'
	h := Holder{
		f:   f
		arr: [f]
		m:   {
			'k': f
		}
		o:   f
		b:   samename.Box[int]{7}
		c:   .green
		n:   samename.Num(5)
		al:  sn.Foo{'al'}
	}
	assert h.f.a == 'hi'
	assert h.arr[0].a == 'hi'
	assert h.m['k'].a == 'hi'
	assert h.o?.a == 'hi'
	assert h.b.v == 7
	assert h.c == samename.Color.green
	assert h.n == 5
	assert h.s.speak() == 'mod x'
	assert h.sum is int
	assert h.al.a == 'al'
	assert name_of[samename.Foo]() == 'samename.Foo'
	assert name_of[sn.Foo]() == 'samename.Foo'
	assert name_of[Foo]() == 'Foo'
	assert sizeof(samename.Foo) == sizeof(string)
	s := samename.Sum(f)
	assert s is samename.Foo
	arr := []samename.Foo{len: 2}
	assert arr.len == 2
	mut mm := map[string]sn.Foo{}
	mm['a'] = f
	assert mm['a'].a == 'hi'
	x := sn.Box[string]{
		v: 's'
	}
	assert x.v == 's'
}

fn test_main_types_named_like_module_types() {
	mf := Foo{3}
	assert mf.b == 3
	mb := Box[int]{1, 2}
	assert mb.w == 1
	assert Color.blue == Color.blue
	assert Num(1) == 1
	ms := Sum('str')
	assert ms is string
}
