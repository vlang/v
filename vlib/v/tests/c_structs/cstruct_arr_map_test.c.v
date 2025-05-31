module main

#include "@VMODROOT/cstruct.h"

@[typedef]
struct C.Foo {
	a int = 100
}

type Foo = C.Foo

struct GameObject {
mut:
	map_a map[string]Foo
	map_b map[string]?Foo
	b     []Foo
	c     []?Foo
}

fn test_main() {
	mut g := GameObject{}
	g.map_a['a'] = Foo{}
	g.map_b['a'] = ?Foo{
		a: 123
	}
	g.map_b['aa'] = ?Foo{}
	g.b << Foo{}
	g.c << ?Foo{
		a: 123
	}
	g.c << ?Foo{}
	println(g)
	dump(g)
	assert g.map_a.len == 1
	assert g.map_b.len == 2
	assert g.b.len == 1
	assert g.c.len == 2
	assert g.c[0] != none
	assert g.c[1] == none

	t := ?Foo{
		a: 123
	}
	dump(t)
	assert t?.a == 123
}
