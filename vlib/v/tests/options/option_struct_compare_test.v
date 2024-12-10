interface Interface {
	a ?int
}

struct Test2 {
	a ?int
}

type Alias = int
type AliasStruct = Test2
type AliasStruct2 = fn ()

type AliasStruct3 = []int

type SumType = Alias | AliasStruct2 | f64 | int

struct Test {
mut:
	a ?int
	b ?f64
	c ?string
	d ?[]int
	e ?[]string
	f ?[]f64
	g ?fn ()
	h ?Alias
	i ?Interface = Test2{
		a: -1
	}
	j ?AliasStruct
	k ?AliasStruct2
	l ?AliasStruct3
	m ?SumType
	n SumType
}

fn test_simple_compare() {
	mut a := Test{}
	mut b := Test{}
	assert a == b
}

fn test_anon_compare() {
	z := fn () {}
	d := fn () {}
	mut a := Test{}
	mut b := Test{}

	assert a == b

	a.g = z
	b.g = d
	assert a != b
}

fn test_alias_compare() {
	z := 1
	d := 2
	mut a := Test{}
	mut b := Test{}

	assert a == b

	a.h = z
	b.h = d
	assert a != b
}

fn test_iface_compare() {
	z := Test2{
		a: -1
	}
	d := Test2{
		a: 0
	}
	mut a := Test{
		a: 0
	}
	mut b := Test{
		a: 0
	}

	assert a == b

	a.i = z
	b.i = d
	assert a != b
}
