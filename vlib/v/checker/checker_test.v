module main

import moda

import (
	modb
	modc
)

fn main() {
	b := Bar{a: 122}
	mut a := Foo{
			a: 'hello'
			b: b
	}
	// a.c = 'sa'
	a.a = 'da'
	a.b.a = 111
	
	a1 := a.a
	a2 := b.testa()
	
	mut c := testa()
	c = 1
	mut d := testb(1)
	d = 'hello'

	mut e = 'hello'
	e = testb(111)
	e = 'world'


	// c := 1 + 'string'
	// zz := hi + 1
}


fn testa() int {
	return testc(1)
}

fn testb(a int) string {
	return 'hello'
}

fn testc(a int) int {
	return a
}

fn (f &Foo) testa() int {
	mut a := f.testb()
	a = 1
	return 4
}

fn (f &Foo) testb() int {
	return 4
}

fn (b &Bar) testa() int {
	return 4
}

struct Bar {
	a int
}

struct Foo{
	a string
	b Bar
}
