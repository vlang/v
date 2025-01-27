module main

import priv_sym

fn t() int {
	return 0
}

fn main() {
	priv_sym.priv()
	a := priv_sym.Foo(0)
	dump(a)
	b := priv_sym.BarFn(t)
	dump(b)
	c := priv_sym.PubFoo(0)
	dump(c)
	d := priv_sym.PubBarFn(t)
	dump(d)
}
