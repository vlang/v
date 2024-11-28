module main

struct StructA {
	name string  = 'struct a'
	b    StructB = StructB{}
}

struct StructB {
	name string  = 'struct b'
	c    StructC = StructC{}
}

struct StructC {
	name string  = 'struct c'
	d    StructD = StructD{}
}

struct StructD {
	name string  = 'struct d'
	e    StructE = StructE{}
}

struct StructE {
	name string = 'struct e'
}

fn main() {
	a := StructA{}
	e := a.b.c.d.e
	println(e.name)
	println(a.b.c.d.e.name)
}
