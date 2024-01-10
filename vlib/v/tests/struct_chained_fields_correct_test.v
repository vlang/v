struct Axx {
mut:
	v int
}

struct Bxx {
	a Axx
}

struct Cxx {
mut:
	b Bxx
}

struct Dxx {
mut:
	c Cxx
}

struct Exx {
mut:
	v []int
}

struct Fxx {
	e []Exx
}

fn test_chained_string() {
	mut b := Bxx{}
	b = Bxx{Axx{2}}
	assert 'b is: ' + b.a.v.str() == 'b is: 2'
}

fn test_chained_assignments() {
	mut c := Cxx{}
	c.b = Bxx{}
	mut d := Dxx{}
	d.c.b = Bxx{}
	assert true
}

fn test_chained_array_access() {
	f := Fxx{[Exx{[10, 20, 30]}, Exx{[100, 200, 300, 400]}]}
	assert 'f.e[0].v.len: 3' == 'f.e[0].v.len: ${f.e[0].v.len}'
	assert 'f.e[1].v.len: 4' == 'f.e[1].v.len: ${f.e[1].v.len}'
}
