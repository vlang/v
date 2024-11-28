@[heap]
struct Abc {
mut:
	n int
}

struct St {
	Abc
}

struct Qwe {
mut:
	f f64
	a Abc
}

fn pass_abc(q &Abc) &Abc {
	return q
}

fn pass_st(q &St) &St {
	return q
}

fn pass_qwe(q &Qwe) &Qwe {
	return q
}

fn get_ref_structs() (&Abc, &St, &Qwe) {
	a := Abc{
		n: 3
	}
	b := St{Abc{
		n: 7
	}}
	x := Qwe{
		f: 12.25
		a: Abc{
			n: 23
		}
	}
	aa := pass_abc(&a)
	bb := pass_st(&b)
	xx := pass_qwe(&x)
	return aa, bb, xx
}

fn overwrite_stack() f64 {
	a := 12.5
	b := 3.5
	c := a + b
	return c
}

fn test_ref_struct() {
	u, v, w := get_ref_structs()
	d := overwrite_stack()
	assert u.n == 3
	assert v.n == 7
	assert w.a.n == 23
	assert d == 16.0
}

fn return_heap_obj_value_as_ref(qpast Qwe) &Qwe {
	return &qpast
}

fn test_value_ref_heap_struct() {
	mut x := Qwe{
		f: -13.25
		a: Abc{
			n: -129
		}
	}
	y := return_heap_obj_value_as_ref(x)
	x.f = 22.0625
	d := overwrite_stack()
	assert typeof(y).name == '&Qwe'
	assert x.f == 22.0625
	assert x.a.n == -129
	assert y.f == -13.25
	assert y.a.n == -129
	assert d == 16.0
}

struct NotHeap {
mut:
	f f64
}

fn return_struct_value_as_ref(q NotHeap) &NotHeap {
	return &q
}

fn test_value_ref_struct() {
	mut x := NotHeap{
		f: -17.125
	}
	y := return_struct_value_as_ref(x)
	x.f = 91.0625
	d := overwrite_stack()
	assert typeof(y).name == '&NotHeap'
	assert y.f == -17.125
	assert x.f == 91.0625
	assert d == 16.0
}

fn get_int_ref() &int {
	i := 49154
	return &i
}

fn test_int_ref() {
	iptr := get_int_ref()
	assert typeof(iptr).name == '&int'
	d := overwrite_stack()
	assert *iptr == 49154
	assert d == 16.0
}

fn pass_f64_as_ref(f f64) &f64 {
	return &f
}

fn test_value_as_ref() {
	mut x := -31.75
	y := pass_f64_as_ref(x)
	assert typeof(y).name == '&f64'
	x = 23.0625
	d := overwrite_stack()
	assert x == 23.0625
	assert *y == -31.75
	assert d == 16.0
}
