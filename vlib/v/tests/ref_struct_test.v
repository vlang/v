[heap]
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

struct Rtz {
mut:
	f f64
	a &Abc
}

fn test_ref() {
	a := &Abc{
		n: 3
	}
	assert a.n == 3
}

fn test_shared() {
	shared a := Abc{
		n: 4
	}
	res := rlock a {
		a.n
	}
	assert res == 4
}

fn test_embed_in_ref_struct() {
	a := &St{Abc{
		n: 5
	}}
	assert a.n == 5
}

fn test_field_in_ref_struct() {
	x := &Qwe{
		f: 12.25
		a: Abc{
			n: 23
		}
	}
	assert x.a.n == 23
}

fn test_ref_field() {
	y := Rtz{
		f: -6.25
		a: &Abc{
			n: 29
		}
	}
	assert y.a.n == 29
}

// Yxc should become [heap] implicitly because `Abc` is

struct Yxc {
mut:
	a []Abc
}

fn mod_heap_array(mut x Yxc) {
	x.a << Abc{
		n: 9
	}
	x.a[1] = Abc{
		n: 13
	}
}

fn test_heap_array() {
	mut z := &Yxc{
		a: [
			Abc{
				n: 4
			},
			Abc{
				n: 7
			},
		]
	}
	mod_heap_array(mut z)
	assert z.a[1].n == 13
}
