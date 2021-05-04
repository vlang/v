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

fn main() {
	a := Abc{ n: 3 }
	b := St{
		Abc{ n: 7 }
	}
	x := Qwe{
		f: 12.25
		a: Abc{ n: 23 }
	}
	println('$a.n $b.n $x.a.n')
}
