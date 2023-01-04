[heap]
struct Aa {
mut:
	bs []Bb
}

struct Bb {
mut:
	a &Aa
}

fn test_circular() {
	mut b := Bb{
		a: &Aa{
			bs: []Bb{cap: 1}
		}
	}
	b.a.bs << b
	s := b.str()
	assert s.len < 3500
}
