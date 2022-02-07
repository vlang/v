[heap]
struct Aa {
mut:
	bs []Bb
}

struct Bb {
mut:
	a &Aa
}

fn test_circular_1() {
	mut b := Bb{
		a: &Aa{
			bs: []Bb{cap: 1}
		}
	}
	b.a.bs << b
	s := b.str()

	dump(b)
	assert s.len < 100
}
