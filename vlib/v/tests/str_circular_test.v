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

struct Window {
mut:
	widgets []Widget
}

struct Widget {
mut:
	parent &Window = voidptr(0)
}

fn test_circular_2() {
	mut window := &Window{}

	mut btn := &Widget{}
	btn.parent = window

	window.widgets << btn

	dump(window)
	assert '$window'.len < 100
}
