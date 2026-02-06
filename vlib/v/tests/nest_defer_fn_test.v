struct Fst {
mut:
	f fn () int
}

fn setfn(mut f Fst) {
	mut i := 0
	f.f = fn () int {
		return 5
	}
	defer {
		if i > 0 {
			f.f = fn () int {
				a := 0
				defer {
					assert a == 0
				}
				return 7
			}
		}
	}
	i = 1
}

fn test_nested_defer() {
	mut g := Fst{}
	setfn(mut g)
	x := g.f()
	assert x == 7
}
