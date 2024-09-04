struct Struct {
	f fn () ?[2]int
	g fn () [2]int
}

fn test_struct_member() {
	s := Struct{
		f: fn () ?[2]int {
			return [1, 2]!
		}
		g: fn () [2]int {
			return [1, 2]!
		}
	}

	mut a := s.f()
	println(s.f())
	dump(a)
	mut b := s.g()
	println(s.g())
	dump(b)
}

fn test_fn_var() {
	mut h := fn () [2]int {
		return [1, 2]!
	}
	mut i := fn () ?[2]int {
		return [1, 2]!
	}

	mut c := h()
	println(h())
	dump(c)
	mut d := i()
	println(i())
	dump(d)
}
