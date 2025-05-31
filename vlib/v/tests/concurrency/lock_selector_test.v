struct St {
mut:
	x f64
}

fn (s &St) get_f64() f64 {
	return s.x
}

struct Gen {
	s shared St
}

fn (g &Gen) set_val() bool {
	lock g.s {
		g.s.x = 6.25
		if g.s.x == 6.25 {
			return true
		}
		g.s.x == 7.125
	}
	return false
}

fn (g &Gen) inc_val() {
	shared q := St{
		x: 1.0
	}
	shared v := St{
		x: 0.25
	}
	lock q, g.s, v {
		g.s.x += q.x
		g.s.x += v.x
	}
}

fn test_lock_selector_expression() {
	g := Gen{
		s: St{
			x: 12.5
		}
	}
	g.set_val()
	g.inc_val()
	a := rlock g.s {
		g.s.get_f64()
	}
	assert a == 7.5
}
