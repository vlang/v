struct St {
mut:
	a int
}

fn (shared x St) f(shared y St, shared z St) {
	for _ in 0 .. 10000 {
		lock x; rlock y, z {
			x.a = y.a + z.a
			if x.a > 1000000 {
				x.a /= 2
			}
		}
	}
}

fn (shared x St) g(shared y St, shared z St) {
	for _ in 0 .. 10000 {
		rlock x; lock y, z {
			y.a += x.a
			if y.a > 1000000 {
				y.a /= 2
			}
			z.a += x.a
			if z.a > 1000000 {
				z.a /= 2
			}
		}
	}
}

fn test_shared_receiver_lock() {
	shared x := St{
		a: 5
	}
	shared y := St{
		a: 7
	}
	shared z := St{
		a: 103
	}
	t1 := go x.f(shared y, shared x)
	t2 := go y.f(shared y, shared z)
	t3 := go z.g(shared x, shared z)
	t4 := go x.g(shared x, shared x)
	t5 := go z.f(shared z, shared z)
	t1.wait()
	t2.wait()
	t3.wait()
	t4.wait()
	t5.wait()
	// the result is unpredictable - but should be positive
	rlock x, y, z {
		assert x.a > 10000
		assert y.a > 10000
		assert z.a > 10000
		println('$x.a $y.a $z.a')
	}
}
