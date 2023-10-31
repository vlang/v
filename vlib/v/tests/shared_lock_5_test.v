struct St {
mut:
	a int
}

fn (shared x St) f(shared y St, shared z St) {
	for _ in 0 .. 10000 {
		lock x, y, z {
			tmp := z.a
			z.a = y.a
			y.a = x.a
			x.a = tmp
		}
	}
}

fn (shared x St) g(shared y St, shared z St) {
	for _ in 0 .. 10000 {
		lock z, x, y {
			tmp := x.a
			x.a = z.a
			z.a = y.a
			y.a = tmp
		}
	}
}

fn h(shared x St, shared y St, shared z St) {
	for _ in 0 .. 10000 {
		lock y, x, z {
			tmp := y.a
			y.a = z.a
			z.a = x.a
			x.a = tmp
		}
	}
}

fn test_shared_receiver_lock() {
	shared x := &St{
		a: 5
	}
	shared y := &St{
		a: 7
	}
	shared z := &St{
		a: 1
	}
	t1 := spawn x.f(shared y, shared z)
	t2 := spawn x.f(shared y, shared z)
	t3 := spawn h(shared x, shared y, shared z)
	for _ in 0 .. 10000 {
		lock z, y, x {
			tmp := y.a
			y.a = x.a
			x.a = z.a
			z.a = tmp
		}
	}
	t1.wait()
	t2.wait()
	t3.wait()
	rlock x, y, z {
		assert x.a == 7
		assert y.a == 1
		assert z.a == 5
	}
}
