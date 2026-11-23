struct St {
mut:
	a int
}

fn (shared x St) f(shared y St, shared z St) {
	for _ in 0 .. 101 {
		lock x, y {
			tmp := y.a
			y.a = x.a
			x.a = tmp
		}
	}
	lock z {
		z.a--
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
	worker := spawn x.f(shared y, shared z)
	for _ in 0 .. 100 {
		lock x, y {
			tmp := x.a
			x.a = y.a
			y.a = tmp
		}
	}
	worker.wait()
	rlock z {
		assert z.a == 0
	}
	rlock x, y {
		assert x.a == 7 && y.a == 5
	}
}
