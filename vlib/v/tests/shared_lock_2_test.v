import sync
import time

struct St {
mut:
	a int
}

fn (shared x St) f(shared y St, shared z St) {
	for _ in 0..10001 {
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
	go x.f(shared y, shared z)
	for _ in 0..10000 {
		lock x, y {
			tmp := x.a
			x.a = y.a
			y.a = tmp
		}
	}
	// the following would be a good application for a channel
	for finished := false; ; {
		lock z {
			finished = z.a == 0
		}
		if finished {
			break
		}
		time.sleep_ms(100)
	}
	lock x, y {
		assert x.a == 7 && y.a == 5
	}
}
