import sync
import time

struct St {
mut:
	a int
}

fn (shared x St) z(shared y St) {
	for _ in 0..10001 {
		lock x, y {
			tmp := y.a
			y.a = x.a
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
	go x.z(shared y)
	for _ in 0..10000 {
		lock x, y {
			tmp := x.a
			x.a = y.a
			y.a = tmp
		}
	}
	time.sleep_ms(600)
	lock x, y {
		assert x.a == 7 && y.a == 5
	}
}
