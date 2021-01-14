import time

struct St {
mut:
	x f64
}

fn f(x int, y f64, shared s St) {
	time.usleep(50000)
	lock s {
		s.x = x * y
	}
	return
}

fn test_go_return() {
	shared t := &St{}
	r := go f(3, 4.0, shared t)
	r.wait()
	rlock t {
		assert t.x == 12.0
	}
}
