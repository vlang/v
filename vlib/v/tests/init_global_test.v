import sync

fn one() int {
	return 1
}

fn pushf64() {
	ch <- 12.5
}

fn test_global_init() {
	intmap['two'] = 27
	key := 'two'
	assert intmap[key] == 27
	t := go pushf64()
	numberfns['one'] = one
	numberfns['two'] = fn () int {
		return 2
	}
	f := numberfns['one']
	n := f()
	assert n == 1
	m := numberfns['two']()
	assert m == 2
	got := <-ch
	assert got == 12.5
	t.wait()
	assert true
}

__global (
	intmap    map[string]int
	numberfns map[string]fn () int
	ch        chan f64
	mys       shared MyStruct
	sem       sync.Semaphore
	shmap     shared map[string]f64
	mtx       sync.RwMutex
	f1        = f64(34.0625)
	f2        f64
)

fn init() {
	// semaphores and mutexes must not be moved in memory, so for technical
	// reasons they cannot have a "default constructor" at the moment and must
	// be initialized "manually"
	sem.init(0)
	mtx.init()
}

struct MyStruct {
mut:
	x f64
	y f64
}

fn switch() {
	for !sem.try_wait() {
		lock mys {
			if mys.x == 13.0 {
				mys.x = 13.75
			} else if mys.y == 13.0 {
				mys.y = 13.75
			}
		}
		lock mys {
			mys.x, mys.y = mys.y, mys.x
		}
	}
}

fn test_global_shared() {
	lock mys {
		mys.x = 13.0
		mys.y = -35.125
	}
	t := go switch()
	for _ in 0 .. 2500000 {
		lock mys {
			mys.x, mys.y = mys.y, mys.x
		}
	}
	a, b := rlock mys {
		mys.x, mys.y
	}
	sem.post()
	t.wait()
	assert (a == 13.75 && b == -35.125) || (a == -35.125 && b == 13.75)
}

fn test_global_shared_map() {
	lock shmap {
		shmap['one'] = 1.25
		shmap['two'] = -0.75
	}
	x, y := rlock shmap {
		shmap['two'], shmap['one']
	}
	assert x == -0.75
	assert y == 1.25
}

fn switch2() u64 {
	mut cnt := u64(0)
	for {
		mtx.@lock()
		f1, f2 = f2, f1
		if f1 == 17.0 || f2 == 17.0 {
			mtx.unlock()
			return cnt
		}
		mtx.unlock()
		cnt++
	}
	return 0
}

fn test_global_mutex() {
	t := go switch2()
	for _ in 0 .. 2500000 {
		mtx.@lock()
		f1, f2 = f2, f1
		mtx.unlock()
	}
	mtx.@lock()
	if f1 == 0.0 {
		f1 = 17.0
	} else {
		f2 = 17.0
	}
	mtx.@rlock()
	assert (f1 == 17.0 && f2 == 34.0625) || (f1 == 34.0625 && f2 == 17.0)
	mtx.runlock()
	n := t.wait()
	assert n > 0
}
