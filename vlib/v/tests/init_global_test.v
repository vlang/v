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
)

fn init() {
	// semaphores must not be moved in memory, so they cannot have
	// a "default constructor" for technical reasons at the moment
	sem.init(0)
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
