// integer values from -2^191 .. 2^191-1
struct Large {
mut:
	l u64
	m u64
	h u64
}

fn (a Large) clone() Large {
	r := Large{
		l: a.l
		m: a.m
		h: a.h
	}
	return r
}

fn (mut a Large) add(b Large) {
	oldl := a.l
	a.l += b.l
	oldm := a.m
	if a.l < oldl {
		a.m++
	}
	a.m += b.m
	if a.m < oldm || (a.l < oldl && a.m <= oldm) {
		a.h++
	}
	a.h += b.h
}

fn doub_large(shared a Large, shared b Large, shared c Large, shared d Large, shared e Large) {
	for _ in 0 .. 50 {
		lock a, b; rlock c, d, e {
			// double the sum by adding all objects to a or b
			old_a := a.clone()
			a.add(b)
			b.add(old_a)
			a.add(c)
			b.add(d)
			a.add(e)
		}
	}
}

fn test_mixed_order_lock_rlock() {
	// initialze objects so that their sum = 1
	shared a := Large{
		l: 4
	}
	shared b := Large{
		l: u64(-7)
		m: u64(-1)
		h: u64(-1)
	}
	shared c := Large{
		l: 17
	}
	shared d := Large{
		l: u64(-11)
		m: u64(-1)
		h: u64(-1)
	}
	shared e := Large{
		l: u64(-2)
		m: u64(-1)
		h: u64(-1)
	}
	// spawn 3 threads for doubling with different orders of objects
	t1 := spawn doub_large(shared a, shared b, shared c, shared d, shared e)
	t2 := spawn doub_large(shared e, shared b, shared a, shared c, shared d)
	t3 := spawn doub_large(shared b, shared a, shared e, shared d, shared c)
	t1.wait()
	t2.wait()
	t3.wait()
	// calculate the sum after 3*50 doublings
	mut sum := Large{}
	rlock a, b, c, d, e {
		sum.add(a)
		sum.add(b)
		sum.add(c)
		sum.add(d)
		sum.add(e)
	}
	// the sum should be 2^150
	assert sum.l == 0
	assert sum.m == 0
	assert sum.h == 4194304
}
