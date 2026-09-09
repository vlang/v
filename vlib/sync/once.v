module sync

import sync.stdatomic

pub struct Once {
mut:
	m RwMutex
pub:
	count u64
}

// new_once return a new Once struct.
pub fn new_once() &Once {
	mut once := &Once{}
	once.m.init()
	return once
}

// do executes the function `f()` only once, regardless of how many goroutines
// call it concurrently. When do returns, f() has completed.
// Note: if f() calls do on the same Once, it will deadlock.
pub fn (mut o Once) do(f fn ()) {
	if stdatomic.load_u64(&o.count) < 1 {
		o.do_slow(f)
	}
}

fn (mut o Once) do_slow(f fn ()) {
	o.m.lock()
	if o.count < 1 {
		f()
		stdatomic.store_u64(&o.count, 1)
	}
	o.m.unlock()
}

// do_with_param executes `f(param)` only once.
// This method can be used as a workaround for passing closures to once.do/1 on Windows
// (they are not implemented there yet) - just pass your data explicitly.
// i.e. instead of:
// ```v
//    once.do(fn [mut o] () {
//        o.add(5)
// })
// ```
//
// ... you can use:
// ```v
//    once.do_with_param(fn (mut o One) {
//        o.add(5)
//    }, o)
// ```

// do_with_param executes the function `f(param)` only once, regardless of how
// many goroutines call it concurrently. When do_with_param returns, f() has
// completed.
// Note: if f() calls do_with_param on the same Once, it will deadlock.
pub fn (mut o Once) do_with_param(f fn (voidptr), param voidptr) {
	if stdatomic.load_u64(&o.count) < 1 {
		o.do_slow_with_param(f, param)
	}
}

fn (mut o Once) do_slow_with_param(f fn (p voidptr), param voidptr) {
	o.m.lock()
	if o.count < 1 {
		f(param)
		stdatomic.store_u64(&o.count, 1)
	}
	o.m.unlock()
}
