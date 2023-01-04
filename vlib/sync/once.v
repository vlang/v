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

// do executes the function `f()` only once
pub fn (mut o Once) do(f fn ()) {
	if stdatomic.load_u64(&o.count) < 1 {
		o.do_slow(f)
	}
}

fn (mut o Once) do_slow(f fn ()) {
	o.m.@lock()
	if o.count < 1 {
		stdatomic.store_u64(&o.count, 1)
		f()
	}
	o.m.unlock()
}

// do_with_param executes `f(param)` only once`
// This method can be used as a workaround for passing closures to once.do/1 on Windows
// (they are not implemented there yet) - just pass your data explicitly.
// i.e. instead of:
// ```v
//    once.do(fn [mut o] () {
//        o.add(5)
// })
// ```
// ... you can use:
// ```v
//    once.do_with_param(fn (mut o One) {
//        o.add(5)
//    }, o)
// ```

pub fn (mut o Once) do_with_param(f fn (voidptr), param voidptr) {
	if stdatomic.load_u64(&o.count) < 1 {
		o.do_slow_with_param(f, param)
	}
}

fn (mut o Once) do_slow_with_param(f fn (p voidptr), param voidptr) {
	o.m.@lock()
	if o.count < 1 {
		stdatomic.store_u64(&o.count, 1)
		f(param)
	}
	o.m.unlock()
}
