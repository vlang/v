module sync

import sync.stdatomic

pub struct ManyTimes {
mut:
	m RwMutex
pub:
	times u64 = 1
	count u64
}

// new_many_times return a new ManyTimes struct.
pub fn new_many_times(times u64) &ManyTimes {
	mut many_times := &ManyTimes{
		times: times
	}
	many_times.m.init()
	return many_times
}

// do execute the function only setting times.
pub fn (mut m ManyTimes) do(f fn ()) {
	if stdatomic.load_u64(&m.count) < m.times {
		m.do_slow(f)
	}
}

fn (mut m ManyTimes) do_slow(f fn ()) {
	m.m.@lock()
	if m.count < m.times {
		stdatomic.store_u64(&m.count, m.count + 1)
		f()
	}
	m.m.unlock()
}
