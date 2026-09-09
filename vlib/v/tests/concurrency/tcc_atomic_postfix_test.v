// vtest build: tinyc && linux
// vtest vflags: -cc tcc -no-retry-compilation
module main

struct Counter {
mut:
	value atomic int
}

fn (mut c Counter) add_many(n int) {
	for _ in 0 .. n {
		c.value++
	}
}

fn test_tcc_atomic_postfix_is_atomic() {
	mut counter := &Counter{}
	mut threads := []thread{cap: 8}
	for _ in 0 .. 8 {
		threads << spawn counter.add_many(200_000)
	}
	threads.wait()
	assert counter.value == 1_600_000
}
