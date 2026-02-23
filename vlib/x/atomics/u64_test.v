// vtest build: !(macos || windows)

module atomics

fn test_load_u64_basic() {
	mut x := u64(1234567890123)
	assert load_u64(&x) == 1234567890123
}

fn test_store_u64_basic() {
	mut x := u64(1)
	store_u64(&x, 999999)
	assert x == 999999
}

fn test_swap_u64_basic() {
	mut x := u64(123)
	old := swap_u64(&x, 999)
	assert old == 123
	assert x == 999
}

fn test_swap_u64_same() {
	mut x := u64(777777)
	old := swap_u64(&x, 777777)
	assert old == 777777
	assert x == 777777
}

fn test_add_u64_basic() {
	mut x := u64(0)
	for _ in 0 .. 1000 {
		add_u64(&x, 2)
	}
	assert x == 2000
}

fn test_add_u64_wraparound() {
	mut x := u64(0xffffffffffffffff)
	add_u64(&x, 1)
	assert x == 0
}

fn test_add_u64_return() {
	mut x := u64(10)
	r := add_u64(&x, 100)
	assert r == 110
	assert x == 110
}

fn test_cas_u64_basic() {
	mut x := u64(111)
	assert cas_u64(&x, 111, 222)
	assert x == 222
}

fn test_cas_u64_fail() {
	mut x := u64(500)
	assert !cas_u64(&x, 100, 200)
	assert x == 500
}

fn test_cas_u64_nochange_fail() {
	mut x := u64(999)
	cas_u64(&x, 1, 2)
	assert x == 999
}

fn test_cas_u64_boundary() {
	mut x := u64(0xffffffffffffffff)
	assert cas_u64(&x, 0xffffffffffffffff, 0)
	assert x == 0
}

fn test_add_u64_concurrent() {
	mut x := u64(0)
	mut threads := []thread{}

	for _ in 0 .. 8 {
		threads << spawn fn (px &u64) {
			for _ in 0 .. 100_000 {
				add_u64(px, 1)
			}
		}(&x)
	}

	for t in threads {
		t.wait()
	}

	assert x == 800_000
}

fn test_swap_u64_concurrent() {
	mut x := u64(0)
	mut threads := []thread{}

	for _ in 0 .. 8 {
		threads << spawn fn (px &u64) {
			for _ in 0 .. 50_000 {
				swap_u64(px, 123456)
			}
		}(&x)
	}

	for t in threads {
		t.wait()
	}

	assert x == 123456
}

fn test_cas_u64_concurrent_inc() {
	mut x := u64(0)
	mut threads := []thread{}

	for _ in 0 .. 8 {
		threads << spawn fn (px &u64) {
			for _ in 0 .. 50_000 {
				for {
					old := load_u64(px)
					if cas_u64(px, old, old + 1) {
						break
					}
				}
			}
		}(&x)
	}

	for t in threads {
		t.wait()
	}

	assert x == 400_000
}

fn test_cas_u64_contended_flip() {
	mut x := u64(0)
	mut threads := []thread{}

	for _ in 0 .. 4 {
		threads << spawn fn (px &u64) {
			for _ in 0 .. 200_000 {
				cas_u64(px, 0, 1)
				cas_u64(px, 1, 0)
			}
		}(&x)
	}

	for t in threads {
		t.wait()
	}

	assert x == 0 || x == 1
}

fn test_and_u64_concurrent() {
	mut x := u64(0xffffffffffffffff)
	mut threads := []thread{}
	for _ in 0 .. 8 {
		threads << spawn fn (px &u64) {
			for _ in 0 .. 100_000 {
				and_u64(px, 0x00ff00ff00ff00ff)
			}
		}(&x)
	}
	for t in threads {
		t.wait()
	}
	assert x == 0x00ff00ff00ff00ff
}

fn test_or_u64_concurrent() {
	mut x := u64(0)
	mut threads := []thread{}
	for _ in 0 .. 8 {
		threads << spawn fn (px &u64) {
			for _ in 0 .. 100_000 {
				or_u64(px, 0xfedcba9876543210)
			}
		}(&x)
	}
	for t in threads {
		t.wait()
	}
	assert x == u64(0xfedcba9876543210)
}
