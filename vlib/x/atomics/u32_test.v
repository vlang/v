// vtest build: !(macos || windows)

module atomics

fn test_add_u32_basic() {
	mut x := u32(0)
	for _ in 0 .. 1000 {
		add_u32(&x, 1)
	}
	assert x == 1000
}

fn test_add_u32_wraparound() {
	mut x := u32(0xffffffff)
	add_u32(&x, 1)
	assert x == 0
}

fn test_add_u32_large() {
	mut x := u32(10)
	r := add_u32(&x, 1000)
	assert r == 1010
	assert x == 1010
}

fn test_swap_u32_basic() {
	mut x := u32(123)
	old := swap_u32(&x, 999)
	assert old == 123
	assert x == 999
}

fn test_swap_u32_same() {
	mut x := u32(777)
	old := swap_u32(&x, 777)
	assert old == 777
	assert x == 777
}

fn test_store_u32_basic() {
	mut x := u32(1)
	store_u32(&x, 555)
	assert x == 555
}

fn test_load_u32_basic() {
	mut x := u32(888)
	assert load_u32(&x) == 888
}

fn test_cas_u32_basic() {
	mut x := u32(10)
	assert cas_u32(&x, 10, 50)
	assert x == 50
}

fn test_cas_u32_fail() {
	mut x := u32(10)
	assert !cas_u32(&x, 5, 999)
	assert x == 10
}

fn test_cas_u32_nochange_on_fail() {
	mut x := u32(777)
	cas_u32(&x, 5, 9)
	assert x == 777
}

fn test_cas_u32_boundary() {
	mut x := u32(0xffffffff)
	assert cas_u32(&x, 0xffffffff, 0)
	assert x == 0
}

fn test_add_u32_concurrent() {
	mut x := u32(0)
	mut threads := []thread{}

	for _ in 0 .. 8 {
		threads << spawn fn (px &u32) {
			for _ in 0 .. 100_000 {
				add_u32(px, 1)
			}
		}(&x)
	}

	for t in threads {
		t.wait()
	}

	assert x == 800_000
}

fn test_swap_u32_concurrent() {
	mut x := u32(0)
	mut threads := []thread{}

	for _ in 0 .. 8 {
		threads << spawn fn (px &u32) {
			for _ in 0 .. 50_000 {
				swap_u32(px, 123)
			}
		}(&x)
	}

	for t in threads {
		t.wait()
	}

	assert x == 123
}

fn test_cas_u32_concurrent_inc() {
	mut x := u32(0)
	mut threads := []thread{}

	for _ in 0 .. 8 {
		threads << spawn fn (px &u32) {
			for _ in 0 .. 50_000 {
				for {
					old := load_u32(px)
					if cas_u32(px, old, old + 1) {
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

fn test_cas_u32_contended_flip() {
	mut x := u32(0)
	mut threads := []thread{}

	for _ in 0 .. 4 {
		threads << spawn fn (px &u32) {
			for _ in 0 .. 200_000 {
				cas_u32(px, 0, 1)
				cas_u32(px, 1, 0)
			}
		}(&x)
	}

	for t in threads {
		t.wait()
	}

	assert x == 0 || x == 1
}

fn test_load_store_u32_concurrent() {
	mut x := u32(0)
	mut threads := []thread{}

	for i in 0 .. 8 {
		threads << spawn fn (px &u32, id int) {
			for _ in 0 .. 50_000 {
				if id % 2 == 0 {
					store_u32(px, 1)
				} else {
					_ = load_u32(px)
				}
			}
		}(&x, i)
	}

	for t in threads {
		t.wait()
	}

	assert x == 1
}

fn test_and_u32_concurrent() {
	mut x := u32(0xffffffff)
	mut threads := []thread{}
	for _ in 0 .. 8 {
		threads << spawn fn (px &u32) {
			for _ in 0 .. 100_000 {
				and_u32(px, 0x0f0f0f0f)
			}
		}(&x)
	}
	for t in threads {
		t.wait()
	}
	assert x == 0x0f0f0f0f
}

fn test_or_u32_concurrent() {
	mut x := u32(0)
	mut threads := []thread{}
	for _ in 0 .. 8 {
		threads << spawn fn (px &u32) {
			for _ in 0 .. 100_000 {
				or_u32(px, 0xdeadbeef)
			}
		}(&x)
	}
	for t in threads {
		t.wait()
	}
	assert x == u32(0xdeadbeef)
}
