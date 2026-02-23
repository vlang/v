// vtest build: !(macos || windows)

module atomics

fn test_cas_i32_basic() {
	mut x := i32(10)
	ok := cas_i32(&x, 10, 20)
	assert ok == true
	assert x == 20
}

fn test_cas_fail() {
	mut x := i32(5)
	assert !cas_i32(&x, 10, 42)
	assert x == 5
}

fn test_cas_fail_memory_unchanged() {
	mut x := i32(7)
	cas_i32(&x, 1, 2)
	assert x == 7
}

fn test_cas_exact_match() {
	mut x := i32(-1)
	assert !cas_i32(&x, 0, 999)
	assert x == -1
}

fn test_cas_twice() {
	mut x := i32(1)
	assert cas_i32(&x, 1, 2)
	assert cas_i32(&x, 2, 3)
	assert x == 3
}

fn test_cas_with_negative() {
	mut x := i32(-123)
	assert cas_i32(&x, -123, 8)
	assert x == 8
}

fn test_add_i32_basic() {
	mut x := i32(0)
	for _ in 0 .. 1000 {
		add_i32(&x, 1)
	}
	assert x == 1000
}

fn test_add_i32_negative() {
	mut x := i32(10)
	add_i32(&x, -3)
	assert x == 7
}

fn test_add_i32_return_value() {
	mut x := i32(5)
	r := add_i32(&x, 7)
	assert r == 12
	assert x == 12
}

fn test_add_i32_overflow_wraps() {
	mut x := i32(2147483647)
	add_i32(&x, 1)
	assert x == -2147483648
}

fn test_swap_i32_basic() {
	mut x := i32(5)
	old := swap_i32(&x, 99)
	assert old == 5
	assert x == 99
}

fn test_swap_i32_twice() {
	mut x := i32(1)
	assert swap_i32(&x, 2) == 1
	assert swap_i32(&x, 3) == 2
	assert x == 3
}

fn test_swap_i32_with_cas() {
	mut x := i32(10)
	assert cas_i32(&x, 10, 20)
	old := swap_i32(&x, 30)
	assert old == 20
	assert x == 30
}

fn test_load_i32_basic() {
	mut x := i32(123456)
	assert load_i32(&x) == 123456
}

fn test_store_i32_basic() {
	mut x := i32(5)
	store_i32(&x, 777)
	assert x == 777
}

fn test_add_i32_concurrent() {
	mut x := i32(0)
	mut threads := []thread{}

	for _ in 0 .. 9 {
		threads << spawn fn (ptr &i32) {
			for _ in 0 .. 100_000 {
				add_i32(ptr, 1)
			}
		}(&x)
	}

	for t in threads {
		t.wait()
	}

	assert x == 900_000
}

fn test_swap_i32_concurrent() {
	mut x := i32(0)
	mut threads := []thread{}

	for _ in 0 .. 8 {
		threads << spawn fn (ptr &i32) {
			for _ in 0 .. 50_000 {
				swap_i32(ptr, 123)
			}
		}(&x)
	}

	for t in threads {
		t.wait()
	}

	assert x == 123
}

fn test_cas_i32_concurrent() {
	mut x := i32(0)
	mut threads := []thread{}

	for _ in 0 .. 8 {
		threads << spawn fn (ptr &i32) {
			for _ in 0 .. 100_000 {
				for {
					old := load_i32(ptr)
					if cas_i32(ptr, old, old + 1) {
						break
					}
				}
			}
		}(&x)
	}

	for t in threads {
		t.wait()
	}

	assert x == 800_000
}

fn test_load_store_i32_concurrent() {
	mut x := i32(0)
	mut threads := []thread{}

	for _ in 0 .. 4 {
		threads << spawn fn (px &i32) {
			for _ in 0 .. 50_000 {
				add_i32(px, 1)
			}
		}(&x)
	}
	for _ in 0 .. 4 {
		threads << spawn fn (px &i32) {
			for _ in 0 .. 50_000 {
				_ = load_i32(px)
			}
		}(&x)
	}

	for t in threads {
		t.wait()
	}

	assert x == 4 * 50_000
}

fn test_and_i32_concurrent() {
	mut x := i32(0x7fffffff)
	mut threads := []thread{}
	for _ in 0 .. 8 {
		threads << spawn fn (px &i32) {
			for _ in 0 .. 100_000 {
				and_i32(px, 0x0fffffff)
			}
		}(&x)
	}
	for t in threads {
		t.wait()
	}
	assert x == 0x0fffffff
}

fn test_or_i32_concurrent() {
	mut x := i32(0)
	mut threads := []thread{}
	for _ in 0 .. 8 {
		threads << spawn fn (px &i32) {
			for _ in 0 .. 100_000 {
				or_i32(px, 0x12345678)
			}
		}(&x)
	}
	for t in threads {
		t.wait()
	}
	assert x == 0x12345678
}
