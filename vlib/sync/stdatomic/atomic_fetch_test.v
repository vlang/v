import sync
import sync.stdatomic

fn test_fetch_add_u64_returns_the_previous_value() {
	mut c := u64(5)
	assert stdatomic.fetch_add_u64(&c, 3) == 5
	assert c == 8
	assert stdatomic.fetch_add_u64(&c, 0) == 8
	assert c == 8
}

fn test_fetch_add_u64_wraps_at_the_unsigned_boundary() {
	mut c := u64(0xffff_ffff_ffff_fffe)
	assert stdatomic.fetch_add_u64(&c, 1) == 0xffff_ffff_ffff_fffe
	assert c == 0xffff_ffff_ffff_ffff
	assert stdatomic.fetch_add_u64(&c, 1) == 0xffff_ffff_ffff_ffff
	assert c == 0
}

fn test_fetch_sub_u64_returns_the_previous_value() {
	mut c := u64(9)
	assert stdatomic.fetch_sub_u64(&c, 4) == 9
	assert c == 5
	assert stdatomic.fetch_sub_u64(&c, 0) == 5
	assert c == 5
}

fn test_fetch_sub_u64_wraps_below_zero() {
	mut c := u64(1)
	assert stdatomic.fetch_sub_u64(&c, 1) == 1
	assert c == 0
	assert stdatomic.fetch_sub_u64(&c, 1) == 0
	assert c == 0xffff_ffff_ffff_ffff
}

// `add_u64` returns the new value while `fetch_add_u64` returns the old one,
// so the two must differ by exactly the delta.
fn test_fetch_add_u64_differs_from_add_u64_by_the_delta() {
	mut a := u64(100)
	mut b := u64(100)
	added := stdatomic.add_u64(&a, 25)
	fetched := stdatomic.fetch_add_u64(&b, 25)
	assert added == 125
	assert fetched == 100
	assert added - fetched == 25
	assert a == b
}

fn test_fetch_sub_u64_differs_from_sub_u64_by_the_delta() {
	mut a := u64(100)
	mut b := u64(100)
	subtracted := stdatomic.sub_u64(&a, 25)
	fetched := stdatomic.fetch_sub_u64(&b, 25)
	assert subtracted == 75
	assert fetched == 100
	assert fetched - subtracted == 25
	assert a == b
}

fn test_fetch_add_and_sub_round_trip() {
	mut c := u64(0)
	for i in 0 .. 64 {
		assert stdatomic.fetch_add_u64(&c, 1) == u64(i)
	}
	assert c == 64
	for i in 0 .. 64 {
		assert stdatomic.fetch_sub_u64(&c, 1) == u64(64 - i)
	}
	assert c == 0
}

// Four threads doing the same number of increments must not lose any of them,
// which is the property the fetch_* variants exist to provide.
fn test_fetch_add_u64_is_atomic_across_threads() {
	mut c := u64(0)
	mut wg := sync.new_waitgroup()
	wg.add(4)
	for _ in 0 .. 4 {
		spawn bump(&c, 10_000, mut wg)
	}
	wg.wait()
	assert c == 40_000
}

fn bump(c &u64, times int, mut wg sync.WaitGroup) {
	for _ in 0 .. times {
		stdatomic.fetch_add_u64(c, 1)
	}
	wg.done()
}
