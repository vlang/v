module ratelimit

// MsClock is a settable millisecond clock for deterministic tests (closures
// capture plain values by copy, so shared state lives behind a reference).
struct MsClock {
mut:
	now i64
}

fn test_token_bucket_burst_then_throttle() {
	mut clk := &MsClock{}
	clock := fn [clk] () i64 {
		return clk.now
	}
	mut b := new_token_bucket_with_clock(TokenBucketConfig{ rate_per_sec: 10.0, burst: 3 },
		clock)
	assert b.allow() == true
	assert b.allow() == true
	assert b.allow() == true
	assert b.allow() == false, 'burst exhausted: must deny'
	assert b.allow() == false
	s := b.stats()
	assert s.allowed == 3 && s.denied == 2
}

fn test_token_bucket_refills_over_time() {
	mut clk := &MsClock{}
	clock := fn [clk] () i64 {
		return clk.now
	}
	mut b := new_token_bucket_with_clock(TokenBucketConfig{ rate_per_sec: 10.0, burst: 2 },
		clock)
	assert b.allow() == true
	assert b.allow() == true
	assert b.allow() == false
	clk.now += 100 // 100ms at 10/s refills exactly 1 token
	assert b.allow() == true
	assert b.allow() == false
	clk.now += 5000 // long idle refills to capacity, never beyond
	assert b.allow() == true
	assert b.allow() == true
	assert b.allow() == false
	s := b.stats()
	assert s.allowed == 5 && s.denied == 3
}

fn test_token_bucket_concurrent_allow() {
	mut b := new_token_bucket(TokenBucketConfig{ rate_per_sec: 1000000.0, burst: 100 })
	done_ch := chan bool{cap: 8}
	defer {
		done_ch.close()
	}
	for _ in 0 .. 8 {
		spawn fn [mut b, done_ch] () {
			for _ in 0 .. 25 {
				b.allow()
			}
			done_ch <- true
		}()
	}
	for _ in 0 .. 8 {
		_ := <-done_ch
	}
	s := b.stats()
	assert s.allowed + s.denied == 200, 'every call must be counted exactly once'
}

fn test_token_bucket_clock_does_not_refill_twice() {
	mut clk := &MsClock{ now: 1000 }
	clock := fn [clk] () i64 { return clk.now }
	mut b := new_token_bucket_with_clock(TokenBucketConfig{ rate_per_sec: 10.0, burst: 1 }, clock)
	assert b.allow()
	clk.now = 900
	assert !b.allow()
	clk.now = 1000
	assert !b.allow()
	clk.now = 1100
	assert b.allow()
	assert !b.allow()
}
