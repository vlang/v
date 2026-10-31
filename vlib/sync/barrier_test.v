import sync
import sync.stdatomic { new_atomic }

// All threads must pass the barrier on every round.
fn test_barrier_all_threads_pass_each_round() {
	n := 4
	rounds := 5
	mut b := sync.new_barrier(n)
	mut passed := new_atomic(0)
	done_ch := chan bool{cap: n}
	defer {
		done_ch.close()
	}
	for _ in 0 .. n {
		spawn fn [mut b, mut passed, rounds, done_ch] () {
			for _ in 0 .. rounds {
				_ = b.wait()
				passed.add(1)
			}
			done_ch <- true
		}()
	}
	for _ in 0 .. n {
		_ := <-done_ch
	}
	assert passed.load() == n * rounds, 'all threads must pass the barrier every round'
}

// Exactly one thread is the leader per round.
fn test_barrier_single_leader_per_round() {
	n := 4
	rounds := 5
	mut b := sync.new_barrier(n)
	mut leaders := new_atomic(0)
	done_ch := chan bool{cap: n}
	defer {
		done_ch.close()
	}
	for _ in 0 .. n {
		spawn fn [mut b, mut leaders, rounds, done_ch] () {
			for _ in 0 .. rounds {
				if b.wait() {
					leaders.add(1)
				}
			}
			done_ch <- true
		}()
	}
	for _ in 0 .. n {
		_ := <-done_ch
	}
	assert leaders.load() == rounds, 'exactly one leader per round'
}

// A barrier of one passes immediately and reports leadership.
fn test_barrier_single_thread() {
	mut b := sync.new_barrier(1)
	assert b.wait() == true, 'single-thread barrier must pass and lead'
	assert b.wait() == true, 'single-thread barrier must be reusable'
}
