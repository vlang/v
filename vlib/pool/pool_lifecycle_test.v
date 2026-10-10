import time
import pool
import sync

@[heap]
struct LifeConn {
mut:
	healthy    bool = true
	close_flag bool
	reset_flag bool
	closed     int
	resets     int
	id         string
}

fn (mut c LifeConn) validate() !bool {
	return c.healthy
}

fn (mut c LifeConn) close() ! {
	if c.close_flag {
		return error('simulated close error')
	}
	c.closed++
}

fn (mut c LifeConn) reset() ! {
	if c.reset_flag {
		return error('simulated reset error')
	}
	c.resets++
}

// life_source returns a factory plus the record of every connection it made.
// The pool hands connections out from the end of the idle pool, so tests use
// min_idle_conns: 0 to make the first created connection the first one handed
// out, and index conns[0] to reach the object the pool is actually holding.
@[heap]
struct LifeSource {
mut:
	conns     []&LifeConn
	fail_next int
	failures  int
	made      int
}

fn life_source(mut src LifeSource) fn () !&pool.ConnectionPoolable {
	return fn [mut src] () !&pool.ConnectionPoolable {
		if src.failures < src.fail_next {
			src.failures++
			return error('connection creation failed')
		}
		src.made++
		mut conn := &LifeConn{
			id: 'c${src.made}'
		}
		src.conns << conn
		return conn
	}
}

fn small_pool(mut src LifeSource) !&pool.ConnectionPool {
	return pool.new_connection_pool(life_source(mut src), pool.ConnectionPoolConfig{
		max_conns:      4
		min_idle_conns: 0
		idle_timeout:   time.minute
		get_timeout:    2 * time.second
	})
}

// wait_for_idle polls until the idle pool holds at least n connections, and
// reports whether that happened within the deadline. A fixed sleep would make
// these tests read as failures on a loaded machine.
fn wait_for_idle(mut p pool.ConnectionPool, n int, budget time.Duration) bool {
	stop_at := time.utc().add(budget)
	for {
		if p.stats().idle_conns >= n {
			return true
		}
		if time.utc() > stop_at {
			return false
		}
		time.sleep(5 * time.millisecond)
	}
	return false
}

fn test_close_is_idempotent_and_clears_every_stat() {
	mut src := &LifeSource{}
	mut p := small_pool(mut src)!
	c := p.get()!
	p.put(c)!
	assert p.stats().total_conns == 1
	assert p.stats().idle_conns == 1

	p.close()
	p.close()

	assert p.stats().total_conns == 0
	assert p.stats().idle_conns == 0
	assert p.stats().active_conns == 0
	assert p.stats().waiting_clients == 0
	assert p.stats().evicted_count == 0
	assert p.stats().creation_errors == 0
	assert p.stats().creating_count == 0
	assert src.conns[0].closed == 1

	if _ := p.get() {
		assert false, 'a closed pool handed out a connection'
	} else {
		assert err.msg() == 'Connection pool closed'
	}
}

fn test_put_an_unmanaged_connection_errors_and_closes_it() {
	mut src := &LifeSource{}
	mut p := small_pool(mut src)!
	defer {
		p.close()
	}
	mut orphan := &LifeConn{
		id: 'orphan'
	}
	if _ := p.put(orphan) {
		assert false, 'an unmanaged connection was accepted'
	} else {
		assert err.msg() == 'Unmanaged connection'
	}
	assert orphan.closed == 1
	assert p.stats().total_conns == 0
}

fn test_put_on_a_closed_pool_closes_the_connection() {
	mut src := &LifeSource{}
	mut p := small_pool(mut src)!
	c := p.get()!
	p.put(c)!
	p.close()
	assert src.conns[0].closed == 1

	// close() already closed it; put() closes the checked-out connection again
	// instead of returning it to the idle pool.
	p.put(c)!
	assert src.conns[0].closed == 2
	assert p.stats().idle_conns == 0
}

fn test_reset_failure_returns_the_error_and_evicts_the_connection() {
	mut src := &LifeSource{}
	mut p := small_pool(mut src)!
	defer {
		p.close()
	}
	c := p.get()!
	assert p.stats().total_conns == 1
	src.conns[0].reset_flag = true

	if _ := p.put(c) {
		assert false, 'put accepted a connection that failed to reset'
	} else {
		assert err.msg() == 'simulated reset error'
	}
	assert src.conns[0].resets == 0
	assert src.conns[0].closed == 1
	assert p.stats().total_conns == 0
	assert p.stats().active_conns == 0
	assert p.stats().idle_conns == 0

	c2 := p.get()!
	assert src.made == 2
	p.put(c2)!
}

fn test_close_failure_while_handling_a_reset_failure_is_not_reported() {
	mut src := &LifeSource{}
	mut p := small_pool(mut src)!
	defer {
		p.close()
	}
	c := p.get()!
	src.conns[0].reset_flag = true
	src.conns[0].close_flag = true

	if _ := p.put(c) {
		assert false, 'put accepted a connection that failed to reset'
	} else {
		assert err.msg() == 'simulated reset error'
	}
	assert p.stats().total_conns == 0
}

fn test_an_invalid_idle_connection_is_discarded_when_getting() {
	mut src := &LifeSource{}
	mut p := small_pool(mut src)!
	defer {
		p.close()
	}
	c := p.get()!
	p.put(c)!
	src.conns[0].healthy = false

	c2 := p.get()!
	assert src.made == 2
	assert src.conns[0].closed == 1
	assert src.conns[1].healthy
	p.put(c2)!
}

fn test_an_expired_connection_is_discarded_when_getting() {
	mut src := &LifeSource{}
	mut p := pool.new_connection_pool(life_source(mut src), pool.ConnectionPoolConfig{
		max_conns:      4
		min_idle_conns: 0
		max_lifetime:   10 * time.millisecond
		idle_timeout:   10 * time.millisecond
		get_timeout:    2 * time.second
	})!
	defer {
		p.close()
	}
	c := p.get()!
	p.put(c)!
	time.sleep(100 * time.millisecond)

	c2 := p.get()!
	assert src.conns[0].closed == 1
	assert src.made == 2
	p.put(c2)!
}

fn test_a_waiting_client_is_woken_when_a_connection_is_returned() {
	mut src := &LifeSource{}
	mut p := pool.new_connection_pool(life_source(mut src), pool.ConnectionPoolConfig{
		max_conns:      2
		min_idle_conns: 0
		idle_timeout:   time.minute
		get_timeout:    2 * time.second
	})!
	defer {
		p.close()
	}
	first := p.get()!
	second := p.get()!
	assert p.stats().active_conns == 2

	mut wg := sync.new_waitgroup()
	wg.add(1)
	spawn fn (mut p pool.ConnectionPool, mut wg sync.WaitGroup) {
		defer {
			wg.done()
		}
		c := p.get() or { panic(err) }
		p.put(c) or { panic(err) }
	}(mut p, mut wg)

	time.sleep(150 * time.millisecond)
	// Polled rather than slept-for: the goroutine may not have reached the
	// wait queue yet when the fixed sleep above expires on a loaded machine.
	for _ in 0 .. 400 {
		if p.stats().waiting_clients == 1 {
			break
		}
		time.sleep(5 * time.millisecond)
	}
	assert p.stats().waiting_clients == 1
	assert p.stats().active_conns == 2

	p.put(first)!
	for _ in 0 .. 400 {
		if p.stats().waiting_clients == 0 {
			break
		}
		time.sleep(5 * time.millisecond)
	}
	assert p.stats().waiting_clients == 0
	wg.wait()

	assert p.stats().active_conns == 1
	assert p.stats().total_conns == 2
	p.put(second)!
}

fn test_retry_exhaustion_reports_the_underlying_factory_error() {
	mut src := &LifeSource{
		fail_next: 100
	}
	mut p := pool.new_connection_pool(life_source(mut src), pool.ConnectionPoolConfig{
		min_idle_conns:     0
		max_retry_attempts: 0
		retry_base_delay:   time.millisecond
		idle_timeout:       time.minute
	})!
	defer {
		p.close()
	}

	if _ := p.get() {
		assert false, 'a failing factory produced a connection'
	} else {
		assert err.msg() == 'Connection creation failed after 0 attempts: connection creation failed'
	}
	assert src.made == 0
	assert p.stats().creation_errors == 0
	assert p.stats().total_conns == 0
}

fn test_creation_failures_are_counted_up_to_the_retry_limit() {
	mut src := &LifeSource{
		fail_next: 2
	}
	mut p := pool.new_connection_pool(life_source(mut src), pool.ConnectionPoolConfig{
		min_idle_conns:     0
		max_retry_attempts: 5
		retry_base_delay:   time.millisecond
		idle_timeout:       time.minute
	})!
	defer {
		p.close()
	}
	c := p.get()!
	assert src.made == 1
	assert p.stats().creation_errors == 2
	p.put(c)!
}

fn test_recovery_and_every_eviction_priority_leave_the_pool_usable() {
	mut src := &LifeSource{}
	mut p := pool.new_connection_pool(life_source(mut src), pool.ConnectionPoolConfig{
		max_conns:      4
		min_idle_conns: 1
		idle_timeout:   time.minute
	})!
	defer {
		p.close()
	}
	for priority in [pool.EvictionPriority.low, .medium, .high, .urgent] {
		p.send_eviction(priority)
	}
	p.signal_recovery_event()
	for _ in 0 .. 400 {
		if p.stats().total_conns >= 1 {
			break
		}
		time.sleep(5 * time.millisecond)
	}

	c := p.get()!
	assert p.stats().active_conns == 1
	p.put(c)!
	assert p.stats().active_conns == 0
}

fn test_stats_reports_the_pool_timestamp_and_no_in_flight_creations() {
	mut src := &LifeSource{}
	mut p := small_pool(mut src)!
	defer {
		p.close()
	}
	before := time.utc()
	stats := p.stats()
	assert stats.created_at.unix() > 0
	assert stats.created_at.unix() >= before.unix() - 1
	assert stats.creating_count == 0
}

fn test_eviction_priority_enum_names() {
	assert pool.EvictionPriority.low.str() == 'low'
	assert pool.EvictionPriority.medium.str() == 'medium'
	assert pool.EvictionPriority.high.str() == 'high'
	assert pool.EvictionPriority.urgent.str() == 'urgent'
}
