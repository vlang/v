// vtest build: !windows // msvc hung, maybe sync/atomic bug, gcc on windows does too, although less frequently
import time
import sync
import pool
import rand

// Mock connection implementation
struct MockConn {
mut:
	healthy    bool
	close_flag bool
	reset_flag bool
	closed     int
	id         string
}

fn (mut c MockConn) validate() !bool {
	return c.healthy
}

fn (mut c MockConn) close() ! {
	if c.close_flag {
		return error('simulated close error')
	}
	c.closed++
}

fn (mut c MockConn) reset() ! {
	if c.reset_flag {
		return error('simulated reset error')
	}
	c.reset_flag = true
}

// Test utility functions
fn create_mock_factory(mut arr []&pool.ConnectionPoolable, healthy bool, fail_times int) fn () !&pool.ConnectionPoolable {
	mut count := 0
	return fn [mut arr, healthy, fail_times, mut count] () !&pool.ConnectionPoolable {
		if count < fail_times {
			count++
			return error('connection creation failed')
		}
		mut conn := &MockConn{
			healthy: healthy
			id:      rand.uuid_v7()
		}
		arr << conn
		return conn
	}
}

fn is_same_conn(conn1 &pool.ConnectionPoolable, conn2 &pool.ConnectionPoolable) bool {
	c1 := conn1 as MockConn
	c2 := conn2 as MockConn
	return c1.id == c2.id
}

// Test cases
fn test_basic_usage() {
	for _ in 0 .. 1 {
		mut test_conns := []&pool.ConnectionPoolable{}
		factory := create_mock_factory(mut test_conns, true, 0)
		config := pool.ConnectionPoolConfig{
			max_conns:      5
			min_idle_conns: 2
			idle_timeout:   100 * time.millisecond
			get_timeout:    50 * time.millisecond
		}

		mut p := pool.new_connection_pool(factory, config)!

		// Acquire a connection
		mut conn1 := p.get()!
		assert p.stats().active_conns == 1

		// Acquire multiple connections
		mut conns := [p.get()!, p.get()!, p.get()!]
		assert p.stats().active_conns == 4

		// Return connections
		for c in conns {
			p.put(c)!
		}
		p.put(conn1)!
		assert p.stats().active_conns == 0
		assert p.stats().total_conns >= 4
		p.close()
	}
}

fn test_pool_exhaustion() {
	for i in 0 .. 1 {
		mut test_conns := []&pool.ConnectionPoolable{}
		factory := create_mock_factory(mut test_conns, true, 0)
		config := pool.ConnectionPoolConfig{
			max_conns:      2
			min_idle_conns: 1
			get_timeout:    10 * time.millisecond
		}

		mut p := pool.new_connection_pool(factory, config)!

		// Acquire all connections
		c1 := p.get()!
		c2 := p.get()!
		assert p.stats().active_conns == 2

		// Attempt to acquire third connection (should timeout)
		p.get() or { assert err.msg().contains('timeout') }

		// After returning, should be able to acquire again
		p.put(c2)!
		c3 := p.get()!
		assert is_same_conn(c3, c2)
		assert p.stats().active_conns == 2
		p.close()
	}
}

fn test_connection_validation() {
	mut test_conns := []&pool.ConnectionPoolable{}
	factory := create_mock_factory(mut test_conns, false, 0) // Create unhealthy connections
	config := pool.ConnectionPoolConfig{
		min_idle_conns: 1
	}

	mut p := pool.new_connection_pool(factory, config) or {
		assert err.msg().contains('connection validation failed')
		return
	}
	defer {
		p.close()
	}
}

fn test_eviction() {
	mut test_conns := []&pool.ConnectionPoolable{}
	factory := create_mock_factory(mut test_conns, true, 0)
	config := pool.ConnectionPoolConfig{
		max_lifetime:   10 * time.millisecond
		idle_timeout:   10 * time.millisecond
		min_idle_conns: 0
	}

	mut p := pool.new_connection_pool(factory, config)!
	defer {
		p.close()
	}

	// Acquire and return a connection
	c1 := p.get()!
	p.put(c1)!

	// Wait longer than timeout thresholds
	time.sleep(100 * time.millisecond)
	p.send_eviction(.urgent)
	time.sleep(10 * time.millisecond)
	stats := p.stats()
	assert stats.evicted_count > 0
	assert stats.total_conns == 0
	assert stats.idle_conns == 0
	c2 := p.get()!
	assert !is_same_conn(c1, c2)
	assert p.stats().total_conns == 1
	assert p.stats().evicted_count > 0
}

fn test_retry_mechanism() {
	mut test_conns := []&pool.ConnectionPoolable{}
	factory := create_mock_factory(mut test_conns, true, 3) // First 3 attempts fail
	config := pool.ConnectionPoolConfig{
		max_retry_attempts: 5
		retry_base_delay:   10 * time.millisecond
		min_idle_conns:     0 // No idle connections
	}

	mut p := pool.new_connection_pool(factory, config)!
	defer {
		p.close()
	}
	// Should successfully create connection after retries
	conn := p.get()!
	assert test_conns.len == 1
	assert p.stats().creation_errors == 3
}

fn test_concurrent_access() {
	mut test_conns := []&pool.ConnectionPoolable{}
	factory := create_mock_factory(mut test_conns, true, 0)
	config := pool.ConnectionPoolConfig{
		max_conns: 10
	}

	mut p := pool.new_connection_pool(factory, config)!
	defer {
		p.close()
	}
	mut wg := sync.new_waitgroup()

	for _ in 0 .. 20 {
		wg.add(1)
		spawn fn (mut p pool.ConnectionPool, mut wg sync.WaitGroup) ! {
			defer { wg.done() }
			conn := p.get()!
			time.sleep(5 * time.millisecond)
			p.put(conn)!
		}(mut p, mut wg)
	}

	wg.wait()
	stats := p.stats()
	assert stats.total_conns <= 10
	assert stats.active_conns == 0
}

fn test_pool_close() {
	mut test_conns := []&pool.ConnectionPoolable{}
	factory := create_mock_factory(mut test_conns, true, 0)

	mut p := pool.new_connection_pool(factory, pool.ConnectionPoolConfig{})!
	c := p.get()!
	p.put(c)!
	assert p.stats().active_conns == 0
	assert p.stats().idle_conns >= 5

	p.close()

	// Attempt to acquire connection after close
	p.get() or { assert err.msg().contains('closed') }
	assert p.stats().active_conns == 0
	assert p.stats().idle_conns == 0

	// Verify connection was closed
	mock_conn := test_conns[0] as MockConn
	assert mock_conn.closed == 1
}

fn test_config_update() {
	mut dummy := []&pool.ConnectionPoolable{}
	mut p := pool.new_connection_pool(create_mock_factory(mut dummy, true, 0), pool.ConnectionPoolConfig{
		max_conns:      2
		min_idle_conns: 1
	})!
	defer {
		p.close()
	}
	assert p.stats().idle_conns == 1

	// Modify configuration
	new_config := pool.ConnectionPoolConfig{
		max_conns:      5
		min_idle_conns: 3
	}

	p.update_config(new_config)!

	// Trigger idle connection replenishment
	time.sleep(100 * time.millisecond)
	assert p.stats().idle_conns >= 3
}

fn test_error_handling() {
	// Test close error handling
	mut test_conns := []&pool.ConnectionPoolable{}
	factory := create_mock_factory(mut test_conns, true, 0)
	mut p := pool.new_connection_pool(factory, pool.ConnectionPoolConfig{})!
	defer {
		p.close()
	}

	// default configuration has 5 idle_conns
	assert p.stats().idle_conns == 5

	// Bug Fix Needed! msvc generated wrong code for `mut conn := p.get()! as MockConn`
	mut connx := p.get()!
	mut conn := connx as MockConn
	assert p.stats().active_conns == 1
	// it depend on `background_maintenance` thread to keep 5 idle_conns
	assert p.stats().idle_conns >= 4
	conn.close_flag = true
	p.put(conn) or { assert err.msg().contains('simulated close error') }
	assert p.stats().active_conns == 0
	// it depend on `background_maintenance` thread to keep 5 idle_conns
	assert p.stats().idle_conns >= 4

	// Test reset error handling
	conn.reset_flag = true
	p.put(conn) or { assert err.msg().contains('reset') }
	assert p.stats().active_conns == 0
	// it depend on `background_maintenance` thread to keep 5 idle_conns
	assert p.stats().idle_conns >= 4
}
