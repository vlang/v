import pool
import sync
import time

struct ConcurrentPoolConn {}

fn (mut c ConcurrentPoolConn) validate() !bool {
	return true
}

fn (mut c ConcurrentPoolConn) close() ! {}

fn (mut c ConcurrentPoolConn) reset() ! {}

fn concurrent_pool_factory() !&pool.ConnectionPoolable {
	return &ConcurrentPoolConn{}
}

fn test_repeated_concurrent_pool_use_and_shutdown() {
	done := chan bool{cap: 1}
	watchdog := spawn fn (done chan bool) {
		select {
			_ := <-done {
			}
			30 * time.second {
				panic('concurrent pool use and shutdown did not complete in time')
			}
		}
	}(done)
	defer {
		done <- true
		watchdog.wait()
	}

	// Repeat in one process to exercise maintenance thread shutdown and later pools.
	for _ in 0 .. 12 {
		config := pool.ConnectionPoolConfig{
			max_conns:      64
			min_idle_conns: 4
			idle_timeout:   100 * time.millisecond
			get_timeout:    2 * time.second
		}
		mut p := pool.new_connection_pool(concurrent_pool_factory, config)!
		mut wg := sync.new_waitgroup()
		start := chan bool{cap: 22}
		for _ in 0 .. 20 {
			wg.add(1)
			spawn fn (mut p pool.ConnectionPool, mut wg sync.WaitGroup, start chan bool) {
				defer { wg.done() }
				_ = <-start
				for _ in 0 .. 8 {
					conn := p.get() or { panic(err) }
					time.sleep(time.millisecond)
					p.put(conn) or { panic(err) }
				}
			}(mut p, mut wg, start)
		}
		wg.add(1)
		spawn fn (mut p pool.ConnectionPool, mut wg sync.WaitGroup, start chan bool) {
			defer { wg.done() }
			_ = <-start
			for _ in 0 .. 40 {
				stats := p.stats()
				assert stats.total_conns <= 64
				assert stats.idle_conns <= stats.total_conns
				time.sleep(time.millisecond)
			}
		}(mut p, mut wg, start)
		wg.add(1)
		spawn fn (mut p pool.ConnectionPool, mut wg sync.WaitGroup, start chan bool, config pool.ConnectionPoolConfig) {
			defer { wg.done() }
			_ = <-start
			for _ in 0 .. 12 {
				p.update_config(config) or { panic(err) }
				time.sleep(time.millisecond)
			}
		}(mut p, mut wg, start, config)
		for _ in 0 .. 22 {
			start <- true
		}
		wg.wait()
		assert p.stats().active_conns == 0
		assert p.stats().waiting_clients == 0
		p.close()
		stats := p.stats()
		assert stats.total_conns == 0
		assert stats.idle_conns == 0
		if _ := p.get() {
			assert false, 'closed pool returned a connection'
		} else {
			assert err.msg().contains('closed')
		}
	}
}
