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
		worker_count := 20
		mut phase := sync.new_barrier(worker_count + 2)
		for _ in 0 .. worker_count {
			wg.add(1)
			spawn fn (mut p pool.ConnectionPool, mut wg sync.WaitGroup, mut phase sync.Barrier) {
				defer { wg.done() }
				phase.wait()
				for i in 0 .. 8 {
					conn := p.get() or { panic(err) }
					if i == 0 {
						// Keep every first lease checked out for the stats and config checks.
						phase.wait()
						phase.wait()
					}
					p.put(conn) or { panic(err) }
				}
			}(mut p, mut wg, mut phase)
		}
		wg.add(1)
		spawn fn [worker_count] (mut p pool.ConnectionPool, mut wg sync.WaitGroup, mut phase sync.Barrier) {
			defer { wg.done() }
			phase.wait()
			phase.wait()
			for i in 0 .. 40 {
				stats := p.stats()
				assert stats.total_conns <= 64
				assert stats.idle_conns <= stats.total_conns
				if i == 0 {
					assert stats.active_conns == worker_count
					phase.wait()
				}
			}
		}(mut p, mut wg, mut phase)
		wg.add(1)
		spawn fn (mut p pool.ConnectionPool, mut wg sync.WaitGroup, mut phase sync.Barrier, config pool.ConnectionPoolConfig) {
			defer { wg.done() }
			phase.wait()
			phase.wait()
			for i in 0 .. 12 {
				p.update_config(config) or { panic(err) }
				if i == 0 {
					phase.wait()
				}
			}
		}(mut p, mut wg, mut phase, config)
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
