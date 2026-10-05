module db

import sync
import time

@[heap]
struct TestPoolState {
mut:
	mu            &sync.Mutex = sync.new_mutex()
	opened        int
	closed        int
	resets        int
	fail_connect  bool
	fail_reset    bool
	valid         bool = true
	invalid_id    int
	gate_reset    bool
	reset_started chan bool
	reset_resume  chan bool
}

struct TestPoolFactory {
	state &TestPoolState
}

fn (factory TestPoolFactory) connect() !&Driver {
	mut state := factory.state
	state.mu.lock()
	defer { state.mu.unlock() }
	if state.fail_connect { return error('test: connect failed') }
	state.opened++
	mut driver := &TestPoolDriver{ state: state, id: state.opened }
	return driver
}

@[heap]
struct TestPoolDriver {
	id int
mut:
	state &TestPoolState
}

fn (mut driver TestPoolDriver) exec(query string) ![]DriverRow {
	if query == 'fail' { return error('test: query failed') }
	return [DriverRow{ vals: [driver.id.str()], names: ['id'] }]
}

fn (mut driver TestPoolDriver) exec_one(query string) !DriverRow {
	return driver.exec(query)![0]
}

fn (mut driver TestPoolDriver) exec_param_many(query string, _ []string) ![]DriverRow {
	return driver.exec(query)
}

fn (mut driver TestPoolDriver) validate() !bool {
	driver.state.mu.lock()
	defer { driver.state.mu.unlock() }
	return driver.state.valid && driver.id != driver.state.invalid_id
}

fn (mut driver TestPoolDriver) reset() ! {
	driver.state.mu.lock()
	defer { driver.state.mu.unlock() }
	driver.state.resets++
	if driver.state.gate_reset {
		driver.state.reset_started <- true
		_ := <-driver.state.reset_resume
	}
	if driver.state.fail_reset { return error('test: reset failed') }
}

fn (mut driver TestPoolDriver) close() ! {
	driver.state.mu.lock()
	driver.state.closed++
	driver.state.mu.unlock()
}

fn test_pool_reuses_driver_but_never_revives_released_handle() {
	state := &TestPoolState{}
	mut pool := new_pool(TestPoolFactory{state}, max_open_conns: 1)
	mut first := pool.acquire()!
	assert first.exec_one('id')!.val(0) == '1'
	pool.release(first)
	mut second := pool.acquire()!
	assert second.exec_one('id')!.val(0) == '1'
	if _ := first.exec('id') {
		assert false
	} else {
		assert err.msg() == 'db: connection is released'
	}
	pool.release(first)
	assert pool.stats().in_use == 1
	assert state.opened == 1
	second.close()!
	assert state.resets == 2
	assert pool.stats().idle == 1
	pool.close()
	pool.close()
	assert state.closed == 1
	assert pool.stats().open_connections == 0
}

fn test_pool_discard_reset_failure_and_invalid_idle_connection() {
	mut state := &TestPoolState{}
	mut pool := new_pool(TestPoolFactory{state}, max_open_conns: 1)
	mut first := pool.acquire()!
	state.fail_reset = true
	first.close()!
	assert state.closed == 1
	assert pool.stats().open_connections == 0
	state.fail_reset = false
	mut second := pool.acquire()!
	second.close()!
	state.valid = false
	mut third := pool.acquire()!
	assert third.exec_one('id')!.val(0) == '3'
	assert state.closed == 2
	third.close()!
	pool.close()
}

fn test_pool_no_idle_limit_shrink_and_lifetime() {
	state := &TestPoolState{}
	mut pool := new_pool(TestPoolFactory{state}, max_open_conns: 2)
	mut first := pool.acquire()!
	mut second := pool.acquire()!
	first.close()!
	second.close()!
	pool.set_max_open(1)
	assert pool.stats().open_connections == 1
	assert state.closed == 1
	pool.set_max_lifetime(time.nanosecond)
	mut third := pool.acquire()!
	assert third.exec_one('id')!.val(0) == '3'
	pool.set_max_lifetime(0)
	pool.set_max_idle(0)
	third.close()!
	assert state.closed == 3
	assert pool.stats().open_connections == 0
	pool.close()
}

fn test_pool_connect_failure_and_release_after_close() {
	mut state := &TestPoolState{ fail_connect: true }
	mut pool := new_pool(TestPoolFactory{state}, max_open_conns: 1)
	if _ := pool.acquire() {
		assert false
	} else {
		assert err.msg() == 'test: connect failed'
	}
	assert pool.stats().open_connections == 0
	state.fail_connect = false
	mut conn := pool.acquire()!
	pool.close()
	assert pool.stats().in_use == 1
	conn.close()!
	assert state.closed == 1
	assert pool.stats().open_connections == 0
	if _ := pool.acquire() {
		assert false
	} else {
		assert err.msg() == 'db: pool is closed'
	}
}

fn test_db_lazy_connection_and_release_after_query_error() {
	state := &TestPoolState{}
	mut database := new_db(TestPoolFactory{state}, max_open_conns: 1)
	assert state.opened == 0
	if _ := database.exec('fail') {
		assert false
	} else {
		assert err.msg() == 'test: query failed'
	}
	assert database.stats().in_use == 0
	assert database.exec_one('id')!.val(0) == '1'
	database.ping()!
	assert state.opened == 1
	database.close()!
	assert state.closed == 1
}

fn test_pooled_sqlite_builtin_driver() {
	mut database := open_pooled(DriverConfig{ kind: .sqlite, path: ':memory:' },
		max_open_conns: 1
		max_idle_conns: 1
	)
	defer { database.close() or {} }
	assert database.stats().open_connections == 0
	database.exec('create table users (name text)')!
	database.exec_param_many('insert into users (name) values (?)', ['alice'])!
	assert database.exec_one('select name from users')!.val(0) == 'alice'
	database.ping()!
	assert database.stats().open_connections == 1
}

fn pool_waiter(mut pool Pool, result chan string) {
	mut conn := pool.acquire() or {
		result <- err.msg()
		return
	}
	row := conn.exec_one('id') or { panic(err) }
	result <- row.val(0)
	conn.close() or { panic(err) }
}

fn wait_for_pool_waiter(mut pool Pool) {
	deadline := time.now().add(5 * time.second)
	for pool.stats().wait_count == 0 {
		assert time.now() < deadline, 'waiter did not enter pool'
		time.sleep(time.millisecond)
	}
}

fn test_pool_waiter_handoff_capacity_change_and_shutdown() {
	state := &TestPoolState{}
	mut pool := new_pool(TestPoolFactory{state}, max_open_conns: 1)
	mut first := pool.acquire()!
	result := chan string{cap: 1}
	worker := spawn pool_waiter(mut pool, result)
	wait_for_pool_waiter(mut pool)
	first.close()!
	assert <-result == '1'
	worker.wait()
	mut pinned := pool.acquire()!
	worker2 := spawn pool_waiter(mut pool, result)
	wait_for_pool_waiter(mut pool)
	pool.set_max_open(2)
	assert <-result == '2'
	worker2.wait()
	pool.set_max_open(1)
	worker3 := spawn pool_waiter(mut pool, result)
	wait_for_pool_waiter(mut pool)
	pool.close()
	assert <-result == 'db: pool is closed'
	worker3.wait()
	pinned.close()!
	assert pool.stats().open_connections == 0
	assert state.closed == 2
}

fn test_pool_waiter_discards_invalid_released_connection() {
	state := &TestPoolState{ invalid_id: 1 }
	mut pool := new_pool(TestPoolFactory{state}, max_open_conns: 1)
	defer { pool.close() }
	mut first := pool.acquire()!
	result := chan string{cap: 1}
	worker := spawn pool_waiter(mut pool, result)
	wait_for_pool_waiter(mut pool)
	first.close()!
	id := <-result
	worker.wait()
	assert id == '2'
	assert state.opened == 2
	assert state.closed == 1
	assert state.resets == 2
	stats := pool.stats()
	assert stats.open_connections == 1
	assert stats.in_use == 0
	assert stats.idle == 1
	assert stats.wait_count == 0
	pool.close()
	assert state.closed == 2
	assert pool.stats().open_connections == 0
	if _ := pool.acquire() {
		assert false
	} else {
		assert err.msg() == 'db: pool is closed'
	}
}

struct NilPoolFactory {}

fn (factory NilPoolFactory) connect() !&Driver {
	return unsafe { nil }
}

struct GatedPoolFactory {
	state   &TestPoolState
	started chan bool
	resume  chan bool
}

fn (factory GatedPoolFactory) connect() !&Driver {
	factory.started <- true
	_ := <-factory.resume
	return TestPoolFactory{factory.state}.connect()
}

fn test_pool_rejects_nil_driver_without_consuming_capacity() {
	mut pool := new_pool(NilPoolFactory{}, max_open_conns: 1)
	if _ := pool.acquire() {
		assert false
	} else {
		assert err.msg() == 'db: driver factory returned a nil connection'
	}
	assert pool.stats().open_connections == 0
	pool.close()
}

fn test_pool_closes_connection_opened_during_shutdown() {
	state := &TestPoolState{}
	started := chan bool{}
	resume := chan bool{}
	result := chan string{cap: 1}
	mut pool := new_pool(GatedPoolFactory{state, started, resume}, max_open_conns: 1)
	worker := spawn pool_waiter(mut pool, result)
	assert <-started
	pool.close()
	resume <- true
	assert <-result == 'db: pool is closed'
	worker.wait()
	assert state.opened == 1
	assert state.closed == 1
	assert pool.stats().open_connections == 0
}

fn pool_release_worker(mut conn Conn) {
	conn.close() or { panic(err) }
}

fn test_pool_closes_connection_reset_during_shutdown() {
	state := &TestPoolState{ gate_reset: true }
	mut pool := new_pool(TestPoolFactory{state}, max_open_conns: 1)
	mut conn := pool.acquire()!
	worker := spawn pool_release_worker(mut conn)
	assert <-state.reset_started
	pool.close()
	state.reset_resume <- true
	worker.wait()
	assert state.closed == 1
	assert pool.stats().open_connections == 0
}
