module db

import os
import sync
import time

@[heap]
struct TxTestState {
mut:
	mu             &sync.Mutex = sync.new_mutex()
	opened         int
	closed         []int
	resets         int
	unclean_resets int
	active         map[int]bool
	queries        []string
	query_ids      []int
	fail_query     string
	fail_reset     bool
	gate_query     string
	started        chan bool = chan bool{}
	resume         chan bool = chan bool{}
}

struct TxTestFactory {
	state  &TxTestState
	native bool
}

fn (factory TxTestFactory) connect() !&Driver {
	mut state := factory.state
	state.mu.lock()
	state.opened++
	id := state.opened
	state.mu.unlock()
	mut driver := &TxTestDriver{ state: state, id: id }
	if factory.native {
		return &TxNativeDriver{ inner: driver }
	}
	return driver
}

@[heap]
struct TxTestDriver {
	id int
mut:
	state &TxTestState
}

fn (mut driver TxTestDriver) exec(query string) ![]DriverRow {
	mut state := driver.state
	state.mu.lock()
	defer { state.mu.unlock() }
	state.queries << query
	state.query_ids << driver.id
	if query == state.gate_query {
		state.started <- true
		_ := <-state.resume
	}
	command := query.trim_string_left('NATIVE ')
	if command == state.fail_query {
		return error('test: ${command} failed')
	}
	if command == 'BEGIN' {
		assert !state.active[driver.id], 'connection already has a transaction'
		state.active[driver.id] = true
	} else if command in ['COMMIT', 'ROLLBACK'] {
		assert state.active[driver.id], 'connection has no transaction'
		state.active[driver.id] = false
	}
	return [DriverRow{ vals: [driver.id.str()], names: ['id'] }]
}

fn (mut driver TxTestDriver) exec_one(query string) !DriverRow {
	return driver.exec(query)![0]
}

fn (mut driver TxTestDriver) exec_param_many(query string, params []string) ![]DriverRow {
	driver.exec(query)!
	return [DriverRow{ vals: params.clone() }]
}

fn (mut driver TxTestDriver) validate() !bool {
	return true
}

fn (mut driver TxTestDriver) reset() ! {
	mut state := driver.state
	state.mu.lock()
	defer { state.mu.unlock() }
	state.resets++
	if state.active[driver.id] {
		state.unclean_resets++
	}
	if state.fail_reset { return error('test: reset failed') }
}

fn (mut driver TxTestDriver) close() ! {
	mut state := driver.state
	state.mu.lock()
	defer { state.mu.unlock() }
	assert driver.id !in state.closed, 'physical connection closed twice'
	state.closed << driver.id
	state.active[driver.id] = false
}

@[heap]
struct TxNativeDriver {
mut:
	inner &TxTestDriver
}

fn (mut driver TxNativeDriver) exec(query string) ![]DriverRow {
	return driver.inner.exec(query)
}

fn (mut driver TxNativeDriver) exec_one(query string) !DriverRow {
	return driver.inner.exec_one(query)
}

fn (mut driver TxNativeDriver) exec_param_many(query string, params []string) ![]DriverRow {
	return driver.inner.exec_param_many(query, params)
}

fn (mut driver TxNativeDriver) validate() !bool {
	return driver.inner.validate()
}

fn (mut driver TxNativeDriver) reset() ! {
	driver.inner.reset()!
}

fn (mut driver TxNativeDriver) close() ! {
	driver.inner.close()!
}

fn (mut driver TxNativeDriver) transaction(command TransactionCommand, name string) ! {
	query := match command {
		.begin { 'BEGIN' }
		.commit { 'COMMIT' }
		.rollback { 'ROLLBACK' }
		.savepoint { 'SAVEPOINT ${name}' }
		.rollback_to { 'ROLLBACK TO SAVEPOINT ${name}' }
		.release_savepoint { 'RELEASE SAVEPOINT ${name}' }
	}
	driver.inner.exec('NATIVE ${query}')!
}

fn tx_database_worker(mut database DB, result chan string) {
	row := database.exec_one('id') or {
		result <- err.msg()
		return
	}
	result <- row.val(0)
}

fn tx_wait_for_waiter(mut database DB) {
	deadline := time.now().add(5 * time.second)
	for database.stats().wait_count == 0 {
		assert time.now() < deadline, 'database worker did not wait for capacity'
		time.sleep(time.millisecond)
	}
}

fn test_tx_pins_connection_and_releases_waiter_only_after_commit() {
	mut state := &TxTestState{}
	mut database := new_db(TxTestFactory{ state: state }, max_open_conns: 1)
	defer { database.close() or {} }
	mut tx := database.begin()!
	mut stale := tx.state.conn
	assert database.stats().in_use == 1
	assert tx.exec('id')![0].val(0) == '1'
	assert tx.exec_one('id')!.val(0) == '1'
	assert tx.exec_param_many('params', ['alice', 'bob'])![0].values() == ['alice', 'bob']
	state.fail_query = 'fail'
	if _ := tx.exec('fail') {
		assert false
	} else {
		assert err.msg() == 'test: fail failed'
	}
	state.fail_query = ''
	result := chan string{cap: 1}
	worker := spawn tx_database_worker(mut database, result)
	tx_wait_for_waiter(mut database)
	tx.commit()!
	assert <-result == '1'
	worker.wait()
	assert database.stats().in_use == 0
	assert state.opened == 1
	assert state.resets == 2
	assert state.unclean_resets == 0
	if _ := stale.exec('id') {
		assert false, 'transaction connection survived release'
	} else {
		assert err.msg() == 'db: connection is released'
	}
}

fn assert_finished_transaction(mut tx Tx) {
	if _ := tx.exec('id') {
		assert false
	} else {
		assert err.msg() == 'db: transaction is already finished'
	}
	if _ := tx.exec_one('id') {
		assert false
	} else {
		assert err.msg() == 'db: transaction is already finished'
	}
	if _ := tx.exec_param_many('params', []) {
		assert false
	} else {
		assert err.msg() == 'db: transaction is already finished'
	}
	if _ := tx.commit() {
		assert false
	} else {
		assert err.msg() == 'db: transaction is already finished'
	}
	if _ := tx.rollback() {
		assert false
	} else {
		assert err.msg() == 'db: transaction is already finished'
	}
	if _ := tx.savepoint('point') {
		assert false
	} else {
		assert err.msg() == 'db: transaction is already finished'
	}
	if _ := tx.rollback_to('point') {
		assert false
	} else {
		assert err.msg() == 'db: transaction is already finished'
	}
	if _ := tx.release_savepoint('point') {
		assert false
	} else {
		assert err.msg() == 'db: transaction is already finished'
	}
}

fn test_tx_finished_state_is_shared_by_copies_and_releases_once() {
	state := &TxTestState{}
	mut database := new_db(TxTestFactory{ state: state }, max_open_conns: 1)
	defer { database.close() or {} }
	mut tx := database.begin()!
	mut copied := *tx
	tx.rollback()!
	assert_finished_transaction(mut tx)
	assert_finished_transaction(mut copied)
	assert state.resets == 1
	assert state.queries == ['BEGIN', 'ROLLBACK']
	assert state.unclean_resets == 0
	mut empty := Tx{}
	assert_finished_transaction(mut empty)
}

fn test_tx_begin_and_terminal_failures_discard_uncertain_connections() {
	for command in ['BEGIN', 'COMMIT', 'ROLLBACK'] {
		mut state := &TxTestState{}
		mut database := new_db(TxTestFactory{ state: state }, max_open_conns: 1)
		if command == 'BEGIN' {
			state.fail_query = command
			if _ := database.begin() {
				assert false
			} else {
				assert err.msg() == 'test: BEGIN failed'
			}
		} else {
			mut tx := database.begin()!
			state.fail_query = command
			if command == 'COMMIT' {
				if _ := tx.commit() {
					assert false
				} else {
					assert err.msg() == 'test: COMMIT failed'
				}
			} else {
				if _ := tx.rollback() {
					assert false
				} else {
					assert err.msg() == 'test: ROLLBACK failed'
				}
			}
			assert_finished_transaction(mut tx)
		}
		assert state.closed == [1]
		assert state.resets == 0
		assert database.stats().open_connections == 0
		state.fail_query = ''
		mut replacement := database.begin()!
		assert replacement.exec_one('id')!.val(0) == '2'
		replacement.commit()!
		assert state.unclean_resets == 0
		database.close()!
		assert state.closed == [1, 2]
	}
}

fn test_tx_native_transaction_hook_and_savepoint_validation() {
	state := &TxTestState{}
	mut database := new_db(TxTestFactory{ state: state, native: true }, max_open_conns: 1)
	defer { database.close() or {} }
	mut tx := database.begin()!
	tx.savepoint('point')!
	tx.rollback_to('point')!
	tx.release_savepoint('point')!
	for name in ['', 'bad name', 'point; COMMIT', '1point', '"point"'] {
		if _ := tx.savepoint(name) {
			assert false
		} else {
			assert err.msg() == 'db: savepoint name must be an identifier'
		}
		if _ := tx.rollback_to(name) {
			assert false
		} else {
			assert err.msg() == 'db: savepoint name must be an identifier'
		}
		if _ := tx.release_savepoint(name) {
			assert false
		} else {
			assert err.msg() == 'db: savepoint name must be an identifier'
		}
	}
	tx.rollback()!
	assert state.queries == ['NATIVE BEGIN', 'NATIVE SAVEPOINT point',
		'NATIVE ROLLBACK TO SAVEPOINT point', 'NATIVE RELEASE SAVEPOINT point', 'NATIVE ROLLBACK']
	assert state.query_ids == [1, 1, 1, 1, 1]
	assert state.resets == 1
}

fn tx_terminal_worker(mut tx Tx, commit bool, entered chan bool, result chan string) {
	entered <- true
	if commit {
		tx.commit() or {
			result <- err.msg()
			return
		}
	} else {
		tx.rollback() or {
			result <- err.msg()
			return
		}
	}
	result <- 'ok'
}

fn test_tx_concurrent_terminal_operations_finish_exactly_once() {
	state := &TxTestState{ gate_query: 'COMMIT' }
	mut database := new_db(TxTestFactory{ state: state }, max_open_conns: 1)
	defer { database.close() or {} }
	mut tx := database.begin()!
	entered := chan bool{cap: 1}
	committed := chan string{cap: 1}
	rolled_back := chan string{cap: 1}
	first := spawn tx_terminal_worker(mut tx, true, entered, committed)
	assert <-entered
	assert <-state.started
	second := spawn tx_terminal_worker(mut tx, false, entered, rolled_back)
	assert <-entered
	assert database.stats().in_use == 1
	state.resume <- true
	assert <-committed == 'ok'
	assert <-rolled_back == 'db: transaction is already finished'
	first.wait()
	second.wait()
	assert state.queries == ['BEGIN', 'COMMIT']
	assert state.resets == 1
	assert state.unclean_resets == 0
}

fn tx_query_worker(mut tx Tx, result chan string) {
	row := tx.exec_one('blocked') or {
		result <- err.msg()
		return
	}
	result <- row.val(0)
}

fn test_tx_finish_waits_for_an_in_flight_query() {
	state := &TxTestState{ gate_query: 'blocked' }
	mut database := new_db(TxTestFactory{ state: state }, max_open_conns: 1)
	defer { database.close() or {} }
	mut tx := database.begin()!
	queried := chan string{cap: 1}
	committed := chan string{cap: 1}
	entered := chan bool{cap: 1}
	query := spawn tx_query_worker(mut tx, queried)
	assert <-state.started
	finish := spawn tx_terminal_worker(mut tx, true, entered, committed)
	assert <-entered
	assert database.stats().in_use == 1
	state.resume <- true
	assert <-queried == '1'
	assert <-committed == 'ok'
	query.wait()
	finish.wait()
	assert state.queries == ['BEGIN', 'blocked', 'COMMIT']
	assert state.resets == 1
	assert state.unclean_resets == 0
}

fn tx_begin_worker(mut database DB, result chan string) {
	mut tx := database.begin() or {
		result <- err.msg()
		return
	}
	tx.rollback() or { panic(err) }
	result <- 'ok'
}

fn test_tx_failed_begin_frees_capacity_for_a_waiting_database_call() {
	state := &TxTestState{ gate_query: 'BEGIN', fail_query: 'BEGIN' }
	mut database := new_db(TxTestFactory{ state: state }, max_open_conns: 1)
	defer { database.close() or {} }
	started := chan string{cap: 1}
	acquired := chan string{cap: 1}
	begin := spawn tx_begin_worker(mut database, started)
	assert <-state.started
	query := spawn tx_database_worker(mut database, acquired)
	tx_wait_for_waiter(mut database)
	state.resume <- true
	assert <-started == 'test: BEGIN failed'
	assert <-acquired == '2'
	begin.wait()
	query.wait()
	assert state.closed == [1]
	assert state.resets == 1
	assert state.unclean_resets == 0
	assert database.stats().in_use == 0
}

fn test_tx_terminal_success_still_discards_reset_failure() {
	state := &TxTestState{ fail_reset: true }
	mut database := new_db(TxTestFactory{ state: state }, max_open_conns: 1)
	mut tx := database.begin()!
	tx.commit()!
	assert_finished_transaction(mut tx)
	assert state.closed == [1]
	assert state.resets == 1
	assert database.stats().open_connections == 0
	database.close()!
}

fn test_tx_sqlite_commit_rollback_and_savepoints() {
	mut database := open_pooled(DriverConfig{ kind: .sqlite, path: ':memory:' },
		max_open_conns: 1
		max_idle_conns: 1
	)
	defer { database.close() or {} }
	database.exec('CREATE TABLE users (name TEXT)')!
	mut tx := database.begin()!
	defer { tx.rollback() or {} }
	tx.exec_param_many('INSERT INTO users (name) VALUES (?)', ['alice'])!
	tx.savepoint('select')!
	tx.exec("INSERT INTO users (name) VALUES ('bob')")!
	assert tx.exec_one('SELECT COUNT(*) FROM users')!.val(0) == '2'
	tx.rollback_to('select')!
	tx.release_savepoint('select')!
	tx.commit()!
	assert database.exec_one('SELECT name FROM users')!.val(0) == 'alice'
	mut rolled_back := database.begin()!
	rolled_back.exec("INSERT INTO users (name) VALUES ('bob')")!
	rolled_back.rollback()!
	assert database.exec_one('SELECT COUNT(*) FROM users')!.val(0) == '1'
	assert database.stats().in_use == 0
}

fn test_tx_sqlite_failed_commit_rolls_back_on_physical_close() {
	path := os.join_path(os.vtmp_dir(), 'shared_tx_${os.getpid()}_${time.now().unix_nano()}.db')
	mut database := open_pooled(DriverConfig{ kind: .sqlite, path: path }, max_open_conns: 1)
	defer {
		database.close() or {}
		os.rm(path) or {}
	}
	database.exec('PRAGMA foreign_keys = ON')!
	database.exec('CREATE TABLE parents (id INTEGER PRIMARY KEY)')!
	database.exec('CREATE TABLE children (parent_id INTEGER REFERENCES parents(id) DEFERRABLE INITIALLY DEFERRED)')!
	mut tx := database.begin()!
	tx.exec('INSERT INTO children (parent_id) VALUES (42)')!
	if _ := tx.commit() {
		assert false, 'deferred constraint must fail at commit'
	} else {
		assert err.msg().contains('FOREIGN KEY constraint failed'), err.msg()
	}
	assert_finished_transaction(mut tx)
	assert database.stats().open_connections == 0
	assert database.exec_one('SELECT COUNT(*) FROM children')!.val(0) == '0'
	mut next := database.begin()!
	next.rollback()!
}

fn test_tx_remains_usable_after_database_close_and_releases_on_finish() {
	state := &TxTestState{}
	mut database := new_db(TxTestFactory{ state: state }, max_open_conns: 1)
	mut tx := database.begin()!
	database.close()!
	assert tx.exec_one('id')!.val(0) == '1'
	tx.rollback()!
	assert state.closed == [1]
	assert database.stats().open_connections == 0
	if _ := database.begin() {
		assert false
	} else {
		assert err.msg() == 'db: pool is closed'
	}
}
