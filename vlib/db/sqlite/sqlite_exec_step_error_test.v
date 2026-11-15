// vtest build: present_sqlite3?
import db.sqlite
import orm
import os

const constraint_code = int(sqlite.Result.constraint)
const busy_code = int(sqlite.Result.busy)

fn test_exec_reports_step_errors() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	db.exec('CREATE TABLE t (x INTEGER UNIQUE)')!
	db.exec('INSERT INTO t VALUES (1)')!
	if _ := db.exec('INSERT INTO t VALUES (1)') {
		assert false, 'exec should report the constraint violation'
	} else {
		assert err.code() == constraint_code
		assert err.msg().contains('UNIQUE constraint failed')
	}
	if _ := db.exec_map('INSERT INTO t VALUES (1)') {
		assert false, 'exec_map should report the constraint violation'
	} else {
		assert err.code() == constraint_code
	}
	if _ := db.exec_one('INSERT INTO t VALUES (1)') {
		assert false, 'exec_one should report the constraint violation'
	} else {
		assert err.code() == constraint_code
	}
	// a statement that completes without rows is still a success
	assert db.exec('INSERT INTO t VALUES (2)')!.len == 0
	assert db.q_int('SELECT COUNT(*) FROM t')! == 2
}

fn test_exec_accepts_queries_without_a_statement() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	for query in ['', '   ', '-- only a comment', ';'] {
		assert db.exec(query)!.len == 0
		assert db.exec_map(query)!.len == 0
	}
}

fn test_transaction_helpers_report_busy_and_commit_errors() {
	path := os.join_path(os.vtmp_dir(), 'sqlite_exec_step_error_${os.getpid()}.db')
	defer {
		os.rm(path) or {}
	}
	mut a := sqlite.connect(path)!
	defer {
		a.close() or {}
	}
	a.exec('CREATE TABLE t (x INTEGER)')!
	mut b := sqlite.connect(path)!
	defer {
		b.close() or {}
	}
	b.busy_timeout(0)
	a.exec('BEGIN IMMEDIATE')!
	if _ := b.exec('BEGIN IMMEDIATE') {
		assert false, 'BEGIN IMMEDIATE should fail while another connection holds the write lock'
	} else {
		assert err.code() == busy_code
	}
	if _ := b.exec('INSERT INTO t VALUES (1)') {
		assert false, 'INSERT should fail while another connection holds the write lock'
	} else {
		assert err.code() == busy_code
	}
	if _ := b.commit() {
		assert false, 'COMMIT should fail without an active transaction'
	} else {
		assert err.msg().contains('no transaction is active')
	}
	a.commit()!
	assert a.q_int('SELECT COUNT(*) FROM t')! == 0
}

@[table: 'items']
struct Item {
	id   int @[primary; sql: serial]
	name string
}

fn test_orm_select_reports_busy() {
	path := os.join_path(os.vtmp_dir(), 'sqlite_orm_select_busy_${os.getpid()}.db')
	defer {
		os.rm(path) or {}
	}
	mut a := sqlite.connect(path)!
	defer {
		a.close() or {}
	}
	sql a {
		create table Item
	}!
	item := Item{
		name: 'x'
	}
	sql a {
		insert item into Item
	}!
	mut b := sqlite.connect(path)!
	defer {
		b.close() or {}
	}
	b.busy_timeout(0)
	// load the schema on `b` first, so that only the step (not the prepare) meets the lock
	b.exec('SELECT count(*) FROM items')!
	a.exec('BEGIN EXCLUSIVE')!
	mut select_err_code := 0
	rows := sql b {
		select from Item
	} or {
		select_err_code = err.code()
		[]Item{}
	}
	assert select_err_code == busy_code, 'the ORM select should fail while another connection holds an exclusive lock, got ${rows.len} rows'
	a.rollback()!
	assert (sql b {
		select from Item
	}!).len == 1
}

fn new_db_with_rollback_trigger() !sqlite.DB {
	mut db := sqlite.connect(':memory:')!
	db.exec('CREATE TABLE t (x INTEGER)')!
	db.exec("CREATE TRIGGER no_negatives BEFORE INSERT ON t WHEN new.x < 0 BEGIN SELECT RAISE(ROLLBACK, 'negative x'); END")!
	return db
}

fn test_rollback_after_sqlite_ended_the_transaction() {
	mut db := new_db_with_rollback_trigger()!
	defer {
		db.close() or {}
	}
	db.begin()!
	db.exec('INSERT INTO t VALUES (1)')!
	if _ := db.exec('INSERT INTO t VALUES (-1)') {
		assert false, 'the trigger should reject the insert'
	} else {
		assert err.code() == constraint_code
	}
	// RAISE(ROLLBACK) already ended the transaction, so there is nothing left to roll back
	db.rollback()!
	db.rollback_to('missing')!
	db.release_savepoint('missing')!
	if _ := db.commit() {
		assert false, 'COMMIT should fail without an active transaction'
	} else {
		assert err.msg().contains('no transaction is active')
	}
	assert db.q_int('SELECT COUNT(*) FROM t')! == 0
	// inside an active transaction, a missing savepoint is still an error
	db.begin()!
	if _ := db.rollback_to('missing') {
		assert false, 'ROLLBACK TO an unknown savepoint should fail'
	} else {
		assert err.msg().contains('no such savepoint')
	}
	db.rollback()!
}

@[table: 't']
struct TRow {
	x int
}

fn insert_one_then_a_negative(mut tx orm.Tx) !int {
	good := TRow{
		x: 1
	}
	bad := TRow{
		x: -1
	}
	sql tx {
		insert good into TRow
	}!
	sql tx {
		insert bad into TRow
	}!
	return 0
}

fn test_orm_transactions_keep_the_callback_error_after_an_automatic_rollback() {
	mut db := new_db_with_rollback_trigger()!
	defer {
		db.close() or {}
	}
	mut root_err_code := 0
	orm.transaction[int](mut db, insert_one_then_a_negative) or {
		root_err_code = err.code()
		assert err.msg().contains('negative x'), err.msg()
		assert !err.msg().contains('rollback failed'), err.msg()
	}
	assert root_err_code == constraint_code
	mut nested_err_code := 0
	orm.transaction[int](mut db, fn (mut tx orm.Tx) !int {
		return tx.transaction[int](insert_one_then_a_negative)
	}) or {
		nested_err_code = err.code()
		assert !err.msg().contains('rollback failed'), err.msg()
	}
	assert nested_err_code == constraint_code
	assert db.q_int('SELECT COUNT(*) FROM t')! == 0
}

fn insert_two(mut tx orm.Tx) !int {
	row := TRow{
		x: 2
	}
	sql tx {
		insert row into TRow
	}!
	return 0
}

fn test_orm_transaction_rolls_back_after_a_busy_commit() {
	path := os.join_path(os.vtmp_dir(), 'sqlite_busy_commit_${os.getpid()}.db')
	defer {
		os.rm(path) or {}
	}
	mut a := sqlite.connect(path)!
	defer {
		a.close() or {}
	}
	a.exec('CREATE TABLE t (x INTEGER)')!
	mut b := sqlite.connect(path)!
	defer {
		b.close() or {}
	}
	// a read transaction on `b` keeps a shared lock, so COMMIT on `a` cannot write
	b.exec('BEGIN')!
	b.exec('SELECT * FROM t')!
	mut commit_err_code := 0
	orm.transaction[int](mut a, insert_two) or { commit_err_code = err.code() }
	assert commit_err_code == busy_code
	b.commit()!
	// the busy COMMIT was rolled back, so `a` is usable again and its row is gone
	orm.transaction[int](mut a, insert_two)!
	assert a.q_int('SELECT COUNT(*) FROM t')! == 1
}
