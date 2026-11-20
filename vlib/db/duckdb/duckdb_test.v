// vtest build: present_duckdb?
import db.duckdb

fn test_duckdb_exec_and_params() {
	mut db := duckdb.connect(':memory:') or { panic(err) }
	defer {
		db.close() or { panic(err) }
	}
	assert db.is_open

	db.exec('CREATE TABLE people (id INTEGER, name TEXT, age BIGINT, nick TEXT)') or {
		panic(err)
	}
	db.exec_param('INSERT INTO people (id, name, age, nick) VALUES (?1, ?2, ?3, ?4)', [
		'1',
		'Alice',
		'30',
		'al',
	]) or {
		panic(err)
	}
	db.exec_param('INSERT INTO people (id, name, age, nick) VALUES (?1, ?2, ?3, ?4)', [
		'2',
		'Bob',
		'25',
		'bob',
	]) or {
		panic(err)
	}

	rows := db.exec_param('SELECT name, age FROM people WHERE age > ?1 ORDER BY age LIMIT 2', [
		'24',
	]) or {
		panic(err)
	}
	assert rows.len == 2
	assert rows[0].vals == ['Bob', '25']
	assert rows[1].vals == ['Alice', '30']
	assert rows[0].names == ['name', 'age']
	assert rows[0].val(0) == 'Bob'
	assert rows[0].values() == ['Bob', '25']
	assert rows[0].get_string('age') == '25'
	assert rows[0].get_int('age') == 25
}

fn test_duckdb_null_tracking() {
	mut db := duckdb.open(':memory:') or { panic(err) }
	defer {
		db.close() or { panic(err) }
	}
	db.exec('CREATE TABLE t (a INTEGER, b VARCHAR)') or { panic(err) }
	db.exec("INSERT INTO t VALUES (NULL, 'x')") or { panic(err) }
	db.exec('INSERT INTO t VALUES (3, NULL)') or { panic(err) }

	rows := db.exec('SELECT a, b FROM t ORDER BY a NULLS FIRST') or { panic(err) }
	assert rows.len == 2
	assert rows[0].is_null(0)
	assert !rows[0].is_null(1)
	assert rows[0].vals[1] == 'x'
	assert !rows[1].is_null(0)
	assert rows[1].is_null(1)
	assert rows[1].vals[0] == '3'
}

fn test_duckdb_exec_helpers() {
	mut db := duckdb.connect(':memory:') or { panic(err) }
	defer {
		db.close() or { panic(err) }
	}
	db.exec('CREATE TABLE n (v INTEGER)') or { panic(err) }
	n := db.rows_changed('INSERT INTO n VALUES (1), (2), (3)') or { panic(err) }
	assert n == 3
	assert (db.q_int('SELECT COUNT(*) FROM n') or { 0 }) == 3
	assert (db.q_string('SELECT CAST(42 AS VARCHAR)') or { '' }) == '42'

	m := db.exec_map('SELECT v FROM n ORDER BY v LIMIT 1') or { panic(err) }
	assert m.len == 1
	assert m[0]['v'] == '1'

	one := db.exec_one('SELECT v FROM n ORDER BY v LIMIT 1') or { panic(err) }
	assert one.vals == ['1']
}

fn test_duckdb_error_propagation() {
	mut db := duckdb.connect(':memory:') or { panic(err) }
	defer {
		db.close() or { panic(err) }
	}
	if _ := db.exec('SELECT * FROM no_such_table') {
		panic('expected a query error')
	} else {
		assert err.msg().contains('no_such_table')
	}
	if _ := db.exec_param('SELECT ?1, ?2', ['only one']) {
		panic('expected a missing-parameter error')
	} else {
		assert err.msg().len > 0
	}
}

fn test_duckdb_bind_u64_max() {
	mut db := duckdb.connect(':memory:') or { panic(err) }
	defer {
		db.close() or { panic(err) }
	}
	db.exec('CREATE TABLE u (v UBIGINT)') or { panic(err) }
	stmt := db.prepare('INSERT INTO u VALUES (?1)') or { panic(err) }
	defer {
		stmt.finalize()
	}
	max := u64(18446744073709551615)
	assert stmt.bind_u64(1, max) == duckdb.success
	stmt.exec('INSERT INTO u VALUES (?1)') or { panic(err) }
	rows := db.exec('SELECT v FROM u') or { panic(err) }
	assert rows.len == 1
	assert rows[0].vals[0] == '18446744073709551615'
}

fn test_duckdb_bind_u64_does_not_wrap_into_bigint() {
	mut db := duckdb.connect(':memory:') or { panic(err) }
	defer {
		db.close() or { panic(err) }
	}
	db.exec('CREATE TABLE s (v BIGINT)') or { panic(err) }
	stmt := db.prepare('INSERT INTO s VALUES (?1)') or { panic(err) }
	defer {
		stmt.finalize()
	}
	assert stmt.bind_u64(1, u64(18446744073709551615)) == duckdb.success
	// A signed BIGINT column cannot hold u64 max: DuckDB must reject it
	// instead of silently storing the wrapped -1.
	if _ := stmt.exec('INSERT INTO s VALUES (?1)') {
		panic('expected out-of-range error for u64 max in BIGINT column')
	} else {
		assert err.msg().len > 0
	}
	rows := db.exec('SELECT v FROM s') or { panic(err) }
	assert rows.len == 0
}

fn test_duckdb_failed_prepare_cleanup() {
	mut db := duckdb.connect(':memory:') or { panic(err) }
	defer {
		db.close() or { panic(err) }
	}
	// A failed prepare must release its handle; repeated failures must not
	// exhaust resources or leave the connection in a broken state.
	for i in 0 .. 200 {
		if _ := db.prepare('SELECT * FROM missing_${i}') {
			panic('expected prepare failure')
		} else {
			assert err.msg().contains('missing_${i}')
		}
	}
	stmt := db.prepare('SELECT 1') or { panic(err) }
	stmt.finalize()
}

fn test_duckdb_transactions() {
	mut db := duckdb.connect(':memory:') or { panic(err) }
	defer {
		db.close() or { panic(err) }
	}
	db.exec('CREATE TABLE t (v INTEGER)') or { panic(err) }
	db.exec('INSERT INTO t VALUES (1)') or { panic(err) }

	db.begin() or { panic(err) }
	db.exec('INSERT INTO t VALUES (2)') or { panic(err) }
	db.rollback() or { panic(err) }
	assert (db.q_int('SELECT COUNT(*) FROM t') or { 0 }) == 1

	db.begin() or { panic(err) }
	db.exec('INSERT INTO t VALUES (3)') or { panic(err) }
	db.commit() or { panic(err) }
	assert (db.q_int('SELECT COUNT(*) FROM t') or { 0 }) == 2

	// savepoints are not supported by DuckDB 2.0
	if _ := db.orm_savepoint('sp1') {
		panic('expected savepoint error')
	} else {
		assert err.msg().contains('SAVEPOINT is not supported')
	}
}
