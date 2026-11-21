// vtest build: present_sqlite3?
import db.sqlite

fn test_q_int_reads_the_width_of_int() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	assert db.q_int('SELECT 42')! == 42
	assert db.q_int('SELECT -7')! == -7
	assert db.q_int('SELECT 2147483647')! == 2147483647
	db.exec('CREATE TABLE t (x INTEGER)')!
	db.exec('INSERT INTO t VALUES (5000000000), (-5000000000)')!
	$if new_int ?&& x64 {
		assert db.q_int('SELECT MAX(x) FROM t')! == 5_000_000_000
		assert db.q_int('SELECT MIN(x) FROM t')! == -5_000_000_000
		assert db.q_int('SELECT 9223372036854775807')! == max_int
	} $else {
		// the low 32 bits, as sqlite3_column_int returns them
		assert db.q_int('SELECT MAX(x) FROM t')! == 705_032_704
	}
	db.q_int('SELECT 1 WHERE 0') or {
		assert err.code() == sqlite.sqlite_done
		return
	}
	assert false, 'a query without rows should fail'
}
