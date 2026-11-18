// vtest build: present_sqlite3?
import db.sqlite
import os

fn test_begin_with_a_transaction_level() {
	path := os.join_path(os.vtmp_dir(), 'sqlite_transaction_level_${os.getpid()}.db')
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
	for level in [sqlite.Sqlite3TransactionLevel.immediate, .exclusive] {
		a.begin(transaction_level: level)!
		// the write lock is taken by BEGIN itself
		b.begin(transaction_level: .immediate) or {
			assert err.code() == int(sqlite.Result.busy)
			a.commit()!
			continue
		}
		assert false, '${level}: the second BEGIN IMMEDIATE should fail'
	}
	// a deferred transaction takes no lock until it reads or writes
	a.begin(transaction_level: .immediate)!
	b.begin()!
	b.commit()!
	a.commit()!
}
