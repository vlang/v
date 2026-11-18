// vtest build: present_sqlite3?
import db.sqlite
import os

fn test_journal_mode_of_a_database_file() {
	path := os.join_path(os.vtmp_dir(), 'sqlite_journal_mode_${os.getpid()}.db')
	defer {
		for suffix in ['', '-wal', '-shm'] {
			os.rm(path + suffix) or {}
		}
	}
	mut a := sqlite.connect(path)!
	defer {
		a.close() or {}
	}
	assert a.current_journal_mode()! == .delete
	a.journal_mode(.wal)!
	assert a.current_journal_mode()! == .wal
	// the mode is stored in the database file
	assert journal_mode_of(path)! == .wal
	for mode in [sqlite.JournalMode.delete, .truncate, .persist, .memory, .wal] {
		a.journal_mode(mode)!
		assert a.current_journal_mode()! == mode
	}
	// SQLite refuses `.off` in defensive mode (SQLITE_DBCONFIG_DEFENSIVE), which the macOS
	// system library enables, and then keeps the current mode
	a.journal_mode(.off)!
	assert a.current_journal_mode()! in [.off, .wal]
	a.journal_mode(.wal)!
	a.begin()!
	a.journal_mode(.delete) or {
		assert err.msg().contains('cannot change out of wal mode from within a transaction')
		a.rollback()!
		assert a.current_journal_mode()! == .wal
		return
	}
	assert false, 'leaving wal mode inside a transaction should fail'
}

// journal_mode_of returns the journal mode seen by a new connection to `path`.
fn journal_mode_of(path string) !sqlite.JournalMode {
	mut db := sqlite.connect(path)!
	defer {
		db.close() or {}
	}
	return db.current_journal_mode()!
}

fn test_journal_mode_of_an_in_memory_database() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	assert db.current_journal_mode()! == .memory
	db.journal_mode(.wal)!
	assert db.current_journal_mode()! == .memory
	db.journal_mode(.delete)!
	assert db.current_journal_mode()! == .memory
}
