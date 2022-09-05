module sqlite

$if freebsd || openbsd {
	#flag -I/usr/local/include
	#flag -L/usr/local/lib
}
$if windows {
	#flag windows -I@VEXEROOT/thirdparty/sqlite
	#flag windows -L@VEXEROOT/thirdparty/sqlite
	#flag windows @VEXEROOT/thirdparty/sqlite/sqlite3.o
} $else {
	#flag -lsqlite3
}

#include "sqlite3.h"

pub const (
	sqlite_ok    = 0
	sqlite_error = 1
	sqlite_row   = 100
	sqlite_done  = 101
)

pub enum SyncMode {
	off
	normal
	full
}

pub enum JournalMode {
	off
	delete
	truncate
	persist
	memory
}

struct C.sqlite3 {
}

struct C.sqlite3_stmt {
}

[heap]
struct Stmt {
	stmt &C.sqlite3_stmt
	db   &DB
}

struct SQLError {
	MessageError
}

//
[heap]
pub struct DB {
pub mut:
	is_open bool
mut:
	conn &C.sqlite3
}

pub fn (db &DB) str() string {
	return 'sqlite.DB{ conn: ' + ptr_str(db.conn) + ' }'
}

pub struct Row {
pub mut:
	vals []string
}

//
fn C.sqlite3_open(&char, &&C.sqlite3) int

fn C.sqlite3_close(&C.sqlite3) int

fn C.sqlite3_busy_timeout(db &C.sqlite3, ms int) int

fn C.sqlite3_last_insert_rowid(&C.sqlite3) i64

//
fn C.sqlite3_prepare_v2(&C.sqlite3, &char, int, &&C.sqlite3_stmt, &&char) int

fn C.sqlite3_step(&C.sqlite3_stmt) int

fn C.sqlite3_finalize(&C.sqlite3_stmt) int

//
fn C.sqlite3_column_name(&C.sqlite3_stmt, int) &char

fn C.sqlite3_column_text(&C.sqlite3_stmt, int) &u8

fn C.sqlite3_column_int(&C.sqlite3_stmt, int) int

fn C.sqlite3_column_int64(&C.sqlite3_stmt, int) i64

fn C.sqlite3_column_double(&C.sqlite3_stmt, int) f64

fn C.sqlite3_column_count(&C.sqlite3_stmt) int

//
fn C.sqlite3_errstr(int) &char

fn C.sqlite3_errmsg(&C.sqlite3) &char

fn C.sqlite3_free(voidptr)

// connect Opens the connection with a database.
pub fn connect(path string) ?DB {
	db := &C.sqlite3(0)
	code := C.sqlite3_open(&char(path.str), &db)
	if code != 0 {
		return IError(&SQLError{
			msg: unsafe { cstring_to_vstring(&char(C.sqlite3_errstr(code))) }
			code: code
		})
	}
	return DB{
		conn: db
		is_open: true
	}
}

// close Closes the DB.
// TODO: For all functions, determine whether the connection is
// closed first, and determine what to do if it is
pub fn (mut db DB) close() ?bool {
	code := C.sqlite3_close(db.conn)
	if code == 0 {
		db.is_open = false
	} else {
		return IError(&SQLError{
			msg: unsafe { cstring_to_vstring(&char(C.sqlite3_errstr(code))) }
			code: code
		})
	}
	return true // successfully closed
}

// Only for V ORM
fn get_int_from_stmt(stmt &C.sqlite3_stmt) int {
	x := C.sqlite3_step(stmt)
	if x != C.SQLITE_OK && x != C.SQLITE_DONE {
		C.puts(C.sqlite3_errstr(x))
	}

	res := C.sqlite3_column_int(stmt, 0)
	C.sqlite3_finalize(stmt)
	return res
}

// Returns last insert rowid
// https://www.sqlite.org/c3ref/last_insert_rowid.html
pub fn (db &DB) last_insert_rowid() i64 {
	return C.sqlite3_last_insert_rowid(db.conn)
}

// Returns a single cell with value int.
pub fn (db &DB) q_int(query string) int {
	stmt := &C.sqlite3_stmt(0)
	defer {
		C.sqlite3_finalize(stmt)
	}
	C.sqlite3_prepare_v2(db.conn, &char(query.str), query.len, &stmt, 0)
	C.sqlite3_step(stmt)

	res := C.sqlite3_column_int(stmt, 0)
	return res
}

// Returns a single cell with value string.
pub fn (db &DB) q_string(query string) string {
	stmt := &C.sqlite3_stmt(0)
	defer {
		C.sqlite3_finalize(stmt)
	}
	C.sqlite3_prepare_v2(db.conn, &char(query.str), query.len, &stmt, 0)
	C.sqlite3_step(stmt)

	val := unsafe { &u8(C.sqlite3_column_text(stmt, 0)) }
	return if val != &u8(0) { unsafe { tos_clone(val) } } else { '' }
}

// Execute the query on db, return an array of all the results, alongside any result code.
// Result codes: https://www.sqlite.org/rescode.html
[manualfree]
pub fn (db &DB) exec(query string) ([]Row, int) {
	stmt := &C.sqlite3_stmt(0)
	defer {
		C.sqlite3_finalize(stmt)
	}
	C.sqlite3_prepare_v2(db.conn, &char(query.str), query.len, &stmt, 0)
	nr_cols := C.sqlite3_column_count(stmt)
	mut res := 0
	mut rows := []Row{}
	for {
		res = C.sqlite3_step(stmt)
		// Result Code SQLITE_ROW; Another row is available
		if res != 100 {
			// C.puts(C.sqlite3_errstr(res))
			break
		}
		mut row := Row{}
		for i in 0 .. nr_cols {
			val := unsafe { &u8(C.sqlite3_column_text(stmt, i)) }
			if val == &u8(0) {
				row.vals << ''
			} else {
				row.vals << unsafe { tos_clone(val) }
			}
		}
		rows << row
	}
	return rows, res
}

// Execute a query, handle error code
// Return the first row from the resulting table
[manualfree]
pub fn (db &DB) exec_one(query string) ?Row {
	rows, code := db.exec(query)
	defer {
		unsafe { rows.free() }
	}
	if rows.len == 0 {
		return IError(&SQLError{
			msg: 'No rows'
			code: code
		})
	} else if code != 101 {
		return IError(&SQLError{
			msg: unsafe { cstring_to_vstring(&char(C.sqlite3_errstr(code))) }
			code: code
		})
	}
	res := rows[0]
	return res
}

[manualfree]
pub fn (db &DB) error_message(code int, query string) IError {
	errmsg := unsafe { cstring_to_vstring(&char(C.sqlite3_errmsg(db.conn))) }
	msg := '$errmsg ($code) ($query)'
	unsafe { errmsg.free() }
	return SQLError{
		msg: msg
		code: code
	}
}

// Execute a query returning only the result code.
// In case you don't expect any row results, but still want a result code.
// e.g. INSERT INTO ... VALUES (...)
pub fn (db &DB) exec_none(query string) int {
	stmt := &C.sqlite3_stmt(0)
	C.sqlite3_prepare_v2(db.conn, &char(query.str), query.len, &stmt, 0)
	code := C.sqlite3_step(stmt)
	C.sqlite3_finalize(stmt)
	return code
}

/*
TODO
pub fn (db &DB) exec_param(query string, param string) []Row {
}
*/

// Issue a "create table if not exists" command to the db.
// Creates table named 'table_name', with columns generated from 'columns' array.
// Default columns type will be TEXT.
pub fn (db &DB) create_table(table_name string, columns []string) {
	db.exec('create table if not exists $table_name (' + columns.join(',\n') + ')')
}

// Set a busy timeout in milliseconds.
// Sleeps for a specified amount of time when a table is locked. The handler
// will sleep multiple times until at least "ms" milliseconds of sleeping have accumulated.
// (see https://www.sqlite.org/c3ref/busy_timeout.html)
pub fn (db &DB) busy_timeout(ms int) int {
	return C.sqlite3_busy_timeout(db.conn, ms)
}

// Sets disk synchronization mode,
// which controls how aggressively SQLite will write data to physical storage.
// off: No syncs at all. (fastest)
// normal: Sync after each sequence of critical disk operations.
// full: Sync after each critical disk operation (slowest).
pub fn (db &DB) synchronization_mode(sync_mode SyncMode) {
	if sync_mode == .off {
		db.exec('pragma synchronous = OFF;')
	} else if sync_mode == .full {
		db.exec('pragma synchronous = FULL;')
	} else {
		db.exec('pragma synchronous = NORMAL;')
	}
}

// Controls how the journal file is stored and processed.
// off: No journal record is kept. (fastest)
// memory: Journal record is held in memory, rather than on disk.
// delete: At the conclusion of a transaction, journal file is deleted.
// truncate: Journal file is truncated to a length of zero bytes.
// persist: Journal file is left in place, but the header is overwritten to indicate journal is no longer valid.
pub fn (db &DB) journal_mode(journal_mode JournalMode) {
	if journal_mode == .off {
		db.exec('pragma journal_mode = OFF;')
	} else if journal_mode == .delete {
		db.exec('pragma journal_mode = DELETE;')
	} else if journal_mode == .truncate {
		db.exec('pragma journal_mode = TRUNCATE;')
	} else if journal_mode == .persist {
		db.exec('pragma journal_mode = PERSIST;')
	} else if journal_mode == .memory {
		db.exec('pragma journal_mode = MEMORY;')
	} else {
		db.exec('pragma journal_mode = MEMORY;')
	}
}
