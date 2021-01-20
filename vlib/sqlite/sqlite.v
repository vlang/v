module sqlite

#flag darwin  -lsqlite3
#flag linux   -lsqlite3
#flag solaris -lsqlite3
#flag freebsd -I/usr/local/include
#flag freebsd -Wl -L/usr/local/lib -lsqlite3
#flag windows -I@VROOT/thirdparty/sqlite
#flag windows -L@VROOT/thirdparty/sqlite
#flag windows @VROOT/thirdparty/sqlite/sqlite3.o
// #flag linux -I @VROOT/thirdparty/sqlite
// #flag @VROOT/thirdparty/sqlite/sqlite.c
#include "sqlite3.h"
//
struct C.sqlite3 {
}

struct C.sqlite3_stmt {
}

//
pub struct DB {
pub mut:
	is_open bool
mut:
	conn &C.sqlite3
}

pub fn (db DB) str() string {
	return 'sqlite.DB{ conn: ' + ptr_str(db.conn) + ' }'
}

pub struct Row {
pub mut:
	vals []string
}

//
fn C.sqlite3_open(charptr, &&C.sqlite3) int

fn C.sqlite3_close(&C.sqlite3) int

//
fn C.sqlite3_prepare_v2(&C.sqlite3, charptr, int, &&sqlite3_stmt, &charptr) int

fn C.sqlite3_step(&C.sqlite3_stmt) int

fn C.sqlite3_finalize(&C.sqlite3_stmt) int

//
fn C.sqlite3_column_name(&C.sqlite3_stmt, int) charptr

fn C.sqlite3_column_text(&C.sqlite3_stmt, int) byteptr

fn C.sqlite3_column_int(&C.sqlite3_stmt, int) int

fn C.sqlite3_column_int64(&C.sqlite3_stmt, int) int64

fn C.sqlite3_column_double(&C.sqlite3_stmt, int) f64

fn C.sqlite3_column_count(&C.sqlite3_stmt) int

//
fn C.sqlite3_errstr(int) charptr

fn C.sqlite3_free(voidptr)

// connect Opens the connection with a database.
pub fn connect(path string) ?DB {
	db := &C.sqlite3(0)
	if C.sqlite3_open(path.str, &db) != 0 {
		return error('sqlite db error')
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
		return error('sqlite db error: failed to close with code: $code')
	}
	return true // successfully closed
}

// Only for V ORM
fn (db DB) init_stmt(query string) &C.sqlite3_stmt {
	stmt := &C.sqlite3_stmt(0)
	C.sqlite3_prepare_v2(db.conn, query.str, -1, &stmt, 0)
	return stmt
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

// Returns a single cell with value int.
pub fn (db DB) q_int(query string) int {
	stmt := &C.sqlite3_stmt(0)
	C.sqlite3_prepare_v2(db.conn, query.str, -1, &stmt, 0)
	C.sqlite3_step(stmt)
	res := C.sqlite3_column_int(stmt, 0)
	C.sqlite3_finalize(stmt)
	return res
}

// Returns a single cell with value string.
pub fn (db DB) q_string(query string) string {
	stmt := &C.sqlite3_stmt(0)
	C.sqlite3_prepare_v2(db.conn, query.str, -1, &stmt, 0)
	C.sqlite3_step(stmt)
	res := tos_clone(C.sqlite3_column_text(stmt, 0))
	C.sqlite3_finalize(stmt)
	return res
}

// Execute the query on db, return an array of all the results, alongside any result code.
// Result codes: https://www.sqlite.org/rescode.html
pub fn (db DB) exec(query string) ([]Row, int) {
	stmt := &C.sqlite3_stmt(0)
	C.sqlite3_prepare_v2(db.conn, query.str, -1, &stmt, 0)
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
			val := tos_clone(C.sqlite3_column_text(stmt, i))
			row.vals << val
		}
		rows << row
	}
	C.sqlite3_finalize(stmt)
	return rows, res
}

// Execute a query, handle error code
// Return the first row from the resulting table
pub fn (db DB) exec_one(query string) ?Row {
	rows, code := db.exec(query)
	if rows.len == 0 || code != 101 {
		return error('SQL Error: Rows #$rows.len Return code $code')
	}
	return rows[0]
}

// In case you don't expect any result, but still want an error code
// e.g. INSERT INTO ... VALUES (...)
pub fn (db DB) exec_none(query string) int {
	_, code := db.exec(query)
	return code
}

/*
TODO
pub fn (db DB) exec_param(query string, param string) []Row {
}
*/
pub fn (db DB) insert<T>(x T) {
}

pub fn (db DB) create_table(table_name string, rows []string) {
	db.exec('create table $table_name (' + rows.join(',\n') + ')')
}
