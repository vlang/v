module sqlite

#flag -lsqlite3
#flag freebsd -I/usr/local/include
#flag freebsd -Wl -L/usr/local/lib -lsqlite3
#include "sqlite3.h"

struct C.sqlite3 {}
struct C.sqlite3_stmt {}

pub struct DB {
mut:
	conn &C.sqlite3
}

pub struct Row {
pub mut:
	vals []string
}

fn C.sqlite3_column_text(voidptr, int) byteptr
fn C.sqlite3_column_int(voidptr, int) int
fn C.sqlite3_open()
fn C.sqlite3_step() int
fn C.sqlite3_prepare_v2()
fn C.sqlite3_finalize()
fn C.sqlite3_column_count(voidptr) int

// Opens the connection with a database.
pub fn connect(path string) DB {
	db := &C.sqlite3(0)
	C.sqlite3_open(path.str, &db)
	return DB{
		conn: db
	}
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
pub fn (db DB) exec(query string) ([]Row,int) {
	stmt := &C.sqlite3_stmt(0)
	C.sqlite3_prepare_v2(db.conn, query.str, -1, &stmt, 0)
	nr_cols := C.sqlite3_column_count(stmt)
	mut res := 0
	mut rows := []Row{}
	for {
		res = C.sqlite3_step(stmt)
		// Result Code SQLITE_ROW; Another row is available
		if res != 100 {
			break
		}
		mut row := Row{}
		for i in 0 .. nr_cols {
			val := tos_clone(C.sqlite3_column_text(stmt, i))
			row.vals << val
		}
		rows << row
	}
	return rows,res
}

// Execute a query, handle error code
// Return the first row from the resulting table
pub fn (db DB) exec_one(query string) ?Row {
	rows,code := db.exec(query)
	if rows.len == 0 || code != 101 {
		return error('SQL Error: Rows #$rows.len Return code $code')
	}
	return rows[0]
}

// In case you don't expect any result, but still want an error code
// e.g. INSERT INTO ... VALUES (...)
pub fn (db DB) exec_none(query string) int {
	_,code := db.exec(query)
	return code
}

/* TODO
pub fn (db DB) exec_param(query string, param string) []Row {
}
*/
