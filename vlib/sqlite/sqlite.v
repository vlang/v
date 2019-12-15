module sqlite

#flag -lsqlite3
#flag freebsd -I/usr/local/include
#flag freebsd -Wl -L/usr/local/lib -lsqlite3
#include "sqlite3.h"

struct C.sqlite3
struct C.sqlite3_stmt

struct DB {
mut:
	conn &C.sqlite3
}

struct Row {
pub mut:
	vals []string
}

pub fn connect(path string) DB {
	db := &C.sqlite3(0)
	C.sqlite3_open(path.str, &db)
	return DB {conn: db}
}

pub fn (db DB) q_int(query string) int {
	stmt := &C.sqlite3_stmt(0)
	C.sqlite3_prepare_v2(db.conn, query.str, - 1, &stmt, 0)
	C.sqlite3_step(stmt)
	res := C.sqlite3_column_int(stmt, 0)
	C.sqlite3_finalize(stmt)
	return res
}

fn C.sqlite3_column_text(voidptr, int) byteptr
fn C.sqlite3_column_int(voidptr, int) int
fn C.sqlite3_open()
fn C.sqlite3_step() int
fn C.sqlite3_prepare_v2()
fn C.sqlite3_finalize()

pub fn (db DB) q_string(query string) string {
	stmt := &C.sqlite3_stmt(0)
	C.sqlite3_prepare_v2(db.conn, query.str, - 1, &stmt, 0)
	C.sqlite3_step(stmt)
	f := C.sqlite3_column_text(stmt, 0)
	res := tos_clone(C.sqlite3_column_text(stmt, 0))
	C.sqlite3_finalize(stmt)
	return res
}

fn C.sqlite3_column_count(voidptr) int

pub fn (db DB) exec(query string) []Row {
	stmt := &C.sqlite3_stmt(0)
	C.sqlite3_prepare_v2(db.conn, query.str, - 1, &stmt, 0)
	nr_cols := C.sqlite3_column_count(stmt)
	//println('nr cols $nr_cols')
	mut rows := []Row
	for {
		ret := C.sqlite3_step(stmt)
		if ret != 0x64 {
			break
		}
		mut row := Row{}
		for i in 0..nr_cols {
			val := tos_clone(C.sqlite3_column_text(stmt, i))
			row.vals << val
			//println(val)
		}
		rows << row
	}
	return rows
}

pub fn (db DB) exec_one(query string) ?Row {
	rows := db.exec(query)
	return rows[0]
}

/*
pub fn (db DB) exec_param(query string, param string) []Row {
}
*/


