module mysql

// if mysql.h is not in your CPATH (include path), set environment CPATH 
//     export CPATH=$CPATH:/usr/include/mysql
// or include -cflags flag to v compiler
//     v -cflags '-I/usr/include/mysql' program.v

#flag -lmysqlclient
#flag linux -I/usr/include/mysql
#include <mysql.h>

pub struct DB {
	conn &C.MYSQL
}

pub struct Result {
	result &C.MYSQL_RES
}

pub struct Row {
pub mut:
	vals []string
}

struct C.MYSQL
struct C.MYSQL_RES

fn C.mysql_init(mysql &C.MYSQL) &C.MYSQL
fn C.mysql_real_connect(mysql &C.MYSQL, host byteptr, user byteptr, passwd byteptr, db byteptr, port u32, unix_socket byteptr, clientflag u64) &C.MYSQL
fn C.mysql_query(mysql &C.MYSQL, q byteptr) int
fn C.mysql_error(mysql &C.MYSQL) byteptr
fn C.mysql_errno(mysql &C.MYSQL) int
fn C.mysql_num_fields(res &C.MYSQL_RES) int
fn C.mysql_store_result(mysql &C.MYSQL) &C.MYSQL_RES
fn C.mysql_fetch_row(res &C.MYSQL_RES) &byteptr
fn C.mysql_free_result(res &C.MYSQL_RES)
fn C.mysql_real_escape_string_quote(mysql &C.MYSQL, to byteptr, from byteptr, len u64, quote byte) u64
fn C.mysql_close(sock &C.MYSQL)

pub fn connect(server, user, passwd, dbname string) ?DB {
	conn := C.mysql_init(0)
	if isnil(conn) {
		return error_with_code(get_error_msg(conn), get_errno(conn))
	}
	conn2 := C.mysql_real_connect(conn, server.str, user.str, passwd.str, dbname.str, 0, 0, 0)
	if isnil(conn2) {
		return error_with_code(get_error_msg(conn), get_errno(conn))
	}
	return DB {conn: conn2}
}

pub fn (db DB) query(q string) ?Result {
	ret := C.mysql_query(db.conn, q.str)
	if ret != 0 {
		return error_with_code(get_error_msg(db.conn), get_errno(db.conn))
	}
	res := C.mysql_store_result(db.conn)
	return Result {result: res}
}

pub fn (db DB) escape_string(s string) string {
    len := strlen(s.str)
    to := malloc(2 * len + 1)
    quote := byte(39) // single quote

    C.mysql_real_escape_string_quote(db.conn, to, s.str, len, quote)
    return string(to)
}

pub fn (db DB) close() {
	C.mysql_close(db.conn)
}

pub fn (r Result) fetch_row() &byteptr {
	return C.mysql_fetch_row(r.result)
}

pub fn (r Result) num_fields() int {
	return C.mysql_num_fields(r.result)
}

pub fn (r Result) rows() []Row {
	mut rows := []Row
	nr_cols := r.num_fields()
	for rr := r.fetch_row(); rr; rr = r.fetch_row() {
		mut row := Row{}
		for i := 0; i < nr_cols; i++ {
			if rr[i] == 0 {
				row.vals << ''
			} else {
				row.vals << string(rr[i])
			}
		}
		rows << row
	}
	return rows
}

pub fn (r Result) free() {
	C.mysql_free_result(r.result)
}

fn get_error_msg(conn &C.MYSQL) string {
	return string(C.mysql_error(conn))
}

fn get_errno(conn &C.MYSQL) int {
	return C.mysql_errno(conn)
}

