module pg

import os
import time

#flag -lpq
#flag linux -I/usr/include/postgresql
#include <libpq-fe.h>

struct DB {
mut:
	conn *C.PGconn
}

struct Row {
pub:
	vals []string
}

import const (
	CONNECTION_OK
) 

struct C.PGResult { }

fn C.PQconnectdb(a byteptr) *C.PGconn
fn C.PQerrorMessage(voidptr) byteptr 

pub fn connect(dbname, user string) DB {
	conninfo := 'host=localhost user=$user dbname=$dbname'
	conn:=C.PQconnectdb(conninfo.cstr())
	status := C.PQstatus(conn)
	if status != CONNECTION_OK { 
		error_msg := C.PQerrorMessage(conn) 
		eprintln('Connection to a PG database failed: ' + string(error_msg)) 
		exit(1) 
	}
	return DB {conn: conn} 
}

fn res_to_rows(res voidptr) []pg.Row {
	nr_rows := C.PQntuples(res) 
	nr_cols := C.PQnfields(res) 
	mut rows := []pg.Row
	for i := 0; i < nr_rows; i++ {
		mut row := Row{}
		for j := 0; j < nr_cols; j++ {
			val := C.PQgetvalue(res, i, j) 
			row.vals << string(val)  
		}
		rows << row
	}
	return rows
}

pub fn (db DB) q_int(query string) int {
	rows := db.exec(query)
	if rows.len == 0 {
		println('q_int "$query" not found')
		return 0
	}
	row := rows[0]
	if row.vals.len == 0 {
		return 0
	}
	val := row.vals[0]
	return val.int() 
}

pub fn (db DB) q_string(query string) string {
	rows := db.exec(query)
	if rows.len == 0 {
		println('q_string "$query" not found')
		return ''
	}
	row := rows[0]
	if row.vals.len == 0 {
		return ''
	}
	val := row.vals[0]
	return val
}

pub fn (db DB) q_strings(query string) []pg.Row {
	return db.exec(query)
}

pub fn (db DB) exec(query string) []pg.Row {
	res := C.PQexec(db.conn, query.cstr())
	e := string(C.PQerrorMessage(db.conn))
	if e != '' {
		println('pg exec error:')
		println(e)
		return res_to_rows(res)
	}
	return res_to_rows(res)
}


// 
pub fn (db DB) exec_param2(query string, param, param2 string) []pg.Row {
	mut param_vals := [2]byteptr 
	param_vals[0] = param.str 
	param_vals[1] = param2.str 
	res := C.PQexecParams(db.conn, query.str, 2, 0, param_vals, 0, 0, 0)  
	return res_to_rows(res)
}

pub fn (db DB) exec_param(query string, param string) []pg.Row {
	mut param_vals := [1]byteptr 
	param_vals[0] = param.str 
	res := C.PQexecParams(db.conn, query.str, 1, 0, param_vals, 0, 0, 0)  
	return res_to_rows(res)
}

