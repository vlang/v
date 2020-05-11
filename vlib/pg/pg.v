module pg

#flag -lpq
#flag linux -I/usr/include/postgresql
#flag darwin -I/opt/local/include/postgresql11
#flag windows -I @VROOT/thirdparty/pg/include
#flag windows -L @VROOT/thirdparty/pg/win64
#include <libpq-fe.h>

pub struct DB {
mut:
	conn &C.PGconn
}

pub struct Row {
pub mut:
	vals []string
}

struct C.PGResult { }

pub struct Config {
pub:
	host string
	port int = 5432
	user string
	password string
	dbname string
}

fn C.PQconnectdb(a byteptr) &C.PGconn
fn C.PQerrorMessage(voidptr) byteptr
fn C.PQgetvalue(voidptr, int, int) byteptr
fn C.PQstatus(voidptr) int
fn C.PQntuples(voidptr) int
fn C.PQnfields(voidptr) int
fn C.PQexec(voidptr) voidptr
fn C.PQexecParams(voidptr) voidptr

pub fn connect(config Config) ?DB {
	conninfo := 'host=$config.host port=$config.port user=$config.user dbname=$config.dbname password=$config.password'
	conn := C.PQconnectdb(conninfo.str)
	status := C.PQstatus(conn)
	println("status=$status")
	if status != C.CONNECTION_OK {
		error_msg := C.PQerrorMessage(conn)
		return error ('Connection to a PG database failed: ' + string(error_msg))
	}
	return DB {conn: conn}
}

fn res_to_rows(res voidptr) []Row {
	nr_rows := C.PQntuples(res)
	nr_cols := C.PQnfields(res)
	mut rows := []Row{}
	for i in 0..nr_rows {
		mut row := Row{}
		for j in 0..nr_cols {
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

pub fn (db DB) q_strings(query string) []Row {
	return db.exec(query)
}

pub fn (db DB) exec(query string) []Row {
	res := C.PQexec(db.conn, query.str)
	e := string(C.PQerrorMessage(db.conn))
	if e != '' {
		println('pg exec error:')
		println(e)
		return res_to_rows(res)
	}
	return res_to_rows(res)
}

fn rows_first_or_empty(rows []Row) ?Row {
	if rows.len == 0 {
		return error('no row')
	}
	return rows[0]
}

pub fn (db DB) exec_one(query string) ?Row {
	res := C.PQexec(db.conn, query.str)
	e := string(C.PQerrorMessage(db.conn))
	if e != '' {
		return error('pg exec error: "$e"')
	}
	row := rows_first_or_empty( res_to_rows(res) ) or { return error(err) }
	return row
}

// The entire function can be considered unsafe because of the malloc and the
// free. This prevents warnings and doesn't seem to affect behavior.
pub fn (db DB) exec_param_many(query string, params []string) []Row {
	unsafe {
		mut param_vals := &byteptr( malloc( params.len * sizeof(byteptr) ) )
		for i in 0..params.len {
			param_vals[i] = params[i].str
		}
		res := C.PQexecParams(db.conn, query.str, params.len, 0, param_vals, 0, 0, 0)
		free(param_vals)
		return db.handle_error_or_result(res, 'exec_param_many')
	}
}

pub fn (db DB) exec_param2(query string, param, param2 string) []Row {
	mut param_vals := [2]byteptr
	param_vals[0] = param.str
	param_vals[1] = param2.str
	res := C.PQexecParams(db.conn, query.str, 2, 0, param_vals, 0, 0, 0)
	return db.handle_error_or_result(res, 'exec_param2')
}

pub fn (db DB) exec_param(query string, param string) []Row {
	mut param_vals := [1]byteptr
	param_vals[0] = param.str
	res := C.PQexecParams(db.conn, query.str, 1, 0, param_vals, 0, 0, 0)
	return db.handle_error_or_result(res, 'exec_param')
}

fn (db DB) handle_error_or_result(res voidptr, elabel string) []Row {
	e := string(C.PQerrorMessage(db.conn))
	if e != '' {
		println('pg $elabel error:')
		println(e)
		return res_to_rows(res)
	}
	return res_to_rows(res)
}
