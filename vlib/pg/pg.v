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

struct C.PGResult

pub struct Config {
pub:
	host     string
	port     int = 5432
	user     string
	password string
	dbname   string
}

fn C.PQconnectdb(a byteptr) &C.PGconn

fn C.PQerrorMessage(voidptr) byteptr

fn C.PQgetvalue(&C.PGResult, int, int) byteptr

fn C.PQstatus(voidptr) int

fn C.PQresultStatus(voidptr) int

fn C.PQntuples(&C.PGResult) int

fn C.PQnfields(&C.PGResult) int

fn C.PQexec(voidptr) &C.PGResult

fn C.PQexecParams(voidptr) &C.PGResult

fn C.PQclear(&C.PGResult) voidptr

fn C.PQfinish(voidptr)

// connect makes a new connection to the database server using
// the parameters from the `Config` structure, returning
// a connection error when something goes wrong
pub fn connect(config Config) ?DB {
	conninfo := 'host=$config.host port=$config.port user=$config.user dbname=$config.dbname password=$config.password'
	conn := C.PQconnectdb(conninfo.str)
	if conn == 0 {
		return error('libpq memory allocation error')
	}
	status := C.PQstatus(conn)
	if status != C.CONNECTION_OK {
		// We force the construction of a new string as the
		// error message will be freed by the next `PQfinish`
		// call
		c_error_msg := unsafe {C.PQerrorMessage(conn).vstring()}
		error_msg := '$c_error_msg'
		C.PQfinish(conn)
		return error('Connection to a PG database failed: $error_msg')
	}
	return DB{
		conn: conn
	}
}

fn res_to_rows(res voidptr) []Row {
	nr_rows := C.PQntuples(res)
	nr_cols := C.PQnfields(res)

	mut rows := []Row{}
	for i in 0 .. nr_rows {
		mut row := Row{}
		for j in 0 .. nr_cols {
			val := C.PQgetvalue(res, i, j)
			sval := unsafe {val.vstring()}
			row.vals << sval
		}
		rows << row
	}

	C.PQclear(res)
	return rows
}

// close frees the underlying resource allocated by the database connection
pub fn (db DB) close() {
	C.PQfinish(db.conn)
}

// q_int submit a command to the database server and
// returns an the first field in the first tuple
// converted to an int. If no row is found or on
// command failure, an error is returned
pub fn (db DB) q_int(query string) ?int {
	rows := db.exec(query) ?
	if rows.len == 0 {
		return error('q_int "$query" not found')
	}
	row := rows[0]
	if row.vals.len == 0 {
		return 0
	}
	val := row.vals[0]
	return val.int()
}

// q_int submit a command to the database server and
// returns an the first field in the first tuple
// as a string. If no row is found or on
// command failure, an error is returned
pub fn (db DB) q_string(query string) ?string {
	rows := db.exec(query) ?
	if rows.len == 0 {
		return error('q_string "$query" not found')
	}
	row := rows[0]
	if row.vals.len == 0 {
		return ''
	}
	val := row.vals[0]
	return val
}

// q_strings submit a command to the database server and
// returns the resulting row set. Alias of `exec`
pub fn (db DB) q_strings(query string) ?[]Row {
	return db.exec(query)
}

// exec submit a command to the database server and wait
// for the result, returning an error on failure and a
// row set on success
pub fn (db DB) exec(query string) ?[]Row {
	res := C.PQexec(db.conn, query.str)
	return db.handle_error_or_result(res, 'exec')
}

fn rows_first_or_empty(rows []Row) ?Row {
	if rows.len == 0 {
		return error('no row')
	}
	return rows[0]
}

pub fn (db DB) exec_one(query string) ?Row {
	res := C.PQexec(db.conn, query.str)
	e := unsafe {C.PQerrorMessage(db.conn).vstring()}
	if e != '' {
		return error('pg exec error: "$e"')
	}
	row := rows_first_or_empty(res_to_rows(res)) ?
	return row
}

// exec_param_many executes a query with the provided parameters
pub fn (db DB) exec_param_many(query string, params []string) ?[]Row {
	mut param_vals := []charptr{ len: params.len }
	for i in 0 .. params.len {
		param_vals[i] = params[i].str
	}

	res := C.PQexecParams(db.conn, query.str, params.len, 0, param_vals.data, 0, 0, 0)
	return db.handle_error_or_result(res, 'exec_param_many')
}

pub fn (db DB) exec_param2(query string, param string, param2 string) ?[]Row {
	return db.exec_param_many(query, [param, param2])
}

pub fn (db DB) exec_param(query string, param string) ?[]Row {
	return db.exec_param_many(query, [param])
}

fn (db DB) handle_error_or_result(res voidptr, elabel string) ?[]Row {
	e := unsafe {C.PQerrorMessage(db.conn).vstring()}
	if e != '' {
		C.PQclear(res)
		return error('pg $elabel error:\n$e')
	}
	return res_to_rows(res)
}
