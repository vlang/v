module pg

import io

$if $pkgconfig('libpq') {
	#pkgconfig --cflags --libs libpq
} $else {
	#flag -lpq
	#flag linux -I/usr/include/postgresql

	#flag darwin -I/opt/local/include/postgresql11
	#flag darwin -L/opt/local/lib/postgresql11

	#flag darwin -I/usr/local/opt/libpq/include
	#flag darwin -L/usr/local/opt/libpq/lib

	#flag darwin -I/opt/homebrew/include
	#flag darwin -L/opt/homebrew/lib

	#flag darwin -I/opt/homebrew/opt/libpq/include
	#flag darwin -L/opt/homebrew/opt/libpq/lib

	#flag windows -I @VEXEROOT/thirdparty/pg/include
	#flag windows -L @VEXEROOT/thirdparty/pg/win64
}

// PostgreSQL Source Code
// https://doxygen.postgresql.org/libpq-fe_8h.html
#include <libpq-fe.h>

// for PG_VERSION_NUM, which is defined everywhere at least since PG 9.5
#include <pg_config.h>

// for orm
#include <arpa/inet.h>

#include "@VMODROOT/vlib/db/pg/compatibility.h"

pub struct DB {
mut:
	conn voidptr = unsafe { nil }
}

pub struct Row {
pub mut:
	vals []string
}

pub struct Config {
pub:
	host     string = 'localhost'
	port     int    = 5432
	user     string
	password string
	dbname   string
}

pub struct Service {
pub:
	name string
}

//

struct C.pg_result {}

struct C.pg_conn {}

[typedef]
pub struct C.PGresult {}

[typedef]
pub struct C.PGconn {}

pub enum ConnStatusType {
	ok = C.CONNECTION_OK
	bad = C.CONNECTION_BAD
	// Non-blocking mode only below here
	// The existence of these should never be relied upon - they should only be used for user feedback or similar purposes.
	started = C.CONNECTION_STARTED // Waiting for connection to be made.
	made = C.CONNECTION_MADE // Connection OK; waiting to send.
	awaiting_response = C.CONNECTION_AWAITING_RESPONSE // Waiting for a response from the postmaster.
	auth_ok = C.CONNECTION_AUTH_OK // Received authentication; waiting for backend startup.
	setenv = C.CONNECTION_SETENV // Negotiating environment.
	ssl_startup = C.CONNECTION_SSL_STARTUP // Negotiating SSL.
	needed = C.CONNECTION_NEEDED // Internal state: connect() needed . Available in PG 8
	check_writable = C.CONNECTION_CHECK_WRITABLE // Check if we could make a writable connection. Available since PG 10
	consume = C.CONNECTION_CONSUME // Wait for any pending message and consume them. Available since PG 10
	gss_startup = C.CONNECTION_GSS_STARTUP // Negotiating GSSAPI; available since PG 12
}

[typedef]
pub enum ExecStatusType {
	empty_query = C.PGRES_EMPTY_QUERY // empty query string was executed
	command_ok = C.PGRES_COMMAND_OK // a query command that doesn't return anything was executed properly by the backend
	tuples_ok = C.PGRES_TUPLES_OK // a query command that returns tuples was executed properly by the backend, PGresult contains the result tuples
	copy_out = C.PGRES_COPY_OUT // Copy Out data transfer in progress
	copy_in = C.PGRES_COPY_IN // Copy In data transfer in progress
	bad_response = C.PGRES_BAD_RESPONSE // an unexpected response was recv'd from the backend
	nonfatal_error = C.PGRES_NONFATAL_ERROR // notice or warning message
	fatal_error = C.PGRES_FATAL_ERROR // query failed
	copy_both = C.PGRES_COPY_BOTH // Copy In/Out data transfer in progress
	single_tuple = C.PGRES_SINGLE_TUPLE // single tuple from larger resultset
}

//

fn C.PQconnectdb(const_conninfo &char) &C.PGconn

fn C.PQstatus(const_conn &C.PGconn) int

fn C.PQerrorMessage(const_conn &C.PGconn) &char

fn C.PQexec(res &C.PGconn, const_query &char) &C.PGresult

//

fn C.PQgetvalue(const_res &C.PGresult, int, int) &char

fn C.PQresultStatus(const_res &C.PGresult) int

fn C.PQntuples(const_res &C.PGresult) int

fn C.PQnfields(const_res &C.PGresult) int

// Params:
// const Oid *paramTypes
// const char *const *paramValues
// const int *paramLengths
// const int *paramFormats
fn C.PQexecParams(conn &C.PGconn, const_command &char, nParams int, const_paramTypes &int, const_paramValues &char, const_paramLengths &int, const_paramFormats &int, resultFormat int) &C.PGresult

fn C.PQputCopyData(conn &C.PGconn, const_buffer &char, nbytes int) int

fn C.PQputCopyEnd(conn &C.PGconn, const_errmsg &char) int

fn C.PQgetCopyData(conn &C.PGconn, buffer &&char, async int) int

// cleanup

fn C.PQclear(res &C.PGresult)

fn C.PQfreemem(ptr voidptr)

fn C.PQfinish(conn &C.PGconn)

// connect makes a new connection to the database server using
// the parameters from the `Config` structure, returning
// a connection error when something goes wrong
pub fn connect(config Config) !DB {
	conninfo := 'host=${config.host} port=${config.port} user=${config.user} dbname=${config.dbname} password=${config.password}'

	return do_connect(conninfo)!
}

// connect makes a new connection to the database server using
// the parameters from the `Config` structure, returning
// a connection error when something goes wrong
pub fn connect_with_service(service Service) !DB {
	conninfo := 'service=${service.name}'

	return do_connect(conninfo)!
}

// do_connect makes a new connection to the database server using
// the `conninfo` connection string, returning
// a connection error when something goes wrong
fn do_connect(conninfo string) !DB {
	conn := C.PQconnectdb(&char(conninfo.str))
	if conn == 0 {
		return error('libpq memory allocation error')
	}
	status := unsafe { ConnStatusType(C.PQstatus(conn)) }
	if status != .ok {
		// We force the construction of a new string as the
		// error message will be freed by the next `PQfinish`
		// call
		c_error_msg := unsafe { C.PQerrorMessage(conn).vstring() }
		error_msg := '${c_error_msg}'
		C.PQfinish(conn)
		return error('Connection to a PG database failed: ${error_msg}')
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
			sval := unsafe { val.vstring() }
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
pub fn (db DB) q_int(query string) !int {
	rows := db.exec(query)!
	if rows.len == 0 {
		return error('q_int "${query}" not found')
	}
	row := rows[0]
	if row.vals.len == 0 {
		return 0
	}
	val := row.vals[0]
	return val.int()
}

// q_string submit a command to the database server and
// returns an the first field in the first tuple
// as a string. If no row is found or on
// command failure, an error is returned
pub fn (db DB) q_string(query string) !string {
	rows := db.exec(query)!
	if rows.len == 0 {
		return error('q_string "${query}" not found')
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
pub fn (db DB) q_strings(query string) ![]Row {
	return db.exec(query)
}

// exec submits a command to the database server and wait for the result, returning an error on failure and a row set on success
pub fn (db DB) exec(query string) ![]Row {
	res := C.PQexec(db.conn, &char(query.str))
	return db.handle_error_or_result(res, 'exec')
}

fn rows_first_or_empty(rows []Row) !Row {
	if rows.len == 0 {
		return error('no row')
	}
	return rows[0]
}

// exec_one executes a query and returns its first row as a result, or an error on failure
pub fn (db DB) exec_one(query string) !Row {
	res := C.PQexec(db.conn, &char(query.str))
	e := unsafe { C.PQerrorMessage(db.conn).vstring() }
	if e != '' {
		return error('pg exec error: "${e}"')
	}
	row := rows_first_or_empty(res_to_rows(res))!
	return row
}

// exec_param_many executes a query with the parameters provided as ($1), ($2), ($n)
pub fn (db DB) exec_param_many(query string, params []string) ![]Row {
	unsafe {
		mut param_vals := []&char{len: params.len}
		for i in 0 .. params.len {
			param_vals[i] = &char(params[i].str)
		}

		res := C.PQexecParams(db.conn, &char(query.str), params.len, 0, param_vals.data,
			0, 0, 0)
		return db.handle_error_or_result(res, 'exec_param_many')
	}
}

// exec_param2 executes a query with 1 parameter ($1), and returns either an error on failure, or the full result set on success
pub fn (db DB) exec_param(query string, param string) ![]Row {
	return db.exec_param_many(query, [param])
}

// exec_param2 executes a query with 2 parameters ($1) and ($2), and returns either an error on failure, or the full result set on success
pub fn (db DB) exec_param2(query string, param string, param2 string) ![]Row {
	return db.exec_param_many(query, [param, param2])
}

fn (db DB) handle_error_or_result(res voidptr, elabel string) ![]Row {
	e := unsafe { C.PQerrorMessage(db.conn).vstring() }
	if e != '' {
		C.PQclear(res)
		$if trace_pg_error ? {
			eprintln('pg error: ${e}')
		}
		return error('pg ${elabel} error:\n${e}')
	}
	return res_to_rows(res)
}

// copy_expert executes COPY command
// https://www.postgresql.org/docs/9.5/libpq-copy.html
pub fn (db DB) copy_expert(query string, mut file io.ReaderWriter) !int {
	mut res := C.PQexec(db.conn, &char(query.str))
	status := unsafe { ExecStatusType(C.PQresultStatus(res)) }
	defer {
		C.PQclear(res)
	}

	e := unsafe { C.PQerrorMessage(db.conn).vstring() }
	if e != '' {
		return error('pg copy error:\n${e}')
	}

	if status == .copy_in {
		mut buf := []u8{len: 4 * 1024}
		for {
			n := file.read(mut buf) or {
				msg := 'pg copy error: Failed to read from input'
				C.PQputCopyEnd(db.conn, &char(msg.str))
				return err
			}
			if n <= 0 {
				break
			}

			code := C.PQputCopyData(db.conn, buf.data, n)
			if code == -1 {
				return error('pg copy error: Failed to send data, code=${code}')
			}
		}

		code := C.PQputCopyEnd(db.conn, &char(0))

		if code != 1 {
			return error('pg copy error: Failed to finish copy command, code: ${code}')
		}
	} else if status == .copy_out {
		for {
			address := &char(0)
			n_bytes := C.PQgetCopyData(db.conn, &address, 0)
			if n_bytes > 0 {
				mut local_buf := []u8{len: n_bytes}
				unsafe { C.memcpy(&u8(local_buf.data), address, n_bytes) }
				file.write(local_buf) or {
					C.PQfreemem(address)
					return err
				}
			} else if n_bytes == -1 {
				break
			} else if n_bytes == -2 {
				// consult PQerrorMessage for the reason
				return error('pg copy error: read error')
			}
			if address != 0 {
				C.PQfreemem(address)
			}
		}
	}

	return 0
}
