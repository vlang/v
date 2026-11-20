module duckdb

// The header is only read by the C compiler; V never parses it. It supplies
// authoritative prototypes and struct layouts for the declarations below.
#include "duckdb.h"

// DuckDB C API declarations. V never parses duckdb.h; the declarations below
// mirror DuckDB v2.0.0 exactly. `idx_t` is uint64_t. duckdb_result is passed by
// pointer, so its field order and sizes must match the C struct from duckdb.h.

struct C.duckdb_string {
	data &char
	size u64
}

struct C.duckdb_column {
	deprecated_data     voidptr
	deprecated_nullmask voidptr
	deprecated_type     int
	deprecated_name     &char
	internal_data       voidptr
}

struct C.duckdb_result {
	deprecated_column_count  u64
	deprecated_row_count     u64
	deprecated_rows_changed  u64
	deprecated_columns       &C.duckdb_column
	deprecated_error_message &char
	internal_data            voidptr
}

struct C.duckdb_database {
	internal_ptr voidptr
}

struct C.duckdb_connection {
	internal_ptr voidptr
}

struct C.duckdb_prepared_statement {
	internal_ptr voidptr
}

fn C.duckdb_open(&char, &&C.duckdb_database) int

fn C.duckdb_close(&&C.duckdb_database)

fn C.duckdb_connect(&C.duckdb_database, &&C.duckdb_connection) int

fn C.duckdb_disconnect(&&C.duckdb_connection)

fn C.duckdb_query(&C.duckdb_connection, &char, &C.duckdb_result) int

fn C.duckdb_destroy_result(&C.duckdb_result)

fn C.duckdb_column_name(&C.duckdb_result, u64) &char

fn C.duckdb_column_count(&C.duckdb_result) u64

fn C.duckdb_rows_changed(&C.duckdb_result) u64

fn C.duckdb_result_error(&C.duckdb_result) &char

fn C.duckdb_free(voidptr)

fn C.duckdb_row_count(&C.duckdb_result) u64

fn C.duckdb_value_string(&C.duckdb_result, u64, u64) C.duckdb_string

fn C.duckdb_value_is_null(&C.duckdb_result, u64, u64) u8

fn C.duckdb_prepare(&C.duckdb_connection, &char, &&C.duckdb_prepared_statement) int

fn C.duckdb_bind_null(&C.duckdb_prepared_statement, u64) int

fn C.duckdb_bind_varchar(&C.duckdb_prepared_statement, u64, &char) int

fn C.duckdb_bind_int64(&C.duckdb_prepared_statement, u64, i64) int

fn C.duckdb_bind_uint64(&C.duckdb_prepared_statement, u64, u64) int

fn C.duckdb_bind_double(&C.duckdb_prepared_statement, u64, f64) int

fn C.duckdb_bind_boolean(&C.duckdb_prepared_statement, u64, u8) int

fn C.duckdb_execute_prepared(&C.duckdb_prepared_statement, &C.duckdb_result) int

fn C.duckdb_destroy_prepare(&&C.duckdb_prepared_statement)

fn C.duckdb_prepare_error(&C.duckdb_prepared_statement) &char

pub const success = 0
pub const error_code = 1

struct SQLError {
	MessageError
	code int
}

// DB is a connection to a DuckDB database. Use `connect`/`open` to create one
// and `close` to release it. A DB value can be passed to V's `sql` statements
// because it implements `orm.Connection` and `orm.TransactionalConnection`.
@[heap]
pub struct DB {
	database &C.duckdb_database   = unsafe { nil }
	conn     &C.duckdb_connection = unsafe { nil }
pub mut:
	is_open bool
mut:
	// name of the sequence backing the most recent serial primary key insert
	last_seq string
}

// str returns a text representation of the DB handle.
pub fn (db &DB) str() string {
	return 'duckdb.DB{ conn: ${ptr_str(db.conn)}, is_open: ${db.is_open} }'
}

// Row holds one result row. Values are always stored as strings; `is_null`
// tracks which columns were SQL NULL, so callers can distinguish NULL from
// the empty string.
pub struct Row {
pub mut:
	vals    []string
	names   []string
	is_null []bool
}

// val returns the value at `index` as a string ('' for SQL NULL).
pub fn (row Row) val(index int) string {
	if index < 0 || index >= row.vals.len {
		return ''
	}
	return row.vals[index]
}

// values returns a copy of all row values.
pub fn (row Row) values() []string {
	return row.vals.clone()
}

// is_null reports whether the column at `index` was SQL NULL.
pub fn (row Row) is_null(index int) bool {
	return index >= 0 && index < row.is_null.len && row.is_null[index]
}

// get_string returns the value for the given column name, or '' if the column
// is not found or the value was NULL.
pub fn (r &Row) get_string(col_name string) string {
	for i, name in r.names {
		if name == col_name && i < r.vals.len {
			return r.vals[i]
		}
	}
	return ''
}

// get_int returns the integer value for the given column name, or 0 when the
// column is missing or not a number.
pub fn (r &Row) get_int(col_name string) int {
	return r.get_string(col_name).int()
}

fn cstring(ptr &char) string {
	if ptr == &char(unsafe { nil }) {
		return ''
	}
	return unsafe { cstring_to_vstring(ptr) }
}

fn cstring_len(ptr &char, len int) string {
	if ptr == &char(unsafe { nil }) || len <= 0 {
		return ''
	}
	// vstring_with_len reuses the C buffer, so clone it before the DuckDB
	// result (or the duckdb_free'd value buffer) goes away.
	return unsafe { ptr.vstring_with_len(len) }.clone()
}

// read_result copies a DuckDB result into V-owned Row values. The caller must
// destroy the DuckDB result afterwards.
fn read_result(res &C.duckdb_result) []Row {
	ncols := int(C.duckdb_column_count(res))
	nrows := int(C.duckdb_row_count(res))
	mut names := []string{cap: ncols}
	for c in 0 .. ncols {
		names << cstring(C.duckdb_column_name(res, u64(c)))
	}
	mut rows := []Row{cap: nrows}
	for r in 0 .. nrows {
		mut vals := []string{cap: ncols}
		mut nulls := []bool{cap: ncols}
		for c in 0 .. ncols {
			if C.duckdb_value_is_null(res, u64(c), u64(r)) != 0 {
				vals << ''
				nulls << true
				continue
			}
			s := C.duckdb_value_string(res, u64(c), u64(r))
			if s.data == &char(unsafe { nil }) {
				vals << ''
				nulls << true
			} else {
				vals << cstring_len(s.data, int(s.size))
				nulls << false
				C.duckdb_free(s.data)
			}
		}
		rows << Row{
			vals:    vals
			names:   names.clone()
			is_null: nulls
		}
	}
	return rows
}

fn result_error(res &C.duckdb_result, query string) SQLError {
	msg := cstring(C.duckdb_result_error(res))
	if msg == '' {
		return SQLError{
			msg:  'unknown DuckDB error: ${query}'
			code: error_code
		}
	}
	return SQLError{
		msg:  '${msg} (${query})'
		code: error_code
	}
}

// open opens the database at `path` (use ':memory:' for an in-memory database)
// and returns a connection to it.
pub fn open(path string) !DB {
	mut database := &C.duckdb_database(unsafe { nil })
	if C.duckdb_open(&char(path.str), &database) != success {
		return error('duckdb: failed to open ${path}')
	}
	mut conn := &C.duckdb_connection(unsafe { nil })
	if C.duckdb_connect(database, &conn) != success {
		C.duckdb_close(&database)
		return error('duckdb: failed to connect to ${path}')
	}
	return DB{
		database: database
		conn:     conn
		is_open:  true
	}
}

// connect is an alias for `open`.
pub fn connect(path string) !DB {
	return open(path)
}

// close disconnects and closes the database. It is safe to call more than once.
pub fn (mut db DB) close() ! {
	if !db.is_open {
		return
	}
	C.duckdb_disconnect(&db.conn)
	C.duckdb_close(&db.database)
	db.is_open = false
}

// exec runs a raw SQL statement and returns all result rows.
pub fn (db &DB) exec(query string) ![]Row {
	$if trace_duckdb ? {
		eprintln('> duckdb.exec: ${query}')
	}
	if !db.is_open {
		return error('duckdb: database is closed')
	}
	mut res := C.duckdb_result{}
	if C.duckdb_query(db.conn, &char(query.str), &res) != success {
		e := result_error(&res, query)
		C.duckdb_destroy_result(&res)
		return e
	}
	defer {
		C.duckdb_destroy_result(&res)
	}
	return read_result(&res)
}

// exec_one runs a raw SQL statement and returns only its first row.
pub fn (db &DB) exec_one(query string) !Row {
	rows := db.exec(query)!
	if rows.len == 0 {
		return error('duckdb: query returned no rows: ${query}')
	}
	return rows[0]
}

// exec_map runs a raw SQL statement and returns rows as column-name maps.
pub fn (db &DB) exec_map(query string) ![]map[string]string {
	rows := db.exec(query)!
	mut out := []map[string]string{cap: rows.len}
	for row in rows {
		mut m := map[string]string{}
		for i, name in row.names {
			if i < row.vals.len {
				m[name] = row.vals[i]
			}
		}
		out << m
	}
	return out
}

// rows_changed returns the number of rows modified by the last statement.
pub fn (db &DB) rows_changed(query string) !u64 {
	if !db.is_open {
		return error('duckdb: database is closed')
	}
	mut res := C.duckdb_result{}
	if C.duckdb_query(db.conn, &char(query.str), &res) != success {
		e := result_error(&res, query)
		C.duckdb_destroy_result(&res)
		return e
	}
	defer {
		C.duckdb_destroy_result(&res)
	}
	return C.duckdb_rows_changed(&res)
}

// q_int runs `query` and returns the first column of the first row as an int.
pub fn (db &DB) q_int(query string) !int {
	rows := db.exec(query)!
	if rows.len == 0 || rows[0].vals.len == 0 || rows[0].is_null(0) {
		return error('duckdb: query returned no value: ${query}')
	}
	return rows[0].vals[0].int()
}

// q_string runs `query` and returns the first column of the first row as a
// string. SQL NULL is returned as an empty string.
pub fn (db &DB) q_string(query string) !string {
	rows := db.exec(query)!
	if rows.len == 0 || rows[0].vals.len == 0 {
		return error('duckdb: query returned no value: ${query}')
	}
	return rows[0].vals[0]
}

// prepare creates a prepared statement. The caller must call `finalize`.
pub fn (db &DB) prepare(query string) !Stmt {
	mut handle := &C.duckdb_prepared_statement(unsafe { nil })
	if C.duckdb_prepare(db.conn, &char(query.str), &handle) != success {
		mut msg := cstring(C.duckdb_prepare_error(handle))
		if msg == '' {
			msg = 'unknown prepare error'
		}
		return error('duckdb: prepare failed: ${msg} (${query})')
	}
	return Stmt{
		stmt: handle
	}
}

// Stmt wraps a DuckDB prepared statement. It is used internally by the ORM
// to bind parameters safely, and is exposed for callers that need prepared
// statements directly.
@[heap]
pub struct Stmt {
	stmt &C.duckdb_prepared_statement = unsafe { nil }
}

// finalize releases the prepared statement.
pub fn (stmt &Stmt) finalize() {
	handle := stmt.stmt
	C.duckdb_destroy_prepare(&handle)
}

// bind_null binds SQL NULL at the 1-based parameter index.
pub fn (stmt &Stmt) bind_null(idx int) int {
	return int(C.duckdb_bind_null(stmt.stmt, u64(idx)))
}

// bind_text binds a string value at the 1-based parameter index.
pub fn (stmt &Stmt) bind_text(idx int, s string) int {
	return int(C.duckdb_bind_varchar(stmt.stmt, u64(idx), &char(s.str)))
}

// bind_i64 binds a 64-bit integer at the 1-based parameter index.
pub fn (stmt &Stmt) bind_i64(idx int, v i64) int {
	return int(C.duckdb_bind_int64(stmt.stmt, u64(idx), v))
}

// bind_u64 binds an unsigned 64-bit integer at the 1-based parameter index.
pub fn (stmt &Stmt) bind_u64(idx int, v u64) int {
	return int(C.duckdb_bind_uint64(stmt.stmt, u64(idx), v))
}

// bind_f64 binds a double at the 1-based parameter index.
pub fn (stmt &Stmt) bind_f64(idx int, v f64) int {
	return int(C.duckdb_bind_double(stmt.stmt, u64(idx), v))
}

// bind_bool binds a boolean at the 1-based parameter index.
pub fn (stmt &Stmt) bind_bool(idx int, v bool) int {
	return int(C.duckdb_bind_boolean(stmt.stmt, u64(idx), u8(v)))
}

// exec executes the prepared statement and returns the result rows.
pub fn (stmt &Stmt) exec(query string) ![]Row {
	mut res := C.duckdb_result{}
	if C.duckdb_execute_prepared(stmt.stmt, &res) != success {
		e := result_error(&res, query)
		C.duckdb_destroy_result(&res)
		return e
	}
	defer {
		C.duckdb_destroy_result(&res)
	}
	return read_result(&res)
}

// exec_param runs `query` with the given string parameters bound as VARCHAR.
pub fn (db &DB) exec_param(query string, params []string) ![]Row {
	stmt := db.prepare(query)!
	defer {
		stmt.finalize()
	}
	for i, param in params {
		if stmt.bind_text(i + 1, param) != success {
			return error('duckdb: failed to bind parameter ${i + 1}')
		}
	}
	return stmt.exec(query)
}
