module mysql

@[typedef]
pub struct C.MYSQL_STMT {
	mysql   &C.MYSQL
	stmt_id u32
}

@[typedef]
pub struct C.MYSQL_BIND {
mut:
	buffer_type   int
	buffer        voidptr
	buffer_length u32
	length        &u32
	is_null       &bool
}

const mysql_type_decimal = C.MYSQL_TYPE_DECIMAL
const mysql_type_tiny = C.MYSQL_TYPE_TINY
const mysql_type_short = C.MYSQL_TYPE_SHORT
const mysql_type_long = C.MYSQL_TYPE_LONG
const mysql_type_float = C.MYSQL_TYPE_FLOAT
const mysql_type_double = C.MYSQL_TYPE_DOUBLE
const mysql_type_null = C.MYSQL_TYPE_NULL
const mysql_type_timestamp = C.MYSQL_TYPE_TIMESTAMP
const mysql_type_longlong = C.MYSQL_TYPE_LONGLONG
const mysql_type_int24 = C.MYSQL_TYPE_INT24
const mysql_type_date = C.MYSQL_TYPE_DATE
const mysql_type_time = C.MYSQL_TYPE_TIME
const mysql_type_datetime = C.MYSQL_TYPE_DATETIME
const mysql_type_year = C.MYSQL_TYPE_YEAR
const mysql_type_varchar = C.MYSQL_TYPE_VARCHAR
const mysql_type_bit = C.MYSQL_TYPE_BIT
const mysql_type_timestamp22 = C.MYSQL_TYPE_TIMESTAMP
const mysql_type_json = C.MYSQL_TYPE_JSON
const mysql_type_newdecimal = C.MYSQL_TYPE_NEWDECIMAL
const mysql_type_enum = C.MYSQL_TYPE_ENUM
const mysql_type_set = C.MYSQL_TYPE_SET
const mysql_type_tiny_blob = C.MYSQL_TYPE_TINY_BLOB
const mysql_type_medium_blob = C.MYSQL_TYPE_MEDIUM_BLOB
const mysql_type_long_blob = C.MYSQL_TYPE_LONG_BLOB
const mysql_type_blob = C.MYSQL_TYPE_BLOB
const mysql_type_var_string = C.MYSQL_TYPE_VAR_STRING
const mysql_type_string = C.MYSQL_TYPE_STRING
const mysql_type_geometry = C.MYSQL_TYPE_GEOMETRY
const mysql_no_data = C.MYSQL_NO_DATA

fn C.mysql_stmt_init(&C.MYSQL) &C.MYSQL_STMT
fn C.mysql_stmt_prepare(&C.MYSQL_STMT, const_query charptr, u32) int
fn C.mysql_stmt_bind_param(&C.MYSQL_STMT, &C.MYSQL_BIND) bool
fn C.mysql_stmt_execute(&C.MYSQL_STMT) int
fn C.mysql_stmt_close(&C.MYSQL_STMT) bool
fn C.mysql_stmt_free_result(&C.MYSQL_STMT) bool
fn C.mysql_stmt_error(&C.MYSQL_STMT) &char
fn C.mysql_stmt_result_metadata(&C.MYSQL_STMT) &C.MYSQL_RES

fn C.mysql_stmt_field_count(&C.MYSQL_STMT) u16
fn C.mysql_stmt_bind_result(&C.MYSQL_STMT, &C.MYSQL_BIND) bool
fn C.mysql_stmt_fetch(&C.MYSQL_STMT) int
fn C.mysql_stmt_next_result(&C.MYSQL_STMT) int
fn C.mysql_stmt_store_result(&C.MYSQL_STMT) int
fn C.mysql_stmt_fetch_column(&C.MYSQL_STMT, &C.MYSQL_BIND, u32, u64) int

pub struct Stmt {
	stmt  &C.MYSQL_STMT = &C.MYSQL_STMT(unsafe { nil })
	query string
mut:
	binds []C.MYSQL_BIND
	res   []C.MYSQL_BIND
}

// str returns a text representation of the given mysql statement `s`.
pub fn (s &Stmt) str() string {
	return 'mysql.Stmt{ stmt: ${voidptr(s.stmt):x}, query: `${s.query}`, binds.len: ${s.binds.len}, res.len: ${s.res.len} }'
}

// init_stmt creates a new statement, given the `query`.
pub fn (db DB) init_stmt(query string) Stmt {
	return Stmt{
		stmt:  C.mysql_stmt_init(db.conn)
		query: query
		binds: []C.MYSQL_BIND{}
	}
}

// prepare a statement for execution.
pub fn (stmt Stmt) prepare() ! {
	result := C.mysql_stmt_prepare(stmt.stmt, charptr(stmt.query.str), stmt.query.len)

	if result != 0 && stmt.get_error_msg() != '' {
		return stmt.error(result)
	}
}

// bind_params binds all the parameters in `stmt`.
pub fn (stmt Stmt) bind_params() ! {
	result := C.mysql_stmt_bind_param(stmt.stmt, unsafe { &C.MYSQL_BIND(stmt.binds.data) })

	if result && stmt.get_error_msg() != '' {
		return stmt.error(1)
	}
}

// execute executes the given `stmt` and waits for the result.
pub fn (stmt Stmt) execute() !int {
	result := C.mysql_stmt_execute(stmt.stmt)

	if result != 0 && stmt.get_error_msg() != '' {
		return stmt.error(result)
	}

	return result
}

// next retrieves the next available result from the execution of `stmt`
pub fn (stmt Stmt) next() !int {
	result := C.mysql_stmt_next_result(stmt.stmt)

	if result != 0 && stmt.get_error_msg() != '' {
		return stmt.error(result)
	}

	return result
}

// gen_metadata executes mysql_stmt_result_metadata over the given `stmt`
// It requires that the statement has produced a result set, since the metadata will be for that result set.
// See https://dev.mysql.com/doc/c-api/5.7/en/mysql-stmt-result-metadata.html
pub fn (stmt Stmt) gen_metadata() &C.MYSQL_RES {
	return C.mysql_stmt_result_metadata(stmt.stmt)
}

// fetch_fields retrieves the fields from the metadata result of the execution of `stmt`.
// See https://dev.mysql.com/doc/c-api/5.7/en/mysql-fetch-fields.html
// See also Result.n_fields for the size of the returned C array.
pub fn (stmt Stmt) fetch_fields(res &C.MYSQL_RES) &C.MYSQL_FIELD {
	return C.mysql_fetch_fields(res)
}

// fetch_stmt fetches the next row in the result set. It returns the status of the execution of mysql_stmt_fetch .
// See https://dev.mysql.com/doc/c-api/5.7/en/mysql-stmt-fetch.html
pub fn (stmt Stmt) fetch_stmt() !int {
	result := C.mysql_stmt_fetch(stmt.stmt)

	if result !in [0, 100] && stmt.get_error_msg() != '' {
		return stmt.error(result)
	}

	return result
}

// close disposes the prepared `stmt`. The statement becomes invalid, and should not be used anymore after this call.
// If the current statement has pending or unread results, this method cancels them too.
// See https://dev.mysql.com/doc/c-api/5.7/en/mysql-stmt-close.html
pub fn (stmt Stmt) close() ! {
	if !C.mysql_stmt_close(stmt.stmt) && stmt.get_error_msg() != '' {
		return stmt.error(1)
	}

	if !C.mysql_stmt_free_result(stmt.stmt) && stmt.get_error_msg() != '' {
		return stmt.error(1)
	}
}

fn (stmt Stmt) get_error_msg() string {
	return unsafe { cstring_to_vstring(&char(C.mysql_stmt_error(stmt.stmt))) }
}

// error returns a proper V error with a human readable description, given the error code returned by MySQL
pub fn (stmt Stmt) error(code int) IError {
	msg := stmt.get_error_msg()

	return &SQLError{
		msg:  '${msg} (${code}) (${stmt.query})'
		code: code
	}
}

fn (stmt Stmt) get_field_count() u16 {
	return C.mysql_stmt_field_count(stmt.stmt)
}

// bind_bool binds a single boolean value to the statement `stmt`
pub fn (mut stmt Stmt) bind_bool(b &bool) {
	stmt.bind(mysql_type_tiny, b, 0)
}

// bind_byte binds a single byte value to the statement `stmt`
pub fn (mut stmt Stmt) bind_byte(b &u8) {
	stmt.bind(mysql_type_tiny, b, 0)
}

// bind_u8 binds a single u8 value to the statement `stmt`
pub fn (mut stmt Stmt) bind_u8(b &u8) {
	stmt.bind(mysql_type_tiny, b, 0)
}

// bind_i8 binds a single i8 value to the statement `stmt`
pub fn (mut stmt Stmt) bind_i8(b &i8) {
	stmt.bind(mysql_type_tiny, b, 0)
}

// bind_i16 binds a single i16 value to the statement `stmt`
pub fn (mut stmt Stmt) bind_i16(b &i16) {
	stmt.bind(mysql_type_short, b, 0)
}

// bind_u16 binds a single u16 value to the statement `stmt`
pub fn (mut stmt Stmt) bind_u16(b &u16) {
	stmt.bind(mysql_type_short, b, 0)
}

// bind_int binds a single int value to the statement `stmt`
pub fn (mut stmt Stmt) bind_int(b &int) {
	stmt.bind(mysql_type_long, b, 0)
}

// bind_u32 binds a single u32 value to the statement `stmt`
pub fn (mut stmt Stmt) bind_u32(b &u32) {
	stmt.bind(mysql_type_long, b, 0)
}

// bind_i64 binds a single i64 value to the statement `stmt`
pub fn (mut stmt Stmt) bind_i64(b &i64) {
	stmt.bind(mysql_type_longlong, b, 0)
}

// bind_u64 binds a single u64 value to the statement `stmt`
pub fn (mut stmt Stmt) bind_u64(b &u64) {
	stmt.bind(mysql_type_longlong, b, 0)
}

// bind_f32 binds a single f32 value to the statement `stmt`
pub fn (mut stmt Stmt) bind_f32(b &f32) {
	stmt.bind(mysql_type_float, b, 0)
}

// bind_f64 binds a single f64 value to the statement `stmt`
pub fn (mut stmt Stmt) bind_f64(b &f64) {
	stmt.bind(mysql_type_double, b, 0)
}

// bind_text binds a single string value to the statement `stmt`
pub fn (mut stmt Stmt) bind_text(b string) {
	stmt.bind(mysql_type_string, b.str, u32(b.len))
}

// bind_null binds a single NULL value to the statement `stmt`
pub fn (mut stmt Stmt) bind_null() {
	stmt.binds << C.MYSQL_BIND{
		buffer_type: mysql_type_null
		length:      0
		is_null:     0
	}
}

// bind binds a single value pointed by `buffer`, to the statement `stmt`. The buffer length must be passed as well in `buf_len`.
// Note: it is more convenient to use one of the other bind_XYZ methods.
pub fn (mut stmt Stmt) bind(typ int, buffer voidptr, buf_len u32) {
	stmt.binds << C.MYSQL_BIND{
		buffer_type:   typ
		buffer:        buffer
		buffer_length: buf_len
		length:        0
		is_null:       0
	}
}

// bind_res will store one result in the statement `stmt`
pub fn (mut stmt Stmt) bind_res(fields &C.MYSQL_FIELD, dataptr []&u8, lengths []u32, is_null []bool, num_fields int) {
	for i in 0 .. num_fields {
		stmt.res << C.MYSQL_BIND{
			buffer_type: unsafe { fields[i].type }
			buffer:      dataptr[i]
			length:      &lengths[i]
			is_null:     &is_null[i]
		}
	}
}

// bind_result_buffer binds one result value, by calling mysql_stmt_bind_result .
// See https://dev.mysql.com/doc/c-api/8.0/en/mysql-stmt-bind-result.html
pub fn (mut stmt Stmt) bind_result_buffer() ! {
	result := C.mysql_stmt_bind_result(stmt.stmt, unsafe { &C.MYSQL_BIND(stmt.res.data) })

	if result && stmt.get_error_msg() != '' {
		return stmt.error(1)
	}
}

// store_result will *buffer the complete result set* from the execution of `stmt` *on the client side*.
// Note: result sets are produced by calling mysql_stmt_execute() to executed prepared statements for SQL
// statements such as SELECT, SHOW, DESCRIBE, and EXPLAIN.
// By default, result sets for successfully executed prepared statements are *not buffered on the client*,
// and mysql_stmt_fetch() fetches them one at a time from the server.
// Note 2: call store_result, *after* binding data buffers with bind_result_buffer,
// and *before* calling fetch_stmt to fetch rows.
// See https://dev.mysql.com/doc/c-api/8.0/en/mysql-stmt-store-result.html
pub fn (mut stmt Stmt) store_result() ! {
	result := C.mysql_stmt_store_result(stmt.stmt)

	if result != 0 && stmt.get_error_msg() != '' {
		return stmt.error(result)
	}
}

// fetch_column fetches one column from the current result set row.
// `bind` provides the buffer where data should be placed.
// It should be set up the same way as for `mysql_stmt_bind_result()`.
// `column` indicates which column to fetch. The first column is numbered 0.
pub fn (mut stmt Stmt) fetch_column(bind &C.MYSQL_BIND, column int) ! {
	result := C.mysql_stmt_fetch_column(stmt.stmt, bind, column, 0)

	if result != 0 && stmt.get_error_msg() != '' {
		return stmt.error(result)
	}
}
