module mysql

struct C.MYSQL_STMT {}

struct C.MYSQL_BIND {
	buffer_type   int
	buffer        voidptr
	buffer_length u32
}

struct StmtResultBuffer {
	buffer []C.MYSQL_BIND
}

const (
	mysql_type_decimal     = C.MYSQL_TYPE_DECIMAL
	mysql_type_tiny        = C.MYSQL_TYPE_TINY
	mysql_type_short       = C.MYSQL_TYPE_SHORT
	mysql_type_long        = C.MYSQL_TYPE_LONG
	mysql_type_float       = C.MYSQL_TYPE_FLOAT
	mysql_type_double      = C.MYSQL_TYPE_DOUBLE
	mysql_type_null        = C.MYSQL_TYPE_NULL
	mysql_type_timestamp   = C.MYSQL_TYPE_TIMESTAMP
	mysql_type_longlong    = C.MYSQL_TYPE_LONGLONG
	mysql_type_int24       = C.MYSQL_TYPE_INT24
	mysql_type_date        = C.MYSQL_TYPE_DATE
	mysql_type_time        = C.MYSQL_TYPE_TIME
	mysql_type_datetime    = C.MYSQL_TYPE_DATETIME
	mysql_type_year        = C.MYSQL_TYPE_YEAR
	mysql_type_varchar     = C.MYSQL_TYPE_VARCHAR
	mysql_type_bit         = C.MYSQL_TYPE_BIT
	mysql_type_timestamp22 = C.MYSQL_TYPE_TIMESTAMP
	mysql_type_invalid     = C.MYSQL_TYPE_INVALID
	mysql_type_json        = C.MYSQL_TYPE_JSON
	mysql_type_newdecimal  = C.MYSQL_TYPE_NEWDECIMAL
	mysql_type_enum        = C.MYSQL_TYPE_ENUM
	mysql_type_set         = C.MYSQL_TYPE_SET
	mysql_type_tiny_blob   = C.MYSQL_TYPE_TINY_BLOB
	mysql_type_medium_blob = C.MYSQL_TYPE_MEDIUM_BLOB
	mysql_type_long_blob   = C.MYSQL_TYPE_LONG_BLOB
	mysql_type_blob        = C.MYSQL_TYPE_BLOB
	mysql_type_var_string  = C.MYSQL_TYPE_VAR_STRING
	mysql_type_string      = C.MYSQL_TYPE_STRING
	mysql_type_geometry    = C.MYSQL_TYPE_GEOMETRY
)

fn C.mysql_stmt_init(&C.MYSQL) &C.MYSQL_STMT
fn C.mysql_stmt_prepare(&C.MYSQL_STMT, &char, u32) int
fn C.mysql_stmt_bind_param(&C.MYSQL_STMT, &C.MYSQL_BIND) bool
fn C.mysql_stmt_execute(&C.MYSQL_STMT) int
fn C.mysql_stmt_close(&C.MYSQL_STMT) bool
fn C.mysql_stmt_free_result(&C.MYSQL_STMT) bool
fn C.mysql_stmt_error(&C.MYSQL_STMT) &char

fn C.mysql_stmt_field_count(&C.MYSQL_STMT) u16
fn C.mysql_stmt_bind_result(&C.MYSQL_STMT, &C.MYSQL_BIND) bool

struct Stmt {
	stmt  &C.MYSQL_STMT = &C.MYSQL_STMT(0)
	query string
mut:
	binds []C.MYSQL_BIND
}

pub fn (db Connection) init_stmt(query string) Stmt {
	return Stmt{
		stmt: C.mysql_stmt_init(db.conn)
		query: query
		binds: []C.MYSQL_BIND{}
	}
}

pub fn (stmt Stmt) prepare() ? {
	res := C.mysql_stmt_prepare(stmt.stmt, stmt.query.str, stmt.query.len)
	if res != 0 && stmt.get_error_msg() != '' {
		return stmt.error(res)
	}
}

pub fn (stmt Stmt) bind_params() ? {
	eprintln(stmt.binds)
	res := C.mysql_stmt_bind_param(stmt.stmt, &C.MYSQL_BIND(stmt.binds.data))
	if res && stmt.get_error_msg() != '' {
		return stmt.error(1)
	}
}

pub fn (stmt Stmt) execute() ? {
	res := C.mysql_stmt_execute(stmt.stmt)
	if res != 0 && stmt.get_error_msg() != '' {
		return stmt.error(res)
	}
}

pub fn (stmt Stmt) bind_result_buffer(buffer StmtResultBuffer) ? {
	eprintln(stmt.binds)
	res := C.mysql_stmt_bind_result(stmt.stmt, &C.MYSQL_BIND(buffer.buffer.data))
	if res && stmt.get_error_msg() != '' {
		return stmt.error(1)
	}
}

pub fn (stmt Stmt) gen_result_buffer() StmtResultBuffer {
	return StmtResultBuffer{
		buffer: []C.MYSQL_BIND{len: int(stmt.get_field_count())}
	}
}

pub fn (stmt Stmt) close() ? {
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

pub fn (stmt Stmt) error(code int) IError {
	msg := stmt.get_error_msg()
	return IError(&SQLError{
		msg: '$msg ($code) ($stmt.query)'
		code: code
	})
}

fn (stmt Stmt) get_field_count() u16 {
	return C.mysql_stmt_field_count(stmt.stmt)
}

pub fn (mut stmt Stmt) bind_bool(b &bool) {
	stmt.bind(mysql.mysql_type_tiny, b, 0)
}

pub fn (mut stmt Stmt) bind_byte(b &byte) {
	stmt.bind(mysql.mysql_type_tiny, b, 0)
}

pub fn (mut stmt Stmt) bind_i8(b &i8) {
	stmt.bind(mysql.mysql_type_tiny, b, 0)
}

pub fn (mut stmt Stmt) bind_i16(b &i16) {
	stmt.bind(mysql.mysql_type_short, b, 0)
}

pub fn (mut stmt Stmt) bind_u16(b &u16) {
	stmt.bind(mysql.mysql_type_short, b, 0)
}

pub fn (mut stmt Stmt) bind_int(b &int) {
	stmt.bind(mysql.mysql_type_long, b, 0)
}

pub fn (mut stmt Stmt) bind_u32(b &u32) {
	stmt.bind(mysql.mysql_type_long, b, 0)
}

pub fn (mut stmt Stmt) bind_i64(b &i64) {
	stmt.bind(mysql.mysql_type_longlong, b, 0)
}

pub fn (mut stmt Stmt) bind_u64(b &u64) {
	stmt.bind(mysql.mysql_type_longlong, b, 0)
}

pub fn (mut stmt Stmt) bind_f32(b &f32) {
	stmt.bind(mysql.mysql_type_float, b, 0)
}

pub fn (mut stmt Stmt) bind_f64(b &f64) {
	stmt.bind(mysql.mysql_type_double, b, 0)
}

pub fn (mut stmt Stmt) bind_text(b string) {
	stmt.bind(mysql.mysql_type_string, b.str, u32(b.len))
}

pub fn (mut stmt Stmt) bind(typ int, buffer voidptr, buf_len u32) {
	stmt.binds << C.MYSQL_BIND{
		buffer_type: typ
		buffer: buffer
		buffer_length: buf_len
	}
}
