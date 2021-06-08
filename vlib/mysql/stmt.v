module mysql

struct C.MYSQL_STMT {}

struct C.MYSQL_BIND {
	buffer_type   int
	buffer        voidptr
	buffer_length &u32
	is_null       &bool
	length        &u32
}

struct StmtResultBuffer {
	buffer []C.MYSQL_BIND
}

enum FieldTypes {
	mysql_type_decimal
	mysql_type_tiny
	mysql_type_short
	mysql_type_long
	mysql_type_float
	mysql_type_double
	mysql_type_null
	mysql_type_timestamp
	mysql_type_longlong
	mysql_type_int24
	mysql_type_date
	mysql_type_time
	mysql_type_datetime
	mysql_type_year
	mysql_type_varchar
	mysql_type_bit
	mysql_type_timestamp2
	mysql_type_invalid = 243
	mysql_type_json = 245
	mysql_type_newdecimal = 246
	mysql_type_enum = 247
	mysql_type_set = 248
	mysql_type_tiny_blob = 249
	mysql_type_medium_blob = 250
	mysql_type_long_blob = 251
	mysql_type_blob = 252
	mysql_type_var_string = 253
	mysql_type_string = 254
	mysql_type_geometry = 255
}

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
	res := C.mysql_stmt_bind_param(stmt.stmt, stmt.binds.data)
	if !res && stmt.get_error_msg() != '' {
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
	res := C.mysql_stmt_bind_result(stmt.stmt, buffer.buffer.data)
	if !res && stmt.get_error_msg() != '' {
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

pub fn (mut stmt Stmt) bind_bool(b bool) {
	stmt.bind(.mysql_type_tiny, &b, 0, false, 0)
}

pub fn (mut stmt Stmt) bind_byte(b byte) {
	stmt.bind(.mysql_type_tiny, &b, 0, false, 0)
}

pub fn (mut stmt Stmt) bind_i8(b i8) {
	stmt.bind(.mysql_type_tiny, &b, 0, false, 0)
}

pub fn (mut stmt Stmt) bind_i16(b i16) {
	stmt.bind(.mysql_type_short, &b, 0, false, 0)
}

pub fn (mut stmt Stmt) bind_u16(b u16) {
	stmt.bind(.mysql_type_short, &b, 0, false, 0)
}

pub fn (mut stmt Stmt) bind_int(b int) {
	stmt.bind(.mysql_type_long, &b, 0, false, 0)
}

pub fn (mut stmt Stmt) bind_u32(b u32) {
	stmt.bind(.mysql_type_long, &b, 0, false, 0)
}

pub fn (mut stmt Stmt) bind_i64(b i64) {
	stmt.bind(.mysql_type_longlong, &b, 0, false, 0)
}

pub fn (mut stmt Stmt) bind_u64(b u64) {
	stmt.bind(.mysql_type_longlong, &b, 0, false, 0)
}

pub fn (mut stmt Stmt) bind_f32(b f32) {
	stmt.bind(.mysql_type_float, unsafe { *(&f32(&b)) }, 0, false, 0)
}

pub fn (mut stmt Stmt) bind_f64(b f64) {
	stmt.bind(.mysql_type_double, unsafe { *(&f64(&b)) }, 0, false, 0)
}

pub fn (mut stmt Stmt) bind_text(b string) {
	stmt.bind(.mysql_type_string, b.str, u32(b.len), false, 0)
}

pub fn (mut stmt Stmt) bind(typ FieldTypes, buffer voidptr, len u32, is_null bool, length u32) {
	stmt.binds << C.MYSQL_BIND{
		buffer_type: unsafe { int(typ) }
		buffer: buffer
		buffer_length: len
		is_null: unsafe { &is_null }
		length: unsafe { &length }
	}
}
