module mysql

// StreamResult reads an unbuffered query result in batches. It keeps the
// connection exclusively locked until the result is exhausted or closed.
// A StreamResult must be consumed and closed on the thread that created it.
pub struct StreamResult {
mut:
	result &C.MYSQL_RES = unsafe { nil }
	guard  MySQLConnectionGuard
	fields []Field
	closed bool
}

// query_stream executes `query` and returns an unbuffered result. Unlike
// `query`, rows are read directly from the server instead of being buffered by
// the client. The caller must close the result if it is not read to exhaustion.
pub fn (db &DB) query_stream(query string) !StreamResult {
	mut guard := db.acquire_connection_guard()!
	if C.mysql_real_query(guard.conn, query.str, query.len) != 0 {
		err := error_with_code(get_error_msg(guard.conn), get_errno(guard.conn))
		guard.release()
		return err
	}
	result := C.mysql_use_result(guard.conn)
	if result == unsafe { nil } && get_errno(guard.conn) != 0 {
		err := error_with_code(get_error_msg(guard.conn), get_errno(guard.conn))
		guard.release()
		return err
	}
	return StreamResult{
		result: result
		guard:  guard
		fields: fields_from_result(result)
	}
}

// fields returns metadata for the streamed result columns.
pub fn (r &StreamResult) fields() []Field {
	return r.fields.clone()
}

// next_batch reads at most `size` rows from the server. An empty batch means
// that the result is exhausted. Exhausting the result closes it automatically.
pub fn (mut r StreamResult) next_batch(size int) ![]NullableRow {
	if size <= 0 {
		return error('db.mysql: stream batch size must be greater than zero')
	}
	if r.closed || r.result == unsafe { nil } {
		r.close()
		return []NullableRow{}
	}
	mut rows := []NullableRow{cap: size}
	for rows.len < size {
		row_data := C.mysql_fetch_row(r.result)
		if row_data == unsafe { nil } {
			if get_errno(r.guard.conn) != 0 {
				err := error_with_code(get_error_msg(r.guard.conn), get_errno(r.guard.conn))
				r.close()
				return err
			}
			r.close()
			break
		}
		mut row := NullableRow{
			vals: []?string{cap: r.fields.len}
		}
		for i in 0 .. r.fields.len {
			if unsafe { row_data[i] == nil } {
				row.vals << none
			} else {
				length := int(C.v_mysql_fetch_column_length(r.result, u32(i)))
				value := unsafe { (&u8(row_data[i])).vstring_with_len(length).clone() }
				row.vals << value
			}
		}
		rows << row
	}
	return rows
}

// close frees an unbuffered result and releases its connection lock. It is
// safe to call close more than once.
pub fn (mut r StreamResult) close() {
	if r.closed {
		return
	}
	if r.result != unsafe { nil } {
		C.mysql_free_result(r.result)
		r.result = unsafe { nil }
	}
	r.guard.release()
	r.closed = true
}

// StreamStmt is a one-shot prepared statement whose result is fetched in
// batches. It keeps the connection exclusively locked until exhausted or
// closed, and must be consumed and closed on the thread that created it.
pub struct StreamStmt {
mut:
	stmt     &C.MYSQL_STMT = unsafe { nil }
	guard    MySQLConnectionGuard
	fields   []Field
	lengths  voidptr
	is_null  []bool
	binds    []C.MYSQL_BIND
	executed bool
	closed   bool
}

// prepare_stream prepares a one-shot statement for streaming. Call execute,
// read rows with next_batch, and close it if it is not read to exhaustion.
pub fn (db &DB) prepare_stream(query string) !StreamStmt {
	mut guard := db.acquire_connection_guard()!
	stmt := C.mysql_stmt_init(guard.conn)
	if stmt == unsafe { nil } {
		err := error_with_code(get_error_msg(guard.conn), get_errno(guard.conn))
		guard.release()
		return err
	}
	if C.mysql_stmt_prepare(stmt, query.str, query.len) != 0 {
		err := error_with_code(get_stmt_error_msg(stmt), get_stmt_errno(stmt))
		C.mysql_stmt_close(stmt)
		guard.release()
		return err
	}
	return StreamStmt{
		stmt:  stmt
		guard: guard
	}
}

// execute binds string parameters and starts the unbuffered statement result.
pub fn (mut stmt StreamStmt) execute(params []string) ! {
	if stmt.closed {
		return error('db.mysql: cannot execute a closed stream statement')
	}
	if stmt.executed {
		return error('db.mysql: a stream statement can only be executed once')
	}
	mut params_bind := []C.MYSQL_BIND{cap: params.len}
	for param in params {
		params_bind << C.MYSQL_BIND{
			buffer_type:   mysql_type_string
			buffer:        param.str
			buffer_length: u32(param.len)
		}
	}
	if params_bind.len > 0
		&& C.mysql_stmt_bind_param(stmt.stmt, unsafe { &C.MYSQL_BIND(params_bind.data) }) {
		err := error_with_code(get_stmt_error_msg(stmt.stmt), get_stmt_errno(stmt.stmt))
		stmt.close()
		return err
	}
	if C.mysql_stmt_execute(stmt.stmt) != 0 {
		err := error_with_code(get_stmt_error_msg(stmt.stmt), get_stmt_errno(stmt.stmt))
		stmt.close()
		return err
	}
	stmt.executed = true
	metadata_result := C.mysql_stmt_result_metadata(stmt.stmt)
	if metadata_result == unsafe { nil } {
		stmt.close()
		return
	}
	stmt.fields = fields_from_result(metadata_result)
	C.mysql_free_result(metadata_result)
	num_fields := stmt.fields.len
	stmt.lengths = C.v_mysql_lengths_new(u32(num_fields))
	if stmt.lengths == unsafe { nil } {
		stmt.close()
		return error('db.mysql: failed to allocate stream result lengths')
	}
	stmt.is_null = []bool{len: num_fields}
	stmt.binds = []C.MYSQL_BIND{cap: num_fields}
	for i in 0 .. num_fields {
		stmt.binds << C.MYSQL_BIND{
			buffer_type: mysql_type_string
			is_null:     unsafe { &stmt.is_null[i] }
		}
	}
	for i in 0 .. num_fields {
		C.v_mysql_bind_set_length_at(unsafe { &stmt.binds[i] }, stmt.lengths, u32(i))
	}
	if C.mysql_stmt_bind_result(stmt.stmt, unsafe { &C.MYSQL_BIND(stmt.binds.data) }) {
		err := error_with_code(get_stmt_error_msg(stmt.stmt), get_stmt_errno(stmt.stmt))
		stmt.close()
		return err
	}
}

// fields returns metadata for the prepared statement result columns.
pub fn (stmt &StreamStmt) fields() []Field {
	return stmt.fields.clone()
}

// next_batch reads at most `size` rows from the prepared statement result. An
// empty batch means that the result is exhausted and has been closed.
pub fn (mut stmt StreamStmt) next_batch(size int) ![]NullableRow {
	if size <= 0 {
		return error('db.mysql: stream batch size must be greater than zero')
	}
	if !stmt.executed {
		return error('db.mysql: execute the stream statement before fetching rows')
	}
	if stmt.closed {
		return []NullableRow{}
	}
	mut rows := []NullableRow{cap: size}
	for rows.len < size {
		code := C.mysql_stmt_fetch(stmt.stmt)
		if code == mysql_no_data {
			stmt.close()
			break
		}
		if code !in [0, mysql_data_truncated] {
			err := error_with_code(get_stmt_error_msg(stmt.stmt), get_stmt_errno(stmt.stmt))
			stmt.close()
			return err
		}
		mut row := NullableRow{
			vals: []?string{cap: stmt.fields.len}
		}
		for i in 0 .. stmt.fields.len {
			if stmt.is_null[i] {
				row.vals << none
				continue
			}
			length := int(C.v_mysql_length_at(stmt.lengths, u32(i)))
			if length == 0 {
				row.vals << ''
				continue
			}
			mut data := unsafe { malloc(length + 1) }
			mut column_bind := C.MYSQL_BIND{
				buffer_type:   mysql_type_string
				buffer:        data
				buffer_length: u32(length)
				is_null:       unsafe { &stmt.is_null[i] }
			}
			C.v_mysql_bind_set_length_at(&column_bind, stmt.lengths, u32(i))
			column_code := C.mysql_stmt_fetch_column(stmt.stmt, &column_bind, i, 0)
			if column_code !in [0, mysql_data_truncated] {
				unsafe { free(data) }
				err := error_with_code(get_stmt_error_msg(stmt.stmt), get_stmt_errno(stmt.stmt))
				stmt.close()
				return err
			}
			value := unsafe { (&u8(data)).vstring_with_len(length).clone() }
			unsafe { free(data) }
			row.vals << value
		}
		rows << row
	}
	return rows
}

// close frees pending statement results, closes the statement, and releases
// its connection lock. It is safe to call close more than once.
pub fn (mut stmt StreamStmt) close() {
	if stmt.closed {
		return
	}
	if stmt.stmt != unsafe { nil } {
		C.mysql_stmt_free_result(stmt.stmt)
		C.mysql_stmt_close(stmt.stmt)
		stmt.stmt = unsafe { nil }
	}
	if stmt.lengths != unsafe { nil } {
		unsafe { free(stmt.lengths) }
		stmt.lengths = unsafe { nil }
	}
	stmt.guard.release()
	stmt.closed = true
}
