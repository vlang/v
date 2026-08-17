module mysql

// StreamResult reads an unbuffered query result in batches. It keeps the
// connection exclusively locked until the result is exhausted or closed.
// A StreamResult must be consumed and closed on the thread that created it.
pub struct StreamResult {
mut:
	state &StreamResultState = unsafe { nil }
}

@[heap]
struct StreamResultState {
mut:
	result &C.MYSQL_RES = unsafe { nil }
	guard  MySQLConnectionGuard
	fields []Field
	closed bool
}

fn stream_result_from_guard(mut guard MySQLConnectionGuard) !StreamResult {
	result := C.mysql_use_result(guard.conn)
	if result == unsafe { nil } && get_errno(guard.conn) != 0 {
		err := error_with_code(get_error_msg(guard.conn), get_errno(guard.conn))
		guard.release()
		return err
	}
	return StreamResult{
		state: &StreamResultState{
			result: result
			guard:  guard
			fields: fields_from_result(result)
		}
	}
}

fn drain_remaining_stream_results(conn &C.MYSQL) {
	if conn == unsafe { nil } {
		return
	}
	for C.mysql_more_results(conn) {
		if C.mysql_next_result(conn) != 0 {
			break
		}
		result := C.mysql_use_result(conn)
		if result != unsafe { nil } {
			C.mysql_free_result(result)
		}
	}
}

fn checked_stream_value_length(length u64) !int {
	if length > u64(max_int) {
		return error('db.mysql: streamed column length ${length} exceeds the supported maximum ${max_int}')
	}
	return int(length)
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
	return stream_result_from_guard(mut guard)
}

// fields returns metadata for the streamed result columns.
pub fn (r &StreamResult) fields() []Field {
	if r.state == unsafe { nil } {
		return []Field{}
	}
	return r.state.fields.clone()
}

// next_batch reads at most `size` rows from the server. An empty batch means
// that the result is exhausted. Exhausting the result closes it automatically.
pub fn (mut r StreamResult) next_batch(size int) ![]NullableRow {
	if size <= 0 {
		return error('db.mysql: stream batch size must be greater than zero')
	}
	if r.state == unsafe { nil } {
		return []NullableRow{}
	}
	mut state := r.state
	if state.closed || state.result == unsafe { nil } {
		state.close()
		return []NullableRow{}
	}
	mut rows := []NullableRow{cap: size}
	for rows.len < size {
		row_data := C.mysql_fetch_row(state.result)
		if row_data == unsafe { nil } {
			if get_errno(state.guard.conn) != 0 {
				err := error_with_code(get_error_msg(state.guard.conn), get_errno(state.guard.conn))
				state.close()
				return err
			}
			state.close()
			break
		}
		mut row := NullableRow{
			vals: []?string{cap: state.fields.len}
		}
		for i in 0 .. state.fields.len {
			if unsafe { row_data[i] == nil } {
				row.vals << none
			} else {
				length := checked_stream_value_length(C.v_mysql_fetch_column_length(state.result,
					u32(i))) or {
					state.close()
					return err
				}
				value := unsafe { (&u8(row_data[i])).vstring_with_len(length).clone() }
				row.vals << value
			}
		}
		rows << row
	}
	return rows
}

fn (mut state StreamResultState) close() {
	if state.closed {
		return
	}
	if state.result != unsafe { nil } {
		C.mysql_free_result(state.result)
		state.result = unsafe { nil }
	}
	drain_remaining_stream_results(state.guard.conn)
	state.guard.release()
	state.closed = true
}

// close frees an unbuffered result, discards any later results from a
// multi-statement query, and releases its connection lock. It is safe to call
// close more than once, including through copied handles.
pub fn (mut r StreamResult) close() {
	if r.state == unsafe { nil } {
		return
	}
	mut state := r.state
	state.close()
}

// StreamStmt is a one-shot prepared statement whose result is fetched in
// batches. It keeps the connection exclusively locked until exhausted or
// closed, and must be consumed and closed on the thread that created it.
pub struct StreamStmt {
mut:
	state &StreamStmtState = unsafe { nil }
}

@[heap]
struct StreamStmtState {
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
		state: &StreamStmtState{
			stmt:  stmt
			guard: guard
		}
	}
}

// execute binds string parameters and starts the unbuffered statement result.
pub fn (mut stmt StreamStmt) execute(params []string) ! {
	if stmt.state == unsafe { nil } {
		return error('db.mysql: cannot execute an uninitialized stream statement')
	}
	mut state := stmt.state
	if state.closed {
		return error('db.mysql: cannot execute a closed stream statement')
	}
	if state.executed {
		return error('db.mysql: a stream statement can only be executed once')
	}
	expected_params := int(C.mysql_stmt_param_count(state.stmt))
	if params.len != expected_params {
		state.close()
		return error('db.mysql: stream statement parameter count mismatch: expected ${expected_params}, got ${params.len}')
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
		&& C.mysql_stmt_bind_param(state.stmt, unsafe { &C.MYSQL_BIND(params_bind.data) }) {
		err := error_with_code(get_stmt_error_msg(state.stmt), get_stmt_errno(state.stmt))
		state.close()
		return err
	}
	if C.mysql_stmt_execute(state.stmt) != 0 {
		err := error_with_code(get_stmt_error_msg(state.stmt), get_stmt_errno(state.stmt))
		state.close()
		return err
	}
	state.executed = true
	metadata_result := C.mysql_stmt_result_metadata(state.stmt)
	if metadata_result == unsafe { nil } {
		state.close()
		return
	}
	state.fields = fields_from_result(metadata_result)
	C.mysql_free_result(metadata_result)
	num_fields := state.fields.len
	state.lengths = C.v_mysql_lengths_new(u32(num_fields))
	if state.lengths == unsafe { nil } {
		state.close()
		return error('db.mysql: failed to allocate stream result lengths')
	}
	state.is_null = []bool{len: num_fields}
	state.binds = []C.MYSQL_BIND{cap: num_fields}
	for i in 0 .. num_fields {
		state.binds << C.MYSQL_BIND{
			buffer_type: mysql_type_string
			is_null:     unsafe { &state.is_null[i] }
		}
	}
	for i in 0 .. num_fields {
		C.v_mysql_bind_set_length_at(unsafe { &state.binds[i] }, state.lengths, u32(i))
	}
	if C.mysql_stmt_bind_result(state.stmt, unsafe { &C.MYSQL_BIND(state.binds.data) }) {
		err := error_with_code(get_stmt_error_msg(state.stmt), get_stmt_errno(state.stmt))
		state.close()
		return err
	}
}

// fields returns metadata for the prepared statement result columns.
pub fn (stmt &StreamStmt) fields() []Field {
	if stmt.state == unsafe { nil } {
		return []Field{}
	}
	return stmt.state.fields.clone()
}

// next_batch reads at most `size` rows from the prepared statement result. An
// empty batch means that the result is exhausted and has been closed.
pub fn (mut stmt StreamStmt) next_batch(size int) ![]NullableRow {
	if size <= 0 {
		return error('db.mysql: stream batch size must be greater than zero')
	}
	if stmt.state == unsafe { nil } {
		return error('db.mysql: execute the stream statement before fetching rows')
	}
	mut state := stmt.state
	if !state.executed {
		return error('db.mysql: execute the stream statement before fetching rows')
	}
	if state.closed {
		return []NullableRow{}
	}
	mut rows := []NullableRow{cap: size}
	for rows.len < size {
		code := C.mysql_stmt_fetch(state.stmt)
		if code == mysql_no_data {
			state.close()
			break
		}
		if code !in [0, mysql_data_truncated] {
			err := error_with_code(get_stmt_error_msg(state.stmt), get_stmt_errno(state.stmt))
			state.close()
			return err
		}
		mut row := NullableRow{
			vals: []?string{cap: state.fields.len}
		}
		for i in 0 .. state.fields.len {
			if state.is_null[i] {
				row.vals << none
				continue
			}
			length := checked_stream_value_length(C.v_mysql_length_at(state.lengths, u32(i))) or {
				state.close()
				return err
			}
			if length == 0 {
				row.vals << ''
				continue
			}
			mut data := unsafe { malloc(length) }
			if data == unsafe { nil } {
				state.close()
				return error('db.mysql: failed to allocate streamed column data')
			}
			mut column_bind := C.MYSQL_BIND{
				buffer_type:   mysql_type_string
				buffer:        data
				buffer_length: u32(length)
				is_null:       unsafe { &state.is_null[i] }
			}
			C.v_mysql_bind_set_length_at(&column_bind, state.lengths, u32(i))
			column_code := C.mysql_stmt_fetch_column(state.stmt, &column_bind, i, 0)
			if column_code !in [0, mysql_data_truncated] {
				unsafe { free(data) }
				err := error_with_code(get_stmt_error_msg(state.stmt), get_stmt_errno(state.stmt))
				state.close()
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

fn (mut state StreamStmtState) close() {
	if state.closed {
		return
	}
	if state.stmt != unsafe { nil } {
		C.mysql_stmt_free_result(state.stmt)
		C.mysql_stmt_close(state.stmt)
		state.stmt = unsafe { nil }
	}
	if state.lengths != unsafe { nil } {
		unsafe { free(state.lengths) }
		state.lengths = unsafe { nil }
	}
	state.guard.release()
	state.closed = true
}

// close frees pending statement results, closes the statement, and releases
// its connection lock. It is safe to call close more than once.
pub fn (mut stmt StreamStmt) close() {
	if stmt.state == unsafe { nil } {
		return
	}
	mut state := stmt.state
	state.close()
}
