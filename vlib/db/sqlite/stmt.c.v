module sqlite

fn C.sqlite3_bind_null(&C.sqlite3_stmt, int) int
fn C.sqlite3_bind_double(&C.sqlite3_stmt, int, f64) int
fn C.sqlite3_bind_int(&C.sqlite3_stmt, int, int) int
fn C.sqlite3_bind_int64(&C.sqlite3_stmt, int, i64) int
fn C.sqlite3_bind_text(&C.sqlite3_stmt, int, &char, int, voidptr) int

// Only for V ORM
fn (db &DB) init_stmt(query string) (&C.sqlite3_stmt, int) {
	// println('init_stmt("$query")')
	stmt := &C.sqlite3_stmt(unsafe { nil })
	err := C.sqlite3_prepare_v2(db.conn, &char(query.str), query.len, &stmt, 0)
	return stmt, err
}

fn (db &DB) new_init_stmt(query string) !Stmt {
	stmt, err := db.init_stmt(query)
	if err != sqlite_ok {
		return db.error_message(err, query)
	}
	return Stmt{stmt, db}
}

fn (stmt &Stmt) bind_null(idx int) int {
	return C.sqlite3_bind_null(stmt.stmt, idx)
}

fn (stmt &Stmt) bind_int(idx int, v int) int {
	return C.sqlite3_bind_int(stmt.stmt, idx, v)
}

fn (stmt &Stmt) bind_i64(idx int, v i64) int {
	return C.sqlite3_bind_int64(stmt.stmt, idx, v)
}

fn (stmt &Stmt) bind_f64(idx int, v f64) int {
	return C.sqlite3_bind_double(stmt.stmt, idx, v)
}

fn (stmt &Stmt) bind_text(idx int, s string) int {
	return C.sqlite3_bind_text(stmt.stmt, idx, voidptr(s.str), s.len, 0)
}

fn (stmt &Stmt) get_int(idx int) ?int {
	if C.sqlite3_column_type(stmt.stmt, idx) == C.SQLITE_NULL {
		return none
	} else {
		return C.sqlite3_column_int(stmt.stmt, idx)
	}
}

fn (stmt &Stmt) get_i64(idx int) ?i64 {
	if C.sqlite3_column_type(stmt.stmt, idx) == C.SQLITE_NULL {
		return none
	} else {
		return C.sqlite3_column_int64(stmt.stmt, idx)
	}
}

fn (stmt &Stmt) get_f64(idx int) ?f64 {
	if C.sqlite3_column_type(stmt.stmt, idx) == C.SQLITE_NULL {
		return none
	} else {
		return C.sqlite3_column_double(stmt.stmt, idx)
	}
}

fn (stmt &Stmt) get_text(idx int) ?string {
	if C.sqlite3_column_type(stmt.stmt, idx) == C.SQLITE_NULL {
		return none
	} else {
		b := &char(C.sqlite3_column_text(stmt.stmt, idx))
		if b == &char(unsafe { nil }) {
			return ''
		}
		return unsafe { b.vstring() }
	}
}

fn (stmt &Stmt) get_count() int {
	return C.sqlite3_column_count(stmt.stmt)
}

fn (stmt &Stmt) step() int {
	return C.sqlite3_step(stmt.stmt)
}

fn (stmt &Stmt) orm_step(query string) ! {
	res := stmt.step()
	if res != sqlite_ok && res != sqlite_done && res != sqlite_row {
		return stmt.db.error_message(res, query)
	}
}

fn (stmt &Stmt) finalize() {
	C.sqlite3_finalize(stmt.stmt)
}
