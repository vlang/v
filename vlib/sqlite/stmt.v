module sqlite

fn C.sqlite3_bind_double(&C.sqlite3_stmt, int, f64) int
fn C.sqlite3_bind_int(&C.sqlite3_stmt, int, int) int
fn C.sqlite3_bind_int64(&C.sqlite3_stmt, int, i64) int
fn C.sqlite3_bind_text(&C.sqlite3_stmt, int, charptr, int, voidptr) int

// Only for V ORM
fn (db DB) init_stmt(query string) Stmt {
	// println('init_stmt("$query")')
	stmt := &C.sqlite3_stmt(0)
	C.sqlite3_prepare_v2(db.conn, &char(query.str), query.len, &stmt, 0)
	return Stmt{stmt}
}

fn (stmt Stmt) bind_int(idx int, v int) int {
	return C.sqlite3_bind_int(stmt.stmt, idx, v)
}

fn (stmt Stmt) bind_i64(idx int, v i64) int {
	return C.sqlite3_bind_int64(stmt.stmt, idx, v)
}

fn (stmt Stmt) bind_f64(idx int, v f64) int {
	return C.sqlite3_bind_double(stmt.stmt, idx, v)
}

fn (stmt Stmt) bind_text(idx int, s string) int {
	return C.sqlite3_bind_text(stmt.stmt, idx, s.str, s.len, 0)
}