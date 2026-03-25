module mysql

// get_error_msg returns error message from MySQL instance.
fn get_error_msg(conn &C.MYSQL) string {
	return unsafe { C.mysql_error(conn).vstring() }
}

// get_errno returns error number from MySQL instance.
fn get_errno(conn &C.MYSQL) int {
	return C.mysql_errno(conn)
}

// get_stmt_error_msg returns error message from a MySQL statement instance.
fn get_stmt_error_msg(stmt &C.MYSQL_STMT) string {
	return unsafe { cstring_to_vstring(&char(C.mysql_stmt_error(stmt))) }
}

// get_stmt_errno returns error number from a MySQL statement instance.
fn get_stmt_errno(stmt &C.MYSQL_STMT) int {
	return C.mysql_stmt_errno(stmt)
}

// resolve_nil_str returns an empty string if passed value is a nil pointer.
fn resolve_nil_str(ptr &u8) string {
	if isnil(ptr) {
		return ''
	}
	return unsafe { ptr.vstring() }
}

@[inline]
fn mystring(b &u8) string {
	unsafe {
		return b.vstring()
	}
}
