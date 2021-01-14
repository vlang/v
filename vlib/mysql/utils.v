module mysql

// get_error_msg - returns error message from MySQL instance.
fn get_error_msg(conn &C.MYSQL) string {
	return unsafe { C.mysql_error(conn).vstring() }
}

// get_errno - returns error number from MySQL instance.
fn get_errno(conn &C.MYSQL) int {
	return C.mysql_errno(conn)
}

// resolve_nil_str - returns an empty string if passed value is a nil pointer.
fn resolve_nil_str(ptr byteptr) string {
	if isnil(ptr) {
		return ''
	}
	return unsafe { ptr.vstring() }
}

[inline]
fn mystring(b byteptr) string {
	unsafe {
		return b.vstring()
	}
}
