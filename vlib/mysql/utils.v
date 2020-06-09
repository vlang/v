module mysql

// Return error message from MySQL instance.
fn get_error_msg(conn &C.MYSQL) string {
	return string(C.mysql_error(conn))
}

// Return error number from MySQL instance.
fn get_errno(conn &C.MYSQL) int {
	return C.mysql_errno(conn)
}

// Returns empty string if passed value is a nil pointer.
fn resolve_nil_str(ptr byteptr) string {
	if isnil(ptr) { return '' }
	return string(ptr)
}
