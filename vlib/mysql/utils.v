module mysql

// get_error_msg returns error message from MySQL instance.
fn get_error_msg(conn &C.MYSQL) string {
	return string(C.mysql_error(conn))
}

// get_errno returns error number from MySQL instance.
fn get_errno(conn &C.MYSQL) int {
	return C.mysql_errno(conn)
}

// resolve_nil_str returns empty string if passed value is a nil pointer.
fn resolve_nil_str(ptr byteptr) string {
	if isnil(ptr) { return '' }
	return string(ptr)
}
