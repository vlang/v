module mysql

fn get_error_msg(conn &C.MYSQL) string {
	return string(C.mysql_error(conn))
}

fn get_errno(conn &C.MYSQL) int {
	return C.mysql_errno(conn)
}

fn resolve_nil_str(ptr byteptr) string {
	if isnil(ptr) {
		return ''
	}
	return string(ptr)
}
