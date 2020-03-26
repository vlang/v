module mysql

fn get_error_msg(conn &MYSQL) string {
	return string(mysql_error(conn))
}

fn get_errno(conn &MYSQL) int {
	return mysql_errno(conn)
}

fn resolve_nil_str(ptr byteptr) string {
	if isnil(ptr) {
		return ''
	}
	return string(ptr)
}
