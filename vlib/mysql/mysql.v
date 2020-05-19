module mysql

#flag -lmysqlclient
#flag linux -I/usr/include/mysql
#include <mysql.h>

pub struct Connection {
	host     string
	port     u32
	username string
	password string
	dbname   string
	flag     int
mut:
	conn     &C.MYSQL
}

pub fn new_connection(host, username, password, dbname string) ?Connection {
	instance := C.mysql_init(0)
	if isnil(instance) {
		return error_with_code(get_error_msg(instance), get_errno(instance))
	}
	return Connection{ host, 0, username, password, dbname, 0, instance }
}

pub fn (mut conn Connection) connect() ?bool {
	mut instance := C.mysql_init(0)
	if !isnil(conn.conn) {
		instance = conn.conn
	}
	if isnil(instance) {
		return error_with_code(get_error_msg(instance), get_errno(instance))
	}
	conn.conn = C.mysql_real_connect(
		instance,
		conn.host.str,
		conn.username.str,
		conn.password.str,
		conn.dbname.str,
		conn.port,
		0,
		conn.flag
	)
	if isnil(conn.conn) {
		return error_with_code(get_error_msg(instance), get_errno(instance))
	}
	return true
}

pub fn (conn Connection) query(q string) ?Result {
	if C.mysql_query(conn.conn, q.str) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	res := C.mysql_store_result(conn.conn)
	return Result{res}
}

pub fn (conn Connection) select_db(dbname string) ?bool {
	if C.mysql_select_db(conn.conn, dbname.str) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return true
}

pub fn (conn Connection) change_user(username, password, dbname string) ?bool {
	mut ret := true
	if dbname != '' {
		ret = C.mysql_change_user(conn.conn, username.str, password.str, dbname.str)
	} else {
		ret = C.mysql_change_user(conn.conn, username.str, password.str, 0)
	}
	if !ret {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return ret
}

pub fn (conn Connection) affected_rows() u64 {
	return C.mysql_affected_rows(conn.conn)
}

pub fn (conn Connection) autocommit(mode bool) {
	C.mysql_autocommit(conn.conn, mode)
}


pub fn (conn Connection) escape_string(s string) string {
    len := C.strlen(s.str)
    to := malloc(2 * len + 1)
    quote := byte(39) // single quote

    C.mysql_real_escape_string_quote(conn.conn, to, s.str, len, quote)
    return string(to)
}

pub fn (conn Connection) set_option(option_type int, val voidptr) {
	C.mysql_options(conn.conn, option_type, val)
}

pub fn (conn Connection) get_option(option_type int) ?voidptr {
	ret := voidptr(0)
	if C.mysql_get_option(conn.conn, option_type, &ret) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return ret
}

pub fn (conn Connection) refresh(options u32) ?bool {
	if C.mysql_refresh(conn.conn, options) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return true
}

pub fn (conn Connection) reset_connection() ?bool {
	if C.mysql_reset_connection(conn.conn) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return true
}

pub fn (conn Connection) ping() ?bool {
	if C.mysql_ping(conn.conn) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return true
}

pub fn (conn Connection) close() {
	C.mysql_close(conn.conn)
}

/* MYSQL INFO & VERSION */

pub fn (conn Connection) info() string {
	return string(C.mysql_info(conn.conn))
}

pub fn (conn Connection) get_host_info() string {
	return string(C.mysql_get_host_info(conn.conn))
}

pub fn (conn Connection) get_server_info() string {
	return string(C.mysql_get_server_info(conn.conn))
}

pub fn (conn Connection) get_server_version() u64 {
	return C.mysql_get_server_version(conn.conn)
}

pub fn get_client_version() u64 {
	return C.mysql_get_client_version()
}

pub fn get_client_info() string {
	return string(C.mysql_get_client_info())
}

/* MYSQL DEBUG */
pub fn (conn Connection) dump_debug_info() ?bool {
	if C.mysql_dump_debug_info(conn.conn) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return true
}

pub fn debug(debug string) {
	C.mysql_debug(debug.str)
}
