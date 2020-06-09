module mysql

#flag -lmysqlclient
#flag linux -I/usr/include/mysql
#include <mysql.h>

// TODO: Documentation
pub struct Connection {
mut:
	conn     &C.MYSQL = C.mysql_init(0)
pub mut:
	host     string		= '127.0.0.1'
	port     u32		= 3306
	username string
	password string
	dbname   string
	flag     int
}

// Connects to a MySQL server.
pub fn (mut conn Connection) connect() ?bool {
	instance := C.mysql_init(conn.conn)
	conn.conn = C.mysql_real_connect(
		conn.conn,
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

// Performs a query on the database.
pub fn (conn Connection) query(q string) ?Result {
	if C.mysql_query(conn.conn, q.str) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	res := C.mysql_store_result(conn.conn)
	return Result{res}
}

// Selects the default database for database queries.
pub fn (conn Connection) select_db(dbname string) ?bool {
	if C.mysql_select_db(conn.conn, dbname.str) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return true
}

// Changes the user of the specified database connection.
// If desired, the empty string value can be passed to the `dbname` parameter
// resulting in only changing the user and not selecting a database.
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

// Returns the number of rows changed/deleted/inserted
// by the last UPDATE, DELETE, or INSERT query.
pub fn (conn Connection) affected_rows() u64 {
	return C.mysql_affected_rows(conn.conn)
}

// Turns on or off auto-committing database modifications.
pub fn (conn Connection) autocommit(mode bool) {
	C.mysql_autocommit(conn.conn, mode)
}

// Returns list of tables that match the `wildcard` parameter.
// If empty string is passed, will return all tables.
pub fn (conn Connection) tables(wildcard string) ?[]string {
	cres := C.mysql_list_tables(conn.conn, wildcard.str)
	if isnil(cres) {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	res :=  Result{cres}
	mut tables := []string{}
	for row in res.rows() {
		tables << row.vals[0]
	}
	res.free()
	return tables
}

// Creates a legal SQL string for use in an SQL statement.
pub fn (conn Connection) escape_string(s string) string {
    len := C.strlen(s.str)
    to := malloc(2 * len + 1)
    quote := byte(39) // single quote

    C.mysql_real_escape_string_quote(conn.conn, to, s.str, len, quote)
    return string(to)
}

// Can be used to set extra connect options and affect behavior for a connection.
// This function may be called multiple times to set several options.
// To retrieve option values, use `get_option()`.
pub fn (conn Connection) set_option(option_type int, val voidptr) {
	C.mysql_options(conn.conn, option_type, val)
}

// Returns the current value of an option settable `set_option`.
// The value should be treated as read only.
pub fn (conn Connection) get_option(option_type int) ?voidptr {
	ret := voidptr(0)
	if C.mysql_get_option(conn.conn, option_type, &ret) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return ret
}

// Flushes tables or caches, or resets replication server information.
// The connected user must have the `RELOAD` privilege.
pub fn (conn Connection) refresh(options u32) ?bool {
	if C.mysql_refresh(conn.conn, options) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return true
}

// Resets the connection to clear the session state.
pub fn (conn Connection) reset_connection() ?bool {
	if C.mysql_reset_connection(conn.conn) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return true
}

// Pings a server connection, or tries to reconnect if the connection has gone down.
pub fn (conn Connection) ping() ?bool {
	if C.mysql_ping(conn.conn) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return true
}

// Closes a previously opened database connection.
pub fn (conn &Connection) close() {
	C.mysql_close(conn.conn)
}

/* -------------------------- MYSQL INFO & VERSION -------------------------- */

// Returns information about the most recently executed query.
pub fn (conn Connection) info() string {
	return resolve_nil_str(C.mysql_info(conn.conn))
}

// Returns a string describing the connection.
pub fn (conn Connection) get_host_info() string {
	return string(C.mysql_get_host_info(conn.conn))
}

// Returns the server version number as a string.
pub fn (conn Connection) get_server_info() string {
	return string(C.mysql_get_server_info(conn.conn))
}

// Returns the server version number as an integer.
pub fn (conn Connection) get_server_version() u64 {
	return C.mysql_get_server_version(conn.conn)
}

/* --------------------------------- CLIENT --------------------------------- */

// Returns client version information as a string.
pub fn get_client_info() string {
	return string(C.mysql_get_client_info())
}

// Returns client version information as an integer.
pub fn get_client_version() u64 {
	return C.mysql_get_client_version()
}

/* ------------------------------- MYSQL DEBUG ------------------------------ */

// Causes the server to write debug information to the log
pub fn (conn Connection) dump_debug_info() ?bool {
	if C.mysql_dump_debug_info(conn.conn) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return true
}

// Does a `DBUG_PUSH` with the given string.
// See https://dev.mysql.com/doc/refman/5.7/en/mysql-debug.html
pub fn debug(debug string) {
	C.mysql_debug(debug.str)
}
