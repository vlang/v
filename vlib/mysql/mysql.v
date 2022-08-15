module mysql

// Values for the capabilities flag bitmask used by the MySQL protocol.
// See more on https://dev.mysql.com/doc/dev/mysql-server/latest/group__group__cs__capabilities__flags.html#details
pub enum ConnectionFlag {
	// CAN_HANDLE_EXPIRED_PASSWORDS       = C.CAN_HANDLE_EXPIRED_PASSWORDS
	client_compress = C.CLIENT_COMPRESS
	client_found_rows = C.CLIENT_FOUND_ROWS
	client_ignore_sigpipe = C.CLIENT_IGNORE_SIGPIPE
	client_ignore_space = C.CLIENT_IGNORE_SPACE
	client_interactive = C.CLIENT_INTERACTIVE
	client_local_files = C.CLIENT_LOCAL_FILES
	client_multi_results = C.CLIENT_MULTI_RESULTS
	client_multi_statements = C.CLIENT_MULTI_STATEMENTS
	client_no_schema = C.CLIENT_NO_SCHEMA
	client_odbc = C.CLIENT_ODBC
	// client_optional_resultset_metadata = C.CLIENT_OPTIONAL_RESULTSET_METADATA
	client_ssl = C.CLIENT_SSL
	client_remember_options = C.CLIENT_REMEMBER_OPTIONS
}

struct SQLError {
	MessageError
}

// TODO: Documentation
pub struct Connection {
mut:
	conn &C.MYSQL = C.mysql_init(0)
pub mut:
	host     string = '127.0.0.1'
	port     u32    = 3306
	username string
	password string
	dbname   string
	flag     ConnectionFlag
}

// connect - create a new connection to the MySQL server.
pub fn (mut conn Connection) connect() ?bool {
	instance := C.mysql_init(conn.conn)
	conn.conn = C.mysql_real_connect(instance, conn.host.str, conn.username.str, conn.password.str,
		conn.dbname.str, conn.port, 0, conn.flag)
	if isnil(conn.conn) {
		return error_with_code(get_error_msg(instance), get_errno(instance))
	}
	return true
}

// query - make an SQL query and receive the results.
// `query()` cannot be used for statements that contain binary data;
// Use `real_query()` instead.
pub fn (conn Connection) query(q string) ?Result {
	if C.mysql_query(conn.conn, q.str) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	res := C.mysql_store_result(conn.conn)
	return Result{res}
}

// use_result - reads the result of a query
// used after invoking mysql_real_query() or mysql_query(),
// for every statement that successfully produces a result set
// (SELECT, SHOW, DESCRIBE, EXPLAIN, CHECK TABLE, and so forth).
// This reads the result of a query directly from the server
// without storing it in a temporary table or local buffer,
// mysql_use_result is faster and uses much less memory than C.mysql_store_result().
// You must mysql_free_result() after you are done with the result set.
pub fn (conn Connection) use_result() {
	C.mysql_use_result(conn.conn)
}

// real_query - make an SQL query and receive the results.
// `real_query()` can be used for statements containing binary data.
// (Binary data may contain the `\0` character, which `query()`
// interprets as the end of the statement string). In addition,
// `real_query()` is faster than `query()`.
pub fn (mut conn Connection) real_query(q string) ?Result {
	if C.mysql_real_query(conn.conn, q.str, q.len) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	res := C.mysql_store_result(conn.conn)
	return Result{res}
}

// select_db - change the default database for database queries.
pub fn (mut conn Connection) select_db(dbname string) ?bool {
	if C.mysql_select_db(conn.conn, dbname.str) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return true
}

// change_user - change the mysql user for the connection.
// Passing an empty string for the `dbname` parameter, resultsg in only changing
// the user and not changing the default database for the connection.
pub fn (mut conn Connection) change_user(username string, password string, dbname string) ?bool {
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

// affected_rows - return the number of rows changed/deleted/inserted
// by the last `UPDATE`, `DELETE`, or `INSERT` query.
pub fn (conn &Connection) affected_rows() u64 {
	return C.mysql_affected_rows(conn.conn)
}

// autocommit - turns on/off the auto-committing mode for the connection.
// When it is on, then each query is commited right away.
pub fn (mut conn Connection) autocommit(mode bool) {
	C.mysql_autocommit(conn.conn, mode)
}

// tables - returns a list of the names of the tables in the current database,
// that match the simple regular expression specified by the `wildcard` parameter.
// The `wildcard` parameter may contain the wildcard characters `%` or `_`.
// If an empty string is passed, it will return all tables.
// Calling `tables()` is similar to executing query `SHOW TABLES [LIKE wildcard]`.
pub fn (conn &Connection) tables(wildcard string) ?[]string {
	cres := C.mysql_list_tables(conn.conn, wildcard.str)
	if isnil(cres) {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	res := Result{cres}
	mut tables := []string{}
	for row in res.rows() {
		tables << row.vals[0]
	}
	return tables
}

// escape_string - creates a legal SQL string for use in an SQL statement.
// The `s` argument is encoded to produce an escaped SQL string,
// taking into account the current character set of the connection.
pub fn (conn &Connection) escape_string(s string) string {
	unsafe {
		to := malloc_noscan(2 * s.len + 1)
		C.mysql_real_escape_string(conn.conn, to, s.str, s.len)
		return to.vstring()
	}
}

// set_option - sets extra connect options that affect the behavior of
// a connection. This function may be called multiple times to set several
// options. To retrieve the current values for an option, use `get_option()`.
pub fn (mut conn Connection) set_option(option_type int, val voidptr) {
	C.mysql_options(conn.conn, option_type, val)
}

// get_option - return the value of an option, settable by `set_option`.
// https://dev.mysql.com/doc/c-api/5.7/en/mysql-get-option.html
pub fn (conn &Connection) get_option(option_type int) ?voidptr {
	ret := unsafe { nil }
	if C.mysql_get_option(conn.conn, option_type, &ret) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return ret
}

// refresh - flush the tables or caches, or resets replication server
// information. The connected user must have the `RELOAD` privilege.
pub fn (mut conn Connection) refresh(options u32) ?bool {
	if C.mysql_refresh(conn.conn, options) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return true
}

// reset - resets the connection, and clear the session state.
pub fn (mut conn Connection) reset() ?bool {
	if C.mysql_reset_connection(conn.conn) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return true
}

// ping - pings a server connection, or tries to reconnect if the connection
// has gone down.
pub fn (mut conn Connection) ping() ?bool {
	if C.mysql_ping(conn.conn) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return true
}

// close - closes the connection.
pub fn (mut conn Connection) close() {
	C.mysql_close(conn.conn)
}

// info - returns information about the most recently executed query.
// See more on https://dev.mysql.com/doc/c-api/8.0/en/mysql-info.html
pub fn (conn &Connection) info() string {
	return resolve_nil_str(C.mysql_info(conn.conn))
}

// get_host_info - returns a string describing the type of connection in use,
// including the server host name.
pub fn (conn &Connection) get_host_info() string {
	return unsafe { C.mysql_get_host_info(conn.conn).vstring() }
}

// get_server_info - returns a string representing the MySQL server version.
// For example, `8.0.24`.
pub fn (conn &Connection) get_server_info() string {
	return unsafe { C.mysql_get_server_info(conn.conn).vstring() }
}

// get_server_version - returns an integer, representing the MySQL server
// version. The value has the format `XYYZZ` where `X` is the major version,
// `YY` is the release level (or minor version), and `ZZ` is the sub-version
// within the release level. For example, `8.0.24` is returned as `80024`.
pub fn (conn &Connection) get_server_version() u64 {
	return C.mysql_get_server_version(conn.conn)
}

// dump_debug_info - instructs the server to write debugging information
// to the error log. The connected user must have the `SUPER` privilege.
pub fn (mut conn Connection) dump_debug_info() ?bool {
	if C.mysql_dump_debug_info(conn.conn) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return true
}

// get_client_info - returns client version information as a string.
pub fn get_client_info() string {
	return unsafe { C.mysql_get_client_info().vstring() }
}

// get_client_version - returns the client version information as an integer.
pub fn get_client_version() u64 {
	return C.mysql_get_client_version()
}

// debug - does a `DBUG_PUSH` with the given string.
// `debug()` uses the Fred Fish debug library.
// To use this function, you must compile the client library to support debugging.
// See https://dev.mysql.com/doc/c-api/8.0/en/mysql-debug.html
pub fn debug(debug string) {
	C.mysql_debug(debug.str)
}
