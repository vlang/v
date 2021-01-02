module mysql


// Values for the capabilities flag bitmask used by the MySQL protocol.
// See more on https://dev.mysql.com/doc/dev/mysql-server/latest/group__group__cs__capabilities__flags.html#details
pub enum ConnectionFlag {
	// CAN_HANDLE_EXPIRED_PASSWORDS       = C.CAN_HANDLE_EXPIRED_PASSWORDS
	client_compress         = C.CLIENT_COMPRESS
	client_found_rows       = C.CLIENT_FOUND_ROWS
	client_ignore_sigpipe   = C.CLIENT_IGNORE_SIGPIPE
	client_ignore_space     = C.CLIENT_IGNORE_SPACE
	client_interactive      = C.CLIENT_INTERACTIVE
	client_local_files      = C.CLIENT_LOCAL_FILES
	client_multi_results    = C.CLIENT_MULTI_RESULTS
	client_multi_statements = C.CLIENT_MULTI_STATEMENTS
	client_no_schema        = C.CLIENT_NO_SCHEMA
	client_odbc             = C.CLIENT_ODBC
	// client_optional_resultset_metadata = C.CLIENT_OPTIONAL_RESULTSET_METADATA
	client_ssl              = C.CLIENT_SSL
	client_remember_options = C.CLIENT_REMEMBER_OPTIONS
}

// TODO: Documentation
pub struct Connection {
mut:
	conn     &C.MYSQL = C.mysql_init(0)
pub mut:
	host     string = '127.0.0.1'
	port     u32 = 3306
	username string
	password string
	dbname   string
	flag     ConnectionFlag
}

// connect connects to a MySQL server.
pub fn (mut conn Connection) connect() ?bool {
	instance := C.mysql_init(conn.conn)
	conn.conn = C.mysql_real_connect(conn.conn, conn.host.str, conn.username.str, conn.password.str,
		conn.dbname.str, conn.port, 0, conn.flag)
	if isnil(conn.conn) {
		return error_with_code(get_error_msg(instance), get_errno(instance))
	}
	return true
}

// query executes an SQL query.
// `query()` cannot be used for statements that contain binary data; you must use `real_query()` instead.
pub fn (conn Connection) query(q string) ?Result {
	if C.mysql_query(conn.conn, q.str) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	res := C.mysql_store_result(conn.conn)
	return Result{res}
}

// real_query executes an SQL query. Same as `query ()`,
// But `real_query ()` can be used for statements containing binary data.
// (Binary data may contain the \0 character, which `query()` interprets as the end of the statement string).
// In addition, `real_query()` is faster than `query()`.
pub fn (conn Connection) real_query(q string) ?Result {
	if C.mysql_real_query(conn.conn, q.str, q.len) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	res := C.mysql_store_result(conn.conn)
	return Result{res}
}

// select_db selects the default database for database queries.
pub fn (conn Connection) select_db(dbname string) ?bool {
	if C.mysql_select_db(conn.conn, dbname.str) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return true
}

// change_user changes the user of the specified database connection.
// if desired, the empty string value can be passed to the `dbname` parameter
// resulting in only changing the user and not selecting a database.
pub fn (conn Connection) change_user(username string, password string, dbname string) ?bool {
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

// affected_rows returns the number of rows changed/deleted/inserted
// by the last UPDATE, DELETE, or INSERT query.
pub fn (conn Connection) affected_rows() u64 {
	return C.mysql_affected_rows(conn.conn)
}

// autocommit turns on or off auto-committing database modifications.
pub fn (conn Connection) autocommit(mode bool) {
	C.mysql_autocommit(conn.conn, mode)
}

// tables returns list of tables that match the `wildcard` parameter.
// If empty string is passed, will return all tables.
pub fn (conn Connection) tables(wildcard string) ?[]string {
	cres := C.mysql_list_tables(conn.conn, wildcard.str)
	if isnil(cres) {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	res := Result{cres}
	mut tables := []string{}
	for row in res.rows() {
		tables << row.vals[0]
	}
	res.free()
	return tables
}

// escape_string creates a legal SQL string for use in an SQL statement.
pub fn (conn Connection) escape_string(s string) string {
	to := malloc(2 * s.len + 1)
	C.mysql_real_escape_string_quote(conn.conn, to, s.str, s.len, `\'`)
	return unsafe {to.vstring()}
}

// set_option is used to set extra connect options and affect behavior for a connection.
// This function may be called multiple times to set several options.
// To retrieve option values, use `get_option()`.
pub fn (conn Connection) set_option(option_type int, val voidptr) {
	C.mysql_options(conn.conn, option_type, val)
}

// get_option returns the current value of an option settable `set_option`.
// The value should be treated as read only.
pub fn (conn Connection) get_option(option_type int) ?voidptr {
	ret := voidptr(0)
	if C.mysql_get_option(conn.conn, option_type, &ret) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return ret
}

// refresh flushes tables or caches, or resets replication server information.
// The connected user must have the `RELOAD` privilege.
pub fn (conn Connection) refresh(options u32) ?bool {
	if C.mysql_refresh(conn.conn, options) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return true
}

// reset resets the connection to clear the session state.
pub fn (conn Connection) reset() ?bool {
	if C.mysql_reset_connection(conn.conn) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return true
}

// ping pings a server connection, or tries to reconnect if the connection has gone down.
pub fn (conn Connection) ping() ?bool {
	if C.mysql_ping(conn.conn) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return true
}

// close closes a previously opened database connection.
pub fn (conn &Connection) close() {
	C.mysql_close(conn.conn)
}

// -------------------------- MYSQL INFO & VERSION --------------------------
// info returns information about the most recently executed query.
pub fn (conn Connection) info() string {
	return resolve_nil_str(C.mysql_info(conn.conn))
}

// get_host_info returns a string describing the connection.
pub fn (conn Connection) get_host_info() string {
	return unsafe {C.mysql_get_host_info(conn.conn).vstring()}
}

// get_server_info returns the server version number as a string.
pub fn (conn Connection) get_server_info() string {
	return unsafe {C.mysql_get_server_info(conn.conn).vstring()}
}

// get_server_version returns the server version number as an integer.
pub fn (conn Connection) get_server_version() u64 {
	return C.mysql_get_server_version(conn.conn)
}

// --------------------------------- CLIENT ---------------------------------
// get_client_info returns client version information as a string.
pub fn get_client_info() string {
	return unsafe {C.mysql_get_client_info().vstring()}
}

// get_client_version returns client version information as an integer.
pub fn get_client_version() u64 {
	return C.mysql_get_client_version()
}

// ------------------------------- MYSQL DEBUG ------------------------------
// dump_debug_info causes the server to write debug information to the log
pub fn (conn Connection) dump_debug_info() ?bool {
	if C.mysql_dump_debug_info(conn.conn) != 0 {
		return error_with_code(get_error_msg(conn.conn), get_errno(conn.conn))
	}
	return true
}

// debug does a `DBUG_PUSH` with the given string.
// See https://dev.mysql.com/doc/refman/5.7/en/mysql-debug.html
pub fn debug(debug string) {
	C.mysql_debug(debug.str)
}
