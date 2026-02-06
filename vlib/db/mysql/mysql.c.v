module mysql

// Values for the capabilities flag bitmask used by the MySQL protocol.
// See more on https://dev.mysql.com/doc/dev/mysql-server/latest/group__group__cs__capabilities__flags.html#details
@[flag]
pub enum ConnectionFlag {
	client_long_password                  // (1 <<  0) Use the improved version of Old Password Authentication
	client_found_rows                     // (1 <<  1) Send found rows instead of affected rows in EOF_Packet
	client_long_flag                      // (1 <<  2) Get all column flags
	client_connect_with_db                // (1 <<  3) Database (schema) name can be specified on connect in Handshake Response Packet
	client_no_schema                      // (1 <<  4) DEPRECATED: Don't allow database.table.column
	client_compress                       // (1 <<  5) Compression protocol supported
	client_odbc                           // (1 <<  6) Special handling of ODBC behavior
	client_local_files                    // (1 <<  7) Can use LOAD DATA LOCAL
	client_ignore_space                   // (1 <<  8) Ignore spaces before '('
	client_protocol_41                    // (1 <<  9) New 4.1 protocol
	client_interactive                    // (1 << 10) This is an interactive client
	client_ssl                            // (1 << 11) Use SSL encryption for the session
	client_ignore_sigpipe                 // (1 << 12) Client only flag
	client_transactions                   // (1 << 13) Client knows about transactions
	client_reserved                       // (1 << 14) DEPRECATED: Old flag for 4.1 protocol
	client_reserved2                      // (1 << 15) DEPRECATED: Old flag for 4.1 authentication \ CLIENT_SECURE_CONNECTION
	client_multi_statements               // (1 << 16) Enable/disable multi-stmt support
	client_multi_results                  // (1 << 17) Enable/disable multi-results
	client_ps_multi_results               // (1 << 18) Multi-results and OUT parameters in PS-protocol
	client_plugin_auth                    // (1 << 19) Client supports plugin authentication
	client_connect_attrs                  // (1 << 20) Client supports connection attributes
	client_plugin_auth_lenenc_client_data // (1 << 21) Enable authentication response packet to be larger than 255 bytes
	client_can_handle_expired_passwords   // (1 << 22) Don't close the connection for a user account with expired password
	client_session_track                  // (1 << 23) Capable of handling server state change information
	client_deprecate_eof                  // (1 << 24) Client no longer needs EOF_Packet and will use OK_Packet instead
	client_optional_resultset_metadata    // (1 << 25) The client can handle optional metadata information in the resultset
	client_zstd_compression_algorithm     // (1 << 26) Compression protocol extended to support zstd compression method
	client_query_attributes               // (1 << 27) Support optional extension for query parameters into the COM_QUERY and COM_STMT_EXECUTE packets
	multi_factor_authentication           // (1 << 28) Support Multi factor authentication
	client_capability_extension           // (1 << 29) This flag will be reserved to extend the 32bit capabilities structure to 64bits
	client_ssl_verify_server_cert         // (1 << 30) Verify server certificate
	client_remember_options               // (1 << 31) Don't reset the options after an unsuccessful connect
}

pub enum MySQLTransactionLevel {
	read_uncommitted
	read_committed
	repeatable_read
	serializable
}

struct SQLError {
	MessageError
}

pub struct DB {
mut:
	conn &C.MYSQL = unsafe { nil }
}

@[params]
pub struct Config {
pub mut:
	host     string = '127.0.0.1'
	port     u32    = 3306
	username string
	password string
	dbname   string
	flag     ConnectionFlag

	// SSL params, only valid when set .client_ssl
	ssl_key    string
	ssl_cert   string
	ssl_ca     string
	ssl_capath string
	ssl_cipher string
}

// connect attempts to establish a connection to a MySQL server.
pub fn connect(config Config) !DB {
	mut db := DB{
		conn: C.mysql_init(0)
	}

	if config.flag.has(.client_ssl) {
		if config.ssl_key.len > 0 {
			db.set_option(C.MYSQL_OPT_SSL_KEY, config.ssl_key.str)
		}
		if config.ssl_cert.len > 0 {
			db.set_option(C.MYSQL_OPT_SSL_CERT, config.ssl_cert.str)
		}
		if config.ssl_ca.len > 0 {
			db.set_option(C.MYSQL_OPT_SSL_CA, config.ssl_ca.str)
		}
		if config.ssl_capath.len > 0 {
			db.set_option(C.MYSQL_OPT_SSL_CAPATH, config.ssl_capath.str)
		}
		if config.ssl_cipher.len > 0 {
			db.set_option(C.MYSQL_OPT_SSL_CIPHER, config.ssl_cipher.str)
		}
	}

	connection := C.mysql_real_connect(db.conn, config.host.str, config.username.str,
		config.password.str, config.dbname.str, config.port, 0, config.flag)

	if isnil(connection) {
		db.throw_mysql_error()!
	}

	// Sets `db.conn` after checking for `null`
	// because `throw_mysql_error` can't extract an error from a `null` connection,
	// and `panic` will be with an empty message.
	db.conn = connection

	return db
}

// query executes the SQL statement pointed to by the string `q`.
// It cannot be used for statements that contain binary data;
// Use `real_query()` instead.
pub fn (db &DB) query(q string) !Result {
	if C.mysql_query(db.conn, charptr(q.str)) != 0 {
		db.throw_mysql_error()!
	}

	result := C.mysql_store_result(db.conn)
	return Result{result}
}

// use_result reads the result of a query
// used after invoking mysql_real_query() or mysql_query(),
// for every statement that successfully produces a result set
// (SELECT, SHOW, DESCRIBE, EXPLAIN, CHECK TABLE, and so forth).
// This reads the result of a query directly from the server
// without storing it in a temporary table or local buffer,
// mysql_use_result is faster and uses much less memory than C.mysql_store_result().
// You must mysql_free_result() after you are done with the result set.
pub fn (db &DB) use_result() {
	C.mysql_use_result(db.conn)
}

// real_query makes an SQL query and receive the results.
// `real_query()` can be used for statements containing binary data.
// (Binary data may contain the `\0` character, which `query()`
// interprets as the end of the statement string). In addition,
// `real_query()` is faster than `query()`.
pub fn (mut db DB) real_query(q string) !Result {
	if C.mysql_real_query(db.conn, q.str, q.len) != 0 {
		db.throw_mysql_error()!
	}

	result := C.mysql_store_result(db.conn)
	return Result{result}
}

// select_db causes the database specified by `db` to become
// the default (current) database on the connection specified by mysql.
pub fn (mut db DB) select_db(dbname string) !bool {
	if C.mysql_select_db(db.conn, dbname.str) != 0 {
		db.throw_mysql_error()!
	}

	return true
}

// change_user changes the mysql user for the connection.
// Passing an empty string for the `dbname` parameter, resultsg in only changing
// the user and not changing the default database for the connection.
pub fn (mut db DB) change_user(username string, password string, dbname string) !bool {
	mut result := true

	if dbname != '' {
		result = C.mysql_change_user(db.conn, username.str, password.str, dbname.str)
	} else {
		result = C.mysql_change_user(db.conn, username.str, password.str, 0)
	}
	if !result {
		db.throw_mysql_error()!
	}

	return result
}

// affected_rows returns the number of rows changed, deleted,
// or inserted by the last statement if it was an `UPDATE`, `DELETE`, or `INSERT`.
pub fn (db &DB) affected_rows() u64 {
	return C.mysql_affected_rows(db.conn)
}

// autocommit turns on/off the auto-committing mode for the connection.
// When it is on, then each query is committed right away.
pub fn (mut db DB) autocommit(mode bool) ! {
	db.check_connection_is_established()!
	result := C.mysql_autocommit(db.conn, mode)

	if result != 0 {
		db.throw_mysql_error()!
	}
}

// commit commits the current transaction.
pub fn (mut db DB) commit() ! {
	db.check_connection_is_established()!
	result := C.mysql_commit(db.conn)

	if result != 0 {
		db.throw_mysql_error()!
	}
}

@[params]
pub struct MySQLTransactionParam {
	transaction_level MySQLTransactionLevel = .repeatable_read
}

// begin begins a new transaction.
pub fn (mut db DB) begin(param MySQLTransactionParam) ! {
	db.check_connection_is_established()!
	db.set_transaction_level(param.transaction_level)!
	result := db.exec_none('START TRANSACTION')
	if result != 0 {
		db.throw_mysql_error()!
	}
}

// set_transaction_level set level for the transaction
pub fn (mut db DB) set_transaction_level(level MySQLTransactionLevel) ! {
	db.check_connection_is_established()!
	mut sql_stmt := 'SET TRANSACTION ISOLATION LEVEL '
	match level {
		.read_uncommitted { sql_stmt += 'READ UNCOMMITTED' }
		.read_committed { sql_stmt += 'READ COMMITTED' }
		.repeatable_read { sql_stmt += 'REPEATABLE READ' }
		.serializable { sql_stmt += 'SERIALIZABLE' }
	}
	result := db.exec_none(sql_stmt)
	if result != 0 {
		db.throw_mysql_error()!
	}
}

// rollback rollbacks the current transaction.
pub fn (mut db DB) rollback() ! {
	db.check_connection_is_established()!
	result := C.mysql_rollback(db.conn)

	if result != 0 {
		db.throw_mysql_error()!
	}
}

// rollback_to rollbacks to a specified savepoint.
pub fn (mut db DB) rollback_to(savepoint string) ! {
	if !savepoint.is_identifier() {
		return error('savepoint should be a identifier string')
	}
	db.check_connection_is_established()!
	result := db.exec_none('ROLLBACK TO SAVEPOINT ${savepoint}')
	if result != 0 {
		db.throw_mysql_error()!
	}
}

// savepoint create a new savepoint.
pub fn (mut db DB) savepoint(savepoint string) ! {
	if !savepoint.is_identifier() {
		return error('savepoint should be a identifier string')
	}
	db.check_connection_is_established()!
	result := db.exec_none('SAVEPOINT ${savepoint}')
	if result != 0 {
		db.throw_mysql_error()!
	}
}

// tables returns a list of the names of the tables in the current database,
// that match the simple regular expression specified by the `wildcard` parameter.
// The `wildcard` parameter may contain the wildcard characters `%` or `_`.
// If an empty string is passed, it will return all tables.
// Calling `tables()` is similar to executing query `SHOW TABLES [LIKE wildcard]`.
pub fn (db &DB) tables(wildcard string) ![]string {
	c_mysql_result := C.mysql_list_tables(db.conn, wildcard.str)
	if isnil(c_mysql_result) {
		db.throw_mysql_error()!
	}

	result := Result{c_mysql_result}
	mut tables := []string{}

	for row in result.rows() {
		tables << row.vals[0]
	}

	return tables
}

// escape_string creates a legal SQL string for use in an SQL statement.
// The `s` argument is encoded to produce an escaped SQL string,
// taking into account the current character set of the connection.
pub fn (db &DB) escape_string(s string) string {
	unsafe {
		to := malloc_noscan(2 * s.len + 1)
		C.mysql_real_escape_string(db.conn, to, s.str, s.len)
		return to.vstring()
	}
}

// set_option sets extra connect options that affect the behavior of
// a connection. This function may be called multiple times to set several
// options. To retrieve the current values for an option, use `get_option()`.
pub fn (mut db DB) set_option(option_type int, val voidptr) {
	C.mysql_options(db.conn, option_type, val)
}

// get_option returns the value of an option, settable by `set_option`.
// https://dev.mysql.com/doc/c-api/5.7/en/mysql-get-option.html
pub fn (db &DB) get_option(option_type int) !voidptr {
	mysql_option := unsafe { nil }
	if C.mysql_get_option(db.conn, option_type, &mysql_option) != 0 {
		db.throw_mysql_error()!
	}

	return mysql_option
}

// refresh flush the tables or caches, or resets replication server
// information. The connected user must have the `RELOAD` privilege.
pub fn (mut db DB) refresh(options u32) !bool {
	if C.mysql_refresh(db.conn, options) != 0 {
		db.throw_mysql_error()!
	}

	return true
}

// reset resets the connection, and clear the session state.
pub fn (mut db DB) reset() ! {
	if C.mysql_reset_connection(db.conn) != 0 {
		db.throw_mysql_error()!
	}
}

// ping pings a server connection, or tries to reconnect if the connection
// has gone down.
pub fn (mut db DB) ping() !bool {
	if C.mysql_ping(db.conn) != 0 {
		db.throw_mysql_error()!
	}

	return true
}

// validate pings a server connection, or tries to reconnect if the connection
// has gone down.
pub fn (mut db DB) validate() !bool {
	return db.ping()!
}

// close closes the connection.
pub fn (mut db DB) close() ! {
	C.mysql_close(db.conn)
}

// info returns information about the most recently executed query.
// See more on https://dev.mysql.com/doc/c-api/8.0/en/mysql-info.html
pub fn (db &DB) info() string {
	return resolve_nil_str(C.mysql_info(db.conn))
}

// get_host_info returns a string describing the type of connection in use,
// including the server host name.
pub fn (db &DB) get_host_info() string {
	return unsafe { C.mysql_get_host_info(db.conn).vstring() }
}

// get_server_info returns a string representing the MySQL server version.
// For example, `8.0.24`.
pub fn (db &DB) get_server_info() string {
	return unsafe { C.mysql_get_server_info(db.conn).vstring() }
}

// get_server_version returns an integer, representing the MySQL server
// version. The value has the format `XYYZZ` where `X` is the major version,
// `YY` is the release level (or minor version), and `ZZ` is the sub-version
// within the release level. For example, `8.0.24` is returned as `80024`.
pub fn (db &DB) get_server_version() u64 {
	return C.mysql_get_server_version(db.conn)
}

// dump_debug_info instructs the server to write debugging information
// to the error log. The connected user must have the `SUPER` privilege.
pub fn (mut db DB) dump_debug_info() !bool {
	if C.mysql_dump_debug_info(db.conn) != 0 {
		db.throw_mysql_error()!
	}

	return true
}

// get_client_info returns client version information as a string.
pub fn get_client_info() string {
	return unsafe { C.mysql_get_client_info().vstring() }
}

// get_client_version returns the client version information as an integer.
pub fn get_client_version() u64 {
	return C.mysql_get_client_version()
}

// debug does a `DBUG_PUSH` with the given string.
// `debug()` uses the Fred Fish debug library.
// To use this function, you must compile the client library to support debugging.
// See https://dev.mysql.com/doc/c-api/8.0/en/mysql-debug.html
pub fn debug(debug string) {
	C.mysql_debug(debug.str)
}

// exec executes the `query` on the given `db`, and returns an array of all the results, or an error on failure
pub fn (db &DB) exec(query string) ![]Row {
	if C.mysql_query(db.conn, query.str) != 0 {
		db.throw_mysql_error()!
	}

	result := C.mysql_store_result(db.conn)
	if result == unsafe { nil } {
		return []Row{}
	} else {
		return Result{result}.rows()
	}
}

// exec_one executes the `query` on the given `db`, and returns either the first row from the result, if the query was successful, or an error
pub fn (db &DB) exec_one(query string) !Row {
	if C.mysql_query(db.conn, query.str) != 0 {
		db.throw_mysql_error()!
	}

	result := C.mysql_store_result(db.conn)

	if result == unsafe { nil } {
		db.throw_mysql_error()!
	}
	row_vals := C.mysql_fetch_row(result)
	num_cols := C.mysql_num_fields(result)

	if row_vals == unsafe { nil } {
		return Row{}
	}

	mut row := Row{}
	for i in 0 .. num_cols {
		if unsafe { row_vals[i] == nil } {
			row.vals << ''
		} else {
			row.vals << mystring(unsafe { &u8(row_vals[i]) })
		}
	}

	return row
}

// exec_none executes the `query` on the given `db`, and returns the integer MySQL result code
// Use it, in case you don't expect any row results, but still want a result code.
// e.g. for queries like these: INSERT INTO ... VALUES (...)
pub fn (db &DB) exec_none(query string) int {
	C.mysql_query(db.conn, query.str)

	return get_errno(db.conn)
}

// exec_param_many executes the `query` with parameters provided as `?`'s in the query
// It returns either the full result set, or an error on failure
pub fn (db &DB) exec_param_many(query string, params []string) ![]Row {
	stmt := db.prepare(query)!
	defer {
		stmt.close()
	}
	rows := stmt.execute(params)!
	return rows
}

// exec_param executes the `query` with one parameter provided as an `?` in the query
// It returns either the full result set, or an error on failure
pub fn (db &DB) exec_param(query string, param string) ![]Row {
	return db.exec_param_many(query, [param])!
}

// A StmtHandle is created through prepare, it will be bound
// to one DB connection and will become unusable if the connection
// is closed
pub struct StmtHandle {
	stmt &C.MYSQL_STMT = &C.MYSQL_STMT(unsafe { nil })
	db   DB
}

// prepare takes in a query string, returning a StmtHandle
// that can then be used to execute the query as many times
// as needed, which must be closed manually by the user
// Placeholders are represented by `?`
pub fn (db &DB) prepare(query string) !StmtHandle {
	stmt := C.mysql_stmt_init(db.conn)
	if stmt == unsafe { nil } {
		db.throw_mysql_error()!
	}

	mut code := C.mysql_stmt_prepare(stmt, query.str, query.len)
	if code != 0 {
		db.throw_mysql_error()!
	}

	return StmtHandle{
		stmt: stmt
		db:   DB{
			conn: db.conn
		}
	}
}

// execute takes in an array of params that will be bound to the statement,
// followed by it's execution
// Returns an array of Rows, which will be empty if nothing is returned
// from the query, or possibly an error value
pub fn (stmt &StmtHandle) execute(params []string) ![]Row {
	mut bind_params := []C.MYSQL_BIND{}
	for param in params {
		bind := C.MYSQL_BIND{
			buffer_type:   mysql_type_string
			buffer:        param.str
			buffer_length: u32(param.len)
			length:        0
			is_null:       0
		}
		bind_params << bind
	}

	mut response := C.mysql_stmt_bind_param(stmt.stmt, unsafe { &C.MYSQL_BIND(bind_params.data) })
	if response == true {
		stmt.db.throw_mysql_error()!
	}

	mut code := C.mysql_stmt_execute(stmt.stmt)
	if code != 0 {
		stmt.db.throw_mysql_error()!
	}

	query_metadata := C.mysql_stmt_result_metadata(stmt.stmt)
	// If the query returns no metadata we have no data to return
	// This happens in insert queries
	if query_metadata == unsafe { nil } {
		return []Row{}
	}
	num_cols := C.mysql_num_fields(query_metadata)
	mut length := []u32{len: num_cols}
	mut is_null := []bool{len: num_cols}

	mut binds := []C.MYSQL_BIND{}
	for i in 0 .. num_cols {
		bind := C.MYSQL_BIND{
			buffer_type:   mysql_type_string
			buffer:        0
			buffer_length: 0
			length:        unsafe { &length[i] }
			is_null:       unsafe { &is_null[i] }
		}
		binds << bind
	}

	mut rows := []Row{}
	response = C.mysql_stmt_bind_result(stmt.stmt, unsafe { &C.MYSQL_BIND(binds.data) })
	for {
		code = C.mysql_stmt_fetch(stmt.stmt)
		if code == mysql_no_data {
			break
		}
		lengths := length[0..num_cols].clone()
		mut row := Row{}
		for i in 0 .. num_cols {
			l := lengths[i]
			data := unsafe { malloc(l) }
			binds[i].buffer = data
			binds[i].buffer_length = l
			code = C.mysql_stmt_fetch_column(stmt.stmt, unsafe { &binds[i] }, i, 0)
			if *(binds[i].is_null) {
				row.vals << ''
			} else {
				row.vals << unsafe { data.vstring() }
			}
		}
		rows << row
	}
	return rows
}

// close acts on a StmtHandle to close the mysql Stmt
// meaning it is no longer available for use
pub fn (stmt &StmtHandle) close() {
	C.mysql_stmt_close(stmt.stmt)
}

@[inline]
fn (db &DB) throw_mysql_error() ! {
	return error_with_code(get_error_msg(db.conn), get_errno(db.conn))
}

@[inline]
fn (db &DB) check_connection_is_established() ! {
	if isnil(db.conn) {
		return error('No connection to a MySQL server, use `connect()` to connect to a database for working with it')
	}
}
