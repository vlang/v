// BEAM Backend: MySQL database module requires an Erlang MySQL driver for real functionality.
// BEAM: requires mysql-otp Erlang driver. See vbeam docs for setup.
//
// On BEAM, MySQL connectivity maps to Erlang's mysql-otp library (https://github.com/mysql-otp/mysql-otp).
// Connection functions return errors directing users to the Erlang driver.
// Pure-V helper functions (bind_*, result parsing, struct methods) are implemented with real logic.
// All connection/query functions return descriptive errors since they require the native driver.
//
// To use MySQL on BEAM:
//   1. Add mysql-otp as a dependency in your rebar3/mix project
//   2. Use Erlang interop: mysql:start_link/1, mysql:query/2, etc.
//   3. These V stubs ensure compilation compatibility but do not perform real I/O.
module mysql

// MySQL refresh flags (from consts.c.v)
pub const refresh_grant = u32(0x1)
pub const refresh_log = u32(0x2)
pub const refresh_tables = u32(0x4)
pub const refresh_hosts = u32(0x8)
pub const refresh_status = u32(0x10)
pub const refresh_threads = u32(0x20)
pub const refresh_replica = u32(0x40)
pub const refresh_slave = u32(0x40)
pub const refresh_master = u32(0x80)
pub const refresh_error_log = u32(0x100)
pub const refresh_engine_log = u32(0x200)
pub const refresh_binary_log = u32(0x400)
pub const refresh_relay_log = u32(0x800)
pub const refresh_general_log = u32(0x1000)
pub const refresh_slow_log = u32(0x2000)
pub const refresh_read_lock = u32(0x4000)
pub const refresh_fast = u32(0x8000)
pub const refresh_user_resources = u32(0x80000)
pub const refresh_for_export = u32(0x100000)
pub const refresh_optimizer_costs = u32(0x200000)
pub const refresh_persist = u32(0x400000)

// mysql type constants (from stmt.c.v)
const mysql_type_decimal = 0
const mysql_type_tiny = 1
const mysql_type_short = 2
const mysql_type_long = 3
const mysql_type_float = 4
const mysql_type_double = 5
const mysql_type_null = 6
const mysql_type_timestamp = 7
const mysql_type_longlong = 8
const mysql_type_int24 = 9
const mysql_type_date = 10
const mysql_type_time = 11
const mysql_type_datetime = 12
const mysql_type_year = 13
const mysql_type_varchar = 15
const mysql_type_bit = 16
const mysql_type_timestamp22 = 7
const mysql_type_json = 245
const mysql_type_newdecimal = 246
const mysql_type_enum = 247
const mysql_type_set = 248
const mysql_type_tiny_blob = 249
const mysql_type_medium_blob = 250
const mysql_type_long_blob = 251
const mysql_type_blob = 252
const mysql_type_var_string = 253
const mysql_type_string = 254
const mysql_type_geometry = 255
const mysql_no_data = 100

// ConnectionFlag - capabilities flag bitmask for MySQL protocol.
@[flag]
pub enum ConnectionFlag {
	client_long_password
	client_found_rows
	client_long_flag
	client_connect_with_db
	client_no_schema
	client_compress
	client_odbc
	client_local_files
	client_ignore_space
	client_protocol_41
	client_interactive
	client_ssl
	client_ignore_sigpipe
	client_transactions
	client_reserved
	client_reserved2
	client_multi_statements
	client_multi_results
	client_ps_multi_results
	client_plugin_auth
	client_connect_attrs
	client_plugin_auth_lenenc_client_data
	client_can_handle_expired_passwords
	client_session_track
	client_deprecate_eof
	client_optional_resultset_metadata
	client_zstd_compression_algorithm
	client_query_attributes
	multi_factor_authentication
	client_capability_extension
	client_ssl_verify_server_cert
	client_remember_options
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
	conn voidptr = unsafe { nil }
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

	ssl_key    string
	ssl_cert   string
	ssl_ca     string
	ssl_capath string
	ssl_cipher string
}

pub struct Result {
pub:
	result voidptr = unsafe { nil }
}

pub struct Row {
pub mut:
	vals []string
}

pub struct Field {
	name             string
	org_name         string
	table            string
	org_table        string
	db               string
	catalog          string
	def              string
	length           int
	max_length       int
	name_length      u32
	org_name_length  u32
	table_length     u32
	org_table_length u32
	db_length        u32
	catalog_length   u32
	def_length       u32
	flags            u32
	decimals         u32
	charsetnr        u32
	@type            FieldType
}

// --- Result methods ---

// fetch_row fetches the next row from a result.
// BEAM: requires mysql-otp driver. Returns nil as no connection exists.
pub fn (r Result) fetch_row() &charptr {
	return unsafe { nil }
}

// n_rows returns the number of rows from a result.
// BEAM: requires mysql-otp driver. Returns 0 as no result set exists.
pub fn (r Result) n_rows() u64 {
	return 0
}

// n_fields returns the number of columns from a result.
// BEAM: requires mysql-otp driver. Returns 0 as no result set exists.
pub fn (r Result) n_fields() int {
	return 0
}

// rows returns array of rows.
// BEAM: requires mysql-otp driver. Returns empty array.
pub fn (r Result) rows() []Row {
	return []Row{}
}

// maps returns an array of maps.
// BEAM: requires mysql-otp driver. Returns empty array.
pub fn (r Result) maps() []map[string]string {
	return []map[string]string{}
}

// fields returns an array of fields/columns.
// BEAM: requires mysql-otp driver. Returns empty array.
pub fn (r Result) fields() []Field {
	return []Field{}
}

// str serializes the field. Pure V logic - works on all backends.
pub fn (f Field) str() string {
	return '{ name: "${f.name}" }'
}

// free frees the memory used by a result.
// BEAM: no-op since BEAM VM handles garbage collection.
@[unsafe]
pub fn (r &Result) free() {
}

// --- utils ---

// get_error_msg returns the error message from a MySQL connection.
// BEAM: requires mysql-otp driver. Returns descriptive stub message.
fn get_error_msg(conn voidptr) string {
	return 'BEAM: MySQL driver not available. Use mysql-otp Erlang driver for real connectivity.'
}

// get_errno returns the error number from a MySQL connection.
// BEAM: requires mysql-otp driver. Returns -1 (no connection).
fn get_errno(conn voidptr) int {
	return -1
}

// resolve_nil_str safely converts a nullable byte pointer to a string.
// BEAM: C pointer operations not available; returns empty string for nil.
fn resolve_nil_str(ptr &u8) string {
	if isnil(ptr) {
		return ''
	}
	// BEAM: C-level vstring()/vstrlen() not available. Return empty string.
	return ''
}

// mystring converts a byte pointer to a V string.
// BEAM: C pointer operations not available; returns empty string.
@[inline]
fn mystring(b &u8) string {
	// BEAM: C-level vstring()/vstrlen() not available. Return empty string.
	return ''
}

// --- DB methods ---

// connect attempts to establish a connection to a MySQL server.
// BEAM: requires mysql-otp Erlang driver. Use mysql:start_link(Opts) in Erlang interop.
pub fn connect(config Config) !DB {
	return error('BEAM: MySQL requires mysql-otp Erlang driver. Use mysql:start_link/1 via Erlang interop.')
}

// query executes the SQL statement.
// BEAM: requires mysql-otp driver. Use mysql:query/2 in Erlang interop.
pub fn (db &DB) query(q string) !Result {
	return error('BEAM: MySQL requires mysql-otp Erlang driver. Use mysql:query/2 via Erlang interop.')
}

// use_result reads the result of a query.
// BEAM: no-op since mysql-otp handles result sets differently (no separate fetch step).
pub fn (db &DB) use_result() {
}

// real_query makes an SQL query with binary data support.
// BEAM: requires mysql-otp driver. Use mysql:query/3 with binary params.
pub fn (mut db DB) real_query(q string) !Result {
	return error('BEAM: MySQL requires mysql-otp Erlang driver. Use mysql:query/3 via Erlang interop.')
}

// select_db changes the default database.
// BEAM: requires mysql-otp driver. Use mysql:query(Conn, "USE dbname").
pub fn (mut db DB) select_db(dbname string) !bool {
	return error('BEAM: MySQL requires mysql-otp Erlang driver. Execute "USE ${dbname}" via mysql:query/2.')
}

// change_user changes the mysql user for the connection.
// BEAM: requires mysql-otp driver. Use mysql:change_user/3.
pub fn (mut db DB) change_user(username string, password string, dbname string) !bool {
	return error('BEAM: MySQL requires mysql-otp Erlang driver. Use mysql:change_user/3.')
}

// affected_rows returns the number of rows changed.
// BEAM: requires mysql-otp driver. Returns 0 (no connection).
pub fn (db &DB) affected_rows() u64 {
	return 0
}

// autocommit turns on/off the auto-committing mode.
// BEAM: requires mysql-otp driver. Use mysql:query(Conn, "SET autocommit=N").
pub fn (mut db DB) autocommit(mode bool) ! {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}

// commit commits the current transaction.
// BEAM: requires mysql-otp driver. Use mysql:query(Conn, "COMMIT").
pub fn (mut db DB) commit() ! {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}

@[params]
pub struct MySQLTransactionParam {
	transaction_level MySQLTransactionLevel = .repeatable_read
}

// begin begins a new transaction.
// BEAM: requires mysql-otp driver. Use mysql:transaction/2.
pub fn (mut db DB) begin(param MySQLTransactionParam) ! {
	return error('BEAM: MySQL requires mysql-otp Erlang driver. Use mysql:transaction/2.')
}

// set_transaction_level sets level for the transaction.
// BEAM: requires mysql-otp driver.
pub fn (mut db DB) set_transaction_level(level MySQLTransactionLevel) ! {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}

// rollback rollbacks the current transaction.
// BEAM: requires mysql-otp driver.
pub fn (mut db DB) rollback() ! {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}

// rollback_to rollbacks to a specified savepoint.
// BEAM: requires mysql-otp driver.
pub fn (mut db DB) rollback_to(savepoint string) ! {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}

// savepoint creates a new savepoint.
// BEAM: requires mysql-otp driver.
pub fn (mut db DB) savepoint(savepoint string) ! {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}

// tables returns a list of the names of the tables.
// BEAM: requires mysql-otp driver.
pub fn (db &DB) tables(wildcard string) ![]string {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}

// escape_string creates a legal SQL string for use in an SQL statement.
// Pure V implementation - escapes special MySQL characters.
pub fn (db &DB) escape_string(s string) string {
	// Pure-V MySQL escape: replace dangerous characters
	mut result := s
	result = result.replace('\\', '\\\\')
	result = result.replace("'", "\\'")
	result = result.replace('"', '\\"')
	result = result.replace('\n', '\\n')
	result = result.replace('\r', '\\r')
	result = result.replace('\x00', '\\0')
	result = result.replace('\x1a', '\\Z')
	return result
}

// set_option sets extra connect options.
// BEAM: no-op since mysql-otp uses Erlang option tuples at connection time.
pub fn (mut db DB) set_option(option_type int, val voidptr) {
	// BEAM: no-op. mysql-otp options are set at mysql:start_link/1 time.
}

// get_option returns the value of an option.
// BEAM: requires mysql-otp driver.
pub fn (db &DB) get_option(option_type int) !voidptr {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}

// refresh flushes the tables or caches.
// BEAM: requires mysql-otp driver.
pub fn (mut db DB) refresh(options u32) !bool {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}

// reset resets the connection.
// BEAM: requires mysql-otp driver.
pub fn (mut db DB) reset() ! {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}

// ping pings a server connection.
// BEAM: requires mysql-otp driver.
pub fn (mut db DB) ping() !bool {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}

// validate pings a server connection.
// BEAM: requires mysql-otp driver.
pub fn (mut db DB) validate() !bool {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}

// close closes the connection.
// BEAM: no-op since no real connection is opened. mysql-otp uses mysql:stop/1.
pub fn (mut db DB) close() ! {
	// BEAM: no-op. With mysql-otp, use mysql:stop/1 to close.
}

// info returns information about the most recently executed query.
// BEAM: requires mysql-otp driver. Returns empty string.
pub fn (db &DB) info() string {
	return ''
}

// get_host_info returns a string describing the connection type.
// BEAM: returns backend identifier since no real connection exists.
pub fn (db &DB) get_host_info() string {
	return 'BEAM backend (no native MySQL driver - use mysql-otp)'
}

// get_server_info returns the MySQL server version string.
// BEAM: requires mysql-otp driver. Returns placeholder.
pub fn (db &DB) get_server_info() string {
	return '0.0.0'
}

// get_server_version returns the MySQL server version as integer.
// BEAM: requires mysql-otp driver. Returns 0.
pub fn (db &DB) get_server_version() u64 {
	return 0
}

// dump_debug_info instructs the server to write debugging info.
// BEAM: requires mysql-otp driver.
pub fn (mut db DB) dump_debug_info() !bool {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}

// get_client_info returns client version information as a string.
// BEAM: returns backend identifier.
pub fn get_client_info() string {
	return 'vbeam-mysql-stub (use mysql-otp for real driver)'
}

// get_client_version returns the client version information as an integer.
// BEAM: returns 0 since no native client library is linked.
pub fn get_client_version() u64 {
	return 0
}

// debug does a DBUG_PUSH with the given string.
// BEAM: no-op since DBUG is a C-level MySQL debugging facility.
pub fn debug(dbg string) {
	// BEAM: no-op. C-level MySQL DBUG not available.
}

// exec executes the query and returns all results.
// BEAM: requires mysql-otp driver.
pub fn (db &DB) exec(query string) ![]Row {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}

// exec_one executes the query and returns the first row.
// BEAM: requires mysql-otp driver.
pub fn (db &DB) exec_one(query string) !Row {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}

// exec_none executes the query and returns integer result code.
// BEAM: requires mysql-otp driver. Returns -1 (no connection).
pub fn (db &DB) exec_none(query string) int {
	return -1
}

// exec_param_many executes the query with parameters.
// BEAM: requires mysql-otp driver. Use mysql:query/3 with param list.
pub fn (db &DB) exec_param_many(query string, params []string) ![]Row {
	return error('BEAM: MySQL requires mysql-otp Erlang driver. Use mysql:query/3.')
}

// exec_param executes the query with one parameter.
// BEAM: requires mysql-otp driver.
pub fn (db &DB) exec_param(query string, param string) ![]Row {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}

// StmtHandle is created through prepare.
pub struct StmtHandle {
	stmt voidptr = unsafe { nil }
	db   DB
}

// prepare takes in a query string, returning a StmtHandle.
// BEAM: requires mysql-otp driver. Use mysql:prepare/3.
pub fn (db &DB) prepare(query string) !StmtHandle {
	return error('BEAM: MySQL requires mysql-otp Erlang driver. Use mysql:prepare/3.')
}

// execute takes in an array of params and executes the statement.
// BEAM: requires mysql-otp driver. Use mysql:execute/3.
pub fn (stmt &StmtHandle) execute(params []string) ![]Row {
	return error('BEAM: MySQL requires mysql-otp Erlang driver. Use mysql:execute/3.')
}

// close closes the StmtHandle.
// BEAM: no-op. With mysql-otp, use mysql:unprepare/2.
pub fn (stmt &StmtHandle) close() {
	// BEAM: no-op. mysql-otp prepared statements are closed via mysql:unprepare/2.
}

// throw_mysql_error throws a MySQL error.
// BEAM: returns descriptive error about missing driver.
@[inline]
fn (db &DB) throw_mysql_error() ! {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}

// check_connection_is_established verifies the connection is active.
// BEAM: always returns error since no native connection exists.
@[inline]
fn (db &DB) check_connection_is_established() ! {
	return error('BEAM: No MySQL connection. Use mysql-otp Erlang driver.')
}

// --- Stmt (from stmt.c.v) ---

// BEAM stub for MYSQL_BIND - stores bind metadata for prepared statements.
struct MysqlBind {
mut:
	buffer_type   int
	buffer        voidptr
	buffer_length u32
	length        &u32   = unsafe { nil }
	is_null       &bool  = unsafe { nil }
}

pub struct Stmt {
	stmt  voidptr = unsafe { nil }
	query string
mut:
	binds []MysqlBind
	res   []MysqlBind
}

// str returns a text representation of the statement. Pure V logic.
pub fn (s &Stmt) str() string {
	return 'mysql.Stmt{ query: `${s.query}`, binds.len: ${s.binds.len} }'
}

// init_stmt creates a new statement. Pure V logic - initializes struct.
pub fn (db DB) init_stmt(query string) Stmt {
	return Stmt{
		query: query
	}
}

// prepare prepares a statement for execution.
// BEAM: requires mysql-otp driver.
pub fn (stmt Stmt) prepare() ! {
	return error('BEAM: MySQL requires mysql-otp Erlang driver. Use mysql:prepare/3.')
}

// bind_params binds all the parameters.
// BEAM: requires mysql-otp driver.
pub fn (stmt Stmt) bind_params() ! {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}

// execute executes the statement.
// BEAM: requires mysql-otp driver.
pub fn (stmt Stmt) execute() !int {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}

// next retrieves the next available result.
// BEAM: requires mysql-otp driver.
pub fn (stmt Stmt) next() !int {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}

// gen_metadata executes mysql_stmt_result_metadata.
// BEAM: requires mysql-otp driver. Returns nil.
pub fn (stmt Stmt) gen_metadata() voidptr {
	return unsafe { nil }
}

// fetch_fields retrieves the fields from the metadata result.
// BEAM: requires mysql-otp driver. Returns nil.
pub fn (stmt Stmt) fetch_fields(res voidptr) voidptr {
	return unsafe { nil }
}

// fetch_stmt fetches the next row in the result set.
// BEAM: requires mysql-otp driver.
pub fn (stmt Stmt) fetch_stmt() !int {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}

// close disposes the prepared statement.
// BEAM: requires mysql-otp driver. Use mysql:unprepare/2.
pub fn (stmt Stmt) close() ! {
	return error('BEAM: MySQL requires mysql-otp Erlang driver. Use mysql:unprepare/2.')
}

// get_error_msg returns the error message for this statement.
// BEAM: returns descriptive message about missing driver.
fn (stmt Stmt) get_error_msg() string {
	return 'BEAM: MySQL driver not available. Use mysql-otp Erlang driver.'
}

// error returns a proper V error with the given code.
// Pure V logic - constructs error from available information.
pub fn (stmt Stmt) error(code int) IError {
	return &SQLError{
		msg:  'BEAM: MySQL not available (code ${code}) (query: ${stmt.query}). Use mysql-otp Erlang driver.'
		code: code
	}
}

// get_field_count returns the number of fields in the result.
// BEAM: requires mysql-otp driver. Returns 0.
fn (stmt Stmt) get_field_count() u16 {
	return 0
}

// bind_bool binds a single boolean value. Pure V logic - appends to bind list.
pub fn (mut stmt Stmt) bind_bool(b &bool) {
	stmt.bind(mysql_type_tiny, b, 0)
}

// bind_byte binds a single byte value. Pure V logic - appends to bind list.
pub fn (mut stmt Stmt) bind_byte(b &u8) {
	stmt.bind(mysql_type_tiny, b, 0)
}

// bind_u8 binds a single u8 value. Pure V logic - appends to bind list.
pub fn (mut stmt Stmt) bind_u8(b &u8) {
	stmt.bind(mysql_type_tiny, b, 0)
}

// bind_i8 binds a single i8 value. Pure V logic - appends to bind list.
pub fn (mut stmt Stmt) bind_i8(b &i8) {
	stmt.bind(mysql_type_tiny, b, 0)
}

// bind_i16 binds a single i16 value. Pure V logic - appends to bind list.
pub fn (mut stmt Stmt) bind_i16(b &i16) {
	stmt.bind(mysql_type_short, b, 0)
}

// bind_u16 binds a single u16 value. Pure V logic - appends to bind list.
pub fn (mut stmt Stmt) bind_u16(b &u16) {
	stmt.bind(mysql_type_short, b, 0)
}

// bind_int binds a single int value. Pure V logic - appends to bind list.
pub fn (mut stmt Stmt) bind_int(b &int) {
	stmt.bind(mysql_type_long, b, 0)
}

// bind_u32 binds a single u32 value. Pure V logic - appends to bind list.
pub fn (mut stmt Stmt) bind_u32(b &u32) {
	stmt.bind(mysql_type_long, b, 0)
}

// bind_i64 binds a single i64 value. Pure V logic - appends to bind list.
pub fn (mut stmt Stmt) bind_i64(b &i64) {
	stmt.bind(mysql_type_longlong, b, 0)
}

// bind_u64 binds a single u64 value. Pure V logic - appends to bind list.
pub fn (mut stmt Stmt) bind_u64(b &u64) {
	stmt.bind(mysql_type_longlong, b, 0)
}

// bind_f32 binds a single f32 value. Pure V logic - appends to bind list.
pub fn (mut stmt Stmt) bind_f32(b &f32) {
	stmt.bind(mysql_type_float, b, 0)
}

// bind_f64 binds a single f64 value. Pure V logic - appends to bind list.
pub fn (mut stmt Stmt) bind_f64(b &f64) {
	stmt.bind(mysql_type_double, b, 0)
}

// bind_text binds a single string value. Pure V logic - appends to bind list.
pub fn (mut stmt Stmt) bind_text(b string) {
	stmt.bind(mysql_type_string, b.str, u32(b.len))
}

// bind_null binds a single NULL value. Pure V logic - appends null bind entry.
pub fn (mut stmt Stmt) bind_null() {
	stmt.binds << MysqlBind{
		buffer_type: mysql_type_null
	}
}

// bind binds a single value to the statement. Pure V logic - stores bind metadata.
pub fn (mut stmt Stmt) bind(typ int, buffer voidptr, buf_len u32) {
	stmt.binds << MysqlBind{
		buffer_type:   typ
		buffer:        buffer
		buffer_length: buf_len
	}
}

// bind_res stores one result in the statement.
// BEAM: no-op since result binding requires C-level mysql_stmt_bind_result.
pub fn (mut stmt Stmt) bind_res(fields voidptr, dataptr []&u8, lengths []u32, is_null []bool, num_fields int) {
	// BEAM: no-op. mysql-otp returns results as Erlang terms directly.
}

// bind_result_buffer binds one result value.
// BEAM: requires mysql-otp driver.
pub fn (mut stmt Stmt) bind_result_buffer() ! {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}

// store_result buffers the complete result set.
// BEAM: requires mysql-otp driver. mysql-otp returns full result sets by default.
pub fn (mut stmt Stmt) store_result() ! {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}

// fetch_column fetches one column from the current result set row.
// BEAM: requires mysql-otp driver.
pub fn (mut stmt Stmt) fetch_column(bind &MysqlBind, column int) ! {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}
