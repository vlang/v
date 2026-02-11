// BEAM Backend: PostgreSQL database module requires the epgsql Erlang driver for real functionality.
// BEAM: requires epgsql Erlang driver. See vbeam docs for setup.
//
// On BEAM, PostgreSQL connectivity maps to Erlang's epgsql library (https://github.com/epgsql/epgsql).
// Connection/query functions return errors directing users to the Erlang driver.
// Pure-V helper functions (as_structs, rows_first_or_empty, exec_param delegates) have real logic.
//
// To use PostgreSQL on BEAM:
//   1. Add epgsql as a dependency in your rebar3/mix project
//   2. Use Erlang interop: epgsql:connect/1, epgsql:squery/2, epgsql:equery/3, etc.
//   3. These V stubs ensure compilation compatibility but do not perform real I/O.
module pg

import orm

pub struct DB {
mut:
	conn voidptr = unsafe { nil }
}

pub struct Row {
pub mut:
	vals []?string
}

pub struct RowNoNull {
pub mut:
	vals []string
}

pub struct Result {
pub:
	cols map[string]int
	rows []Row
}

// Notification represents a notification received via LISTEN/NOTIFY.
// BEAM: On Erlang, use epgsql:listen/2 and handle {notification, Channel, Pid, Payload} messages.
pub struct Notification {
pub:
	channel string
	pid     int
	payload string
}

pub struct Config {
pub:
	host     string = 'localhost'
	port     int    = 5432
	user     string
	password string
	dbname   string
}

pub enum ConnStatusType {
	ok                = 0
	bad               = 1
	started           = 2
	made              = 3
	awaiting_response = 4
	auth_ok           = 5
	setenv            = 6
	ssl_startup       = 7
	needed            = 8
	check_writable    = 9
	consume           = 10
	gss_startup       = 11
}

pub enum ExecStatusType {
	empty_query    = 0
	command_ok     = 1
	tuples_ok      = 2
	copy_out       = 3
	copy_in        = 4
	bad_response   = 5
	nonfatal_error = 6
	fatal_error    = 7
	copy_both      = 8
	single_tuple   = 9
}

pub enum PQTransactionLevel {
	read_uncommitted
	read_committed
	repeatable_read
	serializable
}

@[params]
pub struct PQTransactionParam {
	transaction_level PQTransactionLevel = .repeatable_read
}

// connect makes a new connection to the database server.
// BEAM: requires epgsql driver. Use epgsql:connect/1 via Erlang interop.
pub fn connect(config Config) !DB {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver. Use epgsql:connect/1 via Erlang interop.')
}

// connect_with_conninfo makes a new connection using a connection string.
// BEAM: requires epgsql driver. Use epgsql:connect/1 with parsed options.
pub fn connect_with_conninfo(conninfo string) !DB {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver. Use epgsql:connect/1 via Erlang interop.')
}

// res_to_rows converts a raw result pointer to rows.
// BEAM: returns empty since no native result set exists.
fn res_to_rows(res voidptr) []Row {
	return []Row{}
}

// res_to_rows_no_null converts a raw result pointer to non-nullable rows.
// BEAM: returns empty since no native result set exists.
fn res_to_rows_no_null(res voidptr) []RowNoNull {
	return []RowNoNull{}
}

// res_to_result converts a raw result pointer to a Result struct.
// BEAM: returns empty since no native result set exists.
fn res_to_result(res voidptr) Result {
	return Result{}
}

// close frees the underlying resource.
// BEAM: no-op. With epgsql, use epgsql:close/1 to close connections.
pub fn (db &DB) close() ! {
	// BEAM: no-op. Use epgsql:close/1 to close the connection.
}

// q_int returns the first field as int.
// BEAM: requires epgsql driver.
pub fn (db &DB) q_int(query string) !int {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver.')
}

// q_string returns the first field as string.
// BEAM: requires epgsql driver.
pub fn (db &DB) q_string(query string) !string {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver.')
}

// q_strings returns the resulting row set.
// BEAM: requires epgsql driver.
pub fn (db &DB) q_strings(query string) ![]Row {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver.')
}

// exec submits a command and waits for the result.
// BEAM: requires epgsql driver. Use epgsql:squery/2 for simple queries.
pub fn (db &DB) exec(query string) ![]Row {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver. Use epgsql:squery/2.')
}

// exec_no_null works like exec, but fields can't be NULL.
// BEAM: requires epgsql driver.
pub fn (db &DB) exec_no_null(query string) ![]RowNoNull {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver.')
}

// exec_result returns a Result set.
// BEAM: requires epgsql driver.
pub fn (db &DB) exec_result(query string) !Result {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver.')
}

// rows_first_or_empty returns the first row or an error if empty.
// Pure V logic - works on all backends.
fn rows_first_or_empty(rows []Row) !Row {
	if rows.len == 0 {
		return error('no row')
	}
	return rows[0]
}

// exec_one executes a query and returns the first row.
// BEAM: requires epgsql driver.
pub fn (db &DB) exec_one(query string) !Row {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver.')
}

// exec_param_many executes a query with parameters.
// BEAM: requires epgsql driver. Use epgsql:equery/3 for parameterized queries.
pub fn (db &DB) exec_param_many(query string, params []string) ![]Row {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver. Use epgsql:equery/3.')
}

// exec_param_many_result executes a query with parameters and returns a Result.
// BEAM: requires epgsql driver.
pub fn (db &DB) exec_param_many_result(query string, params []string) !Result {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver.')
}

// exec_param executes a query with 1 parameter.
// Pure V logic - delegates to exec_param_many.
pub fn (db &DB) exec_param(query string, param string) ![]Row {
	return db.exec_param_many(query, [param])
}

// exec_param2 executes a query with 2 parameters.
// Pure V logic - delegates to exec_param_many.
pub fn (db &DB) exec_param2(query string, param string, param2 string) ![]Row {
	return db.exec_param_many(query, [param, param2])
}

// prepare submits a request to create a prepared statement.
// BEAM: requires epgsql driver. Use epgsql:parse/3.
pub fn (db &DB) prepare(name string, query string, num_params int) ! {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver. Use epgsql:parse/3.')
}

// exec_prepared sends a request to execute a prepared statement.
// BEAM: requires epgsql driver. Use epgsql:prepared_query/3.
pub fn (db &DB) exec_prepared(name string, params []string) ![]Row {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver. Use epgsql:prepared_query/3.')
}

// exec_prepared_result executes a prepared statement and returns Result.
// BEAM: requires epgsql driver.
pub fn (db &DB) exec_prepared_result(name string, params []string) !Result {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver.')
}

// handle_error_or_rows processes a query result and returns rows or an error.
// BEAM: requires epgsql driver.
fn (db &DB) handle_error_or_rows(res voidptr, elabel string) ![]Row {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver.')
}

// handle_error_or_rows_no_null processes a query result (non-nullable).
// BEAM: requires epgsql driver.
fn (db &DB) handle_error_or_rows_no_null(res voidptr, elabel string) ![]RowNoNull {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver.')
}

// handle_error_or_result processes a query result as a Result set.
// BEAM: requires epgsql driver.
fn (db &DB) handle_error_or_result(res voidptr, elabel string) !Result {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver.')
}

// handle_error checks a result for errors.
// BEAM: requires epgsql driver.
fn (db &DB) handle_error(res voidptr, elabel string) ! {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver.')
}

// copy_expert executes COPY command.
// BEAM: requires epgsql driver. PostgreSQL COPY is supported by epgsql.
pub fn (db &DB) copy_expert(query string, mut file voidptr) !int {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver.')
}

// begin begins a new transaction.
// BEAM: requires epgsql driver. Use epgsql:squery(C, "BEGIN").
pub fn (db &DB) begin(param PQTransactionParam) ! {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver. Use epgsql:squery(C, "BEGIN").')
}

// commit commits the current transaction.
// BEAM: requires epgsql driver. Use epgsql:squery(C, "COMMIT").
pub fn (db &DB) commit() ! {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver.')
}

// rollback rollbacks the current transaction.
// BEAM: requires epgsql driver.
pub fn (db &DB) rollback() ! {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver.')
}

// rollback_to rollbacks to a specified savepoint.
// BEAM: requires epgsql driver.
pub fn (db &DB) rollback_to(savepoint string) ! {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver.')
}

// savepoint creates a new savepoint.
// BEAM: requires epgsql driver.
pub fn (db &DB) savepoint(savepoint string) ! {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver.')
}

// validate checks if the connection is still usable.
// BEAM: requires epgsql driver. Use epgsql:squery(C, "SELECT 1") as health check.
pub fn (db &DB) validate() !bool {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver.')
}

// reset returns the connection to initial state for reuse.
// BEAM: no-op. epgsql connections reset automatically between queries.
pub fn (db &DB) reset() ! {
	// BEAM: no-op. epgsql handles connection state internally.
}

// as_structs maps the results' rows based on the provided mapping function.
// Pure V logic - works on all backends when Result has data.
pub fn (res Result) as_structs[T](mapper fn (Result, Row) !T) ![]T {
	mut typed := []T{}
	for r in res.rows {
		typed << mapper(res, r)!
	}
	return typed
}

// listen registers the connection to receive notifications.
// BEAM: requires epgsql driver. Use epgsql:squery(C, "LISTEN channel").
pub fn (db &DB) listen(channel string) ! {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver. Use epgsql:squery(C, "LISTEN ${channel}").')
}

// unlisten unregisters the connection from notifications.
// BEAM: requires epgsql driver.
pub fn (db &DB) unlisten(channel string) ! {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver.')
}

// unlisten_all unregisters from all notification channels.
// BEAM: requires epgsql driver.
pub fn (db &DB) unlisten_all() ! {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver.')
}

// notify sends a notification on the specified channel.
// BEAM: requires epgsql driver. Use epgsql:squery(C, "NOTIFY channel, payload").
pub fn (db &DB) notify(channel string, payload string) ! {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver.')
}

// consume_input reads any available input from the server.
// BEAM: requires epgsql driver. epgsql handles this internally via gen_tcp.
pub fn (db &DB) consume_input() !bool {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver.')
}

// get_notification returns the next pending notification.
// BEAM: requires epgsql driver. Returns none (no connection).
pub fn (db &DB) get_notification() ?Notification {
	return none
}

// socket returns the file descriptor of the connection socket.
// BEAM: returns -1 since BEAM manages sockets internally via gen_tcp.
pub fn (db &DB) socket() int {
	// BEAM: socket FDs are managed by the BEAM VM, not exposed to V code.
	return -1
}

// pg_stmt_worker executes a parameterized query for the ORM layer.
// BEAM: requires epgsql driver.
fn pg_stmt_worker(db &DB, query string, data orm.QueryData, where orm.QueryData) ![]Row {
	return error('BEAM: PostgreSQL requires epgsql Erlang driver.')
}
