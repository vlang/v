// BEAM Backend: SQLite database module requires the esqlite3 Erlang driver for real functionality.
// BEAM: requires esqlite3 Erlang driver. See vbeam docs for setup.
//
// On BEAM, SQLite connectivity maps to Erlang's esqlite3 library (https://github.com/mmzeeman/esqlite).
// Connection/query functions return errors directing users to the Erlang driver.
// Pure-V helper functions (error_message, exec_param delegate) have real logic.
//
// To use SQLite on BEAM:
//   1. Add esqlite3 as a dependency in your rebar3/mix project
//   2. Use Erlang interop: esqlite3:open/1, esqlite3:exec/2, esqlite3:q/2, etc.
//   3. These V stubs ensure compilation compatibility but do not perform real I/O.
module sqlite

// https://www.sqlite.org/rescode.html
pub const sqlite_ok = 0
pub const sqlite_error = 1
pub const sqlite_row = 100
pub const sqlite_done = 101
pub const sqlite_cantopen = 14
pub const sqlite_ioerr_read = 266
pub const sqlite_ioerr_short_read = 522
pub const sqlite_ioerr_write = 778
pub const sqlite_ioerr_fsync = 1034
pub const sqlite_ioerr_fstat = 1802
pub const sqlite_ioerr_delete = 2570

pub const sqlite_open_main_db = 0x00000100
pub const sqlite_open_temp_db = 0x00000200
pub const sqlite_open_transient_db = 0x00000400
pub const sqlite_open_main_journal = 0x00000800
pub const sqlite_open_temp_journal = 0x00001000
pub const sqlite_open_subjournal = 0x00002000
pub const sqlite_open_super_journal = 0x00004000
pub const sqlite_open_wal = 0x00080000

pub enum SyncMode {
	off
	normal
	full
}

pub enum Sqlite3TransactionLevel {
	deferred
	immediate
	exclusive
}

pub enum JournalMode {
	off
	delete
	truncate
	persist
	memory
}

struct SQLError {
	MessageError
}

// Stmt represents a prepared SQL statement.
// BEAM: requires esqlite3 driver for real functionality.
@[heap]
pub struct Stmt {
	stmt voidptr = unsafe { nil }
	db   &DB     = unsafe { nil }
}

// DB represents a database connection.
// BEAM: requires esqlite3 driver for real connectivity.
@[heap]
pub struct DB {
pub mut:
	is_open bool
mut:
	conn voidptr = unsafe { nil }
}

// str returns a text representation of the DB. Pure V logic.
pub fn (db &DB) str() string {
	return 'sqlite.DB{ is_open: ${db.is_open}, backend: beam_stub }'
}

pub struct Row {
pub mut:
	vals []string
}

pub type Params = []string | [][]string

// NOTE: is_error() is already defined in result_code.v - do NOT redefine here.

// connect opens the connection with a database.
// BEAM: requires esqlite3 driver. Use esqlite3:open/1 via Erlang interop.
pub fn connect(path string) !DB {
	return error('BEAM: SQLite requires esqlite3 Erlang driver. Use esqlite3:open("${path}") via Erlang interop.')
}

// close closes the DB.
// BEAM: sets is_open to false. With esqlite3, use esqlite3:close/1.
pub fn (mut db DB) close() ! {
	db.is_open = false
}

// get_int_from_stmt extracts an integer from a statement (ORM internal).
// BEAM: requires esqlite3 driver. Returns 0.
fn get_int_from_stmt(stmt voidptr) int {
	// BEAM: requires esqlite3 driver. Returns 0 (no statement handle).
	return 0
}

// last_insert_rowid returns last inserted rowid.
// BEAM: requires esqlite3 driver. Returns 0.
pub fn (db &DB) last_insert_rowid() i64 {
	// BEAM: requires esqlite3 driver. Use esqlite3:last_insert_rowid/1.
	return 0
}

// get_affected_rows_count returns amount of rows affected by most recent query.
// BEAM: requires esqlite3 driver. Returns 0.
pub fn (db &DB) get_affected_rows_count() int {
	// BEAM: requires esqlite3 driver. Use esqlite3:changes/1.
	return 0
}

// q_int returns a single integer value from a query.
// BEAM: requires esqlite3 driver.
pub fn (db &DB) q_int(query string) !int {
	return error('BEAM: SQLite requires esqlite3 Erlang driver.')
}

// q_string returns a single string value from a query.
// BEAM: requires esqlite3 driver.
pub fn (db &DB) q_string(query string) !string {
	return error('BEAM: SQLite requires esqlite3 Erlang driver.')
}

// exec_map executes the query and returns an array of maps.
// BEAM: requires esqlite3 driver.
@[manualfree]
pub fn (db &DB) exec_map(query string) ![]map[string]string {
	return error('BEAM: SQLite requires esqlite3 Erlang driver.')
}

// exec executes the query and returns an array of all results.
// BEAM: requires esqlite3 driver. Use esqlite3:q/2.
@[manualfree]
pub fn (db &DB) exec(query string) ![]Row {
	return error('BEAM: SQLite requires esqlite3 Erlang driver. Use esqlite3:q/2.')
}

// exec_one executes a query and returns the first row.
// BEAM: requires esqlite3 driver.
@[manualfree]
pub fn (db &DB) exec_one(query string) !Row {
	return error('BEAM: SQLite requires esqlite3 Erlang driver.')
}

// error_message returns a proper V error.
// Pure V logic - constructs error from provided code and query.
@[manualfree]
pub fn (db &DB) error_message(code int, query string) IError {
	return SQLError{
		msg:  'BEAM: SQLite error (code ${code}) (query: ${query}). Use esqlite3 Erlang driver.'
		code: code
	}
}

// exec_none executes a query and returns the integer result code.
// BEAM: requires esqlite3 driver. Returns -1 (no connection).
pub fn (db &DB) exec_none(query string) int {
	// BEAM: requires esqlite3 driver. Returns -1 (no connection).
	return -1
}

// exec_param_many executes a query with parameters.
// BEAM: requires esqlite3 driver. Use esqlite3:q/3.
pub fn (db &DB) exec_param_many(query string, params Params) ![]Row {
	return error('BEAM: SQLite requires esqlite3 Erlang driver. Use esqlite3:q/3.')
}

// exec_param executes a query with one parameter.
// Pure V logic - delegates to exec_param_many.
pub fn (db &DB) exec_param(query string, param string) ![]Row {
	return db.exec_param_many(query, [param])
}

// create_table issues a "create table if not exists" command.
// BEAM: requires esqlite3 driver.
pub fn (mut db DB) create_table(table_name string, columns []string) ! {
	return error('BEAM: SQLite requires esqlite3 Erlang driver.')
}

// busy_timeout sets a busy timeout in milliseconds.
// BEAM: requires esqlite3 driver. Returns -1.
pub fn (db &DB) busy_timeout(ms int) int {
	// BEAM: requires esqlite3 driver. Use esqlite3 options for busy timeout.
	return -1
}

// synchronization_mode sets disk synchronization mode.
// BEAM: requires esqlite3 driver.
pub fn (db &DB) synchronization_mode(sync_mode SyncMode) ! {
	return error('BEAM: SQLite requires esqlite3 Erlang driver.')
}

// journal_mode controls how the journal file is stored.
// BEAM: requires esqlite3 driver.
pub fn (db &DB) journal_mode(journal_mode JournalMode) ! {
	return error('BEAM: SQLite requires esqlite3 Erlang driver.')
}

@[params]
pub struct Sqlite3TransactionParam {
	transaction_level Sqlite3TransactionLevel = .deferred
}

// begin begins a new transaction.
// BEAM: requires esqlite3 driver. Use esqlite3:exec(Db, "BEGIN").
pub fn (mut db DB) begin(param Sqlite3TransactionParam) ! {
	return error('BEAM: SQLite requires esqlite3 Erlang driver.')
}

// savepoint creates a new savepoint.
// BEAM: requires esqlite3 driver.
pub fn (mut db DB) savepoint(savepoint string) ! {
	return error('BEAM: SQLite requires esqlite3 Erlang driver.')
}

// commit commits the current transaction.
// BEAM: requires esqlite3 driver.
pub fn (mut db DB) commit() ! {
	return error('BEAM: SQLite requires esqlite3 Erlang driver.')
}

// rollback rollbacks the current transaction.
// BEAM: requires esqlite3 driver.
pub fn (mut db DB) rollback() ! {
	return error('BEAM: SQLite requires esqlite3 Erlang driver.')
}

// rollback_to rollbacks to a specified savepoint.
// BEAM: requires esqlite3 driver.
pub fn (mut db DB) rollback_to(savepoint string) ! {
	return error('BEAM: SQLite requires esqlite3 Erlang driver.')
}

// reset returns the connection to initial state for reuse.
// BEAM: no-op. esqlite3 handles connection state internally.
pub fn (mut db DB) reset() ! {
	// BEAM: no-op.
}

// validate checks if the connection is still usable.
// BEAM: returns false since no native connection exists.
pub fn (mut db DB) validate() !bool {
	// BEAM: no native connection. Returns false.
	return false
}

// --- Stmt methods needed by stmt.c.v / ORM ---

// init_stmt initializes a raw statement handle.
// BEAM: requires esqlite3 driver. Returns nil and error code -1.
fn (db &DB) init_stmt(query string) (voidptr, int) {
	// BEAM: requires esqlite3 driver.
	return unsafe { nil }, -1
}

// new_init_stmt creates a new Stmt wrapper.
// BEAM: requires esqlite3 driver. Use esqlite3:prepare/2.
fn (db &DB) new_init_stmt(query string) !Stmt {
	return error('BEAM: SQLite requires esqlite3 Erlang driver. Use esqlite3:prepare/2.')
}

// bind_null binds a NULL value at the given index.
// BEAM: requires esqlite3 driver. Returns -1.
fn (stmt &Stmt) bind_null(idx int) int {
	// BEAM: requires esqlite3 driver.
	return -1
}

// bind_int binds an integer value at the given index.
// BEAM: requires esqlite3 driver. Returns -1.
fn (stmt &Stmt) bind_int(idx int, v int) int {
	// BEAM: requires esqlite3 driver.
	return -1
}

// bind_i64 binds an i64 value at the given index.
// BEAM: requires esqlite3 driver. Returns -1.
fn (stmt &Stmt) bind_i64(idx int, v i64) int {
	// BEAM: requires esqlite3 driver.
	return -1
}

// bind_f64 binds an f64 value at the given index.
// BEAM: requires esqlite3 driver. Returns -1.
fn (stmt &Stmt) bind_f64(idx int, v f64) int {
	// BEAM: requires esqlite3 driver.
	return -1
}

// bind_text binds a text value at the given index.
// BEAM: requires esqlite3 driver. Returns -1.
fn (stmt &Stmt) bind_text(idx int, s string) int {
	// BEAM: requires esqlite3 driver.
	return -1
}

// get_int retrieves an integer value from a column.
// BEAM: requires esqlite3 driver. Returns none.
fn (stmt &Stmt) get_int(idx int) ?int {
	return none
}

// get_i64 retrieves an i64 value from a column.
// BEAM: requires esqlite3 driver. Returns none.
fn (stmt &Stmt) get_i64(idx int) ?i64 {
	return none
}

// get_f64 retrieves an f64 value from a column.
// BEAM: requires esqlite3 driver. Returns none.
fn (stmt &Stmt) get_f64(idx int) ?f64 {
	return none
}

// get_text retrieves a text value from a column.
// BEAM: requires esqlite3 driver. Returns none.
fn (stmt &Stmt) get_text(idx int) ?string {
	return none
}

// get_count returns the number of columns in the result.
// BEAM: requires esqlite3 driver. Returns 0.
fn (stmt &Stmt) get_count() int {
	// BEAM: requires esqlite3 driver.
	return 0
}

// step advances the statement to the next row.
// BEAM: requires esqlite3 driver. Returns sqlite_done (no more rows).
fn (stmt &Stmt) step() int {
	// BEAM: requires esqlite3 driver. Returns sqlite_done to signal completion.
	return sqlite_done
}

// orm_step executes an ORM statement step.
// BEAM: requires esqlite3 driver.
fn (stmt &Stmt) orm_step(query string) ! {
	return error('BEAM: SQLite requires esqlite3 Erlang driver.')
}

// finalize releases the statement resources.
// BEAM: no-op since no native statement exists. With esqlite3, statements are GC'd.
fn (stmt &Stmt) finalize() {
	// BEAM: no-op. esqlite3 handles statement cleanup.
}
