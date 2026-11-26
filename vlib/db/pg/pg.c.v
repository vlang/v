module pg

import io
import orm
import time

// for orm
$if windows {
	#include <winsock2.h>
} $else {
	#include <arpa/inet.h>
}

// Conn is a single libpq connection. It is NOT safe for concurrent use by
// multiple V threads (libpq enforces serial use of `PGconn*`). Use it pinned
// for operations that require a specific connection (LISTEN/NOTIFY, prepared
// statements scoped to the session, manual transactions). For pooled,
// thread-safe access prefer `DB`, which checks out a Conn per call.
@[heap]
pub struct Conn {
mut:
	conn       voidptr = unsafe { nil }
	pool       &Pool   = unsafe { nil }
	created_at time.Time
	bad        bool
}

pub struct Row {
pub mut:
	vals []?string
}

// val returns the value at `index`, flattening SQL NULL to an empty string.
pub fn (row Row) val(index int) string {
	if val := row.vals[index] {
		return val
	}
	return ''
}

// values returns all row values, flattening SQL NULL to empty strings.
pub fn (row Row) values() []string {
	mut values := []string{cap: row.vals.len}
	for val in row.vals {
		values << if value := val { value } else { '' }
	}
	return values
}

// val_opt returns the raw optional value at `index`.
pub fn (row Row) val_opt(index int) ?string {
	return row.vals[index]
}

pub struct RowNoNull {
pub mut:
	vals []string
}

// Field contains the metadata that libpq reports for a result column.
pub struct Field {
pub:
	name          string
	type_oid      u32
	type_modifier int
	size          int
	format        int
	table_oid     u32
	table_column  int
}

pub struct Result {
pub:
	cols   map[string]int
	names  []string
	rows   []Row
	fields []Field
}

// SslMode controls PostgreSQL SSL/TLS negotiation through libpq's `sslmode`
// connection keyword.
pub enum SslMode {
	unset
	disable
	allow
	prefer
	require
	verify_ca
	verify_full
}

// Notification represents a notification received from the server via LISTEN/NOTIFY
pub struct Notification {
pub:
	channel string // notification channel name
	pid     int    // process ID of notifying server process
	payload string // notification payload string (may be empty)
}

pub struct Config {
pub:
	host     string = 'localhost'
	port     int    = 5432
	user     string
	username string
	password string
	dbname   string
	// SSL/TLS configuration, passed through to libpq connection keywords.
	ssl_mode SslMode
	ssl_key  string // client key file path, maps to sslkey
	ssl_cert string // client certificate file path, maps to sslcert
	ssl_ca   string // CA certificate file path, maps to sslrootcert
	ssl_crl  string // certificate revocation list file path, maps to sslcrl
}

// PgNotify mirrors libpq's PGnotify layout for LISTEN/NOTIFY results.
struct PgNotify {
	relname &char // notification channel name
	be_pid  i32   // process ID of notifying server process
	extra   &char // notification payload string
	next    &PgNotify = unsafe { nil }
}

pub enum ConnStatusType {
	ok  = 0
	bad = 1
	// Non-blocking mode only below here
	// The existence of these should never be relied upon - they should only be used for user feedback or similar purposes.
	started           = 2 // Waiting for connection to be made.
	made              = 3 // Connection OK; waiting to send.
	awaiting_response = 4 // Waiting for a response from the postmaster.
	auth_ok           = 5 // Received authentication; waiting for backend startup.
	setenv            = 6 // This state is no longer used.
	ssl_startup       = 7 // Negotiating SSL.
	needed            = 8 // Internal state: connect() needed.
	check_writable    = 9 // Check if we could make a writable connection.
	consume           = 10 // Wait for pending messages and consume them.
	gss_startup       = 11 // Negotiating GSSAPI.
	check_target      = 12 // Checking target server properties.
	check_standby     = 13 // Checking if server is in standby mode.
	allocated         = 14 // Waiting for a connection attempt to start.
	authenticating    = 15 // Authenticating with an external system.
}

@[typedef]
pub enum ExecStatusType {
	empty_query      = 0 // empty query string was executed
	command_ok       = 1 // a query command that doesn't return anything was executed properly by the backend
	tuples_ok        = 2 // a query command that returns tuples was executed properly by the backend, PGresult contains the result tuples
	copy_out         = 3 // Copy Out data transfer in progress
	copy_in          = 4 // Copy In data transfer in progress
	bad_response     = 5 // an unexpected response was recv'd from the backend
	nonfatal_error   = 6 // notice or warning message
	fatal_error      = 7 // query failed
	copy_both        = 8 // Copy In/Out data transfer in progress
	single_tuple     = 9 // single tuple from larger resultset
	pipeline_sync    = 10 // pipeline synchronization point
	pipeline_aborted = 11 // command skipped after an earlier pipeline abort
	tuples_chunk     = 12 // chunk of tuples from a larger resultset
}

fn conninfo_needs_quotes(value string) bool {
	for ch in value {
		if ch.is_space() || ch == `'` || ch == `\\` {
			return true
		}
	}
	return false
}

fn escape_conninfo_value(value string) string {
	if !conninfo_needs_quotes(value) {
		return value
	}
	mut escaped := []u8{cap: value.len + 2}
	escaped << `'`
	for ch in value {
		if ch == `\\` || ch == `'` {
			escaped << `\\`
		}
		escaped << ch
	}
	escaped << `'`
	return escaped.bytestr()
}

fn (mode SslMode) conninfo_value() string {
	return match mode {
		.unset { '' }
		.disable { 'disable' }
		.allow { 'allow' }
		.prefer { 'prefer' }
		.require { 'require' }
		.verify_ca { 'verify-ca' }
		.verify_full { 'verify-full' }
	}
}

// connection_user returns the configured username, accepting both `user` and `username`.
pub fn (config Config) connection_user() !string {
	if config.user != '' && config.username != '' && config.user != config.username {
		return error('db.pg: Config.user and Config.username must match when both are set')
	}
	if config.user != '' {
		return config.user
	}
	return config.username
}

fn (config Config) conninfo() !string {
	mut parts := []string{cap: 10}
	if config.host != '' {
		parts << 'host=${escape_conninfo_value(config.host)}'
	}
	if config.port > 0 {
		parts << 'port=${config.port}'
	}
	user := config.connection_user()!
	if user != '' {
		parts << 'user=${escape_conninfo_value(user)}'
	}
	if config.dbname != '' {
		parts << 'dbname=${escape_conninfo_value(config.dbname)}'
	}
	if config.password != '' {
		parts << 'password=${escape_conninfo_value(config.password)}'
	}
	if config.ssl_mode != .unset {
		parts << 'sslmode=${config.ssl_mode.conninfo_value()}'
	}
	if config.ssl_cert != '' {
		parts << 'sslcert=${escape_conninfo_value(config.ssl_cert)}'
	}
	if config.ssl_key != '' {
		parts << 'sslkey=${escape_conninfo_value(config.ssl_key)}'
	}
	if config.ssl_ca != '' {
		parts << 'sslrootcert=${escape_conninfo_value(config.ssl_ca)}'
	}
	if config.ssl_crl != '' {
		parts << 'sslcrl=${escape_conninfo_value(config.ssl_crl)}'
	}
	return parts.join(' ')
}

// connect_slot opens a single libpq connection and returns it as an
// `IdleSlot` (raw handle + creation timestamp). The pool wraps the slot in a
// fresh `&Conn` per checkout so the wrapper cannot be revived by a stale
// reference after release.
fn connect_slot(conninfo string) !IdleSlot {
	ensure_pg_runtime()!
	conn := pg_runtime.connectdb(&char(conninfo.str))
	if conn == 0 {
		return error('libpq memory allocation error')
	}
	status := unsafe { ConnStatusType(pg_runtime.status(conn)) }
	if status != .ok {
		c_error_msg := unsafe { cstring_to_vstring(pg_runtime.error_message(conn)) }
		pg_runtime.finish(conn)
		return error('Connection to a PG database failed: ${c_error_msg}')
	}
	return IdleSlot{
		handle:     conn
		created_at: time.now()
	}
}

// physical_close_handle tears down a raw libpq handle. Used by the pool when
// it discards an expired/broken slot or unwinds during shutdown.
fn physical_close_handle(handle voidptr) {
	if handle != unsafe { nil } {
		pg_runtime.finish(handle)
	}
}

// slot_expired reports whether `slot` has lived longer than `max_lifetime`.
// A `max_lifetime` of zero means "no limit".
fn slot_expired(slot IdleSlot, max_lifetime time.Duration) bool {
	if max_lifetime <= 0 {
		return false
	}
	return time.now() - slot.created_at > max_lifetime
}

// slot_bad reports whether the libpq handle in `slot` is no longer usable.
fn slot_bad(slot IdleSlot) bool {
	if slot.bad {
		return true
	}
	if slot.handle == unsafe { nil } {
		return true
	}
	status := unsafe { ConnStatusType(pg_runtime.status(slot.handle)) }
	return status == .bad
}

// ensure_active errors if this wrapper has been detached from its handle.
// The pool nils `c.conn` during release() so any stale `&Conn` kept by user
// code becomes inert here — it can never reach the underlying PGconn*, even
// if the pool has since handed that same physical handle to another caller.
fn (mut c Conn) ensure_active() ! {
	if isnil(c.conn) {
		return error('pg: operation on released Conn (was close() called?)')
	}
}

// is_bad reports whether the underlying libpq connection has gone bad
// (e.g. dropped TCP, server idle-timeout).
pub fn (mut c Conn) is_bad() bool {
	if c.bad {
		return true
	}
	if c.conn == unsafe { nil } {
		return true
	}
	status := unsafe { ConnStatusType(pg_runtime.status(c.conn)) }
	return status == .bad
}

// is_expired reports whether the conn has lived longer than `max_lifetime`.
// A `max_lifetime` of zero means "no limit".
pub fn (mut c Conn) is_expired(max_lifetime time.Duration) bool {
	if max_lifetime <= 0 {
		return false
	}
	return time.now() - c.created_at > max_lifetime
}

// physical_close unconditionally tears down the libpq connection.
// It is unsafe because the caller must not use the conn afterwards.
@[unsafe]
fn (mut c Conn) physical_close() {
	if c.conn != unsafe { nil } {
		pg_runtime.finish(c.conn)
		c.conn = unsafe { nil }
	}
}

fn res_to_rows(res voidptr) []Row {
	nr_rows := pg_runtime.ntuples(res)
	nr_cols := pg_runtime.nfields(res)

	mut rows := []Row{}
	for i in 0 .. nr_rows {
		mut row := Row{}
		for j in 0 .. nr_cols {
			if pg_runtime.getisnull(res, i, j) != 0 {
				row.vals << none
			} else {
				val := pg_runtime.getvalue(res, i, j)
				row.vals << unsafe { cstring_to_vstring(val) }
			}
		}
		rows << row
	}

	pg_runtime.clear(res)
	return rows
}

fn res_to_rows_no_null(res voidptr) []RowNoNull {
	nr_rows := pg_runtime.ntuples(res)
	nr_cols := pg_runtime.nfields(res)
	mut rows := []RowNoNull{}
	for i in 0 .. nr_rows {
		mut row := RowNoNull{}
		for j in 0 .. nr_cols {
			val := pg_runtime.getvalue(res, i, j)
			row.vals << unsafe { cstring_to_vstring(val) }
		}
		rows << row
	}
	pg_runtime.clear(res)
	return rows
}

// res_to_result creates a `Result` struct out of a `C.PGresult` pointer
fn res_to_result(res voidptr) Result {
	nr_rows := pg_runtime.ntuples(res)
	nr_cols := pg_runtime.nfields(res)

	mut cols := map[string]int{}
	mut names := []string{}
	mut fields := []Field{cap: nr_cols}
	for j in 0 .. nr_cols {
		field_name := unsafe { cstring_to_vstring(pg_runtime.fname(res, j)) }
		cols[field_name] = j
		names << field_name
		fields << Field{
			name:          field_name
			type_oid:      pg_runtime.ftype(res, j)
			type_modifier: pg_runtime.fmod(res, j)
			size:          pg_runtime.fsize(res, j)
			format:        pg_runtime.fformat(res, j)
			table_oid:     pg_runtime.ftable(res, j)
			table_column:  pg_runtime.ftablecol(res, j)
		}
	}
	mut rows := []Row{}
	for i in 0 .. nr_rows {
		mut row := Row{}
		for j in 0 .. nr_cols {
			if pg_runtime.getisnull(res, i, j) != 0 {
				row.vals << none
			} else {
				val := pg_runtime.getvalue(res, i, j)
				row.vals << unsafe { cstring_to_vstring(val) }
			}
		}
		rows << row
	}

	pg_runtime.clear(res)
	return Result{
		cols:   cols
		names:  names
		rows:   rows
		fields: fields
	}
}

// close releases this conn back to its pool. Safe to call more than once:
// `release` detaches the wrapper from the underlying handle on the first
// call (nils `c.conn`), so any subsequent close or method invocation on the
// same `&Conn` is a no-op or a benign error rather than a use-after-free.
pub fn (mut c Conn) close() ! {
	if isnil(c.pool) {
		unsafe { c.physical_close() }
		return
	}
	mut pool := unsafe { c.pool }
	pool.release(c)
}

// q_int submit a command to the database server and
// returns an the first field in the first tuple
// converted to an int. If no row is found or on
// command failure, an error is returned
pub fn (mut c Conn) q_int(query string) !int {
	rows := c.exec(query)!
	if rows.len == 0 {
		return error('q_int "${query}" not found')
	}
	row := rows[0]
	if row.vals.len == 0 {
		return 0
	}
	val := row.vals[0]
	return val or { '0' }.int()
}

// q_string submit a command to the database server and
// returns an the first field in the first tuple
// as a string. If no row is found or on
// command failure, an error is returned
pub fn (mut c Conn) q_string(query string) !string {
	rows := c.exec(query)!
	if rows.len == 0 {
		return error('q_string "${query}" not found')
	}
	row := rows[0]
	if row.vals.len == 0 {
		return ''
	}
	val := row.vals[0]
	return val or { '' }
}

// q_strings submit a command to the database server and
// returns the resulting row set. Alias of `exec`
pub fn (mut c Conn) q_strings(query string) ![]Row {
	return c.exec(query)
}

// exec submits a command to the database server and wait for the result, returning an error on failure and a row set on success
pub fn (mut c Conn) exec(query string) ![]Row {
	c.ensure_active()!
	res := pg_runtime.exec(c.conn, &char(query.str))
	return c.handle_error_or_rows(res, 'exec')
}

// exec_no_null works like exec, but the fields can't be NULL, no optionals
pub fn (mut c Conn) exec_no_null(query string) ![]RowNoNull {
	c.ensure_active()!
	res := pg_runtime.exec(c.conn, &char(query.str))
	return c.handle_error_or_rows_no_null(res, 'exec')
}

// exec_result submits a command to the database server and wait for the result, returning an error on failure and a `Result` set on success
pub fn (mut c Conn) exec_result(query string) !Result {
	c.ensure_active()!
	res := pg_runtime.exec(c.conn, &char(query.str))
	return c.handle_error_or_result(res, 'exec_result')
}

fn rows_first_or_empty(rows []Row) !Row {
	if rows.len == 0 {
		return error('no row')
	}
	return rows[0]
}

// exec_one executes a query and returns its first row as a result, or an error on failure
pub fn (mut c Conn) exec_one(query string) !Row {
	c.ensure_active()!
	res := pg_runtime.exec(c.conn, &char(query.str))
	e := unsafe { pg_runtime.error_message(c.conn).vstring() }
	if e != '' {
		c.mark_bad_if_disconnected()
		return error('pg exec error: "${e}"')
	}
	row := rows_first_or_empty(res_to_rows(res))!
	return row
}

// exec_param_many executes a query with the parameters provided as ($1), ($2), ($n)
pub fn (mut c Conn) exec_param_many(query string, params []string) ![]Row {
	c.ensure_active()!
	unsafe {
		mut param_vals := []&char{len: params.len}
		for i in 0 .. params.len {
			param_vals[i] = &char(params[i].str)
		}

		res := pg_runtime.exec_params(c.conn, &char(query.str), params.len, 0, param_vals.data, 0, 0, 0)
		return c.handle_error_or_rows(res, 'exec_param_many')
	}
}

// exec_param_many executes a query with the parameters provided as ($1), ($2), ($n) and returns a `Result`
pub fn (mut c Conn) exec_param_many_result(query string, params []string) !Result {
	c.ensure_active()!
	unsafe {
		mut param_vals := []&char{len: params.len}
		for i in 0 .. params.len {
			param_vals[i] = &char(params[i].str)
		}

		res := pg_runtime.exec_params(c.conn, &char(query.str), params.len, 0, param_vals.data, 0, 0, 0)
		return c.handle_error_or_result(res, 'exec_param_many_result')
	}
}

// exec_param executes a query with 1 parameter ($1), and returns either an error on failure, or the full result set on success
pub fn (mut c Conn) exec_param(query string, param string) ![]Row {
	return c.exec_param_many(query, [param])
}

// exec_param2 executes a query with 2 parameters ($1) and ($2), and returns either an error on failure, or the full result set on success
pub fn (mut c Conn) exec_param2(query string, param string, param2 string) ![]Row {
	return c.exec_param_many(query, [param, param2])
}

// prepare submits a request to create a prepared statement with the given parameters, and waits for completion. You must provide the number of parameters (`$1, $2, $3 ...`) used in the statement
pub fn (mut c Conn) prepare(name string, query string, num_params int) ! {
	c.ensure_active()!
	res :=
		pg_runtime.prepare(c.conn, &char(name.str), &char(query.str), num_params, 0) // defining param types is optional

	return c.handle_error(res, 'prepare')
}

// exec_prepared sends a request to execute a prepared statement with given parameters, and waits for the result. The number of parameters must match with the parameters declared in the prepared statement.
pub fn (mut c Conn) exec_prepared(name string, params []string) ![]Row {
	c.ensure_active()!
	unsafe {
		mut param_vals := []&char{len: params.len}
		for i in 0 .. params.len {
			param_vals[i] = &char(params[i].str)
		}

		res := pg_runtime.exec_prepared(c.conn, &char(name.str), params.len, param_vals.data, 0, 0, 0)
		return c.handle_error_or_rows(res, 'exec_prepared')
	}
}

// exec_prepared sends a request to execute a prepared statement with given parameters, and waits for the result. The number of parameters must match with the parameters declared in the prepared statement.
// returns `Result`
pub fn (mut c Conn) exec_prepared_result(name string, params []string) !Result {
	c.ensure_active()!
	unsafe {
		mut param_vals := []&char{len: params.len}
		for i in 0 .. params.len {
			param_vals[i] = &char(params[i].str)
		}

		res := pg_runtime.exec_prepared(c.conn, &char(name.str), params.len, param_vals.data, 0, 0, 0)
		return c.handle_error_or_result(res, 'exec_prepared_result')
	}
}

fn (mut c Conn) mark_bad_if_disconnected() {
	status := unsafe { ConnStatusType(pg_runtime.status(c.conn)) }
	if status == .bad {
		c.bad = true
	}
}

fn (mut c Conn) handle_error_or_rows(res voidptr, elabel string) ![]Row {
	e := unsafe { pg_runtime.error_message(c.conn).vstring() }
	if e != '' {
		pg_runtime.clear(res)
		c.mark_bad_if_disconnected()
		$if trace_pg_error ? {
			eprintln('pg error: ${e}')
		}
		return error('pg ${elabel} error:\n${e}')
	}
	return res_to_rows(res)
}

fn (mut c Conn) handle_error_or_rows_no_null(res voidptr, elabel string) ![]RowNoNull {
	e := unsafe { pg_runtime.error_message(c.conn).vstring() }
	if e != '' {
		pg_runtime.clear(res)
		c.mark_bad_if_disconnected()
		$if trace_pg_error ? {
			eprintln('pg error: ${e}')
		}
		return error('pg ${elabel} error:\n${e}')
	}
	return res_to_rows_no_null(res)
}

// hande_error_or_result is an internal function similar to handle_error_or_rows that returns `Result` instead of `[]Row`
fn (mut c Conn) handle_error_or_result(res voidptr, elabel string) !Result {
	e := unsafe { pg_runtime.error_message(c.conn).vstring() }
	if e != '' {
		pg_runtime.clear(res)
		c.mark_bad_if_disconnected()
		$if trace_pg_error ? {
			eprintln('pg error: ${e}')
		}
		return error('pg ${elabel} error:\n${e}')
	}
	return res_to_result(res)
}

fn (mut c Conn) handle_error(res voidptr, elabel string) ! {
	e := unsafe { pg_runtime.error_message(c.conn).vstring() }
	if e != '' {
		pg_runtime.clear(res)
		c.mark_bad_if_disconnected()
		$if trace_pg_error ? {
			eprintln('pg error: ${e}')
		}
		return error('pg ${elabel} error:\n${e}')
	}
}

// copy_expert executes COPY command
// https://www.postgresql.org/docs/9.5/libpq-copy.html
pub fn (mut c Conn) copy_expert(query string, mut file io.ReaderWriter) !int {
	c.ensure_active()!
	mut res := pg_runtime.exec(c.conn, &char(query.str))
	status := unsafe { ExecStatusType(pg_runtime.result_status(res)) }
	defer {
		pg_runtime.clear(res)
	}

	e := unsafe { pg_runtime.error_message(c.conn).vstring() }
	if e != '' {
		return error('pg copy error:\n${e}')
	}

	if status == .copy_in {
		mut buf := []u8{len: 4 * 1024}
		for {
			n := file.read(mut buf) or {
				msg := 'pg copy error: Failed to read from input'
				pg_runtime.put_copy_end(c.conn, &char(msg.str))
				return err
			}
			if n <= 0 {
				break
			}

			code := pg_runtime.put_copy_data(c.conn, buf.data, n)
			if code == -1 {
				return error('pg copy error: Failed to send data, code=${code}')
			}
		}

		code := pg_runtime.put_copy_end(c.conn, &char(unsafe { nil }))

		if code != 1 {
			return error('pg copy error: Failed to finish copy command, code: ${code}')
		}
	} else if status == .copy_out {
		for {
			address := &char(unsafe { nil })
			n_bytes := pg_runtime.get_copy_data(c.conn, &address, 0)
			if n_bytes > 0 {
				mut local_buf := []u8{len: n_bytes}
				unsafe { C.memcpy(&u8(local_buf.data), address, n_bytes) }
				file.write(local_buf) or {
					pg_runtime.freemem(address)
					return err
				}
			} else if n_bytes == -1 {
				break
			} else if n_bytes == -2 {
				// consult PQerrorMessage for the reason
				return error('pg copy error: read error')
			}
			if address != 0 {
				pg_runtime.freemem(address)
			}
		}
	}

	return 0
}

fn pg_stmt_worker(mut c Conn, query string, data orm.QueryData, where orm.QueryData) ![]Row {
	mut param_types := []u32{}
	mut param_vals := []&char{}
	// C's PQexecParams reads these as `const int *` (4-byte C ints); back them with
	// i32 so `.data` matches the C ABI (a V `[]int` is now 64-bit per element).
	mut param_lens := []i32{}
	mut param_formats := []i32{}

	pg_stmt_binder(mut param_types, mut param_vals, mut param_lens, mut param_formats, data)
	pg_stmt_binder(mut param_types, mut param_vals, mut param_lens, mut param_formats, where)

	res := pg_runtime.exec_params(c.conn, &char(query.str), param_vals.len, param_types.data,
		param_vals.data, param_lens.data, param_formats.data, 0) // here, the last 0 means require text results, 1 - binary results
	return c.handle_error_or_rows(res, 'orm_stmt_worker')
}

pub enum PQTransactionLevel {
	read_uncommitted
	read_committed
	repeatable_read
	serializable
}

@[params]
pub struct PQTransactionParam {
pub:
	transaction_level PQTransactionLevel = .repeatable_read
}

// begin_on_conn begins a transaction on this single connection. Most callers
// should use `DB.begin()` instead, which returns a `Tx` that owns the
// underlying conn for the lifetime of the transaction.
pub fn (mut c Conn) begin_on_conn(param PQTransactionParam) ! {
	c.ensure_active()!
	mut sql_stmt := 'BEGIN TRANSACTION ISOLATION LEVEL '
	match param.transaction_level {
		.read_uncommitted { sql_stmt += 'READ UNCOMMITTED' }
		.read_committed { sql_stmt += 'READ COMMITTED' }
		.repeatable_read { sql_stmt += 'REPEATABLE READ' }
		.serializable { sql_stmt += 'SERIALIZABLE' }
	}

	_ := pg_runtime.exec(c.conn, &char(sql_stmt.str))
	e := unsafe { pg_runtime.error_message(c.conn).vstring() }
	if e != '' {
		c.mark_bad_if_disconnected()
		return error('pg exec error: "${e}"')
	}
}

// commit commits the current transaction on this connection.
pub fn (mut c Conn) commit() ! {
	c.ensure_active()!
	_ := pg_runtime.exec(c.conn, c'COMMIT;')
	e := unsafe { pg_runtime.error_message(c.conn).vstring() }
	if e != '' {
		c.mark_bad_if_disconnected()
		return error('pg exec error: "${e}"')
	}
}

// rollback rolls back the current transaction on this connection.
pub fn (mut c Conn) rollback() ! {
	c.ensure_active()!
	_ := pg_runtime.exec(c.conn, c'ROLLBACK;')
	e := unsafe { pg_runtime.error_message(c.conn).vstring() }
	if e != '' {
		c.mark_bad_if_disconnected()
		return error('pg exec error: "${e}"')
	}
}

// rollback_to rolls back to a specified savepoint on this connection.
pub fn (mut c Conn) rollback_to(savepoint string) ! {
	c.ensure_active()!
	if !savepoint.is_identifier() {
		return error('savepoint should be a identifier string')
	}
	sql_stmt := 'ROLLBACK TO SAVEPOINT ${savepoint};'
	_ := pg_runtime.exec(c.conn, &char(sql_stmt.str))
	e := unsafe { pg_runtime.error_message(c.conn).vstring() }
	if e != '' {
		c.mark_bad_if_disconnected()
		return error('pg exec error: "${e}"')
	}
}

// savepoint creates a new savepoint on this connection.
pub fn (mut c Conn) savepoint(savepoint string) ! {
	c.ensure_active()!
	if !savepoint.is_identifier() {
		return error('savepoint should be a identifier string')
	}
	sql_stmt := 'SAVEPOINT ${savepoint};'
	_ := pg_runtime.exec(c.conn, &char(sql_stmt.str))
	e := unsafe { pg_runtime.error_message(c.conn).vstring() }
	if e != '' {
		c.mark_bad_if_disconnected()
		return error('pg exec error: "${e}"')
	}
}

// release_savepoint releases a specified savepoint on this connection.
pub fn (mut c Conn) release_savepoint(savepoint string) ! {
	c.ensure_active()!
	if !savepoint.is_identifier() {
		return error('savepoint should be a identifier string')
	}
	sql_stmt := 'RELEASE SAVEPOINT ${savepoint};'
	_ := pg_runtime.exec(c.conn, &char(sql_stmt.str))
	e := unsafe { pg_runtime.error_message(c.conn).vstring() }
	if e != '' {
		c.mark_bad_if_disconnected()
		return error('pg exec error: "${e}"')
	}
}

// validate checks if the connection is still usable
pub fn (mut c Conn) validate() !bool {
	c.exec_one('SELECT 1')!
	return true
}

// reset returns the connection to initial state for reuse
pub fn (mut c Conn) reset() ! {
}

// as_structs is a `Result` method that maps the results' rows based on the provided mapping function
pub fn (res Result) as_structs[T](mapper fn (Result, Row) !T) ![]T {
	mut typed := []T{}
	for r in res.rows {
		typed << mapper(res, r)!
	}

	return typed
}

// listen registers the connection to receive notifications on the specified channel.
// After calling this, use consume_input() and get_notification() to receive notifications.
pub fn (mut c Conn) listen(channel string) ! {
	c.ensure_active()!
	if !channel.is_identifier() {
		return error('channel name should be a valid identifier')
	}
	sql_stmt := 'LISTEN ${channel};'
	_ := pg_runtime.exec(c.conn, &char(sql_stmt.str))
	e := unsafe { pg_runtime.error_message(c.conn).vstring() }
	if e != '' {
		c.mark_bad_if_disconnected()
		return error('pg listen error: "${e}"')
	}
}

// unlisten unregisters the connection from receiving notifications on the specified channel.
// Use unlisten_all() to unregister from all channels.
pub fn (mut c Conn) unlisten(channel string) ! {
	c.ensure_active()!
	if !channel.is_identifier() {
		return error('channel name should be a valid identifier')
	}
	sql_stmt := 'UNLISTEN ${channel};'
	_ := pg_runtime.exec(c.conn, &char(sql_stmt.str))
	e := unsafe { pg_runtime.error_message(c.conn).vstring() }
	if e != '' {
		c.mark_bad_if_disconnected()
		return error('pg unlisten error: "${e}"')
	}
}

// unlisten_all unregisters the connection from all notification channels.
pub fn (mut c Conn) unlisten_all() ! {
	c.ensure_active()!
	_ := pg_runtime.exec(c.conn, c'UNLISTEN *;')
	e := unsafe { pg_runtime.error_message(c.conn).vstring() }
	if e != '' {
		c.mark_bad_if_disconnected()
		return error('pg unlisten error: "${e}"')
	}
}

// escape_literal returns `value` as a quoted PostgreSQL literal that is safe to interpolate into
// a query. Prefer parameterized queries when they can express the operation.
pub fn (mut c Conn) escape_literal(value string) !string {
	c.ensure_active()!
	escaped := pg_runtime.escape_literal(c.conn, &char(value.str), uint(value.len))
	if escaped == unsafe { nil } {
		e := unsafe { pg_runtime.error_message(c.conn).vstring() }
		return error('pg escape_literal error: "${e}"')
	}
	result := unsafe { escaped.vstring().clone() }
	pg_runtime.freemem(escaped)
	return result
}

// notify sends a notification on the specified channel with an optional payload.
// All connections currently listening on that channel will receive the notification.
pub fn (mut c Conn) notify(channel string, payload string) ! {
	c.ensure_active()!
	if !channel.is_identifier() {
		return error('channel name should be a valid identifier')
	}
	mut sql_stmt := ''
	if payload.len > 0 {
		escaped := c.escape_literal(payload)!
		sql_stmt = 'NOTIFY ${channel}, ${escaped};'
	} else {
		sql_stmt = 'NOTIFY ${channel};'
	}
	_ := pg_runtime.exec(c.conn, &char(sql_stmt.str))
	e := unsafe { pg_runtime.error_message(c.conn).vstring() }
	if e != '' {
		c.mark_bad_if_disconnected()
		return error('pg notify error: "${e}"')
	}
}

// consume_input reads any available input from the server.
// This must be called before get_notification() to ensure pending notifications are processed.
// Returns true on success, false if there was an error reading from the connection.
pub fn (mut c Conn) consume_input() !bool {
	c.ensure_active()!
	result := pg_runtime.consume_input(c.conn)
	if result == 0 {
		e := unsafe { pg_runtime.error_message(c.conn).vstring() }
		c.mark_bad_if_disconnected()
		return error('pg consume_input error: "${e}"')
	}
	return true
}

// get_notification returns the next pending notification from the server, if any.
// Returns none if there are no pending notifications.
// You should call consume_input() before this to ensure all pending notifications are available.
pub fn (mut c Conn) get_notification() ?Notification {
	c.ensure_active() or { return none }
	notify := &PgNotify(pg_runtime.notifies(c.conn))
	if notify == unsafe { nil } {
		return none
	}
	defer {
		pg_runtime.freemem(notify)
	}
	return Notification{
		channel: unsafe { notify.relname.vstring() }
		pid:     notify.be_pid
		payload: unsafe { notify.extra.vstring() }
	}
}

// socket returns the file descriptor of the connection socket to the server.
// This is useful for applications that want to use select() or poll() to wait
// for notifications without blocking. Returns -1 if no valid socket.
pub fn (mut c Conn) socket() int {
	c.ensure_active() or { return -1 }
	return pg_runtime.socket(c.conn)
}
