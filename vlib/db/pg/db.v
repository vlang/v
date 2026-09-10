module pg

import io
import time

// DB is a thread-safe handle to a PostgreSQL database, backed by a pool of
// `Conn` objects. It mirrors Go's `database/sql.DB` design: methods on `DB`
// transparently acquire a conn for the call, then release it back to the
// pool. For operations that must run on the same physical connection
// (LISTEN/NOTIFY, session-scoped prepared statements, manual transactions),
// use `db.conn()` to pin a conn or `db.begin()` to start a transaction.
pub struct DB {
mut:
	pool &Pool = unsafe { nil }
}

// connect creates a new pool and opens an initial connection to verify the
// config works. The returned `&DB` is safe to share between threads.
pub fn connect(config Config, pcfg PoolConfig) !&DB {
	return connect_with_conninfo(config.conninfo()!, pcfg)!
}

// connect_with_conninfo is the conninfo-string variant of `connect`.
pub fn connect_with_conninfo(conninfo string, pcfg PoolConfig) !&DB {
	mut db := &DB{
		pool: new_pool(conninfo, pcfg)
	}
	// Fail fast if the conninfo is wrong, rather than at first query.
	probe := db.pool.acquire()!
	db.pool.release(probe)
	return db
}

// connect_direct opens a single physical PostgreSQL connection without
// creating an internal connection pool. The returned `&Conn` is not safe for
// concurrent use by multiple V threads; callers are responsible for calling
// `conn.close()` when done.
pub fn connect_direct(config Config) !&Conn {
	return connect_direct_with_conninfo(config.conninfo()!)!
}

// connect_direct_with_conninfo is the conninfo-string variant of `connect_direct`.
pub fn connect_direct_with_conninfo(conninfo string) !&Conn {
	slot := connect_slot(conninfo)!
	return &Conn{
		conn:       slot.handle
		created_at: slot.created_at
		bad:        slot.bad
	}
}

// close shuts down the pool and tears down all idle connections.
// In-flight conns will be closed when released.
pub fn (mut db DB) close() ! {
	if isnil(db.pool) {
		return
	}
	db.pool.close()
}

// stats returns a snapshot of the pool state.
pub fn (mut db DB) stats() PoolStats {
	return db.pool.stats()
}

// set_max_open_conns caps the total number of open connections.
// A value of 0 means unlimited (the default, like Go).
pub fn (mut db DB) set_max_open_conns(n int) {
	db.pool.set_max_open(n)
}

// set_max_idle_conns caps the number of idle connections kept warm.
pub fn (mut db DB) set_max_idle_conns(n int) {
	db.pool.set_max_idle(n)
}

// set_conn_max_lifetime sets the maximum amount of time a conn may be reused.
// A value of zero means conns are reused indefinitely.
pub fn (mut db DB) set_conn_max_lifetime(d time.Duration) {
	db.pool.set_conn_max_lifetime(d)
}

// conn checks a conn out of the pool. The caller is responsible for calling
// `conn.close()` when done; failing to do so leaks the conn. Use this when
// you need session-bound operations like LISTEN/NOTIFY.
pub fn (mut db DB) conn() !&Conn {
	return db.pool.acquire()
}

// validate borrows a conn from the pool and checks it is alive.
pub fn (mut db DB) validate() !bool {
	mut c := db.pool.acquire()!
	defer { c.close() or {} }
	return c.validate()
}

// reset is a no-op kept for ORM compatibility.
pub fn (mut db DB) reset() ! {
}

// ---- exec/query helpers (acquire-use-release) ----

// exec runs `query` on a pooled conn and returns the rows.
pub fn (mut db DB) exec(query string) ![]Row {
	mut c := db.pool.acquire()!
	defer { c.close() or {} }
	return c.exec(query)
}

// exec_no_null runs `query` and returns rows with no nullable fields.
pub fn (mut db DB) exec_no_null(query string) ![]RowNoNull {
	mut c := db.pool.acquire()!
	defer { c.close() or {} }
	return c.exec_no_null(query)
}

// exec_result runs `query` and returns a `Result` (rows + column index).
pub fn (mut db DB) exec_result(query string) !Result {
	mut c := db.pool.acquire()!
	defer { c.close() or {} }
	return c.exec_result(query)
}

// exec_one runs `query` and returns its first row.
pub fn (mut db DB) exec_one(query string) !Row {
	mut c := db.pool.acquire()!
	defer { c.close() or {} }
	return c.exec_one(query)
}

// exec_param_many runs `query` with the given parameters.
pub fn (mut db DB) exec_param_many(query string, params []string) ![]Row {
	mut c := db.pool.acquire()!
	defer { c.close() or {} }
	return c.exec_param_many(query, params)
}

// exec_param_many_result runs `query` with parameters and returns a `Result`.
pub fn (mut db DB) exec_param_many_result(query string, params []string) !Result {
	mut c := db.pool.acquire()!
	defer { c.close() or {} }
	return c.exec_param_many_result(query, params)
}

// exec_param runs `query` with a single `$1` parameter.
pub fn (mut db DB) exec_param(query string, param string) ![]Row {
	return db.exec_param_many(query, [param])
}

// exec_param2 runs `query` with two parameters (`$1`, `$2`).
pub fn (mut db DB) exec_param2(query string, param string, param2 string) ![]Row {
	return db.exec_param_many(query, [param, param2])
}

// q_int runs `query` and returns the first column of the first row as int.
pub fn (mut db DB) q_int(query string) !int {
	mut c := db.pool.acquire()!
	defer { c.close() or {} }
	return c.q_int(query)
}

// q_string runs `query` and returns the first column of the first row as string.
pub fn (mut db DB) q_string(query string) !string {
	mut c := db.pool.acquire()!
	defer { c.close() or {} }
	return c.q_string(query)
}

// q_strings runs `query` and returns the full row set (alias of `exec`).
pub fn (mut db DB) q_strings(query string) ![]Row {
	return db.exec(query)
}

// copy_expert runs a COPY command on a pooled conn.
pub fn (mut db DB) copy_expert(query string, mut file io.ReaderWriter) !int {
	mut c := db.pool.acquire()!
	defer { c.close() or {} }
	return c.copy_expert(query, mut file)
}

// ---- prepared statements ----
//
// NOTE: prepared statements are session-scoped. Calling `prepare` on `DB`
// only registers the statement on the conn that happened to serve the call.
// Use `db.conn()` to pin a conn for prepare+exec_prepared cycles.

// prepare registers a prepared statement on a transient conn.
// For repeated use, pin a conn via `db.conn()`.
pub fn (mut db DB) prepare(name string, query string, num_params int) ! {
	mut c := db.pool.acquire()!
	defer { c.close() or {} }
	return c.prepare(name, query, num_params)
}

// exec_prepared runs a previously-prepared statement.
pub fn (mut db DB) exec_prepared(name string, params []string) ![]Row {
	mut c := db.pool.acquire()!
	defer { c.close() or {} }
	return c.exec_prepared(name, params)
}

// exec_prepared_result runs a previously-prepared statement and returns a `Result`.
pub fn (mut db DB) exec_prepared_result(name string, params []string) !Result {
	mut c := db.pool.acquire()!
	defer { c.close() or {} }
	return c.exec_prepared_result(name, params)
}

// ---- transactions ----

// begin starts a new transaction and returns a `Tx` that pins a conn from
// the pool. The conn is released when `Tx.commit()` or `Tx.rollback()` is
// called. The default isolation level is REPEATABLE READ (matching the old
// single-conn API); pass `PQTransactionParam{ transaction_level: ... }` to
// override.
pub fn (mut db DB) begin(param PQTransactionParam) !&Tx {
	mut c := db.pool.acquire()!
	c.begin_on_conn(param) or {
		c.close() or {}
		return err
	}
	return &Tx{
		conn: c
	}
}
