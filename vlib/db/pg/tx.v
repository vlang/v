module pg

import io

// Tx is a database transaction. It pins a single `Conn` from the pool for
// the lifetime of the transaction, so all queries run on the same physical
// connection. The pinned conn is returned to the pool when `commit()` or
// `rollback()` is called; failing to call either leaks the conn.
@[heap]
pub struct Tx {
mut:
	conn &Conn = unsafe { nil }
	done bool
}

fn (tx &Tx) ensure_active() ! {
	if tx.done {
		return error('pg: transaction is already finished')
	}
	if isnil(tx.conn) {
		return error('pg: transaction has no connection')
	}
}

fn (mut tx Tx) finish() {
	if tx.done {
		return
	}
	tx.done = true
	if !isnil(tx.conn) {
		mut c := tx.conn
		c.close() or {}
		tx.conn = unsafe { nil }
	}
}

// commit commits the transaction and returns the pinned conn to the pool.
pub fn (mut tx Tx) commit() ! {
	tx.ensure_active()!
	defer {
		tx.finish()
	}
	tx.conn.commit()!
}

// rollback rolls back the transaction and returns the pinned conn to the pool.
pub fn (mut tx Tx) rollback() ! {
	tx.ensure_active()!
	defer {
		tx.finish()
	}
	tx.conn.rollback()!
}

// savepoint creates a savepoint named `name`.
pub fn (mut tx Tx) savepoint(name string) ! {
	tx.ensure_active()!
	tx.conn.savepoint(name)!
}

// rollback_to rolls the transaction back to the savepoint named `name`.
pub fn (mut tx Tx) rollback_to(name string) ! {
	tx.ensure_active()!
	tx.conn.rollback_to(name)!
}

// release_savepoint releases the savepoint named `name`.
pub fn (mut tx Tx) release_savepoint(name string) ! {
	tx.ensure_active()!
	tx.conn.release_savepoint(name)!
}

// raw returns the pinned conn for advanced use cases. The caller MUST NOT
// call `close()` on it; the conn is owned by the transaction.
pub fn (mut tx Tx) raw() !&Conn {
	tx.ensure_active()!
	return tx.conn
}

// ---- exec/query helpers ----

// exec runs `query` on the pinned conn.
pub fn (mut tx Tx) exec(query string) ![]Row {
	tx.ensure_active()!
	return tx.conn.exec(query)
}

// exec_no_null runs `query` and returns rows with no nullable fields.
pub fn (mut tx Tx) exec_no_null(query string) ![]RowNoNull {
	tx.ensure_active()!
	return tx.conn.exec_no_null(query)
}

// exec_result runs `query` and returns a `Result`.
pub fn (mut tx Tx) exec_result(query string) !Result {
	tx.ensure_active()!
	return tx.conn.exec_result(query)
}

// exec_one runs `query` and returns its first row.
pub fn (mut tx Tx) exec_one(query string) !Row {
	tx.ensure_active()!
	return tx.conn.exec_one(query)
}

// exec_param_many runs `query` with the given parameters.
pub fn (mut tx Tx) exec_param_many(query string, params []string) ![]Row {
	tx.ensure_active()!
	return tx.conn.exec_param_many(query, params)
}

// exec_param_many_result runs `query` with parameters and returns a `Result`.
pub fn (mut tx Tx) exec_param_many_result(query string, params []string) !Result {
	tx.ensure_active()!
	return tx.conn.exec_param_many_result(query, params)
}

// exec_param runs `query` with one `$1` parameter.
pub fn (mut tx Tx) exec_param(query string, param string) ![]Row {
	return tx.exec_param_many(query, [param])
}

// exec_param2 runs `query` with two parameters.
pub fn (mut tx Tx) exec_param2(query string, param string, param2 string) ![]Row {
	return tx.exec_param_many(query, [param, param2])
}

// q_int runs `query` and returns the first column of the first row as int.
pub fn (mut tx Tx) q_int(query string) !int {
	tx.ensure_active()!
	return tx.conn.q_int(query)
}

// q_string runs `query` and returns the first column of the first row as string.
pub fn (mut tx Tx) q_string(query string) !string {
	tx.ensure_active()!
	return tx.conn.q_string(query)
}

// q_strings runs `query` and returns the full row set (alias of `exec`).
pub fn (mut tx Tx) q_strings(query string) ![]Row {
	return tx.exec(query)
}

// prepare registers a prepared statement on the pinned conn.
pub fn (mut tx Tx) prepare(name string, query string, num_params int) ! {
	tx.ensure_active()!
	return tx.conn.prepare(name, query, num_params)
}

// exec_prepared runs a previously-prepared statement on the pinned conn.
pub fn (mut tx Tx) exec_prepared(name string, params []string) ![]Row {
	tx.ensure_active()!
	return tx.conn.exec_prepared(name, params)
}

// exec_prepared_result runs a previously-prepared statement on the pinned conn.
pub fn (mut tx Tx) exec_prepared_result(name string, params []string) !Result {
	tx.ensure_active()!
	return tx.conn.exec_prepared_result(name, params)
}

// copy_expert runs a COPY command on the pinned conn.
pub fn (mut tx Tx) copy_expert(query string, mut file io.ReaderWriter) !int {
	tx.ensure_active()!
	return tx.conn.copy_expert(query, mut file)
}
