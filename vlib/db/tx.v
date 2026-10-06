module db

import sync

// TransactionCommand identifies a transaction operation for an optional TransactionDriver.
pub enum TransactionCommand {
	begin
	commit
	rollback
	savepoint
	rollback_to
	release_savepoint
}

// TransactionDriver optionally overrides transaction commands for a Driver's SQL dialect.
// Drivers without this interface use BEGIN, COMMIT, ROLLBACK, and SQL savepoint statements.
// name is a validated identifier for savepoint commands and empty for other commands.
pub interface TransactionDriver {
	Driver
mut:
	transaction(command TransactionCommand, name string) !
}

// Tx owns one pooled connection until commit or rollback, including when either fails.
// Its operations are serialized; using a finished transaction returns an error.
// Call rollback in a defer to ensure abandoned transactions release their connection.
@[heap]
pub struct Tx {
	state &TxState = &TxState{}
}

@[heap]
struct TxState {
mut:
	mu   &sync.Mutex = sync.new_mutex()
	conn &Conn       = unsafe { nil }
	done bool
}

fn (tx &TxState) ensure_active() ! {
	if tx.done || isnil(tx.conn) {
		return error('db: transaction is already finished')
	}
}

fn (mut tx TxState) finish(discard bool) {
	tx.done = true
	mut conn := tx.conn
	tx.conn = unsafe { nil }
	if discard {
		conn.discard()
	} else {
		conn.close() or {}
	}
}

// begin acquires an exclusive connection and starts a transaction using the driver's defaults.
// A failed start discards the connection because its transaction state may be uncertain.
pub fn (mut database DB) begin() !&Tx {
	mut conn := database.acquire()!
	conn.transaction(.begin, '') or {
		conn.discard()
		return err
	}
	return &Tx{ state: &TxState{ conn: conn } }
}

// commit commits and releases the pinned connection. A failure discards the connection.
// The transaction is finished even when the driver reports an error.
pub fn (mut tx Tx) commit() ! {
	tx.state.mu.lock()
	defer { tx.state.mu.unlock() }
	tx.state.ensure_active()!
	tx.state.conn.transaction(.commit, '') or {
		tx.state.finish(true)
		return err
	}
	tx.state.finish(false)
}

// rollback rolls back and releases the pinned connection. A failure discards the connection.
// The transaction is finished even when the driver reports an error.
pub fn (mut tx Tx) rollback() ! {
	tx.state.mu.lock()
	defer { tx.state.mu.unlock() }
	tx.state.ensure_active()!
	tx.state.conn.transaction(.rollback, '') or {
		tx.state.finish(true)
		return err
	}
	tx.state.finish(false)
}

// exec executes query on the transaction's pinned connection.
pub fn (mut tx Tx) exec(query string) ![]DriverRow {
	tx.state.mu.lock()
	defer { tx.state.mu.unlock() }
	tx.state.ensure_active()!
	return tx.state.conn.exec(query)
}

// exec_one returns one row from the transaction's pinned connection.
pub fn (mut tx Tx) exec_one(query string) !DriverRow {
	tx.state.mu.lock()
	defer { tx.state.mu.unlock() }
	tx.state.ensure_active()!
	return tx.state.conn.exec_one(query)
}

// exec_param_many executes a parameterized query on the transaction's pinned connection.
pub fn (mut tx Tx) exec_param_many(query string, params []string) ![]DriverRow {
	tx.state.mu.lock()
	defer { tx.state.mu.unlock() }
	tx.state.ensure_active()!
	return tx.state.conn.exec_param_many(query, params)
}

// savepoint creates a savepoint named name, which must contain only identifier characters.
pub fn (mut tx Tx) savepoint(name string) ! {
	tx.state.mu.lock()
	defer { tx.state.mu.unlock() }
	tx.state.ensure_active()!
	tx.state.conn.transaction(.savepoint, name)!
}

// rollback_to rolls back to the named savepoint without finishing the transaction.
pub fn (mut tx Tx) rollback_to(name string) ! {
	tx.state.mu.lock()
	defer { tx.state.mu.unlock() }
	tx.state.ensure_active()!
	tx.state.conn.transaction(.rollback_to, name)!
}

// release_savepoint removes a savepoint if supported by the driver's SQL dialect.
pub fn (mut tx Tx) release_savepoint(name string) ! {
	tx.state.mu.lock()
	defer { tx.state.mu.unlock() }
	tx.state.ensure_active()!
	tx.state.conn.transaction(.release_savepoint, name)!
}

fn (mut c Conn) transaction(command TransactionCommand, name string) ! {
	c.mu.lock()
	defer { c.mu.unlock() }
	c.ensure_active()!
	if command in [.savepoint, .rollback_to, .release_savepoint] && !name.is_identifier() {
		return error('db: savepoint name must be an identifier')
	}
	if mut c.driver is TransactionDriver {
		c.driver.transaction(command, name)!
		return
	}
	c.driver.exec(transaction_query(command, '"${name}"'))!
}

// transaction_query builds SQL with an already quoted savepoint identifier.
fn transaction_query(command TransactionCommand, name string) string {
	return match command {
		.begin { 'BEGIN' }
		.commit { 'COMMIT' }
		.rollback { 'ROLLBACK' }
		.savepoint { 'SAVEPOINT ${name}' }
		.rollback_to { 'ROLLBACK TO SAVEPOINT ${name}' }
		.release_savepoint { 'RELEASE SAVEPOINT ${name}' }
	}
}

// discard invalidates a checked-out handle and closes its physical connection without reset.
fn (mut c Conn) discard() {
	c.mu.lock()
	if isnil(c.driver) {
		c.mu.unlock()
		return
	}
	slot := PoolSlot{ driver: c.driver, created_at: c.created_at }
	c.driver = unsafe { nil }
	c.mu.unlock()
	mut pool := c.pool
	pool.discard(slot)
}
