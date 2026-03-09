module orm

const tx_callback_closed_err = 'transaction callback must not close the transaction explicitly'

enum TxKind {
	root
	savepoint
}

@[heap]
struct TxRootState {
mut:
	next_savepoint_id int  = 1
	active            bool = true
}

@[heap]
struct TxInner {
mut:
	conn           TransactionalConnection
	kind           TxKind
	active         bool = true
	savepoint_name string
	root_state     &TxRootState = unsafe { nil }
}

// Tx is an ORM transaction handle that can be used anywhere an orm.Connection is accepted.
pub struct Tx {
mut:
	inner &TxInner = unsafe { nil }
}

@[heap]
struct SavepointInner {
mut:
	conn   TransactionalConnection
	owner  &TxInner = unsafe { nil }
	name   string
	active bool = true
}

// Savepoint is a manual savepoint handle created from an active transaction.
pub struct Savepoint {
mut:
	inner &SavepointInner = unsafe { nil }
}

// begin starts a new ORM transaction on the provided connection.
pub fn begin(mut conn TransactionalConnection) !Tx {
	conn.orm_begin()!
	return Tx{
		inner: &TxInner{
			conn:       conn
			kind:       .root
			root_state: &TxRootState{}
		}
	}
}

// transaction runs a callback inside a transaction and commits or rolls back automatically.
pub fn transaction[T](mut conn TransactionalConnection, f fn (mut Tx) !T) !T {
	mut tx := begin(mut conn)!
	result := f(mut tx) or {
		callback_err := err
		if !tx.is_active() {
			return error(tx_callback_closed_err)
		}
		tx.rollback() or {
			return error('transaction callback failed: ${callback_err.msg()}; rollback failed: ${err.msg()}')
		}
		return callback_err
	}
	if !tx.is_active() {
		return error(tx_callback_closed_err)
	}
	tx.commit()!
	return result
}

// commit commits an active transaction.
pub fn (mut tx Tx) commit() ! {
	tx.ensure_active('commit')!
	match tx.inner.kind {
		.root {
			tx.inner.conn.orm_commit()!
			tx.inner.root_state.active = false
		}
		.savepoint {
			tx.inner.conn.orm_release_savepoint(tx.inner.savepoint_name)!
		}
	}
	tx.inner.active = false
}

// rollback rolls back an active transaction.
pub fn (mut tx Tx) rollback() ! {
	tx.ensure_active('rollback')!
	match tx.inner.kind {
		.root {
			tx.inner.conn.orm_rollback()!
			tx.inner.root_state.active = false
		}
		.savepoint {
			tx.inner.conn.orm_rollback_to(tx.inner.savepoint_name)!
			tx.inner.conn.orm_release_savepoint(tx.inner.savepoint_name)!
		}
	}
	tx.inner.active = false
}

// savepoint creates a manual savepoint inside an active transaction.
pub fn (mut tx Tx) savepoint() !Savepoint {
	tx.ensure_active('create a savepoint')!
	name := tx.next_savepoint_name()!
	tx.inner.conn.orm_savepoint(name)!
	return Savepoint{
		inner: &SavepointInner{
			conn:  tx.inner.conn
			owner: tx.inner
			name:  name
		}
	}
}

// transaction runs a nested transaction backed by a savepoint.
pub fn (mut tx Tx) transaction[T](f fn (mut Tx) !T) !T {
	tx.ensure_active('start a nested transaction')!
	name := tx.next_savepoint_name()!
	tx.inner.conn.orm_savepoint(name)!
	mut nested := Tx{
		inner: &TxInner{
			conn:           tx.inner.conn
			kind:           .savepoint
			savepoint_name: name
			root_state:     tx.inner.root_state
		}
	}
	result := f(mut nested) or {
		callback_err := err
		if !nested.is_active() {
			return error(tx_callback_closed_err)
		}
		nested.rollback() or {
			return error('transaction callback failed: ${callback_err.msg()}; rollback failed: ${err.msg()}')
		}
		return callback_err
	}
	if !nested.is_active() {
		return error(tx_callback_closed_err)
	}
	nested.commit()!
	return result
}

// rollback rolls back to the savepoint and releases it.
pub fn (mut sp Savepoint) rollback() ! {
	sp.ensure_active('rollback')!
	sp.inner.conn.orm_rollback_to(sp.inner.name)!
	sp.inner.conn.orm_release_savepoint(sp.inner.name)!
	sp.inner.active = false
}

// release releases the savepoint without rolling back.
pub fn (mut sp Savepoint) release() ! {
	sp.ensure_active('release')!
	sp.inner.conn.orm_release_savepoint(sp.inner.name)!
	sp.inner.active = false
}

fn (tx Tx) is_active() bool {
	return !isnil(tx.inner) && !isnil(tx.inner.root_state) && tx.inner.active
		&& tx.inner.root_state.active
}

fn (tx Tx) ensure_active(action string) ! {
	if isnil(tx.inner) {
		return error('transaction is not initialized')
	}
	if !tx.inner.active {
		return error('transaction is inactive; cannot ${action}')
	}
}

fn (mut tx Tx) next_savepoint_name() !string {
	if isnil(tx.inner) || isnil(tx.inner.root_state) {
		return error('transaction is not initialized')
	}
	name := 'v_orm_sp_${tx.inner.root_state.next_savepoint_id}'
	tx.inner.root_state.next_savepoint_id++
	return name
}

fn (sp Savepoint) ensure_active(action string) ! {
	if isnil(sp.inner) {
		return error('savepoint is not initialized')
	}
	if !sp.inner.active || isnil(sp.inner.owner) || !Tx{
		inner: sp.inner.owner
	}.is_active() {
		return error('savepoint is inactive; cannot ${action}')
	}
}

// select forwards ORM select queries through the active transaction.
pub fn (mut tx Tx) select(config SelectConfig, data QueryData, where QueryData) ![][]Primitive {
	tx.ensure_active('use the transaction')!
	return tx.inner.conn.select(config, data, where)
}

// insert forwards ORM insert queries through the active transaction.
pub fn (mut tx Tx) insert(table Table, data QueryData) ! {
	tx.ensure_active('use the transaction')!
	tx.inner.conn.insert(table, data)!
}

// update forwards ORM update queries through the active transaction.
pub fn (mut tx Tx) update(table Table, data QueryData, where QueryData) ! {
	tx.ensure_active('use the transaction')!
	tx.inner.conn.update(table, data, where)!
}

// delete forwards ORM delete queries through the active transaction.
pub fn (mut tx Tx) delete(table Table, where QueryData) ! {
	tx.ensure_active('use the transaction')!
	tx.inner.conn.delete(table, where)!
}

// create forwards ORM create queries through the active transaction.
pub fn (mut tx Tx) create(table Table, fields []TableField) ! {
	tx.ensure_active('use the transaction')!
	tx.inner.conn.create(table, fields)!
}

// drop forwards ORM drop queries through the active transaction.
pub fn (mut tx Tx) drop(table Table) ! {
	tx.ensure_active('use the transaction')!
	tx.inner.conn.drop(table)!
}

// last_id forwards the last inserted id through the wrapped connection.
pub fn (mut tx Tx) last_id() int {
	if isnil(tx.inner) {
		return 0
	}
	return tx.inner.conn.last_id()
}
