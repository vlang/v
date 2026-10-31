// Coverage for the uninitialised half of `transaction.v`. `Tx.inner` and
// `Savepoint.inner` are `&TxInner`/`&SavepointInner` fields whose default is a
// nil pointer, so a zero value is constructible from outside the module and
// `ensure_active` has an explicit branch for it.
//
// Nothing here opens a connection: `ensure_active` and `last_id` both return
// or read from the handle before any driver is touched, so the nil path is
// reachable with no database at all.
import orm

fn test_a_zero_transaction_handle_reports_it_is_not_initialized() {
	mut tx := orm.Tx{}

	// Measured: `ensure_active` checks `isnil(tx.inner)` first, so a nil handle
	// is a distinct error from an inactive one rather than a panic.
	tx.commit() or { assert err.msg() == 'transaction is not initialized' }
	tx.rollback() or { assert err.msg() == 'transaction is not initialized' }
	tx.savepoint() or { assert err.msg() == 'transaction is not initialized' }
}

fn test_the_crud_methods_refuse_a_zero_transaction_handle() {
	mut tx := orm.Tx{}
	table := orm.new_table('users', [])
	empty := orm.QueryData{}

	tx.select(orm.SelectConfig{}, empty, empty) or {
		assert err.msg() == 'transaction is not initialized'
	}
	tx.insert(table, empty) or {
		assert err.msg() == 'transaction is not initialized'
	}
	tx.update(table, empty, empty) or {
		assert err.msg() == 'transaction is not initialized'
	}
	tx.delete(table, empty) or {
		assert err.msg() == 'transaction is not initialized'
	}
	tx.create(table, [orm.TableField{}]) or {
		assert err.msg() == 'transaction is not initialized'
	}
	tx.drop(table) or {
		assert err.msg() == 'transaction is not initialized'
	}
	tx.execute('SELECT 1') or {
		assert err.msg() == 'transaction is not initialized'
	}
}

fn test_a_nested_transaction_refuses_a_zero_transaction_handle() {
	mut tx := orm.Tx{}

	tx.transaction[int](fn (mut inner orm.Tx) !int {
		return 1
	}) or {
		assert err.msg() == 'transaction is not initialized'
	}
}

fn test_last_id_of_a_zero_transaction_handle_is_zero() {
	// Measured: `last_id` guards with `isnil` and returns 0 instead of
	// dereferencing the handle, so it is the one method that cannot fail.
	mut tx := orm.Tx{}
	assert tx.last_id() == 0
}

fn test_a_zero_savepoint_reports_it_is_not_initialized() {
	mut sp := orm.Savepoint{}

	sp.rollback() or { assert err.msg() == 'savepoint is not initialized' }
	sp.release() or { assert err.msg() == 'savepoint is not initialized' }
}
