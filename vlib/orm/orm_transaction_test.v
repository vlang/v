import db.sqlite
import orm

struct TxUser {
	id   int @[primary; sql: serial]
	name string
}

fn setup_tx_db() !sqlite.DB {
	mut db := sqlite.connect(':memory:')!
	sql db {
		create table TxUser
	}!
	return db
}

fn insert_callback_user(mut tx orm.Tx) !int {
	user := TxUser{
		name: 'callback_commit'
	}
	sql tx {
		insert user into TxUser
	}!
	return tx.last_id()
}

fn insert_and_fail(mut tx orm.Tx) !int {
	user := TxUser{
		name: 'callback_rollback'
	}
	sql tx {
		insert user into TxUser
	}!
	return error('force rollback')
}

fn nested_success_inner(mut tx orm.Tx) !int {
	user := TxUser{
		name: 'nested_success'
	}
	sql tx {
		insert user into TxUser
	}!
	return tx.last_id()
}

fn nested_failure_inner(mut tx orm.Tx) !int {
	user := TxUser{
		name: 'nested_failure'
	}
	sql tx {
		insert user into TxUser
	}!
	return error('nested failure')
}

fn count_tx_users(db sqlite.DB) !int {
	return db.q_int('select count(*) from TxUser')!
}

fn tx_user_names(db sqlite.DB) ![]string {
	rows := sql db {
		select from TxUser order by id
	}!
	return rows.map(it.name)
}

fn test_transaction_callback_commits_on_success() {
	mut db := setup_tx_db()!
	defer {
		db.close() or {}
	}

	id := orm.transaction[int](mut db, insert_callback_user)!
	assert id == 1
	assert count_tx_users(db)! == 1
	assert tx_user_names(db)! == ['callback_commit']
}

fn test_transaction_callback_rolls_back_on_error() {
	mut db := setup_tx_db()!
	defer {
		db.close() or {}
	}

	orm.transaction[int](mut db, insert_and_fail) or {
		assert err.msg() == 'force rollback'
		assert count_tx_users(db)! == 0
		return
	}
	assert false
}

fn test_manual_begin_with_sql_tx_queries() {
	mut db := setup_tx_db()!
	defer {
		db.close() or {}
	}

	mut tx := orm.begin(mut db)!
	user := TxUser{
		name: 'manual'
	}
	sql tx {
		insert user into TxUser
	}!
	inserted := sql tx {
		select from TxUser where name == 'manual'
	}!
	assert inserted.len == 1
	assert inserted[0].name == 'manual'

	sql tx {
		update TxUser set name = 'manual_updated' where name == 'manual'
	}!
	updated := sql tx {
		select from TxUser where name == 'manual_updated'
	}!
	assert updated.len == 1

	sql tx {
		delete from TxUser where name == 'manual_updated'
	}!
	assert count_tx_users(db)! == 0
	tx.commit()!
}

fn test_query_builder_works_inside_transaction() {
	mut db := setup_tx_db()!
	defer {
		db.close() or {}
	}

	mut tx := orm.begin(mut db)!
	mut qb := orm.new_query[TxUser](tx)
	qb.insert(TxUser{
		name: 'qb_user'
	})!

	selected := qb.where('name = ?', 'qb_user')!.query()!
	assert selected.len == 1
	assert selected[0].name == 'qb_user'

	tx.commit()!
	assert tx_user_names(db)! == ['qb_user']
}

fn test_nested_transaction_success_releases_savepoint() {
	mut db := setup_tx_db()!
	defer {
		db.close() or {}
	}

	orm.transaction[int](mut db, fn (mut tx orm.Tx) !int {
		before := TxUser{
			name: 'outer_before'
		}
		sql tx {
			insert before into TxUser
		}!

		nested_id := tx.transaction[int](nested_success_inner)!
		after := TxUser{
			name: 'outer_after'
		}
		sql tx {
			insert after into TxUser
		}!
		return nested_id
	})!

	assert tx_user_names(db)! == ['outer_before', 'nested_success', 'outer_after']
}

fn test_nested_transaction_failure_rolls_back_inner_work_only() {
	mut db := setup_tx_db()!
	defer {
		db.close() or {}
	}

	orm.transaction[int](mut db, fn (mut tx orm.Tx) !int {
		before := TxUser{
			name: 'outer_before'
		}
		sql tx {
			insert before into TxUser
		}!

		tx.transaction[int](nested_failure_inner) or {}

		after := TxUser{
			name: 'outer_after'
		}
		sql tx {
			insert after into TxUser
		}!
		return 0
	})!

	assert tx_user_names(db)! == ['outer_before', 'outer_after']
}

fn test_manual_savepoint_rollback_and_release() {
	mut db := setup_tx_db()!
	defer {
		db.close() or {}
	}

	mut tx := orm.begin(mut db)!
	before := TxUser{
		name: 'before_savepoint'
	}
	sql tx {
		insert before into TxUser
	}!

	mut sp := tx.savepoint()!
	rollback_user := TxUser{
		name: 'rollback_me'
	}
	sql tx {
		insert rollback_user into TxUser
	}!
	sp.rollback()!

	after := TxUser{
		name: 'after_rollback'
	}
	sql tx {
		insert after into TxUser
	}!

	mut sp2 := tx.savepoint()!
	released := TxUser{
		name: 'keep_me'
	}
	sql tx {
		insert released into TxUser
	}!
	sp2.release()!

	tx.commit()!
	assert tx_user_names(db)! == ['before_savepoint', 'after_rollback', 'keep_me']
}

fn test_inactive_transaction_errors() {
	mut db := setup_tx_db()!
	defer {
		db.close() or {}
	}

	mut tx := orm.begin(mut db)!
	tx.commit()!

	tx.commit() or { assert err.msg().contains('inactive') }

	user := TxUser{
		name: 'after_close'
	}
	sql tx {
		insert user into TxUser
	} or {
		assert err.msg().contains('inactive')
		return
	}
	assert false
}

fn test_inactive_savepoint_errors() {
	mut db := setup_tx_db()!
	defer {
		db.close() or {}
	}

	mut tx := orm.begin(mut db)!
	mut sp := tx.savepoint()!
	sp.release()!
	sp.rollback() or {
		assert err.msg().contains('inactive')
		tx.rollback()!
		return
	}
	assert false
}

fn test_savepoint_becomes_inactive_when_parent_transaction_closes() {
	mut db := setup_tx_db()!
	defer {
		db.close() or {}
	}

	mut tx := orm.begin(mut db)!
	mut sp := tx.savepoint()!
	tx.commit()!

	sp.release() or {
		assert err.msg().contains('inactive')
		return
	}
	assert false
}
