// vtest build: present_sqlite3? && !sanitize-memory-clang
module migrations

import db.sqlite

struct MigrationWidget {
	id    int @[primary; sql: serial]
	label string
}

fn create_accounts(mut ctx Context) ! {
	ctx.create_table(Table{
		name: 'organizations'
	})!
	ctx.create_table(Table{
		name:         'accounts'
		columns:      [
			Column{
				name:     'email'
				kind:     .varchar
				nullable: false
			},
			Column{
				name:     'organization_id'
				kind:     .bigint
				nullable: false
			},
		]
		foreign_keys: [
			ForeignKey{
				from_table: 'accounts'
				column:     'organization_id'
				to_table:   'organizations'
				on_delete:  'cascade'
			},
		]
	})!
}

fn drop_accounts(mut ctx Context) ! {
	ctx.drop_table('accounts')!
	ctx.drop_table('organizations')!
}

fn add_account_name(mut ctx Context) ! {
	ctx.add_column('accounts', Column{
		name:        'name'
		kind:        .text
		default_sql: "''"
		nullable:    false
	})!
	ctx.add_index(Index{
		table:   'accounts'
		columns: ['name']
		name:    'index_accounts_on_name'
	})!
}

fn remove_account_name(mut ctx Context) ! {
	ctx.remove_index('accounts', 'index_accounts_on_name')!
	ctx.remove_column('accounts', 'name')!
}

fn fail_after_create(mut ctx Context) ! {
	ctx.create_table(Table{
		name: 'should_rollback'
	})!
	return error('forced migration failure')
}

fn drop_should_rollback(mut ctx Context) ! {
	ctx.drop_table('should_rollback')!
}

fn create_widget_with_orm_dsl(mut ctx Context) ! {
	sql ctx {
		create table MigrationWidget
	}!
}

fn drop_widget_with_orm_dsl(mut ctx Context) ! {
	sql ctx {
		drop table MigrationWidget
	}!
}

fn test_migrate_rollback_redo_and_status() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	mut runner := new(mut db, [
		Migration{
			version: 202608160001
			name:    'create_accounts'
			up:      create_accounts
			down:    drop_accounts
		},
		Migration{
			version: 202608160002
			name:    'add_account_name'
			up:      add_account_name
			down:    remove_account_name
		},
	], Config{})!

	assert runner.pending()!.map(it.version) == [i64(202608160001), 202608160002]
	applied := runner.migrate()!
	assert applied.map(it.name) == ['create_accounts', 'add_account_name']
	assert runner.current_version()! == 202608160002
	assert db.q_int("SELECT count(*) FROM pragma_table_info('accounts') WHERE name = 'name';")! == 1
	assert db.q_int("SELECT count(*) FROM pragma_foreign_key_list('accounts') WHERE `from` = 'organization_id';")! == 1
	assert db.q_int("SELECT count(*) FROM sqlite_master WHERE type = 'index' AND name = 'index_accounts_on_name';")! == 1
	assert runner.migrate()!.len == 0

	statuses := runner.status()!
	assert statuses.len == 2
	assert statuses.all(it.state == .applied)

	reverted := runner.rollback(1)!
	assert reverted.map(it.version) == [i64(202608160002)]
	assert db.q_int("SELECT count(*) FROM pragma_table_info('accounts') WHERE name = 'name';")! == 0
	assert db.q_int("SELECT count(*) FROM sqlite_master WHERE type = 'index' AND name = 'index_accounts_on_name';")! == 0
	assert runner.migrate()!.map(it.version) == [i64(202608160002)]
	assert runner.redo(1)!.map(it.version) == [i64(202608160002)]
	assert db.q_int("SELECT count(*) FROM pragma_table_info('accounts') WHERE name = 'name';")! == 1

	runner.migrate_to(0)!
	assert db.q_int("SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'accounts';")! == 0
	assert runner.current_version()! == 0
	db.exec("INSERT INTO schema_migrations (version, name, applied_at) VALUES (99, 'missing_file', '2026-08-16T00:00:00.000Z');")!
	missing := runner.status()!.filter(it.state == .missing)
	assert missing.len == 1
	assert missing[0].version == 99
	runner.rollback_last() or {
		assert err.msg() == 'cannot roll back migration 99: its migration file is missing'
		return
	}
	assert false
}

fn test_failed_migration_rolls_back_schema_and_history() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	mut runner := new(mut db, [
		Migration{
			version: 1
			name:    'fail_after_create'
			up:      fail_after_create
			down:    drop_should_rollback
		},
	], Config{})!

	runner.migrate() or {
		assert err.msg().contains('forced migration failure')
		assert db.q_int("SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'should_rollback';")! == 0
		assert runner.applied()!.len == 0
		return
	}
	assert false
}

fn test_context_supports_v3_orm_sql_blocks() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	mut runner := new(mut db, [
		Migration{
			version: 1
			name:    'create_widget_with_orm_dsl'
			up:      create_widget_with_orm_dsl
			down:    drop_widget_with_orm_dsl
		},
	], Config{})!

	runner.migrate()!
	assert db.q_int("SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'migrationwidget';")! == 1
	runner.rollback(1)!
	assert db.q_int("SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'migrationwidget';")! == 0
}

fn test_validation_and_portable_sql_generation() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	new(mut db, [
		Migration{
			version: 7
			name:    'one'
			up:      create_accounts
			down:    drop_accounts
		},
		Migration{
			version: 7
			name:    'two'
			up:      create_accounts
			down:    drop_accounts
		},
	], Config{}) or {
		assert err.msg() == 'duplicate migration version 7'
		assert column_type_sql(.pg, Column{ name: 'payload', kind: .jsonb })! == 'JSONB'
		assert column_type_sql(.mysql, Column{ name: 'amount', kind: .double_precision })! == 'DOUBLE'
		assert index_name(Index{
			table:   'accounts'
			columns: ['email', 'name']
		})! == 'index_accounts_on_email_and_name'
		return
	}
	assert false
}
