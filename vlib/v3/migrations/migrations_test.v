// vtest build: present_sqlite3? && !sanitize-memory-clang
module migrations

import db.sqlite
import orm

@[heap]
struct RecordingConnection {
mut:
	queries []string
}

fn (mut conn RecordingConnection) select(_ orm.SelectConfig, _ orm.QueryData, _ orm.QueryData) ![][]orm.Primitive {
	return []
}

fn (mut conn RecordingConnection) insert(_ orm.Table, _ orm.QueryData) ! {}

fn (mut conn RecordingConnection) update(_ orm.Table, _ orm.QueryData, _ orm.QueryData) ! {}

fn (mut conn RecordingConnection) delete(_ orm.Table, _ orm.QueryData) ! {}

fn (mut conn RecordingConnection) create(_ orm.Table, _ []orm.TableField) ! {}

fn (mut conn RecordingConnection) drop(_ orm.Table) ! {}

fn (mut conn RecordingConnection) last_id() int {
	return 0
}

fn (mut conn RecordingConnection) execute(query string) ![]orm.Row {
	conn.queries << query
	if query.starts_with('SELECT GET_LOCK(') || query.starts_with('SELECT RELEASE_LOCK(')
		|| query.starts_with('SELECT pg_advisory_unlock(') {
		return [orm.Row{
			vals: ['1']
		}]
	}
	return []
}

fn (mut conn RecordingConnection) orm_begin() ! {
	conn.queries << 'ORM BEGIN'
}

fn (mut conn RecordingConnection) orm_commit() ! {
	conn.queries << 'ORM COMMIT'
}

fn (mut conn RecordingConnection) orm_rollback() ! {
	conn.queries << 'ORM ROLLBACK'
}

fn (mut conn RecordingConnection) orm_savepoint(name string) ! {
	conn.queries << 'ORM SAVEPOINT ${name}'
}

fn (mut conn RecordingConnection) orm_rollback_to(name string) ! {
	conn.queries << 'ORM ROLLBACK TO ${name}'
}

fn (mut conn RecordingConnection) orm_release_savepoint(name string) ! {
	conn.queries << 'ORM RELEASE SAVEPOINT ${name}'
}

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

fn record_locked_migration(mut ctx Context) ! {
	ctx.execute('migration callback;')!
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

fn test_mutating_runner_holds_dialect_lock_around_callbacks() {
	for dialect in [Dialect.sqlite, .pg, .mysql] {
		mut recorder := &RecordingConnection{}
		mut runner := new(mut recorder, [
			Migration{
				version: 1
				name:    'locked'
				up:      record_locked_migration
				down:    record_locked_migration
			},
		], Config{
			dialect: dialect
		})!
		runner.migrate()!
		key := migration_lock_key('schema_migrations')
		match dialect {
			.sqlite {
				assert recorder.queries[0] == 'BEGIN IMMEDIATE;'
				assert recorder.queries.last() == 'ORM COMMIT'
				assert 'ORM BEGIN' !in recorder.queries
			}
			.pg {
				assert recorder.queries[0] == 'SELECT pg_advisory_lock(${key});'
				assert recorder.queries.last() == 'SELECT pg_advisory_unlock(${key});'
				assert 'ORM BEGIN' in recorder.queries
				assert 'ORM COMMIT' in recorder.queries
			}
			.mysql {
				name := migration_lock_name(key)
				assert recorder.queries[0] == "SELECT GET_LOCK('${name}', ${migration_lock_timeout_seconds});"
				assert recorder.queries.last() == "SELECT RELEASE_LOCK('${name}');"
				assert 'ORM BEGIN' !in recorder.queries
			}
		}
		callback_index := recorder.queries.index('migration callback;')
		assert callback_index > 0
		assert callback_index < recorder.queries.len - 1
	}
}

fn test_postgresql_change_column_rejects_constraint_options_before_sql() {
	mut recorder := &RecordingConnection{}
	mut ctx := new_context(recorder, .pg)
	ctx.change_column('accounts', Column{
		name:  'score'
		kind:  .bigint
		limit: 64
	})!
	assert recorder.queries == [
		'ALTER TABLE "accounts" ALTER COLUMN "score" TYPE BIGINT;',
	]

	ctx.change_column('accounts', Column{
		name:           'score'
		kind:           .bigint
		nullable:       false
		default_sql:    '0'
		unique:         true
		primary_key:    true
		auto_increment: true
	}) or {
		assert err.msg() == 'PostgreSQL change_column only supports type, limit, precision, and scale; unsupported options: nullable, default_sql, unique, primary_key, auto_increment; use ctx.execute() for constraint changes'
		assert recorder.queries.len == 1
		return
	}
	assert false
}

fn test_postgresql_change_column_rejects_explicit_constraint_removals() {
	mut recorder := &RecordingConnection{}
	mut ctx := new_context(recorder, .pg)
	ctx.change_column('accounts', Column{
		name:           'score'
		kind:           .bigint
		nullable:       true
		default_sql:    ''
		unique:         false
		primary_key:    false
		auto_increment: false
	}) or {
		assert err.msg() == 'PostgreSQL change_column only supports type, limit, precision, and scale; unsupported options: nullable, default_sql, unique, primary_key, auto_increment; use ctx.execute() for constraint changes'
		assert recorder.queries.len == 0
		return
	}
	assert false
}

fn test_mysql_change_column_requires_complete_definition() {
	mut recorder := &RecordingConnection{}
	mut ctx := new_context(recorder, .mysql)
	mut error_message := ''
	ctx.change_column('accounts', Column{
		name: 'score'
		kind: .bigint
	}) or { error_message = err.msg() }
	assert error_message == 'MySQL change_column requires a complete column definition; missing options: nullable, default_sql, auto_increment'
	assert recorder.queries.len == 0

	error_message = ''
	ctx.change_column('accounts', Column{
		name:           'score'
		kind:           .bigint
		nullable:       true
		default_sql:    ''
		unique:         false
		primary_key:    false
		auto_increment: false
	}) or { error_message = err.msg() }
	assert error_message == 'MySQL change_column cannot remove key constraints; unsupported false options: unique, primary_key; use remove_index() or ctx.execute()'
	assert recorder.queries.len == 0

	ctx.change_column('accounts', Column{
		name:           'score'
		kind:           .bigint
		nullable:       false
		default_sql:    '0'
		auto_increment: false
	})!
	assert recorder.queries == [
		'ALTER TABLE `accounts` MODIFY COLUMN `score` BIGINT NOT NULL DEFAULT 0;',
	]
}

fn test_mysql_auto_increment_requires_a_key() {
	mut recorder := &RecordingConnection{}
	mut ctx := new_context(recorder, .mysql)
	mut add_error := ''
	ctx.add_column('accounts', Column{
		name:           'sequence'
		kind:           .bigint
		auto_increment: true
	}) or { add_error = err.msg() }
	assert add_error == 'MySQL auto-increment column `sequence` must be a primary key or unique'

	mut create_error := ''
	ctx.create_table(Table{
		name:    'events'
		id:      false
		columns: [
			Column{
				name:           'sequence'
				kind:           .bigint
				auto_increment: true
			},
		]
	}) or { create_error = err.msg() }
	assert create_error == 'MySQL auto-increment column `sequence` must be a primary key or unique'
	assert recorder.queries.len == 0

	ctx.add_column('accounts', Column{
		name:           'sequence'
		kind:           .bigint
		auto_increment: true
		unique:         true
	})!
	assert recorder.queries == [
		'ALTER TABLE `accounts` ADD COLUMN `sequence` BIGINT AUTO_INCREMENT UNIQUE;',
	]
}

fn test_rename_table_rejects_qualified_targets_where_required() {
	mut pg_recorder := &RecordingConnection{}
	mut pg_ctx := new_context(pg_recorder, .pg)
	mut pg_error := ''
	pg_ctx.rename_table('public.users', 'public.new_users') or { pg_error = err.msg() }
	assert pg_error == 'rename_table target `public.new_users` must be unqualified for PostgreSQL and SQLite'
	assert pg_recorder.queries.len == 0
	pg_ctx.rename_table('public.users', 'new_users')!
	assert pg_recorder.queries == [
		'ALTER TABLE "public"."users" RENAME TO "new_users";',
	]

	mut sqlite_recorder := &RecordingConnection{}
	mut sqlite_ctx := new_context(sqlite_recorder, .sqlite)
	mut sqlite_error := ''
	sqlite_ctx.rename_table('main.users', 'main.new_users') or { sqlite_error = err.msg() }
	assert sqlite_error == 'rename_table target `main.new_users` must be unqualified for PostgreSQL and SQLite'
	assert sqlite_recorder.queries.len == 0

	mut mysql_recorder := &RecordingConnection{}
	mut mysql_ctx := new_context(mysql_recorder, .mysql)
	mysql_ctx.rename_table('app.users', 'archive.users')!
	assert mysql_recorder.queries == [
		'ALTER TABLE `app`.`users` RENAME TO `archive`.`users`;',
	]
}

fn test_postgresql_remove_index_uses_the_table_schema() {
	mut recorder := &RecordingConnection{}
	mut ctx := new_context(recorder, .pg)
	ctx.remove_index('archive.users', 'index_archive_users_on_email')!
	ctx.remove_index('archive.users', 'reporting.custom_email_index')!
	ctx.remove_index('users', 'index_users_on_email')!
	assert recorder.queries == [
		'DROP INDEX "archive"."index_archive_users_on_email";',
		'DROP INDEX "reporting"."custom_email_index";',
		'DROP INDEX "index_users_on_email";',
	]
}

fn test_postgresql_add_index_rejects_qualified_names() {
	mut recorder := &RecordingConnection{}
	mut ctx := new_context(recorder, .pg)
	mut error_message := ''
	ctx.add_index(Index{
		table:   'reporting.users'
		columns: ['email']
		name:    'reporting.users_email_idx'
	}) or { error_message = err.msg() }
	assert error_message == 'PostgreSQL add_index name `reporting.users_email_idx` must be unqualified'
	assert recorder.queries.len == 0

	ctx.add_index(Index{
		table:   'reporting.users'
		columns: ['email']
		name:    'users_email_idx'
	})!
	assert recorder.queries == [
		'CREATE INDEX "users_email_idx" ON "reporting"."users" ("email");',
	]
}

fn test_mysql_add_index_rejects_qualified_names() {
	mut recorder := &RecordingConnection{}
	mut ctx := new_context(recorder, .mysql)
	mut error_message := ''
	ctx.add_index(Index{
		table:   'app.users'
		columns: ['email']
		name:    'app.users_email_idx'
	}) or { error_message = err.msg() }
	assert error_message == 'MySQL add_index name `app.users_email_idx` must be unqualified'
	assert recorder.queries.len == 0

	ctx.add_index(Index{
		table:   'app.users'
		columns: ['email']
		name:    'users_email_idx'
	})!
	assert recorder.queries == [
		'CREATE INDEX `users_email_idx` ON `app`.`users` (`email`);',
	]
}

fn test_mysql_foreign_keys_reject_set_default_actions() {
	mut recorder := &RecordingConnection{}
	mut ctx := new_context(recorder, .mysql)
	mut error_message := ''
	ctx.add_foreign_key(ForeignKey{
		from_table: 'accounts'
		column:     'organization_id'
		to_table:   'organizations'
		on_delete:  'set_default'
	}) or { error_message = err.msg() }
	assert error_message == 'MySQL does not support SET DEFAULT for foreign-key actions'

	error_message = ''
	ctx.create_table(Table{
		name:         'accounts'
		id:           false
		columns:      [
			Column{
				name: 'organization_id'
				kind: .bigint
			},
		]
		foreign_keys: [
			ForeignKey{
				from_table: 'accounts'
				column:     'organization_id'
				to_table:   'organizations'
				on_update:  'SET DEFAULT'
			},
		]
	}) or { error_message = err.msg() }
	assert error_message == 'MySQL does not support SET DEFAULT for foreign-key actions'
	assert recorder.queries.len == 0
	assert foreign_key_action(.pg, 'set_default')! == 'SET DEFAULT'
}

fn test_column_sql_rejects_postgresql_serial_defaults_and_scale_without_precision() {
	mut recorder := &RecordingConnection{}
	mut ctx := new_context(recorder, .pg)
	mut error_message := ''
	ctx.add_column('accounts', Column{
		name:           'sequence'
		kind:           .bigint
		auto_increment: true
		default_sql:    '5'
	}) or { error_message = err.msg() }
	assert error_message == 'PostgreSQL auto-increment column `sequence` cannot specify default_sql'

	error_message = ''
	ctx.add_column('accounts', Column{
		name:      'amount'
		kind:      .decimal
		precision: 0
		scale:     2
	}) or { error_message = err.msg() }
	assert error_message == 'decimal scale requires a positive precision'
	assert recorder.queries.len == 0

	assert column_sql(.pg, Column{
		name:           'sequence'
		kind:           .bigint
		primary_key:    true
		auto_increment: true
		default_sql:    ''
	})! == '"sequence" BIGSERIAL PRIMARY KEY'
	assert column_type_sql(.mysql, Column{
		name:      'amount'
		kind:      .decimal
		precision: 8
		scale:     2
	})! == 'DECIMAL(8, 2)'
}

fn test_sqlite_add_column_rejects_unsupported_constraints() {
	mut recorder := &RecordingConnection{}
	mut ctx := new_context(recorder, .sqlite)
	mut error_message := ''
	ctx.add_column('accounts', Column{
		name:        'owner_id'
		kind:        .bigint
		primary_key: true
	}) or { error_message = err.msg() }
	assert error_message == 'SQLite add_column does not support primary-key, unique, or auto-increment columns; rebuild the table in the migration'

	error_message = ''
	ctx.add_column('accounts', Column{
		name:   'email'
		kind:   .text
		unique: true
	}) or { error_message = err.msg() }
	assert error_message == 'SQLite add_column does not support primary-key, unique, or auto-increment columns; rebuild the table in the migration'

	mut invalid_defaults := []?string{}
	invalid_defaults << none
	invalid_defaults << ''
	invalid_defaults << 'NULL'
	for default_sql in invalid_defaults {
		error_message = ''
		ctx.add_column('accounts', Column{
			name:        'label'
			kind:        .text
			nullable:    false
			default_sql: default_sql
		}) or { error_message = err.msg() }
		assert error_message == 'SQLite add_column requires a non-NULL default for a NOT NULL column; rebuild the table in the migration'
	}
	for default_sql in ['CURRENT_TIME', 'CURRENT_DATE', 'CURRENT_TIMESTAMP', "(datetime('now'))"] {
		error_message = ''
		ctx.add_column('accounts', Column{
			name:        'created_at'
			kind:        .timestamp
			default_sql: default_sql
		}) or { error_message = err.msg() }
		assert error_message == 'SQLite add_column does not support nonconstant default `${default_sql}`; rebuild the table in the migration'
	}
	assert recorder.queries.len == 0

	ctx.add_column('accounts', Column{
		name:        'label'
		kind:        .text
		nullable:    false
		default_sql: "''"
	})!
	assert recorder.queries == [
		'ALTER TABLE "accounts" ADD COLUMN "label" TEXT NOT NULL DEFAULT \'\';',
	]
}

fn test_sqlite_requires_unqualified_index_and_foreign_key_tables() {
	mut recorder := &RecordingConnection{}
	mut ctx := new_context(recorder, .sqlite)
	mut error_message := ''
	ctx.add_index(Index{
		table:   'main.users'
		columns: ['email']
	}) or { error_message = err.msg() }
	assert error_message == 'SQLite add_index table `main.users` must be unqualified'
	assert recorder.queries.len == 0

	error_message = ''
	ctx.create_table(Table{
		name:         'main.children'
		id:           false
		columns:      [
			Column{
				name: 'parent_id'
				kind: .bigint
			},
		]
		foreign_keys: [
			ForeignKey{
				from_table: 'main.children'
				column:     'parent_id'
				to_table:   'main.parents'
			},
		]
	}) or { error_message = err.msg() }
	assert error_message == 'SQLite foreign-key target table `main.parents` must be unqualified'
	assert recorder.queries.len == 0

	ctx.add_index(Index{
		table:   'users'
		columns: ['email']
	})!
	ctx.create_table(Table{
		name:         'main.children'
		id:           false
		columns:      [
			Column{
				name: 'parent_id'
				kind: .bigint
			},
		]
		foreign_keys: [
			ForeignKey{
				from_table: 'main.children'
				column:     'parent_id'
				to_table:   'parents'
			},
		]
	})!
	assert recorder.queries == [
		'CREATE INDEX "index_users_on_email" ON "users" ("email");',
		'CREATE TABLE "main"."children" ("parent_id" BIGINT, CONSTRAINT "fk_main_children_parent_id" FOREIGN KEY ("parent_id") REFERENCES "parents" ("id"));',
	]
}

fn test_sqlite_autoincrement_precedes_other_constraints() {
	definition := column_sql(.sqlite, Column{
		name:           'id'
		kind:           .integer
		primary_key:    true
		auto_increment: true
		unique:         true
		default_sql:    '5'
	})!
	assert definition == '"id" INTEGER PRIMARY KEY AUTOINCREMENT UNIQUE DEFAULT 5'
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	db.exec('CREATE TABLE items (${definition});')!
}

fn test_sqlite_non_integer_primary_keys_are_not_null() {
	text_definition := column_sql(.sqlite, Column{
		name:        'code'
		kind:        .text
		primary_key: true
	})!
	assert text_definition == '"code" TEXT PRIMARY KEY NOT NULL'
	integer_definition := column_sql(.sqlite, Column{
		name:        'id'
		kind:        .integer
		primary_key: true
	})!
	assert integer_definition == '"id" INTEGER PRIMARY KEY'

	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	db.exec('CREATE TABLE custom_keys (${text_definition});')!
	columns := db.exec("PRAGMA table_info('custom_keys');")!
	assert columns.len == 1
	assert columns[0].vals[3] == '1'
	db.exec('INSERT INTO custom_keys (code) VALUES (NULL);')!
	rows := db.exec('SELECT count(*) FROM custom_keys;')!
	assert rows[0].vals[0] == '0'
}

fn test_column_level_identifiers_must_be_unqualified() {
	mut recorder := &RecordingConnection{}
	mut ctx := new_context(recorder, .pg)
	mut error_message := ''
	ctx.add_column('users', Column{
		name: 'users.email'
		kind: .text
	}) or { error_message = err.msg() }
	assert error_message == 'column name `users.email` must be unqualified'

	error_message = ''
	ctx.rename_column('users', 'users.email', 'address') or { error_message = err.msg() }
	assert error_message == 'column name `users.email` must be unqualified'

	error_message = ''
	ctx.add_index(Index{
		table:   'public.users'
		columns: ['users.email']
	}) or { error_message = err.msg() }
	assert error_message == 'column name `users.email` must be unqualified'

	error_message = ''
	ctx.add_foreign_key(ForeignKey{
		from_table:  'public.posts'
		column:      'posts.author_id'
		to_table:    'public.users'
		primary_key: 'id'
		name:        'fk_posts_author'
	}) or { error_message = err.msg() }
	assert error_message == 'column name `posts.author_id` must be unqualified'
	assert recorder.queries.len == 0
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
