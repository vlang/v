// vtest build: present_sqlite3? && !sanitize-memory-clang
module migrations

import db.sqlite
import orm

@[heap]
struct RecordingConnection {
mut:
	queries                 []string
	database                string = 'test_database'
	schema                  string = 'public'
	lower_case_table_names  int
	history_rows            []orm.Row
	in_transaction          bool
	postgresql_table_schema string = 'public'
	sqlite_table_schema     string = 'main'
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
	if query == 'USE other_database;' {
		conn.database = 'other_database'
	}
	if query == 'SET search_path TO other_schema;' {
		conn.schema = 'other_schema'
	}
	if query.starts_with('SELECT version, name, applied_at FROM ') {
		return conn.history_rows.clone()
	}
	if query == 'SELECT DATABASE();' {
		return [orm.Row{
			vals: [conn.database]
		}]
	}
	if query == 'SELECT current_schema();' {
		return [orm.Row{
			vals: [conn.schema]
		}]
	}
	if query.starts_with('SELECT n.nspname FROM pg_catalog.pg_class AS c ') {
		return [orm.Row{
			vals: [conn.postgresql_table_schema]
		}]
	}
	if query == 'PRAGMA database_list;' {
		mut rows := [orm.Row{
			vals: ['0', 'main', '']
		}]
		if conn.sqlite_table_schema !in ['temp', 'main'] {
			rows << orm.Row{
				vals: ['2', conn.sqlite_table_schema, '']
			}
		}
		return rows
	}
	if query.starts_with('SELECT 1 FROM "${conn.sqlite_table_schema}".sqlite_schema ') {
		return [orm.Row{
			vals: ['1']
		}]
	}
	if query == 'SELECT @@lower_case_table_names;' {
		return [orm.Row{
			vals: [conn.lower_case_table_names.str()]
		}]
	}
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
	conn.in_transaction = true
}

fn (mut conn RecordingConnection) orm_commit() ! {
	conn.queries << 'ORM COMMIT'
	conn.in_transaction = false
}

fn (mut conn RecordingConnection) orm_rollback() ! {
	conn.queries << 'ORM ROLLBACK'
	conn.in_transaction = false
}

fn (mut conn RecordingConnection) orm_savepoint(name string) ! {
	if !conn.in_transaction {
		return error('savepoint requires an active transaction')
	}
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

fn change_connection_namespace(mut ctx Context) ! {
	match ctx.dialect {
		.sqlite {}
		.pg {
			ctx.execute('SET search_path TO other_schema;')!
		}
		.mysql {
			ctx.execute('USE other_database;')!
		}
	}
}

fn create_sqlite_temp_history_shadow(mut ctx Context) ! {
	ctx.create_table(Table{
		name: 'persistent_from_migration'
	})!
	ctx.execute('CREATE TEMP TABLE IF NOT EXISTS schema_migrations (version BIGINT PRIMARY KEY, name VARCHAR(255) NOT NULL, applied_at VARCHAR(32) NOT NULL);')!
}

fn drop_sqlite_temp_history_shadow(mut ctx Context) ! {
	ctx.drop_table('persistent_from_migration')!
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
		match dialect {
			.sqlite {
				assert recorder.queries[0] == 'BEGIN IMMEDIATE;'
				assert recorder.queries.last() == 'ORM COMMIT'
				assert 'ORM BEGIN' !in recorder.queries
			}
			.pg {
				key := postgresql_migration_lock_key(recorder.schema, 'schema_migrations')
				assert recorder.queries[0] == 'SELECT current_schema();'
				assert recorder.queries[1] == 'SELECT pg_advisory_lock(${key});'
				assert recorder.queries.last() == 'SELECT pg_advisory_unlock(${key});'
				assert 'ORM BEGIN' in recorder.queries
				assert 'ORM COMMIT' in recorder.queries
			}
			.mysql {
				name := mysql_migration_lock_name(recorder.database, 'schema_migrations',
					recorder.lower_case_table_names)
				assert recorder.queries[0] == 'SELECT DATABASE();'
				assert recorder.queries[1] == 'SELECT @@lower_case_table_names;'
				assert recorder.queries[2] == "SELECT GET_LOCK('${name}', ${migration_lock_timeout_seconds});"
				assert recorder.queries.last() == "SELECT RELEASE_LOCK('${name}');"
				assert 'ORM BEGIN' !in recorder.queries
			}
		}
		callback_index := recorder.queries.index('migration callback;')
		assert callback_index > 0
		assert callback_index < recorder.queries.len - 1
	}
}

fn test_locked_history_table_survives_callback_namespace_changes() {
	for dialect in [Dialect.pg, .mysql] {
		migration := Migration{
			version: 1
			name:    'change_namespace'
			up:      change_connection_namespace
			down:    change_connection_namespace
		}
		mut up_recorder := &RecordingConnection{
			database: 'app'
			schema:   'app'
		}
		mut up_runner := new(mut up_recorder, [migration], Config{
			dialect:          dialect
			transaction_mode: .never
		})!
		up_runner.migrate()!
		up_recorder.history_rows = [
			orm.Row{
				vals: ['1', migration.name, '2026-08-16T00:00:00Z']
			},
		]
		assert up_runner.migrate()!.len == 0
		qualified_table := quote_identifier(dialect, 'app.schema_migrations')
		mut saw_insert := false
		for query in up_recorder.queries {
			if query.starts_with('CREATE TABLE IF NOT EXISTS ')
				|| query.starts_with('SELECT version, name, applied_at FROM ')
				|| query.starts_with('INSERT INTO ') {
				assert query.contains(qualified_table)
			}
			if query.starts_with('INSERT INTO ') {
				saw_insert = true
			}
		}
		assert saw_insert
		assert up_recorder.queries.filter(it == if dialect == .pg {
			'SET search_path TO other_schema;'
		} else {
			'USE other_database;'
		}).len == 1
		lock_query := if dialect == .pg {
			key := postgresql_migration_lock_key('app', 'schema_migrations')
			'SELECT pg_advisory_lock(${key});'
		} else {
			name := mysql_migration_lock_name('app', 'schema_migrations', 0)
			"SELECT GET_LOCK('${name}', ${migration_lock_timeout_seconds});"
		}
		assert up_recorder.queries.filter(it == lock_query).len == 2
		assert up_runner.resolved_history_namespace == 'app'
		if dialect == .pg {
			assert up_recorder.schema == 'other_schema'
			assert up_recorder.queries.filter(it == 'SELECT current_schema();').len == 1
		} else {
			assert up_recorder.database == 'other_database'
			assert up_recorder.queries.filter(it == 'SELECT DATABASE();').len == 1
		}

		mut down_recorder := &RecordingConnection{
			database:     'app'
			schema:       'app'
			history_rows: [
				orm.Row{
					vals: ['1', migration.name, '2026-08-16T00:00:00Z']
				},
			]
		}
		mut down_runner := new(mut down_recorder, [migration], Config{
			dialect:          dialect
			transaction_mode: .never
		})!
		down_runner.rollback(1)!
		delete_query := down_recorder.queries.filter(it.starts_with('DELETE FROM '))
		assert delete_query == ['DELETE FROM ${qualified_table} WHERE version = 1;']
	}
}

fn test_postgresql_orm_wrapper_is_rejected_without_mutating_its_transaction() {
	mut recorder := &RecordingConnection{}
	mut scoped := orm.new_db(recorder, orm.DataScope{})
	mut error_message := ''
	new(mut scoped, []Migration{}, Config{
		dialect: .pg
	}) or { error_message = err.msg() }
	assert error_message == 'PostgreSQL migrations require a direct session-pinned connection; orm.DB wrappers cannot be validated without mutating the wrapped connection; pass pg.Conn directly'
	assert recorder.queries.len == 0
}

fn test_postgresql_open_transaction_is_rejected_in_every_mode_without_being_finished() {
	for mode in [TransactionMode.automatic, .always, .never] {
		mut recorder := &RecordingConnection{
			in_transaction: true
		}
		mut runner := new(mut recorder, []Migration{}, Config{
			dialect:          .pg
			transaction_mode: mode
		})!
		mut error_message := ''
		runner.migrate() or { error_message = err.msg() }
		assert error_message == 'PostgreSQL migrations require a connection without an already-open transaction; pg.Tx and transactional pg.Conn values are not supported'
		assert recorder.in_transaction
		assert recorder.queries == [
			'ORM SAVEPOINT v3_migrations_transaction_probe',
			'ORM RELEASE SAVEPOINT v3_migrations_transaction_probe',
		]
	}
}

fn test_sqlite_history_table_is_pinned_to_main() {
	for temp_table_exists in [false, true] {
		mut db := sqlite.connect(':memory:')!
		if temp_table_exists {
			db.exec('CREATE TEMP TABLE schema_migrations (version BIGINT PRIMARY KEY, name VARCHAR(255) NOT NULL, applied_at VARCHAR(32) NOT NULL);')!
			db.exec("INSERT INTO temp.schema_migrations VALUES (1, 'shadow', '2026-08-16T00:00:00Z');")!
		}
		mut runner := new(mut db, [
			Migration{
				version: 1
				name:    'persistent_history'
				up:      create_sqlite_temp_history_shadow
				down:    drop_sqlite_temp_history_shadow
			},
		], Config{
			dialect: .sqlite
		})!
		assert runner.migrate()!.map(it.version) == [i64(1)]
		assert runner.applied()!.map(it.name) == ['persistent_history']
		assert db.q_int("SELECT count(*) FROM main.sqlite_master WHERE type = 'table' AND name = 'persistent_from_migration';")! == 1
		assert db.q_string('SELECT name FROM main.schema_migrations WHERE version = 1;')! == 'persistent_history'
		if temp_table_exists {
			assert db.q_string('SELECT name FROM temp.schema_migrations WHERE version = 1;')! == 'shadow'
		} else {
			assert db.q_int('SELECT count(*) FROM temp.schema_migrations;')! == 0
		}
		db.close()!
	}
}

fn test_migration_names_reject_nul_bytes_before_database_access() {
	mut recorder := &RecordingConnection{}
	new(mut recorder, [
		Migration{
			version: 1
			name:    'before\x00after'
			up:      record_locked_migration
			down:    record_locked_migration
		},
	], Config{}) or {
		assert err.msg() == 'migration 1 name must not contain NUL bytes'
		assert recorder.queries.len == 0
		return
	}
	assert false
}

fn test_mysql_migration_locks_are_namespaced_by_database() {
	mut first_recorder := &RecordingConnection{
		database: 'application_one'
	}
	mut first_runner := new(mut first_recorder, []Migration{}, Config{
		dialect: .mysql
	})!
	first_runner.acquire_migration_lock()!
	first_runner.release_migration_lock(true)!
	first_name := mysql_migration_lock_name('application_one', 'schema_migrations', 0)
	assert first_recorder.queries == [
		'SELECT DATABASE();',
		'SELECT @@lower_case_table_names;',
		"SELECT GET_LOCK('${first_name}', ${migration_lock_timeout_seconds});",
		"SELECT RELEASE_LOCK('${first_name}');",
	]

	mut second_recorder := &RecordingConnection{
		database: 'application_two'
	}
	mut second_runner := new(mut second_recorder, []Migration{}, Config{
		dialect: .mysql
	})!
	second_runner.acquire_migration_lock()!
	second_runner.release_migration_lock(true)!
	second_name := mysql_migration_lock_name('application_two', 'schema_migrations', 0)
	assert second_name != first_name
	assert second_recorder.queries[2] == "SELECT GET_LOCK('${second_name}', ${migration_lock_timeout_seconds});"

	qualified_table := 'shared.schema_migrations'
	qualified_name := mysql_migration_lock_name('shared', qualified_table, 0)
	assert qualified_name == mysql_migration_lock_name('shared', 'schema_migrations', 0)
	mut qualified_first_recorder := &RecordingConnection{
		database: 'application_one'
	}
	mut qualified_first_runner := new(mut qualified_first_recorder, []Migration{}, Config{
		dialect: .mysql
		table:   qualified_table
	})!
	qualified_first_runner.acquire_migration_lock()!
	qualified_first_runner.release_migration_lock(true)!
	assert qualified_first_recorder.queries == [
		'SELECT @@lower_case_table_names;',
		"SELECT GET_LOCK('${qualified_name}', ${migration_lock_timeout_seconds});",
		"SELECT RELEASE_LOCK('${qualified_name}');",
	]

	mut qualified_second_recorder := &RecordingConnection{
		database: 'application_two'
	}
	mut qualified_second_runner := new(mut qualified_second_recorder, []Migration{}, Config{
		dialect: .mysql
		table:   qualified_table
	})!
	qualified_second_runner.acquire_migration_lock()!
	qualified_second_runner.release_migration_lock(true)!
	assert qualified_second_recorder.queries == qualified_first_recorder.queries

	mut unqualified_recorder := &RecordingConnection{
		database: 'shared'
	}
	mut unqualified_runner := new(mut unqualified_recorder, []Migration{}, Config{
		dialect: .mysql
	})!
	unqualified_runner.acquire_migration_lock()!
	unqualified_runner.release_migration_lock(true)!
	assert unqualified_recorder.queries[2..] == qualified_first_recorder.queries[1..]
}

fn test_mysql_migration_locks_follow_lower_case_table_names() {
	assert mysql_migration_lock_name('app', 'app.schema_migrations', 0) != mysql_migration_lock_name('APP',
		'APP.Schema_Migrations', 0)
	for mode in [1, 2] {
		lower_name := mysql_migration_lock_name('app', 'app.schema_migrations', mode)
		upper_name := mysql_migration_lock_name('APP', 'APP.Schema_Migrations', mode)
		assert lower_name == upper_name

		mut lower_recorder := &RecordingConnection{
			lower_case_table_names: mode
		}
		mut lower_runner := new(mut lower_recorder, []Migration{}, Config{
			dialect: .mysql
			table:   'app.schema_migrations'
		})!
		lower_runner.acquire_migration_lock()!
		lower_runner.release_migration_lock(true)!

		mut upper_recorder := &RecordingConnection{
			lower_case_table_names: mode
		}
		mut upper_runner := new(mut upper_recorder, []Migration{}, Config{
			dialect: .mysql
			table:   'APP.Schema_Migrations'
		})!
		upper_runner.acquire_migration_lock()!
		upper_runner.release_migration_lock(true)!
		assert upper_recorder.queries == lower_recorder.queries
		assert lower_recorder.queries == [
			'SELECT @@lower_case_table_names;',
			"SELECT GET_LOCK('${lower_name}', ${migration_lock_timeout_seconds});",
			"SELECT RELEASE_LOCK('${lower_name}');",
		]
	}
}

fn test_postgresql_migration_locks_use_full_64bit_keys() {
	first_table := 's_d015hmpz1cdl'
	second_table := 's_syia3wc6g1qq'
	first_key := postgresql_migration_lock_key('public', first_table)
	second_key := postgresql_migration_lock_key('public', second_table)
	assert first_key != second_key

	mut recorder := &RecordingConnection{}
	mut runner := new(mut recorder, []Migration{}, Config{
		dialect: .pg
		table:   first_table
	})!
	runner.acquire_migration_lock()!
	runner.release_migration_lock(true)!
	assert recorder.queries == [
		'SELECT current_schema();',
		'SELECT pg_advisory_lock(${first_key});',
		'SELECT pg_advisory_unlock(${first_key});',
	]
}

fn test_postgresql_migration_locks_canonicalize_history_table() {
	key := postgresql_migration_lock_key('app', 'schema_migrations')
	assert key == postgresql_migration_lock_key('app', 'app.schema_migrations')

	mut unqualified_recorder := &RecordingConnection{
		schema: 'app'
	}
	mut unqualified_runner := new(mut unqualified_recorder, []Migration{}, Config{
		dialect: .pg
	})!
	unqualified_runner.acquire_migration_lock()!
	unqualified_runner.release_migration_lock(true)!
	assert unqualified_recorder.queries == [
		'SELECT current_schema();',
		'SELECT pg_advisory_lock(${key});',
		'SELECT pg_advisory_unlock(${key});',
	]

	mut qualified_recorder := &RecordingConnection{
		schema: 'unrelated'
	}
	mut qualified_runner := new(mut qualified_recorder, []Migration{}, Config{
		dialect: .pg
		table:   'app.schema_migrations'
	})!
	qualified_runner.acquire_migration_lock()!
	qualified_runner.release_migration_lock(true)!
	assert qualified_recorder.queries == unqualified_recorder.queries[1..]
}

fn test_mysql_history_literals_are_backslash_safe() {
	mut recorder := &RecordingConnection{}
	name := "quote\\'and_trailing\\"
	migration := Migration{
		version: 7
		name:    name
		up:      record_locked_migration
		down:    record_locked_migration
	}
	runner := new(mut recorder, [migration], Config{
		dialect: .mysql
	})!
	applied_at := '2026-08-16T00:00:00Z'
	assert runner.history_insert_sql(migration, applied_at) == "INSERT INTO `schema_migrations` (version, name, applied_at) VALUES (7, X'${name.hex()}', X'${applied_at.hex()}');"
	assert string_literal_sql(.sqlite, "quote'only") == "'quote''only'"
}

fn test_postgresql_history_literals_are_backslash_mode_independent() {
	mut recorder := &RecordingConnection{}
	name := "quote\\'and_trailing\\"
	migration := Migration{
		version: 8
		name:    name
		up:      record_locked_migration
		down:    record_locked_migration
	}
	runner := new(mut recorder, [migration], Config{
		dialect: .pg
	})!
	applied_at := '2026-08-16T00:00:00Z'
	escaped_name := escape_postgresql_literal(name)
	assert escaped_name == "quote\\\\''and_trailing\\\\"
	assert runner.history_insert_sql(migration, applied_at) == 'INSERT INTO "schema_migrations" (version, name, applied_at) VALUES (8, E\'${escaped_name}\', E\'${applied_at}\');'
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

fn test_mysql_change_column_preserves_omitted_auto_increment_key() {
	mut recorder := &RecordingConnection{}
	mut ctx := new_context(recorder, .mysql)
	ctx.change_column('accounts', Column{
		name:           'id'
		kind:           .bigint
		nullable:       false
		default_sql:    ''
		auto_increment: true
	})!
	assert recorder.queries == [
		'ALTER TABLE `accounts` MODIFY COLUMN `id` BIGINT AUTO_INCREMENT NOT NULL;',
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

fn test_mysql_create_table_rejects_multiple_auto_increment_columns() {
	mut recorder := &RecordingConnection{}
	mut ctx := new_context(recorder, .mysql)
	mut error_message := ''
	ctx.create_table(Table{
		name:    'events'
		columns: [
			Column{
				name:           'sequence'
				kind:           .bigint
				auto_increment: true
				unique:         true
			},
		]
	}) or { error_message = err.msg() }
	assert error_message == 'MySQL table `events` cannot have more than one auto-increment column'
	assert recorder.queries.len == 0

	ctx.create_table(Table{
		name:    'single_sequence'
		id:      false
		columns: [
			Column{
				name:           'sequence'
				kind:           .bigint
				auto_increment: true
				unique:         true
			},
		]
	})!
	assert recorder.queries.len == 1
}

fn test_sqlite_and_mysql_reject_case_insensitive_duplicate_columns() {
	for dialect in [Dialect.sqlite, .mysql] {
		mut recorder := &RecordingConnection{}
		mut ctx := new_context(recorder, dialect)
		mut error_message := ''
		ctx.create_table(Table{
			name:    'records'
			columns: [
				Column{
					name: 'ID'
					kind: .bigint
				},
			]
		}) or { error_message = err.msg() }
		assert error_message == 'table `records` has duplicate column `ID`'
		assert recorder.queries.len == 0

		error_message = ''
		ctx.create_table(Table{
			name:    'contacts'
			id:      false
			columns: [
				Column{
					name: 'email'
					kind: .text
				},
				Column{
					name: 'Email'
					kind: .text
				},
			]
		}) or { error_message = err.msg() }
		assert error_message == 'table `contacts` has duplicate column `Email`'
		assert recorder.queries.len == 0
	}

	mut postgres_recorder := &RecordingConnection{}
	mut postgres_ctx := new_context(postgres_recorder, .pg)
	postgres_ctx.create_table(Table{
		name:    'contacts'
		id:      false
		columns: [
			Column{
				name: 'email'
				kind: .text
			},
			Column{
				name: 'Email'
				kind: .text
			},
		]
	})!
	assert postgres_recorder.queries == [
		'CREATE TABLE "contacts" ("email" TEXT, "Email" TEXT);',
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
	mut recorder := &RecordingConnection{
		postgresql_table_schema: 'later_schema'
	}
	mut ctx := new_context(recorder, .pg)
	ctx.remove_index('archive.users', 'index_archive_users_on_email')!
	ctx.remove_index('archive.users', 'reporting.custom_email_index')!
	ctx.remove_index('users', 'index_users_on_email')!
	assert recorder.queries == [
		'DROP INDEX "archive"."index_archive_users_on_email";',
		'DROP INDEX "reporting"."custom_email_index";',
		"SELECT n.nspname FROM pg_catalog.pg_class AS c JOIN pg_catalog.pg_namespace AS n ON n.oid = c.relnamespace WHERE c.relname = E'users' AND c.relkind IN ('r', 'p', 'v', 'm', 'f') AND pg_catalog.pg_table_is_visible(c.oid) LIMIT 1;",
		'DROP INDEX "later_schema"."index_users_on_email";',
	]
}

fn test_sqlite_remove_index_uses_the_table_schema() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	db.exec("ATTACH DATABASE ':memory:' AS aux;")!
	db.exec('CREATE TABLE main.users (email TEXT);')!
	db.exec('CREATE TABLE aux.users (email TEXT);')!
	db.exec('CREATE INDEX main.same_idx ON users (email);')!
	db.exec('CREATE INDEX aux.same_idx ON users (email);')!

	mut ctx := new_context(db, .sqlite)
	ctx.remove_index('aux.users', 'same_idx')!
	assert db.q_int("SELECT count(*) FROM main.sqlite_master WHERE type = 'index' AND name = 'same_idx';")! == 1
	assert db.q_int("SELECT count(*) FROM aux.sqlite_master WHERE type = 'index' AND name = 'same_idx';")! == 0

	db.exec('DROP TABLE main.users;')!
	db.exec('CREATE TABLE main.other (email TEXT);')!
	db.exec('CREATE INDEX main.same_idx ON other (email);')!
	db.exec('CREATE INDEX aux.same_idx ON users (email);')!
	ctx.remove_index('users', 'same_idx')!
	assert db.q_int("SELECT count(*) FROM main.sqlite_master WHERE type = 'index' AND name = 'same_idx';")! == 1
	assert db.q_int("SELECT count(*) FROM aux.sqlite_master WHERE type = 'index' AND name = 'same_idx';")! == 0
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

fn test_mysql_index_names_must_be_unqualified() {
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

	error_message = ''
	ctx.remove_index('app.users', 'app.users_email_idx') or { error_message = err.msg() }
	assert error_message == 'MySQL remove_index name `app.users_email_idx` must be unqualified'
	assert recorder.queries.len == 1

	ctx.remove_index('app.users', 'users_email_idx')!
	assert recorder.queries == [
		'CREATE INDEX `users_email_idx` ON `app`.`users` (`email`);',
		'DROP INDEX `users_email_idx` ON `app`.`users`;',
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
	invalid_defaults << 'NULL /* absent */'
	invalid_defaults << '(NULL)'
	invalid_defaults << '+NULL'
	invalid_defaults << '- /* absent */ NULL'
	invalid_defaults << '((+NULL))'
	invalid_defaults << '(NULL) COLLATE binary'
	invalid_defaults << '((+NULL)) /* absent */ COLLATE binary'
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
	for default_sql in ['CURRENT_TIME', 'CURRENT_DATE', 'CURRENT_TIMESTAMP', "(datetime('now'))",
		'CURRENT_TIMESTAMP /* now */', '/* now */ CURRENT_DATE', 'CURRENT_TIME -- now',
		"(datetime('now')) /* now */", 'CURRENT_TIMESTAMP COLLATE binary',
		'CURRENT_DATE\tCOLLATE binary', 'CURRENT_TIME/* now */ COLLATE binary', '+CURRENT_TIMESTAMP',
		'- CURRENT_DATE', '+/* now */ CURRENT_TIME', '(CURRENT_TIMESTAMP) COLLATE binary',
		'((+CURRENT_DATE)) /* now */ COLLATE binary'] {
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
	ctx.add_column('accounts', Column{
		name:        'literal_comment'
		kind:        .text
		nullable:    false
		default_sql: "'CURRENT_TIMESTAMP /* literal */'"
	})!
	assert recorder.queries[1] == 'ALTER TABLE "accounts" ADD COLUMN "literal_comment" TEXT NOT NULL DEFAULT \'CURRENT_TIMESTAMP /* literal */\';'
	for i, default_sql in ['(0)', "('x')", '(NULL)'] {
		ctx.add_column('accounts', Column{
			name:        'parenthesized_${i}'
			kind:        .text
			default_sql: default_sql
		})!
	}
	assert recorder.queries[2..] == [
		'ALTER TABLE "accounts" ADD COLUMN "parenthesized_0" TEXT DEFAULT (0);',
		'ALTER TABLE "accounts" ADD COLUMN "parenthesized_1" TEXT DEFAULT (\'x\');',
		'ALTER TABLE "accounts" ADD COLUMN "parenthesized_2" TEXT DEFAULT (NULL);',
	]
}

fn test_sqlite_add_column_accepts_parenthesized_literal_defaults() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	db.exec('CREATE TABLE accounts (id INTEGER PRIMARY KEY);')!
	db.exec('INSERT INTO accounts (id) VALUES (1);')!
	mut ctx := new_context(db, .sqlite)
	ctx.add_column('accounts', Column{
		name:        'count'
		kind:        .integer
		default_sql: '(0)'
	})!
	ctx.add_column('accounts', Column{
		name:        'label'
		kind:        .text
		default_sql: "('x')"
	})!
	ctx.add_column('accounts', Column{
		name:        'optional'
		kind:        .text
		default_sql: '(NULL)'
	})!
	assert db.q_int('SELECT count FROM accounts WHERE id = 1;')! == 0
	assert db.q_string('SELECT label FROM accounts WHERE id = 1;')! == 'x'
	assert db.q_int('SELECT count(*) FROM accounts WHERE optional IS NULL;')! == 1
	ctx.add_column('accounts', Column{
		name:        'signed_count'
		kind:        .integer
		default_sql: '(-1)'
	})!
	ctx.add_column('accounts', Column{
		name:        'signed_label'
		kind:        .text
		default_sql: "(+'signed')"
	})!
	assert db.q_int('SELECT signed_count FROM accounts WHERE id = 1;')! == -1
	assert db.q_string('SELECT signed_label FROM accounts WHERE id = 1;')! == 'signed'
	ctx.add_column('accounts', Column{
		name:        'collated_count'
		kind:        .integer
		default_sql: '(0) COLLATE binary'
	})!
	ctx.add_column('accounts', Column{
		name:        'collated_label'
		kind:        .text
		default_sql: "('collated') COLLATE binary"
	})!
	assert db.q_int('SELECT collated_count FROM accounts WHERE id = 1;')! == 0
	assert db.q_string('SELECT collated_label FROM accounts WHERE id = 1;')! == 'collated'
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
	ctx.add_index(Index{
		table:   'users'
		columns: ['email']
		name:    'main.aux.users_email_idx'
	}) or { error_message = err.msg() }
	assert error_message == 'SQLite index name `main.aux.users_email_idx` must not exceed 2 components'
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
		'PRAGMA database_list;',
		'SELECT 1 FROM "temp".sqlite_schema WHERE type = \'table\' AND name = \'users\' COLLATE NOCASE LIMIT 1;',
		'SELECT 1 FROM "main".sqlite_schema WHERE type = \'table\' AND name = \'users\' COLLATE NOCASE LIMIT 1;',
		'CREATE INDEX "main"."index_users_on_email" ON "users" ("email");',
		'CREATE TABLE "main"."children" ("parent_id" BIGINT, CONSTRAINT "fk_main_children_parent_id" FOREIGN KEY ("parent_id") REFERENCES "parents" ("id"));',
	]
}

fn test_sqlite_add_index_uses_the_resolved_table_schema() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	db.exec("ATTACH DATABASE ':memory:' AS aux;")!
	db.exec('CREATE TABLE aux.users (email TEXT);')!
	mut ctx := new_context(db, .sqlite)
	ctx.add_index(Index{
		table:   'users'
		columns: ['email']
	})!
	ctx.add_index(Index{
		table:   'users'
		columns: ['email']
		name:    'aux.custom_users_email_idx'
	})!
	assert db.q_int("SELECT count(*) FROM main.sqlite_master WHERE type = 'index';")! == 0
	assert db.q_int("SELECT count(*) FROM aux.sqlite_master WHERE type = 'index';")! == 2
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
		assert index_name(.sqlite, Index{
			table:   'accounts'
			columns: ['email', 'name']
		})! == 'index_accounts_on_email_and_name'
		return
	}
	assert false
}

fn test_generated_index_names_respect_dialect_limits() {
	index := Index{
		table:   'customer_account_records_archive'
		columns: ['external_customer_reference', 'external_organization_reference']
	}
	raw_name := 'index_customer_account_records_archive_on_external_customer_reference_and_external_organization_reference'
	mysql_name := index_name(.mysql, index)!
	postgres_name := index_name(.pg, index)!
	assert mysql_name.len == 64
	assert postgres_name.len == 63
	assert index_name(.sqlite, index)! == raw_name
	assert index_name(.mysql, index)! == mysql_name
	mut recorder := &RecordingConnection{}
	mut ctx := new_context(recorder, .mysql)
	ctx.add_index(index)!
	assert recorder.queries == [
		'CREATE INDEX `${mysql_name}` ON `customer_account_records_archive` (`external_customer_reference`, `external_organization_reference`);',
	]

	other_mysql_name := index_name(.mysql, Index{
		table:   index.table
		columns: ['external_customer_reference', 'external_organization_identifier']
	})!
	assert other_mysql_name != mysql_name

	long_explicit_name := 'x'.repeat(65)
	mut error_message := ''
	index_name(.mysql, Index{
		table:   'accounts'
		columns: ['email']
		name:    long_explicit_name
	}) or { error_message = err.msg() }
	assert error_message == 'MySQL index name component `${long_explicit_name}` must not exceed 64 bytes'
}

fn test_generated_index_names_distinguish_column_boundaries() {
	single_column := Index{
		table:   'users'
		columns: ['a_and_b']
	}
	composite := Index{
		table:   'users'
		columns: ['a', 'b']
	}
	single_name := index_name(.sqlite, single_column)!
	composite_name := index_name(.sqlite, composite)!
	assert single_name != composite_name
	assert single_name.starts_with('index_users_on_a_and_b_')
	assert composite_name == 'index_users_on_a_and_b'
	case_variant_name := index_name(.mysql, Index{
		table:   'users'
		columns: ['a_AnD_b']
	})!
	assert case_variant_name != index_name(.mysql, composite)!
	table_boundary := Index{
		table:   'a_on_b'
		columns: ['c']
	}
	column_boundary := Index{
		table:   'a'
		columns: ['b_on_c']
	}
	for dialect in [Dialect.sqlite, .pg] {
		assert index_name(dialect, table_boundary)! != index_name(dialect, column_boundary)!
	}

	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	db.exec('CREATE TABLE users (a_and_b TEXT, a TEXT, b TEXT);')!
	mut ctx := new_context(db, .sqlite)
	ctx.add_index(single_column)!
	ctx.add_index(composite)!
	db.exec('CREATE TABLE a_on_b (c TEXT);')!
	db.exec('CREATE TABLE a (b_on_c TEXT);')!
	ctx.add_index(table_boundary)!
	ctx.add_index(column_boundary)!
	assert db.q_int("SELECT count(*) FROM sqlite_master WHERE type = 'index';")! == 4
}

fn test_generated_foreign_key_names_respect_dialect_limits() {
	key := ForeignKey{
		from_table: 'customer_account_records_archive'
		column:     'external_organization_reference'
		to_table:   'organizations'
	}
	raw_name := 'fk_customer_account_records_archive_external_organization_reference'
	mysql_name := foreign_key_name(.mysql, key)!
	postgres_name := foreign_key_name(.pg, key)!
	assert mysql_name.len == 64
	assert postgres_name.len == 63
	assert foreign_key_name(.sqlite, key)! == raw_name
	assert foreign_key_name(.mysql, key)! == mysql_name

	other_mysql_name := foreign_key_name(.mysql, ForeignKey{
		from_table: key.from_table
		column:     'external_organization_identifier'
		to_table:   key.to_table
	})!
	assert other_mysql_name != mysql_name

	mut recorder := &RecordingConnection{}
	mut ctx := new_context(recorder, .mysql)
	ctx.add_foreign_key(key)!
	ctx.create_table(Table{
		name:         key.from_table
		id:           false
		columns:      [
			Column{
				name: key.column
				kind: .bigint
			},
		]
		foreign_keys: [key]
	})!
	assert recorder.queries[0].contains('CONSTRAINT `${mysql_name}` FOREIGN KEY')
	assert recorder.queries[1].contains('CONSTRAINT `${mysql_name}` FOREIGN KEY')

	long_explicit_name := 'x'.repeat(65)
	mut error_message := ''
	ctx.add_foreign_key(ForeignKey{
		from_table: 'accounts'
		column:     'organization_id'
		to_table:   'organizations'
		name:       long_explicit_name
	}) or { error_message = err.msg() }
	assert error_message == 'MySQL foreign key name `${long_explicit_name}` must not exceed 64 bytes'
	assert recorder.queries.len == 2
}

fn test_generated_mysql_foreign_key_names_distinguish_component_boundaries() {
	first := ForeignKey{
		from_table: 'a_b'
		column:     'c'
		to_table:   'parents'
	}
	second := ForeignKey{
		from_table: 'a'
		column:     'b_c'
		to_table:   'parents'
	}
	first_name := foreign_key_name(.mysql, first)!
	second_name := foreign_key_name(.mysql, second)!
	assert first_name.starts_with('fk_a_b_c_')
	assert second_name.starts_with('fk_a_b_c_')
	assert first_name != second_name

	mut recorder := &RecordingConnection{}
	mut ctx := new_context(recorder, .mysql)
	ctx.add_foreign_key(first)!
	ctx.add_foreign_key(second)!
	assert recorder.queries[0].contains('CONSTRAINT `${first_name}` FOREIGN KEY')
	assert recorder.queries[1].contains('CONSTRAINT `${second_name}` FOREIGN KEY')
}

fn test_generated_foreign_key_names_include_targets() {
	parent_key := ForeignKey{
		from_table: 'accounts'
		column:     'owner_id'
		to_table:   'parents'
	}
	other_table := ForeignKey{
		...parent_key
		to_table: 'guardians'
	}
	other_primary_key := ForeignKey{
		...parent_key
		primary_key: 'uuid'
	}
	for dialect in [Dialect.pg, .mysql] {
		parent_name := foreign_key_name(dialect, parent_key)!
		other_table_name := foreign_key_name(dialect, other_table)!
		other_primary_key_name := foreign_key_name(dialect, other_primary_key)!
		assert parent_name != other_table_name
		assert parent_name != other_primary_key_name
		assert other_table_name != other_primary_key_name

		mut recorder := &RecordingConnection{}
		mut ctx := new_context(recorder, dialect)
		ctx.create_table(Table{
			name:         'accounts'
			id:           false
			columns:      [
				Column{
					name: 'owner_id'
					kind: .bigint
				},
			]
			foreign_keys: [parent_key, other_table, other_primary_key]
		})!
		assert recorder.queries[0].contains('CONSTRAINT ${quote_identifier(dialect, parent_name)} FOREIGN KEY')
		assert recorder.queries[0].contains('CONSTRAINT ${quote_identifier(dialect,
			other_table_name)} FOREIGN KEY')
		assert recorder.queries[0].contains('CONSTRAINT ${quote_identifier(dialect,
			other_primary_key_name)} FOREIGN KEY')
	}
}

fn test_caller_supplied_identifiers_respect_dialect_limits() {
	mysql_limit_name := 'm'.repeat(64)
	mysql_long_name := 'm'.repeat(65)
	mut mysql_recorder := &RecordingConnection{}
	mut mysql_ctx := new_context(mysql_recorder, .mysql)
	mysql_ctx.create_table(Table{
		name:    mysql_limit_name
		id:      false
		columns: [
			Column{
				name: mysql_limit_name
				kind: .text
			},
		]
	})!
	assert mysql_recorder.queries.len == 1

	mut error_message := ''
	mysql_ctx.create_table(Table{
		name: 'app.${mysql_long_name}'
	}) or { error_message = err.msg() }
	assert error_message == 'MySQL table name component `${mysql_long_name}` must not exceed 64 bytes'
	assert mysql_recorder.queries.len == 1

	error_message = ''
	mysql_ctx.add_column(mysql_limit_name, Column{
		name: mysql_long_name
		kind: .text
	}) or { error_message = err.msg() }
	assert error_message == 'MySQL column name component `${mysql_long_name}` must not exceed 64 bytes'
	assert mysql_recorder.queries.len == 1

	postgres_limit_name := 'p'.repeat(63)
	postgres_long_name := 'p'.repeat(64)
	mut postgres_recorder := &RecordingConnection{}
	mut postgres_ctx := new_context(postgres_recorder, .pg)
	postgres_ctx.drop_table(postgres_limit_name)!
	error_message = ''
	postgres_ctx.drop_table(postgres_long_name) or { error_message = err.msg() }
	assert error_message == 'PostgreSQL table name component `${postgres_long_name}` must not exceed 63 bytes'
	assert postgres_recorder.queries.len == 1

	mut history_recorder := &RecordingConnection{}
	runner := new(mut history_recorder, []Migration{}, Config{
		dialect: .mysql
		table:   '${mysql_limit_name}.${mysql_limit_name}'
	})!
	assert runner.config.table == '${mysql_limit_name}.${mysql_limit_name}'
	error_message = ''
	new(mut history_recorder, []Migration{}, Config{
		dialect: .mysql
		table:   'app.${mysql_long_name}'
	}) or { error_message = err.msg() }
	assert error_message == 'MySQL migration history table name component `${mysql_long_name}` must not exceed 64 bytes'
	assert history_recorder.queries.len == 0

	error_message = ''
	mysql_ctx.drop_table('application.archive.users') or { error_message = err.msg() }
	assert error_message == 'MySQL table name `application.archive.users` must not exceed 2 components'
	assert mysql_recorder.queries.len == 1

	mut sqlite_recorder := &RecordingConnection{}
	mut sqlite_ctx := new_context(sqlite_recorder, .sqlite)
	error_message = ''
	sqlite_ctx.create_table(Table{
		name: 'main.application.users'
	}) or { error_message = err.msg() }
	assert error_message == 'SQLite table name `main.application.users` must not exceed 2 components'
	assert sqlite_recorder.queries.len == 0

	error_message = ''
	new(mut history_recorder, []Migration{}, Config{
		dialect: .mysql
		table:   'application.archive.schema_migrations'
	}) or { error_message = err.msg() }
	assert error_message == 'MySQL migration history table name `application.archive.schema_migrations` must not exceed 2 components'
	assert history_recorder.queries.len == 0
}
