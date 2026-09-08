// vtest build: present_sqlite3? && !sanitize-memory-clang
module migrations

import db.sqlite
import orm

@[heap]
struct RecordingConnection {
mut:
	queries                   []string
	database                  string = 'test_database'
	schema                    string = 'public'
	lower_case_table_names    int
	history_rows              []orm.Row
	in_transaction            bool
	autocommit                bool = true
	savepoints                []string
	savepoint_probe_rows      map[string]int
	sqlite_probe_rows         int
	postgresql_history_schema string
	postgresql_table_schema   string = 'public'
	postgresql_probe_value    string
	postgresql_aborted        bool
	postgresql_lock_held      bool
	postgresql_xact_lock_held bool
	mysql_lock_held           bool
	history_metadata_error    string
	history_version_error     string
	sqlite_table_schema       string = 'main'
	sqlite_version            string = '3.46.0'
	fail_sqlite_lock          bool
	fail_sqlite_commit        bool
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
	if query == 'BEGIN IMMEDIATE;' && conn.fail_sqlite_lock {
		return error('database is locked')
	}
	if query == 'USE other_database;' {
		conn.database = 'other_database'
	}
	if query == 'SET autocommit=0;' {
		conn.autocommit = false
	}
	if query == 'START TRANSACTION;' {
		conn.in_transaction = true
		conn.clear_savepoints()
	}
	if query in ['BEGIN;', 'BEGIN IMMEDIATE;'] {
		if conn.in_transaction {
			return error('cannot start a transaction within a transaction')
		}
		conn.in_transaction = true
		conn.clear_savepoints()
	}
	if query in ['ROLLBACK;', 'COMMIT;'] {
		conn.in_transaction = false
		conn.postgresql_probe_value = ''
		conn.postgresql_aborted = false
		conn.postgresql_xact_lock_held = false
		conn.sqlite_probe_rows = 0
		conn.clear_savepoints()
	}
	if query == 'force PostgreSQL statement error;' {
		conn.postgresql_aborted = true
		return error('forced PostgreSQL statement error')
	}
	if conn.postgresql_aborted {
		return error('current PostgreSQL transaction is aborted')
	}
	if query.starts_with('SAVEPOINT ') {
		if !conn.in_transaction {
			conn.in_transaction = true
		}
		conn.add_savepoint(query.all_after('SAVEPOINT ').all_before(';'))
	}
	if query.starts_with('RELEASE SAVEPOINT ') {
		conn.release_savepoint(query.all_after('RELEASE SAVEPOINT ').all_before(';'))
	}
	if query.starts_with('INSERT INTO temp."v3_migrations_transaction_') {
		conn.sqlite_probe_rows++
	}
	if query.starts_with('ROLLBACK TO SAVEPOINT ') {
		conn.rollback_to_savepoint(query.all_after('ROLLBACK TO SAVEPOINT ').all_before(';'))
	}
	if query.starts_with('SELECT COUNT(*) FROM temp."v3_migrations_transaction_') {
		return [orm.Row{
			vals: [conn.sqlite_probe_rows.str()]
		}]
	}
	if query.starts_with('DELETE FROM temp."v3_migrations_transaction_') {
		conn.sqlite_probe_rows = 0
	}
	if query == 'SET search_path TO other_schema;' {
		conn.schema = 'other_schema'
	}
	probe_prefix := "SELECT pg_catalog.set_config('${postgresql_transaction_probe_setting}', '"
	if query.starts_with(probe_prefix) {
		value := query.all_after(probe_prefix).all_before("', true);")
		if conn.in_transaction {
			conn.postgresql_probe_value = value
		}
		return [orm.Row{
			vals: [value]
		}]
	}
	if query == postgresql_transaction_probe_read_query() {
		return [orm.Row{
			vals: [conn.postgresql_probe_value]
		}]
	}
	if query.starts_with('SELECT pg_advisory_lock(') {
		conn.postgresql_lock_held = true
	}
	if query.starts_with('SELECT pg_advisory_xact_lock(') && conn.in_transaction {
		conn.postgresql_xact_lock_held = true
	}
	if query == 'SELECT pg_advisory_unlock_all();' {
		conn.postgresql_lock_held = false
	}
	if query.starts_with('WITH transaction_guard AS (SELECT pg_catalog.pg_try_advisory_xact_lock(') {
		return [orm.Row{
			vals: [if conn.postgresql_lock_held { 't' } else { 'f' }]
		}]
	}
	if query.starts_with('SELECT version, name, applied_at FROM ') {
		if conn.history_metadata_error != '' {
			return error(conn.history_metadata_error)
		}
		return conn.history_rows.clone()
	}
	if query.starts_with('SELECT version FROM ') {
		if conn.history_version_error != '' {
			return error(conn.history_version_error)
		}
		return conn.history_rows.clone()
	}
	if query == 'SELECT DATABASE();' {
		return [orm.Row{
			vals: [conn.database]
		}]
	}
	if query.starts_with('WITH persistent_search_path AS (') {
		return [
			orm.Row{
				vals: [if conn.postgresql_history_schema == '' {
					conn.schema
				} else {
					conn.postgresql_history_schema
				}]
			},
		]
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
	if query == 'SELECT @@autocommit;' {
		return [orm.Row{
			vals: [if conn.autocommit { '1' } else { '0' }]
		}]
	}
	if query == 'SELECT sqlite_version();' {
		return [orm.Row{
			vals: [conn.sqlite_version]
		}]
	}
	if query.starts_with('SELECT pg_advisory_unlock(') {
		was_held := conn.postgresql_lock_held
		conn.postgresql_lock_held = false
		return [orm.Row{
			vals: [if was_held { 't' } else { 'f' }]
		}]
	}
	if query.starts_with('SELECT GET_LOCK(') {
		conn.mysql_lock_held = true
		return [orm.Row{
			vals: ['1']
		}]
	}
	if query == 'SELECT RELEASE_ALL_LOCKS();' {
		was_held := conn.mysql_lock_held
		conn.mysql_lock_held = false
		return [orm.Row{
			vals: [if was_held { '1' } else { '0' }]
		}]
	}
	if query.starts_with('SELECT COALESCE(IS_USED_LOCK(') {
		return [orm.Row{
			vals: [if conn.mysql_lock_held { '1' } else { '0' }]
		}]
	}
	if query.starts_with('SELECT RELEASE_LOCK(') {
		was_held := conn.mysql_lock_held
		conn.mysql_lock_held = false
		return [orm.Row{
			vals: [if was_held { '1' } else { '0' }]
		}]
	}
	return []
}

fn (mut conn RecordingConnection) orm_begin() ! {
	conn.queries << 'ORM BEGIN'
	conn.in_transaction = true
	conn.postgresql_aborted = false
}

fn (mut conn RecordingConnection) orm_commit() ! {
	conn.queries << 'ORM COMMIT'
	if conn.fail_sqlite_commit {
		return error('FOREIGN KEY constraint failed')
	}
	conn.in_transaction = false
	conn.postgresql_probe_value = ''
	conn.postgresql_aborted = false
	conn.postgresql_xact_lock_held = false
	conn.clear_savepoints()
}

fn (mut conn RecordingConnection) orm_rollback() ! {
	conn.queries << 'ORM ROLLBACK'
	conn.in_transaction = false
	conn.postgresql_probe_value = ''
	conn.postgresql_aborted = false
	conn.postgresql_xact_lock_held = false
	conn.sqlite_probe_rows = 0
	conn.clear_savepoints()
}

fn (mut conn RecordingConnection) orm_savepoint(name string) ! {
	if !conn.in_transaction {
		return error('savepoint requires an active transaction')
	}
	conn.queries << 'ORM SAVEPOINT ${name}'
	conn.add_savepoint(name)
}

fn (mut conn RecordingConnection) orm_rollback_to(name string) ! {
	if !conn.rollback_to_savepoint(name) {
		return error('savepoint `${name}` does not exist')
	}
	conn.queries << 'ORM ROLLBACK TO ${name}'
}

fn (mut conn RecordingConnection) orm_release_savepoint(name string) ! {
	if !conn.release_savepoint(name) {
		return error('savepoint `${name}` does not exist')
	}
	conn.queries << 'ORM RELEASE SAVEPOINT ${name}'
}

fn (mut conn RecordingConnection) add_savepoint(name string) {
	conn.savepoints << name
	conn.savepoint_probe_rows[name] = conn.sqlite_probe_rows
}

fn (mut conn RecordingConnection) release_savepoint(name string) bool {
	index := conn.savepoints.index(name)
	if index < 0 {
		return false
	}
	for savepoint in conn.savepoints[index..] {
		conn.savepoint_probe_rows.delete(savepoint)
	}
	conn.savepoints = conn.savepoints[..index].clone()
	return true
}

fn (mut conn RecordingConnection) rollback_to_savepoint(name string) bool {
	index := conn.savepoints.index(name)
	if index < 0 {
		return false
	}
	conn.sqlite_probe_rows = conn.savepoint_probe_rows[name]
	for savepoint in conn.savepoints[index + 1..] {
		conn.savepoint_probe_rows.delete(savepoint)
	}
	conn.savepoints = conn.savepoints[..index + 1].clone()
	return true
}

fn (mut conn RecordingConnection) clear_savepoints() {
	conn.savepoints.clear()
	conn.savepoint_probe_rows.clear()
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
		name: 'accounts'
		columns: [
			Column{
				name: 'email'
				kind: .varchar
				nullable: false
			},
			Column{
				name: 'organization_id'
				kind: .bigint
				nullable: false
			},
		]
		foreign_keys: [
			ForeignKey{
				from_table: 'accounts'
				column: 'organization_id'
				to_table: 'organizations'
				on_delete: 'cascade'
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
		name: 'name'
		kind: .text
		default_sql: "''"
		nullable: false
	})!
	ctx.add_index(Index{
		table: 'accounts'
		columns: ['name']
		name: 'index_accounts_on_name'
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

fn disable_mysql_autocommit(mut ctx Context) ! {
	ctx.execute('SET autocommit=0;')!
}

fn start_mysql_transaction(mut ctx Context) ! {
	ctx.execute('START TRANSACTION;')!
}

fn unlock_all_mysql_named_locks(mut ctx Context) ! {
	ctx.execute('SELECT RELEASE_ALL_LOCKS();')!
}

fn unlock_mysql_migration_lock(mut ctx Context) ! {
	name := mysql_migration_lock_name('test_database', 'schema_migrations', 0)
	ctx.execute("SELECT RELEASE_LOCK('${name}');")!
}

fn start_postgresql_transaction(mut ctx Context) ! {
	ctx.execute('BEGIN;')!
}

fn abort_postgresql_transaction_and_catch_error(mut ctx Context) ! {
	ctx.execute('BEGIN;')!
	ctx.execute('force PostgreSQL statement error;') or { return }
	return error('expected the PostgreSQL statement to fail')
}

fn rollback_callback_transaction(mut ctx Context) ! {
	ctx.execute('ROLLBACK;')!
}

fn no_op_migration(mut ctx Context) ! {
	_ = ctx.dialect
}

fn unlock_all_postgresql_advisory_locks(mut ctx Context) ! {
	ctx.execute('SELECT pg_advisory_unlock_all();')!
}

fn unlock_postgresql_migration_lock(mut ctx Context) ! {
	key := postgresql_migration_lock_key('public', 'schema_migrations')
	ctx.execute('SELECT pg_advisory_unlock(${key});')!
}

fn replace_postgresql_migration_lock_with_transaction_lock(mut ctx Context) ! {
	key := postgresql_migration_lock_key('public', 'schema_migrations')
	ctx.execute('SELECT pg_advisory_unlock(${key});')!
	ctx.execute('SELECT pg_advisory_xact_lock(${key});')!
}

fn create_sqlite_table_then_rollback(mut ctx Context) ! {
	ctx.create_table(Table{
		name: 'rolled_back_callback_table'
	})!
	ctx.execute('ROLLBACK;')!
}

fn drop_sqlite_table_then_rollback(mut ctx Context) ! {
	ctx.drop_table('rolled_back_callback_table')!
	ctx.execute('ROLLBACK;')!
}

fn create_sqlite_table_then_replace_transaction(mut ctx Context) ! {
	ctx.create_table(Table{
		name: 'replaced_transaction_table'
	})!
	ctx.execute('ROLLBACK;')!
	ctx.execute('BEGIN;')!
}

fn assert_postgresql_transaction_probe(queries []string) {
	assert queries.len >= 2
	assert queries[0].starts_with("SELECT pg_catalog.set_config('${postgresql_transaction_probe_setting}', 'probe_")
	assert queries[1] == postgresql_transaction_probe_read_query()
}

fn test_migrate_rollback_redo_and_status() {
	mut db := sqlite.connect(':memory:')!
	db.exec('PRAGMA foreign_keys = ON;')!
	defer {
		db.close() or {}
	}
	mut runner := new(mut db, [
		Migration{
			version: 202608160001
			name: 'create_accounts'
			up: create_accounts
			down: drop_accounts
		},
		Migration{
			version: 202608160002
			name: 'add_account_name'
			up: add_account_name
			down: remove_account_name
		},
	], Config{ dialect: .sqlite })!

	assert runner.pending()!.map(it.version) == [i64(202608160001), 202608160002]
	applied := runner.migrate()!
	assert applied.map(it.name) == ['create_accounts', 'add_account_name']
	assert runner.current_version()! == 202608160002
	assert db.q_int("SELECT count(*) FROM pragma_table_info('accounts') WHERE name = 'name';")! == 1
	assert db.q_int("SELECT count(*) FROM pragma_foreign_key_list('accounts') WHERE `from` = 'organization_id';")! == 1
	foreign_key_code := db.exec_none("INSERT INTO accounts (email, organization_id) VALUES ('orphan@example.com', 999);")
	assert foreign_key_code == int(sqlite.Result.constraint)
	assert db.q_int('SELECT count(*) FROM accounts;')! == 0
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
	db.exec("INSERT INTO schema_migrations (version) VALUES ('99');")!
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
			name: 'fail_after_create'
			up: fail_after_create
			down: drop_should_rollback
		},
	], Config{ dialect: .sqlite })!

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
			name: 'create_widget_with_orm_dsl'
			up: create_widget_with_orm_dsl
			down: drop_widget_with_orm_dsl
		},
	], Config{ dialect: .sqlite })!

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
				name: 'locked'
				up: record_locked_migration
				down: record_locked_migration
			},
		], Config{
			dialect: dialect
		})!
		runner.migrate()!
		match dialect {
			.sqlite {
				assert recorder.queries[0].starts_with('CREATE TEMP TABLE temp."v3_migrations_transaction_')
				assert recorder.queries[1] == 'BEGIN IMMEDIATE;'
				assert recorder.queries.last().starts_with('DROP TABLE IF EXISTS temp."v3_migrations_transaction_')
				assert 'ORM BEGIN' !in recorder.queries
				assert 'ORM COMMIT' in recorder.queries
			}
			.pg {
				key := postgresql_migration_lock_key(recorder.schema, 'schema_migrations')
				assert recorder.queries[0].starts_with("SELECT pg_catalog.set_config('${postgresql_transaction_probe_setting}', 'probe_")
				assert recorder.queries[1] == postgresql_transaction_probe_read_query()
				assert recorder.queries[2] == postgresql_history_schema_query('schema_migrations')
				assert recorder.queries[3] == 'SELECT pg_advisory_lock(${key});'
				assert recorder.queries.last() == 'SELECT pg_advisory_unlock(${key});'
				assert 'ORM BEGIN' in recorder.queries
				assert 'ORM COMMIT' in recorder.queries
			}
			.mysql {
				name := mysql_migration_lock_name(recorder.database, 'schema_migrations', recorder.lower_case_table_names)
				assert recorder.queries[0] == 'SELECT @@autocommit;'
				assert recorder.queries[1] == 'SELECT DATABASE();'
				assert recorder.queries[2] == 'SELECT @@lower_case_table_names;'
				assert recorder.queries[3] == "SELECT GET_LOCK('${name}', ${migration_lock_timeout_seconds});"
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
			name: 'change_namespace'
			up: change_connection_namespace
			down: change_connection_namespace
		}
		mut up_recorder := &RecordingConnection{
			database: 'app'
			schema: 'app'
		}
		mut up_runner := new(mut up_recorder, [migration], Config{
			dialect: dialect
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
			assert up_recorder.queries.filter(it == postgresql_history_schema_query('schema_migrations')).len == 1
		} else {
			assert up_recorder.database == 'other_database'
			assert up_recorder.queries.filter(it == 'SELECT DATABASE();').len == 1
		}

		mut down_recorder := &RecordingConnection{
			database: 'app'
			schema: 'app'
			history_rows: [
				orm.Row{
					vals: ['1', migration.name, '2026-08-16T00:00:00Z']
				},
			]
		}
		mut down_runner := new(mut down_recorder, [migration], Config{
			dialect: dialect
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
			savepoints: ['v3_migrations_transaction_probe']
		}
		mut runner := new(mut recorder, []Migration{}, Config{
			dialect: .pg
			transaction_mode: mode
		})!
		mut error_message := ''
		runner.migrate() or { error_message = err.msg() }
		assert error_message == 'PostgreSQL migrations require a connection without an already-open transaction; pg.Tx and transactional pg.Conn values are not supported'
		assert recorder.in_transaction
		assert recorder.queries.len == 2
		assert recorder.queries[0].starts_with("SELECT pg_catalog.set_config('${postgresql_transaction_probe_setting}', 'probe_")
		assert recorder.queries[1] == postgresql_transaction_probe_read_query()
	}
}

fn test_mysql_open_transaction_is_rejected_in_every_mode_without_being_finished() {
	for mode in [TransactionMode.automatic, .always, .never] {
		mut recorder := &RecordingConnection{
			in_transaction: true
		}
		mut runner := new(mut recorder, []Migration{}, Config{
			dialect: .mysql
			transaction_mode: mode
		})!
		mut error_message := ''
		runner.migrate() or { error_message = err.msg() }
		assert error_message == 'MySQL migrations require a connection without an already-open transaction'
		assert recorder.in_transaction
		assert recorder.queries.len == 3
		assert recorder.queries[0] == 'SELECT @@autocommit;'
		assert recorder.queries[1].starts_with('ORM SAVEPOINT v3_migrations_transaction_probe_')
		probe := recorder.queries[1].all_after('ORM SAVEPOINT ')
		assert probe != 'v3_migrations_transaction_probe'
		assert recorder.queries[2] == 'ORM RELEASE SAVEPOINT ${probe}'
	}
}

fn test_mysql_disabled_autocommit_is_rejected_before_locking_or_inspection() {
	for mode in [TransactionMode.automatic, .always, .never] {
		mut recorder := &RecordingConnection{
			autocommit: false
		}
		mut runner := new(mut recorder, []Migration{}, Config{
			dialect: .mysql
			transaction_mode: mode
		})!
		mut error_message := ''
		runner.migrate() or { error_message = err.msg() }
		assert error_message == 'MySQL migrations require session autocommit to be enabled'
		assert recorder.queries == ['SELECT @@autocommit;']

		error_message = ''
		runner.applied() or { error_message = err.msg() }
		assert error_message == 'MySQL migrations require session autocommit to be enabled'
		assert recorder.queries == ['SELECT @@autocommit;', 'SELECT @@autocommit;']
	}
}

fn test_mysql_callback_session_state_is_rechecked_before_unlocking() {
	for mode in [TransactionMode.automatic, .never] {
		mut autocommit_recorder := &RecordingConnection{}
		mut autocommit_runner := new(mut autocommit_recorder, [
			Migration{
				version: 1
				name: 'disable_autocommit'
				up: disable_mysql_autocommit
				down: disable_mysql_autocommit
			},
		], Config{
			dialect: .mysql
			transaction_mode: mode
		})!
		mut error_message := ''
		autocommit_runner.migrate() or { error_message = err.msg() }
		assert error_message == 'MySQL migration callback left unsafe session state: MySQL migrations require session autocommit to be enabled'
		assert !autocommit_recorder.in_transaction
		assert !autocommit_recorder.autocommit
		rollback_index := autocommit_recorder.queries.index('ORM ROLLBACK')
		history_inserts := autocommit_recorder.queries.filter(it.starts_with('INSERT INTO '))
		assert history_inserts.len == 1
		assert rollback_index > autocommit_recorder.queries.index('SET autocommit=0;')
		assert rollback_index > autocommit_recorder.queries.index(history_inserts[0])
		assert autocommit_recorder.queries.last().starts_with('SELECT RELEASE_LOCK(')

		mut transaction_recorder := &RecordingConnection{}
		mut transaction_runner := new(mut transaction_recorder, [
			Migration{
				version: 1
				name: 'start_transaction'
				up: start_mysql_transaction
				down: start_mysql_transaction
			},
		], Config{
			dialect: .mysql
			transaction_mode: mode
		})!
		error_message = ''
		transaction_runner.migrate() or { error_message = err.msg() }
		assert error_message == 'MySQL migration callback left unsafe session state: MySQL migrations require a connection without an already-open transaction'
		assert !transaction_recorder.in_transaction
		assert transaction_recorder.autocommit
		transaction_rollback_index := transaction_recorder.queries.index('ORM ROLLBACK')
		transaction_history_inserts :=
			transaction_recorder.queries.filter(it.starts_with('INSERT INTO '))
		assert transaction_history_inserts.len == 1
		assert transaction_rollback_index > transaction_recorder.queries.index('START TRANSACTION;')
		assert transaction_rollback_index > transaction_recorder.queries.index(transaction_history_inserts[0])
		assert transaction_recorder.queries.last().starts_with('SELECT RELEASE_LOCK(')
	}
}

fn test_postgresql_callback_transactions_are_rolled_back_before_unlocking() {
	migration := Migration{
		version: 1
		name: 'start_transaction'
		up: start_postgresql_transaction
		down: start_postgresql_transaction
	}
	mut up_recorder := &RecordingConnection{}
	mut up_runner := new(mut up_recorder, [migration], Config{
		dialect: .pg
		transaction_mode: .never
	})!
	mut error_message := ''
	up_runner.migrate() or { error_message = err.msg() }
	assert error_message == 'PostgreSQL migration callback left unsafe session state: PostgreSQL migrations require a connection without an already-open transaction; pg.Tx and transactional pg.Conn values are not supported'
	assert !up_recorder.in_transaction
	up_rollback_index := up_recorder.queries.index('ORM ROLLBACK')
	history_inserts := up_recorder.queries.filter(it.starts_with('INSERT INTO '))
	assert history_inserts.len == 1
	assert up_rollback_index > up_recorder.queries.index('BEGIN;')
	assert up_rollback_index > up_recorder.queries.index(history_inserts[0])
	key := postgresql_migration_lock_key('public', 'schema_migrations')
	assert up_recorder.queries.last() == 'SELECT pg_advisory_unlock(${key});'

	mut down_recorder := &RecordingConnection{
		history_rows: [
			orm.Row{
				vals: ['1', migration.name, '2026-08-16T00:00:00Z']
			},
		]
	}
	mut down_runner := new(mut down_recorder, [migration], Config{
		dialect: .pg
		transaction_mode: .never
	})!
	error_message = ''
	down_runner.rollback(1) or { error_message = err.msg() }
	assert error_message == 'PostgreSQL migration callback left unsafe session state: PostgreSQL migrations require a connection without an already-open transaction; pg.Tx and transactional pg.Conn values are not supported'
	assert !down_recorder.in_transaction
	down_rollback_index := down_recorder.queries.index('ORM ROLLBACK')
	history_deletes := down_recorder.queries.filter(it.starts_with('DELETE FROM '))
	assert history_deletes.len == 1
	assert down_rollback_index > down_recorder.queries.index('BEGIN;')
	assert down_rollback_index > down_recorder.queries.index(history_deletes[0])
	assert down_recorder.queries.last() == 'SELECT pg_advisory_unlock(${key});'
}

fn test_postgresql_lock_verification_errors_rollback_callback_transaction_state() {
	migration := Migration{
		version: 1
		name: 'aborted_transaction'
		up: abort_postgresql_transaction_and_catch_error
		down: abort_postgresql_transaction_and_catch_error
	}
	mut up_recorder := &RecordingConnection{}
	mut up_runner := new(mut up_recorder, [migration], Config{
		dialect: .pg
		transaction_mode: .never
	})!
	mut error_message := ''
	up_runner.migrate() or { error_message = err.msg() }
	assert error_message.contains('current PostgreSQL transaction is aborted')
	assert error_message.contains('PostgreSQL migration callback left unsafe session state')
	assert !up_recorder.in_transaction
	assert !up_recorder.postgresql_aborted
	up_inserts := up_recorder.queries.filter(it.starts_with('INSERT INTO '))
	assert up_inserts.len == 0
	key := postgresql_migration_lock_key('public', 'schema_migrations')
	lock_check := postgresql_migration_lock_owned_query(key)
	lock_check_index := up_recorder.queries.index(lock_check)
	assert lock_check_index > up_recorder.queries.index('force PostgreSQL statement error;')
	assert up_recorder.queries.index('ORM ROLLBACK') > lock_check_index
	assert up_recorder.queries.last() == 'SELECT pg_advisory_unlock(${key});'

	mut down_recorder := &RecordingConnection{
		history_rows: [
			orm.Row{
				vals: ['1', migration.name, '2026-08-16T00:00:00Z']
			},
		]
	}
	mut down_runner := new(mut down_recorder, [migration], Config{
		dialect: .pg
		transaction_mode: .never
	})!
	error_message = ''
	down_runner.rollback(1) or { error_message = err.msg() }
	assert error_message.contains('current PostgreSQL transaction is aborted')
	assert error_message.contains('PostgreSQL migration callback left unsafe session state')
	assert !down_recorder.in_transaction
	assert !down_recorder.postgresql_aborted
	down_deletes := down_recorder.queries.filter(it.starts_with('DELETE FROM '))
	assert down_deletes.len == 0
	down_lock_check_index := down_recorder.queries.index(lock_check)
	assert down_lock_check_index > down_recorder.queries.index('force PostgreSQL statement error;')
	assert down_recorder.queries.index('ORM ROLLBACK') > down_lock_check_index
	assert down_recorder.queries.last() == 'SELECT pg_advisory_unlock(${key});'
}

fn test_postgresql_owned_transaction_is_verified_before_history_writes() {
	migration := Migration{
		version: 1
		name: 'rollback_transaction'
		up: rollback_callback_transaction
		down: rollback_callback_transaction
	}
	for mode in [TransactionMode.automatic, .always] {
		mut up_recorder := &RecordingConnection{}
		mut up_runner := new(mut up_recorder, [migration], Config{
			dialect: .pg
			transaction_mode: mode
		})!
		mut error_message := ''
		up_runner.migrate() or { error_message = err.msg() }
		assert error_message == 'PostgreSQL migration callback ended the migrator-owned transaction before history was recorded'
		assert !up_recorder.in_transaction
		assert up_recorder.queries.filter(it.starts_with('INSERT INTO ')).len == 0
		callback_index := up_recorder.queries.index('ROLLBACK;')
		assert callback_index > up_recorder.queries.index('ORM BEGIN')
		assert postgresql_transaction_probe_read_query() in up_recorder.queries[callback_index + 1..]

		mut down_recorder := &RecordingConnection{
			history_rows: [
				orm.Row{
					vals: ['1', migration.name, '2026-08-16T00:00:00Z']
				},
			]
		}
		mut down_runner := new(mut down_recorder, [migration], Config{
			dialect: .pg
			transaction_mode: mode
		})!
		error_message = ''
		down_runner.rollback(1) or { error_message = err.msg() }
		assert error_message == 'PostgreSQL migration callback ended the migrator-owned transaction before history was recorded'
		assert !down_recorder.in_transaction
		assert down_recorder.queries.filter(it.starts_with('DELETE FROM ')).len == 0
	}
}

fn test_postgresql_advisory_lock_is_verified_before_history_writes() {
	migrations := [
		Migration{
			version: 1
			name: 'unlock_all'
			up: unlock_all_postgresql_advisory_locks
			down: unlock_all_postgresql_advisory_locks
		},
		Migration{
			version: 1
			name: 'unlock_migration_lock'
			up: unlock_postgresql_migration_lock
			down: unlock_postgresql_migration_lock
		},
	]
	for migration in migrations {
		for mode in [TransactionMode.automatic, .always, .never] {
			mut up_recorder := &RecordingConnection{}
			mut up_runner := new(mut up_recorder, [migration], Config{
				dialect: .pg
				transaction_mode: mode
			})!
			mut error_message := ''
			up_runner.migrate() or { error_message = err.msg() }
			assert error_message.contains('PostgreSQL migration callback released the migration advisory lock before history was recorded')
			assert up_recorder.queries.filter(it.starts_with('INSERT INTO ')).len == 0
			key := postgresql_migration_lock_key('public', 'schema_migrations')
			lock_check := postgresql_migration_lock_owned_query(key)
			callback_index := up_recorder.queries.index(if migration.name == 'unlock_all' {
				'SELECT pg_advisory_unlock_all();'
			} else {
				'SELECT pg_advisory_unlock(${key});'
			})
			assert callback_index >= 0
			assert up_recorder.queries.index(lock_check) > callback_index

			mut down_recorder := &RecordingConnection{
				history_rows: [
					orm.Row{
						vals: ['1', migration.name, '2026-08-16T00:00:00Z']
					},
				]
			}
			mut down_runner := new(mut down_recorder, [migration], Config{
				dialect: .pg
				transaction_mode: mode
			})!
			error_message = ''
			down_runner.rollback(1) or { error_message = err.msg() }
			assert error_message.contains('PostgreSQL migration callback released the migration advisory lock before history was recorded')
			assert down_recorder.queries.filter(it.starts_with('DELETE FROM ')).len == 0
			down_callback_index := down_recorder.queries.index(if migration.name == 'unlock_all' {
				'SELECT pg_advisory_unlock_all();'
			} else {
				'SELECT pg_advisory_unlock(${key});'
			})
			assert down_callback_index >= 0
			assert down_recorder.queries.index(lock_check) > down_callback_index
		}
	}
}

fn test_postgresql_transaction_lock_cannot_replace_session_lock_before_history_writes() {
	migration := Migration{
		version: 1
		name: 'replace_session_lock'
		up: replace_postgresql_migration_lock_with_transaction_lock
		down: replace_postgresql_migration_lock_with_transaction_lock
	}
	key := postgresql_migration_lock_key('public', 'schema_migrations')
	lock_check := postgresql_migration_lock_owned_query(key)
	for mode in [TransactionMode.automatic, .always] {
		mut up_recorder := &RecordingConnection{}
		mut up_runner := new(mut up_recorder, [migration], Config{
			dialect: .pg
			transaction_mode: mode
		})!
		mut error_message := ''
		up_runner.migrate() or { error_message = err.msg() }
		assert error_message.contains('PostgreSQL migration callback released the migration advisory lock before history was recorded')
		assert up_recorder.queries.filter(it.starts_with('INSERT INTO ')).len == 0
		transaction_lock_index := up_recorder.queries.index('SELECT pg_advisory_xact_lock(${key});')
		assert transaction_lock_index >= 0
		assert up_recorder.queries.index(lock_check) > transaction_lock_index
		assert !up_recorder.postgresql_xact_lock_held

		mut down_recorder := &RecordingConnection{
			history_rows: [
				orm.Row{
					vals: ['1', migration.name, '2026-08-16T00:00:00Z']
				},
			]
		}
		mut down_runner := new(mut down_recorder, [migration], Config{
			dialect: .pg
			transaction_mode: mode
		})!
		error_message = ''
		down_runner.rollback(1) or { error_message = err.msg() }
		assert error_message.contains('PostgreSQL migration callback released the migration advisory lock before history was recorded')
		assert down_recorder.queries.filter(it.starts_with('DELETE FROM ')).len == 0
		down_transaction_lock_index :=
			down_recorder.queries.index('SELECT pg_advisory_xact_lock(${key});')
		assert down_transaction_lock_index >= 0
		assert down_recorder.queries.index(lock_check) > down_transaction_lock_index
		assert !down_recorder.postgresql_xact_lock_held
	}
}

fn test_mysql_owned_transaction_is_verified_before_history_writes() {
	migration := Migration{
		version: 1
		name: 'rollback_transaction'
		up: rollback_callback_transaction
		down: rollback_callback_transaction
	}
	mut up_recorder := &RecordingConnection{}
	mut up_runner := new(mut up_recorder, [migration], Config{
		dialect: .mysql
		transaction_mode: .always
	})!
	mut error_message := ''
	up_runner.migrate() or { error_message = err.msg() }
	assert error_message == 'MySQL migration callback ended the migrator-owned transaction before history was recorded'
	assert !up_recorder.in_transaction
	assert up_recorder.queries.filter(it.starts_with('INSERT INTO ')).len == 0
	assert up_recorder.queries.last().starts_with('SELECT RELEASE_LOCK(')

	mut down_recorder := &RecordingConnection{
		history_rows: [
			orm.Row{
				vals: ['1', migration.name, '2026-08-16T00:00:00Z']
			},
		]
	}
	mut down_runner := new(mut down_recorder, [migration], Config{
		dialect: .mysql
		transaction_mode: .always
	})!
	error_message = ''
	down_runner.rollback(1) or { error_message = err.msg() }
	assert error_message == 'MySQL migration callback ended the migrator-owned transaction before history was recorded'
	assert !down_recorder.in_transaction
	assert down_recorder.queries.filter(it.starts_with('DELETE FROM ')).len == 0
	assert down_recorder.queries.last().starts_with('SELECT RELEASE_LOCK(')
}

fn test_mysql_named_lock_is_verified_before_history_writes() {
	migrations := [
		Migration{
			version: 1
			name: 'unlock_all'
			up: unlock_all_mysql_named_locks
			down: unlock_all_mysql_named_locks
		},
		Migration{
			version: 1
			name: 'unlock_migration_lock'
			up: unlock_mysql_migration_lock
			down: unlock_mysql_migration_lock
		},
	]
	for migration in migrations {
		for mode in [TransactionMode.automatic, .always, .never] {
			mut up_recorder := &RecordingConnection{}
			mut up_runner := new(mut up_recorder, [migration], Config{
				dialect: .mysql
				transaction_mode: mode
			})!
			mut error_message := ''
			up_runner.migrate() or { error_message = err.msg() }
			assert error_message.contains('MySQL migration callback released the named migration lock before history was recorded')
			assert up_recorder.queries.filter(it.starts_with('INSERT INTO ')).len == 0
			name := mysql_migration_lock_name('test_database', 'schema_migrations', 0)
			lock_check := mysql_migration_lock_owned_query(name)
			callback_query := if migration.name == 'unlock_all' {
				'SELECT RELEASE_ALL_LOCKS();'
			} else {
				"SELECT RELEASE_LOCK('${name}');"
			}
			callback_index := up_recorder.queries.index(callback_query)
			assert callback_index >= 0
			assert up_recorder.queries.index(lock_check) > callback_index

			mut down_recorder := &RecordingConnection{
				history_rows: [
					orm.Row{
						vals: ['1', migration.name, '2026-08-16T00:00:00Z']
					},
				]
			}
			mut down_runner := new(mut down_recorder, [migration], Config{
				dialect: .mysql
				transaction_mode: mode
			})!
			error_message = ''
			down_runner.rollback(1) or { error_message = err.msg() }
			assert error_message.contains('MySQL migration callback released the named migration lock before history was recorded')
			assert down_recorder.queries.filter(it.starts_with('DELETE FROM ')).len == 0
			down_callback_index := down_recorder.queries.index(callback_query)
			assert down_callback_index >= 0
			assert down_recorder.queries.index(lock_check) > down_callback_index
		}
	}
}

fn test_sqlite_callback_cannot_end_lock_transaction_before_history_writes() {
	mut up_db := sqlite.connect(':memory:')!
	up_db.exec('CREATE TABLE schema_migrations (version BIGINT PRIMARY KEY, name VARCHAR(255) NOT NULL, applied_at VARCHAR(32) NOT NULL);')!
	mut up_runner := new(mut up_db, [
		Migration{
			version: 1
			name: 'rollback_transaction'
			up: create_sqlite_table_then_rollback
			down: drop_sqlite_table_then_rollback
		},
	], Config{
		dialect: .sqlite
	})!
	mut error_message := ''
	up_runner.migrate() or { error_message = err.msg() }
	assert error_message == 'SQLite migration callback ended the migration lock transaction before history was recorded'
	assert up_db.q_int("SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'rolled_back_callback_table';")! == 0
	assert up_db.q_int('SELECT count(*) FROM schema_migrations;')! == 0
	up_db.close()!

	mut down_db := sqlite.connect(':memory:')!
	down_db.exec('CREATE TABLE schema_migrations (version BIGINT PRIMARY KEY, name VARCHAR(255) NOT NULL, applied_at VARCHAR(32) NOT NULL);')!
	down_db.exec('CREATE TABLE rolled_back_callback_table (id INTEGER PRIMARY KEY AUTOINCREMENT);')!
	down_db.exec("INSERT INTO schema_migrations VALUES (1, 'rollback_transaction', '2026-08-16T00:00:00Z');")!
	mut down_runner := new(mut down_db, [
		Migration{
			version: 1
			name: 'rollback_transaction'
			up: create_sqlite_table_then_rollback
			down: drop_sqlite_table_then_rollback
		},
	], Config{
		dialect: .sqlite
	})!
	error_message = ''
	down_runner.rollback(1) or { error_message = err.msg() }
	assert error_message == 'SQLite migration callback ended the migration lock transaction before history was recorded'
	assert down_db.q_int("SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'rolled_back_callback_table';")! == 1
	assert down_db.q_int('SELECT count(*) FROM schema_migrations;')! == 1
	down_db.close()!
}

fn test_sqlite_callback_cannot_replace_lock_transaction_before_history_write() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	db.exec('CREATE TABLE schema_migrations (version BIGINT PRIMARY KEY, name VARCHAR(255) NOT NULL, applied_at VARCHAR(32) NOT NULL);')!
	mut runner := new(mut db, [
		Migration{
			version: 1
			name: 'replace_transaction'
			up: create_sqlite_table_then_replace_transaction
			down: drop_sqlite_table_then_rollback
		},
	], Config{
		dialect: .sqlite
	})!
	mut error_message := ''
	runner.migrate() or { error_message = err.msg() }
	assert error_message == 'SQLite migration callback ended the migration lock transaction before history was recorded'
	assert db.q_int("SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'replaced_transaction_table';")! == 0
	assert db.q_int('SELECT count(*) FROM schema_migrations;')! == 0
	assert runner.sqlite_transaction_probe == ''
}

fn test_sqlite_failed_lock_acquisition_cleans_up_transaction_probe() {
	mut recorder := &RecordingConnection{
		fail_sqlite_lock: true
	}
	mut runner := new(mut recorder, []Migration{}, Config{
		dialect: .sqlite
	})!
	mut error_message := ''
	runner.migrate() or { error_message = err.msg() }
	assert error_message.contains('database is locked'), error_message
	assert !runner.sqlite_lock_active
	assert runner.sqlite_transaction_probe == ''
	assert recorder.queries.len == 3
	assert recorder.queries[0].starts_with('CREATE TEMP TABLE temp."v3_migrations_transaction_')
	assert recorder.queries[1] == 'BEGIN IMMEDIATE;'
	assert recorder.queries[2].starts_with('DROP TABLE IF EXISTS temp."v3_migrations_transaction_')

	recorder.fail_sqlite_lock = false
	runner.migrate()!
	assert runner.sqlite_transaction_probe == ''
	assert recorder.queries.filter(it.starts_with('CREATE TEMP TABLE temp."v3_migrations_transaction_')).len == 2
	assert recorder.queries.filter(it.starts_with('DROP TABLE IF EXISTS temp."v3_migrations_transaction_')).len == 2
}

fn test_sqlite_failed_lock_commit_rolls_back_and_cleans_up_transaction_probe() {
	mut recorder := &RecordingConnection{
		fail_sqlite_commit: true
	}
	mut runner := new(mut recorder, [
		Migration{
			version: 1
			name: 'deferred_foreign_key_violation'
			up: record_locked_migration
			down: record_locked_migration
		},
	], Config{
		dialect: .sqlite
	})!
	mut error_message := ''
	runner.migrate() or { error_message = err.msg() }
	assert error_message.contains('FOREIGN KEY constraint failed'), error_message
	assert !runner.sqlite_lock_active
	assert runner.sqlite_transaction_probe == ''
	assert !recorder.in_transaction
	commit_index := recorder.queries.index('ORM COMMIT')
	rollback_index := recorder.queries.index('ORM ROLLBACK')
	probe_drops :=
		recorder.queries.filter(it.starts_with('DROP TABLE IF EXISTS temp."v3_migrations_transaction_'))
	assert probe_drops.len == 1
	probe_drop_index := recorder.queries.index(probe_drops[0])
	assert commit_index > recorder.queries.index('migration callback;')
	assert rollback_index > commit_index
	assert probe_drop_index > rollback_index

	recorder.fail_sqlite_commit = false
	runner.migrate()!
	assert !runner.sqlite_lock_active
	assert runner.sqlite_transaction_probe == ''
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
				name: 'persistent_history'
				up: create_sqlite_temp_history_shadow
				down: drop_sqlite_temp_history_shadow
			},
		], Config{
			dialect: .sqlite
		})!
		assert runner.migrate()!.map(it.version) == [i64(1)]
		assert runner.applied()!.map(it.name) == ['persistent_history']
		assert db.q_int("SELECT count(*) FROM main.sqlite_master WHERE type = 'table' AND name = 'persistent_from_migration';")! == 1
		assert db.q_int("SELECT count(*) FROM main.schema_migrations WHERE version = '1';")! == 1
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
			name: 'before\x00after'
			up: record_locked_migration
			down: record_locked_migration
		},
	], Config{ dialect: .sqlite }) or {
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
		'SELECT @@autocommit;',
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
	assert second_recorder.queries[3] == "SELECT GET_LOCK('${second_name}', ${migration_lock_timeout_seconds});"

	qualified_table := 'shared.schema_migrations'
	qualified_name := mysql_migration_lock_name('shared', qualified_table, 0)
	assert qualified_name == mysql_migration_lock_name('shared', 'schema_migrations', 0)
	mut qualified_first_recorder := &RecordingConnection{
		database: 'application_one'
	}
	mut qualified_first_runner := new(mut qualified_first_recorder, []Migration{}, Config{
		dialect: .mysql
		table: qualified_table
	})!
	qualified_first_runner.acquire_migration_lock()!
	qualified_first_runner.release_migration_lock(true)!
	assert qualified_first_recorder.queries == [
		'SELECT @@autocommit;',
		'SELECT @@lower_case_table_names;',
		"SELECT GET_LOCK('${qualified_name}', ${migration_lock_timeout_seconds});",
		"SELECT RELEASE_LOCK('${qualified_name}');",
	]

	mut qualified_second_recorder := &RecordingConnection{
		database: 'application_two'
	}
	mut qualified_second_runner := new(mut qualified_second_recorder, []Migration{}, Config{
		dialect: .mysql
		table: qualified_table
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
	assert mysql_migration_lock_name('app', 'app.schema_migrations', 0) != mysql_migration_lock_name('APP', 'APP.Schema_Migrations', 0)
	for mode in [1, 2] {
		lower_name := mysql_migration_lock_name('app', 'app.schema_migrations', mode)
		upper_name := mysql_migration_lock_name('APP', 'APP.Schema_Migrations', mode)
		assert lower_name == upper_name

		mut lower_recorder := &RecordingConnection{
			lower_case_table_names: mode
		}
		mut lower_runner := new(mut lower_recorder, []Migration{}, Config{
			dialect: .mysql
			table: 'app.schema_migrations'
		})!
		lower_runner.acquire_migration_lock()!
		lower_runner.release_migration_lock(true)!

		mut upper_recorder := &RecordingConnection{
			lower_case_table_names: mode
		}
		mut upper_runner := new(mut upper_recorder, []Migration{}, Config{
			dialect: .mysql
			table: 'APP.Schema_Migrations'
		})!
		upper_runner.acquire_migration_lock()!
		upper_runner.release_migration_lock(true)!
		assert upper_recorder.queries == lower_recorder.queries
		assert lower_recorder.queries == [
			'SELECT @@autocommit;',
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
		table: first_table
	})!
	runner.acquire_migration_lock()!
	runner.release_migration_lock(true)!
	assert_postgresql_transaction_probe(recorder.queries)
	assert recorder.queries[2..] == [
		postgresql_history_schema_query(first_table),
		'SELECT pg_advisory_lock(${first_key});',
		'SELECT pg_advisory_unlock(${first_key});',
	]
}

fn test_postgresql_migration_locks_canonicalize_history_table() {
	key := postgresql_migration_lock_key('public', 'schema_migrations')
	assert key == postgresql_migration_lock_key('public', 'public.schema_migrations')

	mut unqualified_recorder := &RecordingConnection{
		schema: 'tenant'
		postgresql_history_schema: 'public'
	}
	mut unqualified_runner := new(mut unqualified_recorder, []Migration{}, Config{
		dialect: .pg
	})!
	unqualified_runner.acquire_migration_lock()!
	unqualified_runner.release_migration_lock(true)!
	assert_postgresql_transaction_probe(unqualified_recorder.queries)
	assert unqualified_recorder.queries[2..] == [
		postgresql_history_schema_query('schema_migrations'),
		'SELECT pg_advisory_lock(${key});',
		'SELECT pg_advisory_unlock(${key});',
	]
	assert unqualified_runner.resolved_history_namespace == 'public'

	mut qualified_recorder := &RecordingConnection{
		schema: 'unrelated'
	}
	mut qualified_runner := new(mut qualified_recorder, []Migration{}, Config{
		dialect: .pg
		table: 'public.schema_migrations'
	})!
	qualified_runner.acquire_migration_lock()!
	qualified_runner.release_migration_lock(true)!
	assert_postgresql_transaction_probe(qualified_recorder.queries)
	assert qualified_recorder.queries[2..] == [
		'SELECT pg_advisory_lock(${key});',
		'SELECT pg_advisory_unlock(${key});',
	]

	temp_key := postgresql_migration_lock_key('pg_temp', 'schema_migrations')
	mut temp_recorder := &RecordingConnection{}
	mut temp_runner := new(mut temp_recorder, []Migration{}, Config{
		dialect: .pg
		table: 'pg_temp.schema_migrations'
	})!
	temp_runner.acquire_migration_lock()!
	temp_runner.release_migration_lock(true)!
	assert_postgresql_transaction_probe(temp_recorder.queries)
	assert temp_recorder.queries[2..] == [
		'SELECT pg_advisory_lock(${temp_key});',
		'SELECT pg_advisory_unlock(${temp_key});',
	]
}

fn test_postgresql_inspection_resolves_and_retains_existing_history_schema() {
	mut recorder := &RecordingConnection{
		schema: 'tenant'
		postgresql_history_schema: 'public'
		history_rows: [
			orm.Row{
				vals: ['7', 'already_applied', '2026-08-16T00:00:00Z']
			},
		]
	}
	mut runner := new(mut recorder, []Migration{}, Config{
		dialect: .pg
	})!
	assert runner.applied()!.map(it.version) == [i64(7)]
	assert runner.pending()!.len == 0
	assert runner.current_version()! == 7
	status := runner.status()!
	assert status.len == 1
	assert status[0].state == .missing
	assert runner.resolved_history_namespace == 'public'
	assert recorder.queries.filter(it == postgresql_history_schema_query('schema_migrations')).len == 1
	for query in recorder.queries {
		if query.starts_with('CREATE TABLE IF NOT EXISTS ')
			|| query.starts_with('SELECT version, name, applied_at FROM ') {
			assert query.contains('"public"."schema_migrations"')
		}
	}
	runner.acquire_migration_lock()!
	runner.release_migration_lock(true)!
	key := postgresql_migration_lock_key('public', 'schema_migrations')
	assert recorder.queries#[-2..] == [
		'SELECT pg_advisory_lock(${key});',
		'SELECT pg_advisory_unlock(${key});',
	]
	query := postgresql_history_schema_query('schema_migrations')
	assert query.contains('current_schemas(false)')
	assert query.contains('n.oid <> pg_catalog.pg_my_temp_schema()')
}

fn test_postgresql_inspection_rejects_active_transaction_without_retaining_schema() {
	mut recorder := &RecordingConnection{
		schema: 'transaction_schema'
		in_transaction: true
	}
	mut runner := new(mut recorder, []Migration{}, Config{
		dialect: .pg
	})!
	expected_error := 'PostgreSQL migrations require a connection without an already-open transaction; pg.Tx and transactional pg.Conn values are not supported'

	mut error_message := ''
	runner.applied() or { error_message = err.msg() }
	assert error_message == expected_error
	error_message = ''
	runner.pending() or { error_message = err.msg() }
	assert error_message == expected_error
	error_message = ''
	runner.current_version() or { error_message = err.msg() }
	assert error_message == expected_error
	error_message = ''
	runner.status() or { error_message = err.msg() }
	assert error_message == expected_error
	assert runner.resolved_history_namespace == ''
	assert recorder.queries.filter(it == postgresql_history_schema_query('schema_migrations')).len == 0
	assert recorder.queries.filter(it.starts_with('CREATE TABLE IF NOT EXISTS ')).len == 0
	assert recorder.queries.filter(it == postgresql_transaction_probe_read_query()).len == 4

	recorder.execute('ROLLBACK;')!
	recorder.schema = 'public'
	assert runner.migrate()!.len == 0
	assert runner.resolved_history_namespace == 'public'
	key := postgresql_migration_lock_key('public', 'schema_migrations')
	assert 'SELECT pg_advisory_lock(${key});' in recorder.queries
}

fn test_mysql_inspection_resolves_and_retains_history_database() {
	mut recorder := &RecordingConnection{
		database: 'app'
		history_rows: [
			orm.Row{
				vals: ['7', 'already_applied', '2026-08-16T00:00:00Z']
			},
		]
	}
	mut runner := new(mut recorder, []Migration{}, Config{
		dialect: .mysql
	})!
	assert runner.applied()!.map(it.version) == [i64(7)]
	assert runner.pending()!.len == 0
	assert runner.current_version()! == 7
	status := runner.status()!
	assert status.len == 1
	assert status[0].state == .missing
	assert runner.resolved_history_namespace == 'app'
	assert recorder.queries.filter(it == 'SELECT DATABASE();').len == 1
	for query in recorder.queries {
		if query.starts_with('CREATE TABLE IF NOT EXISTS ')
			|| query.starts_with('SELECT version, name, applied_at FROM ') {
			assert query.contains('`app`.`schema_migrations`')
		}
	}
	recorder.execute('USE other_database;')!
	assert runner.migrate()!.len == 0
	assert recorder.queries.filter(it == 'SELECT DATABASE();').len == 1
	name := mysql_migration_lock_name('app', 'schema_migrations', 0)
	assert "SELECT GET_LOCK('${name}', ${migration_lock_timeout_seconds});" in recorder.queries
	assert recorder.queries.filter(it.starts_with('CREATE TABLE IF NOT EXISTS ')
		|| it.starts_with('SELECT version, name, applied_at FROM ')).all(it.contains('`app`.`schema_migrations`'))
}

fn test_mysql_history_literals_are_backslash_safe() {
	mut recorder := &RecordingConnection{}
	name := "quote\\'and_trailing\\"
	migration := Migration{
		version: 7
		name: name
		up: record_locked_migration
		down: record_locked_migration
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
		name: name
		up: record_locked_migration
		down: record_locked_migration
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
		name: 'score'
		kind: .bigint
		limit: 64
	})!
	assert recorder.queries == [
		'ALTER TABLE "accounts" ALTER COLUMN "score" TYPE BIGINT;',
	]

	ctx.change_column('accounts', Column{
		name: 'score'
		kind: .bigint
		nullable: false
		default_sql: '0'
		unique: true
		primary_key: true
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
		name: 'score'
		kind: .bigint
		nullable: true
		default_sql: ''
		unique: false
		primary_key: false
		auto_increment: false
	}) or {
		assert err.msg() == 'PostgreSQL change_column only supports type, limit, precision, and scale; unsupported options: nullable, default_sql, unique, primary_key, auto_increment; use ctx.execute() for constraint changes'
		assert recorder.queries.len == 0
		return
	}
	assert false
}

fn test_mysql_change_column_rejects_lossy_redefinitions() {
	mut recorder := &RecordingConnection{}
	mut ctx := new_context(recorder, .mysql)
	mut error_message := ''
	ctx.change_column('accounts', Column{
		name: 'score'
		kind: .bigint
		nullable: false
		default_sql: '0'
		auto_increment: false
	}) or { error_message = err.msg() }
	assert error_message == 'MySQL change_column cannot safely preserve attributes outside Column; use ctx.execute() with a complete MODIFY COLUMN definition'
	assert recorder.queries.len == 0
}

fn test_mysql_change_column_rejects_auto_increment_redefinitions() {
	mut recorder := &RecordingConnection{}
	mut ctx := new_context(recorder, .mysql)
	mut error_message := ''
	ctx.change_column('accounts', Column{
		name: 'id'
		kind: .bigint
		nullable: false
		default_sql: ''
		auto_increment: true
	}) or { error_message = err.msg() }
	assert error_message == 'MySQL change_column cannot safely preserve attributes outside Column; use ctx.execute() with a complete MODIFY COLUMN definition'
	assert recorder.queries.len == 0
}

fn test_mysql_auto_increment_requires_a_key() {
	mut recorder := &RecordingConnection{}
	mut ctx := new_context(recorder, .mysql)
	mut add_error := ''
	ctx.add_column('accounts', Column{
		name: 'sequence'
		kind: .bigint
		auto_increment: true
	}) or { add_error = err.msg() }
	assert add_error == 'MySQL auto-increment column `sequence` must be a primary key or unique'

	mut create_error := ''
	ctx.create_table(Table{
		name: 'events'
		id: false
		columns: [
			Column{
				name: 'sequence'
				kind: .bigint
				auto_increment: true
			},
		]
	}) or { create_error = err.msg() }
	assert create_error == 'MySQL auto-increment column `sequence` must be a primary key or unique'
	assert recorder.queries.len == 0

	ctx.add_column('accounts', Column{
		name: 'sequence'
		kind: .bigint
		auto_increment: true
		unique: true
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
		name: 'events'
		columns: [
			Column{
				name: 'sequence'
				kind: .bigint
				auto_increment: true
				unique: true
			},
		]
	}) or { error_message = err.msg() }
	assert error_message == 'MySQL table `events` cannot have more than one auto-increment column'
	assert recorder.queries.len == 0

	ctx.create_table(Table{
		name: 'single_sequence'
		id: false
		columns: [
			Column{
				name: 'sequence'
				kind: .bigint
				auto_increment: true
				unique: true
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
			name: 'records'
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
			name: 'contacts'
			id: false
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
		name: 'contacts'
		id: false
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
		table: 'reporting.users'
		columns: ['email']
		name: 'reporting.users_email_idx'
	}) or { error_message = err.msg() }
	assert error_message == 'PostgreSQL add_index name `reporting.users_email_idx` must be unqualified'
	assert recorder.queries.len == 0

	ctx.add_index(Index{
		table: 'reporting.users'
		columns: ['email']
		name: 'users_email_idx'
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
		table: 'app.users'
		columns: ['email']
		name: 'app.users_email_idx'
	}) or { error_message = err.msg() }
	assert error_message == 'MySQL add_index name `app.users_email_idx` must be unqualified'
	assert recorder.queries.len == 0

	ctx.add_index(Index{
		table: 'app.users'
		columns: ['email']
		name: 'users_email_idx'
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
		column: 'organization_id'
		to_table: 'organizations'
		on_delete: 'set_default'
	}) or { error_message = err.msg() }
	assert error_message == 'MySQL does not support SET DEFAULT for foreign-key actions'

	error_message = ''
	ctx.create_table(Table{
		name: 'accounts'
		id: false
		columns: [
			Column{
				name: 'organization_id'
				kind: .bigint
			},
		]
		foreign_keys: [
			ForeignKey{
				from_table: 'accounts'
				column: 'organization_id'
				to_table: 'organizations'
				on_update: 'SET DEFAULT'
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
		name: 'sequence'
		kind: .bigint
		auto_increment: true
		default_sql: '5'
	}) or { error_message = err.msg() }
	assert error_message == 'PostgreSQL auto-increment column `sequence` cannot specify default_sql'

	error_message = ''
	ctx.add_column('accounts', Column{
		name: 'amount'
		kind: .decimal
		precision: 0
		scale: 2
	}) or { error_message = err.msg() }
	assert error_message == 'decimal scale requires a positive precision'
	assert recorder.queries.len == 0

	assert column_sql(.pg, Column{
		name: 'sequence'
		kind: .bigint
		primary_key: true
		auto_increment: true
		default_sql: ''
	})! == '"sequence" BIGSERIAL PRIMARY KEY'
	assert column_type_sql(.mysql, Column{
		name: 'amount'
		kind: .decimal
		precision: 8
		scale: 2
	})! == 'DECIMAL(8, 2)'
}

fn test_sqlite_add_column_rejects_unsupported_constraints() {
	mut recorder := &RecordingConnection{}
	mut ctx := new_context(recorder, .sqlite)
	mut error_message := ''
	ctx.add_column('accounts', Column{
		name: 'owner_id'
		kind: .bigint
		primary_key: true
	}) or { error_message = err.msg() }
	assert error_message == 'SQLite add_column does not support primary-key, unique, or auto-increment columns; rebuild the table in the migration'

	error_message = ''
	ctx.add_column('accounts', Column{
		name: 'email'
		kind: .text
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
	invalid_defaults << 'NULL COLLATE binary'
	invalid_defaults << '+NULL /* absent */ COLLATE binary'
	invalid_defaults << '(CAST(NULL AS INTEGER))'
	invalid_defaults << '(CAST((+NULL) AS TEXT))'
	invalid_defaults << '(CAST(CAST(NULL AS TEXT) AS INTEGER))'
	invalid_defaults << '(CAST(NULL /* absent */ AS INTEGER)) COLLATE binary'
	for default_sql in invalid_defaults {
		error_message = ''
		ctx.add_column('accounts', Column{
			name: 'label'
			kind: .text
			nullable: false
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
			name: 'created_at'
			kind: .timestamp
			default_sql: default_sql
		}) or { error_message = err.msg() }
		assert error_message == 'SQLite add_column does not support nonconstant default `${default_sql}`; rebuild the table in the migration'
	}
	assert recorder.queries.len == 0

	ctx.add_column('accounts', Column{
		name: 'label'
		kind: .text
		nullable: false
		default_sql: "''"
	})!
	assert recorder.queries == [
		'ALTER TABLE "accounts" ADD COLUMN "label" TEXT NOT NULL DEFAULT \'\';',
	]
	ctx.add_column('accounts', Column{
		name: 'literal_comment'
		kind: .text
		nullable: false
		default_sql: "'CURRENT_TIMESTAMP /* literal */'"
	})!
	assert recorder.queries[1] == 'ALTER TABLE "accounts" ADD COLUMN "literal_comment" TEXT NOT NULL DEFAULT \'CURRENT_TIMESTAMP /* literal */\';'
	for i, default_sql in ['(0)', "('x')", '(NULL)'] {
		ctx.add_column('accounts', Column{
			name: 'parenthesized_${i}'
			kind: .text
			default_sql: default_sql
		})!
	}
	assert recorder.queries[2..] == [
		'ALTER TABLE "accounts" ADD COLUMN "parenthesized_0" TEXT DEFAULT (0);',
		'ALTER TABLE "accounts" ADD COLUMN "parenthesized_1" TEXT DEFAULT (\'x\');',
		'ALTER TABLE "accounts" ADD COLUMN "parenthesized_2" TEXT DEFAULT (NULL);',
	]
	ctx.add_column('accounts', Column{
		name: 'cast_null_text'
		kind: .text
		nullable: false
		default_sql: "(CAST('NULL' AS TEXT))"
	})!
	assert recorder.queries.last() == 'ALTER TABLE "accounts" ADD COLUMN "cast_null_text" TEXT NOT NULL DEFAULT (CAST(\'NULL\' AS TEXT));'
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
		name: 'count'
		kind: .integer
		default_sql: '(0)'
	})!
	ctx.add_column('accounts', Column{
		name: 'label'
		kind: .text
		default_sql: "('x')"
	})!
	ctx.add_column('accounts', Column{
		name: 'optional'
		kind: .text
		default_sql: '(NULL)'
	})!
	assert db.q_int('SELECT count FROM accounts WHERE id = 1;')! == 0
	assert db.q_string('SELECT label FROM accounts WHERE id = 1;')! == 'x'
	assert db.q_int('SELECT count(*) FROM accounts WHERE optional IS NULL;')! == 1
	ctx.add_column('accounts', Column{
		name: 'signed_count'
		kind: .integer
		default_sql: '(-1)'
	})!
	ctx.add_column('accounts', Column{
		name: 'signed_label'
		kind: .text
		default_sql: "(+'signed')"
	})!
	assert db.q_int('SELECT signed_count FROM accounts WHERE id = 1;')! == -1
	assert db.q_string('SELECT signed_label FROM accounts WHERE id = 1;')! == 'signed'
	ctx.add_column('accounts', Column{
		name: 'collated_count'
		kind: .integer
		default_sql: '(0) COLLATE binary'
	})!
	ctx.add_column('accounts', Column{
		name: 'collated_label'
		kind: .text
		default_sql: "('collated') COLLATE binary"
	})!
	ctx.add_column('accounts', Column{
		name: 'cast_count'
		kind: .integer
		default_sql: '(CAST(42 AS INTEGER))'
	})!
	ctx.add_column('accounts', Column{
		name: 'signed_cast_count'
		kind: .integer
		default_sql: '(+CAST(42 AS INTEGER))'
	})!
	ctx.add_column('accounts', Column{
		name: 'nested_signed_cast_count'
		kind: .integer
		default_sql: '(CAST(-CAST(42 AS INTEGER) AS INTEGER))'
	})!
	assert db.q_int('SELECT collated_count FROM accounts WHERE id = 1;')! == 0
	assert db.q_string('SELECT collated_label FROM accounts WHERE id = 1;')! == 'collated'
	assert db.q_int('SELECT cast_count FROM accounts WHERE id = 1;')! == 42
	assert db.q_int('SELECT signed_cast_count FROM accounts WHERE id = 1;')! == 42
	assert db.q_int('SELECT nested_signed_cast_count FROM accounts WHERE id = 1;')! == -42
}

fn test_sqlite_constant_cast_default_classification() {
	for default_sql in ['(CAST(42 AS INTEGER))', "(CAST('x AS y' AS TEXT))",
		'(CAST(CAST(42 AS TEXT) AS INTEGER))', '(CAST((+42) AS DECIMAL(10, 2)))',
		'(+CAST(42 AS INTEGER))', '(CAST(-CAST(42 AS INTEGER) AS INTEGER))'] {
		assert !sqlite_add_column_default_is_nonconstant(default_sql)
	}
	for default_sql in ['(CAST(CURRENT_TIMESTAMP AS TEXT))', "(CAST(datetime('now') AS TEXT))",
		'(CAST(account_id AS INTEGER))', '(CAST( AS TEXT))'] {
		assert sqlite_add_column_default_is_nonconstant(default_sql)
	}
}

fn test_sqlite_accepts_numeric_literal_digit_separators() {
	for literal in ['1_000', '(1_000)', '1_2.3_4e5_6', '1e+1_0', '0xCA_FE', '(0xA_B_C)', '.5', '1.',
		'1e-2'] {
		assert sqlite_is_literal_default(literal)
	}
	for literal in ['_1000', '1000_', '1__000', '1_.0', '0x_FF', '0xFF_', '0xA__B', 'e1', 'E+1',
		'.e1', '1e', '1e+', '1.2.3'] {
		assert !sqlite_is_literal_default(literal)
	}

	mut recorder := &RecordingConnection{}
	mut ctx := new_context(recorder, .sqlite)
	ctx.add_column('accounts', Column{
		name: 'population'
		kind: .integer
		default_sql: '(1_000)'
	})!
	ctx.add_column('accounts', Column{
		name: 'mask'
		kind: .integer
		default_sql: '(0xCA_FE)'
	})!
	ctx.add_column('accounts', Column{
		name: 'cast_population'
		kind: .integer
		default_sql: '(CAST(1_000 AS INTEGER))'
	})!
	assert recorder.queries == [
		'SELECT sqlite_version();',
		'ALTER TABLE "accounts" ADD COLUMN "population" INTEGER DEFAULT (1_000);',
		'ALTER TABLE "accounts" ADD COLUMN "mask" INTEGER DEFAULT (0xCA_FE);',
		'ALTER TABLE "accounts" ADD COLUMN "cast_population" INTEGER DEFAULT (CAST(1_000 AS INTEGER));',
	]
	mut error_message := ''
	ctx.add_column('accounts', Column{
		name: 'invalid_exponent'
		kind: .real
		default_sql: '(e1)'
	}) or { error_message = err.msg() }
	assert error_message == 'SQLite add_column does not support nonconstant default `(e1)`; rebuild the table in the migration'
	assert recorder.queries.len == 4

	mut old_recorder := &RecordingConnection{
		sqlite_version: '3.45.1'
	}
	mut old_ctx := new_context(old_recorder, .sqlite)
	error_message = ''
	old_ctx.add_column('accounts', Column{
		name: 'population'
		kind: .integer
		default_sql: '(1_000)'
	}) or { error_message = err.msg() }
	assert error_message == 'SQLite add_column default `(1_000)` uses numeric digit separators, which require SQLite 3.46.0 or newer'
	assert old_recorder.queries == ['SELECT sqlite_version();']
	error_message = ''
	old_ctx.add_column('accounts', Column{
		name: 'cast_population'
		kind: .integer
		default_sql: '(CAST(1_000 AS INTEGER))'
	}) or { error_message = err.msg() }
	assert error_message == 'SQLite add_column default `(CAST(1_000 AS INTEGER))` uses numeric digit separators, which require SQLite 3.46.0 or newer'
	assert old_recorder.queries == ['SELECT sqlite_version();']
}

fn test_sqlite_requires_unqualified_index_and_foreign_key_tables() {
	mut recorder := &RecordingConnection{}
	mut ctx := new_context(recorder, .sqlite)
	mut error_message := ''
	ctx.add_index(Index{
		table: 'main.users'
		columns: ['email']
	}) or { error_message = err.msg() }
	assert error_message == 'SQLite add_index table `main.users` must be unqualified'
	assert recorder.queries.len == 0

	error_message = ''
	generated_name := index_name(.sqlite, Index{
		table: 'users'
		columns: ['email']
	})!
	ctx.add_index(Index{
		table: 'users'
		columns: ['email']
		name: 'main.aux.users_email_idx'
	}) or { error_message = err.msg() }
	assert error_message == 'SQLite index name `main.aux.users_email_idx` must not exceed 2 components'
	assert recorder.queries.len == 0

	error_message = ''
	ctx.create_table(Table{
		name: 'main.children'
		id: false
		columns: [
			Column{
				name: 'parent_id'
				kind: .bigint
			},
		]
		foreign_keys: [
			ForeignKey{
				from_table: 'main.children'
				column: 'parent_id'
				to_table: 'main.parents'
			},
		]
	}) or { error_message = err.msg() }
	assert error_message == 'SQLite foreign-key target table `main.parents` must be unqualified'
	assert recorder.queries.len == 0

	ctx.add_index(Index{
		table: 'users'
		columns: ['email']
	})!
	ctx.create_table(Table{
		name: 'main.children'
		id: false
		columns: [
			Column{
				name: 'parent_id'
				kind: .bigint
			},
		]
		foreign_keys: [
			ForeignKey{
				from_table: 'main.children'
				column: 'parent_id'
				to_table: 'parents'
			},
		]
	})!
	assert recorder.queries == [
		'PRAGMA database_list;',
		'SELECT 1 FROM "temp".sqlite_schema WHERE type = \'table\' AND name = \'users\' COLLATE NOCASE LIMIT 1;',
		'SELECT 1 FROM "main".sqlite_schema WHERE type = \'table\' AND name = \'users\' COLLATE NOCASE LIMIT 1;',
		'CREATE INDEX "main"."${generated_name}" ON "users" ("email");',
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
		table: 'users'
		columns: ['email']
	})!
	ctx.add_index(Index{
		table: 'users'
		columns: ['email']
		name: 'aux.custom_users_email_idx'
	})!
	assert db.q_int("SELECT count(*) FROM main.sqlite_master WHERE type = 'index';")! == 0
	assert db.q_int("SELECT count(*) FROM aux.sqlite_master WHERE type = 'index';")! == 2
}

fn test_sqlite_autoincrement_precedes_other_constraints() {
	definition := column_sql(.sqlite, Column{
		name: 'id'
		kind: .integer
		primary_key: true
		auto_increment: true
		unique: true
		default_sql: '5'
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
		name: 'code'
		kind: .text
		primary_key: true
	})!
	assert text_definition == '"code" TEXT PRIMARY KEY NOT NULL'
	integer_definition := column_sql(.sqlite, Column{
		name: 'id'
		kind: .integer
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
		table: 'public.users'
		columns: ['users.email']
	}) or { error_message = err.msg() }
	assert error_message == 'column name `users.email` must be unqualified'

	error_message = ''
	ctx.add_foreign_key(ForeignKey{
		from_table: 'public.posts'
		column: 'posts.author_id'
		to_table: 'public.users'
		primary_key: 'id'
		name: 'fk_posts_author'
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
			name: 'one'
			up: create_accounts
			down: drop_accounts
		},
		Migration{
			version: 7
			name: 'two'
			up: create_accounts
			down: drop_accounts
		},
	], Config{ dialect: .sqlite }) or {
		assert err.msg() == 'duplicate migration version 7'
		assert column_type_sql(.pg, Column{ name: 'payload', kind: .jsonb })! == 'JSONB'
		assert column_type_sql(.mysql, Column{ name: 'amount', kind: .double_precision })! == 'DOUBLE'
		generated_name := index_name(.sqlite, Index{
			table: 'accounts'
			columns: ['email', 'name']
		})!
		assert generated_name.starts_with('index_accounts_on_email_and_name_')
		return
	}
	assert false
}

fn test_generated_index_names_respect_dialect_limits() {
	index := Index{
		table: 'customer_account_records_archive'
		columns: ['external_customer_reference', 'external_organization_reference']
	}
	raw_base := 'index_customer_account_records_archive_on_external_customer_reference_and_external_organization_reference'
	raw_name := generated_index_name(index)
	assert raw_name.starts_with('${raw_base}_')
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
		table: index.table
		columns: ['external_customer_reference', 'external_organization_identifier']
	})!
	assert other_mysql_name != mysql_name

	long_explicit_name := 'x'.repeat(65)
	mut error_message := ''
	index_name(.mysql, Index{
		table: 'accounts'
		columns: ['email']
		name: long_explicit_name
	}) or { error_message = err.msg() }
	assert error_message == 'MySQL index name component `${long_explicit_name}` must not exceed 64 bytes'
}

fn test_generated_index_names_distinguish_column_boundaries() {
	single_column := Index{
		table: 'users'
		columns: ['a_and_b']
	}
	composite := Index{
		table: 'users'
		columns: ['a', 'b']
	}
	single_name := index_name(.sqlite, single_column)!
	composite_name := index_name(.sqlite, composite)!
	assert single_name != composite_name
	assert single_name.starts_with('index_users_on_a_and_b_')
	assert composite_name.starts_with('index_users_on_a_and_b_')
	case_variant_name := index_name(.mysql, Index{
		table: 'users'
		columns: ['a_AnD_b']
	})!
	assert case_variant_name != index_name(.mysql, composite)!
	table_boundary := Index{
		table: 'a_on_b'
		columns: ['c']
	}
	column_boundary := Index{
		table: 'a'
		columns: ['b_on_c']
	}
	for dialect in [Dialect.sqlite, .pg] {
		assert index_name(dialect, table_boundary)! != index_name(dialect, column_boundary)!
	}
	fragment_table_boundary := Index{
		table: 'a_on'
		columns: ['b']
	}
	fragment_column_boundary := Index{
		table: 'a'
		columns: ['on_b']
	}
	for dialect in [Dialect.sqlite, .pg] {
		assert index_name(dialect, fragment_table_boundary)! != index_name(dialect, fragment_column_boundary)!
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
	db.exec('CREATE TABLE a_on (b TEXT);')!
	db.exec('ALTER TABLE a ADD COLUMN on_b TEXT;')!
	ctx.add_index(fragment_table_boundary)!
	ctx.add_index(fragment_column_boundary)!
	assert db.q_int("SELECT count(*) FROM sqlite_master WHERE type = 'index';")! == 6
}

fn test_generated_foreign_key_names_respect_dialect_limits() {
	key := ForeignKey{
		from_table: 'customer_account_records_archive'
		column: 'external_organization_reference'
		to_table: 'organizations'
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
		column: 'external_organization_identifier'
		to_table: key.to_table
	})!
	assert other_mysql_name != mysql_name

	mut recorder := &RecordingConnection{}
	mut ctx := new_context(recorder, .mysql)
	ctx.add_foreign_key(key)!
	ctx.create_table(Table{
		name: key.from_table
		id: false
		columns: [
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
		column: 'organization_id'
		to_table: 'organizations'
		name: long_explicit_name
	}) or { error_message = err.msg() }
	assert error_message == 'MySQL foreign key name `${long_explicit_name}` must not exceed 64 bytes'
	assert recorder.queries.len == 2
}

fn test_generated_mysql_foreign_key_names_distinguish_component_boundaries() {
	first := ForeignKey{
		from_table: 'a_b'
		column: 'c'
		to_table: 'parents'
	}
	second := ForeignKey{
		from_table: 'a'
		column: 'b_c'
		to_table: 'parents'
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
		column: 'owner_id'
		to_table: 'parents'
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
			name: 'accounts'
			id: false
			columns: [
				Column{
					name: 'owner_id'
					kind: .bigint
				},
			]
			foreign_keys: [parent_key, other_table, other_primary_key]
		})!
		assert recorder.queries[0].contains('CONSTRAINT ${quote_identifier(dialect, parent_name)} FOREIGN KEY')
		assert recorder.queries[0].contains('CONSTRAINT ${quote_identifier(dialect, other_table_name)} FOREIGN KEY')
		assert recorder.queries[0].contains('CONSTRAINT ${quote_identifier(dialect, other_primary_key_name)} FOREIGN KEY')
	}
}

fn test_caller_supplied_identifiers_respect_dialect_limits() {
	mysql_limit_name := 'm'.repeat(64)
	mysql_long_name := 'm'.repeat(65)
	mut mysql_recorder := &RecordingConnection{}
	mut mysql_ctx := new_context(mysql_recorder, .mysql)
	mysql_ctx.create_table(Table{
		name: mysql_limit_name
		id: false
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
		table: '${mysql_limit_name}.${mysql_limit_name}'
	})!
	assert runner.config.table == '${mysql_limit_name}.${mysql_limit_name}'
	error_message = ''
	new(mut history_recorder, []Migration{}, Config{
		dialect: .mysql
		table: 'app.${mysql_long_name}'
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
		table: 'application.archive.schema_migrations'
	}) or { error_message = err.msg() }
	assert error_message == 'MySQL migration history table name `application.archive.schema_migrations` must not exceed 2 components'
	assert history_recorder.queries.len == 0
}

fn test_postgresql_inspection_rejects_an_active_transaction() {
	mut recorder := &RecordingConnection{
		in_transaction: true
	}
	mut runner := new(mut recorder, []Migration{}, Config{ dialect: .pg })!
	mut error_message := ''
	runner.applied() or { error_message = err.msg() }
	assert error_message == 'PostgreSQL migrations require a connection without an already-open transaction; pg.Tx and transactional pg.Conn values are not supported'
	assert runner.resolved_history_namespace == ''
	assert recorder.queries.len == 2
}

fn test_rails_and_v_history_tables_are_interoperable() {
	mut rails_db := sqlite.connect(':memory:')!
	defer {
		rails_db.close() or {}
	}
	rails_db.exec('CREATE TABLE schema_migrations (version VARCHAR(255) PRIMARY KEY);')!
	rails_db.exec("INSERT INTO schema_migrations (version) VALUES ('1');")!
	mut rails_runner := new(mut rails_db, [
		Migration{
			version: 1
			name: 'already_applied'
			up: no_op_migration
			down: no_op_migration
		},
		Migration{
			version: 2
			name: 'pending'
			up: no_op_migration
			down: no_op_migration
		},
	], Config{ dialect: .sqlite })!
	assert rails_runner.applied()!.map(it.name) == ['already_applied']
	assert rails_runner.migrate()!.map(it.version) == [i64(2)]
	assert rails_db.q_int('SELECT count(*) FROM schema_migrations;')! == 2
	rails_runner.rollback_last()!
	assert rails_db.q_int('SELECT count(*) FROM schema_migrations;')! == 1

	mut v_db := sqlite.connect(':memory:')!
	defer {
		v_db.close() or {}
	}
	mut v_runner := new(mut v_db, []Migration{}, Config{ dialect: .sqlite })!
	assert v_runner.applied()!.len == 0
	v_db.exec('INSERT INTO schema_migrations (version) VALUES (99);')!
	assert v_runner.applied()!.map(it.version) == [i64(99)]
}

fn test_rails_history_rejects_aliased_and_nonpositive_versions() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	db.exec('CREATE TABLE schema_migrations (version VARCHAR(255) PRIMARY KEY);')!
	db.exec("INSERT INTO schema_migrations (version) VALUES ('1'), ('01');")!
	mut runner := new(mut db, [
		Migration{
			version: 1
			name: 'one'
			up: no_op_migration
			down: no_op_migration
		},
	], Config{ dialect: .sqlite })!
	mut error_message := ''
	runner.rollback(2) or { error_message = err.msg() }
	assert error_message == 'migration history contains noncanonical version `01`'
	assert db.q_int('SELECT count(*) FROM schema_migrations;')! == 2

	db.exec('DELETE FROM schema_migrations;')!
	db.exec("INSERT INTO schema_migrations (version) VALUES ('0');")!
	error_message = ''
	runner.applied() or { error_message = err.msg() }
	assert error_message == 'migration history version `0` must be positive'
}

fn test_history_shape_fallback_does_not_cache_unrelated_errors() {
	mut recorder := &RecordingConnection{
		history_rows: [orm.Row{
			vals: ['1', 'one', '2026-08-16T00:00:00Z']
		}]
		history_metadata_error: 'database connection timed out'
	}
	mut runner := new(mut recorder, []Migration{}, Config{ dialect: .sqlite })!
	mut error_message := ''
	runner.applied() or { error_message = err.msg() }
	assert error_message == 'database connection timed out'
	assert !runner.history_shape_resolved

	recorder.history_metadata_error = ''
	assert runner.applied()!.map(it.name) == ['one']
	assert runner.history_shape_resolved
	assert runner.history_has_metadata

	mut fallback_recorder := &RecordingConnection{
		history_rows: [orm.Row{
			vals: ['1']
		}]
		history_metadata_error: 'no such column: name'
		history_version_error: 'database connection timed out'
	}
	mut fallback_runner := new(mut fallback_recorder, []Migration{}, Config{ dialect: .sqlite })!
	error_message = ''
	fallback_runner.applied() or { error_message = err.msg() }
	assert error_message == 'database connection timed out'
	assert !fallback_runner.history_shape_resolved
	fallback_recorder.history_version_error = ''
	assert fallback_runner.applied()!.map(it.version) == [i64(1)]
	assert fallback_runner.history_shape_resolved
	assert !fallback_runner.history_has_metadata
}

fn test_missing_history_metadata_errors_are_dialect_specific() {
	assert missing_history_metadata_columns_error(.sqlite, 'no such column: name')
	assert missing_history_metadata_columns_error(.pg, 'column "applied_at" does not exist')
	assert missing_history_metadata_columns_error(.mysql, "Unknown column 'name' in 'field list'")
	assert !missing_history_metadata_columns_error(.sqlite, 'database connection timed out')
	assert !missing_history_metadata_columns_error(.pg, 'permission denied for table name')
	assert !missing_history_metadata_columns_error(.mysql, 'connection reset')
}

fn test_history_rejects_duplicate_numeric_versions() {
	mut recorder := &RecordingConnection{
		history_rows: [
			orm.Row{
				vals: ['1', 'one', '2026-08-16T00:00:00Z']
			},
			orm.Row{
				vals: ['1', 'duplicate', '2026-08-17T00:00:00Z']
			},
		]
	}
	mut runner := new(mut recorder, []Migration{}, Config{ dialect: .sqlite })!
	mut error_message := ''
	runner.applied() or { error_message = err.msg() }
	assert error_message == 'migration history contains duplicate version 1'
}

fn test_legacy_v_history_tables_keep_metadata_writes() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	db.exec('CREATE TABLE schema_migrations (version BIGINT PRIMARY KEY, name VARCHAR(255) NOT NULL, applied_at VARCHAR(32) NOT NULL);')!
	mut runner := new(mut db, [
		Migration{
			version: 1
			name: 'legacy_metadata'
			up: no_op_migration
			down: no_op_migration
		},
	], Config{ dialect: .sqlite })!
	runner.migrate()!
	assert db.q_string('SELECT name FROM schema_migrations WHERE version = 1;')! == 'legacy_metadata'
	assert db.q_string('SELECT applied_at FROM schema_migrations WHERE version = 1;')! != ''
	runner.rollback_last()!
	assert db.q_int('SELECT count(*) FROM schema_migrations;')! == 0
}

fn test_migrate_to_requires_an_exact_registered_target() {
	mut recorder := &RecordingConnection{}
	mut runner := new(mut recorder, [
		Migration{
			version: 10
			name: 'ten'
			up: no_op_migration
			down: no_op_migration
		},
	], Config{ dialect: .sqlite })!
	mut error_message := ''
	runner.migrate_to(5) or { error_message = err.msg() }
	assert error_message == 'unknown migration target version 5'
	assert recorder.queries.len == 0
}

fn test_migration_transaction_mode_overrides_config() {
	mut always_recorder := &RecordingConnection{}
	mut always_runner := new(mut always_recorder, [
		Migration{
			version: 1
			name: 'always'
			up: no_op_migration
			down: no_op_migration
			transaction_mode: .always
		},
	], Config{
		dialect: .pg
		transaction_mode: .never
	})!
	always_runner.migrate()!
	assert 'ORM BEGIN' in always_recorder.queries
	assert 'ORM COMMIT' in always_recorder.queries

	mut never_recorder := &RecordingConnection{}
	mut never_runner := new(mut never_recorder, [
		Migration{
			version: 1
			name: 'never'
			up: no_op_migration
			down: no_op_migration
			transaction_mode: .never
		},
	], Config{
		dialect: .pg
		transaction_mode: .always
	})!
	never_runner.migrate()!
	assert 'ORM BEGIN' !in never_recorder.queries
	assert 'ORM COMMIT' !in never_recorder.queries
}

fn test_duplicate_migration_names_are_rejected() {
	mut recorder := &RecordingConnection{}
	new(mut recorder, [
		Migration{
			version: 1
			name: 'duplicate'
			up: no_op_migration
			down: no_op_migration
		},
		Migration{
			version: 2
			name: 'duplicate'
			up: no_op_migration
			down: no_op_migration
		},
	], Config{ dialect: .sqlite }) or {
		assert err.msg() == 'duplicate migration name `duplicate`'
		assert recorder.queries.len == 0
		return
	}
	assert false
}
