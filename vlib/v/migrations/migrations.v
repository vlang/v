module migrations

import hash.fnv1a
import orm
import strconv
import time

const migration_lock_timeout_seconds = 60
const postgresql_transaction_probe_setting = 'v3_migrations.transaction_probe'

// MigrationFn changes the schema through a migration Context.
pub type MigrationFn = fn (mut Context) !

// TransactionMode controls whether a migration is wrapped in a transaction.
pub enum TransactionMode {
	automatic
	always
	never
}

// Migration is one reversible, versioned database schema change.
//
// Versions are positive integers. Timestamp-shaped versions such as
// `20260816143000` make migrations naturally sortable, like Rails migrations.
pub struct Migration {
pub:
	version          i64
	name             string
	up               MigrationFn @[required]
	down             MigrationFn @[required]
	transaction_mode ?TransactionMode
}

// Config configures a Migrator.
pub struct Config {
pub:
	dialect          Dialect @[required]
	table            string          = 'schema_migrations'
	transaction_mode TransactionMode = .automatic
}

// AppliedMigration is a migration recorded in the database history table.
pub struct AppliedMigration {
pub:
	version    i64
	name       string
	applied_at string
}

// MigrationState describes the relationship between code and database history.
pub enum MigrationState {
	applied
	pending
	missing
}

enum MigrationOperation {
	migrate_to
	rollback
	redo
	reset
}

// Status is one row returned by Migrator.status.
// A missing row was applied to the database but is no longer registered in code.
pub struct Status {
pub:
	version    i64
	name       string
	state      MigrationState
	applied_at string
}

// Migrator applies an ordered set of migrations and records their versions.
pub struct Migrator {
mut:
	conn                       orm.TransactionalConnection
	migrations                 []Migration
	config                     Config
	pg_lock_key                ?i64
	mysql_lock_name            string
	sqlite_lock_active         bool
	sqlite_transaction_probe   string
	resolved_history_namespace string
	resolved_history_table_sql string
	history_shape_resolved     bool
	history_has_metadata       bool
}

// new creates and validates a migrator. It does not access the database until
// one of the migration or inspection methods is called.
pub fn new(mut conn orm.TransactionalConnection, registered []Migration, config Config) !Migrator {
	validate_identifier_for_dialect(config.dialect, config.table, 'migration history table')!
	if config.dialect == .pg {
		if mut conn is orm.DB {
			return error('PostgreSQL migrations require a direct session-pinned connection; orm.DB wrappers cannot be validated without mutating the wrapped connection; pass pg.Conn directly')
		}
	}
	mut ordered := registered.clone()
	ordered.sort_with_compare(compare_migrations)
	mut previous := i64(0)
	mut migration_names := map[string]bool{}
	for i, migration in ordered {
		if migration.version <= 0 {
			return error('migration `${migration.name}` has invalid version ${migration.version}; versions must be positive')
		}
		if migration.name.trim_space() == '' {
			return error('migration ${migration.version} must have a name')
		}
		if migration.name.contains_u8(u8(0)) {
			return error('migration ${migration.version} name must not contain NUL bytes')
		}
		if migration.name.len > 255 {
			return error('migration ${migration.version} name must not exceed 255 bytes')
		}
		if migration.name in migration_names {
			return error('duplicate migration name `${migration.name}`')
		}
		migration_names[migration.name] = true
		if i > 0 && migration.version == previous {
			return error('duplicate migration version ${migration.version}')
		}
		previous = migration.version
	}
	return Migrator{
		conn:       conn
		migrations: ordered
		config:     config
	}
}

fn compare_migrations(a &Migration, b &Migration) int {
	if a.version < b.version {
		return -1
	}
	if a.version > b.version {
		return 1
	}
	return 0
}

fn compare_applied_desc(a &AppliedMigration, b &AppliedMigration) int {
	if a.version > b.version {
		return -1
	}
	if a.version < b.version {
		return 1
	}
	return 0
}

fn compare_applied_asc(a &AppliedMigration, b &AppliedMigration) int {
	if a.version < b.version {
		return -1
	}
	if a.version > b.version {
		return 1
	}
	return 0
}

// migrate applies every pending migration in ascending version order.
pub fn (mut m Migrator) migrate() ![]AppliedMigration {
	return m.run_locked(.migrate_to, max_i64)
}

// migrate_to moves the schema to an exact registered target version. Pending
// migrations at or below the target are applied; applied migrations above it
// are rolled back. Zero rolls the schema back completely.
pub fn (mut m Migrator) migrate_to(target_version i64) ![]AppliedMigration {
	if target_version < 0 {
		return error('migration target version must not be negative')
	}
	if target_version != 0 && m.find_migration(target_version) == none {
		return error('unknown migration target version ${target_version}')
	}
	return m.run_locked(.migrate_to, target_version)
}

fn (mut m Migrator) migrate_to_unlocked(target_version i64) ![]AppliedMigration {
	applied := m.applied()!
	mut applied_versions := map[i64]bool{}
	for item in applied {
		applied_versions[item.version] = true
	}
	mut changed := []AppliedMigration{}
	mut descending := applied.clone()
	descending.sort_with_compare(compare_applied_desc)
	for item in descending {
		if item.version > target_version && m.find_migration(item.version) == none {
			return error('cannot roll back migration ${item.version}: its migration file is missing')
		}
	}
	for item in descending {
		if item.version <= target_version {
			continue
		}
		migration := m.find_migration(item.version) or {
			return error('cannot roll back migration ${item.version}: its migration file is missing')
		}
		m.run_down(migration)!
		changed << item
		applied_versions.delete(item.version)
	}
	for migration in m.migrations {
		if migration.version > target_version || migration.version in applied_versions {
			continue
		}
		item := m.run_up(migration)!
		changed << item
		applied_versions[migration.version] = true
	}
	return changed
}

// rollback reverts the newest `steps` applied migrations.
pub fn (mut m Migrator) rollback(steps int) ![]AppliedMigration {
	if steps < 1 {
		return error('rollback steps must be at least 1')
	}
	return m.run_locked(.rollback, i64(steps))
}

fn (mut m Migrator) rollback_unlocked(steps int) ![]AppliedMigration {
	mut applied := m.applied()!
	applied.sort_with_compare(compare_applied_desc)
	mut reverted := []AppliedMigration{}
	limit := if steps < applied.len { steps } else { applied.len }
	for item in applied[..limit] {
		if m.find_migration(item.version) == none {
			return error('cannot roll back migration ${item.version}: its migration file is missing')
		}
	}
	for item in applied[..limit] {
		migration := m.find_migration(item.version) or {
			return error('cannot roll back migration ${item.version}: its migration file is missing')
		}
		m.run_down(migration)!
		reverted << item
	}
	return reverted
}

// rollback_last reverts the newest applied migration.
pub fn (mut m Migrator) rollback_last() ![]AppliedMigration {
	return m.rollback(1)
}

// redo rolls back the newest `steps` migrations and applies them again.
pub fn (mut m Migrator) redo(steps int) ![]AppliedMigration {
	if steps < 1 {
		return error('rollback steps must be at least 1')
	}
	return m.run_locked(.redo, i64(steps))
}

fn (mut m Migrator) redo_unlocked(steps int) ![]AppliedMigration {
	reverted := m.rollback_unlocked(steps)!
	if reverted.len == 0 {
		return []
	}
	mut versions := map[i64]bool{}
	for item in reverted {
		versions[item.version] = true
	}
	mut reapplied := []AppliedMigration{}
	for migration in m.migrations {
		if migration.version in versions {
			reapplied << m.run_up(migration)!
		}
	}
	return reapplied
}

// redo_last rolls back and reapplies the newest migration.
pub fn (mut m Migrator) redo_last() ![]AppliedMigration {
	return m.redo(1)
}

// reset rolls back every applied migration while preserving the history table.
pub fn (mut m Migrator) reset() ![]AppliedMigration {
	return m.run_locked(.reset, 0)
}

fn (mut m Migrator) reset_unlocked() ![]AppliedMigration {
	applied := m.applied()!
	if applied.len == 0 {
		return []
	}
	return m.rollback_unlocked(applied.len)
}

fn (mut m Migrator) run_locked(operation MigrationOperation, argument i64) ![]AppliedMigration {
	m.acquire_migration_lock()!
	result := m.run_unlocked(operation, argument) or {
		operation_err := err
		m.release_migration_lock(false) or {
			return error('${operation_err.msg()}; releasing migration lock failed: ${err.msg()}')
		}
		return operation_err
	}
	m.release_migration_lock(true)!
	return result
}

fn (mut m Migrator) run_unlocked(operation MigrationOperation, argument i64) ![]AppliedMigration {
	match operation {
		.migrate_to {
			return m.migrate_to_unlocked(argument)
		}
		.rollback {
			return m.rollback_unlocked(int(argument))
		}
		.redo {
			return m.redo_unlocked(int(argument))
		}
		.reset {
			return m.reset_unlocked()
		}
	}
}

fn missing_history_metadata_columns_error(dialect Dialect, message string) bool {
	lower := message.to_lower_ascii()
	if !lower.contains('name') && !lower.contains('applied_at') {
		return false
	}
	return match dialect {
		.sqlite { lower.contains('no such column') }
		.pg { lower.contains('column') && lower.contains('does not exist') }
		.mysql { lower.contains('unknown column') }
	}
}

// applied returns the migrations recorded in the database, oldest first.
// Rails-compatible version-only schema_migrations tables are supported alongside
// legacy V history tables that include name and applied_at metadata.
pub fn (mut m Migrator) applied() ![]AppliedMigration {
	m.ensure_history_table()!
	table := m.history_table_sql()
	mut rows := []orm.Row{}
	if m.history_shape_resolved && !m.history_has_metadata {
		rows = m.conn.execute('SELECT version FROM ${table} ORDER BY version ASC;')!
	} else {
		rows = m.conn.execute('SELECT version, name, applied_at FROM ${table} ORDER BY version ASC;') or {
			if !missing_history_metadata_columns_error(m.config.dialect, err.msg()) {
				return err
			}
			version_rows := m.conn.execute('SELECT version FROM ${table} ORDER BY version ASC;')!
			m.history_shape_resolved = true
			m.history_has_metadata = false
			version_rows
		}
		if !m.history_shape_resolved {
			m.history_shape_resolved = true
			m.history_has_metadata = true
		}
	}
	minimum_columns := if m.history_has_metadata { 3 } else { 1 }
	mut result := []AppliedMigration{cap: rows.len}
	mut versions := map[i64]bool{}
	for row in rows {
		if row.vals.len < minimum_columns {
			return error('migration history query returned ${row.vals.len} columns; expected ${minimum_columns}')
		}
		version := strconv.parse_int(row.vals[0], 10, 64) or {
			return error('migration history contains invalid version `${row.vals[0]}`')
		}
		if version <= 0 {
			return error('migration history version `${row.vals[0]}` must be positive')
		}
		if !m.history_has_metadata && row.vals[0] != version.str() {
			return error('migration history contains noncanonical version `${row.vals[0]}`')
		}
		if version in versions {
			return error('migration history contains duplicate version ${version}')
		}
		versions[version] = true
		mut name := if m.history_has_metadata { row.vals[1] } else { '' }
		if name == '' {
			if migration := m.find_migration(version) {
				name = migration.name
			}
		}
		result << AppliedMigration{
			version:    version
			name:       name
			applied_at: if m.history_has_metadata { row.vals[2] } else { '' }
		}
	}
	result.sort_with_compare(compare_applied_asc)
	return result
}

// pending returns registered migrations that have not been applied yet.
pub fn (mut m Migrator) pending() ![]Migration {
	applied := m.applied()!
	mut versions := map[i64]bool{}
	for item in applied {
		versions[item.version] = true
	}
	return m.migrations.filter(it.version !in versions)
}

// current_version returns the greatest applied version, or zero for an empty schema.
pub fn (mut m Migrator) current_version() !i64 {
	applied := m.applied()!
	mut latest := i64(0)
	for item in applied {
		if item.version > latest {
			latest = item.version
		}
	}
	return latest
}

// status reports applied and pending migrations, plus applied versions whose
// migration definitions are missing from the program.
pub fn (mut m Migrator) status() ![]Status {
	applied := m.applied()!
	mut applied_by_version := map[i64]AppliedMigration{}
	mut known_versions := map[i64]bool{}
	for item in applied {
		applied_by_version[item.version] = item
	}
	mut result := []Status{cap: applied.len + m.migrations.len}
	for migration in m.migrations {
		known_versions[migration.version] = true
		if migration.version in applied_by_version {
			item := applied_by_version[migration.version]
			result << Status{
				version:    migration.version
				name:       migration.name
				state:      .applied
				applied_at: item.applied_at
			}
		} else {
			result << Status{
				version: migration.version
				name:    migration.name
				state:   .pending
			}
		}
	}
	for item in applied {
		if item.version !in known_versions {
			result << Status{
				version:    item.version
				name:       item.name
				state:      .missing
				applied_at: item.applied_at
			}
		}
	}
	result.sort_with_compare(compare_status)
	return result
}

fn compare_status(a &Status, b &Status) int {
	if a.version < b.version {
		return -1
	}
	if a.version > b.version {
		return 1
	}
	return 0
}

fn (m &Migrator) find_migration(version i64) ?Migration {
	for migration in m.migrations {
		if migration.version == version {
			return migration
		}
	}
	return none
}

fn (mut m Migrator) ensure_history_table() ! {
	match m.config.dialect {
		.pg {
			if m.pg_lock_key == none {
				m.reject_existing_postgresql_transaction()!
			}
			m.resolve_postgresql_history_schema()!
		}
		.mysql {
			if m.mysql_lock_name == '' {
				m.validate_mysql_session()!
			}
			m.resolve_mysql_history_database()!
		}
		.sqlite {}
	}
	table := m.history_table_sql()
	// Match Rails' version-only schema_migrations shape. Existing V history
	// tables with name/applied_at metadata remain supported.
	m.conn.execute('CREATE TABLE IF NOT EXISTS ${table} (version VARCHAR(255) PRIMARY KEY);')!
}

fn (m &Migrator) history_table_sql() string {
	if m.resolved_history_table_sql != '' {
		return m.resolved_history_table_sql
	}
	if m.config.dialect == .sqlite && !m.config.table.contains('.') {
		return qualified_history_table_sql(.sqlite, 'main', m.config.table)
	}
	return quote_identifier(m.config.dialect, m.config.table)
}

fn (mut m Migrator) acquire_migration_lock() ! {
	match m.config.dialect {
		.sqlite {
			m.prepare_sqlite_transaction_probe()!
			m.conn.execute('BEGIN IMMEDIATE;') or {
				lock_err := err
				m.release_sqlite_migration_lock(false) or {
					return error('${lock_err.msg()}; cleaning up SQLite lock acquisition state failed: ${err.msg()}')
				}
				return lock_err
			}
			m.sqlite_lock_active = true
			m.conn.execute('SAVEPOINT ${m.sqlite_transaction_probe};') or {
				lock_err := err
				m.release_sqlite_migration_lock(false) or {
					return error('${lock_err.msg()}; cleaning up SQLite lock acquisition state failed: ${err.msg()}')
				}
				return lock_err
			}
			namespace := if m.resolved_history_namespace != '' {
				m.resolved_history_namespace
			} else if m.config.table.contains('.') {
				m.config.table.all_before('.')
			} else {
				'main'
			}
			m.retain_history_relation(.sqlite, namespace)
		}
		.pg {
			m.reject_existing_postgresql_transaction()!
			schema := m.resolve_postgresql_history_schema()!
			key := postgresql_migration_lock_key(schema, m.config.table)
			m.conn.execute('SELECT pg_advisory_lock(${key});')!
			m.pg_lock_key = key
			m.retain_history_relation(.pg, schema)
		}
		.mysql {
			m.validate_mysql_session()!
			database := m.resolve_mysql_history_database()!
			case_rows := m.conn.execute('SELECT @@lower_case_table_names;')!
			lower_case_table_names := mysql_lower_case_table_names(case_rows)!
			name := mysql_migration_lock_name(database, m.config.table, lower_case_table_names)
			rows :=
				m.conn.execute("SELECT GET_LOCK('${name}', ${migration_lock_timeout_seconds});")!
			if !migration_lock_result(rows) {
				return error('could not acquire MySQL migration lock `${name}` within ${migration_lock_timeout_seconds} seconds')
			}
			m.mysql_lock_name = name
			m.retain_history_relation(.mysql, database)
		}
	}
}

fn (mut m Migrator) resolve_postgresql_history_schema() !string {
	if m.resolved_history_namespace != '' {
		return m.resolved_history_namespace
	}
	schema := if m.config.table.contains('.') {
		m.config.table.all_before('.')
	} else {
		schema_rows := m.conn.execute(postgresql_history_schema_query(m.config.table))!
		postgresql_schema_name(schema_rows)!
	}
	m.retain_history_relation(.pg, schema)
	return schema
}

fn (mut m Migrator) resolve_mysql_history_database() !string {
	if m.resolved_history_namespace != '' {
		return m.resolved_history_namespace
	}
	database := if m.config.table.contains('.') {
		m.config.table.all_before('.')
	} else {
		database_rows := m.conn.execute('SELECT DATABASE();')!
		mysql_database_name(database_rows)!
	}
	m.retain_history_relation(.mysql, database)
	return database
}

fn (mut m Migrator) reject_existing_postgresql_transaction() ! {
	if m.postgresql_transaction_is_active()! {
		return error('PostgreSQL migrations require a connection without an already-open transaction; pg.Tx and transactional pg.Conn values are not supported')
	}
}

fn (mut m Migrator) postgresql_transaction_is_active() !bool {
	token := new_postgresql_transaction_probe_token()
	m.conn.execute(postgresql_transaction_probe_set_query(token))!
	rows := m.conn.execute(postgresql_transaction_probe_read_query())!
	return postgresql_transaction_probe_value(rows)! == token
}

fn new_postgresql_transaction_probe_token() string {
	return 'probe_${time.now().unix_nano()}'
}

fn postgresql_transaction_probe_set_query(token string) string {
	return "SELECT pg_catalog.set_config('${postgresql_transaction_probe_setting}', '${token}', true);"
}

fn postgresql_transaction_probe_read_query() string {
	return "SELECT pg_catalog.current_setting('${postgresql_transaction_probe_setting}', true);"
}

fn postgresql_transaction_probe_value(rows []orm.Row) !string {
	if rows.len != 1 || rows[0].vals.len == 0 {
		return error('could not determine PostgreSQL transaction state')
	}
	return rows[0].vals[0]
}

fn mark_postgresql_owned_transaction(mut ctx Context) !string {
	token := new_postgresql_transaction_probe_token()
	ctx.execute(postgresql_transaction_probe_set_query(token))!
	return token
}

fn verify_postgresql_owned_transaction(mut ctx Context, token string) ! {
	rows := ctx.execute(postgresql_transaction_probe_read_query())!
	if postgresql_transaction_probe_value(rows)! != token {
		return error('PostgreSQL migration callback ended the migrator-owned transaction before history was recorded')
	}
}

fn (mut m Migrator) mark_mysql_owned_transaction() !string {
	savepoint := 'v3_migrations_owned_${time.now().unix_nano()}'
	m.conn.orm_savepoint(savepoint)!
	return savepoint
}

fn (mut m Migrator) verify_mysql_owned_transaction(savepoint string) ! {
	m.conn.orm_release_savepoint(savepoint) or {
		return error('MySQL migration callback ended the migrator-owned transaction before history was recorded')
	}
}

fn (mut m Migrator) validate_mysql_session() ! {
	autocommit_rows := m.conn.execute('SELECT @@autocommit;')!
	if !mysql_autocommit_enabled(autocommit_rows)! {
		return error('MySQL migrations require session autocommit to be enabled')
	}
	probe := 'v3_migrations_transaction_probe_${time.now().unix_nano()}'
	m.conn.orm_savepoint(probe) or {
		// MySQL rejects the savepoint outside a transaction, or discards it at
		// the end of the probe statement when autocommit is enabled.
		return
	}
	m.conn.orm_release_savepoint(probe) or { return }
	return error('MySQL migrations require a connection without an already-open transaction')
}

fn (mut m Migrator) retain_history_relation(dialect Dialect, namespace string) {
	if m.resolved_history_namespace != '' {
		return
	}
	m.resolved_history_namespace = namespace
	m.resolved_history_table_sql = qualified_history_table_sql(dialect, namespace, m.config.table)
}

fn (mut m Migrator) release_migration_lock(success bool) ! {
	match m.config.dialect {
		.sqlite {
			m.release_sqlite_migration_lock(success)!
		}
		.pg {
			key := m.pg_lock_key or {
				return error('cannot release PostgreSQL migration lock before it is acquired')
			}

			rows := m.conn.execute('SELECT pg_advisory_unlock(${key});')!
			if !migration_lock_result(rows) {
				return error('could not release PostgreSQL migration lock ${key}')
			}
			m.pg_lock_key = none
		}
		.mysql {
			name := m.mysql_lock_name
			if name == '' {
				return error('cannot release MySQL migration lock before it is acquired')
			}
			rows := m.conn.execute("SELECT RELEASE_LOCK('${name}');")!
			if !migration_lock_result(rows) {
				return error('could not release MySQL migration lock `${name}`')
			}
			m.mysql_lock_name = ''
		}
	}
}

fn (mut m Migrator) release_sqlite_migration_lock(success bool) ! {
	mut release_error := ''
	mut transaction_ended := false
	if m.sqlite_lock_active {
		if success {
			m.conn.orm_commit() or {
				commit_err := err
				m.conn.orm_rollback() or {
					release_error = '${commit_err.msg()}; rolling back the failed SQLite lock transaction failed: ${err.msg()}'
				}
				if release_error == '' {
					transaction_ended = true
					release_error = commit_err.msg()
				}
			}
			if release_error == '' {
				transaction_ended = true
			}
		} else {
			m.conn.orm_rollback() or { release_error = err.msg() }
			transaction_ended = release_error == ''
		}
		if transaction_ended {
			m.sqlite_lock_active = false
		}
	}
	m.cleanup_sqlite_transaction_probe() or {
		if release_error != '' {
			return error('${release_error}; cleaning up the SQLite transaction probe failed: ${err.msg()}')
		}
		return err
	}
	if release_error != '' {
		return error(release_error)
	}
}

fn migration_lock_key(identity string) i64 {
	hash := fnv1a.sum64_string(identity)
	if hash <= u64(max_i64) {
		return i64(hash)
	}
	return -i64(~hash) - 1
}

fn postgresql_migration_lock_key(schema string, table string) i64 {
	table_name := if table.contains('.') { table.all_after('.') } else { table }
	identity := '${schema.len}:${schema}:${table_name.len}:${table_name}'
	return migration_lock_key(identity)
}

fn postgresql_history_schema_query(table string) string {
	return 'WITH persistent_search_path AS (SELECT n.oid AS namespace_oid, schemas.schema_name, schemas.search_order FROM pg_catalog.unnest(pg_catalog.current_schemas(false)) WITH ORDINALITY AS schemas(schema_name, search_order) JOIN pg_catalog.pg_namespace AS n ON n.nspname = schemas.schema_name WHERE n.oid <> pg_catalog.pg_my_temp_schema()) SELECT COALESCE((SELECT path.schema_name FROM persistent_search_path AS path JOIN pg_catalog.pg_class AS c ON c.relnamespace = path.namespace_oid WHERE c.relname = ${string_literal_sql(.pg,
		table)} ORDER BY path.search_order LIMIT 1), (SELECT path.schema_name FROM persistent_search_path AS path ORDER BY path.search_order LIMIT 1));'
}

fn qualified_history_table_sql(dialect Dialect, namespace string, table string) string {
	table_name := if table.contains('.') { table.all_after('.') } else { table }
	return '${quote_identifier_component(dialect, namespace)}.${quote_identifier_component(dialect,
		table_name)}'
}

fn mysql_migration_lock_name(database string, table string, lower_case_table_names int) string {
	mut database_name := database
	mut table_name := if table.contains('.') { table.all_after('.') } else { table }
	if lower_case_table_names in [1, 2] {
		database_name = database_name.to_lower_ascii()
		table_name = table_name.to_lower_ascii()
	}
	identity := '${database_name.len}:${database_name}:${table_name.len}:${table_name}'
	return 'v3_migrations_${fnv1a.sum64_string(identity).hex()}'
}

fn mysql_database_name(rows []orm.Row) !string {
	if rows.len != 1 || rows[0].vals.len == 0 || rows[0].vals[0] == '' {
		return error('could not determine current MySQL database for migration lock')
	}
	return rows[0].vals[0]
}

fn mysql_lower_case_table_names(rows []orm.Row) !int {
	if rows.len != 1 || rows[0].vals.len == 0 {
		return error('could not determine MySQL lower_case_table_names for migration lock')
	}
	value := strconv.atoi(rows[0].vals[0]) or {
		return error('could not determine MySQL lower_case_table_names for migration lock')
	}
	if value !in [0, 1, 2] {
		return error('unsupported MySQL lower_case_table_names value `${rows[0].vals[0]}`')
	}
	return value
}

fn mysql_autocommit_enabled(rows []orm.Row) !bool {
	if rows.len != 1 || rows[0].vals.len == 0 {
		return error('could not determine MySQL session autocommit state')
	}
	return match rows[0].vals[0].to_lower_ascii() {
		'1', 'on', 'true' { true }
		'0', 'off', 'false' { false }
		else { return error('unsupported MySQL session autocommit value `${rows[0].vals[0]}`') }
	}
}

fn postgresql_schema_name(rows []orm.Row) !string {
	if rows.len != 1 || rows[0].vals.len == 0 || rows[0].vals[0] == '' {
		return error('could not determine PostgreSQL migration history schema for migration lock')
	}
	return rows[0].vals[0]
}

fn migration_lock_result(rows []orm.Row) bool {
	return rows.len == 1 && rows[0].vals.len > 0 && rows[0].vals[0] in ['1', 't', 'true']
}

fn postgresql_migration_lock_owned_query(key i64) string {
	// pg_locks merges session- and transaction-level ownership for the same tag.
	// The transaction guard keeps exclusion while the session lock is probed and restored.
	return 'WITH transaction_guard AS (SELECT pg_catalog.pg_try_advisory_xact_lock(${key}) AS acquired), session_unlock AS (SELECT pg_catalog.pg_advisory_unlock(${key}) AS held FROM transaction_guard WHERE acquired), session_relock AS (SELECT pg_catalog.pg_advisory_lock(${key}) FROM session_unlock WHERE held) SELECT COALESCE((SELECT session_unlock.held FROM session_unlock LEFT JOIN session_relock ON true), false);'
}

fn (m &Migrator) verify_postgresql_migration_lock(mut ctx Context) ! {
	key := m.pg_lock_key or {
		return error('cannot verify PostgreSQL migration lock before it is acquired')
	}

	rows := ctx.execute(postgresql_migration_lock_owned_query(key))!
	if !migration_lock_result(rows) {
		return error('PostgreSQL migration callback released the migration advisory lock before history was recorded')
	}
}

fn mysql_migration_lock_owned_query(name string) string {
	return "SELECT COALESCE(IS_USED_LOCK('${name}') = CONNECTION_ID(), 0);"
}

fn (m &Migrator) verify_mysql_migration_lock(mut ctx Context) ! {
	name := m.mysql_lock_name
	if name == '' {
		return error('cannot verify MySQL migration lock before it is acquired')
	}

	rows := ctx.execute(mysql_migration_lock_owned_query(name))!
	if !migration_lock_result(rows) {
		return error('MySQL migration callback released the named migration lock before history was recorded')
	}
}

fn (mut m Migrator) prepare_sqlite_transaction_probe() ! {
	m.sqlite_transaction_probe = 'v3_migrations_transaction_${time.now().unix_nano()}'
	table := sqlite_transaction_probe_table_sql(m.sqlite_transaction_probe)
	m.conn.execute('CREATE TEMP TABLE ${table} (marker INTEGER NOT NULL);')!
}

fn (mut m Migrator) cleanup_sqlite_transaction_probe() ! {
	if m.sqlite_transaction_probe == '' {
		return
	}
	table := sqlite_transaction_probe_table_sql(m.sqlite_transaction_probe)
	m.conn.execute('DROP TABLE IF EXISTS ${table};')!
	m.sqlite_transaction_probe = ''
}

fn (mut m Migrator) verify_sqlite_lock_transaction() ! {
	if m.sqlite_transaction_probe == '' {
		return error('cannot verify SQLite migration lock transaction before it is acquired')
	}
	table := sqlite_transaction_probe_table_sql(m.sqlite_transaction_probe)
	check_savepoint := '${m.sqlite_transaction_probe}_check'
	m.conn.execute('SAVEPOINT ${check_savepoint};')!
	m.conn.execute('RELEASE SAVEPOINT ${m.sqlite_transaction_probe};') or {
		return error('SQLite migration callback ended the migration lock transaction before history was recorded')
	}
	m.conn.execute('INSERT INTO ${table} (marker) VALUES (1);')!
	m.conn.execute('ROLLBACK TO SAVEPOINT ${check_savepoint};') or {
		// Releasing the original savepoint also released the nested check
		// savepoint, proving that the original lock transaction is still active.
		m.conn.execute('DELETE FROM ${table};')!
		m.conn.execute('SAVEPOINT ${m.sqlite_transaction_probe};')!
		return
	}
	rows := m.conn.execute('SELECT COUNT(*) FROM ${table};')!
	marker_count := sqlite_transaction_probe_count(rows)!
	m.conn.execute('DELETE FROM ${table};')!
	m.conn.execute('RELEASE SAVEPOINT ${check_savepoint};') or {}
	if marker_count == 1 {
		m.conn.execute('SAVEPOINT ${m.sqlite_transaction_probe};')!
		return
	}
	state_err := 'SQLite migration callback ended the migration lock transaction before history was recorded'
	return error(state_err)
}

fn sqlite_transaction_probe_table_sql(name string) string {
	return 'temp.${quote_identifier_component(.sqlite, name)}'
}

fn sqlite_transaction_probe_count(rows []orm.Row) !int {
	if rows.len != 1 || rows[0].vals.len == 0 {
		return error('could not determine SQLite transaction state')
	}
	return strconv.atoi(rows[0].vals[0]) or {
		return error('could not determine SQLite transaction state')
	}
}

fn (m &Migrator) uses_transactions(migration Migration) bool {
	mode := migration.transaction_mode or { m.config.transaction_mode }
	return match mode {
		.always { true }
		.never { false }
		.automatic { m.config.dialect != .mysql }
	}
}

fn (mut m Migrator) run_up(migration Migration) !AppliedMigration {
	m.ensure_history_table()!
	applied_at := time.utc().format_rfc3339()
	if m.config.dialect != .sqlite && m.uses_transactions(migration) {
		mut tx := orm.begin(mut m.conn)!
		mut ctx := new_context(tx, m.config.dialect)
		mut postgresql_transaction_token := ''
		mut mysql_transaction_savepoint := ''
		if m.config.dialect == .pg {
			postgresql_transaction_token = mark_postgresql_owned_transaction(mut ctx) or {
				probe_err := err
				tx.rollback() or {
					return error('could not mark the migrator-owned PostgreSQL transaction: ${probe_err.msg()}; rollback failed: ${err.msg()}')
				}
				return error('could not mark the migrator-owned PostgreSQL transaction: ${probe_err.msg()}')
			}
		} else if m.config.dialect == .mysql {
			mysql_transaction_savepoint = m.mark_mysql_owned_transaction() or {
				probe_err := err
				tx.rollback() or {
					return error('could not mark the migrator-owned MySQL transaction: ${probe_err.msg()}; rollback failed: ${err.msg()}')
				}
				return error('could not mark the migrator-owned MySQL transaction: ${probe_err.msg()}')
			}
		}
		migration.up(mut ctx) or {
			migration_err := err
			tx.rollback() or {
				return error('migration ${migration.version} `${migration.name}` failed: ${migration_err.msg()}; rollback failed: ${err.msg()}')
			}
			return error('migration ${migration.version} `${migration.name}` failed: ${migration_err.msg()}')
		}
		if m.config.dialect == .pg {
			verify_postgresql_owned_transaction(mut ctx, postgresql_transaction_token) or {
				transaction_err := err
				tx.rollback() or {
					return error('${transaction_err.msg()}; rollback failed: ${err.msg()}')
				}
				return transaction_err
			}
			m.verify_postgresql_migration_lock(mut ctx) or {
				lock_err := err
				tx.rollback() or {
					return error('${lock_err.msg()}; rollback failed: ${err.msg()}')
				}
				return lock_err
			}
		} else if m.config.dialect == .mysql {
			m.verify_mysql_owned_transaction(mysql_transaction_savepoint) or {
				transaction_err := err
				tx.rollback() or {
					return error('${transaction_err.msg()}; rollback failed: ${err.msg()}')
				}
				return transaction_err
			}
			m.verify_mysql_migration_lock(mut ctx) or {
				lock_err := err
				tx.rollback() or {
					return error('${lock_err.msg()}; rollback failed: ${err.msg()}')
				}
				return lock_err
			}
		}
		ctx.execute(m.history_insert_sql(migration, applied_at)) or {
			history_err := err
			tx.rollback() or {
				return error('could not record migration ${migration.version}: ${history_err.msg()}; rollback failed: ${err.msg()}')
			}
			return error('could not record migration ${migration.version}: ${history_err.msg()}')
		}
		tx.commit()!
	} else {
		mut ctx := new_context(m.conn, m.config.dialect)
		migration.up(mut ctx) or {
			migration_err := err
			m.validate_session_after_callback() or {
				return error('${migration_err.msg()}; ${err.msg()}')
			}
			return migration_err
		}
		match m.config.dialect {
			.sqlite {
				m.verify_sqlite_lock_transaction()!
			}
			.pg {
				m.verify_postgresql_migration_lock(mut ctx) or {
					lock_err := err
					m.validate_session_after_callback() or {
						return error('${lock_err.msg()}; ${err.msg()}')
					}
					return lock_err
				}
			}
			.mysql {
				m.verify_mysql_migration_lock(mut ctx) or {
					lock_err := err
					m.validate_session_after_callback() or {
						return error('${lock_err.msg()}; ${err.msg()}')
					}
					return lock_err
				}
			}
		}
		ctx.execute(m.history_insert_sql(migration, applied_at)) or {
			history_err := err
			m.validate_session_after_callback() or {
				return error('could not record migration ${migration.version}: ${history_err.msg()}; ${err.msg()}')
			}
			return error('could not record migration ${migration.version}: ${history_err.msg()}')
		}
	}
	m.validate_session_after_callback()!
	return AppliedMigration{
		version:    migration.version
		name:       migration.name
		applied_at: applied_at
	}
}

fn (mut m Migrator) run_down(migration Migration) ! {
	if m.config.dialect != .sqlite && m.uses_transactions(migration) {
		mut tx := orm.begin(mut m.conn)!
		mut ctx := new_context(tx, m.config.dialect)
		mut postgresql_transaction_token := ''
		mut mysql_transaction_savepoint := ''
		if m.config.dialect == .pg {
			postgresql_transaction_token = mark_postgresql_owned_transaction(mut ctx) or {
				probe_err := err
				tx.rollback() or {
					return error('could not mark the migrator-owned PostgreSQL transaction: ${probe_err.msg()}; transaction rollback failed: ${err.msg()}')
				}
				return error('could not mark the migrator-owned PostgreSQL transaction: ${probe_err.msg()}')
			}
		} else if m.config.dialect == .mysql {
			mysql_transaction_savepoint = m.mark_mysql_owned_transaction() or {
				probe_err := err
				tx.rollback() or {
					return error('could not mark the migrator-owned MySQL transaction: ${probe_err.msg()}; transaction rollback failed: ${err.msg()}')
				}
				return error('could not mark the migrator-owned MySQL transaction: ${probe_err.msg()}')
			}
		}
		migration.down(mut ctx) or {
			migration_err := err
			tx.rollback() or {
				return error('rollback of migration ${migration.version} `${migration.name}` failed: ${migration_err.msg()}; transaction rollback failed: ${err.msg()}')
			}
			return error('rollback of migration ${migration.version} `${migration.name}` failed: ${migration_err.msg()}')
		}
		if m.config.dialect == .pg {
			verify_postgresql_owned_transaction(mut ctx, postgresql_transaction_token) or {
				transaction_err := err
				tx.rollback() or {
					return error('${transaction_err.msg()}; transaction rollback failed: ${err.msg()}')
				}
				return transaction_err
			}
			m.verify_postgresql_migration_lock(mut ctx) or {
				lock_err := err
				tx.rollback() or {
					return error('${lock_err.msg()}; transaction rollback failed: ${err.msg()}')
				}
				return lock_err
			}
		} else if m.config.dialect == .mysql {
			m.verify_mysql_owned_transaction(mysql_transaction_savepoint) or {
				transaction_err := err
				tx.rollback() or {
					return error('${transaction_err.msg()}; transaction rollback failed: ${err.msg()}')
				}
				return transaction_err
			}
			m.verify_mysql_migration_lock(mut ctx) or {
				lock_err := err
				tx.rollback() or {
					return error('${lock_err.msg()}; transaction rollback failed: ${err.msg()}')
				}
				return lock_err
			}
		}
		ctx.execute(m.history_delete_sql(migration.version)) or {
			history_err := err
			tx.rollback() or {
				return error('could not remove migration ${migration.version} from history: ${history_err.msg()}; transaction rollback failed: ${err.msg()}')
			}
			return error('could not remove migration ${migration.version} from history: ${history_err.msg()}')
		}
		tx.commit()!
	} else {
		mut ctx := new_context(m.conn, m.config.dialect)
		migration.down(mut ctx) or {
			migration_err := err
			m.validate_session_after_callback() or {
				return error('${migration_err.msg()}; ${err.msg()}')
			}
			return migration_err
		}
		match m.config.dialect {
			.sqlite {
				m.verify_sqlite_lock_transaction()!
			}
			.pg {
				m.verify_postgresql_migration_lock(mut ctx) or {
					lock_err := err
					m.validate_session_after_callback() or {
						return error('${lock_err.msg()}; ${err.msg()}')
					}
					return lock_err
				}
			}
			.mysql {
				m.verify_mysql_migration_lock(mut ctx) or {
					lock_err := err
					m.validate_session_after_callback() or {
						return error('${lock_err.msg()}; ${err.msg()}')
					}
					return lock_err
				}
			}
		}
		ctx.execute(m.history_delete_sql(migration.version)) or {
			history_err := err
			m.validate_session_after_callback() or {
				return error('could not remove migration ${migration.version} from history: ${history_err.msg()}; ${err.msg()}')
			}
			return error('could not remove migration ${migration.version} from history: ${history_err.msg()}')
		}
	}
	m.validate_session_after_callback()!
}

fn (mut m Migrator) validate_session_after_callback() ! {
	match m.config.dialect {
		.sqlite {}
		.pg {
			m.reject_existing_postgresql_transaction() or {
				session_err := err
				m.conn.orm_rollback() or {
					return error('${session_err.msg()}; rolling back callback-created PostgreSQL transaction state failed: ${err.msg()}')
				}
				return error('PostgreSQL migration callback left unsafe session state: ${session_err.msg()}')
			}
		}
		.mysql {
			m.validate_mysql_session() or {
				session_err := err
				m.conn.orm_rollback() or {
					return error('${session_err.msg()}; rolling back callback-created MySQL transaction state failed: ${err.msg()}')
				}
				return error('MySQL migration callback left unsafe session state: ${session_err.msg()}')
			}
		}
	}
}

fn (m &Migrator) history_insert_sql(migration Migration, applied_at string) string {
	table := m.history_table_sql()
	if m.history_shape_resolved && !m.history_has_metadata {
		version := string_literal_sql(m.config.dialect, migration.version.str())
		return 'INSERT INTO ${table} (version) VALUES (${version});'
	}
	name := string_literal_sql(m.config.dialect, migration.name)
	timestamp := string_literal_sql(m.config.dialect, applied_at)
	return 'INSERT INTO ${table} (version, name, applied_at) VALUES (${migration.version}, ${name}, ${timestamp});'
}

fn (m &Migrator) history_delete_sql(version i64) string {
	table := m.history_table_sql()
	if m.history_shape_resolved && !m.history_has_metadata {
		value := string_literal_sql(m.config.dialect, version.str())
		return 'DELETE FROM ${table} WHERE version = ${value};'
	}
	return 'DELETE FROM ${table} WHERE version = ${version};'
}
