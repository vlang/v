module migrations

import hash.fnv1a
import orm
import strconv
import time

const migration_lock_timeout_seconds = 60

// MigrationFn changes the schema through a migration Context.
pub type MigrationFn = fn (mut Context) !

// Migration is one reversible, versioned database schema change.
//
// Versions are positive integers. Timestamp-shaped versions such as
// `20260816143000` make migrations naturally sortable, like Rails migrations.
pub struct Migration {
pub:
	version i64
	name    string
	up      MigrationFn @[required]
	down    MigrationFn @[required]
}

// TransactionMode controls whether each migration is wrapped in a transaction.
pub enum TransactionMode {
	automatic
	always
	never
}

// Config configures a Migrator.
pub struct Config {
pub:
	dialect          Dialect
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
	resolved_history_namespace string
	resolved_history_table_sql string
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

// migrate applies every pending migration in ascending version order.
pub fn (mut m Migrator) migrate() ![]AppliedMigration {
	return m.migrate_to(max_i64)
}

// migrate_to moves the schema to target_version. Pending migrations at or
// below the target are applied; applied migrations above it are rolled back.
pub fn (mut m Migrator) migrate_to(target_version i64) ![]AppliedMigration {
	if target_version < 0 {
		return error('migration target version must not be negative')
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

// applied returns the migrations recorded in the database, oldest first.
pub fn (mut m Migrator) applied() ![]AppliedMigration {
	m.ensure_history_table()!
	table := m.history_table_sql()
	rows := m.conn.execute('SELECT version, name, applied_at FROM ${table} ORDER BY version ASC;')!
	mut result := []AppliedMigration{cap: rows.len}
	for row in rows {
		if row.vals.len < 3 {
			return error('migration history query returned ${row.vals.len} columns; expected 3')
		}
		version := strconv.parse_int(row.vals[0], 10, 64) or {
			return error('migration history contains invalid version `${row.vals[0]}`')
		}
		result << AppliedMigration{
			version:    version
			name:       row.vals[1]
			applied_at: row.vals[2]
		}
	}
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
	if m.config.dialect == .pg {
		m.resolve_postgresql_history_schema()!
	}
	table := m.history_table_sql()
	m.conn.execute('CREATE TABLE IF NOT EXISTS ${table} (version BIGINT PRIMARY KEY, name VARCHAR(255) NOT NULL, applied_at VARCHAR(32) NOT NULL);')!
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
			m.conn.execute('BEGIN IMMEDIATE;')!
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
			database := if m.resolved_history_namespace != '' {
				m.resolved_history_namespace
			} else if m.config.table.contains('.') {
				m.config.table.all_before('.')
			} else {
				database_rows := m.conn.execute('SELECT DATABASE();')!
				mysql_database_name(database_rows)!
			}
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

fn (mut m Migrator) reject_existing_postgresql_transaction() ! {
	probe := 'v3_migrations_transaction_probe'
	m.conn.orm_savepoint(probe) or {
		// PostgreSQL rejects SAVEPOINT outside a transaction, which is the required
		// state before the migrator acquires its session lock.
		return
	}
	m.conn.orm_release_savepoint(probe) or {
		return error('could not release PostgreSQL transaction ownership probe: ${err.msg()}')
	}
	return error('PostgreSQL migrations require a connection without an already-open transaction; pg.Tx and transactional pg.Conn values are not supported')
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
			if success {
				m.conn.orm_commit()!
			} else {
				m.conn.orm_rollback()!
			}
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

fn postgresql_schema_name(rows []orm.Row) !string {
	if rows.len != 1 || rows[0].vals.len == 0 || rows[0].vals[0] == '' {
		return error('could not determine PostgreSQL migration history schema for migration lock')
	}
	return rows[0].vals[0]
}

fn migration_lock_result(rows []orm.Row) bool {
	return rows.len == 1 && rows[0].vals.len > 0 && rows[0].vals[0] in ['1', 't', 'true']
}

fn (m &Migrator) uses_transactions() bool {
	return match m.config.transaction_mode {
		.always { true }
		.never { false }
		.automatic { m.config.dialect != .mysql }
	}
}

fn (mut m Migrator) run_up(migration Migration) !AppliedMigration {
	m.ensure_history_table()!
	applied_at := time.utc().format_rfc3339()
	if m.config.dialect != .sqlite && m.uses_transactions() {
		mut tx := orm.begin(mut m.conn)!
		mut ctx := new_context(tx, m.config.dialect)
		migration.up(mut ctx) or {
			migration_err := err
			tx.rollback() or {
				return error('migration ${migration.version} `${migration.name}` failed: ${migration_err.msg()}; rollback failed: ${err.msg()}')
			}
			return error('migration ${migration.version} `${migration.name}` failed: ${migration_err.msg()}')
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
		migration.up(mut ctx)!
		ctx.execute(m.history_insert_sql(migration, applied_at))!
	}
	return AppliedMigration{
		version:    migration.version
		name:       migration.name
		applied_at: applied_at
	}
}

fn (mut m Migrator) run_down(migration Migration) ! {
	if m.config.dialect != .sqlite && m.uses_transactions() {
		mut tx := orm.begin(mut m.conn)!
		mut ctx := new_context(tx, m.config.dialect)
		migration.down(mut ctx) or {
			migration_err := err
			tx.rollback() or {
				return error('rollback of migration ${migration.version} `${migration.name}` failed: ${migration_err.msg()}; transaction rollback failed: ${err.msg()}')
			}
			return error('rollback of migration ${migration.version} `${migration.name}` failed: ${migration_err.msg()}')
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
		migration.down(mut ctx)!
		ctx.execute(m.history_delete_sql(migration.version))!
	}
}

fn (m &Migrator) history_insert_sql(migration Migration, applied_at string) string {
	table := m.history_table_sql()
	name := string_literal_sql(m.config.dialect, migration.name)
	timestamp := string_literal_sql(m.config.dialect, applied_at)
	return 'INSERT INTO ${table} (version, name, applied_at) VALUES (${migration.version}, ${name}, ${timestamp});'
}

fn (m &Migrator) history_delete_sql(version i64) string {
	table := m.history_table_sql()
	return 'DELETE FROM ${table} WHERE version = ${version};'
}
