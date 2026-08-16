module migrations

import orm
import strconv
import time

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
	conn       orm.TransactionalConnection
	migrations []Migration
	config     Config
}

// new creates and validates a migrator. It does not access the database until
// one of the migration or inspection methods is called.
pub fn new(mut conn orm.TransactionalConnection, registered []Migration, config Config) !Migrator {
	validate_identifier(config.table, 'migration history table')!
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
	reverted := m.rollback(steps)!
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
	applied := m.applied()!
	if applied.len == 0 {
		return []
	}
	return m.rollback(applied.len)
}

// applied returns the migrations recorded in the database, oldest first.
pub fn (mut m Migrator) applied() ![]AppliedMigration {
	m.ensure_history_table()!
	table := quote_identifier(m.config.dialect, m.config.table)
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
	table := quote_identifier(m.config.dialect, m.config.table)
	m.conn.execute('CREATE TABLE IF NOT EXISTS ${table} (version BIGINT PRIMARY KEY, name VARCHAR(255) NOT NULL, applied_at VARCHAR(32) NOT NULL);')!
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
	if m.uses_transactions() {
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
	if m.uses_transactions() {
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
	table := quote_identifier(m.config.dialect, m.config.table)
	return "INSERT INTO ${table} (version, name, applied_at) VALUES (${migration.version}, '${escape_literal(migration.name)}', '${escape_literal(applied_at)}');"
}

fn (m &Migrator) history_delete_sql(version i64) string {
	table := quote_identifier(m.config.dialect, m.config.table)
	return 'DELETE FROM ${table} WHERE version = ${version};'
}
