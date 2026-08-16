// vtest build: present_sqlite3? && !sanitize-memory-clang
import db.sqlite
import v3.migrations

struct MigrationApiUser {
	id   int @[primary; sql: serial]
	name string
}

fn create_migration_api_users(mut ctx migrations.Context) ! {
	migrations.create_orm_table[MigrationApiUser](mut ctx)!
}

fn drop_migration_api_users(mut ctx migrations.Context) ! {
	migrations.drop_orm_table[MigrationApiUser](mut ctx)!
}

fn test_v3_migrations_public_api_import_and_orm_helpers() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	mut runner := migrations.new(mut db, [
		migrations.Migration{
			version: 202608160100
			name:    "create_migration_api_users'quoted"
			up:      create_migration_api_users
			down:    drop_migration_api_users
		},
	], migrations.Config{
		dialect: .sqlite
	})!

	runner.migrate()!
	assert runner.applied()![0].name == "create_migration_api_users'quoted"
	assert db.q_int("SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'migrationapiuser';")! == 1
	runner.rollback_last()!
	assert db.q_int("SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'migrationapiuser';")! == 0
}
