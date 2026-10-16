// migration_test.v - versioned schema changes apply in order and revert cleanly.
//
// Migrations are the answer to "create table" happening twice: every change
// carries a version, the journal records what ran, and `up`/`down` move the
// database between states. There is no database server here — a fake
// connection records the statements and answers the journal reads, which
// also proves the runner only needs `execute` and works against any driver.
module migration

import orm

// FakeDB is an in-memory `execute` connection: it records statements and
// serves the journal reads from an applied set.
struct FakeDB {
mut:
	log     []string
	applied map[string]bool
	tables  map[string]bool
}

fn (mut db FakeDB) execute(query string) ![]orm.Row {
	db.log << query
	if query.starts_with('SELECT') {
		mut rows := []orm.Row{}
		mut versions := db.applied.keys()
		versions.sort()
		for v in versions {
			rows << orm.Row{
				names: ['version']
				vals:  [v]
			}
		}
		return rows
	}
	if query.starts_with('INSERT') {
		version := quoted_value(query) or { return []orm.Row{} }
		db.applied[version] = true
		return []orm.Row{}
	}
	if query.starts_with('DELETE') {
		version := quoted_value(query) or { return []orm.Row{} }
		db.applied.delete(version)
		return []orm.Row{}
	}
	if query.starts_with('CREATE TABLE users') {
		db.tables['users'] = true
		return []orm.Row{}
	}
	if query.starts_with('DROP TABLE users') {
		db.tables.delete('users')
		return []orm.Row{}
	}
	return []orm.Row{}
}

// quoted_value extracts the single-quoted value from a journal statement the
// runner built.
fn quoted_value(query string) ?string {
	start := query.index("'") or { return none }
	rest := query[start + 1..]
	finish := rest.index("'") or { return none }
	return rest[..finish]
}

fn first_migration() Migration[FakeDB] {
	return Migration[FakeDB]{
		version: '20241015120000_create_users'
		up:      fn (mut db FakeDB) ! {
			db.execute('CREATE TABLE users (id INTEGER PRIMARY KEY)')!
		}
		down:    fn (mut db FakeDB) ! {
			db.execute('DROP TABLE users')!
		}
	}
}

fn second_migration() Migration[FakeDB] {
	return Migration[FakeDB]{
		version: '20241015130000_add_index'
		up:      fn (mut db FakeDB) ! {
			db.execute('CREATE INDEX idx ON users (id)')!
		}
		down:    fn (mut db FakeDB) ! {
			db.execute('DROP INDEX idx')!
		}
	}
}

fn test_up_applies_pending_migrations_in_order() {
	mut db := FakeDB{}
	ran := up(mut db, [second_migration(), first_migration()])!
	// Passed newest-first; the journal order proves oldest ran first.
	assert ran == 2
	assert db.tables['users'] == true
	assert db.applied['20241015120000_create_users'] == true
	assert db.applied['20241015130000_add_index'] == true
	creates := db.log.filter(it.starts_with('CREATE TABLE users'))
	inserts := db.log.filter(it.starts_with('INSERT'))
	assert creates.len == 1
	assert inserts == [
		"INSERT INTO schema_migrations (version) VALUES ('20241015120000_create_users')",
		"INSERT INTO schema_migrations (version) VALUES ('20241015130000_add_index')",
	]
}

fn test_up_skips_what_the_journal_holds() {
	mut db := FakeDB{}
	assert up(mut db, [first_migration()])! == 1
	// Second run: current, nothing to do, and nothing re-executed.
	assert up(mut db, [first_migration()])! == 0
	assert db.log.filter(it.starts_with('CREATE TABLE users')).len == 1
}

fn test_down_reverts_newest_first_and_forgets() {
	mut db := FakeDB{}
	assert up(mut db, [first_migration(), second_migration()])! == 2
	assert down(mut db, [first_migration(), second_migration()], 1)! == 1
	assert db.applied['20241015120000_create_users'] == true
	assert '20241015130000_add_index' !in db.applied
	assert down(mut db, [first_migration(), second_migration()], 1)! == 1
	assert db.applied.len == 0
	assert db.tables['users'] == false
}

fn test_down_refuses_more_steps_than_applied() {
	mut db := FakeDB{}
	assert up(mut db, [first_migration()])! == 1
	down(mut db, [first_migration()], 2) or {
		assert err.msg().contains('only 1 applied')
		return
	}
	assert false, 'rolling back more than applied must fail'
}

fn test_duplicate_versions_are_refused() {
	mut db := FakeDB{}
	up(mut db, [first_migration(), first_migration()]) or {
		assert err.msg().contains('duplicate migration version')
		return
	}
	assert false, 'duplicate versions must fail'
}

fn test_unsafe_versions_are_refused() {
	mut db := FakeDB{}
	bad := Migration[FakeDB]{
		version: "x'; DROP TABLE users; --"
		up:      fn (mut db FakeDB) ! {}
		down:    fn (mut db FakeDB) ! {}
	}
	up(mut db, [bad]) or {
		assert err.msg().contains('invalid migration version')
		return
	}
	assert false, 'a version outside [A-Za-z0-9_] must fail'
}

fn test_status_reports_each_listed_migration() {
	mut db := FakeDB{}
	assert up(mut db, [first_migration()])! == 1
	states := status(mut db, [first_migration(), second_migration()])!
	assert states == [
		MigrationStatus{
			version: '20241015120000_create_users'
			applied: true
		},
		MigrationStatus{
			version: '20241015130000_add_index'
			applied: false
		},
	]
}
