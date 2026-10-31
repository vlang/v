// migration_public_test.v - the migration runner is usable from another module.
//
// `status()` returns `[]migration.MigrationStatus`; a consumer importing
// `orm.migration` must be able to read each entry's `version` and `applied`
// fields, and to build the `[]migration.Migration` list `status()` takes.
// This test lives in a different module on purpose: tests inside
// `module migration` bypass field visibility, so only a cross-module call
// proves the fields are public. It fails to compile while they are private.
module migration_public

import orm
import orm.migration

// PublicFakeDB is a minimal in-memory `execute` connection: it records
// statements and serves the journal reads from an applied set.
struct PublicFakeDB {
mut:
	log     []string
	applied map[string]bool
}

fn (mut db PublicFakeDB) execute(query string) ![]orm.Row {
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

fn public_migration(version string) migration.Migration[PublicFakeDB] {
	return migration.Migration[PublicFakeDB]{
		version: version
		up:      fn (mut db PublicFakeDB) ! {}
		down:    fn (mut db PublicFakeDB) ! {}
	}
}

fn test_status_fields_are_readable_from_another_module() {
	mut db := PublicFakeDB{}
	ran := migration.up(mut db, [public_migration('20241015120000_create_users')])!
	assert ran == 1
	states := migration.status(mut db, [
		public_migration('20241015120000_create_users'),
		public_migration('20241015130000_add_index'),
	])!
	assert states.len == 2
	assert states[0].version == '20241015120000_create_users'
	assert states[0].applied == true
	assert states[1].version == '20241015130000_add_index'
	assert states[1].applied == false
}
