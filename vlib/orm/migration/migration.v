module migration

// journal_table is the only table the runner manages itself. It holds one
// row per applied migration; anything else in the database belongs to the
// migrations.
const journal_table = 'schema_migrations'

// Migration is one versioned schema change: `up` applies it, `down` reverts
// it, and `version` orders it. Versions sort as strings, so stamp them
// `YYYYMMDDHHMMSS_name`, e.g. `20241015120000_create_users`; anything outside
// `[A-Za-z0-9_]` is rejected rather than interpolated into SQL.
//
// A migration file conventionally lives at `db/migrations/<version>.v` and
// returns its struct from a `migration()` function over the app's concrete
// database type, so `up` bodies can use the full `sql db {}` DSL:
//
// ```v ignore
// import db.sqlite
// import orm.migration
//
// pub fn migration() migration.Migration[sqlite.DB] {
//     return migration.Migration[sqlite.DB]{
//         version: '20241015120000_create_users'
//         up:      fn (mut db sqlite.DB) ! {
//             sql db {
//                 create table User
//             }!
//         }
//         down:    fn (mut db sqlite.DB) ! {
//             sql db {
//                 drop table User
//             }!
//         }
//     }
// }
// ```
pub struct Migration[T] {
	version string          @[required]
	up      fn (mut db T) ! @[required]
	down    fn (mut db T) ! @[required]
}

// MigrationStatus pairs one listed migration with whether the journal holds it.
pub struct MigrationStatus {
	version string
	applied bool
}

// valid_version reports whether a version string is safe to interpolate: the
// journal statements are built, not bound, so anything outside alphanumerics
// and underscore is refused.
fn valid_version(version string) bool {
	if version == '' {
		return false
	}
	for c in version.bytes() {
		if !(c == `_` || (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`)
			|| (c >= `0` && c <= `9`)) {
			return false
		}
	}
	return true
}

// ensure_journal creates the journal table when it does not exist yet.
fn ensure_journal[T](mut db T) ! {
	db.execute('CREATE TABLE IF NOT EXISTS ${journal_table} (version TEXT PRIMARY KEY)')!
}

// applied_versions returns the versions the journal holds, oldest first.
fn applied_versions[T](mut db T) ![]string {
	rows := db.execute('SELECT version FROM ${journal_table} ORDER BY version')!
	mut versions := []string{cap: rows.len}
	for row in rows {
		if row.vals.len > 0 {
			versions << row.vals[0]
		}
	}
	return versions
}

// record marks `version` applied in the journal.
fn record_applied[T](mut db T, version string) ! {
	db.execute("INSERT INTO ${journal_table} (version) VALUES ('${version}')")!
}

// record_reverted removes `version` from the journal.
fn record_reverted[T](mut db T, version string) ! {
	db.execute("DELETE FROM ${journal_table} WHERE version = '${version}'")!
}

// sorted_migrations orders a copy of the list by version without touching the
// caller's slice, and refuses duplicate versions: two migrations cannot share
// one journal row.
fn sorted_migrations[T](migrations []Migration[T]) ![]Migration[T] {
	mut ordered := migrations.clone()
	ordered.sort(a.version < b.version)
	for i in 1 .. ordered.len {
		if ordered[i].version == ordered[i - 1].version {
			return error('duplicate migration version "${ordered[i].version}"')
		}
	}
	for m in ordered {
		if !valid_version(m.version) {
			return error('invalid migration version "${m.version}": use letters, digits and underscore')
		}
	}
	return ordered
}

// up applies every listed migration the journal does not hold yet, oldest
// first, recording each one, and returns how many ran. An empty result means
// the database is current, not that anything failed.
pub fn up[T](mut db T, migrations []Migration[T]) !int {
	ordered := sorted_migrations(migrations)!
	ensure_journal(mut db)!
	applied := applied_versions(mut db)!
	mut ran := 0
	for m in ordered {
		if m.version in applied {
			continue
		}
		m.up(mut db)!
		record_applied(mut db, m.version)!
		ran++
	}
	return ran
}

// down reverts the `steps` most recently applied listed migrations, newest
// first, forgetting each one from the journal. Applied versions missing from
// the list are left alone: the runner only reverts what it can see.
pub fn down[T](mut db T, migrations []Migration[T], steps int) !int {
	if steps <= 0 {
		return error('steps must be positive, got ${steps}')
	}
	ordered := sorted_migrations(migrations)!
	ensure_journal(mut db)!
	applied := applied_versions(mut db)!
	mut targets := []Migration[T]{}
	for i := ordered.len - 1; i >= 0; i-- {
		if ordered[i].version in applied {
			targets << ordered[i]
			if targets.len == steps {
				break
			}
		}
	}
	if targets.len < steps {
		return error('cannot roll back ${steps} migrations: only ${targets.len} applied')
	}
	for t in targets {
		t.down(mut db)!
		record_reverted(mut db, t.version)!
	}
	return targets.len
}

// status pairs every listed migration, in list order, with whether the
// journal holds it.
pub fn status[T](mut db T, migrations []Migration[T]) ![]MigrationStatus {
	ensure_journal(mut db)!
	applied := applied_versions(mut db)!
	mut out := []MigrationStatus{cap: migrations.len}
	for m in migrations {
		out << MigrationStatus{
			version: m.version
			applied: m.version in applied
		}
	}
	return out
}
