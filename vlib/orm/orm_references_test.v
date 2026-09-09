// vtest retry: 3
import db.sqlite

struct Boat {
	id          int @[primary; sql: serial]
	color_id    int @[references]
	another1_id int @[references: 'size']
	another2_id int @[references: 'size(secondary_id)']
}

struct Color {
	id  int @[primary; sql: serial]
	hex string
}

struct Size {
	id           int @[primary; sql: serial]
	secondary_id int
}

fn test_references_constraint() {
	db := sqlite.connect(':memory:') or { panic(err) }

	sql db {
		create table Boat
		create table Color
		create table Size
	} or { panic(err) }

	// this pragma returns a row for each foreign key constraint on a table
	pragma_result := db.exec('pragma foreign_key_list(boat)') or { panic('nope') }

	assert pragma_result.len == 3
}

struct Member {
	id   int @[primary; sql: serial]
	name string
}

struct Team {
	id        int @[primary; sql: serial]
	name      string
	member_id int @[references: 'Member(id)']
}

fn test_omitted_references_field_inserts_null() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	db.exec('PRAGMA foreign_keys=ON')!

	sql db {
		create table Member
		create table Team
	}!

	team := Team{
		name: 'unassigned'
	}

	sql db {
		insert team into Team
	}!

	rows := db.exec('select member_id from Team where id = 1')!
	assert rows.len == 1
	assert rows[0].vals[0] == ''

	invalid_team := Team{
		name:      'invalid'
		member_id: 0
	}

	mut failed := false
	sql db {
		insert invalid_team into Team
	} or { failed = true }
	assert failed
}
