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
