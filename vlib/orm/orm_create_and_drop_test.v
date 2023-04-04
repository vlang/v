import db.sqlite

struct Parent {
	id       int     [primary; sql: serial]
	children []Child [fkey: 'parent_id']
	notes    []Note  [fkey: 'owner_id']
}

struct Child {
mut:
	id        int [primary; sql: serial]
	parent_id int
}

struct Note {
mut:
	id       int [primary; sql: serial]
	owner_id int
}

fn test_create_only_one_table() {
	mut db := sqlite.connect(':memory:') or { panic(err) }

	sql db {
		create table Parent
	}!

	mut is_child_created := true
	mut is_note_created := true

	_ := sql db {
		select count from Child
	} or {
		is_child_created = false
		0
	}

	_ := sql db {
		select count from Note
	} or {
		is_note_created = false
		0
	}

	assert is_child_created == false
	assert is_note_created == false
}

fn test_drop_only_one_table() {
	mut db := sqlite.connect(':memory:') or { panic(err) }

	sql db {
		create table Parent
	}!
	sql db {
		create table Child
	}!
	sql db {
		create table Note
	}!

	mut is_parent_dropped := false
	mut is_child_dropped := false
	mut is_note_dropped := false

	sql db {
		drop table Parent
	}!

	_ := sql db {
		select count from Parent
	} or {
		is_parent_dropped = true
		0
	}

	_ := sql db {
		select count from Child
	} or {
		is_child_dropped = true
		0
	}

	_ := sql db {
		select count from Note
	} or {
		is_note_dropped = true
		0
	}

	assert is_parent_dropped
	assert is_child_dropped == false
	assert is_note_dropped == false
}
