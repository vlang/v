// vtest retry: 3
import db.sqlite

struct Parent {
	id       int     @[primary; sql: serial]
	children []Child @[fkey: 'parent_id']
	notes    []Note  @[fkey: 'owner_id']
}

struct Child {
mut:
	id        int @[primary; sql: serial]
	parent_id int
}

struct Note {
mut:
	id       int @[primary; sql: serial]
	owner_id int
}

struct Entity {
	name        string @[primary]
	description string
}

fn test_create_without_id_field() {
	db := sqlite.connect(':memory:')!

	sql db {
		create table Entity
	}!

	first := Entity{
		name:        'First'
		description: 'Such wow! No `id` field'
	}
	second := Entity{
		name:        'Second'
		description: 'Such wow! No `id` field again'
	}

	sql db {
		insert first into Entity
		insert second into Entity
	}!

	entities := sql db {
		select from Entity
	}!

	assert entities.len == 2

	first_entity := sql db {
		select from Entity where name == 'First'
	}!

	assert first_entity.first().name == 'First'

	second_entity := sql db {
		select from Entity where name == 'Second'
	}!

	assert second_entity.first().name == 'Second'
}

fn test_create_only_one_table() {
	mut db := sqlite.connect(':memory:')!

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
	mut db := sqlite.connect(':memory:')!

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
