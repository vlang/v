import db.sqlite

struct Parent {
	id       int     [primary; sql: serial]
	name     string
	children []Child [fkey: 'parent_id']
	notes    []Note  [fkey: 'owner_id']
}

struct Child {
mut:
	id        int    [primary; sql: serial]
	parent_id int
	name      string
}

struct Note {
mut:
	id       int    [primary; sql: serial]
	owner_id int
	text     string
}

struct Account {
	id int [primary; sql: serial]
}

pub fn insert_parent(db sqlite.DB, mut parent Parent) {
	sql db {
		insert parent into Parent
	}
}

fn test_insert_empty_object() {
	db := sqlite.connect(':memory:') or { panic(err) }

	account := Account{}

	sql db {
		create table Account
		insert account into Account
	}

	accounts := sql db {
		select from Account
	}

	assert accounts.len == 1
}

fn test_orm_insert_mut_object() {
	db := sqlite.connect(':memory:') or { panic(err) }

	sql db {
		create table Parent
		create table Child
		create table Note
	}

	mut parent := Parent{
		name: 'test'
	}

	insert_parent(db, mut parent)

	parents := sql db {
		select from Parent
	}

	assert parents.len == 1
}

fn test_orm_insert_with_multiple_child_elements() {
	mut db := sqlite.connect(':memory:') or { panic(err) }

	sql db {
		create table Parent
		create table Child
		create table Note
	}

	new_parent := Parent{
		name: 'test'
		children: [
			Child{
				name: 'Lisa'
			},
			Child{
				name: 'Steve'
			},
		]
		notes: [
			Note{
				text: 'First note'
			},
			Note{
				text: 'Second note'
			},
			Note{
				text: 'Third note'
			},
		]
	}

	sql db {
		insert new_parent into Parent
	}

	parent := sql db {
		select from Parent where id == 1
	}

	assert parent.children.len == new_parent.children.len
	assert parent.notes.len == new_parent.notes.len

	children_count := sql db {
		select count from Child
	}
	assert children_count == new_parent.children.len

	note_count := sql db {
		select count from Note
	}
	assert note_count == new_parent.notes.len

	assert parent.children[0].name == 'Lisa'
	assert parent.children[1].name == 'Steve'

	assert parent.notes[0].text == 'First note'
	assert parent.notes[1].text == 'Second note'
	assert parent.notes[2].text == 'Third note'
}
