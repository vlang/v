import db.sqlite
import rand

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

struct Package {
	id     int    [primary; sql: serial]
	name   string [unique]
	author User   [fkey: 'id']
}

struct User {
pub mut:
	id       int    [primary; sql: serial]
	username string [unique]
}

struct Entity {
	uuid        string [primary]
	description string
}

struct EntityWithFloatPrimary {
	id   f64    [primary]
	name string
}

pub fn insert_parent(db sqlite.DB, mut parent Parent) ! {
	sql db {
		insert parent into Parent
	}!
}

fn test_set_primary_value() {
	// The primary key is an constraint that ensures each record in a table is unique.
	// Primary keys must contain unique values and cannot contain `NULL` values.
	// However, this statement does not imply that a value cannot be inserted by the user.
	// Therefore, let's allow this.
	db := sqlite.connect(':memory:')!

	sql db {
		create table Child
	}!

	child := Child{
		id: 10
		parent_id: 20
	}

	sql db {
		insert child into Child
	}!

	children := sql db {
		select from Child
	}!

	assert children.first() == child
}

fn test_uuid_primary_key() {
	db := sqlite.connect(':memory:')!
	uuid := rand.uuid_v4()

	sql db {
		create table Entity
	}!

	entity := Entity{
		uuid: uuid
		description: 'Test'
	}

	sql db {
		insert entity into Entity
	}!

	entities := sql db {
		select from Entity where uuid == uuid
	}!

	mut is_duplicate_inserted := true

	sql db {
		insert entity into Entity
	} or { is_duplicate_inserted = false }

	assert entities.len == 1
	assert entities.first() == entity
	assert is_duplicate_inserted == false
}

fn test_float_primary_key() {
	db := sqlite.connect(':memory:')!
	id := 3.14

	sql db {
		create table EntityWithFloatPrimary
	}!

	entity := EntityWithFloatPrimary{
		id: id
		name: 'Test'
	}

	sql db {
		insert entity into EntityWithFloatPrimary
	}!

	entities := sql db {
		select from EntityWithFloatPrimary where id == id
	}!

	assert entities.len == 1
	assert entities.first() == entity
}

fn test_does_not_insert_uninitialized_field() {
	db := sqlite.connect(':memory:')!

	sql db {
		create table User
		create table Package
	}!

	package := Package{
		name: 'xml'
		// author
	}

	sql db {
		insert package into Package
	}!

	users := sql db {
		select from User
	}!

	// users must be empty because the package doesn't have an initialized `User` structure.
	assert users.len == 0
}

fn test_insert_empty_field() {
	db := sqlite.connect(':memory:')!

	sql db {
		create table User
		create table Package
	}!

	package := Package{
		name: 'xml'
		author: User{}
	}

	sql db {
		insert package into Package
	}!

	users := sql db {
		select from User
	}!

	assert users.len == 1
}

fn test_insert_empty_object() {
	db := sqlite.connect(':memory:')!

	account := Account{}

	sql db {
		create table Account
		insert account into Account
	}!

	accounts := sql db {
		select from Account
	}!

	assert accounts.len == 1
}

fn test_orm_insert_mut_object() {
	db := sqlite.connect(':memory:')!

	sql db {
		create table Parent
		create table Child
		create table Note
	}!

	mut parent := Parent{
		name: 'test'
	}

	insert_parent(db, mut parent)!

	parents := sql db {
		select from Parent
	}!

	assert parents.len == 1
}

fn test_orm_insert_with_multiple_child_elements() {
	mut db := sqlite.connect(':memory:')!

	sql db {
		create table Parent
		create table Child
		create table Note
	}!

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
	}!

	parents := sql db {
		select from Parent where id == 1
	}!

	parent := parents.first()
	assert parent.children.len == new_parent.children.len
	assert parent.notes.len == new_parent.notes.len

	children_count := sql db {
		select count from Child
	}!
	assert children_count == new_parent.children.len

	note_count := sql db {
		select count from Note
	}!
	assert note_count == new_parent.notes.len

	assert parent.children[0].name == 'Lisa'
	assert parent.children[1].name == 'Steve'

	assert parent.notes[0].text == 'First note'
	assert parent.notes[1].text == 'Second note'
	assert parent.notes[2].text == 'Third note'
}

[table: 'customers']
struct Customer {
	id   i64    [primary; sql: serial]
	name string
}

fn test_i64_primary_field_works_with_insertions_of_id_0() {
	db := sqlite.connect(':memory:')!
	sql db {
		create table Customer
	}!
	for i in ['Bob', 'Charlie'] {
		new_customer := Customer{
			name: i
		}
		sql db {
			insert new_customer into Customer
		}!
	}
	users := sql db {
		select from Customer
	}!
	assert users.len == 2
	// println("${users}")
}
