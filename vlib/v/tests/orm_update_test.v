import db.sqlite
import time

struct Person {
	name   string
	height Height
}

enum Height as u8 {
	tall
	small
}

fn test_main() {
	db := sqlite.connect(':memory:')!

	sql db {
		create table Person
	}!

	a := Person{'A', Height.small}
	b := Person{'A', Height.tall}

	sql db {
		insert a into Person
	}!

	sql db {
		insert b into Person
	}!

	new_height := Height.small
	sql db {
		update Person set height = new_height where height == Height.tall
	}!

	rows := sql db {
		select from Person where height == Height.small
	}!

	assert rows.len == 2
}

@[table: 'orm_update_users']
struct OrmUpdateUser {
pub:
	id         string @[primary]
	name       string
	updated_at time.Time @[sql_type: 'TIMESTAMP']
}

struct OrmUpdateReq {
	updated_at ?time.Time
}

fn test_orm_update_with_option_selector_or_block() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or { panic(err) }
	}

	sql db {
		create table OrmUpdateUser
	}!

	user := OrmUpdateUser{
		id:         '100'
		name:       'Alice'
		updated_at: time.unix(1_700_000_000)
	}
	sql db {
		insert user into OrmUpdateUser
	}!

	req := OrmUpdateReq{}
	expected_updated_at := time.unix(1_700_000_123)

	sql db {
		update OrmUpdateUser set updated_at = req.updated_at or { expected_updated_at } where id == '100'
	}!

	rows := sql db {
		select from OrmUpdateUser where id == '100'
	}!

	assert rows.len == 1
	assert rows[0].updated_at.unix() == expected_updated_at.unix()
}

struct OrmUpdateThing {
	id    int @[primary]
	value string
}

struct OrmUpdateCommit {
	id    int @[primary]
	tag   string
	thing OrmUpdateThing
}

fn test_orm_update_with_struct_field() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or { panic(err) }
	}

	sql db {
		create table OrmUpdateThing
		create table OrmUpdateCommit
	}!

	second := OrmUpdateThing{
		id:    2
		value: 'goodbye'
	}
	sql db {
		insert second into OrmUpdateThing
	}!

	commit := OrmUpdateCommit{
		id:    1
		tag:   'head'
		thing: OrmUpdateThing{
			id:    1
			value: 'hello'
		}
	}
	sql db {
		insert commit into OrmUpdateCommit
	}!

	sql db {
		update OrmUpdateCommit set thing = second where tag == 'head'
	}!

	rows := sql db {
		select from OrmUpdateCommit where tag == 'head'
	}!

	assert rows.len == 1
	assert rows[0].thing.id == second.id
	assert rows[0].thing.value == 'goodbye'
}
