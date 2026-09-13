// vtest build: present_sqlite3?
// Regression test: the ORM object variable (`insert x into T`, `upsert x into T`,
// bulk `insert xs into T`) must be escaped like any other C identifier. Otherwise a
// variable named after a C/C++ reserved word (`new`, `delete`, `operator`, ...) is
// declared as `__v_delete` but referenced by the ORM codegen as `delete`, producing
// an "undeclared identifier" C error. See write_orm_insert_with_last_ids /
// write_orm_upsert / write_orm_bulk_insert in vlib/v/gen/c/orm.v.
import db.sqlite

struct User {
	id   int @[primary; sql: serial]
	name string
}

fn test_insert_with_reserved_keyword_object_var() {
	mut db := sqlite.connect(':memory:')!
	sql db {
		create table User
	}!
	// `delete` is a reserved word; used here as the inserted object variable.
	delete := User{
		name: 'single'
	}
	sql db {
		insert delete into User
	}!
	rows := sql db {
		select from User
	}!
	assert rows.len == 1
	assert rows[0].name == 'single'
}

fn test_bulk_insert_with_reserved_keyword_object_var() {
	mut db := sqlite.connect(':memory:')!
	sql db {
		create table User
	}!
	// `new` is a reserved word; used here as the inserted array variable.
	new := [User{
		name: 'a'
	}, User{
		name: 'b'
	}]
	sql db {
		insert new into User
	}!
	rows := sql db {
		select from User
	}!
	assert rows.len == 2
	assert rows[0].name == 'a'
	assert rows[1].name == 'b'
}
