// vtest build: present_sqlite3?
module main

import db.sqlite

struct DbValue[T] {
	id   int @[primary; sql: serial]
	data T
}

pub fn (row DbValue[T]) str() string {
	return 'DbValue(id=${row.id})'
}

struct User {
	id   int @[primary; sql: serial]
	name string
}

fn run_generic_orm_query[T](value T) ![]DbValue[T] {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}

	sql db {
		create table DbValue[T]
	}!

	row := DbValue[T]{
		data: value
	}

	sql db {
		insert row into DbValue[T]
	}!

	rows := sql db {
		select from DbValue[T]
	}!
	println(rows)

	return rows
}

fn run_generic_struct_orm_query[T](mut db sqlite.DB, value T) ![]DbValue[T] {
	row := DbValue[T]{
		data: value
	}

	sql db {
		insert row into DbValue[T]
	}!

	rows := sql db {
		select from DbValue[T]
	}!

	return rows
}

fn test_orm_generic_struct_select_in_generic_fn() {
	rows := run_generic_orm_query('test') or { panic(err) }
	assert rows.len == 1
	assert rows[0].data == 'test'
}

fn test_orm_generic_struct_select_with_inferred_struct_type() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}

	sql db {
		create table User
		create table DbValue[User]
	}!

	rows := run_generic_struct_orm_query(mut db, User{
		name: 'test'
	}) or { panic(err) }

	assert rows.len == 1
	assert rows[0].data.name == 'test'
}
