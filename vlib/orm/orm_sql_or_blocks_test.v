import os
import sqlite

struct User {
	id   i64    [primary; sql: serial]
	name string [unique]
}

const db_path = os.join_path(os.temp_dir(), 'sql_statement_or_blocks.db')

fn test_ensure_db_exists_and_user_table_is_ok() ? {
	db := sqlite.connect(db_path)?
	assert true

	eprintln('> drop pre-existing User table...')
	db.exec('drop table if exists User')

	eprintln('> creating User table...')
	sql db {
		create table User
	} or { panic(err) }
	assert true
}

fn test_sql_or_block_for_insert() ? {
	db := sqlite.connect(db_path)?
	user := User{1, 'bilbo'}

	eprintln('> inserting user 1 (first try)...')
	sql db {
		insert user into User
	} or {
		println('user should have been inserted, but could not, err: $err')
		assert false
	}

	eprintln('> inserting user 1 (second try)...')
	sql db {
		insert user into User
	} or {
		assert true
		println('user could not be inserted, err: $err')
	}
}

fn test_sql_or_block_for_select() ? {
	db := sqlite.connect(db_path)?

	eprintln('> selecting user with id 1...')
	single := sql db {
		select from User where id == 1
	} or {
		eprintln('could not select user, err: $err')
		User{0, ''}
	}

	assert single.id == 1

	failed := sql db {
		select from User where id == 0
	} or {
		eprintln('could not select user, err: $err')
		User{0, ''}
	}

	assert failed.id == 0
	assert failed.name == ''

	eprintln('> selecting users...')
	multiple := sql db {
		select from User
	} or {
		eprintln('could not users, err: $err')
		[]User{}
	}

	assert multiple.len == 1
}

fn test_finish() ? {
	eprintln('done')
	assert true
}
