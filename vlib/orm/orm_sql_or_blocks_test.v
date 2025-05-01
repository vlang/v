// vtest retry: 3
import os
import db.sqlite

struct User {
	id   i64    @[primary; sql: serial]
	name string @[unique]
}

const db_folder = os.join_path(os.vtmp_dir(), 'orm_sql')
const db_path = os.join_path(db_folder, 'sql_statement_or_blocks.db')

fn testsuite_begin() {
	os.mkdir_all(db_folder) or {}
}

fn testsuite_end() {
	os.rmdir_all(db_folder) or {}
}

fn test_ensure_db_exists_and_user_table_is_ok() {
	mut db := sqlite.connect(db_path)!
	assert true

	eprintln('> drop pre-existing User table...')
	db.exec('drop table if exists User')!

	eprintln('> creating User table...')
	sql db {
		create table User
	} or { panic(err) }
	assert true
	db.close()!
}

fn test_sql_or_block_for_insert() {
	mut db := sqlite.connect(db_path)!
	user := User{1, 'bilbo'}

	eprintln('> inserting user 1 (first try)...')
	mut is_user_inserted := true

	sql db {
		insert user into User
	} or {
		println('user should have been inserted, but could not, err: ${err}')
		is_user_inserted = false
	}

	assert is_user_inserted

	eprintln('> inserting user 1 (second try)...')
	sql db {
		insert user into User
	} or {
		println('user could not be inserted, err: ${err}')
		is_user_inserted = false
	}

	assert !is_user_inserted
	eprintln('LINE: ${@LINE}')
	db.close()!
}

fn test_sql_or_block_for_select() {
	mut db := sqlite.connect(db_path)!

	eprintln('> selecting user with id 1...')
	mut users := sql db {
		select from User where id == 1
	} or {
		eprintln('could not select user, err: ${err}')
		[]User{}
	}
	eprintln('LINE: ${@LINE}')

	single := users.first()
	assert single.id == 1

	users = sql db {
		select from User where id == 0
	} or {
		eprintln('could not select user, err: ${err}')
		[]User{}
	}
	eprintln('LINE: ${@LINE}')

	assert users.len == 0
	eprintln('LINE: ${@LINE}')

	eprintln('> selecting users...')
	multiple := sql db {
		select from User
	} or {
		eprintln('could not users, err: ${err}')
		[]User{}
	}
	eprintln('LINE: ${@LINE}')

	assert multiple.len == 1
	eprintln('LINE: ${@LINE}')
	db.close()!
}

fn test_finish() {
	eprintln('done')
	assert true
}
