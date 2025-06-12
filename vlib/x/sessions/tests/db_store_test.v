// vtest retry: 3
import db.sqlite
import x.json2 as json
import os
import time
import x.sessions

const max_age = time.second
const db_path = os.join_path(os.vtmp_dir(), 'x_sessions_db_test.db')

fn testsuite_begin() {
	os.rm(db_path) or {}
}

fn testsuite_end() {
	os.rm(db_path) or {}
}

pub struct User {
	name string
	age  int
}

const default_user = User{
	name: 'john'
	age:  99
}
const default_user_encoded = json.encode(default_user)

fn test_store_set() {
	mut db := get_connection()!
	defer {
		db.close() or {}
	}
	mut store := sessions.DBStore.create[User](db)!
	store.set('a', default_user)!

	mut rows := sql db {
		select from sessions.DBStoreSessions
	}!
	assert rows.len == 1

	// check if created at time is not empty
	assert rows[0].created_at != time.Time{}
	assert rows[0].data == default_user_encoded

	first_created := rows[0].created_at
	store.set('a', User{ age: 99 })!

	rows = sql db {
		select from sessions.DBStoreSessions
	}!
	assert rows.len == 1
	assert rows[0].created_at == first_created
	assert rows[0].data == json.encode(User{ age: 99 })
}

fn test_store_get() {
	mut db := get_connection()!
	defer {
		db.close() or {}
	}
	mut store := sessions.DBStore.create[User](db)!
	store.set('b', default_user)!

	if data := store.get('b', max_age) {
		assert data == default_user
	} else {
		$if !windows {
			assert false, 'session data should not be none'
		}
	}
}

fn test_store_session_expired() {
	mut db := get_connection()!
	defer {
		db.close() or {}
	}
	mut store := sessions.DBStore.create[User](db)!
	store.set('c', default_user)!

	time.sleep(2 * max_age)

	if data := store.get('c', max_age) {
		assert false, 'session should be expired!'
	} else {
		assert err.msg() == 'session is expired'
	}
	// verify that data is deleted
	rows := sql db {
		select from sessions.DBStoreSessions where session_id == 'c'
	}!
	assert rows.len == 0
}

fn get_connection() !sqlite.DB {
	return sqlite.connect(db_path)!
}
