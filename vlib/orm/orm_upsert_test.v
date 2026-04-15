// vtest retry: 3
// vtest build: present_sqlite3?
import db.sqlite

@[table: 'upsert_users']
struct UpsertUser {
mut:
	id       int    @[primary; sql: serial]
	username string @[unique]
	age      int
}

@[table: 'upsert_configs']
@[unique_key: 'scope, key']
struct UpsertConfig {
mut:
	id    int @[primary; sql: serial]
	scope string
	key   string
	value string
}

@[table: 'ambiguous_upsert_users']
struct AmbiguousUpsertUser {
mut:
	id       int    @[primary]
	username string @[unique]
	note     string
}

fn test_upsert_updates_existing_row_using_unique_field() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table UpsertUser
	}!

	first := UpsertUser{
		username: 'alice'
		age:      30
	}
	second := UpsertUser{
		username: 'alice'
		age:      31
	}

	sql db {
		upsert first into UpsertUser
		upsert second into UpsertUser
	}!

	rows := sql db {
		select from UpsertUser where username == 'alice'
	}!

	assert rows.len == 1
	assert rows[0].username == 'alice'
	assert rows[0].age == 31
	assert rows[0].id == 1
}

fn test_upsert_updates_existing_row_using_composite_unique_key() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table UpsertConfig
	}!

	first := UpsertConfig{
		scope: 'web'
		key:   'theme'
		value: 'light'
	}
	second := UpsertConfig{
		scope: 'web'
		key:   'theme'
		value: 'dark'
	}

	sql db {
		upsert first into UpsertConfig
		upsert second into UpsertConfig
	}!

	rows := sql db {
		select from UpsertConfig where scope == 'web' && key == 'theme'
	}!

	assert rows.len == 1
	assert rows[0].value == 'dark'
	assert rows[0].id == 1
}

fn test_upsert_errors_when_conflict_groups_match_multiple_rows() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table AmbiguousUpsertUser
	}!

	first := AmbiguousUpsertUser{
		id:       1
		username: 'alice'
		note:     'first'
	}
	second := AmbiguousUpsertUser{
		id:       2
		username: 'bob'
		note:     'second'
	}

	sql db {
		insert first into AmbiguousUpsertUser
		insert second into AmbiguousUpsertUser
	}!

	ambiguous := AmbiguousUpsertUser{
		id:       1
		username: 'bob'
		note:     'updated'
	}

	mut got_error := false
	sql db {
		upsert ambiguous into AmbiguousUpsertUser
	} or { got_error = true }

	assert got_error

	rows := sql db {
		select from AmbiguousUpsertUser order by id
	}!
	assert rows.len == 2
	assert rows[0].note == 'first'
	assert rows[1].note == 'second'
}
