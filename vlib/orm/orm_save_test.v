// vtest retry: 3
// vtest build: present_sqlite3?
import db.sqlite
import orm
import time

@[table: 'save_users']
struct SaveUser {
mut:
	id         int @[primary; sql: serial]
	name       string
	age        int
	nickname   ?string
	updated_at time.Time
}

fn test_save_updates_all_mapped_fields_using_primary_key() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table SaveUser
	}!

	first := SaveUser{
		name:       'Alice'
		age:        30
		nickname:   'ally'
		updated_at: time.unix(100)
	}
	second := SaveUser{
		name:       'Bob'
		age:        28
		nickname:   'b'
		updated_at: time.unix(150)
	}

	sql db {
		insert first into SaveUser
		insert second into SaveUser
	}!

	mut selected := sql db {
		select from SaveUser where name == 'Alice'
	}!
	assert selected.len == 1

	mut updated := selected[0]
	updated.name = 'Alice Updated'
	updated.age = 31
	updated.nickname = none
	updated.updated_at = time.unix(200)

	orm.save(db, updated)!

	selected = sql db {
		select from SaveUser where id == updated.id
	}!
	assert selected.len == 1
	assert selected[0].id == updated.id
	assert selected[0].name == 'Alice Updated'
	assert selected[0].age == 31
	assert selected[0].nickname == none
	assert selected[0].updated_at == time.unix(200)

	untouched := sql db {
		select from SaveUser where name == 'Bob'
	}!
	assert untouched.len == 1
	assert untouched[0].age == 28
	assert untouched[0].nickname or { '' } == 'b'
}
