// vtest build: present_h2?
import db.h2

struct H2User {
	id   int @[primary; sql: serial]
	name string
	age  int
}

fn test_h2_orm_crud() {
	mut db := h2.connect(':memory:') or { panic(err) }
	defer {
		db.close()
	}

	sql db {
		create table H2User
	}!

	alice := H2User{
		name: 'Alice'
		age:  30
	}
	sql db {
		insert alice into H2User
	}!
	assert db.last_id() == 1

	bob := H2User{
		name: 'Bob'
		age:  25
	}
	sql db {
		insert bob into H2User
	}!

	all := sql db {
		select from H2User
	}!
	assert all.len == 2
	assert all[0].name == 'Alice'
	assert all[1].name == 'Bob'

	older := sql db {
		select from H2User where age > 26 order by age
	}!
	assert older.len == 1
	assert older[0].name == 'Alice'

	sql db {
		update H2User set name = 'Alicia' where age == 30
	}!
	updated := sql db {
		select from H2User where name == 'Alicia'
	}!
	assert updated.len == 1
	assert updated[0].age == 30

	page := sql db {
		select from H2User order by age limit 1 offset 1
	}!
	assert page.len == 1
	// ORDER BY age is ascending: Bob (25) is first, Alicia (30) is second.
	assert page[0].name == 'Alicia'

	count := sql db {
		select count from H2User
	}!
	assert count == 2

	sql db {
		delete from H2User where age == 25
	}!
	remaining := sql db {
		select from H2User
	}!
	assert remaining.len == 1
	assert remaining[0].name == 'Alicia'

	sql db {
		drop table H2User
	}!
}

fn test_h2_orm_transactions() {
	mut db := h2.connect(':memory:') or { panic(err) }
	defer {
		db.close()
	}
	sql db {
		create table H2User
	}!

	tx := H2User{
		name: 'Tx'
		age:  1
	}
	after := H2User{
		name: 'After'
		age:  2
	}
	db.orm_begin()!
	sql db {
		insert tx into H2User
	}!
	db.orm_savepoint('sp1')!
	sql db {
		insert after into H2User
	}!
	db.orm_rollback_to('sp1')!
	db.orm_release_savepoint('sp1')!
	db.orm_commit()!

	rows := sql db {
		select from H2User
	}!
	assert rows.len == 1
	assert rows[0].name == 'Tx'
}
