import db.sqlite

struct OrmBulkUser {
	id   int @[primary]
	name string
	age  int
}

fn test_orm_bulk_insert_update_and_upsert() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or { panic(err) }
	}

	sql db {
		create table OrmBulkUser
	}!

	users := [
		OrmBulkUser{
			id:   1
			name: 'Alice'
			age:  25
		},
		OrmBulkUser{
			id:   2
			name: 'Bob'
			age:  30
		},
	]

	sql db {
		insert users into OrmBulkUser
	}!

	first := sql db {
		select from OrmBulkUser where id == 1
	}!
	second := sql db {
		select from OrmBulkUser where id == 2
	}!

	assert first.len == 1
	assert first[0].name == 'Alice'
	assert first[0].age == 25
	assert second.len == 1
	assert second[0].name == 'Bob'
	assert second[0].age == 30

	updates := [
		OrmBulkUser{
			id:   1
			name: 'Alicia'
			age:  26
		},
		OrmBulkUser{
			id:   2
			name: 'Robert'
			age:  31
		},
	]

	sql db {
		update OrmBulkUser set name = updates.name, age = updates.age where id == updates.id
	}!

	updated_first := sql db {
		select from OrmBulkUser where id == 1
	}!
	updated_second := sql db {
		select from OrmBulkUser where id == 2
	}!

	assert updated_first.len == 1
	assert updated_first[0].name == 'Alicia'
	assert updated_first[0].age == 26
	assert updated_second.len == 1
	assert updated_second[0].name == 'Robert'
	assert updated_second[0].age == 31

	upserts := [
		OrmBulkUser{
			id:   1
			name: 'Alice Updated'
			age:  27
		},
		OrmBulkUser{
			id:   3
			name: 'Charlie'
			age:  35
		},
	]

	sql db {
		upsert upserts into OrmBulkUser
	}!

	upserted_first := sql db {
		select from OrmBulkUser where id == 1
	}!
	upserted_second := sql db {
		select from OrmBulkUser where id == 2
	}!
	upserted_third := sql db {
		select from OrmBulkUser where id == 3
	}!

	assert upserted_first.len == 1
	assert upserted_first[0].name == 'Alice Updated'
	assert upserted_first[0].age == 27
	assert upserted_second.len == 1
	assert upserted_second[0].name == 'Robert'
	assert upserted_second[0].age == 31
	assert upserted_third.len == 1
	assert upserted_third[0].name == 'Charlie'
	assert upserted_third[0].age == 35
}
