import db.sqlite

struct OrmBulkUser {
	id   int @[primary]
	name string
	age  int
}

struct OrmBulkDefaultRow {
	id int @[primary; sql: serial]
}

struct OrmBulkRenamedUser {
	id           int    @[primary]
	display_name string @[sql: 'display_name_text']
}

fn test_orm_bulk_insert_and_update() {
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
}

fn test_orm_bulk_insert_preserves_all_default_rows() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or { panic(err) }
	}

	sql db {
		create table OrmBulkDefaultRow
	}!

	rows := [OrmBulkDefaultRow{}, OrmBulkDefaultRow{}, OrmBulkDefaultRow{}]

	sql db {
		insert rows into OrmBulkDefaultRow
	}!

	inserted := sql db {
		select from OrmBulkDefaultRow order by id
	}!

	assert inserted.len == 3
	assert inserted[0].id == 1
	assert inserted[1].id == 2
	assert inserted[2].id == 3
}

fn test_orm_bulk_insert_with_mixed_serial_values_keeps_defaults() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or { panic(err) }
	}

	sql db {
		create table OrmBulkDefaultRow
	}!

	rows := [
		OrmBulkDefaultRow{},
		OrmBulkDefaultRow{
			id: 5
		},
	]

	sql db {
		insert rows into OrmBulkDefaultRow
	}!

	inserted := sql db {
		select from OrmBulkDefaultRow order by id
	}!

	assert inserted.len == 2
	assert inserted[0].id == 1
	assert inserted[1].id == 5
}

fn test_orm_bulk_update_with_renamed_column() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or { panic(err) }
	}

	sql db {
		create table OrmBulkRenamedUser
	}!

	users := [
		OrmBulkRenamedUser{
			id:           1
			display_name: 'Alice'
		},
		OrmBulkRenamedUser{
			id:           2
			display_name: 'Bob'
		},
	]

	sql db {
		insert users into OrmBulkRenamedUser
	}!

	updates := [
		OrmBulkRenamedUser{
			id:           1
			display_name: 'Alicia'
		},
		OrmBulkRenamedUser{
			id:           2
			display_name: 'Robert'
		},
	]

	sql db {
		update OrmBulkRenamedUser set display_name = updates.display_name where id == updates.id
	}!

	updated := sql db {
		select from OrmBulkRenamedUser order by id
	}!

	assert updated.len == 2
	assert updated[0].display_name == 'Alicia'
	assert updated[1].display_name == 'Robert'
}
