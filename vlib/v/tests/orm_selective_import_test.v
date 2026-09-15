// vtest build: present_sqlite3? && !sanitize-memory-clang
module main

import db.sqlite
import v.tests.orm_selective_import_entities { Remote }

fn test_orm_resolves_selectively_imported_table_type() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}

	sql db {
		create table Remote
	}!
	row := Remote{
		name: 'selected import'
	}
	sql db {
		insert row into Remote
	}!

	rows := sql db {
		select from Remote
	}!
	assert rows.len == 1
	assert rows[0].name == 'selected import'

	sql db {
		drop table Remote
	}!
	sql db {
		create table Remote
	}!
	tables := db.exec("select name from sqlite_master where type = 'table' and name = 'remote'")!
	assert tables.len == 1
}
