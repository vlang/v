// vtest build: present_sqlite3?
module main

import db.sqlite

struct Client18333 {
	id   int @[primary; sql: serial]
	name ?string
}

fn test_orm_option_field_issue_18333() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table Client18333
	}!

	client := Client18333{
		name: 'alice'
	}

	sql db {
		insert client into Client18333
	}!

	rows := sql db {
		select from Client18333 where name == 'alice'
	}!

	assert rows.len == 1
	assert rows[0].name or { '' } == 'alice'
}
