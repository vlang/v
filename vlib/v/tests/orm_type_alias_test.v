// vtest build: present_sqlite3? && !sanitize-memory-clang
module main

import db.sqlite

type CountryCode = string

@[table: 'addresses']
pub struct Address {
	id      int @[primary; sql: serial]
	name    string
	address string
	country CountryCode
}

fn test_main() {
	mut db := sqlite.connect(':memory:')!

	sql db {
		create table Address
	}!

	new_address := Address{
		name:    'Myself'
		address: 'Here and there'
		country: 'fr'
	}

	sql db {
		insert new_address into Address
	}!

	rows := sql db {
		select from Address where country == 'fr'
	}!

	assert rows.len == 1

	assert typeof(rows.first().country).name == 'CountryCode'

	db.close()!
}

fn main() {
	assert true
}
