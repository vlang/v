// vtest build: present_sqlite3?
module main

import db.sqlite

@[table: 'blog']
pub struct Blog {
	id       int @[primary; sql: serial]
	slug     string
	language string
}

fn records_by_field[T](db sqlite.DB, fieldname string, value string) ![]T {
	$for field in T.fields {
		if field.name == fieldname {
			entries := sql db {
				select from Blog where field.name == value
			} or { return err }
			return entries
		}
	}
	return error('fieldname not found')
}

fn test_main() {
	mut db := sqlite.connect(':memory:')!
	sql db {
		create table Blog
	}!

	row := Blog{
		slug:     'Test'
		language: 'v'
	}

	sql db {
		insert row into Blog
	}!
	rows := records_by_field[Blog](db, 'language', 'v') or {
		println(err)
		return
	}
	assert rows.len == 1
	db.close()!
}
