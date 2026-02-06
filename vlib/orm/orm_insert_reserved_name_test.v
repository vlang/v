// vtest retry: 3
import db.sqlite

@[table: 'bad_table']
struct Bad {
	id   int @[primary; sql: serial]
	link string
}

fn test_insert_with_reserved_name() {
	db := sqlite.connect(':memory:') or { panic(err) }

	bad := Bad{
		link: 'test'
	}
	sql db {
		create table Bad
	}!
	sql db {
		insert bad into Bad
	}!

	sql db {
		insert bad into Bad
		insert bad into Bad
		insert bad into Bad
	}!

	rows := sql db {
		select from Bad
	}!

	assert rows.len == 4
}
