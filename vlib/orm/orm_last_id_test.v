// vtest retry: 3
import db.sqlite

struct User {
	id   int @[primary; sql: serial]
	name string
}

fn test_last_id() {
	mut db := sqlite.connect(':memory:') or { panic(err) }

	sql db {
		create table User
	}!

	first_user := User{
		name: 'first'
	}

	second_user := User{
		name: 'second'
	}

	sql db {
		insert first_user into User
		insert second_user into User
	}!

	last_id := db.last_id()

	assert typeof(last_id).name == 'int'
	assert last_id > 0
}
