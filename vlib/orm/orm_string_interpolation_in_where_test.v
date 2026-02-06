// vtest retry: 3
import db.sqlite

struct User {
	id   int @[primary; sql: serial]
	name string
}

fn test_string_interpolation() {
	mut db := sqlite.connect(':memory:') or { panic(err) }

	sql db {
		create table User
	}!

	user_suffix := '_user'

	first_user := User{
		name: 'first${user_suffix}'
	}

	second_user := User{
		name: 'second${user_suffix}'
	}

	sql db {
		insert first_user into User
		insert second_user into User
	}!

	users := sql db {
		select from User where name == 'first${user_suffix}'
	}!

	assert users.len == 1
	assert users.first().name == 'first${user_suffix}'
}
