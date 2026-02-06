// vtest retry: 3
import db.sqlite

struct User {
	id   int @[primary; sql: serial]
	name string
}

fn get_users(mut db sqlite.DB) ![]User {
	return sql db {
		select from User
	}!
}

fn test_orm_mut_db() {
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

	users := get_users(mut db)!

	assert users.len == 2
}
