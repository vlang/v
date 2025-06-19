// vtest retry: 3
import db.sqlite
import orm

struct User {
	id   int @[primary; sql: serial]
	name string
}

fn test_orm_interface() {
	sqlite_db := sqlite.connect(':memory:') or { panic(err) }
	db := orm.Connection(sqlite_db)

	sql db {
		create table User
	}!

	user := User{
		name: 'test'
	}

	sql db {
		insert user into User
	}!

	users := sql db {
		select from User
	}!

	assert users.len == 1
	assert users.first().name == user.name
}
