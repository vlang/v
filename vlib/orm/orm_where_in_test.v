// vtest flaky: true
// vtest retry: 3
import db.sqlite

struct User {
	id   int @[primary; sql: serial]
	name string
}

fn get_users_in(mut db sqlite.DB, names []string) ![]User {
	return sql db {
		select from User where name in names
	}!
}

fn get_users_not_in(mut db sqlite.DB, names []string) ![]User {
	return sql db {
		select from User where name !in names
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

	in_users := get_users_in(mut db, ['first'])!

	assert in_users.len == 1

	not_in_users := get_users_not_in(mut db, ['second'])!

	assert not_in_users.len == 1

	all_users := get_users_in(mut db, ['first', 'second'])!
	assert all_users.len == 2
}
