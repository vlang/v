// vtest build: !windows
import db.sqlite

@[table: 'Users']
struct User {
	id   int @[primary; sql: serial]
	name string
}

const const_users_offset = 1
const const_users_offset2 = 1

fn test_orm() {
	mut db := sqlite.connect(':memory:') or { panic(err) }

	upper_1 := User{
		name: 'Test'
	}

	upper_2 := User{
		name: 'Test2'
	}

	sql db {
		create table User
		insert upper_1 into User
		insert upper_2 into User
	} or { panic(err) }

	result := sql db {
		select from User limit const_users_offset2
	} or { panic(err) }

	assert result[0].name == 'Test'
	db.close()!
}
