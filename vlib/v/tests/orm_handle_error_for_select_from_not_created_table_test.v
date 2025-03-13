// vtest build: present_sqlite3?
import db.sqlite

struct User {
	id   int
	name string
}

fn test_or_block_error_handling_of_an_invalid_query() {
	db := sqlite.connect(':memory:') or { panic(err) }

	users := sql db {
		select from User
	} or { []User{} }

	println(users)
	assert true
}
