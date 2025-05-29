// vtest retry: 3
import db.sqlite

struct Account {
	id   int @[primary; sql: serial]
	name string
}

struct Note {
	id      int @[primary; sql: serial]
	content string
}

fn test_catch_table_is_not_created() {
	mut db := sqlite.connect(':memory:')!

	mut is_inserted := true

	account := Account{}

	sql db {
		insert account into Account
	} or { is_inserted = false }

	assert !is_inserted
}

fn test_catch_one_of_queries() {
	mut db := sqlite.connect(':memory:')!

	sql db {
		create table Account
	}!

	account := Account{}

	sql db {
		insert account into Account
	}!

	mut are_updated := true

	sql db {
		update Account set name = 'test' where id == 1
		update Note set content = 'test' where id == 1
	} or { are_updated = false }

	assert !are_updated
}

fn test_print_results() {
	mut db := sqlite.connect(':memory:')!

	sql db {
		create table Account
	}!

	account := Account{}

	sql db {
		insert account into Account
	}!

	count := sql db {
		select count from Account
	}!

	println(count)

	user := sql db {
		select from Account
	}!.first()

	println(user)

	users := sql db {
		select from Account
	}!

	println(users)

	assert true
}
