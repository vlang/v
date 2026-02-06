// vtest retry: 3
import db.sqlite

struct User {
	id   int @[primary; sql: serial]
	name string
	age  int
}

fn get_acceptable_age() int {
	return 21
}

fn test_fn_calls() {
	mut db := sqlite.connect(':memory:') or { panic(err) }

	sql db {
		create table User
	}!

	first_user := User{
		name: 'first'
		age:  25
	}

	second_user := User{
		name: 'second'
		age:  14
	}

	sql db {
		insert first_user into User
		insert second_user into User
	}!

	users_with_acceptable_age := sql db {
		select from User where age >= get_acceptable_age()
	}!

	assert users_with_acceptable_age.len == 1
	assert users_with_acceptable_age.first().name == 'first'

	users_with_non_acceptable_age := sql db {
		select from User where age < get_acceptable_age()
	}!

	assert users_with_non_acceptable_age.len == 1
	assert users_with_non_acceptable_age.first().name == 'second'
}
