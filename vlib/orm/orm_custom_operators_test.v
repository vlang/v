// vtest retry: 3
import db.sqlite

struct User {
	id      int @[primary; sql: serial]
	name    string
	country string
}

// like is an example function for checking that V allows using the `like` keyword as an identifier.
fn like() {}

fn test_like_operator() {
	like()
	db := sqlite.connect(':memory:')!

	sql db {
		create table User
	}!

	luke := User{
		name:    'Luke'
		country: 'US'
	}
	sql db {
		insert luke into User
	}!

	james := User{
		name:    'James'
		country: 'UK'
	}
	sql db {
		insert james into User
	}!

	lukas := User{
		name:    'Lucas'
		country: 'DE'
	}
	sql db {
		insert lukas into User
	}!

	users_with_name_starting_with_letter_l := sql db {
		select from User where name like 'L%'
	}!

	assert users_with_name_starting_with_letter_l.len == 2
	assert users_with_name_starting_with_letter_l.filter(it.name.starts_with('L')).len == 2

	users_with_name_with_second_letter_a := sql db {
		select from User where name like '_a%'
	}!

	assert users_with_name_with_second_letter_a.len == 1
	assert users_with_name_with_second_letter_a.first().name == james.name
}
