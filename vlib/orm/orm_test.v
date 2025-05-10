// vtest retry: 3
// vtest build: !windows
// import db.mysql
// import db.pg
import time
import db.sqlite

const offset_const = 2

struct Module {
	id           int @[primary; sql: serial]
	name         string
	nr_downloads int
	test_id      u64
	user         ?User
	created      time.Time
}

@[table: 'userlist']
struct User {
	id              int @[primary; sql: serial]
	age             int
	name            string @[sql: 'username']
	is_customer     bool
	skipped_string  string   @[skip]
	skipped_string2 string   @[sql: '-']
	skipped_array   []string @[skip]
	skipped_array2  []string @[sql: '-']
}

struct Foo {
	age int
}

struct TestTime {
	id     int @[primary; sql: serial]
	create time.Time
}

fn test_use_struct_field_as_limit() {
	db := sqlite.connect(':memory:') or { panic(err) }

	sql db {
		create table User
	}!

	foo := Foo{
		age: 10
	}

	sam := User{
		age:             29
		name:            'Sam'
		skipped_string2: 'this should be ignored'
		skipped_array:   ['ignored', 'array']
		skipped_array2:  ['another', 'ignored', 'array']
	}

	sql db {
		insert sam into User
	}!

	users := sql db {
		select from User limit foo.age
	}!

	assert users.len == 1
	assert users[0].name == 'Sam'
	assert users[0].age == 29
	assert users[0].skipped_string == ''
	assert users[0].skipped_string2 == ''
	assert users[0].skipped_array == [], 'skipped because of the @[skip] tag, used for both sql and json'
	assert users[0].skipped_array2 == [], "should be skipped, because of the sql specific @[sql: '-'] tag"
}

fn test_orm() {
	db := sqlite.connect(':memory:') or { panic(err) }

	sql db {
		create table Module
	}!
	sql db {
		create table User
	}!

	name := 'Peter'

	sam := User{
		age:  29
		name: 'Sam'
	}

	peter := User{
		age:  31
		name: 'Peter'
	}

	k := User{
		age:         30
		name:        'Kate'
		is_customer: true
	}

	sql db {
		insert sam into User
		insert peter into User
		insert k into User
	}!

	c := sql db {
		select count from User where id != 1
	}!
	assert c == 2

	nr_all_users := sql db {
		select count from User
	}!
	assert nr_all_users == 3

	nr_users1 := sql db {
		select count from User where id == 1
	}!
	assert nr_users1 == 1

	nr_peters := sql db {
		select count from User where id == 2 && name == 'Peter'
	}!
	assert nr_peters == 1

	nr_peters2 := sql db {
		select count from User where id == 2 && name == name
	}!
	assert nr_peters2 == 1

	nr_peters3 := sql db {
		select count from User where name == name
	}!
	assert nr_peters3 == 1

	peters := sql db {
		select from User where name == name
	}!
	assert peters.len == 1
	assert peters[0].name == 'Peter'

	mut users := sql db {
		select from User where name == name limit 1
	}!

	one_peter := users.first()
	assert one_peter.name == 'Peter'
	assert one_peter.id == 2

	users = sql db {
		select from User where id == 1
	}!

	user := users.first()
	assert user.name == 'Sam'
	assert user.id == 1
	assert user.age == 29

	users = sql db {
		select from User where id > 0
	}!
	assert users.len == 3
	assert users[0].name == 'Sam'
	assert users[1].name == 'Peter'
	assert users[1].age == 31

	users2 := sql db {
		select from User where id < 0
	}!
	assert users2.len == 0

	users3 := sql db {
		select from User where age == 29 || age == 31
	}!

	assert users3.len == 2
	assert users3[0].age == 29
	assert users3[1].age == 31

	new_user := User{
		name: 'New user'
		age:  30
	}
	sql db {
		insert new_user into User
	}!

	users = sql db {
		select from User where id == 4
	}!

	x := users.first()
	assert x.age == 30
	assert x.id == 4
	assert x.name == 'New user'

	users = sql db {
		select from User where id == 3
	}!

	kate := users.first()
	assert kate.is_customer == true

	users = sql db {
		select from User where is_customer == true limit 1
	}!

	customer := users.first()
	assert customer.is_customer == true
	assert customer.name == 'Kate'

	sql db {
		update User set age = 31 where name == 'Kate'
	}!

	users = sql db {
		select from User where id == 3
	}!
	kate2 := users.first()
	assert kate2.age == 31
	assert kate2.name == 'Kate'

	sql db {
		update User set age = 32, name = 'Kate N' where name == 'Kate'
	}!

	users = sql db {
		select from User where id == 3
	}!
	mut kate3 := users.first()
	assert kate3.age == 32
	assert kate3.name == 'Kate N'

	new_age := 33
	sql db {
		update User set age = new_age, name = 'Kate N' where id == 3
	}!

	users = sql db {
		select from User where id == 3
	}!

	kate3 = users.first()
	assert kate3.age == 33
	assert kate3.name == 'Kate N'

	foo := Foo{34}
	sql db {
		update User set age = foo.age, name = 'Kate N' where id == 3
	}!

	users = sql db {
		select from User where id == 3
	}!
	kate3 = users.first()
	assert kate3.age == 34
	assert kate3.name == 'Kate N'

	no_user := sql db {
		select from User where id == 30
	}!

	assert no_user.len == 0

	two_users := sql db {
		select from User limit 2
	}!
	assert two_users.len == 2
	assert two_users[0].id == 1

	y := sql db {
		select from User limit 2 offset 1
	}!
	assert y.len == 2
	assert y[0].id == 2

	z := sql db {
		select from User order by id limit 2 offset offset_const
	}!
	assert z.len == 2
	assert z[0].id == 3

	users = sql db {
		select from User order by age desc limit 1
	}!

	oldest := users.first()
	assert oldest.age == 34

	offs := 1
	users = sql db {
		select from User order by age desc limit 1 offset offs
	}!

	second_oldest := users.first()
	assert second_oldest.age == 31
	sql db {
		delete from User where age == 34
	}!

	users = sql db {
		select from User order by age desc limit 1
	}!
	updated_oldest := users.first()
	assert updated_oldest.age == 31

	// Remove this when pg is used
	// db.exec('insert into User (name, age) values (NULL, 31)')
	users = sql db {
		select from User where id == 5
	}!
	assert users.len == 0

	users = sql db {
		select from User where id == 1
	}!
	age_test := users.first()

	assert age_test.age == 29

	sql db {
		update User set age = age + 1 where id == 1
	}!

	users = sql db {
		select from User where id == 1
	}!

	mut first := users.first()
	assert first.age == 30

	sql db {
		update User set age = age * 2 where id == 1
	}!

	users = sql db {
		select from User where id == 1
	}!

	first = users.first()
	assert first.age == 60

	sql db {
		create table TestTime
	}!

	tnow := time.now()

	time_test := TestTime{
		create: tnow
	}

	sql db {
		insert time_test into TestTime
	}!

	data := sql db {
		select from TestTime where create == tnow
	}!

	assert data.len == 1
	assert tnow.unix() == data[0].create.unix()

	mod := Module{}

	sql db {
		insert mod into Module
	}!

	sql db {
		update Module set test_id = 11 where id == 1
	}!

	mut modules := sql db {
		select from Module where id == 1
	}!

	assert modules.first().test_id == 11

	t := time.now()
	sql db {
		update Module set created = t where id == 1
	}!

	modules = sql db {
		select from Module where id == 1
	}!

	// Note: usually updated_time_mod.created != t, because t has
	// its microseconds set, while the value retrieved from the DB
	// has them zeroed, because the db field resolution is seconds.
	assert modules.first().created.format_ss() == t.format_ss()

	users = sql db {
		select from User where (name == 'Sam' && is_customer == true) || id == 1
	}!

	assert users.first() == first

	sql db {
		drop table Module
		drop table TestTime
	}!
}
