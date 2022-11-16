// import os
// import term
// import mysql
// import pg
import time
import sqlite

struct Module {
	id           int       [primary; sql: serial]
	name         string
	nr_downloads int
	test_id      u64
	user         User
	created      time.Time
}

[table: 'userlist']
struct User {
	id             int    [primary; sql: serial]
	age            int
	name           string [sql: 'username']
	is_customer    bool
	skipped_string string [skip]
}

struct Foo {
	age int
}

struct TestTime {
	id     int       [primary; sql: serial]
	create time.Time
}

fn test_orm() {
	db := sqlite.connect(':memory:') or { panic(err) }
	db.exec('drop table if exists User')

	// db := pg.connect(host: 'localhost', port: 5432, user: 'louis', password: 'abc', dbname: 'orm') or { panic(err) }
	/*
	mut db := mysql.Connection{
		host: '127.0.0.1'
		username: 'root'
		password: 'pw'
		dbname: 'v'
	}
	db.connect() or { panic(err) }*/

	sql db {
		create table Module
	}

	name := 'Peter'

	sam := User{
		age: 29
		name: 'Sam'
	}

	peter := User{
		age: 31
		name: 'Peter'
	}

	k := User{
		age: 30
		name: 'Kate'
		is_customer: true
	}

	sql db {
		insert sam into User
		insert peter into User
		insert k into User
	}

	c := sql db {
		select count from User where id != 1
	}
	assert c == 2

	nr_all_users := sql db {
		select count from User
	}
	assert nr_all_users == 3
	println('nr_all_users=${nr_all_users}')
	//
	nr_users1 := sql db {
		select count from User where id == 1
	}
	assert nr_users1 == 1
	println('nr_users1=${nr_users1}')
	//
	nr_peters := sql db {
		select count from User where id == 2 && name == 'Peter'
	}
	assert nr_peters == 1
	println('nr_peters=${nr_peters}')
	//
	nr_peters2 := sql db {
		select count from User where id == 2 && name == name
	}
	assert nr_peters2 == 1
	nr_peters3 := sql db {
		select count from User where name == name
	}
	assert nr_peters3 == 1
	peters := sql db {
		select from User where name == name
	}
	assert peters.len == 1
	assert peters[0].name == 'Peter'
	one_peter := sql db {
		select from User where name == name limit 1
	}
	assert one_peter.name == 'Peter'
	assert one_peter.id == 2
	//
	user := sql db {
		select from User where id == 1
	}
	println(user)
	assert user.name == 'Sam'
	assert user.id == 1
	assert user.age == 29
	//
	users := sql db {
		select from User where id > 0
	}
	println(users)
	assert users.len == 3
	assert users[0].name == 'Sam'
	assert users[1].name == 'Peter'
	assert users[1].age == 31
	//
	users2 := sql db {
		select from User where id < 0
	}
	println(users2)
	assert users2.len == 0
	//
	users3 := sql db {
		select from User where age == 29 || age == 31
	}
	println(users3)
	assert users3.len == 2
	assert users3[0].age == 29
	assert users3[1].age == 31
	//
	missing_user := sql db {
		select from User where id == 8777
	}
	println('missing_user:')
	println(missing_user) // zero struct
	//
	new_user := User{
		name: 'New user'
		age: 30
	}
	sql db {
		insert new_user into User
	}

	// db.insert<User>(user2)
	x := sql db {
		select from User where id == 4
	}
	println(x)
	assert x.age == 30
	assert x.id == 4
	assert x.name == 'New user'
	//
	kate := sql db {
		select from User where id == 3
	}
	assert kate.is_customer == true
	//
	customer := sql db {
		select from User where is_customer == true limit 1
	}
	assert customer.is_customer == true
	assert customer.name == 'Kate'
	//
	sql db {
		update User set age = 31 where name == 'Kate'
	}

	kate2 := sql db {
		select from User where id == 3
	}
	assert kate2.age == 31
	assert kate2.name == 'Kate'
	//
	sql db {
		update User set age = 32, name = 'Kate N' where name == 'Kate'
	}

	mut kate3 := sql db {
		select from User where id == 3
	}
	assert kate3.age == 32
	assert kate3.name == 'Kate N'
	//
	/*
	sql db {
		update User set age = age + 1, name = 'Kate N' where name == 'Kate'
	}
	kate3 = sql db {
		select from User where id == 3
	}
	println(kate3)
	assert kate3.age == 32
	assert kate3.name == 'Kate N'
	*/
	new_age := 33
	sql db {
		update User set age = new_age, name = 'Kate N' where id == 3
	}

	kate3 = sql db {
		select from User where id == 3
	}
	assert kate3.age == 33
	assert kate3.name == 'Kate N'
	//
	foo := Foo{34}
	sql db {
		update User set age = foo.age, name = 'Kate N' where id == 3
	}

	kate3 = sql db {
		select from User where id == 3
	}
	assert kate3.age == 34
	assert kate3.name == 'Kate N'
	//
	no_user := sql db {
		select from User where id == 30
	}
	assert no_user.name == '' // TODO optional
	assert no_user.age == 0
	//
	two_users := sql db {
		select from User limit 2
	}
	assert two_users.len == 2
	assert two_users[0].id == 1
	//
	y := sql db {
		select from User limit 2 offset 1
	}
	assert y.len == 2
	assert y[0].id == 2
	//
	offset_const := 2
	z := sql db {
		select from User order by id limit 2 offset offset_const
	}
	assert z.len == 2
	assert z[0].id == 3
	oldest := sql db {
		select from User order by age desc limit 1
	}
	assert oldest.age == 34
	offs := 1
	second_oldest := sql db {
		select from User order by age desc limit 1 offset offs
	}
	assert second_oldest.age == 31
	sql db {
		delete from User where age == 34
	}

	updated_oldest := sql db {
		select from User order by age desc limit 1
	}
	assert updated_oldest.age == 31

	// Remove this when pg is used
	// db.exec('insert into User (name, age) values (NULL, 31)')
	null_user := sql db {
		select from User where id == 5
	}
	assert null_user.name == ''

	age_test := sql db {
		select from User where id == 1
	}

	assert age_test.age == 29

	sql db {
		update User set age = age + 1 where id == 1
	}

	mut first := sql db {
		select from User where id == 1
	}

	assert first.age == 30

	sql db {
		update User set age = age * 2 where id == 1
	}

	first = sql db {
		select from User where id == 1
	}

	assert first.age == 60

	sql db {
		create table TestTime
	}

	tnow := time.now()

	time_test := TestTime{
		create: tnow
	}

	sql db {
		insert time_test into TestTime
	}

	data := sql db {
		select from TestTime where create == tnow
	}

	assert data.len == 1
	assert tnow.unix == data[0].create.unix

	mod := Module{}

	sql db {
		insert mod into Module
	}

	sql db {
		update Module set test_id = 11 where id == 1
	}

	test_id_mod := sql db {
		select from Module where id == 1
	}

	assert test_id_mod.test_id == 11

	t := time.now()
	sql db {
		update Module set created = t where id == 1
	}

	updated_time_mod := sql db {
		select from Module where id == 1
	}

	// Note: usually updated_time_mod.created != t, because t has
	// its microseconds set, while the value retrieved from the DB
	// has them zeroed, because the db field resolution is seconds.
	assert updated_time_mod.created.format_ss() == t.format_ss()

	para_select := sql db {
		select from User where (name == 'Sam' && is_customer == true) || id == 1
	}

	assert para_select[0] == first

	sql db {
		drop table Module
		drop table TestTime
	}
}
