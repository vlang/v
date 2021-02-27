// import os
// import pg
// import term
import sqlite

struct Module {
	id           int
	name         string
	nr_downloads int
}

struct User {
	id             int
	age            int
	name           string
	is_customer    bool
	skipped_string string [skip]
}

struct Foo {
	age int
}

fn test_orm_sqlite() {
	db := sqlite.connect(':memory:') or { panic(err) }
	db.exec('drop table if exists User')
	db.exec("create table User (id integer primary key, age int default 0, name text default '', is_customer int default 0);")
	name := 'Peter'
	db.exec("insert into User (name, age) values ('Sam', 29)")
	db.exec("insert into User (name, age) values ('Peter', 31)")
	db.exec("insert into User (name, age, is_customer) values ('Kate', 30, 1)")

	nr_all_users := sql db {
		select count from User
	}
	assert nr_all_users == 3
	println('nr_all_users=$nr_all_users')
	//
	nr_users1 := sql db {
		select count from User where id == 1
	}
	assert nr_users1 == 1
	println('nr_users1=$nr_users1')
	//
	nr_peters := sql db {
		select count from User where id == 2 && name == 'Peter'
	}
	assert nr_peters == 1
	println('nr_peters=$nr_peters')
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
		select from User limit 2 offset offset_const
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

	db.exec('insert into User (name, age) values (NULL, 31)')
	null_user := sql db {
		select from User where id == 5
	}
	assert null_user.name == ''
}

fn test_orm_pg() {
	/*
	dbname := os.getenv('VDB_NAME')
	dbuser := os.getenv('VDB_USER')
	if dbname == '' || dbuser == '' {
		eprintln(term.red('NB: this test requires VDB_NAME and VDB_USER env variables to be set'))
		return
	}
	db := pg.connect(dbname: dbname, user: dbuser) or { panic(err) }
	_ = db
	nr_modules := db.select count from modules
	//nr_modules := db.select count from Modules where id == 1
	nr_modules := db.select count from Modules where
		name == 'Bob' && id == 1
	println(nr_modules)

	mod := db.select from Modules where id = 1 limit 1
	println(mod)

	mods := db.select from Modules limit 10
	for mod in mods {
	println(mod)
	}
	*/
	/*
	mod := db.retrieve<Module>(1)
	mod := db.select from Module where id = 1

	mod := db.update Module set name = name + '!' where id > 10


	nr_modules := db.select count from Modules
		where id > 1 && name == ''
	println(nr_modules)

	nr_modules := db.select count from modules
	nr_modules := db.select from modules
	nr_modules := db[:modules].select
	*/
	/*
	mod := select from db.modules where id = 1 limit 1
	println(mod.name)
	top_mods := select from db.modules where nr_downloads > 1000 order by nr_downloads desc limit 10
	top_mods := db.select from modules where nr_downloads > 1000 order by nr_downloads desc limit 10
	top_mods := db.select<Module>(m => m.nr_downloads > 1000).order_by(m => m.nr_downloads).desc().limit(10)
	names := select name from db.modules // []string


	n := db.q_int('select count(*) from modules')
	println(n)
	*/
}
