//import os
//import pg
//import term
import sqlite

struct Module {
	id int
	user_id int
	//name string
	//url string
	//nr_downloads int
}

struct User {
	id int
	age int
	name string
}

fn test_orm_sqlite() {
	db := sqlite.connect(':memory:') or { panic(err) }
	db.exec("drop table if exists User")
	db.exec("create table User (id integer primary key, age int default 0, name text default '');")

	name := 'sam'

	db.exec("insert into User (name, age) values ('Sam', 29)")
	db.exec("insert into User (name, age) values ('Peter', 31)")
	db.exec("insert into User (name) values ('Kate')")
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
	nr_sams := sql db {
		select count from User where id == 1 && name == name
	}
	println('nr_sams=$nr_sams')
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

	//user2 := User{}
	//x := sql db {
		//insert user2 into User
	//}
	//db.insert<User>(user2)
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
