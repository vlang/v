import os
import pg
import term
import sqlite

struct Modules {
	id int
	user_id int
	name string
	url string
	//nr_downloads int
}

fn test_orm_sqlite() {
	db := sqlite.connect(':memory:') or { panic(err) }
	/*
	db.exec("drop table if exists users")
	db.exec("create table users (id integer primary key, name text default '');")

	db.exec("insert into users (name) values ('Sam')")
	db.exec("insert into users (name) values ('Peter')")
	db.exec("insert into users (name) values ('Kate')")
	nr_users := sql db {
		//select count from modules
	}
	assert nr_users == 3
	println('nr_users=')
	println(nr_users)
	//nr_modules := db.select count from modules
	//nr_modules := db.select count from Modules where id == 1
	//nr_modules := db.select count from Modules where
		//name == 'Bob' && id == 1
	*/
}


fn test_orm_pg() {
	dbname := os.getenv('VDB_NAME')
	dbuser := os.getenv('VDB_USER')
	if dbname == '' || dbuser == '' {
		eprintln(term.red('NB: this test requires VDB_NAME and VDB_USER env variables to be set'))
		return
	}
	db := pg.connect(dbname: dbname, user: dbuser) or { panic(err) }
	_ = db
/*
	//nr_modules := db.select count from modules
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
