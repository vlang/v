import sqlite

fn test_sqlite() {
	$if !linux {
		return
	}
	db := sqlite.connect(':memory:')
	db.exec("drop table if exists users")
	db.exec("create table users (id integer primary key, name text default '');")

	db.exec("insert into users (name) values ('Sam')")
	db.exec("insert into users (name) values ('Peter')")
	db.exec("insert into users (name) values ('Kate')")

	nr_users := db.q_int('select count(*) from users')
	assert nr_users == 3

	name := db.q_string('select name from users where id = 1')
	assert name == 'Sam'

	users, mut code := db.exec('select * from users')
	assert users.len == 3
	assert code == 101

	code = db.exec_none('vacuum')
	assert code == 101

	user := db.exec_one('select * from users where id = 3') or {
		panic(err)
	}
	assert user.vals.len == 2
}
