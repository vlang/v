import sqlite

fn main() {
	db := sqlite.connect(':memory:')?
	db.exec("create table users (id integer primary key, name text default '');")

	db.exec("insert into users (name) values ('Sam')")
	db.exec("insert into users (name) values ('Peter')")
	db.exec("insert into users (name) values ('Kate')")

	nr_users := db.q_int('select count(*) from users')
	println('nr users = $nr_users')

	name := db.q_string('select name from users where id = 1')
	assert name == 'Sam'

	users, code := db.exec('select * from users')
	println('SQL Result code: $code')
	for row in users {
		println(row.vals)
	}
}
