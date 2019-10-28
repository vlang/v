import os
import sqlite

const (
	sqlite_test_users_db_file = 'sqlite_test_users.db'
)

fn test_sqlite() {
	os.rm( sqlite_test_users_db_file )
	db := sqlite.connect( sqlite_test_users_db_file )
	db.exec("create table users (id integer primary key, name text default '');")

	db.exec("insert into users (name) values ('Sam')")
	db.exec("insert into users (name) values ('Peter')")
	db.exec("insert into users (name) values ('Kate')")

	nr_users := db.q_int('select count(*) from users')
	println('nr users = $nr_users')

	name := db.q_string('select name from users where id = 1')
	assert name == 'Sam'

	users := db.exec('select * from users')
	assert users.len == 3
	for row in users {
		println(row.vals)
	}
	os.rm( sqlite_test_users_db_file )
}
