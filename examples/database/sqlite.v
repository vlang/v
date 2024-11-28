import db.sqlite

fn main() {
	db := sqlite.connect(':memory:')!
	db.exec("create table users (id integer primary key, name text default '');") or { panic(err) }

	db.exec("insert into users (name) values ('Sam')")!
	db.exec("insert into users (name) values ('Peter')")!
	db.exec("insert into users (name) values ('Kate')")!

	nr_users := db.q_int('select count(*) from users')!
	println('nr users = ${nr_users}')

	name := db.q_string('select name from users where id = 1')!
	assert name == 'Sam'

	users := db.exec('select * from users')!
	for row in users {
		println(row.vals)
	}
}
