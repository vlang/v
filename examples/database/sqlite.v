import db.sqlite

fn main() {
	db := sqlite.connect(':memory:')!
	db.exec("create table users (id integer primary key, name text default '');") or { panic(err) }

	db.exec("insert into users (name) values ('Sam')") or { panic(err) }
	db.exec("insert into users (name) values ('Peter')") or { panic(err) }
	db.exec("insert into users (name) values ('Kate')") or { panic(err) }

	nr_users := db.q_int('select count(*) from users') or { panic(err) }
	println('nr users = ${nr_users}')

	name := db.q_string('select name from users where id = 1') or { panic(err) }
	assert name == 'Sam'

	users := db.exec('select * from users') or { panic(err) }
	for row in users {
		println(row.vals)
	}
}
