// vtest build: present_sqlite3?
import db.sqlite

type Connection = sqlite.DB

struct User {
pub:
	id        int @[primary; sql: serial]
	name      string
	last_name ?string
}

type Content = []u8 | string

struct Host {
pub mut:
	db Connection
}

fn (back Host) get_users() []User {
	return []
}

fn create_host(db Connection) !Host {
	sql db {
		create table User
	}!

	return Host{
		db: db
	}
}

fn test_sqlite() {
	$if !linux {
		return
	}
	mut db := sqlite.connect(':memory:') or { panic(err) }
	assert db.is_open
	assert db.validate()!
	db.exec('drop table if exists users')!
	db.exec("create table users (id integer primary key, name text default '', last_name text null default null);")!
	db.exec("insert into users (name) values ('Sam')")!
	assert db.last_insert_rowid() == 1
	assert db.get_affected_rows_count() == 1
	db.exec("insert into users (name) values ('Peter')")!
	assert db.last_insert_rowid() == 2
	db.exec("insert into users (name) values ('Kate')")!
	assert db.last_insert_rowid() == 3
	db.exec_param('insert into users (name) values (?)', 'Tom')!
	assert db.last_insert_rowid() == 4
	nr_users := db.q_int('select count(*) from users')!
	assert nr_users == 4
	name := db.q_string('select name from users where id = 1')!
	assert name == 'Sam'
	username := db.exec_param('select name from users where id = ?', '1')!
	assert username[0].vals[0] == 'Sam'

	// this insert will be rejected due to duplicated id
	db.exec("insert into users (id,name) values (1,'Silly')")!
	assert db.get_affected_rows_count() == 0

	mut users := db.exec('select * from users')!
	dump(users)
	assert users.len == 4
	code := db.exec_none('vacuum')
	assert code == 101
	user := db.exec_one('select * from users where id = 3') or { panic(err) }
	dump(user)
	assert user.vals.len == 3

	db.exec("update users set name='zzzz' where name='qqqq'")!
	assert db.get_affected_rows_count() == 0

	db.exec("update users set name='Peter1' where name='Peter'")!
	assert db.get_affected_rows_count() == 1
	db.exec_param_many('update users set name=? where name=?', ['Peter', 'Peter1'])!
	assert db.get_affected_rows_count() == 1

	db.exec("delete from users where name='qqqq'")!
	assert db.get_affected_rows_count() == 0

	db.exec("delete from users where name='Sam'")!
	assert db.get_affected_rows_count() == 1

	// transaction test
	db.begin()!
	db.exec("insert into users (name) values ('John')")!
	assert db.last_insert_rowid() == 5
	db.savepoint('new_savepoint')!
	db.exec("insert into users (name) values ('Kitty')")!
	assert db.last_insert_rowid() == 6
	db.rollback_to('new_savepoint')!
	db.exec("insert into users (name) values ('Mars')")!
	assert db.last_insert_rowid() == 6
	db.commit()!
	users = db.exec('select * from users')!
	dump(users)
	assert users.len == 5

	db.close() or { panic(err) }
	assert !db.is_open
}

fn test_can_access_sqlite_result_consts() {
	assert sqlite.sqlite_ok == 0
	assert sqlite.sqlite_error == 1
	// assert sqlite.misuse == 21
	assert sqlite.sqlite_row == 100
	assert sqlite.sqlite_done == 101
}

fn test_alias_db() {
	mut host := create_host(sqlite.connect(':memory:')!)!
	host.db.close()!
	assert true
}

fn test_exec_param_many() {
	$if !linux {
		return
	}
	mut db := sqlite.connect(':memory:') or { panic(err) }
	assert db.is_open
	db.exec('drop table if exists users')!
	db.exec("create table users (id integer primary key, name text default '' unique);")!
	db.exec("insert into users (name) values ('Sam')")!
	db.exec_param_many('insert into users (id, name) values (?, ?)', [
		'60',
		'Sam',
	]) or {
		assert err.code() == 19 // constraint failure
		db.close()!
		return
	}
	db.close()!
	assert false
}
