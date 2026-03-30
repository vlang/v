// vtest build: present_sqlite3?
import db.sqlite
import orm
import os

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
	// dump(users)
	assert users.len == 4
	code := db.exec_none('vacuum')
	assert code == 101
	user := db.exec_one('select * from users where id = 3') or { panic(err) }
	// dump(user)
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
	// dump(users)
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

fn test_exec_param_many2() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	assert db.is_open
	db.exec('drop table if exists users')!
	db.exec("create table users (id integer primary key, name text default '' unique);")!
	db.exec_param_many('insert into users (id, name) values (?, ?)', [
		['60', 'Sam'],
		['61', 'Foo'],
		['62', 'Bar'],
	])!
	count := db.q_int('select count(*) from users')!
	assert count == 3

	db.close()!
}

fn test_tables() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	db.exec('create table alpha (id integer)')!
	db.exec('create table beta (id integer)')!
	tbl := db.tables()!
	assert tbl == ['alpha', 'beta']
	db.close()!
}

fn test_columns() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	db.exec('create table items (id integer primary key, name text, price real)')!
	cols := db.columns('items')!
	assert cols == ['id', 'name', 'price']
	db.close()!
}

fn test_schema() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	db.exec('create table things (id integer primary key, label text)')!
	s := db.schema('things')!
	assert s.contains('CREATE TABLE things')
	// empty table name returns all objects
	all := db.schema('')!
	assert all.contains('CREATE TABLE things')
	db.close()!
}

fn test_db_size() {
	tmp := os.join_path(os.temp_dir(), 'test_db_size.db')
	defer {
		os.rm(tmp) or {}
	}
	mut db := sqlite.connect(tmp) or { panic(err) }
	db.exec('create table t (id integer)')!
	sz := db.db_size()!
	assert sz > 0
	db.close()!
}

fn test_orm_transaction_interface() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	mut conn := orm.TransactionalConnection(db)
	mut tx := orm.begin(mut conn)!
	tx.transaction[int](fn (mut tx orm.Tx) !int {
		return 1
	})!
	tx.commit()!
}
