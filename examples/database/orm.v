// vtest build: !(macos || windows)
import os
import db.sqlite
import db.mysql
import db.pg

// The goal of this example, is to show how you can connect to
// several different databases in the same program, and use both
// the ORM and the native connection wrapper, that each DB driver
// provides, if you need to execute more complex SQL queries.
//
// You can use environment variables to pass your local DB connection
// settings, without editing the code, like this:
//
//    MUSER='myuser' MPASS='abc' MDATABASE='vtestdb'  PGUSER='postgres' PGPASS='password' PGDATABASE='postgres'  ./v -g run examples/database/orm.v
//
// WARNING: this example will drop and re-create any tables named:
//   * modules
//   * User
//   * Parent
//   * Child
// in the passed databases, so it is better to use empty DBs for it.

const mysql_host = os.getenv_opt('MHOST') or { 'localhost' }
const mysql_port = os.getenv_opt('MPORT') or { '3306' }.u32()
const mysql_user = os.getenv_opt('MUSER') or { 'myuser' }
const mysql_pass = os.getenv_opt('MPASS') or { 'abc' }
const mysql_db = os.getenv_opt('MDATABASE') or { 'test' }

const pg_host = os.getenv_opt('PGHOST') or { 'localhost' }
const pg_user = os.getenv_opt('PGUSER') or { 'test' }
const pg_pass = os.getenv_opt('PGPASS') or { 'abc' }
const pg_db = os.getenv_opt('PGDATABASE') or { 'test' }

@[table: 'modules']
struct Module {
	id           int @[primary; sql: serial]
	name         string
	nr_downloads int @[sql: u64]
	creator      User
}

struct User {
	id             int    @[primary; sql: serial]
	age            u32    @[unique: 'user']
	name           string @[sql: 'username'; sql_type: 'VARCHAR(200)'; unique]
	is_customer    bool   @[sql: 'abc'; unique: 'user']
	skipped_string string @[skip]
}

struct Parent {
	id       int @[primary; sql: serial]
	name     string
	children []Child @[fkey: 'parent_id']
}

struct Child {
	id        int @[primary; sql: serial]
	parent_id int
	name      string
}

fn sqlite3_array() ! {
	eprintln('------------ ${@METHOD} -----------------')
	mut db := sqlite.connect(':memory:')!
	defer {
		sql db {
			drop table Parent
			drop table Child
		} or {}
		db.close() or {}
	}

	sql db {
		create table Parent
	}!
	sql db {
		create table Child
	}!
	par := Parent{
		name:     'test'
		children: [
			Child{
				name: 'abc'
			},
			Child{
				name: 'def'
			},
		]
	}
	sql db {
		insert par into Parent
	}!
	parent := sql db {
		select from Parent where id == 1
	}!
	eprintln(parent)
}

fn msql_array() ! {
	eprintln('------------ ${@METHOD} -----------------')
	mut db := mysql.connect(
		host:     mysql_host
		port:     mysql_port
		username: mysql_user
		password: mysql_pass
		dbname:   mysql_db
	)!
	defer {
		sql db {
			drop table Parent
		} or {}
		db.close() or {}
	}

	db.query('drop table if exists Parent')!
	db.query('drop table if exists Child')!
	sql db {
		create table Parent
		create table Child
	}!
	par := Parent{
		name:     'test'
		children: [
			Child{
				name: 'abc'
			},
			Child{
				name: 'def'
			},
		]
	}
	sql db {
		insert par into Parent
	}!
	parent := sql db {
		select from Parent where id == 1
	}!
	eprintln(parent)
}

fn psql_array() ! {
	eprintln('------------ ${@METHOD} -----------------')
	mut db := pg.connect(host: pg_host, user: pg_user, password: pg_pass, dbname: pg_db)!
	defer {
		db.exec_one('drop table if exists "Parent", "Child"') or { eprintln(err) }
		db.close() or {}
	}
	db.exec_one('drop table if exists "Parent", "Child"') or { eprintln(err) }

	sql db {
		create table Parent
		create table Child
	}!
	par := Parent{
		name:     'test'
		children: [
			Child{
				name: 'abc'
			},
			Child{
				name: 'def'
			},
		]
	}
	sql db {
		insert par into Parent
	}!
	parent := sql db {
		select from Parent where id == 1
	}!
	eprintln(parent)
}

fn sqlite3() ! {
	eprintln('------------ ${@METHOD} -----------------')
	mut db := sqlite.connect(':memory:')!
	defer {
		sql db {
			drop table Module
			drop table User
		} or {}
		db.close() or {}
	}

	sql db {
		create table Module
	}!
	sql db {
		create table User
	}!
	mod := Module{
		name:         'test'
		nr_downloads: 10
		creator:      User{
			age:         21
			name:        'VUser'
			is_customer: true
		}
	}
	sql db {
		insert mod into Module
	}!
	modul := sql db {
		select from Module where id == 1
	}!
	eprintln(modul)
}

fn msql() ! {
	eprintln('------------ ${@METHOD} -----------------')
	mut conn := mysql.connect(
		host:     mysql_host
		port:     mysql_port
		username: mysql_user
		password: mysql_pass
		dbname:   mysql_db
	)!
	defer {
		conn.query('DROP TABLE IF EXISTS Module') or { eprintln(err) }
		conn.query('DROP TABLE IF EXISTS User') or { eprintln(err) }
		conn.close() or {}
	}
	conn.query('DROP TABLE IF EXISTS Module') or { eprintln(err) }
	conn.query('DROP TABLE IF EXISTS User') or { eprintln(err) }

	sql conn {
		create table Module
	}!
	sql conn {
		create table User
	}!
	mod := Module{
		name:         'test'
		nr_downloads: 10
		creator:      User{
			age:         21
			name:        'VUser'
			is_customer: true
		}
	}
	sql conn {
		insert mod into Module
	}!
	m := sql conn {
		select from Module where id == 1
	}!
	eprintln(m)
}

fn psql() ! {
	eprintln('------------ ${@METHOD} -----------------')
	mut db := pg.connect(host: pg_host, user: pg_user, password: pg_pass, dbname: pg_db)!
	defer {
		db.exec_one('drop table if exists "modules", "User"') or { eprintln(err) }
		db.close() or {}
	}
	db.exec_one('drop table if exists "modules", "User"') or { eprintln(err) }
	sql db {
		create table Module
		create table User
	}!
	mod := Module{
		name:         'test'
		nr_downloads: 10
		creator:      User{
			age:         21
			name:        'VUser'
			is_customer: true
		}
	}
	sql db {
		insert mod into Module
	}!
	modul := sql db {
		select from Module where id == 1
	}!
	sql db {
		drop table Module
	}!
	eprintln(modul)
}

fn main() {
	eprintln('------------ ${@METHOD} -----------------')
	sqlite3_array()!
	msql_array()!
	psql_array()!

	sqlite3()!
	msql()!
	psql()!
}
