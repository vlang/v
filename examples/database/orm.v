import sqlite
import mysql

[table: 'modules']
struct Module {
	id           int    [primary; sql: serial]
	name         string
	nr_downloads int    [sql: u64]
	creator      User
}

struct User {
	id             int    [primary; sql: serial]
	age            int    [unique: 'user']
	name           string [sql: 'username'; unique]
	is_customer    bool   [sql: 'abc'; unique: 'user']
	skipped_string string [skip]
}

fn main() {
	db := sqlite.connect(':memory:') or { panic(err) }
	sql db {
		create table Module
	}

	mod := Module{
		name: 'test'
		nr_downloads: 10
		creator: User{
			age: 21
			name: 'VUser'
			is_customer: true
		}
	}
	sql db {
		insert mod into Module
	}

	modul := sql db {
		select from Module where id == 1
	}

	sql db {
		drop table Module
	}

	eprintln(modul)

	mysql()
}

fn mysql() {
	mut conn := mysql.Connection{
		host: 'localhost'
		port: 3306
		username: 'root'
		password: 'abc'
		dbname: 'test'
	}
	conn.connect() or { panic(err) }

	sql conn {
		create table Module
	}

	mod := Module{
		name: 'test'
		nr_downloads: 10
		creator: User{
			age: 21
			name: 'VUser'
			is_customer: true
		}
	}

	sql conn {
		insert mod into Module
	}

	m := sql conn {
		select from Module where id == 1
	}
	eprintln(m)
}
