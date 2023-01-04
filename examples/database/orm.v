import sqlite
import mysql
import pg

[table: 'modules']
struct Module {
	id           int    [primary; sql: serial]
	name         string
	nr_downloads int    [sql: u64]
	creator      User
}

struct User {
	id             int    [primary; sql: serial]
	age            u32    [unique: 'user']
	name           string [sql: 'username'; unique]
	is_customer    bool   [sql: 'abc'; unique: 'user']
	skipped_string string [skip]
}

struct Parent {
	id       int     [primary; sql: serial]
	name     string
	children []Child [fkey: 'parent_id']
}

struct Child {
	id        int    [primary; sql: serial]
	parent_id int
	name      string
}

fn main() {
	sqlite3_array()
	mysql_array()
	psql_array()

	sqlite3()
	mysql()
	psql()
}

fn sqlite3_array() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	sql db {
		create table Parent
	}

	par := Parent{
		name: 'test'
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
	}

	parent := sql db {
		select from Parent where id == 1
	}

	sql db {
		drop table Parent
	}

	eprintln(parent)
}

fn mysql_array() {
	mut db := mysql.Connection{
		host: 'localhost'
		port: 3306
		username: 'root'
		password: 'abc'
		dbname: 'test'
	}
	db.connect() or { panic(err) }

	sql db {
		create table Parent
	}

	par := Parent{
		name: 'test'
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
	}

	parent := sql db {
		select from Parent where id == 1
	}

	eprintln(parent)

	sql db {
		drop table Parent
	}

	db.close()
}

fn psql_array() {
	mut db := pg.connect(host: 'localhost', user: 'test', password: 'abc', dbname: 'test') or {
		panic(err)
	}

	sql db {
		create table Parent
	}

	par := Parent{
		name: 'test'
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
	}

	parent := sql db {
		select from Parent where id == 1
	}

	eprintln(parent)

	sql db {
		drop table Parent
	}

	db.close()
}

fn sqlite3() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
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
	db.close() or { panic(err) }
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
	conn.close()
}

fn psql() {
	mut db := pg.connect(host: 'localhost', user: 'test', password: 'abc', dbname: 'test') or {
		panic(err)
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
		create table Module
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
	db.close()
}
