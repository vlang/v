import sqlite
import mysql

struct Module {
	id           int
	name         string
	nr_downloads int
	creator      User
}

struct User {
	id             int
	age            int
	name           string
	is_customer    bool
	skipped_string string [skip]
}

fn main() {
	db := sqlite.connect(':memory:') or { panic(err) }
	db.exec('drop table if exists User')
	db.exec("create table Module (id integer primary key, name text default '', nr_downloads int default 0, creator int default 0);")
	db.exec("create table User (id integer primary key, age int default 0, name text default '', is_customer int default 0);")

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

	println(modul.name)
	println(modul.creator.name)

	mysql()
}

fn mysql() {
	mut conn := mysql.Connection{
		host: 'localhost'
		port: 3306
		username: 'v'
		password: ''
		dbname: 'test'
	}
	conn.connect() or { panic(err) }
	_ := conn.query("create table Module (id integer primary key, name text default '', nr_downloads int default 0, creator int default 0);") or { panic(err) }
	a := conn.query("create table User (id integer primary key, age int default 0, name text default '', is_customer int default 0);") or { panic(err) }
	
	eprintln(a.rows())

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

}
