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
		username: 'root'
		password: 'abc'
		dbname: 'test'
	}
	conn.connect() or { panic(err) }
	_ := conn.query("create table if not exists Module (id SERIAL, name TEXT NOT NULL, nr_downloads INT DEFAULT 0, creator INT DEFAULT 0, PRIMARY KEY(`id`));") or { panic(err) }
	_ := conn.query("create table if not exists User (id SERIAL, age INT DEFAULT 0, name TEXT NOT NULL, is_customer INT DEFAULT 0, PRIMARY KEY(`id`));") or { panic(err) }
	
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
