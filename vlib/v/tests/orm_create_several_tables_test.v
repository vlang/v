module main

import os
import db.sqlite

@[table: 'visits']
struct Visit {
	id   int    @[primary; sql: serial]
	site string
}

@[table: 'sites']
struct Site {
	hostname string  @[primary]
	owner    int
	visits   []Visit @[fkey: 'site']
}

@[table: 'users']
struct User {
	id    int    @[primary; sql: serial]
	name  string
	sites []Site @[fkey: 'owner']
}

fn init_db(path string) !sqlite.DB {
	mut db := sqlite.connect(path)!

	sql db {
		create table User
		create table Site
		create table Visit
	}!

	return db
}

fn test_creating_db() {
	db_folder := os.join_path(os.vtmp_dir(), 'creating_db')
	os.mkdir_all(db_folder)!
	db_path := os.join_path(db_folder, 'abc.db')
	dump(db_path)
	mut db := init_db(db_path)!
	dump(db)
	db.close()!
}
