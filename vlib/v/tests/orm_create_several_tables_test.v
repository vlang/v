module main

import db.sqlite

@[table: 'visits']
struct Visit {
	id   int @[primary; sql: serial]
	site string
}

@[table: 'sites']
struct Site {
	hostname string @[primary]
	owner    int
	visits   []Visit @[fkey: 'site']
}

@[table: 'users']
struct User {
	id    int @[primary; sql: serial]
	name  string
	sites []Site @[fkey: 'owner']
}

fn test_creating_db() {
	mut db := sqlite.connect(':memory:')!
	sql db {
		create table User
		create table Site
		create table Visit
	}!
	db.close()!
}
