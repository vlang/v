module main

import db.sqlite

fn main() {
	db := sqlite.connect('testing.db') or { panic(err) }

	println(db.exec_none('CREATE TABLE IF NOT EXISTS users (
							id INTEGER PRIMARY KEY,
							username TEXT NOT NULL UNIQUE,
							password TEXT NOT NULL)'))
	println(db.exec_none('CREATE TABLE IF NOT EXISTS sessions (
							id INTEGER PRIMARY KEY,
							user_id INTEGER NOT NULL,
							session_token TEXT NOT NULL UNIQUE,
							FOREIGN KEY (user_id) REFERENCES users(id))'))

	println(db.exec_none('INSERT INTO users (username, password) VALUES ("jackson", "password")'))
	println(db.exec_none('INSERT INTO SESSIONS (user_id, session_token) VALUES ("1", "testing_token")'))

	println(db.exec_param_many('SELECT * from users WHERE username = ?', ['jackson']) or {
		panic(err)
	})
	println(db.exec_param_many('SELECT * from sessions LEFT JOIN users on users.id = sessions.user_id WHERE username = ?',
		['jackson']) or { panic(err) })
}
