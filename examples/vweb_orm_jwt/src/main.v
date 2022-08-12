module main

import vweb
import databases

const (
	http_port = 8081
)

struct App {
	vweb.Context
}

fn main() {
	mut db := databases.create_db_connection() or { panic(err) }

	sql db {
		create table User
	}

	db.close() or { panic(err) }

	vweb.run(new_app(), http_port)
}

fn new_app() &App {
	mut app := &App{}

	return app
}
