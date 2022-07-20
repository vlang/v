module main

import vweb
import databases
import zztkm.vdotenv

const (
	http_port = 8081
)

struct App {
	vweb.Context
}

fn main() {
	vdotenv.load()
	mut db := databases.create_db_connection() or { panic(err) }

	sql db {
		create table User
	}

	db.close()

	vweb.run(new_app(), http_port)
}

fn new_app() &App {
	mut app := &App{}

	return app
}
