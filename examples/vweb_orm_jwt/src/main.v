module main

import vweb
import databases

const (
	http_port = 8081
)

struct App {
	vweb.Context
}

pub fn (app App) before_request() {
	println('[Vweb] $app.Context.req.method $app.Context.req.url')
}

fn main() {
	mut db := databases.create_db_connection() or { panic(err) }

	sql db {
		create table User
	} or { panic('error on create table: $err') }

	db.close() or { panic(err) }

	vweb.run(new_app(), http_port)
}

fn new_app() &App {
	mut app := &App{}

	return app
}

['/'; get]
pub fn (mut app App) ping() ?vweb.Result {
	return app.text('ping')
}
