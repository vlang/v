module main

// import api
import vweb
import databases
// import x.json2 as json
import json
import os

const (
	port = 8082
)

struct App {
	vweb.Context
}

pub fn (app App) before_request() {
	println('[web] before_request: ${app.req.method} ${app.req.url}')
}

fn main() {
	mut db := databases.create_db_connection() or { panic(err) }

	sql db {
		create table User
	} or { panic('error on create table: ${err}') }

	db.close() or { panic(err) }

	mut app := &App{}
	app.serve_static('/favicon.ico', 'src/assets/favicon.ico')
	// makes all static files available.
	app.mount_static_folder_at(os.resource_abs_path('.'), '/')

	vweb.run(app, port)
}

pub fn (mut app App) index() vweb.Result {
	show := true
	title := 'vweb app'
	hello := 'Hello world from vweb'
	numbers := [1, 2, 3]

	return $vweb.html()
}
