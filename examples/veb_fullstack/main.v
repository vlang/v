module main

import veb
import databases
import os

const port = 8082

pub struct Context {
	veb.Context
}

struct App {
	veb.StaticHandler
}

pub fn (app &App) before_request(mut ctx Context) {
	println('[veb] before_request: ${ctx.req.method} ${ctx.req.url}')
}

fn main() {
	mut db := databases.create_db_connection() or { panic(err) }

	sql db {
		create table User
		create table Product
	} or { panic('error on create table: ${err}') }

	db.close() or { panic(err) }

	mut app := &App{}
	app.serve_static('/favicon.ico', 'assets/favicon.ico') or { panic(err) }
	app.mount_static_folder_at(os.resource_abs_path('.'), '/') or { panic(err) }

	veb.run[App, Context](mut app, port)
}

pub fn (mut app App) index(mut ctx Context) veb.Result {
	title := 'veb app'

	return $veb.html()
}
