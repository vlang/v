module main

import veb
import databases

const http_port = 8081

pub struct Context {
	veb.Context
}

struct App {
	veb.StaticHandler
}

pub fn (app &App) before_request(mut ctx Context) {
	println('[veb] ${ctx.req.method} ${ctx.req.url}')
}

fn main() {
	mut db := databases.create_db_connection() or { panic(err) }

	sql db {
		create table User
	} or { panic('error on create table: ${err}') }

	db.close() or { panic(err) }

	mut app := &App{}
	veb.run[App, Context](mut app, http_port)
}

@['/'; get]
pub fn (mut app App) ping(mut ctx Context) veb.Result {
	return ctx.text('ping')
}
