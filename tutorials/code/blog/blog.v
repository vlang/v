module main

import (
	vweb
	time
	pg
)

struct App {
mut:
	vweb vweb.Context
	db pg.DB
}

fn main() {
	app := App{}
	vweb.run(mut app, 8080)
}

fn (app mut App) index_text() {
	app.vweb.text('Hello, world from vweb!')
}

/*
fn (app &App) index_html() {
	message := 'Hello, world from vweb!'
	$vweb.html()
}
*/

fn (app &App) index() {
	articles := app.find_all_articles()
	$vweb.html()
}

pub fn (app mut App) init() {
	db := pg.connect(pg.Config{
		host:   '127.0.0.1'
		dbname: 'blog'
		user:   'alex'
	}) or { panic(err) }
	app.db = db
}

fn (app mut App) time() {
	app.vweb.text(time.now().format())
}

