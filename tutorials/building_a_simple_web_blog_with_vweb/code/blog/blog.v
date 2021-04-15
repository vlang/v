module main

import vweb
import time
import sqlite
import json

struct App {
	vweb.Context
mut:
	db      sqlite.DB [server_var]
	user_id string
}

fn main() {
	vweb.run<App>(8081)
}

/*
pub fn (mut app App) index_text() vweb.Result {
	app.vweb.text('Hello, world from vweb!')
	return vweb.Result{}
}

pub fn (app &App) index_html() vweb.Result {
	message := 'Hello, world from Vweb!'
	return $vweb.html()
}
*/
pub fn (app &App) index() vweb.Result {
	articles := app.find_all_articles()
	return $vweb.html()
}

pub fn (mut app App) init_server() {
	app.db = sqlite.connect('blog.db') or { panic(err) }
	app.db.create_table('article', [
		'id integer primary key',
		"title text default ''",
		"text text default ''",
	])
}

pub fn (mut app App) before_request() {
	app.user_id = app.get_cookie('id') or { '0' }
}

pub fn (mut app App) new() vweb.Result {
	return $vweb.html()
}

['/new_article'; post]
pub fn (mut app App) new_article() vweb.Result {
	title := app.form['title']
	text := app.form['text']
	if title == '' || text == '' {
		return app.text('Empty text/title')
	}
	article := Article{
		title: title
		text: text
	}
	println('posting article')
	println(article)
	sql app.db {
		insert article into Article
	}
	return app.redirect('/')
}

pub fn (mut app App) articles() {
	articles := app.find_all_articles()
	x := json.encode(articles)
	app.json(x)
}

fn (mut app App) time() {
	app.text(time.now().format())
}
