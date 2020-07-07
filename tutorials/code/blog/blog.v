module main

import vweb
import time
import sqlite
import json

struct App {
pub mut:
	vweb vweb.Context
	db   sqlite.DB
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

pub fn (mut app App) init_once() {
	db := sqlite.connect('blog.db') or {
		panic(err)
	}
	app.db = db
}

pub fn (mut app App) init() {
}

pub fn (mut app App) new() vweb.Result {
	return $vweb.html()
}

pub fn (mut app App) new_article() vweb.Result {
	title := app.vweb.form['title']
	text := app.vweb.form['text']
	if title == '' || text == '' {
		app.vweb.text('Empty text/title')
		return vweb.Result{}
	}
	article := Article {
		title: title
		text: text
	}
	println(article)
	sql app.db {
		insert article into Article
	}
	return app.vweb.redirect('/')
}

pub fn (mut app App) articles() {
	articles := app.find_all_articles()
	app.vweb.json(json.encode(articles))
}

fn (mut app App) time() {
	app.vweb.text(time.now().format())
}
