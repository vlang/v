module main

import vweb
import time
import pg
import json

pub struct App {
mut:
	vweb vweb.Context
	db pg.DB
}

fn main() {
	vweb.run<App>(8080)
}

fn (mut app App) index_text() {
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

pub fn (mut app App) init() {
	db := pg.connect(pg.Config{
		host:   '127.0.0.1'
		dbname: 'blog'
		user:   'alex'
	}) or { panic(err) }
	app.db = db
}

pub fn (mut app App) new() {
	$vweb.html()
}

pub fn (mut app App) reset() {
}

pub fn (mut app App) new_article() {
	title := app.vweb.form['title']
	text := app.vweb.form['text']
	if title == '' || text == ''  {
		app.vweb.text('Empty text/titile')
		return
	}
	article := Article{
		title: title
		text: text
	}
	println(article)
	db := app.db
	db.insert(article)
	app.vweb.redirect('/article/')
}

pub fn (mut app App) articles() {
	articles := app.find_all_articles()
	app.vweb.json(json.encode(articles))
}

fn (mut app App) time() {
	app.vweb.text(time.now().format())
}
