module main

import vweb
import time
import sqlite
import json

struct App {
	vweb.Context
mut:
	db sqlite.DB
}

fn main() {
	vweb.run<App>(8081)
}

/*
pub fn (mut app App) index_text() vweb.Result {
	app.text('Hello, world from vweb!')
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
	app.db = sqlite.connect(':memory:') or { panic(err) }
	app.db.exec('create table `Article` (id integer primary key, title text default "", text text default "")')
	app.db.exec('insert into Article (title, text) values ("Hello, world!", "V is great.")')
	app.db.exec('insert into Article (title, text) values ("Second post.", "Hm... what should I write about?")')
}

pub fn (mut app App) init() {
}

pub fn (mut app App) new() vweb.Result {
	return $vweb.html()
}

[post]
['/new_article']
pub fn (mut app App) new_article() vweb.Result {
	title := app.form['title']
	text := app.form['text']
	if title == '' || text == '' {
		app.text('Empty text/title')
		return vweb.Result{}
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
	app.json(json.encode(articles))
}

fn (mut app App) time() {
	app.text(time.now().format())
}
