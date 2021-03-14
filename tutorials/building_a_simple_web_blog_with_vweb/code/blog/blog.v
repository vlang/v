module main

import vweb
import time
import sqlite
import json

struct App {
mut:
	db sqlite.DB
}

fn main() {
	mut app := App{
		db: sqlite.connect('blog.db') or { panic(err) }
	}
	app.db.create_table('article', [
		'id integer primary key',
		"title text default ''",
		"text text default ''",
	])

	vweb.run_app<App>(mut app, port: 8081)
}

pub fn (mut app App) new(mut c vweb.Context) vweb.Result {
	return $vweb.html()
}

['/new_article'; post]
pub fn (mut app App) new_article(mut c vweb.Context) vweb.Result {
	title := c.form['title']
	text := c.form['text']
	if title == '' || text == '' {
		return c.text('Empty text/title')
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
	return c.redirect('/')
}

pub fn (mut app App) articles(mut c vweb.Context) {
	articles := app.find_all_articles()
	x := json.encode(articles)
	c.json(x)
}

fn (mut app App) time(mut c vweb.Context) {
	c.text(time.now().format())
}
