module main

import veb
import time
import db.sqlite
import json

// Context struct must embed veb.Context
pub struct Context {
	veb.Context
pub mut:
	user_id string
}

// App struct for shared data
pub struct App {
pub mut:
	db sqlite.DB
}

// Main function
fn main() {
	mut app := &App{
		db: sqlite.connect('blog.db') or { panic(err) }
	}
	sql app.db {
		create table Article
	}!
	// Use veb.run with App and Context types
	veb.run[App, Context](mut app, 8081)
}

/*
// Middleware to run before each request
pub fn (mut ctx Context) before_request() bool {
	ctx.user_id = ctx.get_cookie('id') or { '0' }
	return true
}
*/

// Index endpoint
@['/index']
pub fn (app &App) index(mut ctx Context) veb.Result {
	articles := app.find_all_articles()
	return $veb.html()
}

// New article form endpoint
@['/new']
pub fn (app &App) new(mut ctx Context) veb.Result {
	return $veb.html()
}

// Create new article endpoint
@['/new_article'; post]
pub fn (app &App) new_article(mut ctx Context) veb.Result {
	title := ctx.form['title'] or { '' }
	text := ctx.form['text'] or { '' }

	if title == '' || text == '' {
		return ctx.text('Empty text/title')
	}

	article := Article{
		title: title
		text:  text
	}
	println('posting article')
	println(article)

	sql app.db {
		insert article into Article
	} or {}

	return ctx.redirect('/')
}

// Get all articles endpoint
@['/articles'; get]
pub fn (app &App) articles(mut ctx Context) veb.Result {
	articles := app.find_all_articles()
	json_result := json.encode(articles)
	return ctx.json(json_result)
}

// Time endpoint
@['/time']
pub fn (app &App) time(mut ctx Context) veb.Result {
	return ctx.text(time.now().format())
}
