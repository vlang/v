// vtest build: present_sqlite3? // imports db.sqlite
module main

import vweb
import time
import db.sqlite

struct App {
	vweb.Context
pub mut:
	db      sqlite.DB
	user_id string
}

struct Article {
	id    int
	title string
	text  string
}

fn test_a_vweb_application_compiles() {
	vweb.run(&App{}, 18081)
}

pub fn (mut app App) before_accept_loop() {
	exit(0)
}

pub fn (mut app App) before_request() {
	app.user_id = app.get_cookie('id') or { '0' }
}

@['/new_article'; post]
pub fn (mut app App) new_article() vweb.Result {
	title := app.form['title']
	text := app.form['text']
	if title == '' || text == '' {
		return app.text('Empty text/title')
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

	return app.redirect('/')
}

fn (mut app App) time() vweb.Result {
	return app.text(time.now().format())
}

fn (mut app App) time_json() vweb.Result {
	return app.json({
		'time': time.now().format()
	})
}

fn (mut app App) time_json_pretty() vweb.Result {
	return app.json_pretty({
		'time': time.now().format()
	})
}

struct ApiSuccessResponse[T] {
	success bool
	result  T
}

fn (mut app App) json_success[T](result T) vweb.Result {
	response := ApiSuccessResponse[T]{
		success: true
		result:  result
	}

	return app.json(response)
}

// should compile, this is a helper method, not exposed as a route
fn (mut app App) some_helper[T](result T) ApiSuccessResponse[T] {
	response := ApiSuccessResponse[T]{
		success: true
		result:  result
	}
	return response
}

// should compile, the route method itself is not generic
fn (mut app App) ok() vweb.Result {
	return app.json(app.some_helper(123))
}

struct ExampleStruct {
	example int
}

fn (mut app App) request_raw_2() vweb.Result {
	stuff := []ExampleStruct{}
	return app.request_raw(stuff)
}

// should compile, this is a helper method, not exposed as a route
fn (mut app App) request_raw(foo []ExampleStruct) vweb.Result {
	return app.text('Hello world')
}
