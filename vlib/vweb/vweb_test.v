module main

import vweb
import time
import sqlite

struct App {
	vweb.Context
pub mut:
	db      sqlite.DB [vweb_global]
	user_id string
}

struct Article {
	id    int
	title string
	text  string
}

fn test_a_vweb_application_compiles() ? {
	go fn () {
		time.sleep(2 * time.second)
		exit(0)
	}()
	mut router := vweb.new(&App{}) ?
	router.listen(':18081') ?
}

pub fn (mut app App) before_request() {
	app.user_id = app.get_cookie('id') or { '0' }
}

['/new_article'; post]
pub fn (mut app App) new_article() vweb.Result {
	title := app.form['title']
	text := app.form['text']
	if title == '' || text == '' {
		return app.text(.ok, 'Empty text/title')
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

fn (mut app App) time() {
	app.text(.ok, time.now().format())
}
