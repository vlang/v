// vtest build: present_sqlite3? // imports db.sqlite
import veb
import time
import net.http
import db.sqlite

const port = 13004

pub struct Context {
	veb.Context
pub mut:
	cid int
}

pub fn (mut ctx Context) before_request() {
	dump(@LOCATION)
	ctx.cid = 123
}

pub struct App {
pub mut:
	db      sqlite.DB
	started chan bool
}

pub fn (mut app App) before_accept_loop() {
	dump(@LOCATION)
	app.started <- true
}

struct Article {
	id    int
	title string
	text  string
}

fn test_veb_application_compiles() {
	spawn fn () {
		time.sleep(15 * time.second)
		exit(0)
	}()
	mut app := &App{}
	spawn veb.run_at[App, Context](mut app, port: port, family: .ip, timeout_in_seconds: 2)
	// app startup time
	_ := <-app.started
	res := http.fetch(url: 'http://127.0.0.1:${port}/get_cid')!
	assert res.status_code == 200
	assert res.body == '123'
	okres := http.fetch(url: 'http://127.0.0.1:${port}/ok')!
	assert okres.status_code == 200
	assert okres.body == '{"success":true,"result":123}'
}

@['/new_article'; post]
pub fn (mut app App) new_article(mut ctx Context) veb.Result {
	title := ctx.form['title']
	text := ctx.form['text']
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

	return ctx.redirect('/', typ: .see_other)
}

pub fn (mut app App) time(mut ctx Context) veb.Result {
	return ctx.text(time.now().format())
}

pub fn (mut app App) time_json(mut ctx Context) veb.Result {
	return ctx.json({
		'time': time.now().format()
	})
}

fn (mut app App) time_json_pretty(mut ctx Context) veb.Result {
	return ctx.json_pretty({
		'time': time.now().format()
	})
}

struct ApiSuccessResponse[T] {
	success bool
	result  T
}

fn (mut app App) json_success[T](mut ctx Context, result T) {
	response := ApiSuccessResponse[T]{
		success: true
		result:  result
	}

	ctx.json(response)
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
fn (mut app App) ok(mut ctx Context) veb.Result {
	return ctx.json(app.some_helper(ctx.cid))
}

// should compile, the route method itself is not generic
fn (mut app App) get_cid(mut ctx Context) veb.Result {
	return ctx.text(ctx.cid.str())
}

struct ExampleStruct {
	example int
}

fn (mut app App) request_raw_2(mut ctx Context) veb.Result {
	stuff := []ExampleStruct{}
	app.request_raw(mut ctx, stuff)
	return ctx.ok('')
}

// should compile, this is a helper method, not exposed as a route
fn (mut app App) request_raw(mut ctx Context, foo []ExampleStruct) {
	ctx.text('Hello world')
}
