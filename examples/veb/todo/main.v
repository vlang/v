// Simple TODO app using veb
// Run from this directory with `v run main.v`
// You can also enable vebs livereload feature with
// `v watch -d veb_livereload run main.v`
module main

import veb
import db.sqlite
import os
import time

struct Todo {
pub mut:
	// `id` is the primary field. The attribute `sql: serial` acts like AUTO INCREMENT in sql.
	// You can use this attribute if you want a unique id for each row.
	id        int @[primary; sql: serial]
	name      string
	completed bool
	created   time.Time
	updated   time.Time
}

pub struct Context {
	veb.Context
pub mut:
	// we can use this field to check whether we just created a TODO in our html templates
	created_todo bool
}

pub struct App {
	veb.StaticHandler
pub:
	// we can access the SQLITE database directly via `app.db`
	db sqlite.DB
}

// This method will only handle GET requests to the index page
@[get]
pub fn (app &App) index(mut ctx Context) veb.Result {
	todos := sql app.db {
		select from Todo
	} or { return ctx.server_error('could not fetch todos from database!') }
	return $veb.html()
}

// This method will only handle POST requests to the index page
@['/'; post]
pub fn (app &App) create_todo(mut ctx Context, name string) veb.Result {
	// We can receive form input fields as arguments in a route!
	// we could also access the name field by doing `name := ctx.form['name']`

	// validate input field
	if name == '' {
		// set a form error
		ctx.form_error = 'You must fill in all the fields!'
		// send a HTTP 400 response code indicating that the form fields are incorrect
		ctx.res.set_status(.bad_request)
		// render the home page
		return app.index(mut ctx)
	}

	// create a new todo
	todo := Todo{
		name:    name
		created: time.now()
		updated: time.now()
	}

	// insert the todo into our database
	sql app.db {
		insert todo into Todo
	} or { return ctx.server_error('could not insert a new TODO in the database') }

	ctx.created_todo = true

	// render the home page
	return app.index(mut ctx)
}

@['/todo/:id/complete'; post]
pub fn (app &App) complete_todo(mut ctx Context, id int) veb.Result {
	// first check if there exist a TODO record with `id`
	todos := sql app.db {
		select from Todo where id == id
	} or { return ctx.server_error("could not fetch TODO's") }
	if todos.len == 0 {
		// return HTTP 404 when the TODO does not exist
		ctx.res.set_status(.not_found)
		return ctx.text('There is no TODO item with id=${id}')
	}

	// update the TODO field
	sql app.db {
		update Todo set completed = true, updated = time.now() where id == id
	} or { return ctx.server_error('could not update TODO') }

	// redirect client to the home page and tell the browser to sent a GET request
	return ctx.redirect('/', typ: .see_other)
}

@['/todo/:id/delete'; post]
pub fn (app &App) delete_todo(mut ctx Context, id int) veb.Result {
	// first check if there exist a TODO record with `id`
	todos := sql app.db {
		select from Todo where id == id
	} or { return ctx.server_error("could not fetch TODO's") }
	if todos.len == 0 {
		// return HTTP 404 when the TODO does not exist
		ctx.res.set_status(.not_found)
		return ctx.text('There is no TODO item with id=${id}')
	}

	// prevent hackers from deleting TODO's that are not completed ;)
	to_be_deleted := todos[0]
	if !to_be_deleted.completed {
		return ctx.request_error('You must first complete a TODO before you can delete it!')
	}

	// delete the todo
	sql app.db {
		delete from Todo where id == id
	} or { return ctx.server_error('could not delete TODO') }

	// redirect client to the home page and tell the browser to sent a GET request
	return ctx.redirect('/', typ: .see_other)
}

fn main() {
	os.chdir(os.dir(@FILE))!
	// create a new App instance with a connection to the database
	mut app := &App{
		db: sqlite.connect('todo.db')!
	}

	// mount the assets folder at `/assets/`
	app.handle_static('assets', false)!

	// create the table in our database, if it doesn't exist
	sql app.db {
		create table Todo
	}!

	// start our app at port 8080
	veb.run[App, Context](mut app, 8080)
}
