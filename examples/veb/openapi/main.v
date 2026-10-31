module main

import os
import veb

pub struct Context {
	veb.Context
}

pub struct App {
	veb.Controller
pub mut:
	document veb.OpenApiDocument
}

pub struct Users {}

pub struct UserInput {
pub:
	name  string @[description: 'Public display name'; example: '"Ada"']
	email ?string
}

pub struct User {
pub:
	id   int
	name string
}

// specification serves the generated OpenAPI document.
@['/openapi.json'; get]
pub fn (app &App) specification(mut ctx Context) !veb.Result {
	return app.document.serve(mut ctx.Context, true)!
}

// create binds and returns a user.
@['/'; post]
pub fn (app &Users) create(mut ctx Context) veb.Result {
	input := ctx.bind[UserInput]() or { return ctx.request_error('Invalid user input') }
	ctx.res.set_status(.created)
	return ctx.json(User{ id: 1, name: input.name })
}

// read returns a user by identifier.
@['/:id'; get]
pub fn (app &Users) read(mut ctx Context, id int) veb.Result {
	return ctx.json(User{ id: id, name: 'Ada' })
}

fn main() {
	mut app := &App{}
	mut users := &Users{}
	app.register_controller[Users, Context]('/users', mut users)!
	mut doc := veb.new_openapi(
		title:       'Users API'
		description: 'A typed, controller-aware API'
		version:     '1.0.0'
	)!
	doc.add_app[App, Context](app)!
	doc.operation('/users', .post, summary: 'Create a user', tags: ['users'])!
	doc.request[UserInput]('/users', .post)!
	doc.response[User]('/users', .post, status: 201)!
	doc.response[User]('/users/:id', .get)!
	if '--write' in os.args {
		doc.write('openapi.json', true)!
		return
	}
	app.document = doc
	veb.run[App, Context](mut app, 8080)
}
