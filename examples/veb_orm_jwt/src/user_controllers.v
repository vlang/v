module main

import veb
import json
import databases

@['/user/:id/get'; get]
pub fn (mut app App) controller_get_user_by_id(mut ctx Context, id int) veb.Result {
	response := app.service_get_user_by_id(id) or {
		ctx.res.set_status(.bad_request)
		return ctx.text('${err}')
	}
	return ctx.json(response)
}

@['/user/create'; post]
pub fn (mut app App) controller_create_user(mut ctx Context) veb.Result {
	body := json.decode(User, ctx.req.data) or {
		ctx.res.set_status(.bad_request)
		return ctx.text('Failed to decode json, error: ${err}')
	}

	response := app.service_add_user(body.username, body.password) or {
		ctx.res.set_status(.bad_request)
		return ctx.text('error: ${err}')
	}

	return ctx.json(response)
}

@['/user/get_all'; get]
pub fn (mut app App) controller_get_all_user(mut ctx Context) veb.Result {
	token := ctx.get_header('token')

	if !auth_verify(token) {
		ctx.res.set_status(.unauthorized)
		return ctx.text('Not valid token')
	}

	response := app.service_get_all_user() or {
		ctx.res.set_status(.bad_request)
		return ctx.text('${err}')
	}
	return ctx.json(response)
}

@['/user/get_by_username/:username'; get]
pub fn (mut app App) controller_get_by_username(mut ctx Context, username string) veb.Result {
	response := app.service_get_by_username(username) or {
		ctx.res.set_status(.bad_request)
		return ctx.text('${err}')
	}
	return ctx.json(response)
}

@['/user/drop'; delete]
pub fn (mut app App) delete(mut ctx Context) veb.Result {
	mut db := databases.create_db_connection() or {
		ctx.res.set_status(.bad_request)
		return ctx.text('${err}')
	}

	defer {
		db.close() or { panic(err) }
	}

	sql db {
		drop table User
	} or { panic(err) }

	return ctx.text('Successfully deleted table')
}
