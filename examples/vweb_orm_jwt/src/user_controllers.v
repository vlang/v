module main

import vweb
import json
import databases

['/user/:id/get'; get]
pub fn (mut app App) controller_get_user_by_id(id int) vweb.Result {
	response := app.service_get_user_by_id(id) or {
		app.set_status(400, '')
		return app.text('$err')
	}
	return app.json(response)
}

['/user/create'; post]
pub fn (mut app App) controller_create_user() vweb.Result {
	body := json.decode(User, app.req.data) or {
		app.set_status(400, '')
		return app.text('Failed to decode json, error: $err')
	}

	response := app.service_add_user(body.username, body.password) or {
		app.set_status(400, '')
		return app.text('error: $err')
	}

	return app.json(response)
}

['/user/get_all'; get]
pub fn (mut app App) controller_get_all_user() vweb.Result {
	token := app.get_header('token')

	if !auth_verify(token) {
		app.set_status(401, '')
		return app.text('Not valid token')
	}

	response := app.service_get_all_user() or {
		app.set_status(400, '')
		return app.text('$err')
	}
	return app.json(response)
}

['/user/get_by_username/:username'; get]
pub fn (mut app App) controller_get_by_username(username string) vweb.Result {
	response := app.service_get_by_username(username) or {
		app.set_status(400, '')
		return app.text('$err')
	}
	return app.json(response)
}

['/user/drop'; delete]
pub fn (mut app App) delete() vweb.Result {
	mut db := databases.create_db_connection() or {
		app.set_status(400, '')
		return app.text('$err')
	}

	defer {
		db.close()
	}

	sql db {
		drop table User
	}

	return app.text('Tabela deletada com sucesso')
}
