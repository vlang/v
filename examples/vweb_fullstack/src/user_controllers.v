module main

import vweb
import encoding.base64
import json

@['/controller/users'; get]
pub fn (mut app App) controller_get_all_user() vweb.Result {
	// token := app.get_cookie('token') or { '' }
	token := app.req.header.get_custom('token') or { '' }

	if !auth_verify(token) {
		app.set_status(401, '')
		return app.text('Not valid token')
	}

	response := app.service_get_all_user() or {
		app.set_status(400, '')
		return app.text('${err}')
	}
	return app.json(response)
}

@['/controller/user'; get]
pub fn (mut app App) controller_get_user() vweb.Result {
	// token := app.get_cookie('token') or { '' }
	token := app.req.header.get_custom('token') or { '' }

	if !auth_verify(token) {
		app.set_status(401, '')
		return app.text('Not valid token')
	}

	jwt_payload_stringify := base64.url_decode_str(token.split('.')[1])

	jwt_payload := json.decode(JwtPayload, jwt_payload_stringify) or {
		app.set_status(501, '')
		return app.text('jwt decode error')
	}

	user_id := jwt_payload.sub

	response := app.service_get_user(user_id.int()) or {
		app.set_status(400, '')
		return app.text('${err}')
	}
	return app.json(response)
}

@['/controller/user/create'; post]
pub fn (mut app App) controller_create_user(username string, password string) vweb.Result {
	if username == '' {
		app.set_status(400, '')
		return app.text('username cannot be empty')
	}
	if password == '' {
		app.set_status(400, '')
		return app.text('password cannot be empty')
	}
	app.service_add_user(username, password) or {
		app.set_status(400, '')
		return app.text('error: ${err}')
	}
	app.set_status(201, '')
	return app.text('User created successfully')
}
