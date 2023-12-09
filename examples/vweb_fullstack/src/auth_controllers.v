module main

import vweb

@['/controller/auth'; post]
pub fn (mut app App) controller_auth(username string, password string) vweb.Result {
	response := app.service_auth(username, password) or {
		app.set_status(400, '')
		return app.text('error: ${err}')
	}

	return app.json(response)
}
