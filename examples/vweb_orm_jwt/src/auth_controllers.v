module main

import vweb
import json

['/auth/login'; post]
pub fn (mut app App) controller_auth() vweb.Result {
	body := json.decode(AuthRequestDto, app.req.data) or {
		app.set_status(400, '')
		return app.text('Failed to decode json, error: ${err}')
	}

	response := app.service_auth(body.username, body.password) or {
		app.set_status(400, '')
		return app.text('error: ${err}')
	}

	return app.json(response)
}
