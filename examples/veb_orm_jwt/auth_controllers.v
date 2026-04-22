module main

import veb
import json

@['/auth/login'; post]
pub fn (mut app App) controller_auth(mut ctx Context) veb.Result {
	body := json.decode(AuthRequestDto, ctx.req.data) or {
		ctx.res.set_status(.bad_request)
		return ctx.text('Failed to decode json, error: ${err}')
	}

	response := app.service_auth(body.username, body.password) or {
		ctx.res.set_status(.bad_request)
		return ctx.text('error: ${err}')
	}

	return ctx.json(response)
}
