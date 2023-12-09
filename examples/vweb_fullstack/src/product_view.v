module main

import vweb

@['/products'; get]
pub fn (mut app App) products() !vweb.Result {
	token := app.get_cookie('token') or {
		app.set_status(400, '')
		return app.text('${err}')
	}

	user := get_user(token) or {
		app.set_status(400, '')
		return app.text('Failed to fetch data from the server. Error: ${err}')
	}

	return $vweb.html()
}
