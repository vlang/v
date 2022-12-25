module main

import vweb

['/controller/auth'; post]
pub fn (mut app App) controller_auth(username string, password string) vweb.Result {
	response := app.service_auth(username, password) or {
		app.set_status(400, '')
		return app.text('error: ${err}')
	}

	key := 'token'
	value := response
	// duration := time.Duration(2 * time.minute) // add 2 minutes
	// expire_date := time.now().add(duration)

	app.set_cookie(name: key, value: value)
	// app.set_cookie_with_expire_date(key, value, expire_date)

	return app.redirect('/users')
}
