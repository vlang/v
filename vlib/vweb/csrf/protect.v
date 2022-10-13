module csrf

import net.http

// csrf_protect - protects a handler-function against CSRF. Should be set at the beginning of the handler-function.
pub fn (mut app App) csrf_protect() CheckedApp {
	req_cookies := app.req.cookies.clone()
	app_csrf_cookie_str := app.get_cookie(cookie_key) or {
		// Do not return normally!! No Csrf-Token was set!
		app.set_status(403, '')
		return app.text('Error 403 - Forbidden')
	}

	if cookie_key in req_cookies && req_cookies[cookie_key] == app_csrf_cookie_str {
		// Csrf-Check OK - return app as normal in order to handle request normally
		return app
	} else if app.check_headers(app_csrf_cookie_str) {
		// Csrf-Check OK - return app as normal in order to handle request normally
		return app
	} else {
		// Do not return normally!! The client has not passed the Csrf-Check!!
		app.set_status(403, '')
		return app.text('Error 403 - Forbidden')
	}
}

// check_headers - checks if there is a CSRF-Token that was sent with the headers of a request
fn (app App) check_headers(app_csrf_cookie_str string) bool {
	token := app.req.header.get_custom('Csrf-Token', http.HeaderQueryConfig{true}) or {
		return false
	}
	if token == app_csrf_cookie_str {
		return true
	} else {
		return false
	}
}
