module csrf

import net.http

pub fn (mut app App) csrf_protect() CheckedApp {
	req_cookies:=app.req.cookies.clone()
	app_csrf_cookie_str:=app.get_cookie(cookie_key) or {
		app.set_status(403, "")
		return app.text("Error 403 - Forbidden")
	}

	if cookie_key in req_cookies && req_cookies[cookie_key] == app_csrf_cookie_str {
		//Csrf-Check OK - return app as normal in order to handel request normally
		return app
	} else if app.check_headers(app_csrf_cookie_str) {
		//Csrf-Check OK - return app as normal in order to handel request normally
		return app
	} else {
		//TO-DO: Do not return normally!! The client has not passed the Csrf-Check!!
		app.set_status(403, "")
		return app.text("Error 403 - Forbidden")
	}

}

fn (app App) check_headers(app_csrf_cookie_str string) bool {
	token := app.req.header.get_custom("Csrf-Token", http.HeaderQueryConfig{true}) or {return false}
	if token == app_csrf_cookie_str {
		return true
	} else {
		return false
	}
}