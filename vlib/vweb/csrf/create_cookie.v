module csrf

import rand

const chars = 'QWERTZUIOPASDFGHJKLYXCVBNMqwertzuiopasdfghjklyxcvbnm1234567890_-'

const cookie_key = '__Host-Csrf-Token'

// set_csrf_cookie - generates a CSRF-Token and sets the CSRF-Cookie. It is possible to set the HttpOnly-status of the cookie to false by adding an argument of the HttpOnly-struct like this:
// `app.set_csrf_cookie(csrf.HttpOnly{false})`
// If no argument is set, http_only will be set to `true`by default.
pub fn (mut app App) set_csrf_cookie(h ...HttpOnly) App {
	mut http_only := true
	if h.len > 0 {
		http_only = h[0].http_only
	}
	cookie := create_cookie(http_only)
	app = App{app.Context, cookie.value}
	app.set_cookie(cookie)
	return app
}

// generate - generates the CSRF-Token
fn generate() string {
	mut out := ''
	for _ in 0 .. 42 {
		i := rand.intn(csrf.chars.len_utf8()) or {
			panic('Error while trying to generate Csrf-Token: $err')
		}
		out = out + csrf.chars[i..i + 1]
	}
	return out
}

// create_cookie - creates the cookie
fn create_cookie(h bool) CsrfCookie {
	return CsrfCookie{
		name: csrf.cookie_key
		value: generate()
		path: '/'
		max_age: 0
		secure: true
		http_only: h
	}
}

// get_csrf_token - returns the CSRF-Token that has been set. Make sure that you set one by using `set_csrf_cookie()`. If it's value is empty or no cookie has been generated, the function will throw an error.
pub fn (mut app App) get_csrf_token() ?string {
	if app.csrf_cookie_value != '' {
		return app.csrf_cookie_value
	} else {
		return IError(CsrfError{
			m: 'The CSRF-Token-Value is empty. Please check if you have setted a cookie!'
		})
	}
}
