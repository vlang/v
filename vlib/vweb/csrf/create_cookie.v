module csrf

import rand

// creating the value of the csrf-token
const chars = 'QWERTZUIOPASDFGHJKLYXCVBNMqwertzuiopasdfghjklyxcvbnm1234567890_-'

const cookie_key = '__Host-Csrf-Token'

// Public function that generates the token and sets the cookie
pub fn (mut app App) set_csrf_cookie() App {
	app.set_cookie(create_cookie().Cookie)
	return app
}

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

// create the cookie
fn create_cookie() CsrfCookie {
	return CsrfCookie{
		name: csrf.cookie_key
		value: generate()
		path: '/'
		max_age: 0
		secure: true
		http_only: true
	}
}

// optional set of http_only
fn (mut cookie CsrfCookie) http_only(b bool) CsrfCookie {
	cookie.http_only = b
	return cookie
}

// Public function that returns the token as a string
pub fn (mut app App) get_csrf_token() string {
	return app.get_cookie(csrf.cookie_key) or { '' }
}
