module sessions

import crypto.sha256
import crypto.hmac
import encoding.base64
import net.http
import rand
import time

const session_id_length = 32

// new_session_id creates and returns a random session id and its signed version.
// You can directly use the signed version as a cookie value
pub fn new_session_id(secret []u8) (string, string) {
	sid := rand.hex(session_id_length)

	hashed := hmac.new(secret, sid.bytes(), sha256.sum, sha256.block_size)

	// separate session id and hmac with a `.`
	return sid, '${sid}.${base64.url_encode(hashed)}'
}

// verify_session_id verifies the signed session id with `secret`.
// This function returns the session id and if the session id is valid.
pub fn verify_session_id(raw_sid string, secret []u8) (string, bool) {
	parts := raw_sid.split('.')
	if parts.len != 2 {
		return '', false
	}

	sid := parts[0]
	actual_hmac := base64.url_decode(parts[1])

	new_hmac := hmac.new(secret, sid.bytes(), sha256.sum, sha256.block_size)
	// use `hmac.equal` to prevent leaking timing information
	return sid, hmac.equal(actual_hmac, new_hmac)
}

// CurrentSession contains the session data during a request.
// If you use x.vweb you could embed it on your Context struct to have easy access to the session id and data.
// Example:
// ```v
// pub struct Context {
//     vweb.Context
//     sessions.CurrentSessions[User]
// }
// ```
pub struct CurrentSession[T] {
pub mut:
	session_id   string
	session_data ?T
}

// CookieOptions contains the default settings for the cookie created in the `Sessions` struct.
pub struct CookieOptions {
pub:
	cookie_name string = 'sid'
	domain      string
	http_only   bool          = true
	path        string        = '/'
	same_site   http.SameSite = .same_site_strict_mode
	secure      bool
}

// Sessions can be used to easily integrate sessions with x.vweb.
// This struct contains the store that holds all session data it also provides
// an easy way to manage sessions in your vweb app.
// Example:
// ```v
// pub struct App {
// pub mut:
//     sessions &sessions.Sessions[User]
// }
// ```
@[heap]
pub struct Sessions[T] {
pub:
	secret         []u8 @[required]
	cookie_options CookieOptions
	// max age of session data and id, default is 30 days
	max_age time.Duration = time.hour * 24 * 30
	// set to true if you want to create a session if there isn't any data stored yet.
	// Also called pre-sessions
	save_uninitialized bool
pub mut:
	store Store[T] @[required]
}

// set_session_id generates a new session id and set a Set-Cookie header on the response.
pub fn (mut s Sessions[T]) set_session_id[X](mut ctx X) string {
	sid, signed := new_session_id(s.secret)
	ctx.CurrentSession.session_id = sid

	ctx.set_cookie(http.Cookie{
		value:     signed
		max_age:   s.max_age
		domain:    s.cookie_options.domain
		http_only: s.cookie_options.http_only
		name:      s.cookie_options.cookie_name
		path:      s.cookie_options.path
		same_site: s.cookie_options.same_site
		secure:    s.cookie_options.secure
	})
	// indicate that the response should not be cached: we don't want the session id cookie
	// to be cached by the browser, or any other agent
	// https://datatracker.ietf.org/doc/html/rfc7234#section-5.2.1.4
	ctx.res.header.add(.cache_control, 'no-cache="Set-Cookie"')

	return sid
}

// validate_session validates the current session, returns the session id and the validation status.
pub fn (mut s Sessions[T]) validate_session[X](ctx X) (string, bool) {
	cookie := ctx.get_cookie(s.cookie_options.cookie_name) or { return '', false }

	return verify_session_id(cookie, s.secret)
}

// get the data associated with the current session, if it exists.
pub fn (mut s Sessions[T]) get[X](ctx X) !T {
	sid := s.get_session_id(ctx) or { return error('cannot find session id') }
	return s.store.get(sid, s.max_age)!
}

// destroy the data for the current session.
pub fn (mut s Sessions[T]) destroy[X](mut ctx X) ! {
	if sid := s.get_session_id(ctx) {
		s.store.destroy(sid)!
		ctx.session_data = none
	}
}

// logout destroys the data for the current session and removes the session id Cookie.
pub fn (mut s Sessions[T]) logout[X](mut ctx X) ! {
	s.destroy(mut ctx)!
	ctx.set_cookie(http.Cookie{
		name:    s.cookie_options.cookie_name
		value:   ''
		expires: time.unix(0)
	})
}

// save `data` for the current session.
pub fn (mut s Sessions[T]) save[X](mut ctx X, data T) ! {
	if sid := s.get_session_id(ctx) {
		s.store.set(sid, data)!
		ctx.CurrentSession.session_data = data
	} else {
		if s.save_uninitialized == false {
			// no valid session id, but the user only wants to create a session
			// when data is saved. So we create the session here
			sid := s.set_session_id(mut ctx)
			s.store.set(sid, data)!
			ctx.CurrentSession.session_data = data
		}
		eprintln('[vweb.sessions] error: trying to save data without a valid session!')
	}
}

// resave saves `data` for the current session and reset the session id.
// You should use this function when the authentication or authorization status changes
// e.g. when a user signs in or switches between accounts/permissions.
// This function also destroys the data associated to the old session id.
pub fn (mut s Sessions[T]) resave[X](mut ctx X, data T) ! {
	if sid := s.get_session_id(ctx) {
		s.store.destroy(sid)!
	}

	s.save(mut ctx, data)
}

// get_session_id retrieves the current session id, if it is set.
pub fn (s &Sessions[T]) get_session_id[X](ctx X) ?string {
	// first check session id from `ctx`
	sid_from_ctx := ctx.CurrentSession.session_id
	if sid_from_ctx != '' {
		return sid_from_ctx
	} else if cookie := ctx.get_cookie(s.cookie_options.cookie_name) {
		// check request headers for the session_id cookie
		a := cookie.split('.')
		return a[0]
	} else {
		// check the Set-Cookie headers on the response for a session id
		for cookie in ctx.res.cookies() {
			if cookie.name == s.cookie_options.cookie_name {
				return cookie.value
			}
		}

		// No session id is set
		return none
	}
}
