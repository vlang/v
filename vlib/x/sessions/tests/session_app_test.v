// vtest retry: 2
import net.http
import time
import x.sessions
import veb
import x.sessions.vweb2_middleware

const port = 13010
const localserver = 'http://localhost:${port}'
const exit_after = time.second * 10
const cookie_name = 'SESSION_ID'
const max_age = time.second * 2

pub struct User {
pub mut:
	name     string
	age      int
	verified bool
}

const default_user = User{
	name:     'casper'
	age:      21
	verified: false
}

pub struct Context {
	veb.Context
	sessions.CurrentSession[User]
}

pub struct App {
	veb.Middleware[Context]
pub mut:
	sessions &sessions.Sessions[User]
	started  chan bool
}

pub fn (mut app App) before_accept_loop() {
	app.started <- true
}

pub fn (app &App) session_data(mut ctx Context) veb.Result {
	return ctx.text('${ctx.session_data}')
}

pub fn (app &App) protected(mut ctx Context) veb.Result {
	if user := ctx.session_data {
		return ctx.json(user)
	} else {
		ctx.res.set_status(.unauthorized)
		return ctx.text('No session saved!')
	}
}

pub fn (mut app App) save_session(mut ctx Context) veb.Result {
	app.sessions.save(mut ctx, default_user) or { return ctx.server_error(err.msg()) }
	return ctx.ok('')
}

pub fn (mut app App) update_session(mut ctx Context) veb.Result {
	if mut user := ctx.session_data {
		user.age++
		app.sessions.save(mut ctx, user) or { return ctx.server_error(err.msg()) }
		// sessions module should also update the context
		if new_user := ctx.session_data {
			assert new_user == user, 'session data is not updated on the context'
		} else {
			assert false, 'session data should not be none'
		}
	}

	return ctx.ok('')
}

pub fn (mut app App) destroy_session(mut ctx Context) veb.Result {
	app.sessions.destroy(mut ctx) or { return ctx.server_error(err.msg()) }
	// sessions module should also update the context
	assert ctx.session_data == none

	return ctx.ok('')
}

fn testsuite_begin() {
	spawn fn () {
		time.sleep(exit_after)
		assert false, 'timeout reached!'
		exit(1)
	}()

	mut app := &App{
		sessions: &sessions.Sessions[User]{
			store:          sessions.MemoryStore[User]{}
			secret:         'secret'.bytes()
			cookie_options: sessions.CookieOptions{
				cookie_name: cookie_name
			}
			max_age:        max_age
		}
	}

	app.use(vweb2_middleware.create[User, Context](mut app.sessions))

	spawn veb.run_at[App, Context](mut app, port: port, timeout_in_seconds: 2)
	// app startup time
	_ := <-app.started
}

fn test_empty_session() {
	x := http.get('${localserver}/session_data')!

	assert x.body == 'Option(none)'
}

fn test_protected_without_session() {
	x := http.get('${localserver}/protected')!

	assert x.status() == .unauthorized
}

fn test_save_session() {
	sid := get_session_id()!
	x := make_request_with_session_id(.get, '/session_data', sid)!

	// cast to `?User` since, session_data is of type `?T`
	assert x.body == '${?User(default_user)}'
}

fn test_update_session() {
	sid := get_session_id()!

	mut x := make_request_with_session_id(.get, '/update_session', sid)!
	x = make_request_with_session_id(.get, '/session_data', sid)!

	mut updated_user := default_user
	updated_user.age++
	assert x.body == '${?User(updated_user)}'
}

fn test_destroy_session() {
	sid := get_session_id()!
	mut x := make_request_with_session_id(.get, '/session_data', sid)!
	assert x.body != 'Option(none)'

	x = make_request_with_session_id(.get, '/destroy_session', sid)!
	assert x.status() == .ok

	x = make_request_with_session_id(.get, '/session_data', sid)!
	assert x.body == 'Option(none)'
}

fn test_session_expired() {
	sid := get_session_id()!
	mut x := make_request_with_session_id(.get, '/session_data', sid)!
	assert x.body != 'Option(none)'

	time.sleep(max_age)

	x = make_request_with_session_id(.get, '/session_data', sid)!
	assert x.body == 'Option(none)'
}

// Utility

fn get_session_id() !string {
	x := http.get('${localserver}/save_session')!

	return x.cookies()[0].value
}

fn make_request_with_session_id(method http.Method, path string, sid string) !http.Response {
	return http.fetch(http.FetchConfig{
		url:     '${localserver}${path}'
		method:  method
		cookies: {
			cookie_name: sid
		}
	})
}
