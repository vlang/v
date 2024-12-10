import time
import net.http
import net.html
import vweb
import vweb.csrf
import os

const sport = 12385
const localserver = '127.0.0.1:${sport}'
const exit_after_time = 12000 // milliseconds

const session_id_cookie_name = 'session_id'
const csrf_config = &csrf.CsrfConfig{
	secret:         'my-256bit-secret'
	allowed_hosts:  ['*']
	session_cookie: session_id_cookie_name
}

const allowed_origin = 'example.com'
const csrf_config_origin = &csrf.CsrfConfig{
	secret:         'my-256bit-secret'
	allowed_hosts:  [allowed_origin]
	session_cookie: session_id_cookie_name
}

// 			Test CSRF functions
// =====================================

fn test_set_token() {
	mut ctx := vweb.Context{}

	token := csrf.set_token(mut ctx, csrf_config)

	cookie := ctx.header.get(.set_cookie) or { '' }
	assert cookie.len != 0
	assert cookie.starts_with('${csrf_config.cookie_name}=')
}

fn test_protect() {
	mut ctx := vweb.Context{}

	token := csrf.set_token(mut ctx, csrf_config)

	mut cookie := ctx.header.get(.set_cookie) or { '' }
	// get cookie value from "name=value;"
	cookie = cookie.split(' ')[0].all_after('=').replace(';', '')

	form := {
		csrf_config.token_name: token
	}
	ctx = vweb.Context{
		form: form
		req:  http.Request{
			method: .post
		}
	}
	ctx.req.add_cookie(name: csrf_config.cookie_name, value: cookie)
	valid := csrf.protect(mut ctx, csrf_config)

	assert valid == true
}

fn test_timeout() {
	timeout := 1
	short_time_config := &csrf.CsrfConfig{
		secret:         'my-256bit-secret'
		allowed_hosts:  ['*']
		session_cookie: session_id_cookie_name
		max_age:        timeout
	}

	mut ctx := vweb.Context{}

	token := csrf.set_token(mut ctx, short_time_config)

	// after 2 seconds the cookie should expire (maxage)
	time.sleep(2 * time.second)
	mut cookie := ctx.header.get(.set_cookie) or { '' }
	// get cookie value from "name=value;"
	cookie = cookie.split(' ')[0].all_after('=').replace(';', '')

	form := {
		short_time_config.token_name: token
	}
	ctx = vweb.Context{
		form: form
		req:  http.Request{
			method: .post
		}
	}
	ctx.req.add_cookie(name: short_time_config.cookie_name, value: cookie)

	valid := csrf.protect(mut ctx, short_time_config)

	assert valid == false
}

fn test_valid_origin() {
	// valid because both Origin and Referer headers are present
	token, cookie := get_token_cookie('')
	mut req := http.Request{
		method: .post
	}
	req.add_cookie(name: csrf_config.cookie_name, value: cookie)
	req.add_header(.origin, 'http://${allowed_origin}')
	req.add_header(.referer, 'http://${allowed_origin}/test')
	mut ctx := vweb.Context{
		form: {
			csrf_config.token_name: token
		}
		req:  req
	}

	mut valid := csrf.protect(mut ctx, csrf_config_origin)
	assert valid == true
}

fn test_invalid_origin() {
	// invalid because either the Origin, Referer or neither are present
	token, cookie := get_token_cookie('')
	mut req := http.Request{
		method: .post
	}
	req.add_cookie(name: csrf_config.cookie_name, value: cookie)
	req.add_header(.origin, 'http://${allowed_origin}')
	mut ctx := vweb.Context{
		form: {
			csrf_config.token_name: token
		}
		req:  req
	}

	mut valid := csrf.protect(mut ctx, csrf_config_origin)
	assert valid == false

	req = http.Request{
		method: .post
	}
	req.add_cookie(name: csrf_config.cookie_name, value: cookie)
	req.add_header(.referer, 'http://${allowed_origin}/test')
	ctx = vweb.Context{
		form: {
			csrf_config.token_name: token
		}
		req:  req
	}

	valid = csrf.protect(mut ctx, csrf_config_origin)
	assert valid == false

	req = http.Request{
		method: .post
	}
	req.add_cookie(name: csrf_config.cookie_name, value: cookie)
	ctx = vweb.Context{
		form: {
			csrf_config.token_name: token
		}
		req:  req
	}

	valid = csrf.protect(mut ctx, csrf_config_origin)
	assert valid == false
}

// 			Testing App
// ================================

struct App {
	vweb.Context
pub mut:
	csrf        csrf.CsrfApp @[vweb_global]
	middlewares map[string][]vweb.Middleware
}

pub fn (mut app App) index() vweb.Result {
	app.csrf.set_token(mut app.Context)

	return app.html('<form action="/auth" method="post">
    <input type="hidden" name="${app.csrf.token_name}" value="${app.csrf.token}"/>
    <label for="password">Your password:</label>
    <input type="text"  id="password" name="password" placeholder="Your password" />
</form>')
}

@[post]
pub fn (mut app App) auth() vweb.Result {
	app.csrf.protect(mut app.Context)

	return app.ok('authenticated')
}

pub fn (mut app App) middleware_index() vweb.Result {
	csrftoken := csrf.set_token(mut app.Context, csrf_config)

	return app.html('<form action="/auth" method="post">
    <input type="hidden" name="${csrf_config.token_name}" value="${csrftoken}"/>
    <label for="password">Your password:</label>
    <input type="text"  id="password" name="password" placeholder="Your password" />
</form>')
}

@[post]
pub fn (mut app App) middleware_auth() vweb.Result {
	return app.ok('middleware authenticated')
}

// 			App cleanup function
// ======================================

pub fn (mut app App) shutdown() vweb.Result {
	spawn app.exit_gracefully()
	return app.ok('good bye')
}

fn (mut app App) exit_gracefully() {
	eprintln('>> webserver: exit_gracefully')
	time.sleep(100 * time.millisecond)
	exit(0)
}

fn exit_after_timeout[T](mut app T, timeout_in_ms int) {
	time.sleep(timeout_in_ms * time.millisecond)
	eprintln('>> webserver: pid: ${os.getpid()}, exiting ...')
	app.shutdown()

	eprintln('App timed out!')
	assert true == false
}

// 			Tests for the App
// ======================================

fn test_run_app_in_background() {
	mut app := &App{
		csrf:        csrf.CsrfApp{
			secret:         'my-256bit-secret'
			allowed_hosts:  [allowed_origin]
			session_cookie: session_id_cookie_name
		}
		middlewares: {
			'/middleware_auth': [csrf.middleware(csrf_config)]
		}
	}
	spawn vweb.run_at(app, port: sport, family: .ip)
	spawn exit_after_timeout(mut app, exit_after_time)

	time.sleep(500 * time.millisecond)
}

fn test_token_with_app() {
	res := http.get('http://${localserver}/') or { panic(err) }

	mut doc := html.parse(res.body)
	inputs := doc.get_tags_by_attribute_value('type', 'hidden')
	assert inputs.len == 1
	assert csrf_config.token_name == inputs[0].attributes['name']
}

fn test_token_with_middleware() {
	res := http.get('http://${localserver}/middleware_index') or { panic(err) }

	mut doc := html.parse(res.body)
	inputs := doc.get_tags_by_attribute_value('type', 'hidden')
	assert inputs.len == 1
	assert csrf_config.token_name == inputs[0].attributes['name']
}

// utility function to check whether the route at `path` is protected against csrf
fn protect_route_util(path string) {
	mut req := http.Request{
		method: .post
		url:    'http://${localserver}/${path}'
	}
	mut res := req.do() or { panic(err) }
	assert res.status() == .forbidden

	// A valid request with CSRF protection should have a cookie session id,
	// csrftoken in `app.form` and the hmac of that token in a cookie
	session_id := 'user_session_id'
	token, cookie := get_token_cookie(session_id)

	header := http.new_header_from_map({
		http.CommonHeader.origin:  'http://${allowed_origin}'
		http.CommonHeader.referer: 'http://${allowed_origin}/route'
	})

	formdata := http.url_encode_form_data({
		csrf_config.token_name: token
	})

	// session id is altered: test if session hijacking is possible
	// if the session id the csrftoken changes so the cookie can't be validated
	req = http.Request{
		method: .post
		url:    'http://${localserver}/${path}'
		data:   formdata
		header: header
	}
	req.add_cookie(name: csrf_config.cookie_name, value: cookie)
	req.add_cookie(name: session_id_cookie_name, value: 'altered')

	res = req.do() or { panic(err) }
	assert res.status() == .forbidden

	req = http.Request{
		method: .post
		url:    'http://${localserver}/${path}'
		data:   formdata
		header: header
	}
	req.add_cookie(name: csrf_config.cookie_name, value: cookie)
	req.add_cookie(name: session_id_cookie_name, value: session_id)
	// Everything is valid now and the request should succeed
	res = req.do() or { panic(err) }
	assert res.status() == .ok
}

fn test_protect_with_app() {
	protect_route_util('/auth')
}

fn test_protect_with_middleware() {
	protect_route_util('middleware_auth')
}

fn testsuite_end() {
	// This test is guaranteed to be called last.
	// It sends a request to the server to shutdown.
	x := http.get('http://${localserver}/shutdown') or {
		assert err.msg() == ''
		return
	}
	assert x.status() == .ok
	assert x.body == 'good bye'
}

// Utility functions

fn get_token_cookie(session_id string) (string, string) {
	mut ctx := vweb.Context{}
	ctx.req.add_cookie(name: session_id_cookie_name, value: session_id)

	token := csrf.set_token(mut ctx, csrf_config_origin)

	mut cookie := ctx.header.get(.set_cookie) or { '' }
	// get cookie value from "name=value;"
	cookie = cookie.split(' ')[0].all_after('=').replace(';', '')
	return token, cookie
}
