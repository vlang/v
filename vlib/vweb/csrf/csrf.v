module csrf

import crypto.hmac
import crypto.sha256
import encoding.base64
import net.http
import net.urllib
import rand
import time
import vweb

[params]
pub struct CsrfConfig {
pub:
	// get_secret should return a secret that is user unique
	get_secret fn (http.Request) string
	// how long the random csrf-token should be
	token_length int = 64
	// HTTP "safe" methods meaning they shouldn't alter state.
	// If a request with any of these methods is made, `protect` will always return true
	// https://datatracker.ietf.org/doc/html/rfc7231#section-4.2.1
	safe_methods []http.Method = [.get, .head, .options]
	// which hosts are allowed, enforced by checking the Origin and Referer header
	// if allowed_hosts contains '*' the check will be skipped.
	// Subdomains need to be included separately: a request from `"sub.example.com"`
	//  will be rejected when `allowed_host = ['example.com']`.
	allowed_hosts []string
	// the name of the csrftoken in the hidden html input
	token_name string = 'csrftoken'
	// cookie options
	cookie_name string        = 'csrftoken'
	same_site   http.SameSite = .same_site_strict_mode
	http_only   bool   = true
	secure      bool   = true
	cookie_path string = '/'
	// how long the cookie stays valid in seconds. Default is 30 days
	max_age       int = 60 * 60 * 24 * 30
	cookie_domain string
}

pub struct CsrfApp {
	CsrfConfig
pub mut:
	// the csrftoken that should be placed in an html form
	token string
}

// set_token is the app wrapper for `set_token`
pub fn (mut app CsrfApp) set_token(mut ctx vweb.Context) {
	app.token = set_token(mut ctx, app.CsrfConfig)
}

// protect is the app wrapper for `protect`
pub fn (mut app CsrfApp) protect(mut ctx vweb.Context) bool {
	return protect(mut ctx, app.CsrfConfig)
}

// check_origin_and_referer is the app wrapper for `check_origin_and_referer`
fn (app &CsrfApp) check_origin_and_referer(ctx vweb.Context) bool {
	return check_origin_and_referer(ctx, app.CsrfConfig)
}

// middleware returns a function that you can use in `app.middlewares`
pub fn middleware(config &CsrfConfig) vweb.Middleware {
	return fn [config] (mut ctx vweb.Context) bool {
		return protect(mut ctx, config)
	}
}

// set_token returns the csrftoken and sets an encrypted cookie with the hmac of
// `config.get_secret` and the csrftoken
pub fn set_token(mut ctx vweb.Context, config &CsrfConfig) string {
	secret := config.get_secret(ctx.req)

	token := rand.string_from_set('0123456789ABCDEFGHIJKLMNOPQRSTUVXYZabcdefghijklmnopqrstuvwxyz',
		config.token_length)

	// convert secret key based on the request context and a random token into an hmac key
	cookie := base64.url_encode(hmac.new(secret.bytes(), token.bytes(), sha256.sum, sha256.block_size))

	// the hmac key is set as a cookie and later validated with `app.token` that must
	// be in an html form
	ctx.set_cookie(http.Cookie{
		name: config.cookie_name
		value: cookie
		same_site: config.same_site
		http_only: config.http_only
		secure: config.secure
		path: config.cookie_path
		expires: time.now().add_seconds(config.max_age)
		max_age: config.max_age
	})

	return token
}

// protect returns false and sends an http 401 response when the csrf verification
// fails. protect will always return true if the current request method is in
// `config.safe_methods`.
pub fn protect(mut ctx vweb.Context, config &CsrfConfig) bool {
	// if the request method is a "safe" method we allow the request
	if ctx.req.method in config.safe_methods {
		return true
	}

	// check origin and referer header
	if check_origin_and_referer(ctx, config) == false {
		request_is_invalid(mut ctx)
		return false
	}

	// get secret, cookie and csrftoken from the form
	cookie := ctx.get_cookie(config.cookie_name) or {
		request_is_invalid(mut ctx)
		return false
	}
	token := ctx.form[config.token_name] or {
		request_is_invalid(mut ctx)
		return false
	}
	secret := config.get_secret(ctx.req)
	println('${cookie}, ${token}, ${secret}')

	// generate new hmac based on information in the http request
	expected_mac := hmac.new(secret.bytes(), token.bytes(), sha256.sum, sha256.block_size)

	// if the new hmac matches the cookie value the request is legit
	if hmac.equal(base64.url_decode(cookie), expected_mac) == false {
		request_is_invalid(mut ctx)
		return false
	}
	return true
}

// check_origin_and_referer validates the `Origin` and `Referer` headers.
fn check_origin_and_referer(ctx vweb.Context, config &CsrfConfig) bool {
	// wildcard allow all hosts NOT SAFE!
	if '*' in config.allowed_hosts {
		return true
	}

	// only match host and match the full domain name
	// because lets say `allowed_host` = `['example.com']`.
	// Attackers shouldn't be able to bypass this check with the domain `example.com.attacker.com`

	origin := ctx.get_header('Origin')
	origin_url := urllib.parse(origin) or { return false }

	if origin_url.hostname() !in config.allowed_hosts {
		return false
	}

	referer := ctx.get_header('Referer')
	referer_url := urllib.parse(referer) or { return false }

	if referer_url.hostname() !in config.allowed_hosts {
		return false
	}

	return true
}

// request_is_invalid sends an http 401 response
fn request_is_invalid(mut ctx vweb.Context) {
	ctx.set_status(401, '')
	ctx.text('HTTP 401: Invalid or missing CSRF token')
}
