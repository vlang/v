module csrf

import crypto.hmac
import crypto.sha256
import encoding.base64
import net.http
import net.urllib
import rand
import time
import veb

@[params]
pub struct CsrfConfig {
pub:
	secret string
	// how long the random part of the csrf-token should be
	nonce_length int = 64
	// HTTP "safe" methods meaning they shouldn't alter state.
	// If a request with any of these methods is made, `protect` will always return true
	// https://datatracker.ietf.org/doc/html/rfc7231#section-4.2.1
	safe_methods []http.Method = [.get, .head, .options]
	// which hosts are allowed, enforced by checking the Origin and Referer header
	// if allowed_hosts contains '*' the check will be skipped.
	// Subdomains need to be included separately: a request from `"sub.example.com"`
	//  will be rejected when `allowed_host = ['example.com']`.
	allowed_hosts []string
	// if set to true both the Referer and Origin headers must match `allowed_hosts`
	// else if either one is valid the request is accepted
	check_origin_and_referer bool = true
	// the name of the csrf-token in the hidden html input
	token_name string = 'csrftoken'
	// the name of the cookie that contains the session id
	session_cookie string
	// cookie options
	cookie_name string        = 'csrftoken'
	same_site   http.SameSite = .same_site_strict_mode
	cookie_path string        = '/'
	// how long the cookie stays valid in seconds. Default is 30 days
	max_age       int = 60 * 60 * 24 * 30
	cookie_domain string
	// whether the cookie can be send only over HTTPS
	secure bool
	// enable printing verbose statements
	verbose bool
}

pub struct CsrfContext {
pub mut:
	config CsrfConfig
	exempt bool
	// the csrftoken that should be placed in an html form
	csrf_token string
}

// set_token generates a new csrf_token and adds a Cookie to the response
pub fn (mut ctx CsrfContext) set_csrf_token[T](mut user_context T) string {
	ctx.csrf_token = set_token(mut user_context, ctx.config)
	return ctx.csrf_token
}

// clear the csrf token and cookie header from the context
pub fn (ctx &CsrfContext) clear_csrf_token[T](mut user_context T) {
	user_context.set_cookie(http.Cookie{
		name:    config.cookie_name
		value:   ''
		max_age: 0
	})
}

// csrf_token_input returns an HTML hidden input containing the csrf token
pub fn (ctx &CsrfContext) csrf_token_input() veb.RawHtml {
	return '<input type="hidden" name="${ctx.config.token_name}" value="${ctx.csrf_token}">'
}

// middleware returns a handler that you can use with veb's middleware
pub fn middleware[T](config CsrfConfig) veb.MiddlewareOptions[T] {
	return veb.MiddlewareOptions[T]{
		after:   false
		handler: fn [config] [T](mut ctx T) bool {
			ctx.config = config
			if ctx.exempt {
				return true
			} else if ctx.req.method in config.safe_methods {
				return true
			} else {
				return protect(mut ctx, config)
			}
		}
	}
}

// set_token returns the csrftoken and sets an encrypted cookie with the hmac of
// `config.get_secret` and the csrftoken
pub fn set_token(mut ctx veb.Context, config &CsrfConfig) string {
	expire_time := time.now().add_seconds(config.max_age)
	session_id := ctx.get_cookie(config.session_cookie) or { '' }

	token := generate_token(expire_time.unix(), session_id, config.nonce_length)
	cookie := generate_cookie(expire_time.unix(), token, config.secret)

	// the hmac key is set as a cookie and later validated with `app.token` that must
	// be in an html form
	ctx.set_cookie(http.Cookie{
		name:      config.cookie_name
		value:     cookie
		same_site: config.same_site
		http_only: true
		secure:    config.secure
		path:      config.cookie_path
		expires:   expire_time
		max_age:   config.max_age
	})

	return token
}

// protect returns false and sends an http 401 response when the csrf verification
// fails. protect will always return true if the current request method is in
// `config.safe_methods`.
pub fn protect(mut ctx veb.Context, config &CsrfConfig) bool {
	// if the request method is a "safe" method we allow the request
	if ctx.req.method in config.safe_methods {
		return true
	}

	// check origin and referer header
	if check_origin_and_referer(ctx, config) == false {
		request_is_invalid(mut ctx)
		return false
	}

	// use the session id from the cookie, not from the csrftoken
	session_id := ctx.get_cookie(config.session_cookie) or { '' }

	actual_token := ctx.form[config.token_name] or {
		request_is_invalid(mut ctx)
		return false
	}
	// retrieve timestamp and nonce from csrftoken
	data := base64.url_decode_str(actual_token).split('.')
	if config.verbose {
		eprintln('[CSRF] Token data: ${data}')
	}
	if data.len < 3 {
		request_is_invalid(mut ctx)
		return false
	}

	// check the timestamp from the csrftoken against the current time
	// if an attacker would change the timestamp on the cookie, the token or both the
	// hmac would also change.
	now := time.now().unix()
	expire_timestamp := data[0].i64()
	if expire_timestamp < now {
		// token has expired
		request_is_invalid(mut ctx)
		return false
	}
	nonce := data.last()
	expected_token := base64.url_encode_str('${expire_timestamp}.${session_id}.${nonce}')

	mut actual_hash := ctx.get_cookie(config.cookie_name) or {
		request_is_invalid(mut ctx)
		return false
	}
	// old_expire := actual_hash.all_before('.')
	// actual_hash = actual_hash.replace_once (old_expire, expire_timestamp.str())

	// generate new hmac based on information in the http request
	expected_hash := generate_cookie(expire_timestamp, expected_token, config.secret)
	if config.verbose {
		eprintln('[CSRF] Actual Hash: ${actual_hash}')
		eprintln('[CSRF] Expected Hash: ${expected_hash}')
	}

	// if the new hmac matches the cookie value the request is legit
	if actual_hash != expected_hash {
		if config.verbose {
			eprintln('[CSRF] The actual hash differs from the expected hash')
		}
		request_is_invalid(mut ctx)
		return false
	}
	if config.verbose {
		eprintln('[CSRF] The actual hash matches the expected hash')
	}

	return true
}

// check_origin_and_referer validates the `Origin` and `Referer` headers.
fn check_origin_and_referer(ctx veb.Context, config &CsrfConfig) bool {
	// wildcard allow all hosts NOT SAFE!
	if '*' in config.allowed_hosts {
		return true
	}

	// only match host and match the full domain name
	// because lets say `allowed_host` = `['example.com']`.
	// Attackers shouldn't be able to bypass this check with the domain `example.com.attacker.com`

	origin := ctx.get_header(.origin) or { return false }
	origin_url := urllib.parse(origin) or { urllib.URL{} }

	valid_origin := origin_url.hostname() in config.allowed_hosts

	referer := ctx.get_header(.referer) or { return false }
	referer_url := urllib.parse(referer) or { urllib.URL{} }

	valid_referer := referer_url.hostname() in config.allowed_hosts

	if config.check_origin_and_referer {
		return valid_origin && valid_referer
	} else {
		return valid_origin || valid_referer
	}
}

// request_is_invalid sends an http 403 response
fn request_is_invalid(mut ctx veb.Context) {
	ctx.res.set_status(.forbidden)
	ctx.text('Forbidden: Invalid or missing CSRF token')
}

fn generate_token(expire_time i64, session_id string, nonce_length int) string {
	nonce := rand.string_from_set('0123456789ABCDEFGHIJKLMNOPQRSTUVXYZabcdefghijklmnopqrstuvwxyz',
		nonce_length)
	token := '${expire_time}.${session_id}.${nonce}'

	return base64.url_encode_str(token)
}

// generate_cookie converts secret key based on the request context and a random
// token into an hmac key
fn generate_cookie(expire_time i64, token string, secret string) string {
	hash := base64.url_encode(hmac.new(secret.bytes(), token.bytes(), sha256.sum, sha256.block_size))
	cookie := '${expire_time}.${hash}'

	return cookie
}
