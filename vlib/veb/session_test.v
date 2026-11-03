module veb

import net.http

// In-process tests for signed-cookie sessions. No network involved.
fn session_test_request(cookie_pair string) http.Request {
	mut headers := {
		'Host': '127.0.0.1'
	}
	mut cookie_name := ''
	mut cookie_value := ''
	if cookie_pair != '' {
		headers['Cookie'] = cookie_pair
		kv := cookie_pair.split_nth('=', 2)
		if kv.len == 2 {
			cookie_name = kv[0]
			cookie_value = kv[1]
		}
	}
	mut req := http.Request{
		method: .get
		url:    '/'
		header: http.new_custom_header_from_map(headers) or { panic(err) }
	}
	if cookie_name != '' {
		// Mirror the wire parser, which fills req.cookies from the header.
		req.add_cookie(http.Cookie{
			name:  cookie_name
			value: cookie_value
		})
	}
	return req
}

fn session_roundtrip(opts SessionOptions, values map[string]string) Session {
	mut save_ctx := &Context{
		req: session_test_request('')
	}
	mut s := save_ctx.session(opts)
	for key, value in values {
		s.set(key, value)
	}
	save_ctx.save_session(s)
	set_cookie := save_ctx.res.header.get(.set_cookie) or { panic('missing Set-Cookie') }
	cookie_pair := set_cookie.all_before(';')
	mut load_ctx := &Context{
		req: session_test_request(cookie_pair)
	}
	return load_ctx.session(opts)
}

fn test_session_roundtrip() {
	opts := SessionOptions{
		secret: 'test-secret-that-is-long-enough'
	}
	loaded := session_roundtrip(opts, {
		'user': 'ada'
		'role': 'admin'
	})
	assert loaded.get('user') or { '' } == 'ada'
	assert loaded.get('role') or { '' } == 'admin'
	if _ := loaded.get('missing') {
		assert false, 'absent key must be none'
	}
}

fn test_session_roundtrip_empty_keys_and_values() {
	opts := SessionOptions{
		secret: 'test-secret-that-is-long-enough'
	}
	loaded := session_roundtrip(opts, {
		'':      'empty key'
		'empty': ''
		'user':  'ada'
	})
	assert loaded.get('') or { panic('missing empty key') } == 'empty key'
	assert loaded.get('empty') or { panic('missing empty value') } == ''
	assert loaded.get('user') or { panic('missing user') } == 'ada'
	blank := session_roundtrip(opts, {
		'': ''
	})
	assert blank.get('') or { panic('missing empty entry') } == ''
}

fn test_session_rejects_tampering() {
	opts := SessionOptions{
		secret: 'test-secret-that-is-long-enough'
	}
	mut save_ctx := &Context{
		req: session_test_request('')
	}
	mut s := save_ctx.session(opts)
	s.set('user', 'ada')
	save_ctx.save_session(s)
	set_cookie := save_ctx.res.header.get(.set_cookie) or { panic('missing Set-Cookie') }
	mut pair := set_cookie.all_before(';')
	// Tamper with the first payload character: any single base64 change breaks
	// the HMAC. (The last char may hold only zero padding bits, so flipping
	// it is not guaranteed to change the decoded bytes.)
	eq := pair.index('=') or { panic('no = in cookie') }
	rest := pair[eq + 1..]
	flipped_first := if rest[0] == `A` { 'B' } else { 'A' }
	pair = pair[..eq + 1] + flipped_first + rest[1..]
	mut load_ctx := &Context{
		req: session_test_request(pair)
	}
	loaded := load_ctx.session(opts)
	if _ := loaded.get('user') {
		assert false, 'tampered cookie must not load'
	}
}

fn test_session_rejects_wrong_secret_and_expiry() {
	mut save_ctx := &Context{
		req: session_test_request('')
	}
	mut s := save_ctx.session(SessionOptions{
		secret: 'correct-secret'
	})
	s.set('user', 'ada')
	save_ctx.save_session(s)
	set_cookie := save_ctx.res.header.get(.set_cookie) or { panic('missing Set-Cookie') }
	pair := set_cookie.all_before(';')

	mut wrong_ctx := &Context{
		req: session_test_request(pair)
	}
	wrong := wrong_ctx.session(SessionOptions{
		secret: 'wrong-secret'
	})
	if _ := wrong.get('user') {
		assert false, 'wrong secret must not load'
	}

	expired_opts := SessionOptions{
		secret:  'correct-secret'
		max_age: -100
	}
	mut exp_ctx := &Context{
		req: session_test_request('')
	}
	mut exp_session := exp_ctx.session(expired_opts)
	exp_session.set('user', 'ada')
	exp_ctx.save_session(exp_session)
	exp_cookie := exp_ctx.res.header.get(.set_cookie) or { panic('missing Set-Cookie') }
	mut load_ctx := &Context{
		req: session_test_request(exp_cookie.all_before(';'))
	}
	if _ := load_ctx.session(expired_opts).get('user') {
		assert false, 'expired session must not load'
	}
}

fn test_session_empty_secret_loads_and_saves_nothing() {
	mut save_ctx := &Context{
		req: session_test_request('')
	}
	opts := SessionOptions{
		secret: ''
	}
	mut s := save_ctx.session(opts)
	s.set('user', 'ada')
	save_ctx.save_session(s)
	if _ := save_ctx.res.header.get(.set_cookie) {
		assert false, 'empty secret must not set a cookie'
	}
}

fn test_session_clear_deletes_cookie() {
	opts := SessionOptions{
		secret: 'test-secret-that-is-long-enough'
	}
	mut ctx := &Context{
		req: session_test_request('veb_session=anything')
	}
	ctx.clear_session(opts)
	set_cookie := ctx.res.header.get(.set_cookie) or { panic('missing Set-Cookie') }
	assert set_cookie.contains('veb_session=')
	// Negative max-age is rendered as Max-Age=0: the browser deletes it.
	assert set_cookie.contains('Max-Age=0')
}
