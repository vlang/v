import veb
import veb.csrf

fn test_clear_csrf_token_uses_configured_cookie_name() {
	ctx := csrf.CsrfContext{
		config: csrf.CsrfConfig{ cookie_name: 'custom_csrf' }
	}
	mut user_context := veb.Context{}
	ctx.clear_csrf_token(mut user_context)
	cookies := user_context.res.cookies()
	assert cookies.len == 1
	assert cookies[0].name == 'custom_csrf'
	assert cookies[0].value == ''
}

fn test_clear_csrf_token_expires_cookie() {
	ctx := csrf.CsrfContext{}
	mut user_context := veb.Context{}
	ctx.clear_csrf_token(mut user_context)
	cookie_header := user_context.res.header.get(.set_cookie) or { panic('missing Set-Cookie header') }
	assert 'Max-Age=0' in cookie_header.split('; ')
}

fn test_clear_csrf_token_uses_configured_cookie_path() {
	for config in [csrf.CsrfConfig{}, csrf.CsrfConfig{ cookie_path: '/account' }] {
		ctx := csrf.CsrfContext{
			config: config
		}
		mut user_context := veb.Context{}
		ctx.clear_csrf_token(mut user_context)
		cookies := user_context.res.cookies()
		assert cookies.len == 1
		assert cookies[0].path == config.cookie_path
	}
}
