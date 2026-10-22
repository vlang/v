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
