module veb

import net.http
import net.urllib

const issue_20757_allowed_origin = 'https://vlang.io'
const issue_20757_route = '/before_request'
const issue_20757_secret_key = 'secretkey'

struct Issue20757Context {
	Context
}

fn (mut ctx Issue20757Context) before_request() {
	if ctx.req.url != issue_20757_route {
		return
	}
	ctx.set_custom_header('X-BEFORE-REQUEST', 'true') or {}
	key := ctx.get_header(.authorization) or { 'Basic none' }
	if key != 'Basic ${issue_20757_secret_key}' {
		ctx.request_error('Authorization failed')
	}
}

struct Issue20757App {
	Middleware[Issue20757Context]
}

@['/before_request']
fn (app &Issue20757App) authorized(mut ctx Issue20757Context) Result {
	return ctx.text('authorized')
}

fn new_issue_20757_context(auth_header string) Issue20757Context {
	mut header := http.new_header_from_map({
		http.CommonHeader.origin: issue_20757_allowed_origin
	})
	if auth_header != '' {
		header.add(.authorization, auth_header)
	}
	return Issue20757Context{
		Context: Context{
			req:   http.Request{
				method: .get
				url:    issue_20757_route
				header: header
			}
			query: {}
			form:  {}
			files: {}
			res:   http.Response{
				header: http.new_header()
			}
		}
	}
}

fn new_issue_20757_app() &Issue20757App {
	mut app := &Issue20757App{}
	app.use(cors[Issue20757Context](CorsOptions{
		origins:         [issue_20757_allowed_origin]
		allowed_methods: [.get]
	}))
	return app
}

fn test_before_request_still_runs_with_cors_middleware() {
	mut app := new_issue_20757_app()
	mut ctx := new_issue_20757_context('')
	routes := generate_routes[Issue20757App, Issue20757Context](app)!
	url := urllib.parse(issue_20757_route)!

	handle_route[Issue20757App, Issue20757Context](mut app, mut ctx, url, '', &routes)

	assert ctx.res.status_code == int(http.Status.bad_request)
	assert ctx.res.body == 'Authorization failed'
	assert ctx.res.header.get_custom('X-BEFORE-REQUEST')! == 'true'
}

fn test_before_request_can_allow_cors_request() {
	mut app := new_issue_20757_app()
	mut ctx := new_issue_20757_context('Basic ${issue_20757_secret_key}')
	routes := generate_routes[Issue20757App, Issue20757Context](app)!
	url := urllib.parse(issue_20757_route)!

	handle_route[Issue20757App, Issue20757Context](mut app, mut ctx, url, '', &routes)

	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'authorized'
	assert ctx.res.header.get_custom('X-BEFORE-REQUEST')! == 'true'
	assert ctx.res.header.get(.access_control_allow_origin)! == issue_20757_allowed_origin
}
