module oauth

import json
import net.http

pub enum TokenPostType {
	form
	json
}

pub struct Context {
pub:
	token_url       string
	client_id       string
	client_secret   string
	token_post_type TokenPostType = .form
	redirect_uri    string
}

pub struct Request {
pub:
	client_id     string
	client_secret string
	code          string
	state         string
}

pub fn (ctx &Context) get_token(code string) string {
	oauth_request := Request{
		client_id:     ctx.client_id
		client_secret: ctx.client_secret
		code:          code
		//        state: csrf
	}

	js := json.encode(oauth_request)
	if ctx.token_post_type == .json {
		resp := http.post_json(ctx.token_url, js) or {
			//        app.info(err.msg())

			//        return app.redirect_to_index()
			return ''
		}
		println('OAUTH RESPONSE ${resp}')
		return resp.body
	} else {
		resp := http.post_form(ctx.token_url, {
			'client_id':     ctx.client_id
			'client_secret': ctx.client_secret
			'code':          code
			'grant_type':    'authorization_code'
			//'scope':         bot
			'redirect_uri':  ctx.redirect_uri
		}) or {
			//        app.info(err.msg())

			//        return app.redirect_to_index()
			return ''
		}
		println('OAUTH RESPONSE ${resp}')
		return resp.body
	}
	return ''
}
