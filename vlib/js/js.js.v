module js

import js.promise

pub fn JS.fetch(input JS.String, init JS.Object) JS.Promise

pub interface JS.Body {
	body JS.Uint8Array
	bodyUse JS.Boolean
	blob() JS.Promise
	json() JS.Promise
	text() JS.Promise
}

pub interface JS.Response {
	JS.Body
	ok JS.Boolean
	redirected JS.Boolean
	status JS.Number
	statusText JS.String
	url JS.String
	clone() JS.Response
}

pub fn fetch(input string, init map[string]JS.Any) promise.Promise<JS.Response, JS.String> {
	p_init := JS.Any(unsafe { nil })
	p := promise.Promise<JS.Response, String>{p_init}

	#let obj = {}; for (let [key,val] of init.map) { obj[key] = val; }
	#p.promise = fetch(input.str,obj);

	return p
}
