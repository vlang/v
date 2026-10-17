// bind_test.v - one call decodes the request and validates it.
//
// Binding is what turns an untyped bag of strings into the struct the handler
// actually wants, or into an error naming the field that refused. A JSON body
// decodes into the full type; query and form fill the scalar fields, form
// winning on conflict. The handlers below bind exactly the way user code
// does, driven through `render_request`, so what is asserted is what a route
// receives.
module veb

import net.http

struct SignupInput {
	name     string @[required; max: 64]
	email    string @[required; email]
	password string @[required; min: 12]
	age      int    @[min: 13]
	nick     string
	active   bool
}

struct SearchInput {
	name   string
	age    int
	active bool
}

struct BindApp {}

struct BindContext {
	Context
}

@['/signup'; post]
fn (app &BindApp) signup(mut ctx BindContext) Result {
	input := ctx.bind[SignupInput]() or {
		return ctx.request_error(err.msg())
	}
	return ctx.text('${input.name}:${input.email}:${input.age}:${input.nick}:${input.active}')
}

@['/search'; get; post]
fn (app &BindApp) search(mut ctx BindContext) Result {
	input := ctx.bind[SearchInput]() or {
		return ctx.request_error(err.msg())
	}
	return ctx.text('${input.name}:${input.age}')
}

// bind_request posts or gets one body through the dispatcher.
fn bind_request(method http.Method, url string, data string, content_type string) &Context {
	mut app := BindApp{}
	mut header := http.new_header_from_map({
		http.CommonHeader.host: 'localhost'
	})
	if content_type != '' {
		header.set(.content_type, content_type)
	}
	header.set(.content_length, '${data.len}')
	return render_request[BindApp, BindContext](mut app, http.Request{
		method: method
		url:    url
		data:   data
		header: header
	}) or { panic(err) }
}

fn valid_signup_json() string {
	return '{"name":"amy","email":"amy@example.com","password":"twelve-chars-here","age":20,"nick":"a","active":true}'
}

fn test_bind_decodes_a_json_body_into_the_struct() {
	ctx := bind_request(.post, '/signup', valid_signup_json(), 'application/json')
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'amy:amy@example.com:20:a:true'
}

fn test_bind_rejects_a_missing_required_field() {
	ctx := bind_request(.post, '/signup', '{"email":"amy@example.com","password":"twelve-chars-here","age":20}',
		'application/json')
	assert ctx.res.status_code == int(http.Status.bad_request)
	assert ctx.res.body.contains('name')
}

fn test_bind_rejects_a_short_password() {
	ctx := bind_request(.post, '/signup', '{"name":"amy","email":"amy@example.com","password":"short","age":20}',
		'application/json')
	assert ctx.res.status_code == int(http.Status.bad_request)
	assert ctx.res.body.contains('password')
}

fn test_bind_rejects_a_bad_email() {
	ctx := bind_request(.post, '/signup', '{"name":"amy","email":"not-an-email","password":"twelve-chars-here","age":20}',
		'application/json')
	assert ctx.res.status_code == int(http.Status.bad_request)
	assert ctx.res.body.contains('email')
}

fn test_bind_rejects_an_underage_value() {
	ctx := bind_request(.post, '/signup', '{"name":"amy","email":"amy@example.com","password":"twelve-chars-here","age":12}',
		'application/json')
	assert ctx.res.status_code == int(http.Status.bad_request)
	assert ctx.res.body.contains('age')
}

fn test_bind_rejects_a_body_that_is_not_an_object() {
	ctx := bind_request(.post, '/signup', '[1,2]', 'application/json')
	assert ctx.res.status_code == int(http.Status.bad_request)
}

fn test_bind_fills_scalars_from_the_query() {
	ctx := bind_request(.get, '/search?name=amy&age=20', '', '')
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'amy:20'
}

fn test_bind_rejects_an_unparsable_query_int() {
	// The value cannot fill an `int`, so it is an error rather than a
	// silently coerced zero.
	ctx := bind_request(.get, '/search?name=amy&age=abc', '', '')
	assert ctx.res.status_code == int(http.Status.bad_request)
	assert ctx.res.body.contains('age')
}

fn test_bind_fills_scalars_from_the_form() {
	ctx := bind_request(.post, '/search', 'name=amy&age=20', 'application/x-www-form-urlencoded')
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'amy:20'
}

fn test_bind_prefers_the_form_over_the_query() {
	ctx := bind_request(.post, '/search?name=amy&age=20', 'name=zed&age=21',
		'application/x-www-form-urlencoded')
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'zed:21'
}

fn test_bind_rejects_an_empty_required_string_without_a_min() {
	// `name` carries no `min`, so only the shared required-string check
	// rejects the empty value on the JSON path as it does on the form path.
	ctx := bind_request(.post, '/signup',
		'{"name":"","email":"amy@example.com","password":"twelve-chars-here","age":20}',
		'application/json')
	assert ctx.res.status_code == int(http.Status.bad_request)
	assert ctx.res.body.contains('name')
}

fn test_bind_rejects_empty_required_strings_on_the_json_path() {
	ctx := bind_request(.post, '/signup',
		'{"name":"","email":"","password":"twelve-chars-here","age":20}', 'application/json')
	assert ctx.res.status_code == int(http.Status.bad_request)
	assert ctx.res.body.contains('name') || ctx.res.body.contains('email')
}

fn test_bind_rejects_a_present_but_empty_query_int() {
	ctx := bind_request(.get, '/search?name=amy&age=', '', '')
	assert ctx.res.status_code == int(http.Status.bad_request)
	assert ctx.res.body.contains('age')
}

fn test_bind_rejects_a_present_but_empty_query_bool() {
	ctx := bind_request(.get, '/search?name=amy&age=20&active=', '', '')
	assert ctx.res.status_code == int(http.Status.bad_request)
	assert ctx.res.body.contains('active')
}

fn test_bind_rejects_a_present_but_empty_form_int() {
	ctx := bind_request(.post, '/search', 'name=amy&age=',
		'application/x-www-form-urlencoded')
	assert ctx.res.status_code == int(http.Status.bad_request)
	assert ctx.res.body.contains('age')
}

fn test_bind_rejects_an_empty_form_value_over_a_valid_query_value() {
	// The form wins on conflict, so its empty value must fail even though
	// the query holds a parsable one.
	ctx := bind_request(.post, '/search?name=amy&age=20', 'age=',
		'application/x-www-form-urlencoded')
	assert ctx.res.status_code == int(http.Status.bad_request)
	assert ctx.res.body.contains('age')
}

fn test_bind_accepts_a_valid_form_value_over_an_empty_query_value() {
	ctx := bind_request(.post, '/search?name=amy&age=', 'age=21',
		'application/x-www-form-urlencoded')
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'amy:21'
}
