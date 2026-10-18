import net.http
import veb

struct RenamedInput {
	display_name string @[required; json: 'name']
}

struct RenamedApp {}

struct RenamedContext {
	veb.Context
}

@['/renamed'; post]
fn (app &RenamedApp) renamed(mut ctx RenamedContext) veb.Result {
	input := ctx.bind[RenamedInput]() or {
		return ctx.request_error(err.msg())
	}
	return ctx.text(input.display_name)
}

fn renamed_request(data string) &veb.Context {
	mut app := RenamedApp{}
	mut header := http.new_header_from_map({
		http.CommonHeader.host: 'localhost'
	})
	header.set(.content_type, 'application/json')
	header.set(.content_length, '${data.len}')
	return veb.render_request[RenamedApp, RenamedContext](mut app, http.Request{
		method: .post
		url:    '/renamed'
		data:   data
		header: header
	}) or { panic(err) }
}

fn test_bind_accepts_a_required_renamed_json_key() {
	ctx := renamed_request('{"name":"Amy"}')
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'Amy'
}

fn test_bind_rejects_a_missing_renamed_json_key() {
	ctx := renamed_request('{}')
	assert ctx.res.status_code == int(http.Status.bad_request)
	assert ctx.res.body.contains('display_name')
}
