module vweb

import net
import net.http
import strings
import time

[noinit]
pub struct Context {
mut:
	content_type string = 'text/plain'
	status       string = '200 OK'
pub:
	req http.Request
	// TODO Response
	static_files      map[string]string
	static_mime_types map[string]string
pub mut:
	conn              &net.TcpConn
	// TODO: make form, query, and files read-only
	form              map[string]string
	query             map[string]string
	files             map[string][]FileData
	headers           string // response headers
	done              bool
	page_gen_start    i64
	form_error        string
	chunked_transfer  bool
	max_chunk_len     int = 20
}

struct FileData {
pub:
	filename     string
	content_type string
	data         string
}

// vweb intern function
[manualfree]
pub fn (mut ctx Context) send_response_to_client(mimetype string, res string) bool {
	if ctx.done {
		return false
	}
	ctx.done = true
	mut sb := strings.new_builder(1024)
	defer {
		unsafe { sb.free() }
	}
	sb.write_string('HTTP/1.1 $ctx.status')
	sb.write_string('\r\nContent-Type: $mimetype')
	sb.write_string('\r\nContent-Length: $res.len')
	if ctx.chunked_transfer {
		sb.write_string('\r\nTransfer-Encoding: chunked')
	}
	sb.write_string(ctx.headers)
	sb.write_string('\r\n')
	sb.write_string(vweb.headers_close)
	if ctx.chunked_transfer {
		mut i := 0
		mut len := res.len
		for {
			if len <= 0 {
				break
			}
			mut chunk := ''
			if len > ctx.max_chunk_len {
				chunk = res[i..i + ctx.max_chunk_len]
				i += ctx.max_chunk_len
				len -= ctx.max_chunk_len
			} else {
				chunk = res[i..]
				len = 0
			}
			sb.write_string(chunk.len.hex())
			sb.write_string('\r\n$chunk\r\n')
		}
		sb.write_string('0\r\n\r\n') // End of chunks
	} else {
		sb.write_string(res)
	}
	s := sb.str()
	defer {
		unsafe { s.free() }
	}
	send_string(mut ctx.conn, s) or { return false }
	return true
}

// Response HTTP_OK with s as payload with content-type `text/html`
pub fn (mut ctx Context) html(s string) Result {
	ctx.send_response_to_client('text/html', s)
	return Result{}
}

// Response HTTP_OK with s as payload with content-type `text/plain`
pub fn (mut ctx Context) text(s string) Result {
	ctx.send_response_to_client('text/plain', s)
	return Result{}
}

// Response HTTP_OK with s as payload with content-type `application/json`
pub fn (mut ctx Context) json(s string) Result {
	ctx.send_response_to_client('application/json', s)
	return Result{}
}

// Response HTTP_OK with s as payload
pub fn (mut ctx Context) ok(s string) Result {
	ctx.send_response_to_client(ctx.content_type, s)
	return Result{}
}

// Response a server error
pub fn (mut ctx Context) server_error(ecode int) Result {
	$if debug {
		eprintln('> ctx.server_error ecode: $ecode')
	}
	if ctx.done {
		return Result{}
	}
	send_string(mut ctx.conn, vweb.http_500) or {}
	return Result{}
}

// Redirect to an url
pub fn (mut ctx Context) redirect(url string) Result {
	if ctx.done {
		return Result{}
	}
	ctx.done = true
	send_string(mut ctx.conn, 'HTTP/1.1 302 Found\r\nLocation: $url$ctx.headers\r\n$vweb.headers_close') or {
		return Result{}
	}
	return Result{}
}

// Send an not_found response
pub fn (mut ctx Context) not_found() Result {
	if ctx.done {
		return Result{}
	}
	ctx.done = true
	send_string(mut ctx.conn, vweb.http_404) or {}
	return Result{}
}

// Returns an empty result
pub fn not_found() Result {
	return Result{}
}

// Enables chunk transfer with max_chunk_len per chunk
pub fn (mut ctx Context) enable_chunked_transfer(max_chunk_len int) {
	ctx.chunked_transfer = true
	ctx.max_chunk_len = max_chunk_len
}

// Sets a cookie
pub fn (mut ctx Context) set_cookie(cookie Cookie) {
	mut cookie_data := []string{}
	mut secure := if cookie.secure { 'Secure;' } else { '' }
	secure += if cookie.http_only { ' HttpOnly' } else { ' ' }
	cookie_data << secure
	if cookie.expires.unix > 0 {
		cookie_data << 'expires=$cookie.expires.utc_string()'
	}
	data := cookie_data.join(' ')
	ctx.add_header('Set-Cookie', '$cookie.name=$cookie.value; $data')
}

// Old function
[deprecated]
pub fn (mut ctx Context) set_cookie_old(key string, val string) {
	// TODO support directives, escape cookie value (https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie)
	// ctx.add_header('Set-Cookie', '${key}=${val};  Secure; HttpOnly')
	ctx.add_header('Set-Cookie', '$key=$val; HttpOnly')
}

// Sets the response content type
pub fn (mut ctx Context) set_content_type(typ string) {
	ctx.content_type = typ
}

// Sets a cookie with a `expire_data`
pub fn (mut ctx Context) set_cookie_with_expire_date(key string, val string, expire_date time.Time) {
	ctx.add_header('Set-Cookie', '$key=$val;  Secure; HttpOnly; expires=$expire_date.utc_string()')
}

// Gets a cookie by a key
pub fn (ctx &Context) get_cookie(key string) ?string { // TODO refactor
	mut cookie_header := ctx.get_header('cookie')
	if cookie_header == '' {
		cookie_header = ctx.get_header('Cookie')
	}
	cookie_header = ' ' + cookie_header
	// println('cookie_header="$cookie_header"')
	// println(ctx.req.headers)
	cookie := if cookie_header.contains(';') {
		cookie_header.find_between(' $key=', ';')
	} else {
		cookie_header.find_between(' $key=', '\r')
	}
	if cookie != '' {
		return cookie.trim_space()
	}
	return error('Cookie not found')
}

// Sets the response status
pub fn (mut ctx Context) set_status(code int, desc string) {
	if code < 100 || code > 599 {
		ctx.status = '500 Internal Server Error'
	} else {
		ctx.status = '$code $desc'
	}
}

// Adds an header to the response with key and val
pub fn (mut ctx Context) add_header(key string, val string) {
	// println('add_header($key, $val)')
	ctx.headers = ctx.headers + '\r\n$key: $val'
	// println(ctx.headers)
}

// Returns the header data from the key
pub fn (ctx &Context) get_header(key string) string {
	return ctx.req.lheaders[key.to_lower()]
}

// Returns the ip address from the current user
pub fn (ctx &Context) ip() string {
	mut ip := ctx.req.lheaders['x-forwarded-for']
	if ip == '' {
		ip = ctx.req.lheaders['x-real-ip']
	}

	if ip.contains(',') {
		ip = ip.all_before(',')
	}
	if ip == '' {
		ip = ctx.conn.peer_ip() or { '' }
	}
	return ip
}

// Set s to the form error
pub fn (mut ctx Context) error(s string) {
	ctx.form_error = s
}

