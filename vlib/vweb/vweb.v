module vweb

import (
	os
	strings 
	net 
	http 
) 

struct Context {
	static_files map[string]string 
pub: 
	req http.Request 
	conn net.Socket 
	post_form map[string]string 
	// TODO Response 
	headers []string  // response headers 
} 

pub fn (ctx Context) text(s string) {
	h := ctx.headers.join('\n')
	ctx.conn.write('HTTP/1.1 200 OK 
Content-Type: text/plain 
$h
$s 
') 
} 

pub fn (ctx Context) json(s string) {
	h := ctx.headers.join('\n')
	ctx.conn.write('HTTP/1.1 200 OK 
Content-Type: application/json 
$h

$s 
') 
} 

pub fn (ctx Context) redirect(url string) {
        h := ctx.headers.join('\n')
        ctx.conn.write('
HTTP/1.1 302 Found
Location: $url
$h
') 
} 

pub fn (ctx mut Context) set_cookie(key, val string) {
	ctx.set_header('Set-Cookie', '$key=$val') 
} 

pub fn (ctx Context) get_cookie(key string) string { 
	cookie := ctx.req.headers['Cookie']
	return cookie.find_between('$key=', ';')
} 

fn (ctx mut Context) set_header(key, val string) {
	// ctx.resp.headers[key] = val
	ctx.headers << '$key: $val'
}

pub fn (ctx Context) html(html string) { 
	//tmpl := os.read_file(path)  or {return} 
	ctx.conn.write('HTTP/1.1 200 OK 
Content-Type: text/html 

$html 
')
	 
} 

pub fn run<T>(port int) { 
	println('Running vweb app on http://localhost:$port ...')  
	l := net.listen(port) or { panic('failed to listen') return } 
	for {
		conn := l.accept() or {
			panic('accept() failed') 
			return 
		} 
		// TODO move this to handle_conn<T>(conn, app)
		s := conn.read_line() 
		// Parse the first line
		// "GET / HTTP/1.1"
		first_line := s.all_before('\n')
		vals := first_line.split(' ') 
		mut action := vals[1].right(1).all_before('/') 
		if action == '' {
			action = 'index' 
		} 
		req := http.Request{
		        headers: map[string]string{} 
		        ws_func: 0
		        user_ptr: 0
		        method: vals[0]
		        url: vals[1] 
		} 
		mut app := T{
			vweb: Context{
				req: req 
				conn: conn 
				post_form: map[string]string{} 
				static_files: map[string]string{} 
			} 
		} 
		app.init() 
		if req.method == 'POST' {
			app.vweb.parse_form(s) 
		} 
		println('vweb action = "$action"') 
		if vals.len < 2 {
			println('no vals for http') 
			return 
		} 
		// Serve a static file if it's one 
		if app.vweb.handle_static() {
			conn.close()
			continue 
		} 
		// Call the right action 
		app.$action() 
		conn.close()
	}
} 


fn (ctx mut Context) parse_form(s string) { 
	if ctx.req.method != 'POST' {
		return 
	} 
	pos := s.index('\r\n\r\n')
	if pos > -1 {
		mut str_form := s.substr(pos, s.len)
		str_form = str_form.replace('+', ' ')
		words := str_form.split('&')
		for word in words {
			keyval := word.split('=')
			key := keyval[0]
			val := keyval[1]
			//println('http form $key => $val') 
			ctx.post_form[key] = http.unescape(val) 
		}
	}
} 

fn (ctx mut Context) handle_static() bool { 
	static_file := ctx.static_files[ctx.req.url] 
	if static_file != '' { 
		data := os.read_file(static_file) or { return false }  
		ctx.conn.write('HTTP/1.1 200 OK 
Content-Type: text/css 

$data 
')
		return true 
	} 
	return false 
} 

pub fn (ctx mut Context) serve_static(url, file_path string) { 
	ctx.static_files[url] = file_path 
} 


