module vweb

import (
	os
	strings 
	net 
	http 
	net.urllib 
) 

const (
	methods_with_form = ['POST', 'PUT', 'PATCH']
	HEADER_SERVER = 'Server: VWeb\r\n' // TODO add to the headers
	HTTP_404 = 'HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\n\r\n404 Not Found'
	HTTP_500 = 'HTTP/1.1 500 Internal Server Error\r\nContent-Type: text/plain\r\n\r\n500 Internal Server Error'
)

struct Context {
	static_files map[string]string
	static_mime_types map[string]string
pub: 
	req http.Request 
	conn net.Socket 
	form map[string]string 
	// TODO Response 
	headers map[string]string // response headers 
} 

pub fn (ctx Context) parse_headers() string {
	mut headers := ''
	for k, v in ctx.headers {
		headers += '$k: $v'
	}
	return headers
}

pub fn (ctx Context) text(s string) {
	h := ctx.parse_headers()
	ctx.conn.write('HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n$h\r\n\r\n$s')
}

pub fn (ctx Context) json(s string) {
	h := ctx.parse_headers()
	ctx.conn.write('HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n$h\r\n\r\n$s') 
}

pub fn (ctx Context) redirect(url string) {
	h := ctx.parse_headers()
	ctx.conn.write('HTTP/1.1 302 Found\r\nLocation: $url\r\n\r\n$h') 
}

pub fn (ctx Context) not_found(s string) {
	ctx.conn.write(HTTP_404)
}

pub fn (ctx mut Context) set_cookie(key, val string) {
	ctx.set_header('Set-Cookie', '$key=$val')
} 

pub fn (ctx Context) get_cookie(key string) ?string { 
	for k, v in ctx.req.headers {
		if k.eq('Cookie') || k.eq('cookie') {
			cookie := v.split('; ')
			if cookie.len == 2 && key.eq(cookie[0]) { // TODO repace with key.eq(cookie[0])
				return cookie[1]
			}
			return v
		}
	}
	return error('Cookie not found')
	/*
	cookie := ctx.req.headers['Cookie']
	println('get cookie $key : "$cookie"') 
	return cookie.find_between('$key=', ';')
	*/
} 

fn (ctx mut Context) set_header(key, val string) {
	// ctx.resp.headers[key] = val
	ctx.headers[key] = val
}

pub fn (ctx Context) html(html string) { 
	h := ctx.parse_headers()
	ctx.conn.write('HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n$h\r\n\r\n$html')
	 
} 

pub fn run<T>(port int) { 
	println('Running vweb app on http://localhost:$port ...') 
	l := net.listen(port) or { panic('failed to listen') } 
	mut app := T{} 
	app.init() 
	for {
		conn := l.accept() or {
			panic('accept() failed') 
		} 
		// TODO move this to handle_conn<T>(conn, app)
		s := conn.read_line()
		if s == '' {
			conn.write(HTTP_500)
			conn.close()
			continue
		}
		 // Parse request headers
		 lines := s.split_into_lines()
		 mut headers := map[string]string{}
		 for i, line in lines {
			if i == 0 {
				continue
			}
			words := line.split(': ')
			if words.len != 2 {
				continue
			}
			headers[words[0]] = words[1]
		} 
		// Parse the first line
		// "GET / HTTP/1.1"
		first_line := s.all_before('\n')
		vals := first_line.split(' ') 
		mut action := vals[1].right(1).all_before('/') 
		if action.contains('?') {
			action = action.all_before('?') 
		} 
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
		println('vweb action = "$action"') 
		//mut app := T{
		app.vweb = Context{
			req: req 
			conn: conn 
			form: map[string]string{} 
			static_files: map[string]string{} 
			static_mime_types: map[string]string{}
		} 
		//} 
		if req.method in methods_with_form {
			app.vweb.parse_form(s) 
		} 
		if vals.len < 2 {
			println('no vals for http') 
			conn.close()
			continue 
		} 

		// Serve a static file if it's one 
		// if app.vweb.handle_static() {
		// 	conn.close()
		// 	continue 
		// } 

		// Call the right action 
		app.$action() or { 
			conn.write(HTTP_404) 
		}
		conn.close()
	}
} 


fn (ctx mut Context) parse_form(s string) { 
	if !(ctx.req.method in methods_with_form) {
		return 
	} 
	pos := s.index('\r\n\r\n')
	if pos > -1 {
		mut str_form := s.substr(pos, s.len)
		str_form = str_form.replace('+', ' ')
		words := str_form.split('&')
		for word in words {
			println('parse form keyval="$word"') 
			keyval := word.trim_space().split('=') 
			if keyval.len != 2 { continue } 
			key := keyval[0]
			val := urllib.query_unescape(keyval[1]) or {
				continue 
			} 
			println('http form "$key" => "$val"') 
			ctx.form[key] = val 
		}
	}
} 
const ( 
	mime_types = {
		'.css': 'text/css; charset=utf-8', 
		'.gif': 'image/gif', 
		'.htm': 'text/html; charset=utf-8', 
		'.html': 'text/html; charset=utf-8', 
		'.jpg': 'image/jpeg', 
		'.js': 'application/javascript', 
		'.wasm': 'application/wasm', 
		'.pdf': 'application/pdf', 
		'.png': 'image/png', 
		'.svg': 'image/svg+xml', 
		'.xml': 'text/xml; charset=utf-8' 
	} 
) 

fn (ctx mut Context) scan_static_directory(directory_path, mount_path string) {
	files := os.ls(directory_path)
	if files.len > 0 {
		for file in files {			
			mut ext := ''
			mut i := file.len
			mut flag := true
			for i > 0 {
		 		i--
				if flag {
					ext = file.substr(i, i + 1) + ext
				}
				if file.substr(i, i + 1) == '.' {
					flag = false
				}
			}

			// todo: os.is_dir is broken now
			//       so we expect that file is dir it has no extension
			if flag {
				ctx.scan_static_directory(directory_path + '/' + file, mount_path + '/' + file)
			} else {
				ctx.static_files[mount_path + '/' + file] = directory_path + '/' + file 
				ctx.static_mime_types[mount_path + '/' + file] = mime_types[ext]
			}
		}
	}
}

pub fn (ctx mut Context) handle_static(directory_path string) bool { 
	ctx.scan_static_directory(directory_path, '')

	static_file := ctx.static_files[ctx.req.url] 
	mime_type := ctx.static_mime_types[ctx.req.url]

	if static_file != '' { 
		data := os.read_file(static_file) or { return false }  
		ctx.conn.write('HTTP/1.1 200 OK\r\nContent-Type: $mime_type\r\n\r\n$data')
		return true 
	} 
	return false 
} 

pub fn (ctx mut Context) serve_static(url, file_path, mime_type string) { 
	ctx.static_files[url] = file_path 
	ctx.static_mime_types[url] = mime_type
}
