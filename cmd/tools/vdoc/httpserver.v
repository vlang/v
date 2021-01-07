module main

import io
import net
import strings

fn (mut vd VDoc) serve_html(out Output) {
	cfg := vd.cfg
	if out.typ == .html {
		vd.render_static_html(true, out)
	}
	docs := vd.render(out)
	dkeys := docs.keys()
	if dkeys.len < 1 {
		eprintln('no documentation created, the module has no `pub` functions')
		exit(1)
	}
	def_name := docs.keys()[0]
	server_url := 'http://localhost:' + cfg.server_port.str()
	server := net.listen_tcp(cfg.server_port) or { panic(err) }
	println('Serving docs on: $server_url')
	if cfg.open_docs {
		open_url(server_url)
	}
	content_type := match out.typ {
		.html { 'text/html' }
		.markdown { 'text/markdown' }
		.json { 'application/json' }
		else { 'text/plain' }
	}
	server_context := VdocHttpServerContext{
		docs: docs
		content_type: content_type
		default_filename: def_name
	}
	for {
		mut conn := server.accept() or {
			server.close() or { }
			panic(err)
		}
		handle_http_connection(mut conn, server_context)
		conn.close() or { eprintln('error closing the connection: $err') }
	}
}

struct VdocHttpServerContext {
	docs             map[string]string
	content_type     string
	default_filename string
}

fn handle_http_connection(mut con net.TcpConn, ctx &VdocHttpServerContext) {
	mut reader := io.new_buffered_reader(reader: io.make_reader(con))
	first_line := reader.read_line() or {
		send_http_response(mut con, 501, ctx.content_type, 'bad request')
		return
	}
	request_parts := first_line.split(' ')
	if request_parts.len != 3 {
		send_http_response(mut con, 501, ctx.content_type, 'bad request')
		return
	}
	urlpath := request_parts[1]
	filename := if urlpath == '/' {
		ctx.default_filename.trim_left('/')
	} else {
		urlpath.trim_left('/')
	}
	if ctx.docs[filename].len == 0 {
		send_http_response(mut con, 404, ctx.content_type, 'file not found')
		return
	}
	send_http_response(mut con, 200, ctx.content_type, ctx.docs[filename])
}

fn send_http_response(mut con net.TcpConn, http_code int, content_type string, html string) {
	content_length := html.len.str()
	shttp_code := http_code.str()
	mut http_response := strings.new_builder(20000)
	http_response.write('HTTP/1.1 ')
	http_response.write(shttp_code)
	http_response.write(' OK\r\n')
	http_response.write('Server: VDoc\r\n')
	http_response.write('Content-Type: ')
	http_response.write(content_type)
	http_response.write('\r\n')
	http_response.write('Content-Length: ')
	http_response.write(content_length)
	http_response.write('\r\n')
	http_response.write('Connection: close\r\n')
	http_response.write('\r\n')
	http_response.write(html)
	sresponse := http_response.str()
	con.write_str(sresponse) or { eprintln('error sending http response: $err') }
}
