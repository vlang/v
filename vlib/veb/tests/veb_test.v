// vtest retry: 3
// vtest build: !sanitized_job? && !windows // !windows: fasthttp.Server.run not implemented yet
import os
import time
import json2 as json
import net
import net.http
import io
import veb

const veb_server_probe = '--vtest-veb-server'
const veb_logfile = os.getenv('VEB_LOGFILE')
const tcp_r_timeout = 10 * time.second
const tcp_w_timeout = 10 * time.second
const veb_fixture_timeout_seconds = 1

struct VebTestServer {
mut:
	process &os.Process = unsafe { nil }
	address string
}

const veb_test_server = &VebTestServer{}

fn testsuite_begin() {
	if os.args.len == 3 && os.args[1] == veb_server_probe {
		run_veb_test_server(os.args[2].int())
		exit(0)
	}
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := listener.addr()!.port()!
	listener.close()!

	mut child := os.new_process(os.executable())
	child.set_args([veb_server_probe, port.str()])
	if veb_logfile != '' {
		// exec keeps the server in the tracked child process after redirecting its logs.
		child = os.new_process('sh')
		child.set_args(['-c', 'exec "$1" "$2" "$3" >> "$4" 2>&1', 'sh', os.executable(),
			veb_server_probe, port.str(), veb_logfile])
	}
	child.run()
	// The suite hooks share ownership of the child.
	mut server := unsafe { veb_test_server }
	server.address = '127.0.0.1:${port}'
	server.process = child
}

fn test_a_simple_veb_app_runs_in_the_background() {
	sw := time.new_stopwatch()
	for sw.elapsed() < 10 * time.second {
		mut probe := net.dial_tcp(veb_test_server.address) or {
			time.sleep(10 * time.millisecond)
			continue
		}
		probe.close() or {}
		return
	}
	assert false, 'veb test server did not start listening on ${veb_test_server.address}'
}

// web client tests follow
fn assert_common_headers(received string) {
	assert received.starts_with('HTTP/1.1 200 OK\r\n'), received
	assert received.contains('Server: veb\r\n'), received
	assert received.contains('Content-Length:'), received
	assert received.contains('Connection: close\r\n'), received
}

fn test_a_simple_tcp_client_can_connect_to_the_veb_server() {
	received := simple_tcp_client(path: '/') or {
		assert err.msg() == ''
		return
	}
	assert_common_headers(received)
	assert received.contains('Content-Type: text/plain'), received
	assert received.contains('Content-Length: 14'), received
	assert received.ends_with('Welcome to veb'), received
}

fn test_a_simple_tcp_client_simple_route() {
	received := simple_tcp_client(path: '/simple') or {
		assert err.msg() == ''
		return
	}
	assert_common_headers(received)
	assert received.contains('Content-Type: text/plain')
	assert received.contains('Content-Length: 15')
	assert received.ends_with('A simple result')
}

fn test_a_simple_tcp_client_zero_content_length() {
	// tests that sending a content-length header of 0 doesn't hang on a read timeout
	watch := time.new_stopwatch(auto_start: true)
	simple_tcp_client(path: '/', headers: 'Content-Length: 0\r\n\r\n') or {
		assert err.msg() == ''
		return
	}
	assert watch.elapsed() < 1 * time.second
}

fn test_timeout_after_delayed_body() ! {
	// Send the headers and let the server time out waiting for the body.
	watch := time.new_stopwatch()
	res := simple_tcp_client(
		path:       '/json_echo'
		headers:    'Content-Length: 10\r\n\r\n'
		method_str: 'POST'
	)!

	assert res.starts_with('HTTP/1.1 408 Request Timeout\r\n')
	assert res.ends_with('408 Request Timeout')
	elapsed := watch.elapsed()
	assert elapsed >= veb_fixture_timeout_seconds * time.second, '408 arrived after ${elapsed}'
}

fn test_a_simple_tcp_client_html_page() {
	received := simple_tcp_client(path: '/html_page') or {
		assert err.msg() == ''
		return
	}
	assert_common_headers(received)
	assert received.contains('Content-Type: text/html')
	assert received.ends_with('<h1>ok</h1>')
}

// net.http client based tests follow:
fn assert_common_http_headers(x http.Response) ! {
	assert x.status() == .ok
	assert x.header.get(.server) or { '' } == 'veb'
	assert (x.header.get(.content_length) or { '0' }).int() > 0
}

fn test_http_client_index() {
	x := http.get('http://${veb_test_server.address}/') or { panic(err) }
	assert_common_http_headers(x)!
	assert x.header.get(.content_type)? == 'text/plain'
	assert x.body == 'Welcome to veb'
	// The default http client keeps connections alive (it no longer sends
	// `Connection: close`), so veb must not answer with a close either.
	if conn_header := x.header.get(.connection) {
		assert conn_header != 'close'
	}
	// Opting out of connection reuse restores the historical behavior
	// end-to-end: the client sends `Connection: close` and veb honors it.
	y := http.fetch(url: 'http://${veb_test_server.address}/', disable_connection_reuse: true) or { panic(err) }
	assert y.header.get(.connection)? == 'close'
}

fn test_http_client_404() {
	server := 'http://${veb_test_server.address}'
	url_404_list := [
		'/zxcnbnm',
		'/JHKAJA',
		'/unknown',
	]
	for url in url_404_list {
		res := http.get('${server}${url}') or { panic(err) }
		assert res.status() == .not_found
		assert res.body == '404 on "${url}"'
	}
}

fn test_http_client_simple() {
	x := http.get('http://${veb_test_server.address}/simple') or { panic(err) }
	assert_common_http_headers(x)!
	assert x.header.get(.content_type)? == 'text/plain'
	assert x.body == 'A simple result'
}

fn test_http_client_html_page() {
	x := http.get('http://${veb_test_server.address}/html_page') or { panic(err) }
	assert_common_http_headers(x)!
	assert x.header.get(.content_type)? == 'text/html'
	assert x.body == '<h1>ok</h1>'
}

fn test_http_client_settings_page() {
	x := http.get('http://${veb_test_server.address}/bilbo/settings') or { panic(err) }
	assert_common_http_headers(x)!
	assert x.body == 'username: bilbo'

	y := http.get('http://${veb_test_server.address}/kent/settings') or { panic(err) }
	assert_common_http_headers(y)!
	assert y.body == 'username: kent'
}

fn test_http_client_user_repo_settings_page() {
	x := http.get('http://${veb_test_server.address}/bilbo/gostamp/settings') or { panic(err) }
	assert_common_http_headers(x)!
	assert x.body == 'username: bilbo | repository: gostamp'

	y := http.get('http://${veb_test_server.address}/kent/golang/settings') or { panic(err) }
	assert_common_http_headers(y)!
	assert y.body == 'username: kent | repository: golang'

	z := http.get('http://${veb_test_server.address}/missing/golang/settings') or { panic(err) }
	assert z.status() == .not_found
}

struct User {
	name string
	age  int
}

fn test_http_client_json_post() {
	ouser := User{
		name: 'Bilbo'
		age:  123
	}
	json_for_ouser := json.encode(ouser)
	mut x := http.post_json('http://${veb_test_server.address}/json_echo', json_for_ouser) or { panic(err) }
	$if debug_net_socket_client ? {
		eprintln('/json_echo endpoint response: ${x}')
	}
	assert x.header.get(.content_type)? == 'application/json'
	assert x.body == json_for_ouser
	nuser := json.decode[User](x.body) or { User{} }
	assert '${ouser}' == '${nuser}'

	x = http.post_json('http://${veb_test_server.address}/json', json_for_ouser) or { panic(err) }
	$if debug_net_socket_client ? {
		eprintln('/json endpoint response: ${x}')
	}
	assert x.header.get(.content_type)? == 'application/json'
	assert x.body == json_for_ouser
	nuser2 := json.decode[User](x.body) or { User{} }
	assert '${ouser}' == '${nuser2}'
}

fn test_http_client_multipart_form_data() {
	mut form_config := http.PostMultipartFormConfig{
		form: {
			'foo': 'baz buzz'
		}
	}

	mut x := http.post_multipart_form('http://${veb_test_server.address}/form_echo', form_config)!

	$if debug_net_socket_client ? {
		eprintln('/form_echo endpoint response: ${x}')
	}
	assert x.body == form_config.form['foo']

	mut files := []http.FileData{}
	files << http.FileData{
		filename:     'veb'
		content_type: 'text'
		data:         '"veb test"'
	}

	mut form_config_files := http.PostMultipartFormConfig{
		files: {
			'file': files
		}
	}

	x = http.post_multipart_form('http://${veb_test_server.address}/file_echo', form_config_files)!
	$if debug_net_socket_client ? {
		eprintln('/form_echo endpoint response: ${x}')
	}
	assert x.body == files[0].data
}

fn test_login_with_multipart_form_data_send_by_fetch() {
	mut form_config := http.PostMultipartFormConfig{
		form: {
			'username': 'myusername'
			'password': 'mypassword123'
		}
	}
	x := http.post_multipart_form('http://${veb_test_server.address}/login', form_config)!
	assert x.status_code == 200
	assert x.status_msg == 'OK'
	assert x.body == 'username: xmyusernamex | password: xmypassword123x'
}

fn test_query_params_are_passed_as_arguments() {
	x := http.get('http://${veb_test_server.address}/query_echo?c=3&a="test"&b=20')!
	assert x.status() == .ok
	assert x.body == 'a: x"test"x | b: x20x'
}

fn test_host() {
	mut req := http.Request{
		url:    'http://${veb_test_server.address}/with_host'
		method: .get
	}

	mut x := req.do()!
	assert x.status() == .not_found

	req.add_header(.host, 'example.com')
	x = req.do()!
	assert x.status() == .ok
}

fn test_empty_response_body_has_content_length() {
	req := http.Request{
		url:    'http://${veb_test_server.address}/empty_response_body'
		method: .get
	}

	mut x := req.do()!
	assert x.status() == .ok
	assert x.header.get(.content_length)? == '0'
}

fn test_http_client_shutdown_does_not_work_without_a_cookie() {
	x := http.get('http://${veb_test_server.address}/shutdown') or {
		assert err.msg() == ''
		return
	}
	assert x.status() == .not_found
}

fn testsuite_end() {
	mut child := veb_test_server.process
	defer {
		child.signal_kill()
		child.wait()
		child.close()
	}
	// Run the authenticated shutdown check after the tests have used the server.
	x := http.fetch(
		url:     'http://${veb_test_server.address}/shutdown'
		method:  .get
		cookies: {
			'skey': 'superman'
		}
	) or {
		assert false, 'could not shut down the fixture: ${err}'
		return
	}
	assert x.status() == .ok
	assert x.body == 'good bye'
	sw := time.new_stopwatch()
	for child.is_alive() && sw.elapsed() < 2 * time.second {
		time.sleep(10 * time.millisecond)
	}
	assert child.status == .exited, 'veb test server did not exit after shutdown'
	assert child.code == 0
}

// utility code:
struct SimpleTcpClientConfig {
	retries    int    = 4
	host       string = 'static.dev'
	path       string = '/'
	agent      string = 'v/net.tcp.v'
	headers    string = '\r\n'
	content    string
	method_str string = 'GET'
}

@[unsafe]
fn simple_tcp_client(config SimpleTcpClientConfig) !string {
	mut client := &net.TcpConn(unsafe { nil })
	mut tries := 0
	for tries < config.retries {
		tries++
		eprintln('> client retries: ${tries}')
		client = net.dial_tcp(veb_test_server.address) or {
			eprintln('dial error: ${err.msg()}')
			if tries > config.retries {
				return err
			}
			time.sleep(100 * time.millisecond)
			continue
		}
		break
	}
	if client == unsafe { nil } {
		eprintln('could not create a tcp client connection to http://${veb_test_server.address} after ${config.retries} retries')
		exit(1)
	}
	client.set_read_timeout(tcp_r_timeout)
	client.set_write_timeout(tcp_w_timeout)
	defer {
		client.close() or {}
	}
	message := '${config.method_str} ${config.path} HTTP/1.1
Host: ${config.host}
User-Agent: ${config.agent}
Accept: */*
Connection: close
${config.headers}
${config.content}'
	$if debug_net_socket_client ? {
		eprintln('sending:\n${message}')
	}
	client.write(message.bytes())!
	read := io.read_all(reader: client)!
	$if debug_net_socket_client ? {
		eprintln('received:\n${read}')
	}
	return read.bytestr()
}

// for issue 20476
// phenomenon: parsing url error when querypath is `//`
fn test_empty_querypath() {
	mut x := http.get('http://${veb_test_server.address}') or { panic(err) }
	assert x.body == 'Welcome to veb'
	x = http.get('http://${veb_test_server.address}/') or { panic(err) }
	assert x.body == 'Welcome to veb'
	x = http.get('http://${veb_test_server.address}//') or { panic(err) }
	assert x.body == 'Welcome to veb'
	x = http.get('http://${veb_test_server.address}///') or { panic(err) }
	assert x.body == 'Welcome to veb'
}

fn test_large_response() {
	received := simple_tcp_client(path: '/large_response') or { panic(err) }
	assert_common_headers(received)
	assert received.ends_with('}]')
	assert received.len == 830778
}

const known_users = ['bilbo', 'kent']

struct ServerContext {
	veb.Context
}

// not_found renders the custom 404 page.
pub fn (mut ctx ServerContext) not_found() veb.Result {
	ctx.res.set_status(.not_found)
	return ctx.html('404 on "${ctx.req.url}"')
}

pub struct ServerApp {
mut:
	server        &veb.Server = unsafe { nil }
	global_config Config
}

struct Config {
	max_ping int
}

fn run_veb_test_server(port int) {
	spawn fn () {
		time.sleep(12 * time.second)
		eprintln('veb test server timed out')
		exit(1)
	}()
	mut app := &ServerApp{
		global_config: Config{
			max_ping: 50
		}
	}
	veb.run_at[ServerApp, ServerContext](mut app,
		host:               'localhost'
		port:               port
		family:             .ip
		timeout_in_seconds: veb_fixture_timeout_seconds
	) or { panic(err) }
}

// init_server stores the veb server handle when lifecycle control is available.
pub fn (mut app ServerApp) init_server(server &veb.Server) {
	app.server = server
}

// index returns the welcome message.
pub fn (mut app ServerApp) index(mut ctx ServerContext) veb.Result {
	assert app.global_config.max_ping == 50
	return ctx.text('Welcome to veb')
}

// simple returns a plain-text response.
pub fn (mut app ServerApp) simple(mut ctx ServerContext) veb.Result {
	return ctx.text('A simple result')
}

// html_page returns the HTML fixture.
pub fn (mut app ServerApp) html_page(mut ctx ServerContext) veb.Result {
	return ctx.html('<h1>ok</h1>')
}

// the following serve custom routes
// settings renders settings for a known user.
@['/:user/settings']
pub fn (mut app ServerApp) settings(mut ctx ServerContext, username string) veb.Result {
	if username !in known_users {
		return ctx.not_found()
	}
	return ctx.html('username: ${username}')
}

// user_repo_settings renders repository settings for a known user.
@['/:user/:repo/settings']
pub fn (mut app ServerApp) user_repo_settings(mut ctx ServerContext, username string, repository string) veb.Result {
	if username !in known_users {
		return ctx.not_found()
	}
	return ctx.html('username: ${username} | repository: ${repository}')
}

// json_echo returns the request body with its content type.
@['/json_echo'; post]
pub fn (mut app ServerApp) json_echo(mut ctx ServerContext) veb.Result {
	ctx.set_content_type(ctx.req.header.get(.content_type) or { '' })
	return ctx.ok(ctx.req.data)
}

// login_form echoes the parsed form fields.
@['/login'; post]
pub fn (mut app ServerApp) login_form(mut ctx ServerContext, username string, password string) veb.Result {
	return ctx.html('username: x${username}x | password: x${password}x')
}

// form_echo echoes the form value with the request content type.
@['/form_echo'; post]
pub fn (mut app ServerApp) form_echo(mut ctx ServerContext) veb.Result {
	ctx.set_content_type(ctx.req.header.get(.content_type) or { '' })
	return ctx.ok(ctx.form['foo'])
}

// file_echo echoes the uploaded file.
@['/file_echo'; post]
pub fn (mut app ServerApp) file_echo(mut ctx ServerContext) veb.Result {
	if 'file' !in ctx.files {
		ctx.res.set_status(.internal_server_error)
		return ctx.text('no file')
	}

	return ctx.text(ctx.files['file'][0].data)
}

// query_echo echoes the parsed query arguments.
@['/query_echo']
pub fn (mut app ServerApp) query_echo(mut ctx ServerContext, a string, b int) veb.Result {
	return ctx.text('a: x${a}x | b: x${b}x')
}

// Make sure [post] works without the path
// json echoes the request body from a route using only the post attribute.
@[post]
pub fn (mut app ServerApp) json(mut ctx ServerContext) veb.Result {
	ctx.set_content_type(ctx.req.header.get(.content_type) or { '' })
	return ctx.ok(ctx.req.data)
}

// with_host returns an empty response for the configured host.
@[host: 'example.com']
@['/with_host']
pub fn (mut app ServerApp) with_host(mut ctx ServerContext) veb.Result {
	return ctx.ok('')
}

// empty_response_body returns an empty successful response.
@['/empty_response_body']
pub fn (mut app ServerApp) empty_response_body(mut ctx ServerContext) veb.Result {
	return ctx.ok('')
}

// shutdown stops the fixture gracefully after an authenticated request.
pub fn (mut app ServerApp) shutdown(mut ctx ServerContext) veb.Result {
	session_key := ctx.get_cookie('skey') or { return ctx.not_found() }
	if session_key != 'superman' {
		return ctx.not_found()
	}
	spawn app.exit_gracefully()
	return ctx.ok('good bye')
}

fn (mut app ServerApp) exit_gracefully() {
	eprintln('>> webserver: exit_gracefully')
	app.server.shutdown(timeout: 2 * time.second) or { panic(err) }
}

// large_response returns the large JSON fixture.
pub fn (mut app ServerApp) large_response(mut ctx ServerContext) veb.Result {
	struct Tick {
		date_time string
		timestamp i64
		value     f64
	}

	ticks := []Tick{len: 16000, init: Tick{
		date_time: index.str()
		timestamp: index
		value:     index
	}}

	return ctx.json(ticks)
}
