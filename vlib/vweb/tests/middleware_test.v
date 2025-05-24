import os
import time
import x.json2 as json
import net
import net.http
import io

const sport = 12381
const localserver = '127.0.0.1:${sport}'
const exit_after_time = 12000 // milliseconds

const vexe = os.getenv('VEXE')
const vweb_logfile = os.getenv('VWEB_LOGFILE')
const vroot = os.dir(vexe)
const serverexe = os.join_path(os.cache_dir(), 'middleware_test_server.exe')
const tcp_r_timeout = 30 * time.second
const tcp_w_timeout = 30 * time.second

// setup of vweb webserver
fn testsuite_begin() {
	os.chdir(vroot) or {}
	if os.exists(serverexe) {
		os.rm(serverexe) or {}
	}
}

fn test_middleware_vweb_app_can_be_compiled() {
	// did_server_compile := os.system('${os.quoted_path(vexe)} -g -o ${os.quoted_path(serverexe)} vlib/vweb/tests/middleware_test_server.vv')
	// TODO: find out why it does not compile with -usecache and -g
	did_server_compile := os.system('${os.quoted_path(vexe)} -o ${os.quoted_path(serverexe)} vlib/vweb/tests/middleware_test_server.v')
	assert did_server_compile == 0
	assert os.exists(serverexe)
}

fn test_middleware_vweb_app_runs_in_the_background() {
	mut suffix := ''
	$if !windows {
		suffix = ' > /dev/null &'
	}
	if vweb_logfile != '' {
		suffix = ' 2>> ${os.quoted_path(vweb_logfile)} >> ${os.quoted_path(vweb_logfile)} &'
	}
	server_exec_cmd := '${os.quoted_path(serverexe)} ${sport} ${exit_after_time} ${suffix}'
	$if debug_net_socket_client ? {
		eprintln('running:\n${server_exec_cmd}')
	}
	$if windows {
		spawn os.system(server_exec_cmd)
	} $else {
		res := os.system(server_exec_cmd)
		assert res == 0
	}
	$if macos {
		time.sleep(1000 * time.millisecond)
	} $else {
		time.sleep(100 * time.millisecond)
	}
}

// normal routes:

fn test_app_middleware() {
	x := http.get('http://${localserver}/') or { panic(err) }
	assert x.body == '0app_middlewareindex'
}

fn test_single_middleware() {
	received := simple_tcp_client(path: '/single') or {
		assert err.msg() == ''
		return
	}
	assert received.starts_with('m1HTTP/')
	assert received.ends_with('0single')
}

fn test_multiple_middleware() {
	received := simple_tcp_client(path: '/multiple') or {
		assert err.msg() == ''
		return
	}
	assert received.starts_with('m1m2HTTP/')
	assert received.ends_with('0multiple')
}

fn test_combined_middleware() {
	received := simple_tcp_client(path: '/combined') or {
		assert err.msg() == ''
		return
	}
	assert received.starts_with('m1m2HTTP/')
	assert received.ends_with('0app_middlewarecombined')
}

fn test_nested_middleware() {
	received := simple_tcp_client(path: '/admin/nested') or {
		assert err.msg() == ''
		return
	}
	assert received.starts_with('m1HTTP/')
	assert received.ends_with('0nested')
}

// above routes + post

struct Post {
	msg string
}

fn test_app_post_middleware() {
	test_object := Post{
		msg: 'HI'
	}
	json_test := json.encode(test_object)
	mut x := http.post_json('http://${localserver}/index_post', json_test) or { panic(err) }
	assert x.body == '0app_middlewareindex_post:${json_test}'
}

fn test_single_post_middleware() {
	test_object := Post{
		msg: 'HI'
	}
	json_test := json.encode(test_object)

	received := simple_tcp_client_post_json(
		path:    '/single_post'
		headers: 'Content-Length: ${json_test.len}\r\n'
		content: json_test
	) or {
		assert err.msg() == ''
		return
	}

	assert received.starts_with('m1')
	assert received.ends_with('0single_post:${json_test}')
}

fn test_multiple_post_middleware() {
	test_object := Post{
		msg: 'HI'
	}
	json_test := json.encode(test_object)

	received := simple_tcp_client_post_json(
		path:    '/multiple_post'
		headers: 'Content-Length: ${json_test.len}\r\n'
		content: json_test
	) or {
		assert err.msg() == ''
		return
	}

	assert received.starts_with('m1m2')
	assert received.ends_with('0multiple_post:${json_test}')
}

fn test_combined_post_middleware() {
	test_object := Post{
		msg: 'HI'
	}
	json_test := json.encode(test_object)

	received := simple_tcp_client_post_json(
		path:    '/combined_post'
		headers: 'Content-Length: ${json_test.len}\r\n'
		content: json_test
	) or {
		assert err.msg() == ''
		return
	}
	assert received.starts_with('m1m2')
	assert received.ends_with('0app_middlewarecombined_post:${json_test}')
}

fn test_nested_post_middleware() {
	test_object := Post{
		msg: 'HI'
	}
	json_test := json.encode(test_object)

	received := simple_tcp_client_post_json(
		path:    '/admin/nested_post'
		headers: 'Content-Length: ${json_test.len}\r\n'
		content: json_test
	) or {
		assert err.msg() == ''
		return
	}
	assert received.starts_with('m1')
	assert received.ends_with('0nested_post:${json_test}')
}

// dynamic routes:

fn test_dynamic_middleware() {
	dynamic_path := 'test'
	received := simple_tcp_client(path: '/admin/${dynamic_path}') or {
		assert err.msg() == ''
		return
	}
	assert received.starts_with('m1HTTP/')
	assert received.ends_with('0admin_dynamic:${dynamic_path}')
}

fn test_combined_dynamic_middleware() {
	dynamic_path := 'test'
	received := simple_tcp_client(path: '/other/${dynamic_path}') or {
		assert err.msg() == ''
		return
	}
	assert received.starts_with('m1m2HTTP/')
	assert received.ends_with('0app_middlewarecombined_dynamic:${dynamic_path}')
}

// redirect routes:

fn test_app_redirect_middleware() {
	x := http.get('http://${localserver}/app_redirect') or { panic(err) }
	x_home := http.get('http://${localserver}/') or { panic(err) }
	assert x.body == x_home.body

	received := simple_tcp_client(path: '/app_redirect') or {
		assert err.msg() == ''
		return
	}
	assert received.starts_with('HTTP/1.1 302 Found')
	assert received.ends_with('302 Found')
}

fn test_redirect_middleware() {
	received := simple_tcp_client(path: '/redirect') or {
		assert err.msg() == ''
		return
	}
	println(received)

	assert received.starts_with('m_redirect')
	assert received.contains('HTTP/1.1 302 Found')
	assert received.ends_with('302 Found')
}

// Context's

fn test_middleware_with_context() {
	x := http.get('http://${localserver}/with-context') or { panic(err) }
	assert x.body == 'b'
}

fn testsuite_end() {
	// This test is guaranteed to be called last.
	// It sends a request to the server to shutdown.
	x := http.fetch(
		url:     'http://${localserver}/shutdown'
		method:  .get
		cookies: {
			'skey': 'superman'
		}
	) or {
		assert err.msg() == ''
		return
	}
	assert x.status() == .ok
	assert x.body == 'good bye'
}

// utility code:
struct SimpleTcpClientConfig {
	retries int    = 20
	host    string = 'static.dev'
	path    string = '/'
	agent   string = 'v/net.tcp.v'
	headers string = '\r\n'
	content string
}

fn simple_tcp_client(config SimpleTcpClientConfig) !string {
	mut client := &net.TcpConn(unsafe { nil })
	mut tries := 0
	for tries < config.retries {
		tries++
		eprintln('> client retries: ${tries}')
		client = net.dial_tcp(localserver) or {
			if tries > config.retries {
				return err
			}
			time.sleep(100 * time.millisecond)
			continue
		}
		break
	}
	if client == unsafe { nil } {
		eprintln('could not create a tcp client connection to ${localserver} after ${config.retries} retries')
		exit(1)
	}
	client.set_read_timeout(tcp_r_timeout)
	client.set_write_timeout(tcp_w_timeout)
	defer {
		client.close() or {}
	}
	message := 'GET ${config.path} HTTP/1.1
Host: ${config.host}
User-Agent: ${config.agent}
Accept: */*
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

fn simple_tcp_client_post_json(config SimpleTcpClientConfig) !string {
	mut client := &net.TcpConn(unsafe { nil })
	mut tries := 0
	for tries < config.retries {
		tries++
		eprintln('> client retries: ${tries}')
		client = net.dial_tcp(localserver) or {
			if tries > config.retries {
				return err
			}
			time.sleep(100 * time.millisecond)
			continue
		}
		break
	}
	if client == unsafe { nil } {
		eprintln('could not create a tcp client connection to ${localserver} after ${config.retries} retries')
		exit(1)
	}
	client.set_read_timeout(tcp_r_timeout)
	client.set_write_timeout(tcp_w_timeout)
	defer {
		client.close() or {}
	}
	message := 'POST ${config.path} HTTP/1.1
Host: ${config.host}
User-Agent: ${config.agent}
Accept: */*
Content-Type: application/json
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
