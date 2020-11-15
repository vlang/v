import os
import time
import json
import net
import net.http
import io

const (
	sport           = 12380
	exit_after_time = 5000 // milliseconds
	vexe            = os.getenv('VEXE')
	vroot           = os.dir(vexe)
	serverexe       = os.join_path(os.cache_dir(), 'vweb_test_server.exe')
	tcp_r_timeout   = 30 * time.second
	tcp_w_timeout   = 30 * time.second
)

// setup of vweb webserver
fn testsuite_begin() {
	os.chdir(vroot)
	if os.exists(serverexe) {
		os.rm(serverexe)
	}
}

fn test_a_simple_vweb_app_can_be_compiled() {
	did_server_compile := os.system('$vexe -g -o $serverexe vlib/vweb/tests/vweb_test_server.v')
	assert did_server_compile == 0
	assert os.exists(serverexe)
}

fn test_a_simple_vweb_app_runs_in_the_background() {
	suffix := $if windows { '' } $else { ' > /dev/null &' }
	server_exec_cmd := '$serverexe $sport $exit_after_time $suffix'
	$if debug_net_socket_client ? {
		eprintln('running:\n$server_exec_cmd')
	}
	$if windows {
		go os.system(server_exec_cmd)
	} $else {
		res := os.system(server_exec_cmd)
		assert res == 0
	}
	time.sleep_ms(100)
}

// web client tests follow
fn assert_common_headers(received string) {
	assert received.starts_with('HTTP/1.1 200 OK\r\n')
	assert received.contains('Server: VWeb\r\n')
	assert received.contains('Content-Length:')
	assert received.contains('Connection: close\r\n')
}

fn test_a_simple_tcp_client_can_connect_to_the_vweb_server() {
	received := simple_tcp_client({}) or {
		assert err == ''
		return
	}
	assert_common_headers(received)
	assert received.contains('Content-Type: text/plain')
	assert received.contains('Content-Length: 15')
	assert received.ends_with('Welcome to VWeb')
}

fn test_a_simple_tcp_client_simple_route() {
	received := simple_tcp_client({
		path: '/simple'
	}) or {
		assert err == ''
		return
	}
	assert_common_headers(received)
	assert received.contains('Content-Type: text/plain')
	assert received.contains('Content-Length: 15')
	assert received.ends_with('A simple result')
}

fn test_a_simple_tcp_client_html_page() {
	received := simple_tcp_client({
		path: '/html_page'
	}) or {
		assert err == ''
		return
	}
	assert_common_headers(received)
	assert received.contains('Content-Type: text/html')
	assert received.ends_with('<h1>ok</h1>')
}

// net.http client based tests follow:
fn assert_common_http_headers(x http.Response) {
	assert x.status_code == 200
	assert x.headers['Server'] == 'VWeb'
	assert x.headers['Content-Length'].int() > 0
	assert x.headers['Connection'] == 'close'
}

fn test_http_client_index() {
	x := http.get('http://127.0.0.1:$sport/') or {
		panic(err)
	}
	assert_common_http_headers(x)
	assert x.headers['Content-Type'] == 'text/plain'
	assert x.text == 'Welcome to VWeb'
}

fn test_http_client_404() {
	url_404_list := [
		'http://127.0.0.1:$sport/zxcnbnm',
		'http://127.0.0.1:$sport/JHKAJA',
		'http://127.0.0.1:$sport/unknown',
	]
	for url in url_404_list {
		res := http.get(url) or {
			panic(err)
		}
		assert res.status_code == 404
	}
}

fn test_http_client_simple() {
	x := http.get('http://127.0.0.1:$sport/simple') or {
		panic(err)
	}
	assert_common_http_headers(x)
	assert x.headers['Content-Type'] == 'text/plain'
	assert x.text == 'A simple result'
}

fn test_http_client_html_page() {
	x := http.get('http://127.0.0.1:$sport/html_page') or {
		panic(err)
	}
	assert_common_http_headers(x)
	assert x.headers['Content-Type'] == 'text/html'
	assert x.text == '<h1>ok</h1>'
}

fn test_http_client_settings_page() {
	x := http.get('http://127.0.0.1:$sport/bilbo/settings') or {
		panic(err)
	}
	assert_common_http_headers(x)
	assert x.text == 'username: bilbo'
	//
	y := http.get('http://127.0.0.1:$sport/kent/settings') or {
		panic(err)
	}
	assert_common_http_headers(y)
	assert y.text == 'username: kent'
}

fn test_http_client_user_repo_settings_page() {
	x := http.get('http://127.0.0.1:$sport/bilbo/gostamp/settings') or {
		panic(err)
	}
	assert_common_http_headers(x)
	assert x.text == 'username: bilbo | repository: gostamp'
	//
	y := http.get('http://127.0.0.1:$sport/kent/golang/settings') or {
		panic(err)
	}
	assert_common_http_headers(y)
	assert y.text == 'username: kent | repository: golang'
	//
	z := http.get('http://127.0.0.1:$sport/missing/golang/settings') or {
		panic(err)
	}
	assert z.status_code == 404
}

struct User {
	name string
	age  int
}

fn test_http_client_json_post() {
	ouser := User{
		name: 'Bilbo'
		age: 123
	}
	json_for_ouser := json.encode(ouser)
	x := http.post_json('http://127.0.0.1:$sport/json_echo', json_for_ouser) or {
		panic(err)
	}
	$if debug_net_socket_client ? {
		eprintln('json response: $x')
	}
	assert x.headers['Content-Type'] == 'application/json'
	assert x.text == json_for_ouser
	nuser := json.decode(User, x.text) or {
		User{}
	}
	assert '$ouser' == '$nuser'
}

fn test_http_client_shutdown_does_not_work_without_a_cookie() {
	x := http.get('http://127.0.0.1:$sport/shutdown') or {
		assert err == ''
		return
	}
	assert x.status_code == 404
	assert x.text == '404 Not Found'
}

fn testsuite_end() {
	// This test is guaranteed to be called last.
	// It sends a request to the server to shutdown.
	x := http.fetch('http://127.0.0.1:$sport/shutdown', {
		method: .get
		cookies: {
			'skey': 'superman'
		}
	}) or {
		assert err == ''
		return
	}
	assert x.status_code == 200
	assert x.text == 'good bye'
}

// utility code:
struct SimpleTcpClientConfig {
	retries int = 20
	host    string = 'static.dev'
	path    string = '/'
	agent   string = 'v/net.tcp.v'
	headers string
	content string
}

fn simple_tcp_client(config SimpleTcpClientConfig) ?string {
	mut client := net.TcpConn{}
	mut tries := 0
	for tries < config.retries {
		tries++
		client = net.dial_tcp('127.0.0.1:$sport') or {
			if tries > config.retries {
				return error(err)
			}
			time.sleep_ms(100)
			continue
		}
		break
	}
	client.set_read_timeout(tcp_r_timeout)
	client.set_write_timeout(tcp_w_timeout)
	defer {
		client.close()
	}
	message := 'GET $config.path HTTP/1.1
Host: $config.host
User-Agent: $config.agent
Accept: */*
$config.headers
$config.content'
	$if debug_net_socket_client ? {
		eprintln('sending:\n$message')
	}
	client.write(message.bytes()) ?
	read := io.read_all(client) ?
	$if debug_net_socket_client ? {
		eprintln('received:\n$read')
	}
	return read.bytestr()
}
