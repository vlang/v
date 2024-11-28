import os
import log
import time
import veb
import net.http

const vexe = os.getenv('VEXE')
const vroot = os.dir(vexe)
const port = 48872
const welcome_text = 'Welcome to our simple veb server'

// Use a known good http client like `curl` (if it exists):
const curl_executable = os.find_abs_path_of_executable('curl') or { '' }
const curl_ok = curl_supports_ipv6()

fn curl_supports_ipv6() bool {
	if curl_executable == '' {
		return false
	}
	curl_res := os.execute('${curl_executable} --version')
	if curl_res.exit_code != 0 {
		return false
	}
	if !curl_res.output.match_glob('curl*Features: * IPv6 *') {
		return false
	}
	return true
}

fn testsuite_begin() {
	log.set_level(.debug)
	log.debug(@FN)
	os.chdir(vroot) or {}
	if curl_ok {
		log.info('working curl_executable found at: ${curl_executable}')
	} else {
		log.warn('no working working curl_executable found')
	}
	start_services()
}

fn testsuite_end() {
	log.debug(@FN)
}

//

fn ensure_curl_works(tname string) ? {
	if !curl_ok {
		log.warn('skipping test ${tname}, since it needs a working curl')
		return none
	}
}

fn test_curl_connecting_through_ipv4_works() {
	ensure_curl_works(@FN) or { return }
	res := os.execute('${curl_executable} --connect-timeout 0.5 --silent http://127.0.0.1:${port}/')
	assert res.exit_code == 0, res.output
	assert res.output == welcome_text
	log.info('> ${@FN}')
}

fn test_net_http_connecting_through_ipv4_works() {
	res := http.get('http://127.0.0.1:${port}/')!
	assert res.status_code == 200, res.str()
	assert res.status_msg == 'OK', res.str()
	assert res.body == welcome_text, res.str()
	log.info('> ${@FN}')
}

fn test_curl_connecting_through_ipv6_works() {
	ensure_curl_works(@FN) or { return }
	res := os.execute('${curl_executable} --silent --connect-timeout 0.5 http://[::1]:${port}/')
	assert res.exit_code == 0, res.output
	assert res.output == welcome_text
	log.info('> ${@FN}')
}

fn test_net_http_connecting_through_ipv6_works() {
	$if windows {
		log.warn('skipping test ${@FN} on windows for now')
		return
	}
	res := http.get('http://[::1]:${port}/')!
	assert res.status_code == 200, res.str()
	assert res.status_msg == 'OK', res.str()
	assert res.body == welcome_text, res.str()
	log.info('> ${@FN}')
}

//

pub struct Context {
	veb.Context
}

pub struct App {
mut:
	started chan bool
}

pub fn (mut app App) before_accept_loop() {
	app.started <- true
}

pub fn (mut app App) index(mut ctx Context) veb.Result {
	return ctx.text(welcome_text)
}

fn start_services() {
	log.debug('starting watchdog thread to ensure the test will always exit one way or another...')
	spawn fn (timeout_in_ms int) {
		time.sleep(timeout_in_ms * time.millisecond)
		log.error('Timeout of ${timeout_in_ms} ms reached, for webserver: pid: ${os.getpid()}. Exiting ...')
		exit(1)
	}(10_000)

	log.debug('starting webserver...')
	mut app := &App{}
	spawn veb.run[App, Context](mut app, port)
	_ := <-app.started
	log.debug('webserver started')
}
