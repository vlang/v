import os
import time
import net.http

const sport = 12382
const sport2 = 12383
const localserver = '127.0.0.1:${sport}'
const exit_after_time = 12000 // milliseconds

const vexe = os.getenv('VEXE')
const vweb_logfile = os.getenv('VWEB_LOGFILE')
const vroot = os.dir(vexe)
const serverexe = os.join_path(os.cache_dir(), 'controller_test_server.exe')
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
	// did_server_compile := os.system('${os.quoted_path(vexe)} -g -o ${os.quoted_path(serverexe)} vlib/vweb/tests/controller_test_server.vv')
	// TODO: find out why it does not compile with -usecache and -g
	did_server_compile := os.system('${os.quoted_path(vexe)} -o ${os.quoted_path(serverexe)} vlib/vweb/tests/controller_test_server.v')
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

// test functions:

fn test_app_home() {
	x := http.get('http://${localserver}/') or { panic(err) }
	assert x.body == 'App'
}

fn test_app_path() {
	x := http.get('http://${localserver}/path') or { panic(err) }
	assert x.body == 'App path'
}

fn test_admin_home() {
	x := http.get('http://${localserver}/admin/') or { panic(err) }
	assert x.body == 'Admin'
}

fn test_admin_path() {
	x := http.get('http://${localserver}/admin/path') or { panic(err) }
	assert x.body == 'Admin path'
}

fn test_other_home() {
	x := http.get('http://${localserver}/other/') or { panic(err) }
	assert x.body == 'Other'
}

fn test_other_path() {
	x := http.get('http://${localserver}/other/path') or { panic(err) }
	assert x.body == 'Other path'
}

fn test_other_hided_home() {
	x := http.get('http://${localserver}/other/hide') or { panic(err) }
	assert x.body == 'Other'
}

fn test_other_hided_path() {
	x := http.get('http://${localserver}/other/hide/path') or { panic(err) }
	assert x.body == 'Other path'
}

fn test_different_404() {
	res_app := http.get('http://${localserver}/zxcnbnm') or { panic(err) }
	assert res_app.status() == .not_found
	assert res_app.body == '404 From App'

	res_admin := http.get('http://${localserver}/admin/JHKAJA') or { panic(err) }
	assert res_admin.status() == .not_found
	assert res_admin.body == '404 From Admin'

	// Other doesn't have a custom 404 so the vweb.Context's not_found is expected
	res_other := http.get('http://${localserver}/other/unknown') or { panic(err) }
	assert res_other.status() == .not_found
	assert res_other.body == '404 Not Found'
}

fn test_shutdown() {
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

fn test_duplicate_route() {
	did_server_compile := os.system('${os.quoted_path(vexe)} -o ${os.quoted_path(serverexe)} vlib/vweb/tests/controller_duplicate_server.v')
	assert did_server_compile == 0
	assert os.exists(serverexe)

	mut suffix := ''

	if vweb_logfile != '' {
		suffix = ' 2>> ${os.quoted_path(vweb_logfile)} >> ${os.quoted_path(vweb_logfile)} &'
	}
	server_exec_cmd := '${os.quoted_path(serverexe)} ${sport2} ${exit_after_time} ${suffix}'
	$if debug_net_socket_client ? {
		eprintln('running:\n${server_exec_cmd}')
	}
	$if windows {
		task := spawn os.execute(server_exec_cmd)
		res := task.wait()
		assert res.output.contains('V panic: conflicting paths')
	} $else {
		res := os.execute(server_exec_cmd)
		assert res.output.contains('V panic: conflicting paths')
	}
}
