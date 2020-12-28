module main

import os
import testing
import v.pref

const (
	skip_with_sanitize  = [
		'vlib/x/websocket/websocket_test.v'
		'vlib/encoding/csv/reader_test.v',
		'vlib/net/tcp_test.v',
		'vlib/net/tcp_simple_client_server_test.v',
		'vlib/net/udp_test.v',
		'vlib/net/http/cookie_test.v',
		'vlib/net/http/http_test.v',
		'vlib/net/http/status_test.v',
		'vlib/orm/orm_test.v',
		'vlib/sqlite/sqlite_test.v',
		'vlib/vweb/tests/vweb_test.v',
		'vlib/x/websocket/websocket_test.v'
	]
	skip_test_files     = [
		'vlib/net/http/http_httpbin_test.v',
	]
	skip_on_musl        = [
		'vlib/v/tests/profile/profile_test.v',
	]
	skip_on_ubuntu_musl = [
		// 'vlib/v/gen/js/jsgen_test.v',
		'vlib/net/http/cookie_test.v',
		'vlib/net/http/http_test.v',
		'vlib/net/http/status_test.v',
		'vlib/net/websocket/ws_test.v',
		'vlib/sqlite/sqlite_test.v',
		'vlib/orm/orm_test.v',
		'vlib/clipboard/clipboard_test.v',
		'vlib/vweb/tests/vweb_test.v',
		'vlib/x/websocket/websocket_test.v',
	]
	skip_on_linux       = []string{}
	skip_on_non_linux   = [
		'vlib/net/websocket/ws_test.v',
	]
	skip_on_windows     = [
		'vlib/orm/orm_test.v',
		'vlib/net/websocket/ws_test.v',
		'vlib/x/websocket/websocket_test.v',
		'vlib/vweb/tests/vweb_test.v',
	]
	skip_on_non_windows = []string{}
	skip_on_macos       = []string{}
	skip_on_non_macos   = []string{}
)

// NB: musl misses openssl, thus the http tests can not be done there
// NB: http_httpbin_test.v: fails with 'cgen error: json: map_string_string is not struct'
fn main() {
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	os.chdir(vroot)
	args := os.args.clone()
	args_string := args[1..].join(' ')
	cmd_prefix := args_string.all_before('test-fixed')
	title := 'testing all fixed tests'
	all_test_files := os.walk_ext(os.join_path(vroot, 'vlib'), '_test.v')
	testing.eheader(title)
	mut tsession := testing.new_test_session(cmd_prefix)
	tsession.files << all_test_files
	tsession.skip_files << skip_test_files
	mut sanitize := false
	for arg in args {
		if '-fsanitize' in arg {
			sanitize = true
		}
	}
	if sanitize {
		tsession.skip_files << skip_with_sanitize
	}
	//
	if os.getenv('V_CI_MUSL').len > 0 {
		tsession.skip_files << skip_on_musl
	}
	if os.getenv('V_CI_UBUNTU_MUSL').len > 0 {
		tsession.skip_files << skip_on_ubuntu_musl
	}
	$if !linux {
		tsession.skip_files << skip_on_non_linux
	}
	$if linux {
		tsession.skip_files << skip_on_linux
	}
	$if windows {
		tsession.skip_files << skip_on_windows
	}
	$if !windows {
		tsession.skip_files << skip_on_non_windows
	}
	$if macos {
		tsession.skip_files << skip_on_macos
	}
	$if !macos {
		tsession.skip_files << skip_on_non_macos
	}
	tsession.test()
	eprintln(tsession.benchmark.total_message(title))
	if tsession.benchmark.nfail > 0 {
		eprintln('\nWARNING: failed $tsession.benchmark.nfail times.\n')
		exit(1)
	}
}
