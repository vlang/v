module main

import os
import testing
import v.pref

const (
	skip_with_fsanitize_memory    = [
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
		'vlib/v/tests/unsafe_test.v',
		'vlib/x/websocket/websocket_test.v',
		'vlib/net/http/http_httpbin_test.v',
	]
	skip_with_fsanitize_address   = [
		'vlib/encoding/base64/base64_test.v',
		'vlib/encoding/csv/reader_test.v',
		'vlib/flag/flag_test.v',
		'vlib/io/util/util_test.v',
		'vlib/io/reader_test.v',
		'vlib/json/json_test.v',
		'vlib/net/http/cookie_test.v',
		'vlib/os/inode_test.v',
		'vlib/os/os_test.v',
		'vlib/regex/regex_test.v',
		'vlib/semver/semver_test.v',
		'vlib/sync/channel_opt_propagate_test.v',
		'vlib/time/parse_test.v',
		'vlib/v/fmt/fmt_keep_test.v',
		'vlib/v/fmt/fmt_test.v',
		'vlib/v/tests/array_init_test.v',
		'vlib/v/doc/doc_test.v',
		'vlib/v/tests/const_test.v',
		'vlib/v/tests/fn_multiple_returns_test.v',
		'vlib/v/tests/inout/compiler_test.v',
		'vlib/v/tests/option_default_values_test.v',
		'vlib/v/tests/option_test.v',
		'vlib/v/tests/ptr_arithmetic_test.v',
		'vlib/v/tests/str_gen_test.v',
		'vlib/v/tests/unsafe_test.v',
		'vlib/v/tests/vmod_parser_test.v',
		'vlib/v/vcache/vcache_test.v',
		'vlib/x/json2/decoder_test.v',
		'vlib/x/websocket/websocket_test.v',
	]
	skip_with_fsanitize_undefined = [
		'vlib/encoding/csv/reader_test.v',
	]
	skip_test_files               = []string{}
	skip_on_musl                  = [
		'vlib/v/tests/profile/profile_test.v',
	]
	skip_on_ubuntu_musl           = [
		/* 'vlib/v/gen/js/jsgen_test.v', */
		'vlib/net/http/cookie_test.v',
		'vlib/net/http/http_test.v',
		'vlib/net/http/status_test.v',
		'vlib/net/websocket/ws_test.v',
		'vlib/sqlite/sqlite_test.v',
		'vlib/orm/orm_test.v',
		'vlib/clipboard/clipboard_test.v',
		'vlib/vweb/tests/vweb_test.v',
		'vlib/x/websocket/websocket_test.v',
		'vlib/net/http/http_httpbin_test.v',
	]
	skip_on_linux                 = []string{}
	skip_on_non_linux             = [
		'vlib/net/websocket/ws_test.v',
	]
	skip_on_windows               = [
		'vlib/orm/orm_test.v',
		'vlib/net/websocket/ws_test.v',
		'vlib/x/websocket/websocket_test.v',
		'vlib/vweb/tests/vweb_test.v',
	]
	skip_on_non_windows           = []string{}
	skip_on_macos                 = []string{}
	skip_on_non_macos             = []string{}
)

// NB: musl misses openssl, thus the http tests can not be done there
// NB: http_httpbin_test.v: fails with 'cgen error: json: map_string_string is not struct'
fn main() {
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	os.chdir(vroot)
	args := os.args.clone()
	args_string := args[1..].join(' ')
	cmd_prefix := args_string.all_before('test-self')
	title := 'testing all fixed tests'
	all_test_files := os.walk_ext(os.join_path(vroot, 'vlib'), '_test.v')
	testing.eheader(title)
	mut tsession := testing.new_test_session(cmd_prefix)
	tsession.files << all_test_files
	tsession.skip_files << skip_test_files
	mut sanitize_memory := false
	mut sanitize_address := false
	mut sanitize_undefined := false
	for arg in args {
		if '-fsanitize=memory' in arg {
			sanitize_memory = true
		}
		if '-fsanitize=address' in arg {
			sanitize_address = true
		}
		if '-fsanitize=undefined' in arg {
			sanitize_undefined = true
		}
	}
	if sanitize_memory {
		tsession.skip_files << skip_with_fsanitize_memory
	}
	if sanitize_address {
		tsession.skip_files << skip_with_fsanitize_address
	}
	if sanitize_undefined {
		tsession.skip_files << skip_with_fsanitize_undefined
	}
	// println(tsession.skip_files)
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
