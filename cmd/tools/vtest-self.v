module main

import os
import testing
import v.pref

const github_job = os.getenv('GITHUB_JOB')

const just_essential = os.getenv('VTEST_JUST_ESSENTIAL') != ''

const (
	essential_list                = [
		'cmd/tools/vvet/vet_test.v',
		'vlib/arrays/arrays_test.v',
		'vlib/bitfield/bitfield_test.v',
		//
		'vlib/builtin/int_test.v',
		'vlib/builtin/array_test.v',
		'vlib/builtin/float_test.v',
		'vlib/builtin/byte_test.v',
		'vlib/builtin/rune_test.v',
		'vlib/builtin/builtin_test.v',
		'vlib/builtin/map_of_floats_test.v',
		'vlib/builtin/string_int_test.v',
		'vlib/builtin/utf8_test.v',
		'vlib/builtin/map_test.v',
		'vlib/builtin/string_test.v',
		'vlib/builtin/sorting_test.v',
		'vlib/builtin/gated_array_string_test.v',
		'vlib/builtin/array_shrinkage_test.v',
		'vlib/builtin/isnil_test.v',
		'vlib/builtin/string_match_glob_test.v',
		'vlib/builtin/string_strip_margin_test.v',
		//
		'vlib/cli/command_test.v',
		'vlib/crypto/md5/md5_test.v',
		'vlib/dl/dl_test.v',
		'vlib/encoding/base64/base64_test.v',
		'vlib/encoding/utf8/encoding_utf8_test.v',
		'vlib/encoding/utf8/utf8_util_test.v',
		'vlib/flag/flag_test.v',
		'vlib/json/json_decode_test.v',
		'vlib/math/math_test.v',
		'vlib/net/tcp_test.v',
		'vlib/net/http/http_test.v',
		'vlib/net/http/server_test.v',
		'vlib/net/http/request_test.v',
		'vlib/io/io_test.v',
		'vlib/io/os_file_reader_test.v',
		'vlib/os/process_test.v',
		'vlib/os/file_test.v',
		'vlib/os/notify/notify_test.v',
		'vlib/os/filepath_test.v',
		'vlib/os/environment_test.v',
		'vlib/os/glob_test.v',
		'vlib/os/os_test.v',
		'vlib/rand/random_numbers_test.v',
		'vlib/rand/wyrand/wyrand_test.v',
		'vlib/runtime/runtime_test.v',
		'vlib/semver/semver_test.v',
		'vlib/sync/stdatomic/atomic_test.v',
		'vlib/sync/thread_test.v',
		'vlib/sync/waitgroup_test.v',
		'vlib/sync/pool/pool_test.v',
		'vlib/strings/builder_test.v',
		'vlib/strconv/atof_test.v',
		'vlib/strconv/atoi_test.v',
		'vlib/strconv/f32_f64_to_string_test.v',
		'vlib/strconv/format_test.v',
		'vlib/strconv/number_to_base_test.v',
		'vlib/time/time_test.v',
		'vlib/toml/tests/toml_test.v',
		'vlib/v/compiler_errors_test.v',
		'vlib/v/doc/doc_test.v',
		'vlib/v/eval/interpret_test.v',
		'vlib/v/fmt/fmt_keep_test.v',
		'vlib/v/fmt/fmt_test.v',
		'vlib/v/gen/c/coutput_test.v',
		'vlib/v/gen/js/program_test.v',
		'vlib/v/gen/native/macho_test.v',
		'vlib/v/gen/native/tests/native_test.v',
		'vlib/v/pkgconfig/pkgconfig_test.v',
		'vlib/v/tests/inout/compiler_test.v',
		'vlib/x/json2/json2_test.v',
	]
	skip_test_files               = [
		'cmd/tools/vdoc/html_tag_escape_test.v', /* can't locate local module: markdown */
		'cmd/tools/vdoc/tests/vdoc_file_test.v', /* fails on Windows; order of output is not as expected */
		'vlib/context/onecontext/onecontext_test.v',
		'vlib/context/deadline_test.v' /* sometimes blocks */,
		'vlib/mysql/mysql_orm_test.v' /* mysql not installed */,
		'vlib/pg/pg_orm_test.v' /* pg not installed */,
	]
	skip_fsanitize_too_slow       = [
		// These tests are too slow to be run in the CI on each PR/commit
		// in the sanitized modes:
		'vlib/v/compiler_errors_test.v',
		'vlib/v/doc/doc_test.v',
		'vlib/v/fmt/fmt_test.v',
		'vlib/v/fmt/fmt_keep_test.v',
		'vlib/v/fmt/fmt_vlib_test.v',
		'vlib/v/live/live_test.v',
		'vlib/v/parser/v_parser_test.v',
		'vlib/v/scanner/scanner_test.v',
		'vlib/v/tests/inout/compiler_test.v',
		'vlib/v/tests/prod_test.v',
		'vlib/v/tests/profile/profile_test.v',
		'vlib/v/tests/repl/repl_test.v',
		'vlib/v/tests/valgrind/valgrind_test.v',
	]
	skip_with_fsanitize_memory    = [
		'vlib/net/tcp_simple_client_server_test.v',
		'vlib/net/http/cookie_test.v',
		'vlib/net/http/http_test.v',
		'vlib/net/http/status_test.v',
		'vlib/net/http/http_httpbin_test.v',
		'vlib/net/http/header_test.v',
		'vlib/net/http/server_test.v',
		'vlib/net/udp_test.v',
		'vlib/net/tcp_test.v',
		'vlib/orm/orm_test.v',
		'vlib/sqlite/sqlite_test.v',
		'vlib/sqlite/sqlite_orm_test.v',
		'vlib/v/tests/orm_sub_struct_test.v',
		'vlib/v/tests/orm_sub_array_struct_test.v',
		'vlib/v/tests/orm_joined_tables_select_test.v',
		'vlib/v/tests/sql_statement_inside_fn_call_test.v',
		'vlib/vweb/tests/vweb_test.v',
		'vlib/vweb/request_test.v',
		'vlib/net/http/request_test.v',
		'vlib/net/http/response_test.v',
		'vlib/vweb/route_test.v',
		'vlib/net/websocket/websocket_test.v',
		'vlib/crypto/rand/crypto_rand_read_test.v',
		'vlib/net/smtp/smtp_test.v',
	]
	skip_with_fsanitize_address   = [
		'vlib/net/websocket/websocket_test.v',
	]
	skip_with_fsanitize_undefined = [
		'do_not_remove',
	]
	skip_with_werror              = [
		'do_not_remove',
		'vlib/v/embed_file/tests/embed_file_test.v',
	]
	skip_with_asan_compiler       = [
		'do_not_remove',
	]
	skip_with_msan_compiler       = [
		'do_not_remove',
	]
	skip_on_musl                  = [
		'vlib/v/tests/profile/profile_test.v',
		'vlib/gg/draw_fns_api_test.v',
	]
	skip_on_ubuntu_musl           = [
		//'vlib/v/gen/js/jsgen_test.v',
		'vlib/net/http/cookie_test.v',
		'vlib/net/http/http_test.v',
		'vlib/net/http/status_test.v',
		'vlib/net/websocket/ws_test.v',
		'vlib/sqlite/sqlite_test.v',
		'vlib/sqlite/sqlite_orm_test.v',
		'vlib/orm/orm_test.v',
		'vlib/v/tests/orm_sub_struct_test.v',
		'vlib/v/tests/orm_sub_array_struct_test.v',
		'vlib/v/tests/orm_joined_tables_select_test.v',
		'vlib/v/tests/sql_statement_inside_fn_call_test.v',
		'vlib/clipboard/clipboard_test.v',
		'vlib/vweb/tests/vweb_test.v',
		'vlib/vweb/request_test.v',
		'vlib/net/http/request_test.v',
		'vlib/vweb/route_test.v',
		'vlib/net/websocket/websocket_test.v',
		'vlib/net/http/http_httpbin_test.v',
		'vlib/net/http/header_test.v',
		'vlib/net/http/server_test.v',
		'vlib/net/http/response_test.v',
		'vlib/builtin/js/array_test.js.v',
		'vlib/net/smtp/smtp_test.v',
	]
	skip_on_linux                 = [
		'do_not_remove',
	]
	skip_on_non_linux             = [
		'do_not_remove',
	]
	skip_on_windows_msvc          = [
		'do_not_remove',
		'vlib/v/tests/const_fixed_array_containing_references_to_itself_test.v', // error C2099: initializer is not a constant
	]
	skip_on_windows               = [
		'vlib/context/cancel_test.v',
		'vlib/context/deadline_test.v',
		'vlib/context/empty_test.v',
		'vlib/context/value_test.v',
		'vlib/orm/orm_test.v',
		'vlib/v/tests/orm_sub_struct_test.v',
		'vlib/v/tests/orm_joined_tables_select_test.v',
		'vlib/net/websocket/ws_test.v',
		'vlib/net/unix/unix_test.v',
		'vlib/net/unix/use_net_and_net_unix_together_test.v',
		'vlib/net/websocket/websocket_test.v',
		'vlib/vweb/tests/vweb_test.v',
		'vlib/vweb/request_test.v',
		'vlib/net/http/request_test.v',
		'vlib/vweb/route_test.v',
		'vlib/sync/many_times_test.v',
		'vlib/sync/once_test.v',
		'vlib/net/smtp/smtp_test.v',
	]
	skip_on_non_windows           = [
		'do_not_remove',
	]
	skip_on_macos                 = [
		'do_not_remove',
	]
	skip_on_non_macos             = [
		'do_not_remove',
	]
	skip_on_amd64                 = [
		'do_not_remove',
	]
	skip_on_arm64                 = [
		'do_not_remove',
	]
	skip_on_non_amd64_or_arm64    = [
		// closures aren't implemented yet:
		'vlib/v/tests/closure_test.v',
		'vlib/context/cancel_test.v',
		'vlib/context/deadline_test.v',
		'vlib/context/empty_test.v',
		'vlib/context/value_test.v',
		'vlib/context/onecontext/onecontext_test.v',
		'vlib/sync/once_test.v',
		'vlib/sync/many_times_test.v',
		'do_not_remove',
	]
)

// Note: musl misses openssl, thus the http tests can not be done there
// Note: http_httpbin_test.v: fails with 'cgen error: json: map_string_string is not struct'
fn main() {
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	os.chdir(vroot) or { panic(err) }
	args := os.args.clone()
	args_string := args[1..].join(' ')
	cmd_prefix := args_string.all_before('test-self')
	title := 'testing vlib'
	mut all_test_files := os.walk_ext(os.join_path(vroot, 'vlib'), '_test.v')
	all_test_files << os.walk_ext(os.join_path(vroot, 'cmd'), '_test.v')
	test_js_files := os.walk_ext(os.join_path(vroot, 'vlib'), '_test.js.v')
	all_test_files << test_js_files

	if just_essential {
		rooted_essential_list := essential_list.map(os.join_path(vroot, it))
		all_test_files = all_test_files.filter(rooted_essential_list.contains(it))
	}
	testing.eheader(title)
	mut tsession := testing.new_test_session(cmd_prefix, true)
	tsession.files << all_test_files.filter(!it.contains('testdata' + os.path_separator))
	tsession.skip_files << skip_test_files

	if !testing.is_node_present {
		testroot := vroot + os.path_separator
		tsession.skip_files << test_js_files.map(it.replace(testroot, ''))
	}
	testing.find_started_process('mysqld') or {
		tsession.skip_files << 'vlib/mysql/mysql_orm_test.v'
	}
	testing.find_started_process('postgres') or { tsession.skip_files << 'vlib/pg/pg_orm_test.v' }

	if github_job == 'windows-tcc' {
		// TODO: fix these ASAP
		tsession.skip_files << 'vlib/net/tcp_test.v'
		tsession.skip_files << 'vlib/net/udp_test.v'
	}

	mut werror := false
	mut sanitize_memory := false
	mut sanitize_address := false
	mut sanitize_undefined := false
	mut asan_compiler := false
	mut msan_compiler := false
	for arg in args {
		if arg.contains('-asan-compiler') {
			asan_compiler = true
		}
		if arg.contains('-msan-compiler') {
			msan_compiler = true
		}
		if arg.contains('-Werror') || arg.contains('-cstrict') {
			werror = true
		}
		if arg.contains('-fsanitize=memory') {
			sanitize_memory = true
		}
		if arg.contains('-fsanitize=address') {
			sanitize_address = true
		}
		if arg.contains('-fsanitize=undefined') {
			sanitize_undefined = true
		}
	}
	if os.getenv('VTEST_RUN_FSANITIZE_TOO_SLOW').len == 0
		&& (sanitize_undefined || sanitize_memory || sanitize_address) {
		tsession.skip_files << skip_fsanitize_too_slow
	}
	if werror {
		tsession.skip_files << skip_with_werror
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
	if asan_compiler {
		tsession.skip_files << skip_with_asan_compiler
	}
	if msan_compiler {
		tsession.skip_files << skip_with_msan_compiler
	}
	// println(tsession.skip_files)
	if os.getenv('V_CI_MUSL').len > 0 {
		tsession.skip_files << skip_on_musl
	}
	if os.getenv('V_CI_UBUNTU_MUSL').len > 0 {
		tsession.skip_files << skip_on_ubuntu_musl
	}
	$if !amd64 && !arm64 {
		tsession.skip_files << skip_on_non_amd64
	}
	$if amd64 {
		tsession.skip_files << skip_on_amd64
	}
	$if arm64 {
		tsession.skip_files << skip_on_arm64
	}
	$if !linux {
		tsession.skip_files << skip_on_non_linux
	}
	$if linux {
		tsession.skip_files << skip_on_linux
	}
	$if windows {
		tsession.skip_files << skip_on_windows
		$if msvc {
			tsession.skip_files << skip_on_windows_msvc
		}
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
