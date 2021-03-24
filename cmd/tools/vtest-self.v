module main

import os
import testing
import v.pref

const (
	skip_with_fsanitize_memory    = [
		'vlib/net/tcp_simple_client_server_test.v',
		'vlib/net/http/cookie_test.v',
		'vlib/net/http/http_test.v',
		'vlib/net/http/status_test.v',
		'vlib/net/http/http_httpbin_test.v',
		'vlib/net/http/header_test.v',
		'vlib/net/udp_test.v',
		'vlib/net/tcp_test.v',
		'vlib/orm/orm_test.v',
		'vlib/sqlite/sqlite_test.v',
		'vlib/v/tests/orm_sub_struct_test.v',
		'vlib/vweb/tests/vweb_test.v',
		'vlib/vweb/request_test.v',
		'vlib/vweb/route_test.v',
		'vlib/x/websocket/websocket_test.v',
		'vlib/crypto/rand/crypto_rand_read_test.v',
	]
	skip_with_fsanitize_address   = [
		'vlib/json/json_test.v',
		'vlib/regex/regex_test.v',
		'vlib/x/websocket/websocket_test.v',
	]
	skip_with_fsanitize_undefined = []string{}
	skip_with_werror              = [
		'vlib/sync/array_rlock_test.v',
		'vlib/clipboard/clipboard_test.v',
		'vlib/eventbus/eventbus_test.v',
		'vlib/gx/color_test.v',
		'vlib/json/json_test.v',
		'vlib/math/big/big_test.v',
		'vlib/net/ftp/ftp_test.v',
		'vlib/net/http/cookie_test.v',
		'vlib/net/http/http_httpbin_test.v',
		'vlib/net/http/header_test.v',
		'vlib/net/tcp_test.v',
		'vlib/net/tcp_simple_client_server_test.v',
		'vlib/net/smtp/smtp_test.v',
		'vlib/net/udp_test.v',
		'vlib/net/unix/unix_test.v',
		'vlib/net/http/http_test.v',
		'vlib/net/http/status_test.v',
		'vlib/orm/orm_test.v',
		'vlib/os/os_test.v',
		'vlib/rand/mt19937/mt19937_test.v',
		'vlib/readline/readline_test.v',
		'vlib/regex/regex_test.v',
		'vlib/sqlite/sqlite_test.v',
		'vlib/strconv/atof_test.v',
		'vlib/strconv/f32_f64_to_string_test.v',
		'vlib/strconv/number_to_base_test.v',
		'vlib/sync/atomic2/atomic_test.v',
		'vlib/sync/channel_2_test.v',
		'vlib/sync/channel_1_test.v',
		'vlib/sync/channel_3_test.v',
		'vlib/sync/channel_4_test.v',
		'vlib/sync/channel_array_mut_test.v',
		'vlib/sync/channel_close_test.v',
		'vlib/sync/channel_fill_test.v',
		'vlib/sync/channel_polling_test.v',
		'vlib/sync/channel_opt_propagate_test.v',
		'vlib/sync/channel_push_or_1_test.v',
		'vlib/sync/channel_push_or_2_test.v',
		'vlib/sync/channel_select_2_test.v',
		'vlib/sync/channel_select_3_test.v',
		'vlib/sync/channel_select_4_test.v',
		'vlib/sync/channel_select_5_test.v',
		'vlib/sync/channel_select_6_test.v',
		'vlib/sync/channel_select_test.v',
		'vlib/sync/channel_try_buf_test.v',
		'vlib/sync/select_close_test.v',
		'vlib/sync/struct_chan_init_test.v',
		'vlib/sync/pool/pool_test.v',
		'vlib/sync/channel_try_unbuf_test.v',
		'vlib/szip/szip_test.v',
		'vlib/v/compiler_errors_test.v',
		'vlib/v/gen/js/jsgen_test.v',
		'vlib/v/tests/anon_fn_test.v',
		'vlib/v/tests/array_map_ref_test.v',
		'vlib/v/tests/array_test.v',
		'vlib/v/tests/assert_sumtype_test.v',
		'vlib/v/tests/autolock_array2_test.v',
		'vlib/v/tests/autolock_array1_test.v',
		'vlib/v/tests/blank_ident_test.v',
		'vlib/v/tests/comptime_at_test.v',
		'vlib/v/tests/comptime_call_test.v',
		'vlib/v/tests/comptime_if_expr_test.v',
		'vlib/v/tests/cstrings_test.v',
		'vlib/v/tests/enum_test.v',
		'vlib/v/tests/fixed_array_test.v',
		'vlib/v/tests/fn_shared_return_test.v',
		'vlib/v/tests/fn_variadic_test.v',
		'vlib/v/tests/for_loops_2_test.v',
		'vlib/v/tests/generic_chan_test.v',
		'vlib/v/tests/generics_method_test.v',
		'vlib/v/tests/go_array_wait_test.v',
		'vlib/v/tests/go_call_generic_fn_test.v',
		'vlib/v/tests/generics_test.v',
		'vlib/v/tests/go_wait_2_test.v',
		'vlib/v/tests/if_guard_test.v',
		'vlib/v/tests/in_expression_test.v',
		'vlib/v/tests/interface_edge_cases/assign_to_interface_field_test.v',
		'vlib/v/tests/interface_fields_test.v',
		'vlib/v/tests/interface_variadic_test.v',
		'vlib/v/tests/option_2_test.v',
		'vlib/v/tests/operator_overloading_with_string_interpolation_test.v',
		'vlib/v/tests/orm_sub_struct_test.v',
		'vlib/v/tests/ref_struct_test.v',
		'vlib/v/tests/repl/repl_test.v',
		'vlib/v/tests/semaphore_test.v',
		'vlib/v/tests/semaphore_timed_test.v',
		'vlib/v/tests/shared_arg_test.v',
		'vlib/v/tests/shared_array_test.v',
		'vlib/v/tests/shared_autolock_test.v',
		'vlib/v/tests/shared_fn_return_test.v',
		'vlib/v/tests/shared_lock_2_test.v',
		'vlib/v/tests/shared_elem_test.v',
		'vlib/v/tests/shared_lock_4_test.v',
		'vlib/v/tests/shared_lock_3_test.v',
		'vlib/v/tests/shared_lock_6_test.v',
		'vlib/v/tests/shared_lock_expr_test.v',
		'vlib/v/tests/shared_lock_5_test.v',
		'vlib/v/tests/shared_lock_test.v',
		'vlib/v/tests/shared_map_test.v',
		'vlib/v/tests/shared_unordered_mixed_test.v',
		'vlib/v/tests/shift_test.v',
		'vlib/v/tests/str_gen_test.v',
		'vlib/v/tests/string_interpolation_multi_return_test.v',
		'vlib/v/tests/string_interpolation_shared_test.v',
		'vlib/v/tests/string_interpolation_test.v',
		'vlib/v/tests/struct_allow_both_field_defaults_and_skip_flag_test.v',
		'vlib/v/tests/struct_test.v',
		'vlib/v/tests/sum_type_test.v',
		'vlib/v/tests/type_name_test.v',
		'vlib/v/tests/unsafe_test.v',
		'vlib/v/tests/voidptr_to_u64_cast_a_test.v',
		'vlib/v/tests/voidptr_to_u64_cast_b_test.v',
		'vlib/v/tests/working_with_an_empty_struct_test.v',
		'vlib/vweb/tests/vweb_test.v',
		'vlib/vweb/request_test.v',
		'vlib/vweb/route_test.v',
		'vlib/x/websocket/websocket_test.v',
		'vlib/x/ttf/ttf_test.v',
	]
	skip_with_asan_compiler       = [
		'vlib/readline/readline_test.v',
		'vlib/vweb/tests/vweb_test.v',
	]
	skip_with_msan_compiler       = []string{}
	skip_test_files               = []string{}
	skip_on_musl                  = [
		'vlib/v/tests/profile/profile_test.v',
	]
	skip_on_ubuntu_musl           = [
		//'vlib/v/gen/js/jsgen_test.v',
		'vlib/net/http/cookie_test.v',
		'vlib/net/http/http_test.v',
		'vlib/net/http/status_test.v',
		'vlib/net/websocket/ws_test.v',
		'vlib/sqlite/sqlite_test.v',
		'vlib/orm/orm_test.v',
		'vlib/v/tests/orm_sub_struct_test.v',
		'vlib/clipboard/clipboard_test.v',
		'vlib/vweb/tests/vweb_test.v',
		'vlib/vweb/request_test.v',
		'vlib/vweb/route_test.v',
		'vlib/x/websocket/websocket_test.v',
		'vlib/net/http/http_httpbin_test.v',
		'vlib/net/http/header_test.v',
	]
	skip_on_linux                 = []string{}
	skip_on_non_linux             = [
		'vlib/net/websocket/ws_test.v',
	]
	skip_on_windows               = [
		'vlib/orm/orm_test.v',
		'vlib/v/tests/orm_sub_struct_test.v',
		'vlib/net/websocket/ws_test.v',
		'vlib/net/unix/unix_test.v',
		'vlib/x/websocket/websocket_test.v',
		'vlib/vweb/tests/vweb_test.v',
		'vlib/vweb/request_test.v',
		'vlib/vweb/route_test.v',
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
	title := 'testing vlib'
	all_test_files := os.walk_ext(os.join_path(vroot, 'vlib'), '_test.v')
	testing.eheader(title)
	mut tsession := testing.new_test_session(cmd_prefix)
	tsession.files << all_test_files
	tsession.skip_files << skip_test_files
	tsession.skip_files << 'vlib/v/tests/option_print_errors_test.v'
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
		if arg.contains('-Werror') {
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
