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
		'vlib/net/udp_test.v',
		'vlib/net/tcp_test.v',
		'vlib/orm/orm_test.v',
		'vlib/sqlite/sqlite_test.v',
		'vlib/v/tests/orm_sub_struct_test.v',
		'vlib/vweb/tests/vweb_test.v',
		'vlib/x/websocket/websocket_test.v',
	]
	skip_with_fsanitize_address   = [
		'vlib/encoding/csv/reader_test.v',
		'vlib/encoding/base64/base64_test.v',
		'vlib/json/json_test.v',
		'vlib/regex/regex_test.v',
		'vlib/sync/channel_opt_propagate_test.v',
		'vlib/v/tests/ptr_arithmetic_test.v',
		'vlib/v/tests/unsafe_test.v',
		'vlib/x/websocket/websocket_test.v',
	]
	skip_with_fsanitize_undefined = [
		'vlib/encoding/csv/reader_test.v',
	]
	skip_with_werror              = [
		'vlib/builtin/array_test.v',
		'vlib/clipboard/clipboard_test.v',
		'vlib/dl/example/use_test.v',
		'vlib/eventbus/eventbus_test.v',
		'vlib/gx/color_test.v',
		'vlib/json/json_test.v',
		'vlib/net/ftp/ftp_test.v',
		'vlib/net/smtp/smtp_test.v',
		'vlib/net/http/cookie_test.v',
		'vlib/net/tcp_test.v',
		'vlib/net/udp_test.v',
		'vlib/net/tcp_simple_client_server_test.v',
		'vlib/net/unix/unix_test.v',
		'vlib/net/http/http_httpbin_test.v',
		'vlib/net/http/status_test.v',
		'vlib/net/http/http_test.v',
		'vlib/orm/orm_test.v',
		'vlib/readline/readline_test.v',
		'vlib/sqlite/sqlite_test.v',
		'vlib/strconv/number_to_base_test.v',
		'vlib/sync/channel_1_test.v',
		'vlib/sync/atomic2/atomic_test.v',
		'vlib/sync/channel_2_test.v',
		'vlib/sync/channel_4_test.v',
		'vlib/sync/channel_fill_test.v',
		'vlib/sync/channel_array_mut_test.v',
		'vlib/sync/channel_close_test.v',
		'vlib/sync/channel_3_test.v',
		'vlib/sync/channel_opt_propagate_test.v',
		'vlib/sync/channel_polling_test.v',
		'vlib/sync/channel_push_or_1_test.v',
		'vlib/sync/channel_push_or_2_test.v',
		'vlib/sync/channel_select_2_test.v',
		'vlib/sync/channel_select_3_test.v',
		'vlib/sync/channel_select_5_test.v',
		'vlib/sync/channel_select_4_test.v',
		'vlib/sync/channel_select_6_test.v',
		'vlib/sync/channel_try_unbuf_test.v',
		'vlib/sync/channel_select_test.v',
		'vlib/sync/channel_try_buf_test.v',
		'vlib/sync/pool/pool_test.v',
		'vlib/sync/select_close_test.v',
		'vlib/sync/struct_chan_init_test.v',
		'vlib/szip/szip_test.v',
		'vlib/v/compiler_errors_test.v',
		'vlib/v/tests/anon_fn_test.v',
		'vlib/v/tests/array_map_ref_test.v',
		'vlib/v/tests/array_test.v',
		'vlib/v/tests/assert_sumtype_test.v',
		'vlib/v/tests/autolock_array1_test.v',
		'vlib/v/tests/autolock_array2_test.v',
		'vlib/v/tests/blank_ident_test.v',
		'vlib/v/tests/comptime_call_test.v',
		'vlib/v/tests/comptime_at_test.v',
		'vlib/v/tests/comptime_if_expr_test.v',
		'vlib/v/tests/fixed_array_test.v',
		'vlib/v/tests/fn_variadic_test.v',
		'vlib/v/tests/fn_shared_return_test.v',
		'vlib/v/tests/for_loops_2_test.v',
		'vlib/v/tests/generic_chan_test.v',
		'vlib/v/tests/generics_method_test.v',
		'vlib/v/tests/generics_test.v',
		'vlib/v/tests/go_call_generic_fn_test.v',
		'vlib/v/tests/go_wait_2_test.v',
		'vlib/v/tests/interface_edge_cases/assign_to_interface_field_test.v',
		'vlib/v/tests/interface_fields_test.v',
		'vlib/v/tests/interface_variadic_test.v',
		'vlib/v/tests/orm_sub_struct_test.v',
		'vlib/v/tests/ref_struct_test.v',
		'vlib/v/tests/semaphore_test.v',
		'vlib/v/tests/repl/repl_test.v',
		'vlib/v/tests/semaphore_timed_test.v',
		'vlib/v/tests/shared_arg_test.v',
		'vlib/v/tests/shared_array_test.v',
		'vlib/v/tests/shared_elem_test.v',
		'vlib/v/tests/shared_autolock_test.v',
		'vlib/v/tests/shared_fn_return_test.v',
		'vlib/v/tests/shared_lock_2_test.v',
		'vlib/v/tests/shared_lock_3_test.v',
		'vlib/v/tests/shared_lock_5_test.v',
		'vlib/v/tests/shared_lock_4_test.v',
		'vlib/v/tests/shared_lock_6_test.v',
		'vlib/v/tests/shared_lock_expr_test.v',
		'vlib/v/tests/shared_lock_test.v',
		'vlib/v/tests/shift_test.v',
		'vlib/v/tests/shared_unordered_mixed_test.v',
		'vlib/v/tests/shared_map_test.v',
		'vlib/v/tests/str_gen_test.v',
		'vlib/v/tests/string_interpolation_multi_return_test.v',
		'vlib/v/tests/string_interpolation_shared_test.v',
		'vlib/v/tests/string_interpolation_test.v',
		'vlib/v/tests/struct_allow_both_field_defaults_and_skip_flag_test.v',
		'vlib/v/tests/struct_test.v',
		'vlib/v/tests/sum_type_test.v',
		'vlib/v/tests/type_name_test.v',
		'vlib/v/tests/unsafe_test.v',
		'vlib/v/tests/working_with_an_empty_struct_test.v',
		'vlib/vweb/tests/vweb_test.v',
		'vlib/x/json2/any_test.v',
		'vlib/x/json2/decoder_test.v',
		'vlib/x/json2/json2_test.v',
		'vlib/x/websocket/websocket_test.v',
		'vlib/math/big/big_test.v',
		'vlib/os/os_test.v',
		'vlib/rand/mt19937/mt19937_test.v',
		'vlib/regex/regex_test.v',
		'vlib/strconv/atof_test.v',
		'vlib/v/tests/cstrings_test.v',
		'vlib/v/tests/enum_test.v',
		'vlib/v/tests/in_expression_test.v',
		'vlib/v/tests/operator_overloading_with_string_interpolation_test.v',
		'vlib/v/tests/voidptr_to_u64_cast_a_test.v',
		'vlib/v/tests/voidptr_to_u64_cast_b_test.v',
		'vlib/dl/dl_test.v',
		'vlib/strconv/f32_f64_to_string_test.v',
		'vlib/x/ttf/ttf_test.v',
	]
	skip_with_asan_compiler       = [
		'vlib/builtin/map_of_floats_test.v',
		'vlib/builtin/int_test.v',
		'vlib/builtin/string_test.v',
		'vlib/crypto/aes/aes_test.v',
		'vlib/crypto/md5/md5_test.v',
		'vlib/crypto/rc4/rc4_test.v',
		'vlib/crypto/sha1/sha1_test.v',
		'vlib/crypto/sha256/sha256_test.v',
		'vlib/crypto/sha512/sha512_test.v',
		'vlib/dl/example/use_test.v',
		'vlib/encoding/base64/base64_test.v',
		'vlib/encoding/csv/reader_test.v',
		'vlib/encoding/csv/writer_test.v',
		'vlib/encoding/utf8/encoding_utf8_test.v',
		'vlib/eventbus/eventbus_test.v',
		'vlib/encoding/utf8/utf8_util_test.v',
		'vlib/flag/flag_test.v',
		'vlib/glm/glm_test.v',
		'vlib/hash/crc32/crc32_test.v',
		'vlib/hash/hash_wyhash_test.v',
		'vlib/math/big/big_test.v',
		'vlib/math/complex/complex_test.v',
		'vlib/math/fractions/approximations_test.v',
		'vlib/math/fractions/fraction_test.v',
		'vlib/net/urllib/urllib_test.v',
		'vlib/os/os_test.v',
		'vlib/readline/readline_test.v',
		'vlib/strconv/f32_f64_to_string_test.v',
		'vlib/strings/builder_test.v',
		'vlib/szip/szip_test.v',
		'vlib/v/embed_file/embed_file_test.v',
		'vlib/v/tests/anon_fn_call_test.v',
		'vlib/v/tests/anon_fn_returning_question_test.v',
		'vlib/v/tests/appending_to_mut_array_in_fn_param_test.v',
		'vlib/v/tests/array_append_short_struct_test.v',
		'vlib/v/tests/array_methods_test.v',
		'vlib/v/tests/assert_sumtype_test.v',
		'vlib/v/tests/as_cast_is_expr_sumtype_fn_result_test.v',
		'vlib/v/tests/array_to_string_test.v',
		'vlib/v/tests/array_type_alias_test.v',
		'vlib/v/tests/assign_bitops_with_type_aliases_test.v',
		'vlib/v/tests/cast_to_byte_test.v',
		'vlib/v/tests/cast_to_interface_test.v',
		'vlib/v/tests/complex_assign_test.v',
		'vlib/v/tests/blank_ident_test.v',
		'vlib/v/tests/comptime_field_selector_test.v',
		'vlib/v/tests/comptime_call_test.v',
		'vlib/v/tests/comptime_if_expr_test.v',
		'vlib/v/tests/comptime_for_test.v',
		'vlib/v/tests/const_test.v',
		'vlib/v/tests/cross_assign_test.v',
		'vlib/v/tests/differently_named_structs_test.v',
		'vlib/v/tests/defer_test.v',
		'vlib/v/tests/enum_default_value_in_struct_test.v',
		'vlib/v/tests/enum_bitfield_test.v',
		'vlib/v/tests/enum_hex_test.v',
		'vlib/v/tests/enum_array_field_test.v',
		'vlib/v/tests/enum_test.v',
		'vlib/v/tests/fixed_array_test.v',
		'vlib/v/tests/fixed_array_const_size_test.v',
		'vlib/v/tests/fn_cross_assign_test.v',
		'vlib/v/tests/fn_expecting_ref_but_returning_struct_test.v',
		'vlib/v/tests/fn_variadic_test.v',
		'vlib/v/tests/fn_type_aliases_test.v',
		'vlib/v/tests/fn_with_fixed_array_function_args_test.v',
		'vlib/v/tests/for-in-iterator_test.v',
		'vlib/v/tests/for_in_mut_val_test.v',
		'vlib/v/tests/generic_fn_returning_type_with_T_test.v',
		'vlib/v/tests/generics_method_test.v',
		'vlib/v/tests/generics_return_multi_array_test.v',
		'vlib/v/tests/go_wait_3_test.v',
		'vlib/v/tests/imported_symbols_test.v',
		'vlib/v/tests/go_handle_for_functions_returning_array_test.v',
		'vlib/v/tests/in_expression_test.v',
		'vlib/v/tests/interface_edge_cases/array_of_interfaces_with_utility_fn_test.v',
		'vlib/v/tests/interface_edge_cases/assign_to_interface_field_test.v',
		'vlib/v/tests/interface_edge_cases/i4_test.v',
		'vlib/v/tests/interface_edge_cases/array_of_interfaces_test.v',
		'vlib/v/tests/interface_edge_cases/i3_test.v',
		'vlib/v/tests/interface_edge_cases/i1_test.v',
		'vlib/v/tests/interface_edge_cases/i2_test.v',
		'vlib/v/tests/interface_edge_cases/i5_test.v',
		'vlib/v/tests/interface_struct_test.v',
		'vlib/v/tests/interface_variadic_test.v',
		'vlib/v/tests/interface_edge_cases/i8_test.v',
		'vlib/v/tests/interfaces_map_test.v',
		'vlib/v/tests/interface_edge_cases/i6_test.v',
		'vlib/v/tests/interface_fields_test.v',
		'vlib/v/tests/interface_edge_cases/i7_test.v',
		'vlib/v/tests/interop_test.v',
		'vlib/v/tests/map_alias_key_test.v',
		'vlib/v/tests/map_and_array_with_fns_test.v',
		'vlib/v/tests/map_complex_fixed_array_test.v',
		'vlib/v/tests/map_high_order_assign_test.v',
		'vlib/v/tests/map_to_string_test.v',
		'vlib/v/tests/map_key_expr_test.v',
		'vlib/v/tests/match_expression_for_types_test.v',
		'vlib/v/tests/map_type_alias_test.v',
		'vlib/v/tests/match_sumtype_var_shadow_and_as_test.v',
		'vlib/v/tests/match_expression_with_fn_names_in_branches_test.v',
		'vlib/v/tests/method_call_chain_test.v',
		'vlib/v/tests/methods_on_interfaces_test.v',
		'vlib/v/tests/modules/methods_struct_another_module/methods_struct_test.v',
		'vlib/v/tests/modules/simplemodule/importing_test.v',
		'vlib/v/tests/multiret_with_ptrtype_test.v',
		'vlib/v/tests/offsetof_test.v',
		'vlib/v/tests/nested_option_call_test.v',
		'vlib/v/tests/operator_overloading_cmp_test.v',
		'vlib/v/tests/operator_overloading_with_string_interpolation_test.v',
		'vlib/v/tests/option_default_values_test.v',
		'vlib/v/tests/pointers_test.v',
		'vlib/v/tests/pointers_str_test.v',
		'vlib/v/tests/project_with_c_code/main_test.v',
		'vlib/v/tests/project_with_c_code_2/main2_test.v',
		'vlib/v/tests/ptr_arithmetic_test.v',
		'vlib/v/tests/ref_return_test.v',
		'vlib/v/tests/return_voidptr_test.v',
		'vlib/v/tests/shift_test.v',
		'vlib/v/tests/sizeof_2_test.v',
		'vlib/v/tests/short_struct_param_syntax_test.v',
		'vlib/v/tests/sorting_by_references_test.v',
		'vlib/v/tests/sorting_by_different_criteria_test.v',
		'vlib/v/tests/string_interpolation_array_test.v',
		'vlib/v/tests/string_alias_test.v',
		'vlib/v/tests/string_interpolation_alias_test.v',
		'vlib/v/tests/string_interpolation_custom_str_test.v',
		'vlib/v/tests/string_interpolation_multi_return_test.v',
		'vlib/v/tests/string_interpolation_of_array_of_structs_test.v',
		'vlib/v/tests/string_interpolation_test.v',
		'vlib/v/tests/string_interpolation_variadic_test.v',
		'vlib/v/tests/string_struct_interpolation_test.v',
		'vlib/v/tests/struct_allow_both_field_defaults_and_skip_flag_test.v',
		'vlib/v/tests/struct_child_field_default_test.v',
		'vlib/v/tests/struct_equality_test.v',
		'vlib/v/tests/struct_field_default_value_interface_cast_test.v',
		'vlib/v/tests/struct_field_default_value_sumtype_cast_test.v',
		'vlib/v/tests/struct_eq_op_only_test.v',
		'vlib/v/tests/struct_map_method_test.v',
		'vlib/v/tests/struct_fields_storing_functions_test.v',
		'vlib/v/tests/struct_transmute_test.v',
		'vlib/v/tests/sumtype_literal_test.v',
		'vlib/v/tests/sumtype_calls_test.v',
		'vlib/v/tests/sumtype_str_for_subtypes_with_str_test.v',
		'vlib/v/tests/type_alias_str_method_override_test.v',
		'vlib/v/tests/type_alias_test.v',
		'vlib/v/tests/type_name_test.v',
		'vlib/v/tests/type_promotion_test.v',
		'vlib/v/tests/unsafe_test.v',
		'vlib/v/tests/vargs_auto_str_method_and_println_test.v',
		'vlib/v/tests/vargs_empty_param_test.v',
		'vlib/v/tests/working_with_an_empty_struct_test.v',
		'vlib/v/vcache/vcache_test.v',
		'vlib/vweb/tests/vweb_test.v',
		'vlib/v/compiler_errors_test.v',
		'vlib/v/tests/map_enum_keys_test.v',
		'vlib/v/tests/tmpl_test.v',
		'vlib/v/checker/tests/arrow_op_wrong_left_type_err_b.vv',
		'vlib/v/checker/tests/arrow_op_wrong_right_type_err_a.vv',
		'vlib/v/checker/tests/lock_already_locked.vv',
		'vlib/v/checker/tests/lock_already_rlocked.vv',
		'vlib/v/checker/tests/lock_needed.vv',
		'vlib/v/checker/tests/lock_nonshared.vv',
		'vlib/v/checker/tests/shared_type_mismatch.vv',
		'vlib/v/tests/match_with_complex_exprs_in_branches_test.v',
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
		'vlib/v/tests/orm_sub_struct_test.v',
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
		'vlib/v/tests/orm_sub_struct_test.v',
		'vlib/net/websocket/ws_test.v',
		'vlib/net/unix/unix_test.v',
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
	title := 'testing all tests'
	all_test_files := os.walk_ext(os.join_path(vroot, 'vlib'), '_test.v')
	testing.eheader(title)
	mut tsession := testing.new_test_session(cmd_prefix)
	tsession.files << all_test_files
	tsession.skip_files << skip_test_files
	mut werror := false
	mut sanitize_memory := false
	mut sanitize_address := false
	mut sanitize_undefined := false
	mut asan_compiler := false
	for arg in args {
		if '-asan-compiler' in arg {
			asan_compiler = true
		}
		if '-Werror' in arg {
			werror = true
		}
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
