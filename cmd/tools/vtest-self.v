module main

import os
import testing

struct Config {
	run_just_essential     bool   = '${os.getenv('VTEST_JUST_ESSENTIAL')}${os.getenv('VTEST_SANDBOXED_PACKAGING')}' != ''
	run_slow_sanitize      bool   = os.getenv('VTEST_RUN_FSANITIZE_TOO_SLOW') != ''
	is_musl_ci             bool   = os.getenv('V_CI_MUSL') != ''
	is_ubuntu_musl_ci      bool   = os.getenv('V_CI_UBUNTU_MUSL') != ''
	is_sandboxed_packaging bool   = os.getenv('VTEST_SANDBOXED_PACKAGING') != ''
	github_job             string = os.getenv('GITHUB_JOB')
mut:
	test_dirs         []string = ['cmd', 'vlib']
	is_asan_compiler  bool
	is_msan_compiler  bool
	is_ubsan_compiler bool
	// Options relating to the v command itself (passed in the prefix) `v [...args] test-self`.
	werror             bool
	sanitize_memory    bool
	sanitize_address   bool
	sanitize_undefined bool
}

const vroot = os.dir(os.real_path(os.getenv_opt('VEXE') or { @VEXE }))

const essential_list = [
	'cmd/tools/vvet/vet_test.v',
	'cmd/tools/vdoc/document/doc_test.v',
	'vlib/arrays/arrays_test.v',
	'vlib/bitfield/bitfield_test.v',
	//
	'vlib/builtin/int_test.v',
	'vlib/builtin/array_test.v',
	'vlib/builtin/float_test.v',
	'vlib/builtin/byte_test.v',
	'vlib/builtin/rune_test.v',
	'vlib/builtin/builtin_test.c.v',
	'vlib/builtin/map_of_floats_test.v',
	'vlib/builtin/string_int_test.v',
	'vlib/builtin/utf8_test.v',
	'vlib/builtin/map_test.v',
	'vlib/builtin/string_test.v',
	'vlib/builtin/sorting_test.v',
	'vlib/builtin/gated_array_string_test.v',
	'vlib/builtin/isnil_test.v',
	'vlib/builtin/string_match_glob_test.v',
	'vlib/builtin/string_strip_margin_test.v',
	//
	'vlib/cli/command_test.v',
	'vlib/crypto/md5/md5_test.v',
	'vlib/dl/dl_test.v',
	'vlib/encoding/base64/base64_test.v',
	'vlib/encoding/utf8/validate/encoding_utf8_test.v',
	'vlib/encoding/utf8/utf8_util_test.v',
	'vlib/flag/flag_test.v',
	'vlib/json/tests/json_decode_test.v',
	'vlib/math/math_test.v',
	'vlib/net/tcp_test.v',
	'vlib/net/http/http_test.v',
	'vlib/net/http/server_test.v',
	'vlib/net/http/request_test.v',
	'vlib/io/io_test.v',
	'vlib/io/os_file_reader_test.v',
	'vlib/os/process_test.v',
	'vlib/os/file_test.v',
	'vlib/os/notify/notify_test.c.v',
	'vlib/os/filepath_test.v',
	'vlib/os/environment_test.v',
	'vlib/os/glob_test.v',
	'vlib/os/os_test.c.v',
	'vlib/rand/random_numbers_test.v',
	'vlib/rand/wyrand/wyrand_test.v',
	'vlib/runtime/runtime_test.v',
	'vlib/semver/semver_test.v',
	'vlib/sync/stdatomic/atomic_test.v',
	'vlib/sync/thread_test.v',
	'vlib/sync/waitgroup_test.v',
	'vlib/sync/pool/pool_test.v',
	'vlib/strings/builder_test.v',
	'vlib/strconv/atof_test.c.v',
	'vlib/strconv/atoi_test.v',
	'vlib/strconv/f32_f64_to_string_test.v',
	'vlib/strconv/format_test.v',
	'vlib/strconv/number_to_base_test.v',
	'vlib/time/time_test.v',
	'vlib/toml/tests/toml_test.v',
	'vlib/v/compiler_errors_test.v',
	'vlib/v/eval/interpret_test.v',
	'vlib/v/fmt/fmt_keep_test.v',
	'vlib/v/fmt/fmt_test.v',
	'vlib/v/gen/c/coutput_test.v',
	'vlib/v/gen/js/program_test.v',
	'vlib/v/gen/native/macho_test.v',
	'vlib/v/gen/native/tests/native_test.v',
	'vlib/v/pkgconfig/pkgconfig_test.v',
	'vlib/v/slow_tests/inout/compiler_test.v',
	'vlib/x/json2/tests/json2_test.v',
]
// These tests are too slow to be run in the CI on each PR/commit in the sanitized modes:
const skip_fsanitize_too_slow = [
	'do_not_remove',
	'vlib/v/compiler_errors_test.v',
	'vlib/v/fmt/fmt_test.v',
	'vlib/v/fmt/fmt_keep_test.v',
	'vlib/v/fmt/fmt_vlib_test.v',
	'vlib/v/live/live_test.v',
	'vlib/v/parser/v_parser_test.v',
	'vlib/v/scanner/scanner_test.v',
	'vlib/v/slow_tests/inout/compiler_test.v',
	'vlib/v/slow_tests/prod_test.v',
	'vlib/v/slow_tests/profile/profile_test.v',
	'vlib/v/slow_tests/repl/repl_test.v',
	'vlib/v/slow_tests/valgrind/valgrind_test.v',
	'cmd/tools/vpm/dependency_test.v',
	'cmd/tools/vpm/install_test.v',
	'cmd/tools/vpm/install_version_input_test.v',
	'cmd/tools/vpm/install_version_test.v',
	'cmd/tools/vpm/update_test.v',
	'cmd/tools/vdoc/document/doc_test.v',
]
const skip_with_fsanitize_memory = [
	'do_not_remove',
	'cmd/tools/vpm/dependency_test.v', // known flaky, for fsanitize_memory, due to using mbedtls
	'cmd/tools/vpm/install_test.v', // known flaky, for fsanitize_memory, due to using mbedtls
	'cmd/tools/vpm/install_version_input_test.v', // known flaky, for fsanitize_memory, due to using mbedtls
	'cmd/tools/vpm/install_version_test.v', // known flaky, for fsanitize_memory, due to using mbedtls
	'cmd/tools/vpm/update_test.v', // known flaky, for fsanitize_memory, due to using mbedtls
	'vlib/net/tcp_simple_client_server_test.v',
	'vlib/net/http/cookie_test.v',
	'vlib/net/http/http_test.v',
	'vlib/net/http/status_test.v',
	'vlib/net/http/header_test.v',
	'vlib/net/http/server_test.v',
	'vlib/net/udp_test.v',
	'vlib/net/tcp_test.v',
	'vlib/orm/orm_test.v',
	'vlib/orm/orm_sql_or_blocks_test.v',
	'vlib/orm/orm_create_and_drop_test.v',
	'vlib/orm/orm_insert_test.v',
	'vlib/orm/orm_insert_reserved_name_test.v',
	'vlib/orm/orm_sum_type_insert_test.v',
	'vlib/orm/orm_fn_calls_test.v',
	'vlib/orm/orm_last_id_test.v',
	'vlib/orm/orm_string_interpolation_in_where_test.v',
	'vlib/orm/orm_interface_test.v',
	'vlib/orm/orm_mut_db_test.v',
	'vlib/orm/orm_null_test.v',
	'vlib/orm/orm_result_test.v',
	'vlib/orm/orm_custom_operators_test.v',
	'vlib/orm/orm_fk_test.v',
	'vlib/orm/orm_nested_struct_test.v',
	'vlib/orm/orm_references_test.v',
	'vlib/orm/orm_option_array_test.v',
	'vlib/orm/orm_option_time_test.v',
	'vlib/orm/orm_order_by_custom_field_test.v',
	'vlib/orm/orm_serial_attribute_test.v',
	'vlib/orm/orm_option_subselect_test.v',
	'vlib/orm/orm_func_test.v',
	'vlib/db/sqlite/sqlite_test.v',
	'vlib/db/sqlite/sqlite_orm_test.v',
	'vlib/db/sqlite/sqlite_comptime_field_test.v',
	'vlib/db/sqlite/parent_child_test.v',
	'vlib/db/sqlite/sqlite_vfs_lowlevel_test.v',
	'vlib/v/tests/orm_enum_test.v',
	'vlib/v/tests/orm_sub_struct_test.v',
	'vlib/v/tests/orm_sub_array_struct_test.v',
	'vlib/v/tests/orm_joined_tables_select_test.v',
	'vlib/v/tests/sql_statement_inside_fn_call_test.v',
	'vlib/v/tests/orm_stmt_wrong_return_checking_test.v',
	'vlib/v/tests/orm_table_name_test.v',
	'vlib/v/tests/orm_array_field_test.v',
	'vlib/v/tests/orm_handle_error_for_select_from_not_created_table_test.v',
	'vlib/v/tests/orm_create_several_tables_test.v',
	'vlib/v/tests/orm_update_test.v',
	'vlib/v/tests/orm_or_test.v',
	'vlib/vweb/tests/vweb_test.v',
	'vlib/vweb/csrf/csrf_test.v',
	'vlib/net/http/request_test.v',
	'vlib/net/http/response_test.v',
	'vlib/vweb/route_test.v',
	'vlib/net/websocket/websocket_test.v',
	'vlib/net/smtp/smtp_test.v',
	'vlib/v/tests/websocket_logger_interface_should_compile_test.v',
	'vlib/v/tests/fns/fn_literal_type_test.v',
	'vlib/x/sessions/tests/db_store_test.v',
]
const skip_with_fsanitize_address = [
	'do_not_remove',
	'vlib/net/websocket/websocket_test.v',
	'vlib/orm/orm_create_and_drop_test.v',
	'vlib/orm/orm_insert_test.v',
	'vlib/orm/orm_insert_reserved_name_test.v',
	'vlib/orm/orm_sum_type_insert_test.v',
	'vlib/orm/orm_references_test.v',
	'vlib/v/tests/websocket_logger_interface_should_compile_test.v',
	'vlib/v/tests/orm_enum_test.v',
	'vlib/v/tests/orm_sub_array_struct_test.v',
	'vlib/v/tests/orm_handle_error_for_select_from_not_created_table_test.v',
	'vlib/v/tests/orm_create_several_tables_test.v',
	'vlib/v/tests/orm_update_test.v',
	'vlib/v/tests/orm_or_test.v',
]
const skip_with_fsanitize_undefined = [
	'do_not_remove',
	'vlib/orm/orm_create_and_drop_test.v',
	'vlib/orm/orm_insert_test.v',
	'vlib/orm/orm_insert_reserved_name_test.v',
	'vlib/orm/orm_sum_type_insert_test.v',
	'vlib/orm/orm_references_test.v',
	'vlib/v/tests/orm_enum_test.v',
	'vlib/v/tests/orm_sub_array_struct_test.v',
	'vlib/v/tests/orm_handle_error_for_select_from_not_created_table_test.v',
	'vlib/v/tests/orm_create_several_tables_test.v',
	'vlib/v/tests/orm_update_test.v',
	'vlib/v/tests/orm_or_test.v',
	'vlib/v/tests/project_with_cpp_code/compiling_cpp_files_with_a_cplusplus_compiler_test.c.v', // fails compilation with: undefined reference to vtable for __cxxabiv1::__function_type_info'
]
const skip_on_ubuntu_musl = [
	'do_not_remove',
	'vlib/arrays/parallel/parallel_test.v',
	'vlib/builtin/js/array_test.js.v',
	'vlib/db/sqlite/sqlite_test.v',
	'vlib/db/sqlite/sqlite_orm_test.v',
	'vlib/db/sqlite/sqlite_comptime_field_test.v',
	'vlib/db/sqlite/sqlite_vfs_lowlevel_test.v',
	'vlib/db/sqlite/parent_child_test.v',
	'vlib/orm/orm_test.v',
	'vlib/orm/orm_sql_or_blocks_test.v',
	'vlib/orm/orm_create_and_drop_test.v',
	'vlib/orm/orm_insert_test.v',
	'vlib/orm/orm_insert_reserved_name_test.v',
	'vlib/orm/orm_sum_type_insert_test.v',
	'vlib/orm/orm_fn_calls_test.v',
	'vlib/orm/orm_null_test.v',
	'vlib/orm/orm_last_id_test.v',
	'vlib/orm/orm_string_interpolation_in_where_test.v',
	'vlib/orm/orm_interface_test.v',
	'vlib/orm/orm_mut_db_test.v',
	'vlib/orm/orm_result_test.v',
	'vlib/orm/orm_custom_operators_test.v',
	'vlib/orm/orm_fk_test.v',
	'vlib/orm/orm_nested_struct_test.v',
	'vlib/orm/orm_references_test.v',
	'vlib/orm/orm_option_array_test.v',
	'vlib/orm/orm_option_time_test.v',
	'vlib/orm/orm_order_by_custom_field_test.v',
	'vlib/orm/orm_serial_attribute_test.v',
	'vlib/orm/orm_option_subselect_test.v',
	'vlib/orm/orm_func_test.v',
	'vlib/orm/orm_where_in_test.v',
	'vlib/v/tests/orm_enum_test.v',
	'vlib/v/tests/orm_sub_struct_test.v',
	'vlib/v/tests/orm_sub_array_struct_test.v',
	'vlib/v/tests/orm_joined_tables_select_test.v',
	'vlib/v/tests/orm_stmt_wrong_return_checking_test.v',
	'vlib/v/tests/orm_table_name_test.v',
	'vlib/v/tests/orm_array_field_test.v',
	'vlib/v/tests/orm_handle_error_for_select_from_not_created_table_test.v',
	'vlib/v/tests/orm_create_several_tables_test.v',
	'vlib/v/tests/orm_update_test.v',
	'vlib/v/tests/orm_or_test.v',
	'vlib/v/tests/sql_statement_inside_fn_call_test.v',
	'vlib/v/tests/websocket_logger_interface_should_compile_test.v',
	'vlib/v/tests/fns/fn_literal_type_test.v',
	'vlib/clipboard/clipboard_test.v',
	'vlib/vweb/tests/vweb_test.v',
	'vlib/vweb/csrf/csrf_test.v',
	'vlib/vweb/route_test.v',
	'vlib/net/http/request_test.v',
	'vlib/net/websocket/websocket_test.v',
	'vlib/net/http/header_test.v',
	'vlib/net/http/server_test.v',
	'vlib/net/http/response_test.v',
	'vlib/net/smtp/smtp_test.v',
	'vlib/net/http/cookie_test.v',
	'vlib/net/http/status_test.v',
	'vlib/x/sessions/tests/db_store_test.v',
	'vlib/veb/tests/veb_app_test.v',
]

fn Config.init(vargs []string, targs []string) !Config {
	mut cfg := Config{}
	for arg in vargs {
		match arg {
			'-Werror', '-cstrict' { cfg.werror = true }
			'-fsanitize=memory' { cfg.sanitize_memory = true }
			'-fsanitize=address' { cfg.sanitize_address = true }
			'-fsanitize=undefined' { cfg.sanitize_undefined = true }
			else {}
		}
	}
	if targs.len == 0 {
		return cfg
	}
	mut tdirs := []string{}
	mut errs := []string{}
	for arg in targs {
		match arg {
			'-asan-compiler', '--asan-compiler' {
				cfg.is_asan_compiler = true
			}
			'-msan-compiler', '--msan-compiler' {
				cfg.is_msan_compiler = true
			}
			'-ubsan-compiler', '--ubsan-compiler' {
				cfg.is_ubsan_compiler = true
			}
			else {
				if arg.starts_with('-') {
					errs << 'error: unknown flag `${arg}`'
					continue
				}
				if !os.is_dir(os.join_path(vroot, arg)) {
					errs << 'error: failed to find directory `${arg}`'
					continue
				}
				tdirs << arg
			}
		}
	}
	if errs.len > 0 {
		return error(errs.join_lines())
	}
	if tdirs.len > 0 {
		cfg.test_dirs = tdirs
	}
	return cfg
}

fn main() {
	os.chdir(vroot) or { panic(err) }
	args_idx := os.args.index('test-self')
	vargs := os.args[1..args_idx]
	targs := os.args#[args_idx + 1..]
	cfg := Config.init(vargs, targs) or {
		eprintln(err)
		exit(1)
	}
	// dump(cfg)
	title := 'testing: ${cfg.test_dirs.join(', ')}'
	mut tpaths := map[string]bool{}
	mut tpaths_ref := &tpaths
	for dir in cfg.test_dirs {
		os.walk(os.join_path(vroot, dir), fn [mut tpaths_ref] (p string) {
			if p.ends_with('_test.v') || p.ends_with('_test.c.v')
				|| (testing.is_node_present && p.ends_with('_test.js.v')) {
				unsafe {
					tpaths_ref[p] = true
				}
			}
		})
	}
	mut all_test_files := tpaths.keys()
	if cfg.run_just_essential {
		all_test_files = essential_list.map(os.join_path(vroot, it))
	}
	mut tsession := testing.new_test_session(vargs.join(' '), true)
	tsession.exec_mode = .compile_and_run
	tsession.files << all_test_files.filter(!it.contains('testdata' + os.path_separator))
	if !cfg.run_slow_sanitize
		&& ((cfg.sanitize_undefined || cfg.sanitize_memory || cfg.sanitize_address)
		|| (cfg.is_msan_compiler || cfg.is_asan_compiler || cfg.is_ubsan_compiler)) {
		tsession.skip_files << skip_fsanitize_too_slow
		tsession.custom_defines << 'self_sanitize_too_slow'
	}
	if cfg.werror {
		tsession.custom_defines << 'self_werror'
	}
	if cfg.sanitize_memory {
		tsession.skip_files << skip_with_fsanitize_memory
		tsession.custom_defines << 'self_sanitize_memory'
	}
	if cfg.sanitize_address {
		tsession.skip_files << skip_with_fsanitize_address
		tsession.custom_defines << 'self_sanitize_address'
	}
	if cfg.sanitize_undefined {
		tsession.skip_files << skip_with_fsanitize_undefined
		tsession.custom_defines << 'self_sanitize_undefined'
	}
	if cfg.is_asan_compiler {
		tsession.custom_defines << 'self_asan_compiler'
	}
	if cfg.is_msan_compiler {
		tsession.custom_defines << 'self_msan_compiler'
	}
	if cfg.is_ubsan_compiler {
		tsession.custom_defines << 'self_ubsan_compiler'
	}
	if cfg.is_sandboxed_packaging {
		tsession.custom_defines << 'self_sandboxed_packaging'
	}
	if cfg.is_ubuntu_musl_ci {
		tsession.skip_files << skip_on_ubuntu_musl
		tsession.custom_defines << 'self_ubuntu_musl_ci'
	}
	// dump(tsession.skip_files)
	mut unavailable_files := tsession.files.filter(!os.exists(it))
	unavailable_files << tsession.skip_files.filter(it != 'do_not_remove' && !os.exists(it))
	if unavailable_files.len > 0 {
		for f in unavailable_files {
			eprintln('error: failed to find file: ${f}')
		}
		exit(1)
	}
	tsession.skip_files = tsession.skip_files.map(os.abs_path)
	tsession.session_start(title)
	tsession.test()
	tsession.session_stop(title)
	if tsession.benchmark.nfail > 0 {
		eprintln('\nError: failed ${tsession.benchmark.nfail} times.\n')
		exit(1)
	}
}
