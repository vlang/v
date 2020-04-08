module main

import os
import testing
import v.pref

const (
	skip_test_files = [
		'vlib/arrays/arrays_test.v',
		'vlib/builtin/map_test.v',
		'vlib/cli/command_test.v',
		'vlib/cli/flag_test.v',
		'vlib/clipboard/clipboard_test.v', // Linux only
		'vlib/crypto/aes/aes_test.v',
		'vlib/crypto/rand/rand_test.v',
		'vlib/crypto/rc4/rc4_test.v',
		'vlib/encoding/base64/base64_memory_test.v',
		'vlib/encoding/base64/base64_test.v',
		'vlib/encoding/csv/csv_test.v',
		'vlib/encoding/utf8/utf8_util_test.v',
		'vlib/eventbus/eventbus_test.v',
		'vlib/flag/flag_test.v',
		'vlib/json/json_test.v',
		'vlib/math/big/big_test.v',
		'vlib/math/bits/bits_test.v',
		'vlib/math/complex/complex_test.v',
		'vlib/math/factorial/factorial_test.v',
		'vlib/math/fractions/fraction_test.v',
		'vlib/math/stats/stats_test.v',
		'vlib/net/ftp/ftp_test.v',
		'vlib/net/http/http_httpbin_test.v',
		'vlib/net/http/http_test.v',
		'vlib/net/socket_test.v',
		'vlib/net/socket_udp_test.v',
		'vlib/os/environment_test.v', // Linux only
		'vlib/rand/pcg32_test.v',
		'vlib/rand/splitmix64_test.v',
		'vlib/regex/regex_test.v',
		'vlib/sqlite/sqlite_test.v', // Linux only
		'vlib/strconv/ftoa/f32_f64_to_string_test.v',
		//'vlib/v/parser/parser_test.v', // exits early, but it should compile
		'vlib/v/tests/array_to_string_test.v',
		'vlib/v/tests/asm_test.v', // Linux only
		'vlib/v/tests/backtrace_test.v', // TCC only
		'vlib/v/tests/enum_bitfield_test.v',
		'vlib/v/tests/fixed_array_test.v',
		'vlib/v/tests/fn_test.v',
		'vlib/v/tests/fn_variadic_test.v',
		'vlib/v/tests/live_test.v', // Linux only
		'vlib/v/tests/match_test.v',
		'vlib/v/tests/msvc_test.v',
		'vlib/v/tests/mut_test.v',
		'vlib/v/tests/num_lit_call_method_test.v',
		'vlib/v/tests/option_test.v',
		'vlib/v/tests/pointers_test.v',
		'vlib/v/tests/project_with_c_code/main_test.v',
		'vlib/v/tests/project_with_modules_having_submodules/bin/a_program_under_bin_can_find_mod1_test.v',
		'vlib/v/tests/project_with_modules_having_submodules/tests/submodule_test.v',
		'vlib/v/tests/repl/repl_test.v',
		'vlib/v/tests/string_interpolation_array_of_structs_test.v',
		'vlib/v/tests/string_interpolation_struct_test.v',
		'vlib/v/tests/string_interpolation_variadic_test.v',
		'vlib/v/tests/type_test.v',
		'vlib/v/tests/typeof_test.v',
		'vlib/v/tests/valgrind/valgrind_test.v', // ubuntu-musl only
		'vlib/vweb/assets/assets_test.v',
		'vlib/hash/crc32/crc32_test.v'
	]
)

fn main() {
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	args := os.args
	args_string := args[1..].join(' ')
	cmd_prefix := args_string.all_before('test-fixed')
	title := 'testing all fixed tests'

	all_test_files := os.walk_ext( os.join_path(vroot,'vlib'), '_test.v')
	testing.eheader(title)
	mut tsession := testing.new_test_session(cmd_prefix)
	tsession.files << all_test_files
	tsession.skip_files << skip_test_files
	tsession.test()
	eprintln(tsession.benchmark.total_message(title))
	if tsession.benchmark.nfail > 0 {
		panic('\nWARNING: failed ${tsession.benchmark.nfail} times.\n')
	}
}
