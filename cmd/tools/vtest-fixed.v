module main

import os
import testing

const (
	fixed_test_files = [
		'vlib/builtin/array_test.v',
		'vlib/builtin/string_test.v',
		'vlib/strconv/atof_test.v',
		'vlib/math/math_test.v',
		'vlib/bitfield/bitfield_test.v',
		'vlib/os/os_test.v',
		'vlib/os/inode_test.v',
		'vlib/v/tests/fn_multiple_returns_test.v',
		'vlib/v/tests/return_voidptr_test.v',
		'vlib/v/tests/voidptr_to_u64_cast_b_test.v',
		'vlib/v/tests/cstrings_test.v',
		'vlib/v/tests/valgrind/valgrind_test.v',
		'vlib/v/tests/nameof_test.v',
		'vlib/v/tests/repeated_multiret_values_test.v',
		'vlib/v/tests/modules/amodule/internal_module_test.v',
		'vlib/v/tests/modules/amodule/another_internal_module_test.v',
		'vlib/v/tests/modules/simplemodule/importing_test.v',
		'vlib/v/tests/reusable_mut_multiret_values_test.v',
		'vlib/v/tests/print_test.v',
		'vlib/v/tests/defer_test.v',
		'vlib/v/tests/typeof_simple_types_test.v',
		'vlib/v/tests/struct_chained_fields_correct_test.v',
		'vlib/v/tests/if_expression_test.v',
		'vlib/v/tests/backtrace_test.v',
		'vlib/v/tests/interfaces_map_test.v',
		'vlib/v/tests/fn_expecting_ref_but_returning_struct_test.v',
		'vlib/v/tests/inout/compiler_test.v',
		'vlib/v/tests/generic_test.v',
		'vlib/v/tests/interface_test.v',
		'vlib/v/tests/in_expression_test.v',
		'vlib/v/tests/attribute_test.v',
		'vlib/v/tests/str_gen_test.v',
		'vlib/v/tests/enum_hex_test.v',
		'vlib/v/tests/option_default_values_test.v',
   ]
)

fn main() {
	args := os.args
	args_string := args[1..].join(' ')
	cmd_prefix := args_string.all_before('test-fixed')
	title := 'testing all fixed tests'
	testing.eheader(title)
	mut tsession := testing.new_test_session(cmd_prefix)
	tsession.files << fixed_test_files
	tsession.test()
	eprintln(tsession.benchmark.total_message(title))
	if tsession.benchmark.nfail > 0 {
		panic('\nWARNING: failed ${tsession.benchmark.nfail} times.\n')
	}   
}
