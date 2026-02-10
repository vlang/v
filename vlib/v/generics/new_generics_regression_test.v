// vtest build: !docker-ubuntu-musl && !sanitize-memory-gcc && !sanitize-address-gcc && !sanitize-address-clang
// This test ensures the -new-generic-solver option, and its corresponding compiler stage, will not regress silently,
// as V continues to change. If you make changes to the generics stage to improve it, once you spot a failure here, or
// some of the expected tests starts passing (which will be also a failure as far as this _test.v file is concerned),
// you have to edit new_generics_regression_test.v, and remove the corresponding entry inside `failing_tests`. You also
// need to update the summary line too, so that it matches the current result of running:
// `./v -new-generic-solver test vlib/v/tests/generics/`
// `./v -new-generic-solver test vlib/math/vec`
// on your local working branch.
// TODO: investigate what exact test fails on msvc only and record it in a separate skip list.
// TODO: remove this test, once -new-generic-solver has improved so much, that it becomes the new default, since it is slow (~32s on m1).
import os
import log
import term

const vexe = @VEXE
const vroot = @VEXEROOT
const vtrace_output = os.getenv('VTRACE_OUTPUT').int() != 0

fn testsuite_begin() {
	unbuffer_stdout()
	os.chdir(vroot)!
	os.setenv('VJOBS', '1', true)
	// important, when this test itself is run through `v test`; this simplify the handling of the output of the inner `v test` commands
	os.setenv('VCOLORS', 'never', true)
}

fn test_new_generic_solver_does_not_regress_silently() {
	run_new_generic_solver_tests('vlib/math/vec', '${os.quoted_path(vexe)} -new-generic-solver test vlib/math/vec',
		expected_summary_vec, expected_summsvc_vec, failing_math_vec_tests[..])
	run_new_generic_solver_tests('vlib/flag', '${os.quoted_path(vexe)} -new-generic-solver test vlib/flag/',
		expected_summary_flag, expected_summsvc_flag, failing_flag_tests[..])
	run_new_generic_solver_tests('vlib/v/tests/generics/', '${os.quoted_path(vexe)} -new-generic-solver test vlib/v/tests/generics/',
		expected_summary_generics, expected_summsvc_generics, failing_tests[..])
}

fn run_new_generic_solver_tests(root_label string, test_cmd string, expected_summary string,
	expected_summsvc string, expected_failures []string) {
	log.info('>>> running ${term.colorize(term.magenta, test_cmd)} ...')
	res := os.execute(test_cmd)
	log.info('>>> done running ${test_cmd} ; exit_code: ${res.exit_code}')
	assert res.exit_code != 0

	res_lines := res.output.split_into_lines()
	if vtrace_output {
		for tline in res_lines {
			eprintln('>>>>> tline: ${tline}')
		}
	}

	failure_lines := res_lines.filter(it.starts_with(' FAIL'))
	summary_lines := res_lines.filter(it.starts_with('Summary'))

	for idx, known in expected_failures {
		found_expected_failure := failure_lines.any(it.contains(known))
		assert found_expected_failure, 'expected failing test ${known} , was not found.\nRun `v -new-generic-solver test ${root_label}` manually to verify, and then edit ${@FILE} to reflect the new state.'

		if vtrace_output {
			eprintln('>>>>> found_expected_failure ${idx + 1} `${term.colorize(term.green,
				known)}`, OK')
		}
	}

	actual_expected_summary := $if msvc { expected_summsvc } $else { expected_summary }

	if !summary_lines.any(it.contains(actual_expected_summary)) {
		eprintln('----------------------------------------------------------------')
		eprintln('----------------------------------------------------------------')
		for tline in res_lines {
			eprintln('>>>>> tline: ${tline}')
		}
		eprintln('----------------------------------------------------------------')
		eprintln('----------------------------------------------------------------')
		eprintln('Could not find the actual_expected_summary in: ${summary_lines}')
		eprintln('actual_expected_summary: ${actual_expected_summary}')
		exit(1)
	}
	log.info('>>> Found the expected summary: ${term.colorize(term.yellow, actual_expected_summary)}, OK')
	println('')
}

const expected_summsvc_generics = 'Summary for all V _test.v files: 53 failed, 209 passed, 262 total.'
const expected_summary_generics = 'Summary for all V _test.v files: 52 failed, 210 passed, 262 total.'
const expected_summsvc_vec = 'Summary for all V _test.v files: 3 failed, 3 total.'
const expected_summary_vec = 'Summary for all V _test.v files: 3 failed, 3 total.'
const expected_summsvc_flag = 'Summary for all V _test.v files: 14 failed, 5 passed, 19 total.'
const expected_summary_flag = 'Summary for all V _test.v files: 14 failed, 5 passed, 19 total.'
const failing_tests = [
	'vlib/v/tests/generics/concrete_type_as_generic_fn_type_1_test.v',
	'vlib/v/tests/generics/default_type_with_ref_test.v',
	'vlib/v/tests/generics/generic_alias_callback_test.v',
	'vlib/v/tests/generics/generic_anon_fn_inside_generic_fn_test.v',
	'vlib/v/tests/generics/generic_array_init_test.v',
	'vlib/v/tests/generics/generic_array_ret_test.v',
	'vlib/v/tests/generics/generic_complex_sumtype_test.v',
	'vlib/v/tests/generics/generic_comptime_arg_test.v',
	'vlib/v/tests/generics/generic_comptime_test.v',
	'vlib/v/tests/generics/generic_default_expression_in_or_block_test.v',
	'vlib/v/tests/generics/generic_fn_infer_fixed_array_test.v',
	'vlib/v/tests/generics/generic_fn_infer_multi_paras_test.v',
	'vlib/v/tests/generics/generic_fn_type_with_different_generic_type_test.v',
	'vlib/v/tests/generics/generic_fn_typeof_name_test.v',
	'vlib/v/tests/generics/generic_fn_with_comptime_for_test.v',
	'vlib/v/tests/generics/generic_map_alias_test.v',
	'vlib/v/tests/generics/generic_muls_test.v',
	'vlib/v/tests/generics/generic_operator_overload_test.v',
	'vlib/v/tests/generics/generic_options_with_reserved_ident_test.v',
	'vlib/v/tests/generics/generic_receiver_embed_test.v',
	'vlib/v/tests/generics/generic_resolve_test.v',
	'vlib/v/tests/generics/generic_return_test.v',
	'vlib/v/tests/generics/generic_selector_field_test.v',
	'vlib/v/tests/generics/generic_selector_len_test.v',
	'vlib/v/tests/generics/generic_spawn_test.v',
	'vlib/v/tests/generics/generic_static_call_test.v',
	'vlib/v/tests/generics/generic_struct_cstruct_test.v',
	'vlib/v/tests/generics/generic_struct_test.v',
	'vlib/v/tests/generics/generic_sumtype_cast_test.v',
	'vlib/v/tests/generics/generic_sumtype_str_test.v',
	'vlib/v/tests/generics/generic_sumtype_test.v',
	'vlib/v/tests/generics/generic_typeof_idx_test.v',
	'vlib/v/tests/generics/generic_typeof_test.v',
	'vlib/v/tests/generics/generics_array_of_threads_test.v',
	'vlib/v/tests/generics/generics_closure_fn_direct_call_test.v',
	'vlib/v/tests/generics/generics_closure_fn_test.v',
	'vlib/v/tests/generics/generics_closures_with_different_generic_types_test.v',
	'vlib/v/tests/generics/generics_fn_variable_1_test.v',
	'vlib/v/tests/generics/generics_fn_variable_2_test.v',
	'vlib/v/tests/generics/generics_fn_variable_3_test.v',
	'vlib/v/tests/generics/generics_for_in_iterate_test.v',
	'vlib/v/tests/generics/generics_interface_method_test.v',
	'vlib/v/tests/generics/generics_interface_with_generic_method_using_generic_struct_test.v',
	'vlib/v/tests/generics/generics_interface_with_generic_sumtype_test.v',
	'vlib/v/tests/generics/generics_map_with_reference_arg_test.v',
	'vlib/v/tests/generics/generics_method_chaining_call_test.v',
	'vlib/v/tests/generics/generics_method_with_sumtype_args_test.v',
	'vlib/v/tests/generics/generics_return_closure_test.v',
	'vlib/v/tests/generics/generics_struct_field_with_default_fn_type_test.v',
	'vlib/v/tests/generics/generics_struct_with_inconsistent_generic_types_1_test.v',
	'vlib/v/tests/generics/generics_test.v',
	'vlib/v/tests/generics/generics_with_generics_fn_return_generics_map_type_test.v',
]!
const failing_math_vec_tests = [
	'vlib/math/vec/vec2_test.v',
	'vlib/math/vec/vec3_test.v',
	'vlib/math/vec/vec4_test.v',
]!

const failing_flag_tests = [
	'vlib/flag/cmd_exe_style_flags_test.v',
	'vlib/flag/flag_from_test.v',
	'vlib/flag/flag_to_bool_test.v',
	'vlib/flag/flag_to_doc_test.v',
	'vlib/flag/flag_to_edge_case_1_test.v',
	'vlib/flag/flag_to_misc_test.v',
	'vlib/flag/flag_to_relaxed_test.v',
	'vlib/flag/flag_to_tail_bool_test.v',
	'vlib/flag/flag_to_tail_test.v',
	'vlib/flag/gnu_style_flags_test.v',
	'vlib/flag/go_flag_style_flags_test.v',
	'vlib/flag/posix_style_flags_test.v',
	'vlib/flag/v_flag_parser_style_flags_test.v',
	'vlib/flag/v_style_flags_test.v',
]!
