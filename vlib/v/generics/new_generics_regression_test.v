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
	// The nested `v test` runs should validate their full target sets, not inherit outer runner filters/retry state.
	for key in ['VTEST_ONLY', 'VTEST_ONLY_FN', 'VTEST_FAIL_FAST', 'VTEST_RETRY', 'VTEST_RETRY_MAX'] {
		os.unsetenv(key)
	}
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

	res_lines := res.output.split_into_lines()
	if vtrace_output {
		for tline in res_lines {
			eprintln('>>>>> tline: ${tline}')
		}
	}

	failure_lines := res_lines.filter(it.starts_with(' FAIL'))
	summary_lines := res_lines.filter(it.starts_with('Summary'))

	actual_expected_summary := $if msvc { expected_summsvc } $else { expected_summary }
	actual_clean_summary := if root_label == 'vlib/flag' {
		$if msvc { expected_summsvc_flag_clean } $else { expected_summary_flag_clean }
	} else {
		''
	}
	found_expected_summary := summary_lines.any(it.contains(actual_expected_summary))
	found_clean_summary := actual_clean_summary != ''
		&& summary_lines.any(it.contains(actual_clean_summary))
	if !found_expected_summary && !found_clean_summary {
		eprintln('----------------------------------------------------------------')
		eprintln('----------------------------------------------------------------')
		for tline in res_lines {
			eprintln('>>>>> tline: ${tline}')
		}
		eprintln('----------------------------------------------------------------')
		eprintln('----------------------------------------------------------------')
		eprintln('Could not find an accepted summary in: ${summary_lines}')
		eprintln('actual_expected_summary: ${actual_expected_summary}')
		if actual_clean_summary != '' {
			eprintln('actual_clean_summary: ${actual_clean_summary}')
		}
		exit(1)
	}
	if found_clean_summary {
		log.info('>>> Found an accepted clean summary: ${term.colorize(term.yellow, actual_clean_summary)}, OK')
		println('')
		return
	}

	for idx, known in expected_failures {
		found_expected_failure := failure_lines.any(it.contains(known))
		assert found_expected_failure, 'expected failing test ${known} , was not found.\nRun `v -new-generic-solver test ${root_label}` manually to verify, and then edit ${@FILE} to reflect the new state.'

		if vtrace_output {
			eprintln('>>>>> found_expected_failure ${idx + 1} `${term.colorize(term.green,
				known)}`, OK')
		}
	}
	log.info('>>> Found the expected summary: ${term.colorize(term.yellow, actual_expected_summary)}, OK')
	println('')
}

const expected_summsvc_generics = 'Summary for all V _test.v files: 55 failed, 213 passed, 268 total.'
const expected_summary_generics = 'Summary for all V _test.v files: 54 failed, 214 passed, 268 total.'
const expected_summsvc_vec = 'Summary for all V _test.v files: 3 failed, 3 total.'
const expected_summary_vec = 'Summary for all V _test.v files: 3 failed, 3 total.'
const expected_summsvc_flag = 'Summary for all V _test.v files: 2 failed, 17 passed, 19 total.'
const expected_summary_flag = 'Summary for all V _test.v files: 2 failed, 17 passed, 19 total.'
const expected_summsvc_flag_clean = 'Summary for all V _test.v files: 19 passed, 19 total.'
const expected_summary_flag_clean = 'Summary for all V _test.v files: 19 passed, 19 total.'
const failing_tests = [
	'vlib/v/tests/generics/default_type_with_ref_test.v',
	'vlib/v/tests/generics/generic_array_of_alias_test.v',
	'vlib/v/tests/generics/generic_array_ret_test.v',
	'vlib/v/tests/generics/generic_array_test.v',
	'vlib/v/tests/generics/generic_comptime_arg_test.v',
	'vlib/v/tests/generics/generic_default_expression_in_or_block_test.v',
	'vlib/v/tests/generics/generic_different_type_test.v',
	'vlib/v/tests/generics/generic_dump_test.v',
	'vlib/v/tests/generics/generic_fn_assign_generics_struct_test.v',
	'vlib/v/tests/generics/generic_fn_infer_fn_type_argument_test.v',
	'vlib/v/tests/generics/generic_fn_infer_fixed_array_test.v',
	'vlib/v/tests/generics/generic_fn_infer_multi_paras_test.v',
	'vlib/v/tests/generics/generic_fn_type_with_different_generic_type_test.v',
	'vlib/v/tests/generics/generic_fn_typeof_name_test.v',
	'vlib/v/tests/generics/generic_fn_value_inference_test.v',
	'vlib/v/tests/generics/generic_fn_with_comptime_for_test.v',
	'vlib/v/tests/generics/generic_fn_typeof_name_test.v',
	'vlib/v/tests/generics/generic_fn_with_comptime_for_test.v',
	'vlib/v/tests/generics/generic_function_error_propagation_test.v',
	'vlib/v/tests/generics/generic_map_alias_test.v',
	'vlib/v/tests/generics/generic_muls_test.v',
	'vlib/v/tests/generics/generic_mut_pointer_param_test.v',
	'vlib/v/tests/generics/generic_operator_overload_test.v',
	'vlib/v/tests/generics/generic_receiver_embed_test.v',
	'vlib/v/tests/generics/generic_recursive_fn_test.v',
	'vlib/v/tests/generics/generic_resolve_test.v',
	'vlib/v/tests/generics/generic_return_test.v',
	'vlib/v/tests/generics/generic_selector_field_test.v',
	'vlib/v/tests/generics/generic_selector_type_test.v',
	'vlib/v/tests/generics/generic_smartcast_test.v',
	'vlib/v/tests/generics/generic_struct_cstruct_test.v',
	'vlib/v/tests/generics/generic_struct_test.v',
	'vlib/v/tests/generics/generic_sumtype_str_test.v',
	'vlib/v/tests/generics/generic_typeof_test.v',
	'vlib/v/tests/generics/generics_array_builtin_method_call_test.v',
	'vlib/v/tests/generics/generics_array_delete_test.v',
	'vlib/v/tests/generics/generics_array_method_call_with_multi_types_test.v',
	'vlib/v/tests/generics/generics_array_of_threads_test.v',
	'vlib/v/tests/generics/generics_chans_select_test.v',
	'vlib/v/tests/generics/generics_fn_return_result_test.v',
	'vlib/v/tests/generics/generics_fn_variable_3_test.v',
	'vlib/v/tests/generics/generics_map_with_reference_arg_test.v',
	'vlib/v/tests/generics/generics_method_chaining_call_test.v',
	'vlib/v/tests/generics/generics_method_on_generic_structs_test.v',
	'vlib/v/tests/generics/generics_method_on_nested_struct2_test.v',
	'vlib/v/tests/generics/generics_method_str_overload_test.v',
	'vlib/v/tests/generics/generics_method_variable_test.v',
	'vlib/v/tests/generics/generics_method_with_diff_generic_names_test.v',
	'vlib/v/tests/generics/generics_nested_struct_init_test.v',
	'vlib/v/tests/generics/generics_stack_of_sumtype_push_test.v',
	'vlib/v/tests/generics/generics_str_intp_test.v',
	'vlib/v/tests/generics/generics_struct_inst_method_call_test.v',
	'vlib/v/tests/generics/generics_struct_parent_has_str_to_string_test.v',
	'vlib/v/tests/generics/generics_struct_with_array_test.v',
	'vlib/v/tests/generics/generics_with_assign_nested_generics_call_test.v',
	'vlib/v/tests/generics/generics_with_embed_generics_test.v',
	'vlib/v/tests/generics/generics_with_generics_fn_return_generics_map_type_test.v',
	'vlib/v/tests/generics/generics_with_multi_nested_generic_method_call_ref_arg_test.v',
	'vlib/v/tests/generics/generics_with_multi_nested_generic_method_call_test.v',
	'vlib/v/tests/generics/generics_with_nested_generic_method_call_test.v',
	'vlib/v/tests/generics/generics_with_nested_generics_fn_infer_call_test.v',
]!
const failing_math_vec_tests = [
	'vlib/math/vec/vec2_test.v',
	'vlib/math/vec/vec3_test.v',
	'vlib/math/vec/vec4_test.v',
]!

const failing_flag_tests = [
	'vlib/flag/default_flag_options_test.v',
	'vlib/flag/usage_example_test.v',
]!
