// This test ensures the -new-generic-solver option, and its corresponding compiler stage, will not regress silently,
// as V continues to change. If you make changes to the generics stage to improve it, once you spot a failure here, or
// some of the expected tests starts passing (which will be also a failure as far as this _test.v file is concerned),
// you have to edit new_generics_regression_test.v, and remove the corresponding entry inside `failing_tests`. You also
// need to update the summary line too, so that it matches the current result of running:
// `./v -new-generic-solver test vlib/v/tests/generics/`
// on your local working branch.
import os
import log

const vexe = @VEXE
const vroot = @VROOT
const vtrace_output = os.getenv('VTRACE_OUTPUT').int() != 0

fn testsuite_begin() {
	os.chdir(@VROOT)!
	os.setenv('VJOBS', '1', true)
	os.setenv('VCOLORS', 'never', true)
}

fn test_new_generic_solver_does_not_regress_silently() {
	new_generic_test_cmd := '${os.quoted_path(vexe)} -new-generic-solver test vlib/v/tests/generics/'
	log.info('>>> running ${new_generic_test_cmd} ...')
	res := os.execute(new_generic_test_cmd)
	log.info('>>> done running ${new_generic_test_cmd} ; exit_code: ${res.exit_code}')
	assert res.exit_code != 0

	res_lines := res.output.split_into_lines()
	if vtrace_output {
		for tline in res_lines {
			eprintln('>>>>> tline: ${tline}')
		}
	}

	ok_lines := res_lines.filter(it.starts_with('OK '))
	failure_lines := res_lines.filter(it.starts_with(' FAIL'))
	summary_lines := res_lines.filter(it.starts_with('Summary'))

	for idx, known in failing_tests {
		found_expected_failure := failure_lines.any(it.contains(known))
		assert found_expected_failure, 'expected failing test ${known} , was not found.
        Run `v -new-generic-solver test vlib/v/tests/generics/` manually to verify, and then edit ${@FILE} to reflect the new state.'

		if vtrace_output {
			eprintln('>>>>> found_expected_failure ${idx + 1} ${known}, OK')
		}
	}

	assert summary_lines.any(it.contains(expected_summary)), 'could not find the expected_summary in: ${summary_lines}'
	log.info('>>> Found the expected summary: ${expected_summary}, OK')
}

const expected_summary = 'Summary for all V _test.v files: 52 failed, 209 passed, 261 total.'
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
]
