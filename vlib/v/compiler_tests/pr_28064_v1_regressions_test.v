import os

const pr_28064_test_dir = os.join_path(os.dir(os.dir(@FILE)), 'tests')

fn test_pr_28064_v1_regressions_pass_with_v3() {
	regressions := {
		'#27988': os.join_path(pr_28064_test_dir, 'conditions', 'ifs',
			'if_expr_match_result_propagation_issue_27988_test.v')
		'#28015': os.join_path(pr_28064_test_dir, 'interfaces',
			'result_error_return_from_option_interface_or_block_issue_28015_test.v')
		'#28017': os.join_path(pr_28064_test_dir, 'fns',
			'variadic_optional_fn_type_issue_28017_test.v')
	}
	for issue, source in regressions {
		result := os.exec([@VEXE, '-silent', '-nocache', source])
		assert result.exit_code == 0, '${issue} failed with V3:\n${result.output}'
	}
}
