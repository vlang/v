import os

const pr_28064_vexe = @VEXE
const pr_28064_tests_dir = os.dir(@FILE)
const pr_28064_v3_dir = os.dir(pr_28064_tests_dir)
const pr_28064_vlib_dir = os.dir(pr_28064_v3_dir)
const pr_28064_v3_src = os.join_path(pr_28064_v3_dir, 'v3.v')

fn test_pr_28064_v1_regressions_pass_with_v3() {
	root := os.join_path(os.temp_dir(), 'v3_pr_28064_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}

	v3_bin := os.join_path(root, 'v3')
	build :=
		os.execute('${os.quoted_path(pr_28064_vexe)} -gc none -path "${pr_28064_vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(v3_bin)} ${os.quoted_path(pr_28064_v3_src)}')
	assert build.exit_code == 0, build.output

	regressions := {
		'#27988': os.join_path(pr_28064_vlib_dir, 'v', 'tests', 'conditions', 'ifs',
			'if_expr_match_result_propagation_issue_27988_test.v')
		'#28015': os.join_path(pr_28064_vlib_dir, 'v', 'tests', 'interfaces',
			'result_error_return_from_option_interface_or_block_issue_28015_test.v')
		'#28017': os.join_path(pr_28064_vlib_dir, 'v', 'tests', 'fns',
			'variadic_optional_fn_type_issue_28017_test.v')
	}
	for issue, source in regressions {
		result := os.execute('${os.quoted_path(v3_bin)} -silent -nocache ${os.quoted_path(source)}')
		assert result.exit_code == 0, '${issue} failed with V3:\n${result.output}'
	}
}
