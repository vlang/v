import os

const vexe = os.quoted_path(@VEXE)
const issue_27078_project = os.join_path(os.dir(@FILE), 'project_issue_27078')

fn test_fixed_array_with_int_cast_enum_const_in_separate_file_runs() {
	res := os.execute('${vexe} run ${os.quoted_path(issue_27078_project)}')
	assert res.exit_code == 0, res.output
}
