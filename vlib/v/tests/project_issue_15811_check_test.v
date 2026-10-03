import os

const vexe = os.quoted_path(@VEXE)
const issue_15811_project = os.join_path(os.dir(@FILE), 'project_issue_15811')

fn test_private_c_redeclaration_order_checks_cleanly() {
	res := os.exec([@VEXE, '-check', '${issue_15811_project}'])
	assert res.exit_code == 0, res.output
}
