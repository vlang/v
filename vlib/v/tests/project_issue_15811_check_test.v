import os

const vexe = os.quoted_path(@VEXE)
const issue_15811_project = os.join_path(os.dir(@FILE), 'project_issue_15811')

fn test_private_c_redeclaration_order_checks_cleanly() {
	res := os.execute('${vexe} -check ${os.quoted_path(issue_15811_project)}')
	assert res.exit_code == 0, res.output
}
