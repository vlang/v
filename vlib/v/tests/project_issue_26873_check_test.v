import os

const vexe = os.quoted_path(@VEXE)
const issue_26873_project = os.join_path(os.dir(@FILE), 'project_issue_26873')

fn test_project_with_line_three_top_level_tokens_checks_cleanly() {
	res := os.execute('${vexe} -check ${os.quoted_path(issue_26873_project)}')
	assert res.exit_code == 0, res.output
}
