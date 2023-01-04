import os

const vexe = os.getenv('VEXE')

fn test_help() {
	res := os.execute('${os.quoted_path(vexe)} help')
	assert res.exit_code == 0
	assert res.output.starts_with('V is a tool for managing V source code.')
}

fn test_help_as_short_option() {
	res := os.execute('${os.quoted_path(vexe)} -h')
	assert res.exit_code == 0
	assert res.output.starts_with('V is a tool for managing V source code.')
}

fn test_help_as_long_option() {
	res := os.execute('${os.quoted_path(vexe)} --help')
	assert res.exit_code == 0
	assert res.output.starts_with('V is a tool for managing V source code.')
}
