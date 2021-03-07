import os

fn test_help() {
	vexe := os.getenv('VEXE')
	res := os.exec('"$vexe" help') or { panic(err) }
	assert res.exit_code == 0
	assert res.output.starts_with('V is a tool for managing V source code.')
}

fn test_help_as_short_option() {
	vexe := os.getenv('VEXE')
	res := os.exec('"$vexe" -h') or { panic(err) }
	assert res.exit_code == 0
	assert res.output.starts_with('V is a tool for managing V source code.')
}

fn test_help_as_long_option() {
	vexe := os.getenv('VEXE')
	res := os.exec('"$vexe" --help') or { panic(err) }
	assert res.exit_code == 0
	assert res.output.starts_with('V is a tool for managing V source code.')
}
