import os

const vexe = os.quoted_path(@VEXE)

fn test_help() {
	res := os.execute(vexe + ' help')
	assert res.exit_code == 0
	assert res.output.starts_with('V is a tool for managing V source code.')
}

fn test_help_as_short_option() {
	res := os.execute(vexe + ' -h')
	assert res.exit_code == 0
	assert res.output.starts_with('V is a tool for managing V source code.')
}

fn test_help_as_long_option() {
	res := os.execute(vexe + ' --help')
	assert res.exit_code == 0
	assert res.output.starts_with('V is a tool for managing V source code.')
}

fn test_all_topics() {
	help_dir := os.join_path(@VEXEROOT, 'vlib', 'v', 'help')
	topic_paths := os.walk_ext(help_dir, '.txt')
	topics := topic_paths.map(os.file_name(it).replace('.txt', ''))
	for t in topics {
		res := os.execute(vexe + ' help ${t}')
		assert res.exit_code == 0, res.output
		assert res.output != ''
	}
}

fn test_unknown_topic() {
	res := os.execute(vexe + ' help abc')
	assert res.exit_code == 1, res.output
	assert res.output.starts_with('error: unknown help topic "abc".')
}

fn test_topics_output() {
	res := os.execute(vexe + ' help topics')
	assert res.exit_code == 0, res.output
	assert res.output != '', res.output
	assert !res.output.contains('default')
}

fn test_topic_sub_help() {
	res := os.execute(vexe + ' fmt --help')
	assert res.exit_code == 0, res.output
	assert res.output != ''
}

fn test_help_topic_with_cli_mod() {
	res := os.execute_or_exit(vexe + ' help init')
	assert res.output.contains('Usage: v init [flags]')
	assert res.output.contains('Sets up a V project within the current directory.')
	assert res.output.contains('Flags:')
	assert res.output.contains('--bin               Use the template for an executable application [default]')
	assert res.output.contains('--lib               Use the template for a library project.')
}
