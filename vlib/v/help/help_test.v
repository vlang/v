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
	topics := topic_paths.map(it.all_after_last(os.path_separator).replace('.txt', ''))
	for t in topics {
		res := os.execute(vexe + ' help ${t}')
		assert res.exit_code == 0, res.output
		assert res.output != ''
	}
}

fn test_uknown_topic() {
	res := os.execute(vexe + ' help abc')
	assert res.exit_code == 1, res.output
	assert res.output.starts_with('error: unknown help topic "abc".')
}

fn test_topic_sub_help() {
	res := os.execute(vexe + ' fmt --help')
	assert res.exit_code == 0, res.output
	assert res.output != ''
}
