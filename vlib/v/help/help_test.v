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

fn test_uknown_topic() {
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
	// Note: `os.execute` will add line breaks that are not added when using `os.system('v help init')`
	// Or in the output of the cli module when using `v init --help`. For now, there is a manually
	// added line break after the `--bin` flag line in the output below.
	assert res.output.trim_space() == "Usage: v init [flags]

Sets up a V project within the current directory.

If no `v.mod` exists, a setup prompt is started to create one with the project's metadata.
If no `.v` file exists, a project template is generated. If the current directory is not a
git project and git is installed, `git init` will be performed during the setup.

Flags:
  --bin               Use the template for an executable application [default]
                      .
  --lib               Use the template for a library project.
  --web               Use the template for a vweb project.
  --help              Print help information."
}
