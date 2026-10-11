import os

const vexe = os.quoted_path(@VEXE)

fn test_help() {
	res := os.exec([@VEXE, 'help'])
	assert res.exit_code == 0
	assert res.output.starts_with('V is a tool for managing V source code.')
}

fn test_help_as_short_option() {
	res := os.exec([@VEXE, '-h'])
	assert res.exit_code == 0
	assert res.output.starts_with('V is a tool for managing V source code.')
}

fn test_help_as_long_option() {
	res := os.exec([@VEXE, '--help'])
	assert res.exit_code == 0
	assert res.output.starts_with('V is a tool for managing V source code.')
}

fn test_run_help_text() {
	res := os.exec([@VEXE, 'help'])
	assert res.exit_code == 0, res.output
	assert res.output.contains('executable if V created it for this run.')
	assert res.output.contains('Compile and run a V program, keeping the')
	assert res.output.contains('executable for reuse.')
}

fn test_run_topic_mentions_conditional_cleanup() {
	res := os.exec([@VEXE, 'help', 'run'])
	assert res.exit_code == 0, res.output
	assert res.output.contains('If `v run` created the executable for this run')
	assert res.output.contains('If the executable already existed before the command')
}

fn test_build_topic_lists_fastc_backend() {
	res := os.exec([@VEXE, 'help', 'build'])
	assert res.exit_code == 0, res.output
	assert res.output.contains('* `fastc`'), res.output
	assert res.output.contains('available on macOS, Linux, and Windows'), res.output
	assert res.output.contains('`-d skip_fastc`'), res.output
	assert res.output.contains('See `v help vsh`'), res.output
}

fn test_vsh_topic() {
	res := os.exec([@VEXE, 'help', 'vsh'])
	assert res.exit_code == 0, res.output
	assert res.output.contains('v build script.vsh'), res.output
	assert res.output.contains('`os` types still need it (`os.File`)'), res.output
}

fn test_all_topics() {
	help_dir := os.join_path(@VEXEROOT, 'vlib', 'v', 'help')
	topic_paths := os.walk_ext(help_dir, '.txt')
	topics := topic_paths.map(os.file_name(it).replace('.txt', ''))
	for t in topics {
		res := os.exec([@VEXE, 'help', '${t}'])
		assert res.exit_code == 0, res.output
		assert res.output != ''
	}
}

fn test_up_topic_lists_the_skills_flag() {
	res := os.exec([@VEXE, 'help', 'up'])
	assert res.exit_code == 0, res.output
	assert res.output.contains('-skills'), res.output
	assert res.output.contains('Refresh the installed agent skills'), res.output
}

fn test_up_topic_says_the_report_leaves_edited_skills_alone() {
	res := os.exec([@VEXE, 'help', 'up'])
	assert res.exit_code == 0, res.output
	assert res.output.contains('reported but left alone'), res.output
}

fn test_other_topic_summary_of_skills_mentions_update() {
	res := os.exec([@VEXE, 'help', 'other'])
	assert res.exit_code == 0, res.output
	// The one-line summary is what a reader sees in the command list, so a
	// subcommand missing from it is a subcommand the list claims is absent.
	assert res.output.contains('List, install, update and remove the agent skills'), res.output
}

fn test_unknown_topic() {
	res := os.exec([@VEXE, 'help', 'abc'])
	assert res.exit_code == 1, res.output
	assert res.output.starts_with('error: unknown help topic "abc".')
}

fn test_topics_output() {
	res := os.exec([@VEXE, 'help', 'topics'])
	assert res.exit_code == 0, res.output
	assert res.output != '', res.output
	assert !res.output.contains('default')
}

fn test_topic_sub_help() {
	res := os.exec([@VEXE, 'fmt', '--help'])
	assert res.exit_code == 0, res.output
	assert res.output != ''
}

fn test_help_topic_with_cli_mod() {
	res := os.exec_or_exit([@VEXE, 'help', 'init'])
	assert res.output.contains('Usage: v init [flags]')
	assert res.output.contains('Sets up a V project within the current directory.')
	assert res.output.contains('Flags:')
	assert res.output.contains('--bin               Use the template for an executable application [default]')
	assert res.output.contains('--lib               Use the template for a library project.')
}
