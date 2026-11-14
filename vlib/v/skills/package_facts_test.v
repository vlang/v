module main

import os

// These tests hold the package skills to the CLI they describe. Agents read the
// skills as ground truth, so a subcommand, an option or a file the CLI does not
// have, or no longer has, is a wrong instruction rather than a stale comment.

const skills_dir = os.join_path(@VEXEROOT, 'vlib', 'v', 'skills')

const package_skill_files = [
	os.join_path(skills_dir, 'v-package', 'SKILL.md'),
	os.join_path(skills_dir, 'v-package', 'references', 'SUBCOMMANDS.md'),
	os.join_path(skills_dir, 'v-workflow', 'references', 'V-MOD.md'),
]

const subcommands_file = os.join_path(skills_dir, 'v-package', 'references', 'SUBCOMMANDS.md')

fn help_for(subcommand string) os.Result {
	return os.exec([@VEXE, 'help', subcommand])
}

// shell_commands returns the `v ...` lines of the bash blocks of a markdown file,
// without their trailing comments.
fn shell_commands(path string) []string {
	mut commands := []string{}
	mut in_bash := false
	for line in os.read_lines(path) or { panic(err) } {
		trimmed := line.trim_space()
		if trimmed.starts_with('```') {
			in_bash = !in_bash && trimmed == '```bash'
			continue
		}
		if in_bash && trimmed.starts_with('v ') {
			commands << trimmed.all_before('#').trim_space()
		}
	}
	return commands
}

// backticked_options returns the options quoted in backticks in a piece of text,
// such as `--locked` or `-p <module>`.
fn backticked_options(text string) []string {
	mut options := []string{}
	for i, part in text.split('`') {
		if i % 2 == 1 && part.starts_with('-') {
			options << part.all_before(' ')
		}
	}
	return options
}

fn test_every_subcommand_in_the_package_skills_has_help() {
	for path in package_skill_files {
		for command in shell_commands(path) {
			subcommand := command.fields()[1]
			assert !subcommand.starts_with('-'), '${path}: `${command}`'
			result := help_for(subcommand)
			assert result.exit_code == 0, '${path}: `${command}` names a subcommand `v help` does not know:\n${result.output}'
		}
	}
}

fn test_every_option_in_the_package_skills_is_in_the_subcommand_help() {
	for path in package_skill_files {
		for command in shell_commands(path) {
			fields := command.fields()
			options := fields[2..].filter(it.starts_with('--'))
			if options.len == 0 {
				continue
			}
			help := help_for(fields[1]).output
			for option in options {
				assert help.contains(option), '${path}: `${command}` uses `${option}`, which `v help ${fields[1]}` does not mention'
			}
		}
	}
}

fn test_every_option_described_per_subcommand_is_in_its_help() {
	text := os.read_file(subcommands_file)!
	for section in text.split('\n## ')[1..] {
		subcommand := section.all_before('\n').trim_space()
		if subcommand.contains(' ') {
			// A section about the command surface as a whole, not one subcommand.
			continue
		}
		help := help_for(subcommand)
		assert help.exit_code == 0, 'SUBCOMMANDS.md describes `${subcommand}`, which `v help` does not know'
		for option in backticked_options(section) {
			assert help.output.contains(option), 'SUBCOMMANDS.md gives `${subcommand}` the option `${option}`, which its help does not mention'
		}
	}
}

fn test_the_skills_describe_the_lockfile_the_installer_writes() {
	help := help_for('install').output
	assert help.contains('v.mod.lock')
	for path in [package_skill_files[0], package_skill_files[2]] {
		text := os.read_file(path)!
		assert text.contains('v.mod.lock'), '${path} does not mention the lockfile that `v install` writes'
		assert text.contains('--locked'), '${path} does not mention `v install --locked`'
	}
}
