// This example demonstrates the `vlib/cli` features that show up automatically
// in `--help` when you populate the optional `group`, `examples` and
// `learn_more` fields of `Command`. No opt-in is required — leaving any of
// them empty simply skips the corresponding section.
//
// Compile from the V repo root:
//   v -o tasky examples/cli_groups.v
//
// Try it out:
//   ./tasky --help          # root help with grouped commands and examples
//   ./tasky issue --help    # sub-command help with INHERITED FLAGS
//   ./tasky issue list -v
//   ./tasky version
module main

import cli
import os

fn main() {
	mut app := cli.Command{
		name:        'tasky'
		description: 'A tiny issue tracker CLI'
		version:     '0.1.0'
		posix_mode:  true
		examples:    [
			'\$ tasky issue list',
			'\$ tasky issue create --title "Fix CI"',
			'\$ tasky config get editor',
		]
		learn_more:  'Use `tasky <command> --help` for details about a command.\nDocumentation lives at https://example.test/tasky'
	}
	app.add_flag(cli.Flag{
		flag:        .string
		name:        'config'
		abbrev:      'c'
		description: 'Path to a tasky config file'
		global:      true
	})
	app.add_command(issue_command())
	app.add_command(config_command())
	app.setup()
	app.parse(os.args)
}

fn issue_command() cli.Command {
	return cli.Command{
		name:        'issue'
		description: 'Work with issues'
		group:       'Core commands'
		commands:    [
			cli.Command{
				name:        'list'
				description: 'List open issues'
				execute:     issue_list
				flags:       [
					cli.Flag{
						flag:        .bool
						name:        'verbose'
						abbrev:      'v'
						description: 'Show issue bodies in addition to titles'
					},
				]
			},
			cli.Command{
				name:        'create'
				description: 'Create a new issue'
				execute:     issue_create
				flags:       [
					cli.Flag{
						flag:        .string
						name:        'title'
						abbrev:      't'
						description: 'Title of the issue to create'
						required:    true
					},
				]
			},
		]
	}
}

fn config_command() cli.Command {
	return cli.Command{
		name:        'config'
		description: 'Read or write tasky settings'
		group:       'Additional commands'
		commands:    [
			cli.Command{
				name:          'get'
				description:   'Print the value of a config key'
				usage:         '<key>'
				required_args: 1
				execute:       config_get
			},
			cli.Command{
				name:          'set'
				description:   'Update a config key'
				usage:         '<key> <value>'
				required_args: 2
				execute:       config_set
			},
		]
	}
}

fn issue_list(cmd cli.Command) ! {
	verbose := cmd.flags.get_bool('verbose') or { false }
	println('Listing issues (verbose=${verbose})')
}

fn issue_create(cmd cli.Command) ! {
	title := cmd.flags.get_string('title')!
	println('Created issue: ${title}')
}

fn config_get(cmd cli.Command) ! {
	println('config get ${cmd.args[0]}')
}

fn config_set(cmd cli.Command) ! {
	println('config set ${cmd.args[0]} = ${cmd.args[1]}')
}
