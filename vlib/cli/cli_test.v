// vtest build: tinyc
module main

import cli { Command, CommandFlag, Flag }
import os
import term
import v.util.diff

const cli_probe = '--vtest-cli-output'

struct CliFixture {
	name      string
	run       fn () @[required]
	exit_code int
}

const cli_fixtures = [
	CliFixture{ name: 'default_command_flag', run: default_command_flag },
	CliFixture{ name: 'default_command_no_flag_err', run: default_command_no_flag_err, exit_code: 1 },
	CliFixture{ name: 'default_help', run: default_help },
	CliFixture{ name: 'default_help_flag_no_command', run: default_help_flag_no_command },
	CliFixture{ name: 'default_version_flag_no_command', run: default_version_flag_no_command },
	CliFixture{ name: 'long_description', run: long_description },
	CliFixture{ name: 'no_execute', run: no_execute },
]

fn testsuite_begin() {
	if os.args.len != 3 || os.args[1] != cli_probe {
		return
	}
	// Some CLI paths exit the process directly, so dispatch before running tests.
	for fixture in cli_fixtures {
		if fixture.name == os.args[2] {
			fixture.run()
			exit(0)
		}
	}
	panic('unknown CLI fixture: ${os.args[2]}')
}

fn test_cli_programs() {
	testdata := os.join_path(@VEXEROOT, 'vlib', 'cli', 'testdata')
	mut has_err := false
	for fixture in cli_fixtures {
		out_path := os.join_path(testdata, fixture.name + '.out')
		print(fixture.name + ' ')
		expected_out := os.read_file(out_path)!.replace('\r\n', '\n')
		result := os.exec([os.executable(), cli_probe, fixture.name])
		actual_out := result.output.replace('\r\n', '\n')
		difference := diff.compare_text(expected_out, actual_out)!
		if result.exit_code != fixture.exit_code || difference != '' {
			println(term.red('FAIL'))
			if result.exit_code != fixture.exit_code {
				eprintln('expected exit code ${fixture.exit_code}, got ${result.exit_code}')
			}
			if difference != '' {
				eprintln(difference)
			}
			has_err = true
		} else {
			println(term.green('OK'))
		}
	}
	assert !has_err
}

fn default_command_flag() {
	mut cmd := Command{
		version:  '0.1.0'
		defaults: struct {
			help: CommandFlag{true, false}
			version: CommandFlag{false, true}
			man: false
		}
	}
	cmd.parse([''])
}

fn default_command_no_flag_err() {
	mut cmd := Command{
		name:     'foo'
		defaults: struct {
			man: CommandFlag{true, false}
		}
	}
	cmd.parse(['foo', '-man'])
}

fn default_help() {
	mut cmd := Command{}
	cmd.defaults = struct {
		man: false
	}
	cmd.parse(os.args[..1])
}

fn default_help_flag_no_command() {
	mut cmd := Command{
		name:     'foo'
		defaults: struct {
			help: CommandFlag{false, true}
		}
	}
	cmd.parse(['foo', '-help'])
}

fn default_version_flag_no_command() {
	mut cmd := Command{
		name:       'foo'
		version:    '0.1.0'
		posix_mode: true
		defaults:   struct {
			version: CommandFlag{false, true}
		}
	}
	cmd.parse(['foo', '--version'])
}

fn long_description() {
	mut cmd := Command{
		name:        'cli'
		description: 'An example of the cli library.'
		version:     '1.0.0'
	}
	mut greet_cmd := Command{
		name:          'greet'
		description:   'Prints greeting in different languages.'
		usage:         '<name>'
		required_args: 1
	}
	greet_cmd.add_flag(Flag{
		flag:        .string_array
		name:        'fun'
		description: '\'{"uri":"mqtt://broker.emqx.io:1883","topic":"test_emq/1","filters":[{"producer_id":0,"trace_group":2,"string_id":3001},{"string_id":3002}]}\''
	})
	cmd.add_command(greet_cmd)
	cmd.setup()
	cmd.parse(['cli', 'greet', '-help'])
}

fn no_execute() {
	mut cmd := Command{
		name: 'cmd'
	}
	cmd.add_command(
		name: 'foo'
	)
	cmd.parse(['', 'cmd', 'foo'])
}
