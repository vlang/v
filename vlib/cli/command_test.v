import cli

fn test_if_command_parses_empty_args() {
	mut cmd := cli.Command{
		name:    'command'
		execute: empty_func
	}
	cmd.parse(['command'])
	assert cmd.name == 'command' && cmd.args == []
}

fn test_if_command_parses_args() {
	mut cmd := cli.Command{
		name:    'command'
		execute: empty_func
	}
	cmd.parse(['command', 'arg0', 'arg1'])
	assert cmd.name == 'command' && cmd.args == ['arg0', 'arg1']
}

fn test_if_subcommands_parse_args() {
	mut cmd := cli.Command{
		name: 'command'
	}
	subcmd := cli.Command{
		name:    'subcommand'
		execute: if_subcommands_parse_args_func
	}
	cmd.add_command(subcmd)
	cmd.parse(['command', 'subcommand', 'arg0', 'arg1'])
}

fn if_subcommands_parse_args_func(cmd cli.Command) ! {
	assert cmd.name == 'subcommand' && cmd.args == ['arg0', 'arg1']
}

fn test_default_subcommands() {
	mut cmd := cli.Command{
		name: 'command'
	}
	cmd.parse(['command'])
	assert cmd.commands.any(it.name == 'help')
	assert cmd.commands.any(it.name == 'man')

	cmd = cli.Command{
		name:    'command'
		version: '1.0.0'
	}
	cmd.parse(['command'])
	assert cmd.commands.any(it.name == 'version')
}

fn flag_should_be_set(cmd cli.Command) ! {
	flag := cmd.flags.get_string('flag')!
	assert flag == 'value'
}

fn test_if_flag_gets_set() {
	mut cmd := cli.Command{
		name:    'command'
		execute: flag_should_be_set
	}
	cmd.add_flag(cli.Flag{
		flag: .string
		name: 'flag'
	})
	cmd.parse(['command', '-flag', 'value'])
}

fn test_if_flag_gets_set_with_abbrev() {
	mut cmd := cli.Command{
		name:    'command'
		execute: flag_should_be_set
	}
	cmd.add_flag(cli.Flag{
		flag:   .string
		name:   'flag'
		abbrev: 'f'
	})
	cmd.parse(['command', '-f', 'value'])
}

fn test_if_flag_gets_set_with_long_arg() {
	mut cmd := cli.Command{
		name:       'command'
		execute:    flag_should_be_set
		posix_mode: true
	}
	cmd.add_flag(cli.Flag{
		flag:   .string
		name:   'flag'
		abbrev: 'f'
	})
	cmd.parse(['command', '--flag', 'value'])
}

fn assert_flags(cmd cli.Command) ! {
	flag := cmd.flags.get_string('flag')!
	assert flag == 'value'
	value := cmd.flags.get_int('value')!
	assert value == 42
	flag2 := cmd.flags.get_string('flag-2')!
	assert flag2 == 'value-2'
}

fn test_if_multiple_flags_get_set() {
	mut cmd := cli.Command{
		name:    'command'
		execute: assert_flags
	}
	cmd.add_flag(cli.Flag{
		flag: .string
		name: 'flag'
	})
	cmd.add_flag(cli.Flag{
		flag: .string
		name: 'flag-2'
	})
	cmd.add_flag(cli.Flag{
		flag: .int
		name: 'value'
	})
	cmd.parse(['command', '-flag=value', '-value', '42', '-flag-2', 'value-2'])
}

fn test_if_required_flags_get_set() {
	mut cmd := cli.Command{
		name:    'command'
		execute: assert_flags
	}
	cmd.add_flag(cli.Flag{
		flag: .string
		name: 'flag'
	})
	cmd.add_flag(cli.Flag{
		flag: .string
		name: 'flag-2'
	})
	cmd.add_flag(cli.Flag{
		flag:     .int
		name:     'value'
		required: true
	})
	cmd.parse(['command', '-flag', 'value', '-value', '42', '-flag-2', 'value-2'])
}

fn flag_is_set_in_subcommand(cmd cli.Command) ! {
	flag := cmd.flags.get_string('flag') or { panic(err) }
	assert flag == 'value'
}

fn test_if_flag_gets_set_in_subcommand() {
	mut cmd := cli.Command{
		name:    'command'
		execute: empty_func
	}
	mut subcmd := cli.Command{
		name:    'subcommand'
		execute: flag_is_set_in_subcommand
	}
	subcmd.add_flag(cli.Flag{
		flag: .string
		name: 'flag'
	})
	cmd.add_command(subcmd)
	cmd.parse(['command', 'subcommand', '-flag', 'value'])
}

fn test_if_global_flag_gets_set_in_subcommand() {
	mut cmd := cli.Command{
		name:    'command'
		execute: empty_func
	}
	cmd.add_flag(cli.Flag{
		flag:   .string
		name:   'flag'
		global: true
	})
	subcmd := cli.Command{
		name:    'subcommand'
		execute: flag_is_set_in_subcommand
	}
	cmd.add_command(subcmd)
	cmd.parse(['command', '-flag', 'value', 'subcommand'])
}

fn test_command_setup() {
	mut cmd := cli.Command{
		name:     'root'
		commands: [
			cli.Command{
				name:     'child'
				commands: [
					cli.Command{
						name: 'child-child'
					},
				]
			},
		]
	}
	assert isnil(cmd.commands[0].parent)
	assert isnil(cmd.commands[0].commands[0].parent)
	cmd.setup()
	assert cmd.commands[0].parent.name == 'root'
	assert cmd.commands[0].commands[0].parent.name == 'child'
}

// helper functions
fn empty_func(cmd cli.Command) ! {
}
