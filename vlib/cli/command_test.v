@[has_globals]
module main

import cli

__global (
	if_subcommands_parse_args_called bool
	flag_should_be_set_called        bool
	assert_flags_called              bool
	flag_is_set_in_subcommand_called bool
)

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
	if_subcommands_parse_args_called = false
	subcmd := cli.Command{
		name:    'subcommand'
		execute: if_subcommands_parse_args_func
	}
	cmd.add_command(subcmd)
	cmd.parse(['command', 'subcommand', 'arg0', 'arg1'])
	assert if_subcommands_parse_args_called
}

fn test_if_subcommand_alias_parses_args() {
	mut cmd := cli.Command{
		name: 'command'
	}
	subcmd := cli.Command{
		name:    'subcommand'
		alias:   'sc'
		execute: if_subcommands_parse_args_func
	}
	cmd.add_command(subcmd)
	cmd.parse(['command', 'sc', 'arg0', 'arg1'])
}

fn if_subcommands_parse_args_func(cmd cli.Command) ! {
	if_subcommands_parse_args_called = true
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
	flag_should_be_set_called = true
	flag := cmd.flags.get_string('flag')!
	assert flag == 'value'
}

fn test_if_flag_gets_set() {
	flag_should_be_set_called = false
	mut cmd := cli.Command{
		name:    'command'
		execute: flag_should_be_set
	}
	cmd.add_flag(cli.Flag{
		flag: .string
		name: 'flag'
	})
	cmd.parse(['command', '-flag', 'value'])
	assert flag_should_be_set_called
}

fn test_if_flag_gets_set_with_abbrev() {
	flag_should_be_set_called = false
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
	assert flag_should_be_set_called
}

fn test_if_flag_gets_set_with_long_arg() {
	flag_should_be_set_called = false
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
	assert flag_should_be_set_called
}

fn assert_flags(cmd cli.Command) ! {
	assert_flags_called = true
	flag := cmd.flags.get_string('flag')!
	assert flag == 'value'
	value := cmd.flags.get_int('value')!
	assert value == 42
	flag2 := cmd.flags.get_string('flag-2')!
	assert flag2 == 'value-2'
}

fn test_if_multiple_flags_get_set() {
	assert_flags_called = false
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
	assert assert_flags_called
}

fn test_if_required_flags_get_set() {
	assert_flags_called = false
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
	assert assert_flags_called
}

fn flag_is_set_in_subcommand(cmd cli.Command) ! {
	flag_is_set_in_subcommand_called = true
	flag := cmd.flags.get_string('flag') or { panic(err) }
	assert flag == 'value'
}

fn test_if_flag_gets_set_in_subcommand() {
	mut cmd := cli.Command{
		name:    'command'
		execute: empty_func
	}
	flag_is_set_in_subcommand_called = false
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
	assert flag_is_set_in_subcommand_called
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
	flag_is_set_in_subcommand_called = false
	subcmd := cli.Command{
		name:    'subcommand'
		execute: flag_is_set_in_subcommand
	}
	cmd.add_command(subcmd)
	cmd.parse(['command', '-flag', 'value', 'subcommand'])
	assert flag_is_set_in_subcommand_called
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

fn test_help_message_lists_command_aliases() {
	cmd := cli.Command{
		name:        'command'
		description: 'description'
		commands:    [
			cli.Command{
				name:        'sub'
				alias:       's'
				description: 'subcommand'
			},
		]
	}
	assert cmd.help_message() == r'Usage: command [commands]

description

Commands:
  sub (s)             subcommand
'
}

fn test_manpage_lists_command_aliases() {
	mut cmd := cli.Command{
		name:        'command'
		description: 'description'
		commands:    [
			cli.Command{
				name:        'sub'
				alias:       's'
				description: 'subcommand'
			},
		]
	}
	cmd.setup()
	assert cmd.manpage().after_char(`\n`) == r'.Dt COMMAND 1
.Os
.Sh NAME
.Nm command
.Nd description
.Sh SYNOPSIS
.Nm command
.Nm command
.Ar subcommand
.Sh DESCRIPTION
description
.Pp
The subcommands are as follows:
.Bl -tag -width indent
.It Cm sub Pq Cm s
subcommand
.El
.Sh SEE ALSO
.Xr command-sub 1
'
}

// helper functions
fn empty_func(cmd cli.Command) ! {
}
