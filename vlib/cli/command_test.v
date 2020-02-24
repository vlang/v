import cli

fn test_if_command_parses_empty_args() {
	mut cmd := cli.Command{
		name: 'command', 
		execute: empty_func,
	}
	cmd.parse(['command'])
	assert cmd.name == 'command'
		&& compare_arrays(cmd.args, [])
}

fn test_if_command_parses_args() {
	mut cmd := cli.Command{
		name: 'command', 
		execute: empty_func,
	}
	cmd.parse(['command', 'arg0', 'arg1'])

	assert cmd.name == 'command'
		&& compare_arrays(cmd.args, ['arg0', 'arg1'])
}

fn test_if_subcommands_parse_args() {
	mut cmd := cli.Command{
		name: 'command',
	}
	subcmd := cli.Command{
		name: 'subcommand',
		execute: empty_func,
	}
	cmd.add_command(subcmd)
	cmd.parse(['command', 'subcommand', 'arg0', 'arg1'])
}

fn if_subcommands_parse_args_func(cmd cli.Command) {
	assert cmd.name == 'subcommand'
		&& compare_arrays(cmd.args, ['arg0', 'arg1'])
}

fn test_if_command_has_default_help_subcommand() {
	mut cmd := cli.Command{
		name: 'command',
	}
	cmd.parse(['command'])

	assert has_command(cmd, 'help')
}

fn test_if_command_has_default_version_subcommand_if_version_is_set() {
	mut cmd := cli.Command{
		name: 'command',
		version: '1.0.0',
	}
	cmd.parse(['command'])

	assert has_command(cmd, 'version')
}

fn test_if_flag_gets_set() {
	mut cmd := cli.Command{
		name: 'command',
		execute: if_flag_gets_set_func,
	}
	cmd.add_flag(cli.Flag{
		flag: .string
		name: 'flag'
	})
	cmd.parse(['command', '--flag', 'value'])
}

fn if_flag_gets_set_func(cmd cli.Command) {
	flag := cmd.flags.get_string('flag') or { panic(err) }
	assert flag == 'value'
}

fn test_if_flag_gets_set_with_abbrev() {
	mut cmd := cli.Command{
		name: 'command',
		execute: if_flag_gets_set_with_abbrev_func,
	}
	cmd.add_flag(cli.Flag{
		flag: .string,
		name: 'flag',
		abbrev: 'f',
	})
	cmd.parse(['command', '-f', 'value'])
}

fn if_flag_gets_set_with_abbrev_func(cmd cli.Command) {
	flag := cmd.flags.get_string('flag') or { panic(err) }
	assert flag == 'value'
}

fn test_if_multiple_flags_get_set() {
	mut cmd := cli.Command{
		name: 'command',
		execute: if_multiple_flags_get_set_func,
	}
	cmd.add_flag(cli.Flag{
		flag: .string
		name: 'flag'
	})
	cmd.add_flag(cli.Flag{
		flag: .int
		name: 'value'
	})
	cmd.parse(['command', '--flag', 'value', '--value', '42'])
}

fn if_multiple_flags_get_set_func(cmd cli.Command) {
	flag := cmd.flags.get_string('flag') or { panic(err) }
	value := cmd.flags.get_int('value') or { panic(err) }
	assert flag == 'value'
		&& value == 42
}

fn test_if_flag_gets_set_in_subcommand() {
	mut cmd := cli.Command{
		name: 'command',
		execute: empty_func,
	}
	mut subcmd := cli.Command{
		name: 'subcommand',
		execute: if_flag_gets_set_in_subcommand_func
	}
	subcmd.add_flag(cli.Flag{
		flag: .string
		name: 'flag'
	})
	cmd.add_command(subcmd)
	cmd.parse(['command', 'subcommand', '--flag', 'value'])
}

fn if_flag_gets_set_in_subcommand_func(cmd cli.Command) {
	flag := cmd.flags.get_string('flag') or { panic(err) }
	assert flag == 'value'
}

fn test_if_global_flag_gets_set_in_subcommand() {
	mut cmd := cli.Command{
		name: 'command',
		execute: empty_func,
	}
	cmd.add_flag(cli.Flag{
		flag: .string,
		name: 'flag',
		global: true,
	})
	subcmd := cli.Command{
		name: 'subcommand',
		execute: if_global_flag_gets_set_in_subcommand_func,
	}
	cmd.add_command(subcmd)
	cmd.parse(['command', '--flag', 'value', 'subcommand'])
}

fn if_global_flag_gets_set_in_subcommand_func(cmd cli.Command) {
	flag := cmd.flags.get_string('flag') or { panic(err) }
	assert flag == 'value'
}


// helper functions

fn empty_func(cmd cli.Command) {}

fn has_command(cmd cli.Command, name string) bool {
	for subcmd in cmd.commands {
		if subcmd.name == name {
			return true
		}
	}
	return false
}

fn compare_arrays(array0 []string, array1 []string) bool {
	if array0.len != array1.len {
		return false
	}
	for i in 0..array0.len {
		if array0[i] != array1[i] {
			return false
		}
	}
	return true
}
