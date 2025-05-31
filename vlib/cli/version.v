module cli

fn version_flag(with_abbrev bool) Flag {
	sabbrev := if with_abbrev { 'v' } else { '' }
	return Flag{
		flag:        .bool
		name:        'version'
		abbrev:      sabbrev
		description: 'Prints version information.'
	}
}

fn version_cmd() Command {
	return Command{
		name:        'version'
		description: 'Prints version information.'
		execute:     print_version_for_command
	}
}

fn print_version_for_command(cmd Command) ! {
	if cmd.args.len > 0 {
		for sub_cmd in cmd.commands {
			if sub_cmd.name == cmd.args[0] {
				version_cmd := unsafe { &sub_cmd }
				print(version_cmd.version())
				return
			}
		}
		println('Invalid command: ${cmd.args.join(' ')}')
	} else if cmd.parent != unsafe { nil } {
		println(cmd.parent.version())
	} else {
		println(cmd.version())
	}
}

// version returns a generated version `string` for the `Command`.
pub fn (cmd &Command) version() string {
	return '${cmd.name} version ${cmd.version}'
}
