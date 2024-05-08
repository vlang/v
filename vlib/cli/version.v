module cli

fn version_flag(with_abbrev bool) Flag {
	sabbrev := if with_abbrev { 'v' } else { '' }
	return Flag{
		flag: .bool
		name: 'version'
		abbrev: sabbrev
		description: 'Prints version information.'
	}
}

fn version_cmd() Command {
	return Command{
		name: 'version'
		description: 'Prints version information.'
		execute: print_version_for_command
	}
}

fn print_version_for_command(cmd Command) ! {
	version_cmd := if cmd.args.len > 0 {
		mut cmd_ := &Command{}
		for arg in cmd.args {
			mut found := false
			for sub_cmd in cmd.commands {
				if sub_cmd.name == arg {
					cmd_ = unsafe { &sub_cmd }
					found = true
					break
				}
			}
			if !found {
				args := cmd.args.join(' ')
				println('Invalid command: ${args}')
				return
			}
		}
		cmd_
	} else if cmd.parent != unsafe { nil } {
		cmd.parent
	} else {
		&cmd
	}
	version := '${version_cmd.name} version ${version_cmd.version}'
	println(version)
}
